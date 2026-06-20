/*
 * Copyright 2026 Stephen Tetley
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/// # Acknowledgment
///
/// This is an implementation of Andy Gill's `dotgen` (Text.Dot).
/// https://hackage.haskell.org/package/dotgen
///
/// Note - because it uses effects rather than a monad the implementation
/// is imperative unlike `dotgen`. Do not mix this libary with code that 
/// does backtracking.

pub mod BasicGraphviz.DotGen {
    
    use FlixPretty.Doc
    use FlixPretty.PrettyClass.Pretty
    use FlixPretty.{<<>>, <&>}
    use FlixPretty.{vcat, group, indent, empty, text, dquotes, semi, equals, commaBrackets,
        lbrace, rbrace}


    pub eff DotGen {
        def node(attrs: List[(String, String)]): NodeId
        def userNode(nid: NodeId, attrs: List[(String, String)]): Unit
        def edge(n1: NodeId, n2: NodeId, attrs: List[(String, String)]): Unit

        def attribute(n: String, v: String): Unit
    }

    pub enum NodeId with Eq, Order, ToString {
        case NodeId(String)
        case UserNodeId(Int32)
    }

    pub enum DotElement with Eq, Order, ToString {
        case Attribute(String, String)
        case Node(NodeId, List[(String, String)])
        case Edge(NodeId, NodeId, List[(String, String)])
        case Scope(List[DotElement])
        case SubGraph(NodeId, List[DotElement])
    }


    pub def handle(f: a -> b \ ef): a -> (String, b) \ ef - DotGen = x -> region rc {
        let i = Ref.fresh(rc, 1);
        let w = MutList.empty(rc);
        let ans = run {
            f(x)
        } with handler DotGen {

            def node(attrs, k) = {
                let i1 = Ref.get(i);
                let nid = NodeId.NodeId("n${i1}");
                MutList.push(DotElement.Node(nid, attrs), w);
                Ref.put(i1+1, i);
                k(nid)
            }   
            
            def userNode(nid, attrs, k) = {
                MutList.push(DotElement.Node(nid, attrs), w);
                k()
            }

            def edge(n1, n2, attrs, k) = {
                MutList.push(DotElement.Edge(n1, n2, attrs), w);
                k()
            }

            def attribute(n, v, k)= {
                MutList.push(DotElement.Attribute(n, v), w);
                k()
            }
        };
        let xs = MutList.toList(w);
        let doc = text("digraph") <<>> text("G")  <<>> ppBlock(List.map(Pretty.pPrint, xs));
        (FlixPretty.render(80, doc), ans)
    }

    instance Pretty[DotElement] {
        pub override def pPrint(x: DotElement): Doc = match x {
            case DotElement.Attribute(n, v)         => ppAttr(n, v)
            
            case DotElement.Node(nid, attrs)        => 
                Pretty.pPrint(nid) ++ ppAttrs(attrs) ++ semi()
            
            case DotElement.Edge(n1, n2, attrs)     => 
                Pretty.pPrint(n1) <<>> text("->") <<>> Pretty.pPrint(n2) ++ ppAttrs(attrs) ++ semi()
            
            case DotElement.Scope(elems)            => 
                ppBlock(List.map(Pretty.pPrint, elems))

            case DotElement.SubGraph(nid, elems)    => 
                text("subgraph") <<>> Pretty.pPrint(nid) ++ ppBlock(List.map(Pretty.pPrint, elems))
        }
    }

    instance Pretty[NodeId] {
        pub override def pPrint(x: NodeId): Doc = match x {
            case NodeId.NodeId(s)      => text(s)
            case NodeId.UserNodeId(i)  => if (i<0) text("u_${Int32.abs(i)}") else text("u${i}")
        }
    }

    // Not working properly...
    def ppBlock(docs: List[Doc]): Doc = 
        FlixPretty.hang(4, group(lbrace() <&> vcat(docs))) <&> rbrace()


    def ppAttrs(xs: List[(String, String)]): Doc = match xs {
        case Nil => empty()
        case _   => commaBrackets(List.map(match(x, y) -> ppAttr(x, y), xs))
    }

    def ppAttr(n: String, v: String): Doc = 
        text(n) ++ equals() ++ dquotes(text(escape(v)))


    // escape " and \...
    def escape(s: String): String = s

    pub def runDotGen(f: Unit -> a \ ef): (String, a) \ ef - DotGen = 
        handle(f)()


    pub def userNodeId(i: Int32): NodeId = NodeId.UserNodeId(i)

    

}

