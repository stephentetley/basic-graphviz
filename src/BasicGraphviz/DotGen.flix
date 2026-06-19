

pub mod BasicGraphviz.DotGen {
    use BasicGraphviz.DotGen.DotElement


    use FlixPretty.Doc
    use FlixPretty.PrettyClass.Pretty
    use FlixPretty.{<<>>, <&>}
    use FlixPretty.{vcat, group, indent, empty, text, dquotes, semi, equals, commaBrackets,
        lbrace, rbrace}


    pub eff DotGen {
        def node(): NodeId
        def edge(n1: NodeId, n2: NodeId): Unit
    }

    pub enum NodeId with Eq, Order, ToString {
        case NodeId(String)
        case UserNodeId(Int32)
    }

    pub enum DotElement with Eq, Order, ToString {
        case GraphAttribute(String, String)
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

            def node(k) = {
                let nid = NodeId.NodeId("n${Ref.get(i)}");
                MutList.push(DotElement.Node(nid, Nil), w);
                k(nid)
            }   

            def edge(n1, n2, k) = {
                MutList.push(DotElement.Edge(n1, n2, Nil), w);
                k()
            }
        };
        let xs = MutList.toList(w);
        let doc = text("digraph") <<>> text("G")  <<>> ppBlock(List.map(Pretty.pPrint, xs));
        (FlixPretty.render(80, doc), ans)
    }

    instance Pretty[DotElement] {
        pub override def pPrint(x: DotElement): Doc = match x {
            case DotElement.GraphAttribute(n, v)    => ppAttr(n, v)
            
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


    pub def model1(): Unit \ DotGen = 
        let n1 = node();
        let n2 = node();
        let _ = edge(n1, n2);
        () 
}