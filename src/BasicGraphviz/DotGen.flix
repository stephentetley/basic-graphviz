

pub mod BasicGraphviz.DotGen {


    use FlixPretty.Doc
    use FlixPretty.PrettyClass.Pretty
    use FlixPretty.{<<>>}
    use FlixPretty.{vcat, text, semi}


    pub eff DotGen {
        def node(): NodeId
        def edge(n1: NodeId, n2: NodeId): Unit
    }

    pub enum NodeId with Eq, Order, ToString {
        case NodeId(String)
        case UserNodeId(Int32)
    }

    pub enum DotElement with Eq, Order, ToString {
        case Node(NodeId)
        case Edge(NodeId, NodeId)
    }


    pub def handle(f: a -> b \ ef): a -> (String, b) \ ef - DotGen = x -> region rc {
        let i = Ref.fresh(rc, 1);
        let w = MutList.empty(rc);
        let ans = run {
            f(x)
        } with handler DotGen {

            def node(k) = {
                let nid = NodeId.NodeId("n${Ref.get(i)}");
                MutList.push(DotElement.Node(nid), w);
                k(nid)
            }   

            def edge(n1, n2, k) = {
                MutList.push(DotElement.Edge(n1, n2), w);
                k()
            }
        };
        let xs = MutList.toList(w);
        let doc = vcat(List.map(Pretty.pPrint, xs));
        (FlixPretty.render(80, doc), ans)
    }

    instance Pretty[DotElement] {
        pub override def pPrint(x: DotElement): Doc = match x {
            case DotElement.Node(nid)       => Pretty.pPrint(nid) ++ semi()
            case DotElement.Edge(n1, n2)    => 
                Pretty.pPrint(n1) <<>> text("->") <<>> Pretty.pPrint(n2) ++ semi()
        }
    }

    instance Pretty[NodeId] {
        pub override def pPrint(x: NodeId): Doc = match x {
            case NodeId.NodeId(s)      => text(s)
            case NodeId.UserNodeId(i)  => if (i<0) text("u_${Int32.abs(i)}") else text("u${i}")
        }
    }

    pub def runDotGen(f: Unit -> a \ ef): (String, a) \ ef - DotGen = 
        handle(f)()


    pub def model1(): Unit \ DotGen = 
        let n1 = node();
        let n2 = node();
        let _ = edge(n1, n2);
        () 
}