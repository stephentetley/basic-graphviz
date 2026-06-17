

pub mod BasicGraphviz.DotGen {

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

    pub type alias St = Int32

    pub def handle(f: a -> b \ ef): a -> (List[DotElement], b) \ ef - DotGen = x -> region rc {
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
        (xs, ans)
    }

    pub def runDotGen(f: Unit -> a \ ef): (List[DotElement], a) \ ef - DotGen = 
        handle(f)()


    pub def model1(): Unit \ DotGen = 
        let n1 = node();
        let n2 = node();
        let _ = edge(n1, n2);
        () 
}