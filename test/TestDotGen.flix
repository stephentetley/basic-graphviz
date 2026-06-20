

pub mod TestDotGen {

    use Assert.{assertTrue}
    use BasicGraphviz.DotGen
    use BasicGraphviz.DotGen.{node, edge, runDotGen}


    def model1(): Bool \ DotGen = {
        let n1 = node(Nil);
        let n2 = node(Nil);
        let _ = edge(n1, n2, Nil);
        true 
    }

    @Test
    pub def test01(): Unit \ Assert + IO = 
        let (outp, a) = runDotGen(()-> model1());
        println(outp);
        assertTrue(a)


}

