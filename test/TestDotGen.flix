

pub mod TestDotGen {

    use Assert.{assertTrue}
    use BasicGraphviz.DotGen
    use BasicGraphviz.DotGen.{node, edge, runDotGen}


    def model1(): Bool \ DotGen = {
        let n1 = node();
        let n2 = node();
        let _ = edge(n1, n2);
        true 
    }

    @Test
    pub def test01(): Unit \ Assert + IO = 
        let (outp, a) = runDotGen(()-> model1());
        println(outp);
        assertTrue(a)


}

