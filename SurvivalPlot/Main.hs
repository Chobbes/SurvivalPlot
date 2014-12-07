import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo


main :: IO ()
main = do 
          undefined

plottyPants = toFile def "example1_big.png" $ 
              do layout_title .= "Survival Distribution"
                 plot (line "am" [signal [0,(0.5)..50]])
                 plot (line "wah" [[(x, sin x) | x <- [0,(0.05)..50]]])
                 plot (points "am points" (signal [0,7..50]))
