import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW          as GLFW
import qualified Codec.Picture             as PIC
import qualified Data.ByteString           as B
import Graphics.Rendering.OpenGL (($=))
import Control.Monad             (unless, when)
import System.IO                 (hPutStrLn, stderr)
import System.Exit               (exitSuccess, exitFailure)
import GHC.Float                 (double2Float)
import Foreign.C.Types
import Text.Printf

errorCallback :: GLFW.ErrorCallback
errorCallback err description = hPutStrLn stderr description

keyCallback :: GLFW.KeyCallback
keyCallback window key scancode action mods =
    when (key == GLFW.Key'Escape && action == GLFW.KeyState'Pressed) $
        GLFW.setWindowShouldClose window True

sizeCallback :: GLFW.WindowSizeCallback
sizeCallback window width height = do
    let (w,h) = (fromIntegral width, fromIntegral height)
    let siz = fromIntegral $ min width height
    GL.viewport $= (GL.Position ((w - min w h)`div`2) ((h - min w h)`div`2), GL.Size siz siz)

cursorPosCallback :: GL.UniformLocation -> GLFW.CursorPosCallback
cursorPosCallback c window x y = do
    (w, h) <- GLFW.getWindowSize window
    let cr = 4 * (x / fromIntegral w - 0.5)
        ci = 2 * (0.5 - y / fromIntegral h)
    printf "%.2f :+ %.2f\n" cr ci
    GL.uniform c $= (GL.Vertex2 (CFloat $ double2Float cr) (CFloat $ double2Float ci) :: GL.Vertex2 GL.GLfloat)


fragmentShaderSource :: B.ByteString
fragmentShaderSource = GL.packUtf8 . unlines $ [
    "uniform vec2 c;",
    "void main() {",
    "    vec2 z;",
    "    z.x = 4.2 * (gl_TexCoord[0].x - 0.5);",
    "    z.y = 4.2 * (gl_TexCoord[0].y - 0.5);",

    "    int i;",
    "    for(i=0; i<100; i++) { // iter = 100",
    "        float x = z.x * z.x - z.y * z.y + c.x;",
    "        float y = 2.0 * z.y * z.x + c.y;",

    "        if(x * x + y * y > 2.0) break;",
    "        z.x = x;",
    "        z.y = y;",
    "    }",
    "                              // iter = 100",
    "    gl_FragColor.x = gl_FragColor.y = gl_FragColor.z = (i == 100 ? 1.0 : float(i) / 100.0);",
    "}" ]


main :: IO ()
main = do
  let width  = 512
      height = 512
  GLFW.setErrorCallback (Just errorCallback)
  successfulInit <- GLFW.init
  if not successfulInit then exitFailure else do
      mw <- GLFW.createWindow width height "Julia" Nothing Nothing
      case mw of
        Nothing -> GLFW.terminate >> exitFailure
        Just window -> do
          GLFW.makeContextCurrent mw
          (width, height) <- GLFW.getFramebufferSize window
          let ratio = fromIntegral width / fromIntegral height

          GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral width) (fromIntegral height))

          GL.matrixMode $= GL.Projection
          GL.loadIdentity
          GL.ortho (ratio) ratio (-1.0) 1.0 1.0 (-1.0)
          GL.matrixMode $= GL.Modelview 0

          GL.loadIdentity

          fragmentShader <- GL.createShader GL.FragmentShader
          GL.shaderSourceBS fragmentShader $= fragmentShaderSource
          GL.compileShader fragmentShader
          
          program <- GL.createProgram
          GL.attachedShaders program $= [fragmentShader]
          GL.linkProgram program
          GL.currentProgram $= Just program
          
          c <- GL.get (GL.uniformLocation program "c")
          
          GLFW.setKeyCallback window (Just keyCallback)
          GLFW.setWindowSizeCallback window (Just sizeCallback)
          GLFW.setCursorPosCallback window (Just $ cursorPosCallback c)

          mainLoop window

          GLFW.destroyWindow window
          GLFW.terminate
          exitSuccess


mainLoop :: GLFW.Window -> IO ()
mainLoop w = do 
    shouldClose <- GLFW.windowShouldClose w
    unless shouldClose $ do
        draw

        GLFW.swapBuffers w
        GLFW.waitEvents
        mainLoop w

draw= do
  GL.renderPrimitive GL.Quads $ do
    GL.texCoord (GL.TexCoord2 0 0       :: GL.TexCoord2 GL.GLfloat)
    GL.vertex   (GL.Vertex3 (-1) (-1) 0 :: GL.Vertex3 GL.GLfloat)
    GL.texCoord (GL.TexCoord2 0 1       :: GL.TexCoord2 GL.GLfloat)
    GL.vertex   (GL.Vertex3 (-1) (1) 0  :: GL.Vertex3 GL.GLfloat)
    GL.texCoord (GL.TexCoord2 1 1       :: GL.TexCoord2 GL.GLfloat)
    GL.vertex   (GL.Vertex3 (1) (1) 0   :: GL.Vertex3 GL.GLfloat)
    GL.texCoord (GL.TexCoord2 1 0       :: GL.TexCoord2 GL.GLfloat)
    GL.vertex   (GL.Vertex3 (1) (-1) 0  :: GL.Vertex3 GL.GLfloat)

