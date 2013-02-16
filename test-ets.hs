import           Aws
import           Aws.Ets
import qualified Aws.S3                             as S3
import           Data.Conduit        (($$+-))
import           Data.Conduit.Binary (sinkFile)
import           Network.HTTP.Conduit


data TestType
    = S3
    | Ets
    deriving (Show)


main :: IO ()
main =
    case Ets of
      S3  -> testS3
      Ets -> testEts


testEts :: IO ()
testEts = 
 do cfg <- Aws.baseConfiguration
    rsp <- withManager $ \mgr -> Aws.pureAws cfg my_ets_cfg mgr $ 
                createJob "Wildlife.wmv" "Wildlife-t.f4v" my_preset my_pipeline
    print rsp

my_ets_cfg :: EtsConfiguration NormalQuery
my_ets_cfg = etsConfiguration HTTPS etsEndpointEu

my_preset :: PresetId
my_preset = "1351620000000-000001"

my_pipeline :: PipelineId
my_pipeline = "1359460188157-258e48"

testS3 :: IO ()
testS3 = 
 do -- Set up AWS credentials and the default configuration.
    cfg <- Aws.baseConfiguration
    let s3cfg = Aws.defServiceConfig :: S3.S3Configuration Aws.NormalQuery
    
    -- Set up a ResourceT region with an available HTTP manager.
    withManager $ \mgr ->
     do -- Create a request object with S3.getObject and
        -- run the request with pureAws.
        S3.GetObjectResponse { S3.gorResponse = rsp } <-
            Aws.pureAws cfg s3cfg mgr $
                                S3.getObject "haskell-aws" "cloud-remote.pdf"
        -- Save the response to a file.
        responseBody rsp $$+- sinkFile "cloud-remote.pdf"
