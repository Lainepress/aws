import           Aws
import           Aws.ElasticTranscoder
import qualified Aws.S3                             as S3
import           Data.Conduit        (($$+-))
import           Data.Conduit.Binary (sinkFile)
import           Network.HTTP.Conduit
import           Control.Monad.Trans


data TestType
    = S3
    | EtsCJ
    | EtsGJ
    | EtsErr
    | EtsDJ
    | EtsLJS
    | EtsLJP
    deriving (Show)


main :: IO ()
main =
    case EtsErr of
      S3     -> testS3
      EtsCJ  -> testEtsCJ
      EtsGJ  -> testEtsGJ
      EtsErr -> testEtsErr
      EtsDJ  -> testEtsDJ
      EtsLJS -> testEtsLJS
      EtsLJP -> testEtsLJP


testEtsCJ :: IO ()
testEtsCJ = 
 do cfg <- Aws.baseConfiguration
    rsp <- withManager $ \mgr -> Aws.pureAws cfg my_ets_cfg mgr $ 
                createJob "Wildlife.wmv" "Wildlife-t.f4v" my_preset my_pipeline
    print rsp

testEtsGJ :: IO ()
testEtsGJ = 
 do cfg <- Aws.baseConfiguration
    rsp <- withManager $ \mgr -> Aws.pureAws cfg my_ets_cfg mgr $ 
                GetJob my_job
    print rsp

testEtsErr :: IO ()
testEtsErr = 
 do cfg <- Aws.baseConfiguration
    rsp <- withManager $ \mgr -> Aws.pureAws cfg my_ets_cfg mgr $ 
                GetJob my_ne_job
    print rsp

testEtsDJ :: IO ()
testEtsDJ = 
 do cfg <- Aws.baseConfiguration
    withManager $ \mgr -> 
     do cjr <- Aws.pureAws cfg my_ets_cfg mgr $
            createJob "Wildlife.wmv" "Wildlife-t.f4v" my_preset my_pipeline
        liftIO $ print cjr
        djr <- Aws.pureAws cfg my_ets_cfg mgr $
            DeleteJob $ cjrId cjr 
        liftIO $ print djr

testEtsLJS :: IO ()
testEtsLJS = 
 do cfg <- Aws.baseConfiguration
    rsp <- withManager $ \mgr -> Aws.pureAws cfg my_ets_cfg mgr $ 
                ListJobsByStatus STSComplete True TNNull
    print rsp

testEtsLJP :: IO ()
testEtsLJP = 
 do cfg <- Aws.baseConfiguration
    rsp <- withManager $ \mgr -> Aws.pureAws cfg my_ets_cfg mgr $ 
                ListJobsByPipeline my_pipeline True TNNull
    print rsp

my_ets_cfg :: EtsConfiguration NormalQuery
my_ets_cfg = etsConfiguration HTTPS etsEndpointEu

my_preset :: PresetId
my_preset = "1351620000000-000001"

my_pipeline :: PipelineId
my_pipeline = "1359460188157-258e48"

my_job :: JobId
my_job = "1359460493779-ca6f29"

my_ne_job :: JobId
my_ne_job = "1359460493779-badbad"

my_disposable_job :: JobId
my_disposable_job = "1361017186442-6bbecf"

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
