{-# OPTIONS_GHC -XGeneralizedNewtypeDeriving #-}

module Aws.ElasticTranscoder.Json.Types
    ( S3Object
    , SNSTopic
    , emptySNSTopic
    , IAMRole
    , JobId(..)
    , PresetId(..)
    , PageToken(..)
    , JobSpec(..)
    , JobSingle(..)
    , JobList(..)
    , JobSpecId(..)
    , JSInput(..)
    , JSOutput(..)
    , JSOutputStatus(..)
    , FrameRate(..)
    , Resolution(..)
    , AspectRatio(..)
    , Container(..)
    , Rotate(..)
    , Status(..)
    , status_t
    , PipelineName(..)
    , PipelineId(..)
    , Pipeline(..)
    , PipelineSingle(..)
    , PipelineList(..)
    , PipelineIdStatus(..)
    , Notifications(..)
    , PipelineStatus
    , pipelineStatusActive
    , pipelineStatusPaused
    , PipelineIdAndStatus(..)
    , AutoBool(..)
    , TextOrNull(..)
    , SUCCESS(..)
    , EtsServiceError(..)
    , bool_t
    ) where

import           Control.Monad
import           Control.Applicative
import           Text.Printf
import           Text.Regex
import           Data.String
import qualified Data.Map                       as Map
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Text                      as T
import qualified Test.QuickCheck                as QC
import           Safe


--
-- | Aws.S3 uses Text for object keys
--

type S3Object = T.Text


--
-- | SNS Topics represented by Text values
--

type SNSTopic = T.Text

emptySNSTopic :: SNSTopic
emptySNSTopic = ""


--
-- | IAM Role represented by Yext
--

type IAMRole = T.Text


--
-- | Job Identifiers
--

newtype JobId = JID { _JID :: T.Text }
    deriving (Show,IsString,Eq)

instance FromJSON JobId where
    parseJSON = withText "JobId" $ return . JID

instance ToJSON JobId where
    toJSON = String . _JID

instance QC.Arbitrary JobId where
    arbitrary = JID . T.pack <$> QC.arbitrary


--
-- | Preset Identifiers
--

newtype PresetId = PRID { _PRID :: T.Text }
    deriving (Show,IsString,Eq)

instance FromJSON PresetId where
    parseJSON = withText "PresetId" $ return . PRID

instance ToJSON PresetId where
    toJSON = String . _PRID

instance QC.Arbitrary PresetId where
    arbitrary = PRID . T.pack <$> QC.arbitrary



--
-- | Page Tokens
--

newtype PageToken = PGTK { _PGTK :: T.Text }
    deriving (Show,IsString,Eq)

instance FromJSON PageToken where
    parseJSON = withText "PageToken" $ return . PGTK

instance ToJSON PageToken where
    toJSON = String . _PGTK

instance QC.Arbitrary PageToken where
    arbitrary = PGTK . T.pack <$> QC.arbitrary



--
-- | Job Specifications
--

data JobSpec
    = JobSpec
        { jsInput      :: JSInput 
        , jsOutput     :: JSOutput
        , jsPipelineId :: PipelineId
        }
    deriving (Show,Eq)

instance FromJSON JobSpec where
     parseJSON (Object v) = 
        JobSpec <$>
            v .: "Input"                            <*>
            v .: "Output"                           <*>
            v .: "PipelineId"
     parseJSON _          = mzero

instance ToJSON JobSpec where
     toJSON js@(JobSpec _ _ _) =
        object 
            [ "Input"      .= jsInput      js
            , "Output"     .= jsOutput     js
            , "PipelineId" .= jsPipelineId js
            ]

instance QC.Arbitrary JobSpec where
    arbitrary = JobSpec <$> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary


--
-- | Job Single
--

newtype JobSingle
    = JobSingle
        { jsJob :: JobSpecId
        }
    deriving (Show,Eq)

instance FromJSON JobSingle where
     parseJSON (Object v) = 
        JobSingle <$>
            v .: "Job"
     parseJSON _          = mzero

instance ToJSON JobSingle where
     toJSON js =
        object 
            [ "Job" .= jsJob js
            ]

instance QC.Arbitrary JobSingle where
    arbitrary = JobSingle <$> QC.arbitrary


--
-- | Job List
--

data JobList
    = JobList
        { jlJobs          :: [JobSpecId]
        , jlNextPageToken :: TextOrNull
        }
    deriving (Show,Eq)

instance FromJSON JobList where
     parseJSON (Object v) = 
        JobList <$>
            v .: "Jobs"                             <*>
            v .: "NextPageToken"
     parseJSON _          = mzero

instance ToJSON JobList where
     toJSON js@(JobList _ _) =
        object 
            [ "Jobs"          .= jlJobs          js
            , "NextPageToken" .= jlNextPageToken js
            ]

instance QC.Arbitrary JobList where
    arbitrary = JobList <$> QC.arbitrary <*> QC.arbitrary


--
-- | Job Specifications with JobId & Status
--

data JobSpecId
    = JobSpecId
        { jsiId         :: JobId
        , jsiInput      :: JSInput 
        , jsiOutput     :: JSOutputStatus
        , jsiPipelineId :: PipelineId
        }
    deriving (Show,Eq)

instance FromJSON JobSpecId where
     parseJSON (Object v) = 
        JobSpecId <$>
            v .: "Id"                               <*>
            v .: "Input"                            <*>
            v .: "Output"                           <*>
            v .: "PipelineId"
     parseJSON _          = mzero

instance ToJSON JobSpecId where
     toJSON jsi@(JobSpecId _ _ _ _) =
        object
            [ "Id"         .= jsiId         jsi
            , "Input"      .= jsiInput      jsi
            , "Output"     .= jsiOutput     jsi
            , "PipelineId" .= jsiPipelineId jsi
            ]

instance QC.Arbitrary JobSpecId where
    arbitrary = 
        JobSpecId 
            <$> QC.arbitrary 
            <*> QC.arbitrary 
            <*> QC.arbitrary
            <*> QC.arbitrary


--
-- | Job Input Parameters
--

data JSInput 
    = JSInput
        { jsiKey                :: S3Object
        , jsiFrameRate          :: FrameRate  
        , jsiResolution         :: Resolution
        , jsiAspectRatio        :: AspectRatio
        , jsiInterlaced         :: AutoBool
        , jsiContainer          :: Container
        }
    deriving (Show,Eq)

instance FromJSON JSInput where
    parseJSON (Object v) = 
        JSInput <$>
            v .: "Key"                          <*>
            v .: "FrameRate"                    <*>
            v .: "Resolution"                   <*>
            v .: "AspectRatio"                  <*>
            v .: "Interlaced"                   <*>
            v .: "Container"
    parseJSON _          = mzero

instance ToJSON JSInput where
    toJSON ijs@(JSInput _ _ _ _ _ _) =
        object 
            [ "Key"         .= jsiKey         ijs
            , "FrameRate"   .= jsiFrameRate   ijs
            , "Resolution"  .= jsiResolution  ijs
            , "AspectRatio" .= jsiAspectRatio ijs
            , "Interlaced"  .= jsiInterlaced  ijs
            , "Container"   .= jsiContainer   ijs
            ]

instance QC.Arbitrary JSInput where
    arbitrary =
        JSInput 
            <$> (T.pack <$> QC.arbitrary) 
            <*> QC.arbitrary 
            <*> QC.arbitrary
            <*> QC.arbitrary
            <*> QC.arbitrary
            <*> QC.arbitrary


--
-- | Job Output Parameters
--

data JSOutput 
    = JSOutput
        { jsoKey              :: S3Object
        , jsoThumbnailPattern :: T.Text
        , jsoRotate           :: Rotate
        , jsoPresetId         :: PresetId
        }
    deriving (Show,Eq)

instance FromJSON JSOutput where
    parseJSON (Object v) = 
        JSOutput <$>
            v .: "Key"                              <*>
            v .: "ThumbnailPattern"                 <*>
            v .: "Rotate"                           <*>
            v .: "PresetId"
    parseJSON _          = mzero

instance ToJSON JSOutput where
    toJSON jso@(JSOutput _ _ _ _) =
        object 
            [ "Key"             .= jsoKey              jso
            , "ThumbnailPattern".= jsoThumbnailPattern jso
            , "Rotate"          .= jsoRotate           jso
            , "PresetId"        .= jsoPresetId         jso
            ]

instance QC.Arbitrary JSOutput where
    arbitrary = JSOutput 
                    <$> (T.pack <$> QC.arbitrary) 
                    <*> (T.pack <$> QC.arbitrary)
                    <*>             QC.arbitrary
                    <*>             QC.arbitrary


--
-- | Job Output Parameters with Status
--

data JSOutputStatus 
    = JSOutputStatus
        { jsosKey              :: S3Object
        , jsosThumbnailPattern :: Maybe T.Text
        , jsosRotate           :: Rotate
        , jsosPresetId         :: PresetId
        , jsosStatus           :: Status
        , jsosStatusDetail     :: TextOrNull
        }
    deriving (Show,Eq)

instance FromJSON JSOutputStatus where
    parseJSON (Object v) = 
        JSOutputStatus <$>
            v .: "Key"                              <*>
            v .: "ThumbnailPattern"                 <*>
            v .: "Rotate"                           <*>
            v .: "PresetId"                         <*>
            v .: "Status"                           <*>
            v .: "StatusDetail"
    parseJSON _          = mzero

instance ToJSON JSOutputStatus where
    toJSON jsos@(JSOutputStatus _ _ _ _ _ _) =
        object 
            [ "Key"             .= jsosKey               jsos
            , "ThumbnailPattern".= jsosThumbnailPattern  jsos
            , "Rotate"          .= jsosRotate            jsos
            , "PresetId"        .= jsosPresetId          jsos
            , "Status"          .= jsosStatus            jsos
            , "StatusDetail"    .= jsosStatusDetail      jsos
            ]

instance QC.Arbitrary JSOutputStatus where
    arbitrary = JSOutputStatus 
                    <$> (     T.pack <$> QC.arbitrary) 
                    <*> (fmap T.pack <$> QC.arbitrary)
                    <*>                  QC.arbitrary
                    <*>                  QC.arbitrary
                    <*>                  QC.arbitrary
                    <*>                  QC.arbitrary


--
-- | Input Frame Rate
--

data FrameRate
    = FRauto
    | FR10
    | FR15
    | FR23_97
    | FR24
    | FR25
    | FR29_97
    | FR30
    | FR60
    deriving (Show,Eq,Ord,Bounded,Enum)

framerate_t :: FrameRate -> T.Text
framerate_t fr =
        case fr of
          FRauto    -> "auto"
          FR10      -> "10" 
          FR15      -> "15"
          FR23_97   -> "23.97"
          FR24      -> "24"
          FR25      -> "25"
          FR29_97   -> "29.97"
          FR30      -> "30"
          FR60      -> "60"

framerate_m :: Map.Map T.Text FrameRate
framerate_m = text_map framerate_t
    
instance FromJSON FrameRate where
    parseJSON = json_str_map_p framerate_m

instance ToJSON FrameRate where
    toJSON = String . framerate_t

instance QC.Arbitrary FrameRate where
    arbitrary = QC.elements [minBound..maxBound]


--
-- | Input Resolution
--

data Resolution
    = Rauto
    | Rpixels (Int,Int)
    deriving (Show,Eq)

resolution_t :: Resolution -> T.Text
resolution_t fr =
    case fr of
      Rauto         -> "auto"
      Rpixels (w,h) -> T.pack $ printf "%dx%d" w h

instance FromJSON Resolution where
    parseJSON = withText "Resolution" $ parse_res . T.unpack

instance ToJSON Resolution where
    toJSON = String . resolution_t

instance QC.Arbitrary Resolution where
    arbitrary = inj <$> poss nat_pair
          where
            inj Nothing  = Rauto
            inj (Just p) = Rpixels p

parse_res :: String -> Parser Resolution
parse_res "auto" = return Rauto
parse_res s      = maybe err return $
     do [ws,hs] <- matchRegex res_re s
        w       <- readMay ws 
        h       <- readMay hs
        return $ Rpixels (w,h)
      where
        err = typeMismatch "resolution" $ toJSON s

res_re :: Regex
res_re = mkRegex "([0-9]+)[xX]([0-9]+)"


--
-- | Input Aspect Ratio
--

data AspectRatio
    = ARauto
    | AR1_1
    | AR4_3
    | AR3_2
    | AR16_9
    deriving (Show,Eq,Ord,Bounded,Enum)

aspectratio_t :: AspectRatio -> T.Text
aspectratio_t fr =
        case fr of
          ARauto    -> "auto"
          AR1_1     -> "1:1"
          AR4_3     -> "4:3"
          AR3_2     -> "3:2"
          AR16_9    -> "16:9"

aspectratio_m :: Map.Map T.Text AspectRatio
aspectratio_m = text_map aspectratio_t
    
instance FromJSON AspectRatio where
    parseJSON = json_str_map_p aspectratio_m

instance ToJSON AspectRatio where
    toJSON = String . aspectratio_t

instance QC.Arbitrary AspectRatio where
    arbitrary = QC.elements [minBound..maxBound]



--
-- | Input Container Type
--

data Container
    = Cauto
    | C3gp
    | Casf
    | Cavi
    | Cdivx
    | Cflv
    | Cmkv
    | Cmov
    | Cmp4
    | Cmpeg
    | Cmpeg_ps
    | Cmpeg_ts
    | Cmxf
    | Cogg
    | Cvob
    | Cwav
    | Cwebm
    deriving (Show,Eq,Ord,Bounded,Enum)

container_t :: Container -> T.Text
container_t fr =
    case fr of
      Cauto     -> "auto"
      C3gp      -> "3gp"
      Casf      -> "asf"
      Cavi      -> "avi"
      Cdivx     -> "divx"
      Cflv      -> "flv"
      Cmkv      -> "mkv"
      Cmov      -> "mov"
      Cmp4      -> "mp4"
      Cmpeg     -> "mpeg"
      Cmpeg_ps  -> "mpeg-ps"
      Cmpeg_ts  -> "mpeg-ts"
      Cmxf      -> "mxf"
      Cogg      -> "ogg"
      Cvob      -> "vob"
      Cwav      -> "wav"
      Cwebm     -> "webm"

container_m :: Map.Map T.Text Container
container_m = text_map container_t
    
instance FromJSON Container where
    parseJSON = json_str_map_p container_m

instance ToJSON Container where
    toJSON = String . container_t

instance QC.Arbitrary Container where
    arbitrary = QC.elements [minBound..maxBound]


--
-- | Output Rotation
--

data Rotate
    = ROTauto
    | ROT0
    | ROT90
    | ROT180
    | ROT270
    deriving (Show,Eq,Ord,Bounded,Enum)

rotate_t :: Rotate -> T.Text
rotate_t rot =
        case rot of
          ROTauto   -> "auto"
          ROT0      -> "0"
          ROT90     -> "90"
          ROT180    -> "180"
          ROT270    -> "270"

rotate_m :: Map.Map T.Text Rotate
rotate_m = text_map rotate_t

instance FromJSON Rotate where
    parseJSON = json_str_map_p rotate_m

instance ToJSON Rotate where
    toJSON = String . rotate_t

instance QC.Arbitrary Rotate where
    arbitrary = QC.elements [minBound..maxBound]


--
-- | Job Status
--

data Status
    = STSSubmitted
    | STSProgressing
    | STSComplete
    | STSCancelled
    | STSError
    deriving (Show,Eq,Ord,Bounded,Enum)

status_t :: Status -> T.Text
status_t sts =
    case sts of
      STSSubmitted   -> "Submitted"
      STSProgressing -> "Progressing"
      STSComplete    -> "Complete"
      STSCancelled   -> "Canceled"
      STSError       -> "Error"

status_m :: Map.Map T.Text Status
status_m = text_map status_t

instance FromJSON Status where
    parseJSON = json_str_map_p status_m

instance ToJSON Status where
    toJSON = String . status_t

instance QC.Arbitrary Status where
    arbitrary = QC.elements [minBound..maxBound]


--
-- | PipelineName
--

newtype PipelineName = PNM { _PNM :: T.Text }
    deriving (Show,IsString,Eq)

instance FromJSON PipelineName where
    parseJSON = withText "PipelineName" $ return . PNM

instance ToJSON PipelineName where
    toJSON = String . _PNM

instance QC.Arbitrary PipelineName where
    arbitrary = PNM . T.pack <$> QC.arbitrary


--
-- | PipelineId
--

newtype PipelineId = PID { _PID :: T.Text }
    deriving (Show,IsString,Eq)

instance FromJSON PipelineId where
    parseJSON = withText "PipelineId" $ return . PID

instance ToJSON PipelineId where
    toJSON = String . _PID

instance QC.Arbitrary PipelineId where
    arbitrary = PID . T.pack <$> QC.arbitrary


--
-- | Pipeline
--

data Pipeline
    = PLN
        { plnName          :: PipelineName
        , plnInputBucket   :: S3Object
        , plnOutputBucket  :: S3Object
        , plnRole          :: IAMRole
        , plnNotifications :: Notifications
        }
    deriving (Show,Eq)

instance FromJSON Pipeline where
    parseJSON (Object v) = 
        PLN <$>
            v .: "Name"                             <*>
            v .: "InputBucket"                      <*>
            v .: "OutputBucket"                     <*>
            v .: "Role"                             <*>
            v .: "Notifications"
    parseJSON _          = mzero

instance ToJSON Pipeline where
    toJSON pln@(PLN _ _ _ _ _) =
        object 
            [ "Name"          .= plnName          pln
            , "InputBucket"   .= plnInputBucket   pln
            , "OutputBucket"  .= plnOutputBucket  pln
            , "Role"          .= plnRole          pln
            , "Notifications" .= plnNotifications pln
            ]

instance QC.Arbitrary Pipeline where
    arbitrary = PLN 
                    <$>             QC.arbitrary 
                    <*> (T.pack <$> QC.arbitrary)
                    <*> (T.pack <$> QC.arbitrary)
                    <*> (T.pack <$> QC.arbitrary)
                    <*>             QC.arbitrary


--
-- | Pipeline Single
--

newtype PipelineSingle
    = PipelineSingle
        { psPipeline :: PipelineIdStatus
        }
    deriving (Show,Eq)

instance FromJSON PipelineSingle where
     parseJSON (Object v) = 
        PipelineSingle <$>
            v .: "Pipeline"
     parseJSON _          = mzero

instance ToJSON PipelineSingle where
     toJSON js =
        object 
            [ "Pipeline" .= psPipeline js
            ]

instance QC.Arbitrary PipelineSingle where
    arbitrary = PipelineSingle <$> QC.arbitrary


--
-- | Pipeline List
--

data PipelineList
    = PipelineList
        { plPipelines     :: [PipelineIdStatus]
        }
    deriving (Show,Eq)

instance FromJSON PipelineList where
     parseJSON (Object v) = 
        PipelineList <$>
            v .: "Pipelines"
     parseJSON _          = mzero

instance ToJSON PipelineList where
     toJSON ps@(PipelineList _) =
        object 
            [ "Pipelines"     .= plPipelines     ps
            ]

instance QC.Arbitrary PipelineList where
    arbitrary = PipelineList <$> QC.arbitrary


--
-- | PipelineIdStatus
--

data PipelineIdStatus
    = PIS
        { pisName          :: PipelineName
        , pisInputBucket   :: S3Object
        , pisOutputBucket  :: S3Object
        , pisRole          :: IAMRole
        , pisNotifications :: Notifications
        , pisId            :: PipelineId
        , pisStatus        :: PipelineStatus
        }
    deriving (Show,Eq)

instance FromJSON PipelineIdStatus where
    parseJSON (Object v) = 
        PIS <$>
            v .: "Name"                             <*>
            v .: "InputBucket"                      <*>
            v .: "OutputBucket"                     <*>
            v .: "Role"                             <*>
            v .: "Notifications"                    <*>
            v .: "Id"                               <*>
            v .: "Status"
    parseJSON _          = mzero

instance ToJSON PipelineIdStatus where
    toJSON pis@(PIS _ _ _ _ _ _ _) =
        object 
            [ "Name"          .= pisName          pis
            , "InputBucket"   .= pisInputBucket   pis
            , "OutputBucket"  .= pisOutputBucket  pis
            , "Role"          .= pisRole          pis
            , "Notifications" .= pisNotifications pis
            , "Id"            .= pisId            pis
            , "Status"        .= pisStatus        pis
            ]

instance QC.Arbitrary PipelineIdStatus where
    arbitrary = PIS 
                    <$>             QC.arbitrary 
                    <*> (T.pack <$> QC.arbitrary)
                    <*> (T.pack <$> QC.arbitrary)
                    <*> (T.pack <$> QC.arbitrary)
                    <*>             QC.arbitrary
                    <*>             QC.arbitrary
                    <*> (T.pack <$> QC.arbitrary)


--
-- | Notifications
--

data Notifications
    = NTF
        { ntfCompleted    :: SNSTopic
        , ntfError        :: SNSTopic
        , ntfProgressing  :: SNSTopic
        , ntfWarning      :: SNSTopic
        }
    deriving (Show,Eq)

instance FromJSON Notifications where
    parseJSON (Object v) = 
        NTF <$>
            v .: "Completed"                        <*>
            v .: "Error"                            <*>
            v .: "Progressing"                      <*>
            v .: "Warning"
    parseJSON _          = mzero

instance ToJSON Notifications where
    toJSON ntf@(NTF _ _ _ _) =
        object 
            [ "Completed"       .= ntfCompleted        ntf
            , "Error"           .= ntfError            ntf
            , "Progressing"     .= ntfProgressing      ntf
            , "Warning"         .= ntfWarning          ntf
            ]

instance QC.Arbitrary Notifications where
    arbitrary = NTF <$> (T.pack <$> QC.arbitrary) 
                    <*> (T.pack <$> QC.arbitrary)
                    <*> (T.pack <$> QC.arbitrary)
                    <*> (T.pack <$> QC.arbitrary)


--
-- | PipelineStatus
--

-- Documentation is disturbingly vague on the values this type can
-- take so we represent it with Text

type PipelineStatus = T.Text

pipelineStatusActive :: PipelineStatus
pipelineStatusActive = "active"

pipelineStatusPaused :: PipelineStatus
pipelineStatusPaused = "paused"


--
-- | PipelineIdAndStatus
--

data PipelineIdAndStatus
    = PAS
        { pasId     :: PipelineId
        , pasStatus :: PipelineStatus
        }
    deriving (Show,Eq)

instance FromJSON PipelineIdAndStatus where
    parseJSON (Object v) = 
        PAS <$>
            v .: "Id"                               <*>
            v .: "Status"
    parseJSON _          = mzero

instance ToJSON PipelineIdAndStatus where
    toJSON pas@(PAS _ _) =
        object 
            [ "Id"              .= pasId     pas
            , "Status"          .= pasStatus pas
            ]

instance QC.Arbitrary PipelineIdAndStatus where
    arbitrary = PAS <$>             QC.arbitrary 
                    <*> (T.pack <$> QC.arbitrary)


--
-- | 'auto', 'true' or 'false'
--

data AutoBool
    = ABauto    
    | ABtrue
    | ABfalse
    deriving (Show,Eq,Ord,Bounded,Enum)

autobool_t :: AutoBool -> T.Text
autobool_t rot =
    case rot of
      ABauto    -> "auto"
      ABtrue    -> "true"
      ABfalse   -> "false"

autobool_m :: Map.Map T.Text AutoBool
autobool_m = text_map autobool_t

instance FromJSON AutoBool where
    parseJSON = json_str_map_p autobool_m

instance ToJSON AutoBool where
    toJSON = String . autobool_t

instance QC.Arbitrary AutoBool where
    arbitrary = QC.elements [minBound..maxBound]



--
-- | Text or Null
--

data TextOrNull
    = TNText T.Text
    | TNNull
    deriving (Show,Eq) 

instance IsString TextOrNull where
    fromString = TNText . T.pack

instance FromJSON TextOrNull where
    parseJSON Null       = return   TNNull 
    parseJSON (String t) = return $ TNText t
    parseJSON _          = mzero

instance ToJSON TextOrNull where
    toJSON  TNNull    = Null
    toJSON (TNText t) = String t

instance QC.Arbitrary TextOrNull where
    arbitrary = maybe TNNull TNText <$> (poss $ T.pack <$> QC.arbitrary)


--
-- | Ets error message
--

newtype EtsServiceError = ESE { _ESE :: T.Text }
    deriving (Show,IsString,Eq)

instance FromJSON EtsServiceError where
    parseJSON (Object v) = ESE <$> v .: "message"
    parseJSON _          = mzero

instance ToJSON EtsServiceError where
    toJSON (ESE msg) =
        object 
            [ "message" .= msg
            ]

instance QC.Arbitrary EtsServiceError where
    arbitrary = ESE . T.pack <$> QC.arbitrary



--
-- | 'success'
--

newtype SUCCESS = SUCCESS { _SUCCESS :: Bool }
    deriving (Show,Eq)

instance FromJSON SUCCESS where
    parseJSON (Object v) = SUCCESS <$> v .: "success"
    parseJSON _          = mzero

instance ToJSON SUCCESS where
    toJSON = Bool . _SUCCESS

instance QC.Arbitrary SUCCESS where
    arbitrary = SUCCESS <$> QC.arbitrary



--
-- | 'true', 'false'
--


bool_t :: Bool -> T.Text
bool_t True  = "true"
bool_t False = "false"



------------------------------------------------------------------------------
--
-- Parser Toolkit
--
------------------------------------------------------------------------------


json_str_map_p :: Ord a => Map.Map T.Text a -> Value -> Parser a
json_str_map_p mp = json_string_p $ flip Map.lookup mp 

json_string_p :: Ord a => (T.Text->Maybe a) -> Value -> Parser a
json_string_p p (String t) | Just val <- p t = return val
                           | otherwise       = mzero
json_string_p _  _                           = mzero

text_map :: (Ord a,Bounded a,Enum a) => (a->T.Text) -> Map.Map T.Text a
text_map f = Map.fromList [ (f x,x) | x<-[minBound..maxBound] ]



------------------------------------------------------------------------------
--
-- QC Toolkit
--
------------------------------------------------------------------------------


poss :: QC.Gen a -> QC.Gen (Maybe a)
poss gen = QC.frequency 
    [ (,) 1  $ QC.elements [Nothing]
    , (,) 20 $ Just <$> gen
    ]

nat_pair :: QC.Gen (Int,Int)
nat_pair = two $ QC.sized $ \n -> QC.choose (0, n)

two :: QC.Gen a -> QC.Gen (a,a)
two gen = (,) <$> gen <*> gen
