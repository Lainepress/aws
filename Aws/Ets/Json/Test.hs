module Aws.Ets.Json.Test
    ( tests
    , testAll
    , testAllVerbose
    ) where

import           Aws.Ets.Json.Types
import           Text.Printf
import           Data.Aeson
import qualified Test.QuickCheck                as QC
import qualified Distribution.TestSuite         as TS


-- | Detailed-0.9/Cabal-1.14.0 test suite:

tests :: [TS.Test] 
tests = map TS.impure simple_tests


-- | Something to run in ghci:

testAll :: IO ()
testAll = mapM_ part simple_tests
      where
        part st = 
             do putStr $ printf "%-25s: " $ nameST st
                testST st False


-- | Verbose tests from ghci:

testAllVerbose :: IO ()
testAllVerbose = mapM_ part simple_tests
      where
        part st = 
             do putStr $ printf "\n%s:\n" $ nameST st
                testST st True


-- Our list of tests:

simple_tests :: [SimpleTest]
simple_tests =
        [ ae (mk_aet "JobId"            :: AETest JobId          )
        , ae (mk_aet "PipelineId"       :: AETest PipelineId     )
        , ae (mk_aet "PresetId"         :: AETest PresetId       )
        , ae (mk_aet "JobSpec"          :: AETest JobSpec        )
        , ae (mk_aet "JobSpecId"        :: AETest JobSpecId      )
        , ae (mk_aet "JSInput"          :: AETest JSInput        )
        , ae (mk_aet "JSOutput"         :: AETest JSOutput       )
        , ae (mk_aet "JSOutputStatus"   :: AETest JSOutputStatus )
        , ae (mk_aet "FrameRate"        :: AETest FrameRate      )
        , ae (mk_aet "Resolution"       :: AETest Resolution     )
        , ae (mk_aet "AspectRatio"      :: AETest AspectRatio    )
        , ae (mk_aet "Container"        :: AETest Container      )
        , ae (mk_aet "Rotate"           :: AETest Rotate         )
        , ae (mk_aet "Status"           :: AETest Status         )
        , ae (mk_aet "AutoBool"         :: AETest AutoBool       )
        , ae (mk_aet "TextOrNull"       :: AETest TextOrNull     )
        , ae (mk_aet "EtsServiceError"  :: AETest EtsServiceError)
        ]
      where
        ae (AET st) = st


-- Our simple QC tests and instances to insert into the Cabal framework.

data SimpleTest = ST
    { nameST :: String
    , testST :: Bool -> IO QC.Result
    }

instance TS.TestOptions SimpleTest where
    name           = nameST
    options        = const []
    defaultOptions = const $ return $ TS.Options []
    check          = const $ const $ return []

instance TS.ImpureTestable SimpleTest where
    runM st _ = cnv_result `fmap` testST st False

cnv_result :: QC.Result -> TS.Result
cnv_result qcr =
        case qcr of
          QC.Success _ _ _ -> TS.Pass
          _                -> TS.Fail "Failed"

-- AETest adds a phantom index to SimpleTest as a convenience
-- to simplify usage of mk_aet, the generator for the
-- aeson-round-trip tests.

newtype AETest a = AET SimpleTest

-- Check aeson decode is an inverse of encode, namely that:
--
--       decode (encode [x]) == [x]
--
-- N.B. x rwapped in list as JSON not good for encoding simple
--      data types like strings and numbers and aeson strictly
--      adheres to the standard.

mk_aet :: (QC.Arbitrary a,Show a,FromJSON a,ToJSON a,Eq a) => String -> AETest a
mk_aet nm0 = tst $ \x -> maybe False (==[x]) $ decode $ encode [x]
      where
        nm     = "aeson." ++ nm0

        tst    :: (QC.Arbitrary a,Show a,FromJSON a,ToJSON a,Eq a) => 
                                                        (a->Bool) -> AETest a
        tst p  = AET $ ST nm $ qc p

        qc p v = if v then QC.verboseCheckResult p else QC.quickCheckResult p
