module Aws.ElasticTranscoder.Commands.UpdatePipelineStatus
    ( UpdatePipelineStatus(..)
    , UpdatePipelineStatusResponse(..)
    ) where

import           Aws.Core
import           Aws.ElasticTranscoder.Core
import           Control.Applicative
import           Data.Aeson


data UpdatePipelineStatus
    = UpdatePipelineStatus
        { upsStatus :: PipelineStatus
        }
    deriving (Show,Eq)

data UpdatePipelineStatusResponse
    = UpdatePipelineStatusResponse
        { uprId      :: PipelineId
        , uprStatus  :: PipelineStatus
        }
    deriving (Show,Eq)

instance SignQuery UpdatePipelineStatus where

    type ServiceConfiguration UpdatePipelineStatus = EtsConfiguration

    signQuery UpdatePipelineStatus{..} = etsSignQuery 
        EtsQuery
            { etsqMethod  = Get
            , etsqRequest = "pipelines"
            , etsqQuery   = []
            , etsqBody    = Just $ object [ "Status" .= upsStatus ]
            }

instance ResponseConsumer UpdatePipelineStatus UpdatePipelineStatusResponse 
                                                                        where

    type ResponseMetadata UpdatePipelineStatusResponse = EtsMetadata

    responseConsumer _ mref = etsResponseConsumer mref $ \rsp ->
                                                    cnv <$> jsonConsumer rsp
          where
            cnv (PAS a b) = UpdatePipelineStatusResponse a b

instance Transaction UpdatePipelineStatus UpdatePipelineStatusResponse

instance AsMemoryResponse UpdatePipelineStatusResponse where

    type MemoryResponse UpdatePipelineStatusResponse = 
                                                UpdatePipelineStatusResponse

    loadToMemory = return
