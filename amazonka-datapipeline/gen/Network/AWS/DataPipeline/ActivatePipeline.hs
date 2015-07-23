{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.ActivatePipeline
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Validates the specified pipeline and starts processing pipeline tasks.
-- If the pipeline does not pass validation, activation fails.
--
-- If you need to pause the pipeline to investigate an issue with a
-- component, such as a data source or script, call DeactivatePipeline.
--
-- To activate a finished pipeline, modify the end date for the pipeline
-- and then activate it.
--
-- <http://docs.aws.amazon.com/datapipeline/latest/APIReference/API_ActivatePipeline.html>
module Network.AWS.DataPipeline.ActivatePipeline
    (
    -- * Request
      ActivatePipeline
    -- ** Request constructor
    , activatePipeline
    -- ** Request lenses
    , aprqStartTimestamp
    , aprqParameterValues
    , aprqPipelineId

    -- * Response
    , ActivatePipelineResponse
    -- ** Response constructor
    , activatePipelineResponse
    -- ** Response lenses
    , aprsStatus
    ) where

import           Network.AWS.DataPipeline.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the parameters for ActivatePipeline.
--
-- /See:/ 'activatePipeline' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aprqStartTimestamp'
--
-- * 'aprqParameterValues'
--
-- * 'aprqPipelineId'
data ActivatePipeline = ActivatePipeline'
    { _aprqStartTimestamp  :: !(Maybe POSIX)
    , _aprqParameterValues :: !(Maybe [ParameterValue])
    , _aprqPipelineId      :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ActivatePipeline' smart constructor.
activatePipeline :: Text -> ActivatePipeline
activatePipeline pPipelineId_ =
    ActivatePipeline'
    { _aprqStartTimestamp = Nothing
    , _aprqParameterValues = Nothing
    , _aprqPipelineId = pPipelineId_
    }

-- | The date and time to resume the pipeline. By default, the pipeline
-- resumes from the last completed execution.
aprqStartTimestamp :: Lens' ActivatePipeline (Maybe UTCTime)
aprqStartTimestamp = lens _aprqStartTimestamp (\ s a -> s{_aprqStartTimestamp = a}) . mapping _Time;

-- | A list of parameter values to pass to the pipeline at activation.
aprqParameterValues :: Lens' ActivatePipeline [ParameterValue]
aprqParameterValues = lens _aprqParameterValues (\ s a -> s{_aprqParameterValues = a}) . _Default;

-- | The ID of the pipeline.
aprqPipelineId :: Lens' ActivatePipeline Text
aprqPipelineId = lens _aprqPipelineId (\ s a -> s{_aprqPipelineId = a});

instance AWSRequest ActivatePipeline where
        type Sv ActivatePipeline = DataPipeline
        type Rs ActivatePipeline = ActivatePipelineResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ActivatePipelineResponse' <$> (pure (fromEnum s)))

instance ToHeaders ActivatePipeline where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DataPipeline.ActivatePipeline" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ActivatePipeline where
        toJSON ActivatePipeline'{..}
          = object
              ["startTimestamp" .= _aprqStartTimestamp,
               "parameterValues" .= _aprqParameterValues,
               "pipelineId" .= _aprqPipelineId]

instance ToPath ActivatePipeline where
        toPath = const "/"

instance ToQuery ActivatePipeline where
        toQuery = const mempty

-- | Contains the output of ActivatePipeline.
--
-- /See:/ 'activatePipelineResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aprsStatus'
newtype ActivatePipelineResponse = ActivatePipelineResponse'
    { _aprsStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ActivatePipelineResponse' smart constructor.
activatePipelineResponse :: Int -> ActivatePipelineResponse
activatePipelineResponse pStatus_ =
    ActivatePipelineResponse'
    { _aprsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
aprsStatus :: Lens' ActivatePipelineResponse Int
aprsStatus = lens _aprsStatus (\ s a -> s{_aprsStatus = a});
