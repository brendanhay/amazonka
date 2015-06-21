{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.DataPipeline.ActivatePipeline
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Validates the specified pipeline and starts processing pipeline tasks.
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
    , apStartTimestamp
    , apParameterValues
    , apPipelineId

    -- * Response
    , ActivatePipelineResponse
    -- ** Response constructor
    , activatePipelineResponse
    ) where

import Network.AWS.DataPipeline.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'activatePipeline' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'apStartTimestamp'
--
-- * 'apParameterValues'
--
-- * 'apPipelineId'
data ActivatePipeline = ActivatePipeline'{_apStartTimestamp :: Maybe POSIX, _apParameterValues :: Maybe [ParameterValue], _apPipelineId :: Text} deriving (Eq, Read, Show)

-- | 'ActivatePipeline' smart constructor.
activatePipeline :: Text -> ActivatePipeline
activatePipeline pPipelineId = ActivatePipeline'{_apStartTimestamp = Nothing, _apParameterValues = Nothing, _apPipelineId = pPipelineId};

-- | The date and time to resume the pipeline. By default, the pipeline
-- resumes from the last completed execution.
apStartTimestamp :: Lens' ActivatePipeline (Maybe UTCTime)
apStartTimestamp = lens _apStartTimestamp (\ s a -> s{_apStartTimestamp = a}) . mapping _Time;

-- | A list of parameter values to pass to the pipeline at activation.
apParameterValues :: Lens' ActivatePipeline [ParameterValue]
apParameterValues = lens _apParameterValues (\ s a -> s{_apParameterValues = a}) . _Default;

-- | The ID of the pipeline.
apPipelineId :: Lens' ActivatePipeline Text
apPipelineId = lens _apPipelineId (\ s a -> s{_apPipelineId = a});

instance AWSRequest ActivatePipeline where
        type Sv ActivatePipeline = DataPipeline
        type Rs ActivatePipeline = ActivatePipelineResponse
        request = postJSON
        response = receiveNull ActivatePipelineResponse'

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
              ["startTimestamp" .= _apStartTimestamp,
               "parameterValues" .= _apParameterValues,
               "pipelineId" .= _apPipelineId]

instance ToPath ActivatePipeline where
        toPath = const "/"

instance ToQuery ActivatePipeline where
        toQuery = const mempty

-- | /See:/ 'activatePipelineResponse' smart constructor.
data ActivatePipelineResponse = ActivatePipelineResponse' deriving (Eq, Read, Show)

-- | 'ActivatePipelineResponse' smart constructor.
activatePipelineResponse :: ActivatePipelineResponse
activatePipelineResponse = ActivatePipelineResponse';
