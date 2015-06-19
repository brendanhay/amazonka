{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.DataPipeline.DeactivatePipeline
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

-- | Deactivates the specified running pipeline. The pipeline is set to the
-- @DEACTIVATING@ state until the deactivation process completes.
--
-- To resume a deactivated pipeline, use ActivatePipeline. By default, the
-- pipeline resumes from the last completed execution. Optionally, you can
-- specify the date and time to resume the pipeline.
--
-- <http://docs.aws.amazon.com/datapipeline/latest/APIReference/API_DeactivatePipeline.html>
module Network.AWS.DataPipeline.DeactivatePipeline
    (
    -- * Request
      DeactivatePipeline
    -- ** Request constructor
    , deactivatePipeline
    -- ** Request lenses
    , deaCancelActive
    , deaPipelineId

    -- * Response
    , DeactivatePipelineResponse
    -- ** Response constructor
    , deactivatePipelineResponse
    ) where

import Network.AWS.DataPipeline.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deactivatePipeline' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'deaCancelActive'
--
-- * 'deaPipelineId'
data DeactivatePipeline = DeactivatePipeline'{_deaCancelActive :: Maybe Bool, _deaPipelineId :: Text} deriving (Eq, Read, Show)

-- | 'DeactivatePipeline' smart constructor.
deactivatePipeline :: Text -> DeactivatePipeline
deactivatePipeline pPipelineId = DeactivatePipeline'{_deaCancelActive = Nothing, _deaPipelineId = pPipelineId};

-- | Indicates whether to cancel any running objects. The default is true,
-- which sets the state of any running objects to @CANCELED@. If this value
-- is false, the pipeline is deactivated after all running objects finish.
deaCancelActive :: Lens' DeactivatePipeline (Maybe Bool)
deaCancelActive = lens _deaCancelActive (\ s a -> s{_deaCancelActive = a});

-- | The ID of the pipeline.
deaPipelineId :: Lens' DeactivatePipeline Text
deaPipelineId = lens _deaPipelineId (\ s a -> s{_deaPipelineId = a});

instance AWSRequest DeactivatePipeline where
        type Sv DeactivatePipeline = DataPipeline
        type Rs DeactivatePipeline =
             DeactivatePipelineResponse
        request = postJSON
        response = receiveNull DeactivatePipelineResponse'

instance ToHeaders DeactivatePipeline where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DataPipeline.DeactivatePipeline" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeactivatePipeline where
        toJSON DeactivatePipeline'{..}
          = object
              ["cancelActive" .= _deaCancelActive,
               "pipelineId" .= _deaPipelineId]

instance ToPath DeactivatePipeline where
        toPath = const "/"

instance ToQuery DeactivatePipeline where
        toQuery = const mempty

-- | /See:/ 'deactivatePipelineResponse' smart constructor.
data DeactivatePipelineResponse = DeactivatePipelineResponse' deriving (Eq, Read, Show)

-- | 'DeactivatePipelineResponse' smart constructor.
deactivatePipelineResponse :: DeactivatePipelineResponse
deactivatePipelineResponse = DeactivatePipelineResponse';
