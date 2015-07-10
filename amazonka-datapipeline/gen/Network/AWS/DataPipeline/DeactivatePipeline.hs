{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.DeactivatePipeline
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deactivates the specified running pipeline. The pipeline is set to the
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
    -- ** Response lenses
    , deaStatus
    ) where

import           Network.AWS.DataPipeline.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the parameters for DeactivatePipeline.
--
-- /See:/ 'deactivatePipeline' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'deaCancelActive'
--
-- * 'deaPipelineId'
data DeactivatePipeline = DeactivatePipeline'
    { _deaCancelActive :: !(Maybe Bool)
    , _deaPipelineId   :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeactivatePipeline' smart constructor.
deactivatePipeline :: Text -> DeactivatePipeline
deactivatePipeline pPipelineId =
    DeactivatePipeline'
    { _deaCancelActive = Nothing
    , _deaPipelineId = pPipelineId
    }

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
        response
          = receiveJSON
              (\ s h x ->
                 DeactivatePipelineResponse' <$> (pure (fromEnum s)))

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

-- | Contains the output of DeactivatePipeline.
--
-- /See:/ 'deactivatePipelineResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'deaStatus'
newtype DeactivatePipelineResponse = DeactivatePipelineResponse'
    { _deaStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeactivatePipelineResponse' smart constructor.
deactivatePipelineResponse :: Int -> DeactivatePipelineResponse
deactivatePipelineResponse pStatus =
    DeactivatePipelineResponse'
    { _deaStatus = pStatus
    }

-- | FIXME: Undocumented member.
deaStatus :: Lens' DeactivatePipelineResponse Int
deaStatus = lens _deaStatus (\ s a -> s{_deaStatus = a});
