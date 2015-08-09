{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.DeactivatePipeline
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deactivates the specified running pipeline. The pipeline is set to the
-- @DEACTIVATING@ state until the deactivation process completes.
--
-- To resume a deactivated pipeline, use ActivatePipeline. By default, the
-- pipeline resumes from the last completed execution. Optionally, you can
-- specify the date and time to resume the pipeline.
--
-- /See:/ <http://docs.aws.amazon.com/datapipeline/latest/APIReference/API_DeactivatePipeline.html AWS API Reference> for DeactivatePipeline.
module Network.AWS.DataPipeline.DeactivatePipeline
    (
    -- * Creating a Request
      DeactivatePipeline
    , deactivatePipeline
    -- * Request Lenses
    , dCancelActive
    , dPipelineId

    -- * Destructuring the Response
    , DeactivatePipelineResponse
    , deactivatePipelineResponse
    -- * Response Lenses
    , drsStatus
    ) where

import           Network.AWS.DataPipeline.Types
import           Network.AWS.DataPipeline.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the parameters for DeactivatePipeline.
--
-- /See:/ 'deactivatePipeline' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dCancelActive'
--
-- * 'dPipelineId'
data DeactivatePipeline = DeactivatePipeline'
    { _dCancelActive :: !(Maybe Bool)
    , _dPipelineId   :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeactivatePipeline' smart constructor.
deactivatePipeline :: Text -> DeactivatePipeline
deactivatePipeline pPipelineId_ =
    DeactivatePipeline'
    { _dCancelActive = Nothing
    , _dPipelineId = pPipelineId_
    }

-- | Indicates whether to cancel any running objects. The default is true,
-- which sets the state of any running objects to @CANCELED@. If this value
-- is false, the pipeline is deactivated after all running objects finish.
dCancelActive :: Lens' DeactivatePipeline (Maybe Bool)
dCancelActive = lens _dCancelActive (\ s a -> s{_dCancelActive = a});

-- | The ID of the pipeline.
dPipelineId :: Lens' DeactivatePipeline Text
dPipelineId = lens _dPipelineId (\ s a -> s{_dPipelineId = a});

instance AWSRequest DeactivatePipeline where
        type Sv DeactivatePipeline = DataPipeline
        type Rs DeactivatePipeline =
             DeactivatePipelineResponse
        request = postJSON
        response
          = receiveEmpty
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
              ["cancelActive" .= _dCancelActive,
               "pipelineId" .= _dPipelineId]

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
-- * 'drsStatus'
newtype DeactivatePipelineResponse = DeactivatePipelineResponse'
    { _drsStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeactivatePipelineResponse' smart constructor.
deactivatePipelineResponse :: Int -> DeactivatePipelineResponse
deactivatePipelineResponse pStatus_ =
    DeactivatePipelineResponse'
    { _drsStatus = pStatus_
    }

-- | Undocumented member.
drsStatus :: Lens' DeactivatePipelineResponse Int
drsStatus = lens _drsStatus (\ s a -> s{_drsStatus = a});
