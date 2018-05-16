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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deactivates the specified running pipeline. The pipeline is set to the @DEACTIVATING@ state until the deactivation process completes.
--
--
-- To resume a deactivated pipeline, use 'ActivatePipeline' . By default, the pipeline resumes from the last completed execution. Optionally, you can specify the date and time to resume the pipeline.
--
module Network.AWS.DataPipeline.DeactivatePipeline
    (
    -- * Creating a Request
      deactivatePipeline
    , DeactivatePipeline
    -- * Request Lenses
    , dCancelActive
    , dPipelineId

    -- * Destructuring the Response
    , deactivatePipelineResponse
    , DeactivatePipelineResponse
    -- * Response Lenses
    , drsResponseStatus
    ) where

import Network.AWS.DataPipeline.Types
import Network.AWS.DataPipeline.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for DeactivatePipeline.
--
--
--
-- /See:/ 'deactivatePipeline' smart constructor.
data DeactivatePipeline = DeactivatePipeline'
  { _dCancelActive :: !(Maybe Bool)
  , _dPipelineId   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeactivatePipeline' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dCancelActive' - Indicates whether to cancel any running objects. The default is true, which sets the state of any running objects to @CANCELED@ . If this value is false, the pipeline is deactivated after all running objects finish.
--
-- * 'dPipelineId' - The ID of the pipeline.
deactivatePipeline
    :: Text -- ^ 'dPipelineId'
    -> DeactivatePipeline
deactivatePipeline pPipelineId_ =
  DeactivatePipeline' {_dCancelActive = Nothing, _dPipelineId = pPipelineId_}


-- | Indicates whether to cancel any running objects. The default is true, which sets the state of any running objects to @CANCELED@ . If this value is false, the pipeline is deactivated after all running objects finish.
dCancelActive :: Lens' DeactivatePipeline (Maybe Bool)
dCancelActive = lens _dCancelActive (\ s a -> s{_dCancelActive = a})

-- | The ID of the pipeline.
dPipelineId :: Lens' DeactivatePipeline Text
dPipelineId = lens _dPipelineId (\ s a -> s{_dPipelineId = a})

instance AWSRequest DeactivatePipeline where
        type Rs DeactivatePipeline =
             DeactivatePipelineResponse
        request = postJSON dataPipeline
        response
          = receiveEmpty
              (\ s h x ->
                 DeactivatePipelineResponse' <$> (pure (fromEnum s)))

instance Hashable DeactivatePipeline where

instance NFData DeactivatePipeline where

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
              (catMaybes
                 [("cancelActive" .=) <$> _dCancelActive,
                  Just ("pipelineId" .= _dPipelineId)])

instance ToPath DeactivatePipeline where
        toPath = const "/"

instance ToQuery DeactivatePipeline where
        toQuery = const mempty

-- | Contains the output of DeactivatePipeline.
--
--
--
-- /See:/ 'deactivatePipelineResponse' smart constructor.
newtype DeactivatePipelineResponse = DeactivatePipelineResponse'
  { _drsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeactivatePipelineResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsResponseStatus' - -- | The response status code.
deactivatePipelineResponse
    :: Int -- ^ 'drsResponseStatus'
    -> DeactivatePipelineResponse
deactivatePipelineResponse pResponseStatus_ =
  DeactivatePipelineResponse' {_drsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
drsResponseStatus :: Lens' DeactivatePipelineResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a})

instance NFData DeactivatePipelineResponse where
