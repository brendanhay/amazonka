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
-- Module      : Network.AWS.CodeDeploy.StopDeployment
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attempts to stop an ongoing deployment.
--
--
module Network.AWS.CodeDeploy.StopDeployment
    (
    -- * Creating a Request
      stopDeployment
    , StopDeployment
    -- * Request Lenses
    , sdAutoRollbackEnabled
    , sdDeploymentId

    -- * Destructuring the Response
    , stopDeploymentResponse
    , StopDeploymentResponse
    -- * Response Lenses
    , sdrsStatus
    , sdrsStatusMessage
    , sdrsResponseStatus
    ) where

import Network.AWS.CodeDeploy.Types
import Network.AWS.CodeDeploy.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a StopDeployment operation.
--
--
--
-- /See:/ 'stopDeployment' smart constructor.
data StopDeployment = StopDeployment'
  { _sdAutoRollbackEnabled :: !(Maybe Bool)
  , _sdDeploymentId        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopDeployment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdAutoRollbackEnabled' - Indicates, when a deployment is stopped, whether instances that have been updated should be rolled back to the previous version of the application revision.
--
-- * 'sdDeploymentId' - The unique ID of a deployment.
stopDeployment
    :: Text -- ^ 'sdDeploymentId'
    -> StopDeployment
stopDeployment pDeploymentId_ =
  StopDeployment'
    {_sdAutoRollbackEnabled = Nothing, _sdDeploymentId = pDeploymentId_}


-- | Indicates, when a deployment is stopped, whether instances that have been updated should be rolled back to the previous version of the application revision.
sdAutoRollbackEnabled :: Lens' StopDeployment (Maybe Bool)
sdAutoRollbackEnabled = lens _sdAutoRollbackEnabled (\ s a -> s{_sdAutoRollbackEnabled = a})

-- | The unique ID of a deployment.
sdDeploymentId :: Lens' StopDeployment Text
sdDeploymentId = lens _sdDeploymentId (\ s a -> s{_sdDeploymentId = a})

instance AWSRequest StopDeployment where
        type Rs StopDeployment = StopDeploymentResponse
        request = postJSON codeDeploy
        response
          = receiveJSON
              (\ s h x ->
                 StopDeploymentResponse' <$>
                   (x .?> "status") <*> (x .?> "statusMessage") <*>
                     (pure (fromEnum s)))

instance Hashable StopDeployment where

instance NFData StopDeployment where

instance ToHeaders StopDeployment where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeDeploy_20141006.StopDeployment" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StopDeployment where
        toJSON StopDeployment'{..}
          = object
              (catMaybes
                 [("autoRollbackEnabled" .=) <$>
                    _sdAutoRollbackEnabled,
                  Just ("deploymentId" .= _sdDeploymentId)])

instance ToPath StopDeployment where
        toPath = const "/"

instance ToQuery StopDeployment where
        toQuery = const mempty

-- | Represents the output of a StopDeployment operation.
--
--
--
-- /See:/ 'stopDeploymentResponse' smart constructor.
data StopDeploymentResponse = StopDeploymentResponse'
  { _sdrsStatus         :: !(Maybe StopStatus)
  , _sdrsStatusMessage  :: !(Maybe Text)
  , _sdrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopDeploymentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdrsStatus' - The status of the stop deployment operation:     * Pending: The stop operation is pending.     * Succeeded: The stop operation was successful.
--
-- * 'sdrsStatusMessage' - An accompanying status message.
--
-- * 'sdrsResponseStatus' - -- | The response status code.
stopDeploymentResponse
    :: Int -- ^ 'sdrsResponseStatus'
    -> StopDeploymentResponse
stopDeploymentResponse pResponseStatus_ =
  StopDeploymentResponse'
    { _sdrsStatus = Nothing
    , _sdrsStatusMessage = Nothing
    , _sdrsResponseStatus = pResponseStatus_
    }


-- | The status of the stop deployment operation:     * Pending: The stop operation is pending.     * Succeeded: The stop operation was successful.
sdrsStatus :: Lens' StopDeploymentResponse (Maybe StopStatus)
sdrsStatus = lens _sdrsStatus (\ s a -> s{_sdrsStatus = a})

-- | An accompanying status message.
sdrsStatusMessage :: Lens' StopDeploymentResponse (Maybe Text)
sdrsStatusMessage = lens _sdrsStatusMessage (\ s a -> s{_sdrsStatusMessage = a})

-- | -- | The response status code.
sdrsResponseStatus :: Lens' StopDeploymentResponse Int
sdrsResponseStatus = lens _sdrsResponseStatus (\ s a -> s{_sdrsResponseStatus = a})

instance NFData StopDeploymentResponse where
