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
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attempts to stop an ongoing deployment.
--
-- /See:/ <http://docs.aws.amazon.com/codedeploy/latest/APIReference/API_StopDeployment.html AWS API Reference> for StopDeployment.
module Network.AWS.CodeDeploy.StopDeployment
    (
    -- * Creating a Request
      stopDeployment
    , StopDeployment
    -- * Request Lenses
    , sdDeploymentId

    -- * Destructuring the Response
    , stopDeploymentResponse
    , StopDeploymentResponse
    -- * Response Lenses
    , sdrsStatusMessage
    , sdrsStatus
    ) where

import           Network.AWS.CodeDeploy.Types
import           Network.AWS.CodeDeploy.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a stop deployment operation.
--
-- /See:/ 'stopDeployment' smart constructor.
newtype StopDeployment = StopDeployment'
    { _sdDeploymentId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'StopDeployment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdDeploymentId'
stopDeployment
    :: Text -- ^ 'sdDeploymentId'
    -> StopDeployment
stopDeployment pDeploymentId_ =
    StopDeployment'
    { _sdDeploymentId = pDeploymentId_
    }

-- | The unique ID of a deployment.
sdDeploymentId :: Lens' StopDeployment Text
sdDeploymentId = lens _sdDeploymentId (\ s a -> s{_sdDeploymentId = a});

instance AWSRequest StopDeployment where
        type Rs StopDeployment = StopDeploymentResponse
        request = postJSON codeDeploy
        response
          = receiveJSON
              (\ s h x ->
                 StopDeploymentResponse' <$>
                   (x .?> "statusMessage") <*> (pure (fromEnum s)))

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
                 [Just ("deploymentId" .= _sdDeploymentId)])

instance ToPath StopDeployment where
        toPath = const "/"

instance ToQuery StopDeployment where
        toQuery = const mempty

-- | Represents the output of a stop deployment operation.
--
-- /See:/ 'stopDeploymentResponse' smart constructor.
data StopDeploymentResponse = StopDeploymentResponse'
    { _sdrsStatusMessage :: !(Maybe Text)
    , _sdrsStatus        :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'StopDeploymentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdrsStatusMessage'
--
-- * 'sdrsStatus'
stopDeploymentResponse
    :: Int -- ^ 'sdrsStatus'
    -> StopDeploymentResponse
stopDeploymentResponse pStatus_ =
    StopDeploymentResponse'
    { _sdrsStatusMessage = Nothing
    , _sdrsStatus = pStatus_
    }

-- | An accompanying status message.
sdrsStatusMessage :: Lens' StopDeploymentResponse (Maybe Text)
sdrsStatusMessage = lens _sdrsStatusMessage (\ s a -> s{_sdrsStatusMessage = a});

-- | The response status code.
sdrsStatus :: Lens' StopDeploymentResponse Int
sdrsStatus = lens _sdrsStatus (\ s a -> s{_sdrsStatus = a});
