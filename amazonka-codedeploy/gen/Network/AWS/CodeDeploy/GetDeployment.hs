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
-- Module      : Network.AWS.CodeDeploy.GetDeployment
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a deployment.
--
-- /See:/ <http://docs.aws.amazon.com/codedeploy/latest/APIReference/API_GetDeployment.html AWS API Reference> for GetDeployment.
module Network.AWS.CodeDeploy.GetDeployment
    (
    -- * Creating a Request
      getDeployment
    , GetDeployment
    -- * Request Lenses
    , gdDeploymentId

    -- * Destructuring the Response
    , getDeploymentResponse
    , GetDeploymentResponse
    -- * Response Lenses
    , gdrsDeploymentInfo
    , gdrsResponseStatus
    ) where

import           Network.AWS.CodeDeploy.Types
import           Network.AWS.CodeDeploy.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a get deployment operation.
--
-- /See:/ 'getDeployment' smart constructor.
newtype GetDeployment = GetDeployment'
    { _gdDeploymentId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetDeployment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdDeploymentId'
getDeployment
    :: Text -- ^ 'gdDeploymentId'
    -> GetDeployment
getDeployment pDeploymentId_ =
    GetDeployment'
    { _gdDeploymentId = pDeploymentId_
    }

-- | An existing deployment ID associated with the applicable IAM user or AWS
-- account.
gdDeploymentId :: Lens' GetDeployment Text
gdDeploymentId = lens _gdDeploymentId (\ s a -> s{_gdDeploymentId = a});

instance AWSRequest GetDeployment where
        type Rs GetDeployment = GetDeploymentResponse
        request = postJSON codeDeploy
        response
          = receiveJSON
              (\ s h x ->
                 GetDeploymentResponse' <$>
                   (x .?> "deploymentInfo") <*> (pure (fromEnum s)))

instance ToHeaders GetDeployment where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeDeploy_20141006.GetDeployment" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetDeployment where
        toJSON GetDeployment'{..}
          = object
              (catMaybes
                 [Just ("deploymentId" .= _gdDeploymentId)])

instance ToPath GetDeployment where
        toPath = const "/"

instance ToQuery GetDeployment where
        toQuery = const mempty

-- | Represents the output of a get deployment operation.
--
-- /See:/ 'getDeploymentResponse' smart constructor.
data GetDeploymentResponse = GetDeploymentResponse'
    { _gdrsDeploymentInfo :: !(Maybe DeploymentInfo)
    , _gdrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetDeploymentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdrsDeploymentInfo'
--
-- * 'gdrsResponseStatus'
getDeploymentResponse
    :: Int -- ^ 'gdrsResponseStatus'
    -> GetDeploymentResponse
getDeploymentResponse pResponseStatus_ =
    GetDeploymentResponse'
    { _gdrsDeploymentInfo = Nothing
    , _gdrsResponseStatus = pResponseStatus_
    }

-- | Information about the deployment.
gdrsDeploymentInfo :: Lens' GetDeploymentResponse (Maybe DeploymentInfo)
gdrsDeploymentInfo = lens _gdrsDeploymentInfo (\ s a -> s{_gdrsDeploymentInfo = a});

-- | The response status code.
gdrsResponseStatus :: Lens' GetDeploymentResponse Int
gdrsResponseStatus = lens _gdrsResponseStatus (\ s a -> s{_gdrsResponseStatus = a});
