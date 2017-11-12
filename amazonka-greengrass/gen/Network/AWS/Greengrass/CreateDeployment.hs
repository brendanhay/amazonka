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
-- Module      : Network.AWS.Greengrass.CreateDeployment
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a deployment.
module Network.AWS.Greengrass.CreateDeployment
    (
    -- * Creating a Request
      createDeployment
    , CreateDeployment
    -- * Request Lenses
    , cdDeploymentId
    , cdAmznClientToken
    , cdDeploymentType
    , cdGroupVersionId
    , cdGroupId

    -- * Destructuring the Response
    , createDeploymentResponse
    , CreateDeploymentResponse
    -- * Response Lenses
    , cdrsDeploymentId
    , cdrsDeploymentARN
    , cdrsResponseStatus
    ) where

import Network.AWS.Greengrass.Types
import Network.AWS.Greengrass.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createDeployment' smart constructor.
data CreateDeployment = CreateDeployment'
  { _cdDeploymentId    :: !(Maybe Text)
  , _cdAmznClientToken :: !(Maybe Text)
  , _cdDeploymentType  :: !(Maybe DeploymentType)
  , _cdGroupVersionId  :: !(Maybe Text)
  , _cdGroupId         :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDeployment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdDeploymentId' - Id of the deployment if you wish to redeploy a previous deployment.
--
-- * 'cdAmznClientToken' - The client token used to request idempotent operations.
--
-- * 'cdDeploymentType' - Type of deployment. When used in CreateDeployment, only NewDeployment and Redeployment are valid.
--
-- * 'cdGroupVersionId' - Group Version you wish to deploy.
--
-- * 'cdGroupId' - The unique Id of the AWS Greengrass Group
createDeployment
    :: Text -- ^ 'cdGroupId'
    -> CreateDeployment
createDeployment pGroupId_ =
  CreateDeployment'
  { _cdDeploymentId = Nothing
  , _cdAmznClientToken = Nothing
  , _cdDeploymentType = Nothing
  , _cdGroupVersionId = Nothing
  , _cdGroupId = pGroupId_
  }


-- | Id of the deployment if you wish to redeploy a previous deployment.
cdDeploymentId :: Lens' CreateDeployment (Maybe Text)
cdDeploymentId = lens _cdDeploymentId (\ s a -> s{_cdDeploymentId = a});

-- | The client token used to request idempotent operations.
cdAmznClientToken :: Lens' CreateDeployment (Maybe Text)
cdAmznClientToken = lens _cdAmznClientToken (\ s a -> s{_cdAmznClientToken = a});

-- | Type of deployment. When used in CreateDeployment, only NewDeployment and Redeployment are valid.
cdDeploymentType :: Lens' CreateDeployment (Maybe DeploymentType)
cdDeploymentType = lens _cdDeploymentType (\ s a -> s{_cdDeploymentType = a});

-- | Group Version you wish to deploy.
cdGroupVersionId :: Lens' CreateDeployment (Maybe Text)
cdGroupVersionId = lens _cdGroupVersionId (\ s a -> s{_cdGroupVersionId = a});

-- | The unique Id of the AWS Greengrass Group
cdGroupId :: Lens' CreateDeployment Text
cdGroupId = lens _cdGroupId (\ s a -> s{_cdGroupId = a});

instance AWSRequest CreateDeployment where
        type Rs CreateDeployment = CreateDeploymentResponse
        request = postJSON greengrass
        response
          = receiveJSON
              (\ s h x ->
                 CreateDeploymentResponse' <$>
                   (x .?> "DeploymentId") <*> (x .?> "DeploymentArn")
                     <*> (pure (fromEnum s)))

instance Hashable CreateDeployment where

instance NFData CreateDeployment where

instance ToHeaders CreateDeployment where
        toHeaders CreateDeployment'{..}
          = mconcat
              ["X-Amzn-Client-Token" =# _cdAmznClientToken,
               "Content-Type" =#
                 ("application/x-amz-json-1.1" :: ByteString)]

instance ToJSON CreateDeployment where
        toJSON CreateDeployment'{..}
          = object
              (catMaybes
                 [("DeploymentId" .=) <$> _cdDeploymentId,
                  ("DeploymentType" .=) <$> _cdDeploymentType,
                  ("GroupVersionId" .=) <$> _cdGroupVersionId])

instance ToPath CreateDeployment where
        toPath CreateDeployment'{..}
          = mconcat
              ["/greengrass/groups/", toBS _cdGroupId,
               "/deployments"]

instance ToQuery CreateDeployment where
        toQuery = const mempty

-- | /See:/ 'createDeploymentResponse' smart constructor.
data CreateDeploymentResponse = CreateDeploymentResponse'
  { _cdrsDeploymentId   :: !(Maybe Text)
  , _cdrsDeploymentARN  :: !(Maybe Text)
  , _cdrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDeploymentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdrsDeploymentId' - The id of the deployment.
--
-- * 'cdrsDeploymentARN' - The arn of the deployment.
--
-- * 'cdrsResponseStatus' - -- | The response status code.
createDeploymentResponse
    :: Int -- ^ 'cdrsResponseStatus'
    -> CreateDeploymentResponse
createDeploymentResponse pResponseStatus_ =
  CreateDeploymentResponse'
  { _cdrsDeploymentId = Nothing
  , _cdrsDeploymentARN = Nothing
  , _cdrsResponseStatus = pResponseStatus_
  }


-- | The id of the deployment.
cdrsDeploymentId :: Lens' CreateDeploymentResponse (Maybe Text)
cdrsDeploymentId = lens _cdrsDeploymentId (\ s a -> s{_cdrsDeploymentId = a});

-- | The arn of the deployment.
cdrsDeploymentARN :: Lens' CreateDeploymentResponse (Maybe Text)
cdrsDeploymentARN = lens _cdrsDeploymentARN (\ s a -> s{_cdrsDeploymentARN = a});

-- | -- | The response status code.
cdrsResponseStatus :: Lens' CreateDeploymentResponse Int
cdrsResponseStatus = lens _cdrsResponseStatus (\ s a -> s{_cdrsResponseStatus = a});

instance NFData CreateDeploymentResponse where
