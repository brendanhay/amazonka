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
-- Module      : Network.AWS.CodeDeploy.GetDeploymentInstance
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about an instance as part of a deployment.
--
-- /See:/ <http://docs.aws.amazon.com/codedeploy/latest/APIReference/API_GetDeploymentInstance.html AWS API Reference> for GetDeploymentInstance.
module Network.AWS.CodeDeploy.GetDeploymentInstance
    (
    -- * Creating a Request
      getDeploymentInstance
    , GetDeploymentInstance
    -- * Request Lenses
    , gdiDeploymentId
    , gdiInstanceId

    -- * Destructuring the Response
    , getDeploymentInstanceResponse
    , GetDeploymentInstanceResponse
    -- * Response Lenses
    , gdirsInstanceSummary
    , gdirsStatus
    ) where

import           Network.AWS.CodeDeploy.Types
import           Network.AWS.CodeDeploy.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a get deployment instance operation.
--
-- /See:/ 'getDeploymentInstance' smart constructor.
data GetDeploymentInstance = GetDeploymentInstance'
    { _gdiDeploymentId :: !Text
    , _gdiInstanceId   :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetDeploymentInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdiDeploymentId'
--
-- * 'gdiInstanceId'
getDeploymentInstance
    :: Text -- ^ 'gdiDeploymentId'
    -> Text -- ^ 'gdiInstanceId'
    -> GetDeploymentInstance
getDeploymentInstance pDeploymentId_ pInstanceId_ =
    GetDeploymentInstance'
    { _gdiDeploymentId = pDeploymentId_
    , _gdiInstanceId = pInstanceId_
    }

-- | The unique ID of a deployment.
gdiDeploymentId :: Lens' GetDeploymentInstance Text
gdiDeploymentId = lens _gdiDeploymentId (\ s a -> s{_gdiDeploymentId = a});

-- | The unique ID of an instance in the deployment\'s deployment group.
gdiInstanceId :: Lens' GetDeploymentInstance Text
gdiInstanceId = lens _gdiInstanceId (\ s a -> s{_gdiInstanceId = a});

instance AWSRequest GetDeploymentInstance where
        type Rs GetDeploymentInstance =
             GetDeploymentInstanceResponse
        request = postJSON codeDeploy
        response
          = receiveJSON
              (\ s h x ->
                 GetDeploymentInstanceResponse' <$>
                   (x .?> "instanceSummary") <*> (pure (fromEnum s)))

instance ToHeaders GetDeploymentInstance where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeDeploy_20141006.GetDeploymentInstance" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetDeploymentInstance where
        toJSON GetDeploymentInstance'{..}
          = object
              (catMaybes
                 [Just ("deploymentId" .= _gdiDeploymentId),
                  Just ("instanceId" .= _gdiInstanceId)])

instance ToPath GetDeploymentInstance where
        toPath = const "/"

instance ToQuery GetDeploymentInstance where
        toQuery = const mempty

-- | Represents the output of a get deployment instance operation.
--
-- /See:/ 'getDeploymentInstanceResponse' smart constructor.
data GetDeploymentInstanceResponse = GetDeploymentInstanceResponse'
    { _gdirsInstanceSummary :: !(Maybe InstanceSummary)
    , _gdirsStatus          :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetDeploymentInstanceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdirsInstanceSummary'
--
-- * 'gdirsStatus'
getDeploymentInstanceResponse
    :: Int -- ^ 'gdirsStatus'
    -> GetDeploymentInstanceResponse
getDeploymentInstanceResponse pStatus_ =
    GetDeploymentInstanceResponse'
    { _gdirsInstanceSummary = Nothing
    , _gdirsStatus = pStatus_
    }

-- | Information about the instance.
gdirsInstanceSummary :: Lens' GetDeploymentInstanceResponse (Maybe InstanceSummary)
gdirsInstanceSummary = lens _gdirsInstanceSummary (\ s a -> s{_gdirsInstanceSummary = a});

-- | The response status code.
gdirsStatus :: Lens' GetDeploymentInstanceResponse Int
gdirsStatus = lens _gdirsStatus (\ s a -> s{_gdirsStatus = a});
