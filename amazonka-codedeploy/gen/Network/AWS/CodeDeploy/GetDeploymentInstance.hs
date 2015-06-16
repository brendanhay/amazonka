{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CodeDeploy.GetDeploymentInstance
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

-- | Gets information about an instance as part of a deployment.
--
-- <http://docs.aws.amazon.com/codedeploy/latest/APIReference/API_GetDeploymentInstance.html>
module Network.AWS.CodeDeploy.GetDeploymentInstance
    (
    -- * Request
      GetDeploymentInstance
    -- ** Request constructor
    , getDeploymentInstance
    -- ** Request lenses
    , gdiDeploymentId
    , gdiInstanceId

    -- * Response
    , GetDeploymentInstanceResponse
    -- ** Response constructor
    , getDeploymentInstanceResponse
    -- ** Response lenses
    , gdirInstanceSummary
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.CodeDeploy.Types

-- | /See:/ 'getDeploymentInstance' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdiDeploymentId'
--
-- * 'gdiInstanceId'
data GetDeploymentInstance = GetDeploymentInstance'{_gdiDeploymentId :: Text, _gdiInstanceId :: Text} deriving (Eq, Read, Show)

-- | 'GetDeploymentInstance' smart constructor.
getDeploymentInstance :: Text -> Text -> GetDeploymentInstance
getDeploymentInstance pDeploymentId pInstanceId = GetDeploymentInstance'{_gdiDeploymentId = pDeploymentId, _gdiInstanceId = pInstanceId};

-- | The unique ID of a deployment.
gdiDeploymentId :: Lens' GetDeploymentInstance Text
gdiDeploymentId = lens _gdiDeploymentId (\ s a -> s{_gdiDeploymentId = a});

-- | The unique ID of an instance in the deployment\'s deployment group.
gdiInstanceId :: Lens' GetDeploymentInstance Text
gdiInstanceId = lens _gdiInstanceId (\ s a -> s{_gdiInstanceId = a});

instance AWSRequest GetDeploymentInstance where
        type Sv GetDeploymentInstance = CodeDeploy
        type Rs GetDeploymentInstance =
             GetDeploymentInstanceResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 GetDeploymentInstanceResponse' <$>
                   (x .?> "instanceSummary"))

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
              ["deploymentId" .= _gdiDeploymentId,
               "instanceId" .= _gdiInstanceId]

instance ToPath GetDeploymentInstance where
        toPath = const "/"

instance ToQuery GetDeploymentInstance where
        toQuery = const mempty

-- | /See:/ 'getDeploymentInstanceResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdirInstanceSummary'
newtype GetDeploymentInstanceResponse = GetDeploymentInstanceResponse'{_gdirInstanceSummary :: Maybe InstanceSummary} deriving (Eq, Read, Show)

-- | 'GetDeploymentInstanceResponse' smart constructor.
getDeploymentInstanceResponse :: GetDeploymentInstanceResponse
getDeploymentInstanceResponse = GetDeploymentInstanceResponse'{_gdirInstanceSummary = Nothing};

-- | Information about the instance.
gdirInstanceSummary :: Lens' GetDeploymentInstanceResponse (Maybe InstanceSummary)
gdirInstanceSummary = lens _gdirInstanceSummary (\ s a -> s{_gdirInstanceSummary = a});
