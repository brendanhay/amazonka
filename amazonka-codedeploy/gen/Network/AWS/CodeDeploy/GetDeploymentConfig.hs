{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CodeDeploy.GetDeploymentConfig
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

-- | Gets information about a deployment configuration.
--
-- <http://docs.aws.amazon.com/codedeploy/latest/APIReference/API_GetDeploymentConfig.html>
module Network.AWS.CodeDeploy.GetDeploymentConfig
    (
    -- * Request
      GetDeploymentConfig
    -- ** Request constructor
    , getDeploymentConfig
    -- ** Request lenses
    , gdcDeploymentConfigName

    -- * Response
    , GetDeploymentConfigResponse
    -- ** Response constructor
    , getDeploymentConfigResponse
    -- ** Response lenses
    , gdcrDeploymentConfigInfo
    , gdcrStatus
    ) where

import           Network.AWS.CodeDeploy.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a get deployment configuration operation.
--
-- /See:/ 'getDeploymentConfig' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdcDeploymentConfigName'
newtype GetDeploymentConfig = GetDeploymentConfig'
    { _gdcDeploymentConfigName :: Text
    } deriving (Eq,Read,Show)

-- | 'GetDeploymentConfig' smart constructor.
getDeploymentConfig :: Text -> GetDeploymentConfig
getDeploymentConfig pDeploymentConfigName =
    GetDeploymentConfig'
    { _gdcDeploymentConfigName = pDeploymentConfigName
    }

-- | The name of an existing deployment configuration associated with the
-- applicable IAM user or AWS account.
gdcDeploymentConfigName :: Lens' GetDeploymentConfig Text
gdcDeploymentConfigName = lens _gdcDeploymentConfigName (\ s a -> s{_gdcDeploymentConfigName = a});

instance AWSRequest GetDeploymentConfig where
        type Sv GetDeploymentConfig = CodeDeploy
        type Rs GetDeploymentConfig =
             GetDeploymentConfigResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 GetDeploymentConfigResponse' <$>
                   (x .?> "deploymentConfigInfo") <*>
                     (pure (fromEnum s)))

instance ToHeaders GetDeploymentConfig where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeDeploy_20141006.GetDeploymentConfig" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetDeploymentConfig where
        toJSON GetDeploymentConfig'{..}
          = object
              ["deploymentConfigName" .= _gdcDeploymentConfigName]

instance ToPath GetDeploymentConfig where
        toPath = const "/"

instance ToQuery GetDeploymentConfig where
        toQuery = const mempty

-- | Represents the output of a get deployment configuration operation.
--
-- /See:/ 'getDeploymentConfigResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdcrDeploymentConfigInfo'
--
-- * 'gdcrStatus'
data GetDeploymentConfigResponse = GetDeploymentConfigResponse'
    { _gdcrDeploymentConfigInfo :: !(Maybe DeploymentConfigInfo)
    , _gdcrStatus               :: !Int
    } deriving (Eq,Read,Show)

-- | 'GetDeploymentConfigResponse' smart constructor.
getDeploymentConfigResponse :: Int -> GetDeploymentConfigResponse
getDeploymentConfigResponse pStatus =
    GetDeploymentConfigResponse'
    { _gdcrDeploymentConfigInfo = Nothing
    , _gdcrStatus = pStatus
    }

-- | Information about the deployment configuration.
gdcrDeploymentConfigInfo :: Lens' GetDeploymentConfigResponse (Maybe DeploymentConfigInfo)
gdcrDeploymentConfigInfo = lens _gdcrDeploymentConfigInfo (\ s a -> s{_gdcrDeploymentConfigInfo = a});

-- | FIXME: Undocumented member.
gdcrStatus :: Lens' GetDeploymentConfigResponse Int
gdcrStatus = lens _gdcrStatus (\ s a -> s{_gdcrStatus = a});
