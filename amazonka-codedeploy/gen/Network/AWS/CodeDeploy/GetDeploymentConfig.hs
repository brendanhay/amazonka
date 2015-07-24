{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.GetDeploymentConfig
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a deployment configuration.
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
    , gdcrsDeploymentConfigInfo
    , gdcrsStatus
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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetDeploymentConfig' smart constructor.
getDeploymentConfig :: Text -> GetDeploymentConfig
getDeploymentConfig pDeploymentConfigName_ =
    GetDeploymentConfig'
    { _gdcDeploymentConfigName = pDeploymentConfigName_
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
-- * 'gdcrsDeploymentConfigInfo'
--
-- * 'gdcrsStatus'
data GetDeploymentConfigResponse = GetDeploymentConfigResponse'
    { _gdcrsDeploymentConfigInfo :: !(Maybe DeploymentConfigInfo)
    , _gdcrsStatus               :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetDeploymentConfigResponse' smart constructor.
getDeploymentConfigResponse :: Int -> GetDeploymentConfigResponse
getDeploymentConfigResponse pStatus_ =
    GetDeploymentConfigResponse'
    { _gdcrsDeploymentConfigInfo = Nothing
    , _gdcrsStatus = pStatus_
    }

-- | Information about the deployment configuration.
gdcrsDeploymentConfigInfo :: Lens' GetDeploymentConfigResponse (Maybe DeploymentConfigInfo)
gdcrsDeploymentConfigInfo = lens _gdcrsDeploymentConfigInfo (\ s a -> s{_gdcrsDeploymentConfigInfo = a});

-- | FIXME: Undocumented member.
gdcrsStatus :: Lens' GetDeploymentConfigResponse Int
gdcrsStatus = lens _gdcrsStatus (\ s a -> s{_gdcrsStatus = a});
