{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CodeDeploy.GetDeploymentConfig
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

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
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.CodeDeploy.Types
import qualified GHC.Exts

newtype GetDeploymentConfig = GetDeploymentConfig
    { _gdcDeploymentConfigName :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'GetDeploymentConfig' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdcDeploymentConfigName' @::@ 'Text'
--
getDeploymentConfig :: Text -- ^ 'gdcDeploymentConfigName'
                    -> GetDeploymentConfig
getDeploymentConfig p1 = GetDeploymentConfig
    { _gdcDeploymentConfigName = p1
    }

-- | The name of an existing deployment configuration within the AWS user
-- account.
gdcDeploymentConfigName :: Lens' GetDeploymentConfig Text
gdcDeploymentConfigName =
    lens _gdcDeploymentConfigName (\s a -> s { _gdcDeploymentConfigName = a })

newtype GetDeploymentConfigResponse = GetDeploymentConfigResponse
    { _gdcrDeploymentConfigInfo :: Maybe DeploymentConfigInfo
    } deriving (Eq, Show, Generic)

-- | 'GetDeploymentConfigResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdcrDeploymentConfigInfo' @::@ 'Maybe' 'DeploymentConfigInfo'
--
getDeploymentConfigResponse :: GetDeploymentConfigResponse
getDeploymentConfigResponse = GetDeploymentConfigResponse
    { _gdcrDeploymentConfigInfo = Nothing
    }

-- | Information about the deployment configuration.
gdcrDeploymentConfigInfo :: Lens' GetDeploymentConfigResponse (Maybe DeploymentConfigInfo)
gdcrDeploymentConfigInfo =
    lens _gdcrDeploymentConfigInfo
        (\s a -> s { _gdcrDeploymentConfigInfo = a })

instance ToPath GetDeploymentConfig where
    toPath = const "/"

instance ToQuery GetDeploymentConfig where
    toQuery = const mempty

instance ToHeaders GetDeploymentConfig

instance ToJSON GetDeploymentConfig where
    toJSON GetDeploymentConfig{..} = object
        [ "deploymentConfigName" .= _gdcDeploymentConfigName
        ]

instance AWSRequest GetDeploymentConfig where
    type Sv GetDeploymentConfig = CodeDeploy
    type Rs GetDeploymentConfig = GetDeploymentConfigResponse

    request  = post "GetDeploymentConfig"
    response = jsonResponse

instance FromJSON GetDeploymentConfigResponse where
    parseJSON = withObject "GetDeploymentConfigResponse" $ \o -> GetDeploymentConfigResponse
        <$> o .:? "deploymentConfigInfo"
