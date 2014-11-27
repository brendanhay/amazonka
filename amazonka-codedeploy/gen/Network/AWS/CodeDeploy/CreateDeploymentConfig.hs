{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CodeDeploy.CreateDeploymentConfig
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Creates a new deployment configuration.
--
-- <http://docs.aws.amazon.com/codedeploy/latest/APIReference/API_CreateDeploymentConfig.html>
module Network.AWS.CodeDeploy.CreateDeploymentConfig
    (
    -- * Request
      CreateDeploymentConfig
    -- ** Request constructor
    , createDeploymentConfig
    -- ** Request lenses
    , cdcDeploymentConfigName
    , cdcMinimumHealthyHosts

    -- * Response
    , CreateDeploymentConfigResponse
    -- ** Response constructor
    , createDeploymentConfigResponse
    -- ** Response lenses
    , cdcrDeploymentConfigId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.CodeDeploy.Types
import qualified GHC.Exts

data CreateDeploymentConfig = CreateDeploymentConfig
    { _cdcDeploymentConfigName :: Text
    , _cdcMinimumHealthyHosts  :: Maybe MinimumHealthyHosts
    } deriving (Eq, Show)

-- | 'CreateDeploymentConfig' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdcDeploymentConfigName' @::@ 'Text'
--
-- * 'cdcMinimumHealthyHosts' @::@ 'Maybe' 'MinimumHealthyHosts'
--
createDeploymentConfig :: Text -- ^ 'cdcDeploymentConfigName'
                       -> CreateDeploymentConfig
createDeploymentConfig p1 = CreateDeploymentConfig
    { _cdcDeploymentConfigName = p1
    , _cdcMinimumHealthyHosts  = Nothing
    }

-- | The name of the deployment configuration to create.
cdcDeploymentConfigName :: Lens' CreateDeploymentConfig Text
cdcDeploymentConfigName =
    lens _cdcDeploymentConfigName (\s a -> s { _cdcDeploymentConfigName = a })

-- | The minimum number of healthy instances that should be available at any time
-- during the deployment. There are two parameters expected in the input: type
-- and value.
--
-- The type parameter takes either of the following values:
--
-- HOST_COUNT: The value parameter represents the minimum number of healthy
-- instances, as an absolute value. FLEET_PERCENT: The value parameter
-- represents the minimum number of healthy instances, as a percentage of the
-- total number of instances in the deployment. If you specify FLEET_PERCENT,
-- then at the start of the deployment AWS CodeDeploy converts the percentage to
-- the equivalent number of instances and rounds fractional instances up.  The
-- value parameter takes an integer.
--
-- For example, to set a minimum of 95% healthy instances, specify a type of
-- FLEET_PERCENT and a value of 95.
cdcMinimumHealthyHosts :: Lens' CreateDeploymentConfig (Maybe MinimumHealthyHosts)
cdcMinimumHealthyHosts =
    lens _cdcMinimumHealthyHosts (\s a -> s { _cdcMinimumHealthyHosts = a })

newtype CreateDeploymentConfigResponse = CreateDeploymentConfigResponse
    { _cdcrDeploymentConfigId :: Maybe Text
    } deriving (Eq, Ord, Show, Monoid)

-- | 'CreateDeploymentConfigResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdcrDeploymentConfigId' @::@ 'Maybe' 'Text'
--
createDeploymentConfigResponse :: CreateDeploymentConfigResponse
createDeploymentConfigResponse = CreateDeploymentConfigResponse
    { _cdcrDeploymentConfigId = Nothing
    }

-- | A unique deployment configuration ID.
cdcrDeploymentConfigId :: Lens' CreateDeploymentConfigResponse (Maybe Text)
cdcrDeploymentConfigId =
    lens _cdcrDeploymentConfigId (\s a -> s { _cdcrDeploymentConfigId = a })

instance ToPath CreateDeploymentConfig where
    toPath = const "/"

instance ToQuery CreateDeploymentConfig where
    toQuery = const mempty

instance ToHeaders CreateDeploymentConfig

instance ToJSON CreateDeploymentConfig where
    toJSON CreateDeploymentConfig{..} = object
        [ "deploymentConfigName" .= _cdcDeploymentConfigName
        , "minimumHealthyHosts"  .= _cdcMinimumHealthyHosts
        ]

instance AWSRequest CreateDeploymentConfig where
    type Sv CreateDeploymentConfig = CodeDeploy
    type Rs CreateDeploymentConfig = CreateDeploymentConfigResponse

    request  = post "CreateDeploymentConfig"
    response = jsonResponse

instance FromJSON CreateDeploymentConfigResponse where
    parseJSON = withObject "CreateDeploymentConfigResponse" $ \o -> CreateDeploymentConfigResponse
        <$> o .:? "deploymentConfigId"
