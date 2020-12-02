{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.DeploymentConfigInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.DeploymentConfigInfo where

import Network.AWS.CodeDeploy.Types.ComputePlatform
import Network.AWS.CodeDeploy.Types.MinimumHealthyHosts
import Network.AWS.CodeDeploy.Types.TrafficRoutingConfig
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a deployment configuration.
--
--
--
-- /See:/ 'deploymentConfigInfo' smart constructor.
data DeploymentConfigInfo = DeploymentConfigInfo'
  { _dciDeploymentConfigName ::
      !(Maybe Text),
    _dciComputePlatform :: !(Maybe ComputePlatform),
    _dciMinimumHealthyHosts ::
      !(Maybe MinimumHealthyHosts),
    _dciTrafficRoutingConfig ::
      !(Maybe TrafficRoutingConfig),
    _dciDeploymentConfigId :: !(Maybe Text),
    _dciCreateTime :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeploymentConfigInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dciDeploymentConfigName' - The deployment configuration name.
--
-- * 'dciComputePlatform' - The destination platform type for the deployment (@Lambda@ , @Server@ , or @ECS@ ).
--
-- * 'dciMinimumHealthyHosts' - Information about the number or percentage of minimum healthy instance.
--
-- * 'dciTrafficRoutingConfig' - The configuration that specifies how the deployment traffic is routed. Used for deployments with a Lambda or ECS compute platform only.
--
-- * 'dciDeploymentConfigId' - The deployment configuration ID.
--
-- * 'dciCreateTime' - The time at which the deployment configuration was created.
deploymentConfigInfo ::
  DeploymentConfigInfo
deploymentConfigInfo =
  DeploymentConfigInfo'
    { _dciDeploymentConfigName = Nothing,
      _dciComputePlatform = Nothing,
      _dciMinimumHealthyHosts = Nothing,
      _dciTrafficRoutingConfig = Nothing,
      _dciDeploymentConfigId = Nothing,
      _dciCreateTime = Nothing
    }

-- | The deployment configuration name.
dciDeploymentConfigName :: Lens' DeploymentConfigInfo (Maybe Text)
dciDeploymentConfigName = lens _dciDeploymentConfigName (\s a -> s {_dciDeploymentConfigName = a})

-- | The destination platform type for the deployment (@Lambda@ , @Server@ , or @ECS@ ).
dciComputePlatform :: Lens' DeploymentConfigInfo (Maybe ComputePlatform)
dciComputePlatform = lens _dciComputePlatform (\s a -> s {_dciComputePlatform = a})

-- | Information about the number or percentage of minimum healthy instance.
dciMinimumHealthyHosts :: Lens' DeploymentConfigInfo (Maybe MinimumHealthyHosts)
dciMinimumHealthyHosts = lens _dciMinimumHealthyHosts (\s a -> s {_dciMinimumHealthyHosts = a})

-- | The configuration that specifies how the deployment traffic is routed. Used for deployments with a Lambda or ECS compute platform only.
dciTrafficRoutingConfig :: Lens' DeploymentConfigInfo (Maybe TrafficRoutingConfig)
dciTrafficRoutingConfig = lens _dciTrafficRoutingConfig (\s a -> s {_dciTrafficRoutingConfig = a})

-- | The deployment configuration ID.
dciDeploymentConfigId :: Lens' DeploymentConfigInfo (Maybe Text)
dciDeploymentConfigId = lens _dciDeploymentConfigId (\s a -> s {_dciDeploymentConfigId = a})

-- | The time at which the deployment configuration was created.
dciCreateTime :: Lens' DeploymentConfigInfo (Maybe UTCTime)
dciCreateTime = lens _dciCreateTime (\s a -> s {_dciCreateTime = a}) . mapping _Time

instance FromJSON DeploymentConfigInfo where
  parseJSON =
    withObject
      "DeploymentConfigInfo"
      ( \x ->
          DeploymentConfigInfo'
            <$> (x .:? "deploymentConfigName")
            <*> (x .:? "computePlatform")
            <*> (x .:? "minimumHealthyHosts")
            <*> (x .:? "trafficRoutingConfig")
            <*> (x .:? "deploymentConfigId")
            <*> (x .:? "createTime")
      )

instance Hashable DeploymentConfigInfo

instance NFData DeploymentConfigInfo
