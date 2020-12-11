{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.CreateDeploymentConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a deployment configuration.
module Network.AWS.CodeDeploy.CreateDeploymentConfig
  ( -- * Creating a request
    CreateDeploymentConfig (..),
    mkCreateDeploymentConfig,

    -- ** Request lenses
    cdcComputePlatform,
    cdcMinimumHealthyHosts,
    cdcTrafficRoutingConfig,
    cdcDeploymentConfigName,

    -- * Destructuring the response
    CreateDeploymentConfigResponse (..),
    mkCreateDeploymentConfigResponse,

    -- ** Response lenses
    cdcrsDeploymentConfigId,
    cdcrsResponseStatus,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @CreateDeploymentConfig@ operation.
--
-- /See:/ 'mkCreateDeploymentConfig' smart constructor.
data CreateDeploymentConfig = CreateDeploymentConfig'
  { computePlatform ::
      Lude.Maybe ComputePlatform,
    minimumHealthyHosts ::
      Lude.Maybe MinimumHealthyHosts,
    trafficRoutingConfig ::
      Lude.Maybe TrafficRoutingConfig,
    deploymentConfigName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDeploymentConfig' with the minimum fields required to make a request.
--
-- * 'computePlatform' - The destination platform type for the deployment (@Lambda@ , @Server@ , or @ECS@ ).
-- * 'deploymentConfigName' - The name of the deployment configuration to create.
-- * 'minimumHealthyHosts' - The minimum number of healthy instances that should be available at any time during the deployment. There are two parameters expected in the input: type and value.
--
-- The type parameter takes either of the following values:
--
--     * HOST_COUNT: The value parameter represents the minimum number of healthy instances as an absolute value.
--
--
--     * FLEET_PERCENT: The value parameter represents the minimum number of healthy instances as a percentage of the total number of instances in the deployment. If you specify FLEET_PERCENT, at the start of the deployment, AWS CodeDeploy converts the percentage to the equivalent number of instances and rounds up fractional instances.
--
--
-- The value parameter takes an integer.
-- For example, to set a minimum of 95% healthy instance, specify a type of FLEET_PERCENT and a value of 95.
-- * 'trafficRoutingConfig' - The configuration that specifies how the deployment traffic is routed.
mkCreateDeploymentConfig ::
  -- | 'deploymentConfigName'
  Lude.Text ->
  CreateDeploymentConfig
mkCreateDeploymentConfig pDeploymentConfigName_ =
  CreateDeploymentConfig'
    { computePlatform = Lude.Nothing,
      minimumHealthyHosts = Lude.Nothing,
      trafficRoutingConfig = Lude.Nothing,
      deploymentConfigName = pDeploymentConfigName_
    }

-- | The destination platform type for the deployment (@Lambda@ , @Server@ , or @ECS@ ).
--
-- /Note:/ Consider using 'computePlatform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcComputePlatform :: Lens.Lens' CreateDeploymentConfig (Lude.Maybe ComputePlatform)
cdcComputePlatform = Lens.lens (computePlatform :: CreateDeploymentConfig -> Lude.Maybe ComputePlatform) (\s a -> s {computePlatform = a} :: CreateDeploymentConfig)
{-# DEPRECATED cdcComputePlatform "Use generic-lens or generic-optics with 'computePlatform' instead." #-}

-- | The minimum number of healthy instances that should be available at any time during the deployment. There are two parameters expected in the input: type and value.
--
-- The type parameter takes either of the following values:
--
--     * HOST_COUNT: The value parameter represents the minimum number of healthy instances as an absolute value.
--
--
--     * FLEET_PERCENT: The value parameter represents the minimum number of healthy instances as a percentage of the total number of instances in the deployment. If you specify FLEET_PERCENT, at the start of the deployment, AWS CodeDeploy converts the percentage to the equivalent number of instances and rounds up fractional instances.
--
--
-- The value parameter takes an integer.
-- For example, to set a minimum of 95% healthy instance, specify a type of FLEET_PERCENT and a value of 95.
--
-- /Note:/ Consider using 'minimumHealthyHosts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcMinimumHealthyHosts :: Lens.Lens' CreateDeploymentConfig (Lude.Maybe MinimumHealthyHosts)
cdcMinimumHealthyHosts = Lens.lens (minimumHealthyHosts :: CreateDeploymentConfig -> Lude.Maybe MinimumHealthyHosts) (\s a -> s {minimumHealthyHosts = a} :: CreateDeploymentConfig)
{-# DEPRECATED cdcMinimumHealthyHosts "Use generic-lens or generic-optics with 'minimumHealthyHosts' instead." #-}

-- | The configuration that specifies how the deployment traffic is routed.
--
-- /Note:/ Consider using 'trafficRoutingConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcTrafficRoutingConfig :: Lens.Lens' CreateDeploymentConfig (Lude.Maybe TrafficRoutingConfig)
cdcTrafficRoutingConfig = Lens.lens (trafficRoutingConfig :: CreateDeploymentConfig -> Lude.Maybe TrafficRoutingConfig) (\s a -> s {trafficRoutingConfig = a} :: CreateDeploymentConfig)
{-# DEPRECATED cdcTrafficRoutingConfig "Use generic-lens or generic-optics with 'trafficRoutingConfig' instead." #-}

-- | The name of the deployment configuration to create.
--
-- /Note:/ Consider using 'deploymentConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcDeploymentConfigName :: Lens.Lens' CreateDeploymentConfig Lude.Text
cdcDeploymentConfigName = Lens.lens (deploymentConfigName :: CreateDeploymentConfig -> Lude.Text) (\s a -> s {deploymentConfigName = a} :: CreateDeploymentConfig)
{-# DEPRECATED cdcDeploymentConfigName "Use generic-lens or generic-optics with 'deploymentConfigName' instead." #-}

instance Lude.AWSRequest CreateDeploymentConfig where
  type Rs CreateDeploymentConfig = CreateDeploymentConfigResponse
  request = Req.postJSON codeDeployService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateDeploymentConfigResponse'
            Lude.<$> (x Lude..?> "deploymentConfigId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateDeploymentConfig where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeDeploy_20141006.CreateDeploymentConfig" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateDeploymentConfig where
  toJSON CreateDeploymentConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("computePlatform" Lude..=) Lude.<$> computePlatform,
            ("minimumHealthyHosts" Lude..=) Lude.<$> minimumHealthyHosts,
            ("trafficRoutingConfig" Lude..=) Lude.<$> trafficRoutingConfig,
            Lude.Just ("deploymentConfigName" Lude..= deploymentConfigName)
          ]
      )

instance Lude.ToPath CreateDeploymentConfig where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateDeploymentConfig where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @CreateDeploymentConfig@ operation.
--
-- /See:/ 'mkCreateDeploymentConfigResponse' smart constructor.
data CreateDeploymentConfigResponse = CreateDeploymentConfigResponse'
  { deploymentConfigId ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDeploymentConfigResponse' with the minimum fields required to make a request.
--
-- * 'deploymentConfigId' - A unique deployment configuration ID.
-- * 'responseStatus' - The response status code.
mkCreateDeploymentConfigResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateDeploymentConfigResponse
mkCreateDeploymentConfigResponse pResponseStatus_ =
  CreateDeploymentConfigResponse'
    { deploymentConfigId =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A unique deployment configuration ID.
--
-- /Note:/ Consider using 'deploymentConfigId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcrsDeploymentConfigId :: Lens.Lens' CreateDeploymentConfigResponse (Lude.Maybe Lude.Text)
cdcrsDeploymentConfigId = Lens.lens (deploymentConfigId :: CreateDeploymentConfigResponse -> Lude.Maybe Lude.Text) (\s a -> s {deploymentConfigId = a} :: CreateDeploymentConfigResponse)
{-# DEPRECATED cdcrsDeploymentConfigId "Use generic-lens or generic-optics with 'deploymentConfigId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcrsResponseStatus :: Lens.Lens' CreateDeploymentConfigResponse Lude.Int
cdcrsResponseStatus = Lens.lens (responseStatus :: CreateDeploymentConfigResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateDeploymentConfigResponse)
{-# DEPRECATED cdcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
