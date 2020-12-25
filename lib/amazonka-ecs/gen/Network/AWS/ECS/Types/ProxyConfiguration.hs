{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.ProxyConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.ProxyConfiguration
  ( ProxyConfiguration (..),

    -- * Smart constructor
    mkProxyConfiguration,

    -- * Lenses
    pContainerName,
    pProperties,
    pType,
  )
where

import qualified Network.AWS.ECS.Types.KeyValuePair as Types
import qualified Network.AWS.ECS.Types.ProxyConfigurationType as Types
import qualified Network.AWS.ECS.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The configuration details for the App Mesh proxy.
--
-- For tasks using the EC2 launch type, the container instances require at least version 1.26.0 of the container agent and at least version 1.26.0-1 of the @ecs-init@ package to enable a proxy configuration. If your container instances are launched from the Amazon ECS-optimized AMI version @20190301@ or later, then they contain the required versions of the container agent and @ecs-init@ . For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html Amazon ECS-optimized Linux AMI>
--
-- /See:/ 'mkProxyConfiguration' smart constructor.
data ProxyConfiguration = ProxyConfiguration'
  { -- | The name of the container that will serve as the App Mesh proxy.
    containerName :: Types.String,
    -- | The set of network configuration parameters to provide the Container Network Interface (CNI) plugin, specified as key-value pairs.
    --
    --
    --     * @IgnoredUID@ - (Required) The user ID (UID) of the proxy container as defined by the @user@ parameter in a container definition. This is used to ensure the proxy ignores its own traffic. If @IgnoredGID@ is specified, this field can be empty.
    --
    --
    --     * @IgnoredGID@ - (Required) The group ID (GID) of the proxy container as defined by the @user@ parameter in a container definition. This is used to ensure the proxy ignores its own traffic. If @IgnoredUID@ is specified, this field can be empty.
    --
    --
    --     * @AppPorts@ - (Required) The list of ports that the application uses. Network traffic to these ports is forwarded to the @ProxyIngressPort@ and @ProxyEgressPort@ .
    --
    --
    --     * @ProxyIngressPort@ - (Required) Specifies the port that incoming traffic to the @AppPorts@ is directed to.
    --
    --
    --     * @ProxyEgressPort@ - (Required) Specifies the port that outgoing traffic from the @AppPorts@ is directed to.
    --
    --
    --     * @EgressIgnoredPorts@ - (Required) The egress traffic going to the specified ports is ignored and not redirected to the @ProxyEgressPort@ . It can be an empty list.
    --
    --
    --     * @EgressIgnoredIPs@ - (Required) The egress traffic going to the specified IP addresses is ignored and not redirected to the @ProxyEgressPort@ . It can be an empty list.
    properties :: Core.Maybe [Types.KeyValuePair],
    -- | The proxy type. The only supported value is @APPMESH@ .
    type' :: Core.Maybe Types.ProxyConfigurationType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ProxyConfiguration' value with any optional fields omitted.
mkProxyConfiguration ::
  -- | 'containerName'
  Types.String ->
  ProxyConfiguration
mkProxyConfiguration containerName =
  ProxyConfiguration'
    { containerName,
      properties = Core.Nothing,
      type' = Core.Nothing
    }

-- | The name of the container that will serve as the App Mesh proxy.
--
-- /Note:/ Consider using 'containerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pContainerName :: Lens.Lens' ProxyConfiguration Types.String
pContainerName = Lens.field @"containerName"
{-# DEPRECATED pContainerName "Use generic-lens or generic-optics with 'containerName' instead." #-}

-- | The set of network configuration parameters to provide the Container Network Interface (CNI) plugin, specified as key-value pairs.
--
--
--     * @IgnoredUID@ - (Required) The user ID (UID) of the proxy container as defined by the @user@ parameter in a container definition. This is used to ensure the proxy ignores its own traffic. If @IgnoredGID@ is specified, this field can be empty.
--
--
--     * @IgnoredGID@ - (Required) The group ID (GID) of the proxy container as defined by the @user@ parameter in a container definition. This is used to ensure the proxy ignores its own traffic. If @IgnoredUID@ is specified, this field can be empty.
--
--
--     * @AppPorts@ - (Required) The list of ports that the application uses. Network traffic to these ports is forwarded to the @ProxyIngressPort@ and @ProxyEgressPort@ .
--
--
--     * @ProxyIngressPort@ - (Required) Specifies the port that incoming traffic to the @AppPorts@ is directed to.
--
--
--     * @ProxyEgressPort@ - (Required) Specifies the port that outgoing traffic from the @AppPorts@ is directed to.
--
--
--     * @EgressIgnoredPorts@ - (Required) The egress traffic going to the specified ports is ignored and not redirected to the @ProxyEgressPort@ . It can be an empty list.
--
--
--     * @EgressIgnoredIPs@ - (Required) The egress traffic going to the specified IP addresses is ignored and not redirected to the @ProxyEgressPort@ . It can be an empty list.
--
--
--
-- /Note:/ Consider using 'properties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pProperties :: Lens.Lens' ProxyConfiguration (Core.Maybe [Types.KeyValuePair])
pProperties = Lens.field @"properties"
{-# DEPRECATED pProperties "Use generic-lens or generic-optics with 'properties' instead." #-}

-- | The proxy type. The only supported value is @APPMESH@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pType :: Lens.Lens' ProxyConfiguration (Core.Maybe Types.ProxyConfigurationType)
pType = Lens.field @"type'"
{-# DEPRECATED pType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromJSON ProxyConfiguration where
  toJSON ProxyConfiguration {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("containerName" Core..= containerName),
            ("properties" Core..=) Core.<$> properties,
            ("type" Core..=) Core.<$> type'
          ]
      )

instance Core.FromJSON ProxyConfiguration where
  parseJSON =
    Core.withObject "ProxyConfiguration" Core.$
      \x ->
        ProxyConfiguration'
          Core.<$> (x Core..: "containerName")
          Core.<*> (x Core..:? "properties")
          Core.<*> (x Core..:? "type")
