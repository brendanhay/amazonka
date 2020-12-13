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
    pType,
    pProperties,
  )
where

import Network.AWS.ECS.Types.KeyValuePair
import Network.AWS.ECS.Types.ProxyConfigurationType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The configuration details for the App Mesh proxy.
--
-- For tasks using the EC2 launch type, the container instances require at least version 1.26.0 of the container agent and at least version 1.26.0-1 of the @ecs-init@ package to enable a proxy configuration. If your container instances are launched from the Amazon ECS-optimized AMI version @20190301@ or later, then they contain the required versions of the container agent and @ecs-init@ . For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html Amazon ECS-optimized Linux AMI>
--
-- /See:/ 'mkProxyConfiguration' smart constructor.
data ProxyConfiguration = ProxyConfiguration'
  { -- | The name of the container that will serve as the App Mesh proxy.
    containerName :: Lude.Text,
    -- | The proxy type. The only supported value is @APPMESH@ .
    type' :: Lude.Maybe ProxyConfigurationType,
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
    properties :: Lude.Maybe [KeyValuePair]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProxyConfiguration' with the minimum fields required to make a request.
--
-- * 'containerName' - The name of the container that will serve as the App Mesh proxy.
-- * 'type'' - The proxy type. The only supported value is @APPMESH@ .
-- * 'properties' - The set of network configuration parameters to provide the Container Network Interface (CNI) plugin, specified as key-value pairs.
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
mkProxyConfiguration ::
  -- | 'containerName'
  Lude.Text ->
  ProxyConfiguration
mkProxyConfiguration pContainerName_ =
  ProxyConfiguration'
    { containerName = pContainerName_,
      type' = Lude.Nothing,
      properties = Lude.Nothing
    }

-- | The name of the container that will serve as the App Mesh proxy.
--
-- /Note:/ Consider using 'containerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pContainerName :: Lens.Lens' ProxyConfiguration Lude.Text
pContainerName = Lens.lens (containerName :: ProxyConfiguration -> Lude.Text) (\s a -> s {containerName = a} :: ProxyConfiguration)
{-# DEPRECATED pContainerName "Use generic-lens or generic-optics with 'containerName' instead." #-}

-- | The proxy type. The only supported value is @APPMESH@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pType :: Lens.Lens' ProxyConfiguration (Lude.Maybe ProxyConfigurationType)
pType = Lens.lens (type' :: ProxyConfiguration -> Lude.Maybe ProxyConfigurationType) (\s a -> s {type' = a} :: ProxyConfiguration)
{-# DEPRECATED pType "Use generic-lens or generic-optics with 'type'' instead." #-}

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
pProperties :: Lens.Lens' ProxyConfiguration (Lude.Maybe [KeyValuePair])
pProperties = Lens.lens (properties :: ProxyConfiguration -> Lude.Maybe [KeyValuePair]) (\s a -> s {properties = a} :: ProxyConfiguration)
{-# DEPRECATED pProperties "Use generic-lens or generic-optics with 'properties' instead." #-}

instance Lude.FromJSON ProxyConfiguration where
  parseJSON =
    Lude.withObject
      "ProxyConfiguration"
      ( \x ->
          ProxyConfiguration'
            Lude.<$> (x Lude..: "containerName")
            Lude.<*> (x Lude..:? "type")
            Lude.<*> (x Lude..:? "properties" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON ProxyConfiguration where
  toJSON ProxyConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("containerName" Lude..= containerName),
            ("type" Lude..=) Lude.<$> type',
            ("properties" Lude..=) Lude.<$> properties
          ]
      )
