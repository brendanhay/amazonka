{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.ProxyConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.ProxyConfiguration where

import Network.AWS.ECS.Types.KeyValuePair
import Network.AWS.ECS.Types.ProxyConfigurationType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The configuration details for the App Mesh proxy.
--
-- For tasks using the EC2 launch type, the container instances require at
-- least version 1.26.0 of the container agent and at least version
-- 1.26.0-1 of the @ecs-init@ package to enable a proxy configuration. If
-- your container instances are launched from the Amazon ECS-optimized AMI
-- version @20190301@ or later, then they contain the required versions of
-- the container agent and @ecs-init@. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html Amazon ECS-optimized Linux AMI>
--
-- /See:/ 'newProxyConfiguration' smart constructor.
data ProxyConfiguration = ProxyConfiguration'
  { -- | The set of network configuration parameters to provide the Container
    -- Network Interface (CNI) plugin, specified as key-value pairs.
    --
    -- -   @IgnoredUID@ - (Required) The user ID (UID) of the proxy container
    --     as defined by the @user@ parameter in a container definition. This
    --     is used to ensure the proxy ignores its own traffic. If @IgnoredGID@
    --     is specified, this field can be empty.
    --
    -- -   @IgnoredGID@ - (Required) The group ID (GID) of the proxy container
    --     as defined by the @user@ parameter in a container definition. This
    --     is used to ensure the proxy ignores its own traffic. If @IgnoredUID@
    --     is specified, this field can be empty.
    --
    -- -   @AppPorts@ - (Required) The list of ports that the application uses.
    --     Network traffic to these ports is forwarded to the
    --     @ProxyIngressPort@ and @ProxyEgressPort@.
    --
    -- -   @ProxyIngressPort@ - (Required) Specifies the port that incoming
    --     traffic to the @AppPorts@ is directed to.
    --
    -- -   @ProxyEgressPort@ - (Required) Specifies the port that outgoing
    --     traffic from the @AppPorts@ is directed to.
    --
    -- -   @EgressIgnoredPorts@ - (Required) The egress traffic going to the
    --     specified ports is ignored and not redirected to the
    --     @ProxyEgressPort@. It can be an empty list.
    --
    -- -   @EgressIgnoredIPs@ - (Required) The egress traffic going to the
    --     specified IP addresses is ignored and not redirected to the
    --     @ProxyEgressPort@. It can be an empty list.
    properties :: Prelude.Maybe [KeyValuePair],
    -- | The proxy type. The only supported value is @APPMESH@.
    type' :: Prelude.Maybe ProxyConfigurationType,
    -- | The name of the container that will serve as the App Mesh proxy.
    containerName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ProxyConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'properties', 'proxyConfiguration_properties' - The set of network configuration parameters to provide the Container
-- Network Interface (CNI) plugin, specified as key-value pairs.
--
-- -   @IgnoredUID@ - (Required) The user ID (UID) of the proxy container
--     as defined by the @user@ parameter in a container definition. This
--     is used to ensure the proxy ignores its own traffic. If @IgnoredGID@
--     is specified, this field can be empty.
--
-- -   @IgnoredGID@ - (Required) The group ID (GID) of the proxy container
--     as defined by the @user@ parameter in a container definition. This
--     is used to ensure the proxy ignores its own traffic. If @IgnoredUID@
--     is specified, this field can be empty.
--
-- -   @AppPorts@ - (Required) The list of ports that the application uses.
--     Network traffic to these ports is forwarded to the
--     @ProxyIngressPort@ and @ProxyEgressPort@.
--
-- -   @ProxyIngressPort@ - (Required) Specifies the port that incoming
--     traffic to the @AppPorts@ is directed to.
--
-- -   @ProxyEgressPort@ - (Required) Specifies the port that outgoing
--     traffic from the @AppPorts@ is directed to.
--
-- -   @EgressIgnoredPorts@ - (Required) The egress traffic going to the
--     specified ports is ignored and not redirected to the
--     @ProxyEgressPort@. It can be an empty list.
--
-- -   @EgressIgnoredIPs@ - (Required) The egress traffic going to the
--     specified IP addresses is ignored and not redirected to the
--     @ProxyEgressPort@. It can be an empty list.
--
-- 'type'', 'proxyConfiguration_type' - The proxy type. The only supported value is @APPMESH@.
--
-- 'containerName', 'proxyConfiguration_containerName' - The name of the container that will serve as the App Mesh proxy.
newProxyConfiguration ::
  -- | 'containerName'
  Prelude.Text ->
  ProxyConfiguration
newProxyConfiguration pContainerName_ =
  ProxyConfiguration'
    { properties = Prelude.Nothing,
      type' = Prelude.Nothing,
      containerName = pContainerName_
    }

-- | The set of network configuration parameters to provide the Container
-- Network Interface (CNI) plugin, specified as key-value pairs.
--
-- -   @IgnoredUID@ - (Required) The user ID (UID) of the proxy container
--     as defined by the @user@ parameter in a container definition. This
--     is used to ensure the proxy ignores its own traffic. If @IgnoredGID@
--     is specified, this field can be empty.
--
-- -   @IgnoredGID@ - (Required) The group ID (GID) of the proxy container
--     as defined by the @user@ parameter in a container definition. This
--     is used to ensure the proxy ignores its own traffic. If @IgnoredUID@
--     is specified, this field can be empty.
--
-- -   @AppPorts@ - (Required) The list of ports that the application uses.
--     Network traffic to these ports is forwarded to the
--     @ProxyIngressPort@ and @ProxyEgressPort@.
--
-- -   @ProxyIngressPort@ - (Required) Specifies the port that incoming
--     traffic to the @AppPorts@ is directed to.
--
-- -   @ProxyEgressPort@ - (Required) Specifies the port that outgoing
--     traffic from the @AppPorts@ is directed to.
--
-- -   @EgressIgnoredPorts@ - (Required) The egress traffic going to the
--     specified ports is ignored and not redirected to the
--     @ProxyEgressPort@. It can be an empty list.
--
-- -   @EgressIgnoredIPs@ - (Required) The egress traffic going to the
--     specified IP addresses is ignored and not redirected to the
--     @ProxyEgressPort@. It can be an empty list.
proxyConfiguration_properties :: Lens.Lens' ProxyConfiguration (Prelude.Maybe [KeyValuePair])
proxyConfiguration_properties = Lens.lens (\ProxyConfiguration' {properties} -> properties) (\s@ProxyConfiguration' {} a -> s {properties = a} :: ProxyConfiguration) Prelude.. Lens.mapping Prelude._Coerce

-- | The proxy type. The only supported value is @APPMESH@.
proxyConfiguration_type :: Lens.Lens' ProxyConfiguration (Prelude.Maybe ProxyConfigurationType)
proxyConfiguration_type = Lens.lens (\ProxyConfiguration' {type'} -> type') (\s@ProxyConfiguration' {} a -> s {type' = a} :: ProxyConfiguration)

-- | The name of the container that will serve as the App Mesh proxy.
proxyConfiguration_containerName :: Lens.Lens' ProxyConfiguration Prelude.Text
proxyConfiguration_containerName = Lens.lens (\ProxyConfiguration' {containerName} -> containerName) (\s@ProxyConfiguration' {} a -> s {containerName = a} :: ProxyConfiguration)

instance Prelude.FromJSON ProxyConfiguration where
  parseJSON =
    Prelude.withObject
      "ProxyConfiguration"
      ( \x ->
          ProxyConfiguration'
            Prelude.<$> ( x Prelude..:? "properties"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "type")
            Prelude.<*> (x Prelude..: "containerName")
      )

instance Prelude.Hashable ProxyConfiguration

instance Prelude.NFData ProxyConfiguration

instance Prelude.ToJSON ProxyConfiguration where
  toJSON ProxyConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("properties" Prelude..=) Prelude.<$> properties,
            ("type" Prelude..=) Prelude.<$> type',
            Prelude.Just
              ("containerName" Prelude..= containerName)
          ]
      )
