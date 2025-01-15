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
-- Module      : Amazonka.ECS.Types.PortMapping
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.PortMapping where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECS.Types.ApplicationProtocol
import Amazonka.ECS.Types.TransportProtocol
import qualified Amazonka.Prelude as Prelude

-- | Port mappings allow containers to access ports on the host container
-- instance to send or receive traffic. Port mappings are specified as part
-- of the container definition.
--
-- If you use containers in a task with the @awsvpc@ or @host@ network
-- mode, specify the exposed ports using @containerPort@. The @hostPort@
-- can be left blank or it must be the same value as the @containerPort@.
--
-- You can\'t expose the same container port for multiple protocols. If you
-- attempt this, an error is returned.
--
-- After a task reaches the @RUNNING@ status, manual and automatic host and
-- container port assignments are visible in the @networkBindings@ section
-- of DescribeTasks API responses.
--
-- /See:/ 'newPortMapping' smart constructor.
data PortMapping = PortMapping'
  { -- | The application protocol that\'s used for the port mapping. This
    -- parameter only applies to Service Connect. We recommend that you set
    -- this parameter to be consistent with the protocol that your application
    -- uses. If you set this parameter, Amazon ECS adds protocol-specific
    -- connection handling to the Service Connect proxy. If you set this
    -- parameter, Amazon ECS adds protocol-specific telemetry in the Amazon ECS
    -- console and CloudWatch.
    --
    -- If you don\'t set a value for this parameter, then TCP is used. However,
    -- Amazon ECS doesn\'t add protocol-specific telemetry for TCP.
    --
    -- Tasks that run in a namespace can use short names to connect to services
    -- in the namespace. Tasks can connect to services across all of the
    -- clusters in the namespace. Tasks connect through a managed proxy
    -- container that collects logs and metrics for increased visibility. Only
    -- the tasks that Amazon ECS services create are supported with Service
    -- Connect. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-connect.html Service Connect>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    appProtocol :: Prelude.Maybe ApplicationProtocol,
    -- | The port number on the container that\'s bound to the user-specified or
    -- automatically assigned host port.
    --
    -- If you use containers in a task with the @awsvpc@ or @host@ network
    -- mode, specify the exposed ports using @containerPort@.
    --
    -- If you use containers in a task with the @bridge@ network mode and you
    -- specify a container port and not a host port, your container
    -- automatically receives a host port in the ephemeral port range. For more
    -- information, see @hostPort@. Port mappings that are automatically
    -- assigned in this way do not count toward the 100 reserved ports limit of
    -- a container instance.
    containerPort :: Prelude.Maybe Prelude.Int,
    -- | The port number range on the container that\'s bound to the dynamically
    -- mapped host port range.
    --
    -- The following rules apply when you specify a @containerPortRange@:
    --
    -- -   You must use either the @bridge@ network mode or the @awsvpc@
    --     network mode.
    --
    -- -   This parameter is available for both the EC2 and Fargate launch
    --     types.
    --
    -- -   This parameter is available for both the Linux and Windows operating
    --     systems.
    --
    -- -   The container instance must have at least version 1.67.0 of the
    --     container agent and at least version 1.67.0-1 of the @ecs-init@
    --     package
    --
    -- -   You can specify a maximum of 100 port ranges per container.
    --
    -- -   You do not specify a @hostPortRange@. The value of the
    --     @hostPortRange@ is set as follows:
    --
    --     -   For containers in a task with the @awsvpc@ network mode, the
    --         @hostPort@ is set to the same value as the @containerPort@. This
    --         is a static mapping strategy.
    --
    --     -   For containers in a task with the @bridge@ network mode, the
    --         Amazon ECS agent finds open host ports from the default
    --         ephemeral range and passes it to docker to bind them to the
    --         container ports.
    --
    -- -   The @containerPortRange@ valid values are between 1 and 65535.
    --
    -- -   A port can only be included in one port mapping per container.
    --
    -- -   You cannot specify overlapping port ranges.
    --
    -- -   The first port in the range must be less than last port in the
    --     range.
    --
    -- -   Docker recommends that you turn off the docker-proxy in the Docker
    --     daemon config file when you have a large number of ports.
    --
    --     For more information, see
    --     <https://github.com/moby/moby/issues/11185 Issue #11185> on the
    --     Github website.
    --
    --     For information about how to turn off the docker-proxy in the Docker
    --     daemon config file, see
    --     <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/bootstrap_container_instance.html#bootstrap_docker_daemon Docker daemon>
    --     in the /Amazon ECS Developer Guide/.
    --
    -- You can call
    -- <https://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_DescribeTasks.html DescribeTasks>
    -- to view the @hostPortRange@ which are the host ports that are bound to
    -- the container ports.
    containerPortRange :: Prelude.Maybe Prelude.Text,
    -- | The port number on the container instance to reserve for your container.
    --
    -- If you specify a @containerPortRange@, leave this field empty and the
    -- value of the @hostPort@ is set as follows:
    --
    -- -   For containers in a task with the @awsvpc@ network mode, the
    --     @hostPort@ is set to the same value as the @containerPort@. This is
    --     a static mapping strategy.
    --
    -- -   For containers in a task with the @bridge@ network mode, the Amazon
    --     ECS agent finds open ports on the host and automaticaly binds them
    --     to the container ports. This is a dynamic mapping strategy.
    --
    -- If you use containers in a task with the @awsvpc@ or @host@ network
    -- mode, the @hostPort@ can either be left blank or set to the same value
    -- as the @containerPort@.
    --
    -- If you use containers in a task with the @bridge@ network mode, you can
    -- specify a non-reserved host port for your container port mapping, or you
    -- can omit the @hostPort@ (or set it to @0@) while specifying a
    -- @containerPort@ and your container automatically receives a port in the
    -- ephemeral port range for your container instance operating system and
    -- Docker version.
    --
    -- The default ephemeral port range for Docker version 1.6.0 and later is
    -- listed on the instance under
    -- @\/proc\/sys\/net\/ipv4\/ip_local_port_range@. If this kernel parameter
    -- is unavailable, the default ephemeral port range from 49153 through
    -- 65535 is used. Do not attempt to specify a host port in the ephemeral
    -- port range as these are reserved for automatic assignment. In general,
    -- ports below 32768 are outside of the ephemeral port range.
    --
    -- The default reserved ports are 22 for SSH, the Docker ports 2375 and
    -- 2376, and the Amazon ECS container agent ports 51678-51680. Any host
    -- port that was previously specified in a running task is also reserved
    -- while the task is running. That is, after a task stops, the host port is
    -- released. The current reserved ports are displayed in the
    -- @remainingResources@ of DescribeContainerInstances output. A container
    -- instance can have up to 100 reserved ports at a time. This number
    -- includes the default reserved ports. Automatically assigned ports
    -- aren\'t included in the 100 reserved ports quota.
    hostPort :: Prelude.Maybe Prelude.Int,
    -- | The name that\'s used for the port mapping. This parameter only applies
    -- to Service Connect. This parameter is the name that you use in the
    -- @serviceConnectConfiguration@ of a service. The name can include up to
    -- 64 characters. The characters can include lowercase letters, numbers,
    -- underscores (_), and hyphens (-). The name can\'t start with a hyphen.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-connect.html Service Connect>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    name :: Prelude.Maybe Prelude.Text,
    -- | The protocol used for the port mapping. Valid values are @tcp@ and
    -- @udp@. The default is @tcp@.
    protocol :: Prelude.Maybe TransportProtocol
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PortMapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appProtocol', 'portMapping_appProtocol' - The application protocol that\'s used for the port mapping. This
-- parameter only applies to Service Connect. We recommend that you set
-- this parameter to be consistent with the protocol that your application
-- uses. If you set this parameter, Amazon ECS adds protocol-specific
-- connection handling to the Service Connect proxy. If you set this
-- parameter, Amazon ECS adds protocol-specific telemetry in the Amazon ECS
-- console and CloudWatch.
--
-- If you don\'t set a value for this parameter, then TCP is used. However,
-- Amazon ECS doesn\'t add protocol-specific telemetry for TCP.
--
-- Tasks that run in a namespace can use short names to connect to services
-- in the namespace. Tasks can connect to services across all of the
-- clusters in the namespace. Tasks connect through a managed proxy
-- container that collects logs and metrics for increased visibility. Only
-- the tasks that Amazon ECS services create are supported with Service
-- Connect. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-connect.html Service Connect>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'containerPort', 'portMapping_containerPort' - The port number on the container that\'s bound to the user-specified or
-- automatically assigned host port.
--
-- If you use containers in a task with the @awsvpc@ or @host@ network
-- mode, specify the exposed ports using @containerPort@.
--
-- If you use containers in a task with the @bridge@ network mode and you
-- specify a container port and not a host port, your container
-- automatically receives a host port in the ephemeral port range. For more
-- information, see @hostPort@. Port mappings that are automatically
-- assigned in this way do not count toward the 100 reserved ports limit of
-- a container instance.
--
-- 'containerPortRange', 'portMapping_containerPortRange' - The port number range on the container that\'s bound to the dynamically
-- mapped host port range.
--
-- The following rules apply when you specify a @containerPortRange@:
--
-- -   You must use either the @bridge@ network mode or the @awsvpc@
--     network mode.
--
-- -   This parameter is available for both the EC2 and Fargate launch
--     types.
--
-- -   This parameter is available for both the Linux and Windows operating
--     systems.
--
-- -   The container instance must have at least version 1.67.0 of the
--     container agent and at least version 1.67.0-1 of the @ecs-init@
--     package
--
-- -   You can specify a maximum of 100 port ranges per container.
--
-- -   You do not specify a @hostPortRange@. The value of the
--     @hostPortRange@ is set as follows:
--
--     -   For containers in a task with the @awsvpc@ network mode, the
--         @hostPort@ is set to the same value as the @containerPort@. This
--         is a static mapping strategy.
--
--     -   For containers in a task with the @bridge@ network mode, the
--         Amazon ECS agent finds open host ports from the default
--         ephemeral range and passes it to docker to bind them to the
--         container ports.
--
-- -   The @containerPortRange@ valid values are between 1 and 65535.
--
-- -   A port can only be included in one port mapping per container.
--
-- -   You cannot specify overlapping port ranges.
--
-- -   The first port in the range must be less than last port in the
--     range.
--
-- -   Docker recommends that you turn off the docker-proxy in the Docker
--     daemon config file when you have a large number of ports.
--
--     For more information, see
--     <https://github.com/moby/moby/issues/11185 Issue #11185> on the
--     Github website.
--
--     For information about how to turn off the docker-proxy in the Docker
--     daemon config file, see
--     <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/bootstrap_container_instance.html#bootstrap_docker_daemon Docker daemon>
--     in the /Amazon ECS Developer Guide/.
--
-- You can call
-- <https://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_DescribeTasks.html DescribeTasks>
-- to view the @hostPortRange@ which are the host ports that are bound to
-- the container ports.
--
-- 'hostPort', 'portMapping_hostPort' - The port number on the container instance to reserve for your container.
--
-- If you specify a @containerPortRange@, leave this field empty and the
-- value of the @hostPort@ is set as follows:
--
-- -   For containers in a task with the @awsvpc@ network mode, the
--     @hostPort@ is set to the same value as the @containerPort@. This is
--     a static mapping strategy.
--
-- -   For containers in a task with the @bridge@ network mode, the Amazon
--     ECS agent finds open ports on the host and automaticaly binds them
--     to the container ports. This is a dynamic mapping strategy.
--
-- If you use containers in a task with the @awsvpc@ or @host@ network
-- mode, the @hostPort@ can either be left blank or set to the same value
-- as the @containerPort@.
--
-- If you use containers in a task with the @bridge@ network mode, you can
-- specify a non-reserved host port for your container port mapping, or you
-- can omit the @hostPort@ (or set it to @0@) while specifying a
-- @containerPort@ and your container automatically receives a port in the
-- ephemeral port range for your container instance operating system and
-- Docker version.
--
-- The default ephemeral port range for Docker version 1.6.0 and later is
-- listed on the instance under
-- @\/proc\/sys\/net\/ipv4\/ip_local_port_range@. If this kernel parameter
-- is unavailable, the default ephemeral port range from 49153 through
-- 65535 is used. Do not attempt to specify a host port in the ephemeral
-- port range as these are reserved for automatic assignment. In general,
-- ports below 32768 are outside of the ephemeral port range.
--
-- The default reserved ports are 22 for SSH, the Docker ports 2375 and
-- 2376, and the Amazon ECS container agent ports 51678-51680. Any host
-- port that was previously specified in a running task is also reserved
-- while the task is running. That is, after a task stops, the host port is
-- released. The current reserved ports are displayed in the
-- @remainingResources@ of DescribeContainerInstances output. A container
-- instance can have up to 100 reserved ports at a time. This number
-- includes the default reserved ports. Automatically assigned ports
-- aren\'t included in the 100 reserved ports quota.
--
-- 'name', 'portMapping_name' - The name that\'s used for the port mapping. This parameter only applies
-- to Service Connect. This parameter is the name that you use in the
-- @serviceConnectConfiguration@ of a service. The name can include up to
-- 64 characters. The characters can include lowercase letters, numbers,
-- underscores (_), and hyphens (-). The name can\'t start with a hyphen.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-connect.html Service Connect>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'protocol', 'portMapping_protocol' - The protocol used for the port mapping. Valid values are @tcp@ and
-- @udp@. The default is @tcp@.
newPortMapping ::
  PortMapping
newPortMapping =
  PortMapping'
    { appProtocol = Prelude.Nothing,
      containerPort = Prelude.Nothing,
      containerPortRange = Prelude.Nothing,
      hostPort = Prelude.Nothing,
      name = Prelude.Nothing,
      protocol = Prelude.Nothing
    }

-- | The application protocol that\'s used for the port mapping. This
-- parameter only applies to Service Connect. We recommend that you set
-- this parameter to be consistent with the protocol that your application
-- uses. If you set this parameter, Amazon ECS adds protocol-specific
-- connection handling to the Service Connect proxy. If you set this
-- parameter, Amazon ECS adds protocol-specific telemetry in the Amazon ECS
-- console and CloudWatch.
--
-- If you don\'t set a value for this parameter, then TCP is used. However,
-- Amazon ECS doesn\'t add protocol-specific telemetry for TCP.
--
-- Tasks that run in a namespace can use short names to connect to services
-- in the namespace. Tasks can connect to services across all of the
-- clusters in the namespace. Tasks connect through a managed proxy
-- container that collects logs and metrics for increased visibility. Only
-- the tasks that Amazon ECS services create are supported with Service
-- Connect. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-connect.html Service Connect>
-- in the /Amazon Elastic Container Service Developer Guide/.
portMapping_appProtocol :: Lens.Lens' PortMapping (Prelude.Maybe ApplicationProtocol)
portMapping_appProtocol = Lens.lens (\PortMapping' {appProtocol} -> appProtocol) (\s@PortMapping' {} a -> s {appProtocol = a} :: PortMapping)

-- | The port number on the container that\'s bound to the user-specified or
-- automatically assigned host port.
--
-- If you use containers in a task with the @awsvpc@ or @host@ network
-- mode, specify the exposed ports using @containerPort@.
--
-- If you use containers in a task with the @bridge@ network mode and you
-- specify a container port and not a host port, your container
-- automatically receives a host port in the ephemeral port range. For more
-- information, see @hostPort@. Port mappings that are automatically
-- assigned in this way do not count toward the 100 reserved ports limit of
-- a container instance.
portMapping_containerPort :: Lens.Lens' PortMapping (Prelude.Maybe Prelude.Int)
portMapping_containerPort = Lens.lens (\PortMapping' {containerPort} -> containerPort) (\s@PortMapping' {} a -> s {containerPort = a} :: PortMapping)

-- | The port number range on the container that\'s bound to the dynamically
-- mapped host port range.
--
-- The following rules apply when you specify a @containerPortRange@:
--
-- -   You must use either the @bridge@ network mode or the @awsvpc@
--     network mode.
--
-- -   This parameter is available for both the EC2 and Fargate launch
--     types.
--
-- -   This parameter is available for both the Linux and Windows operating
--     systems.
--
-- -   The container instance must have at least version 1.67.0 of the
--     container agent and at least version 1.67.0-1 of the @ecs-init@
--     package
--
-- -   You can specify a maximum of 100 port ranges per container.
--
-- -   You do not specify a @hostPortRange@. The value of the
--     @hostPortRange@ is set as follows:
--
--     -   For containers in a task with the @awsvpc@ network mode, the
--         @hostPort@ is set to the same value as the @containerPort@. This
--         is a static mapping strategy.
--
--     -   For containers in a task with the @bridge@ network mode, the
--         Amazon ECS agent finds open host ports from the default
--         ephemeral range and passes it to docker to bind them to the
--         container ports.
--
-- -   The @containerPortRange@ valid values are between 1 and 65535.
--
-- -   A port can only be included in one port mapping per container.
--
-- -   You cannot specify overlapping port ranges.
--
-- -   The first port in the range must be less than last port in the
--     range.
--
-- -   Docker recommends that you turn off the docker-proxy in the Docker
--     daemon config file when you have a large number of ports.
--
--     For more information, see
--     <https://github.com/moby/moby/issues/11185 Issue #11185> on the
--     Github website.
--
--     For information about how to turn off the docker-proxy in the Docker
--     daemon config file, see
--     <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/bootstrap_container_instance.html#bootstrap_docker_daemon Docker daemon>
--     in the /Amazon ECS Developer Guide/.
--
-- You can call
-- <https://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_DescribeTasks.html DescribeTasks>
-- to view the @hostPortRange@ which are the host ports that are bound to
-- the container ports.
portMapping_containerPortRange :: Lens.Lens' PortMapping (Prelude.Maybe Prelude.Text)
portMapping_containerPortRange = Lens.lens (\PortMapping' {containerPortRange} -> containerPortRange) (\s@PortMapping' {} a -> s {containerPortRange = a} :: PortMapping)

-- | The port number on the container instance to reserve for your container.
--
-- If you specify a @containerPortRange@, leave this field empty and the
-- value of the @hostPort@ is set as follows:
--
-- -   For containers in a task with the @awsvpc@ network mode, the
--     @hostPort@ is set to the same value as the @containerPort@. This is
--     a static mapping strategy.
--
-- -   For containers in a task with the @bridge@ network mode, the Amazon
--     ECS agent finds open ports on the host and automaticaly binds them
--     to the container ports. This is a dynamic mapping strategy.
--
-- If you use containers in a task with the @awsvpc@ or @host@ network
-- mode, the @hostPort@ can either be left blank or set to the same value
-- as the @containerPort@.
--
-- If you use containers in a task with the @bridge@ network mode, you can
-- specify a non-reserved host port for your container port mapping, or you
-- can omit the @hostPort@ (or set it to @0@) while specifying a
-- @containerPort@ and your container automatically receives a port in the
-- ephemeral port range for your container instance operating system and
-- Docker version.
--
-- The default ephemeral port range for Docker version 1.6.0 and later is
-- listed on the instance under
-- @\/proc\/sys\/net\/ipv4\/ip_local_port_range@. If this kernel parameter
-- is unavailable, the default ephemeral port range from 49153 through
-- 65535 is used. Do not attempt to specify a host port in the ephemeral
-- port range as these are reserved for automatic assignment. In general,
-- ports below 32768 are outside of the ephemeral port range.
--
-- The default reserved ports are 22 for SSH, the Docker ports 2375 and
-- 2376, and the Amazon ECS container agent ports 51678-51680. Any host
-- port that was previously specified in a running task is also reserved
-- while the task is running. That is, after a task stops, the host port is
-- released. The current reserved ports are displayed in the
-- @remainingResources@ of DescribeContainerInstances output. A container
-- instance can have up to 100 reserved ports at a time. This number
-- includes the default reserved ports. Automatically assigned ports
-- aren\'t included in the 100 reserved ports quota.
portMapping_hostPort :: Lens.Lens' PortMapping (Prelude.Maybe Prelude.Int)
portMapping_hostPort = Lens.lens (\PortMapping' {hostPort} -> hostPort) (\s@PortMapping' {} a -> s {hostPort = a} :: PortMapping)

-- | The name that\'s used for the port mapping. This parameter only applies
-- to Service Connect. This parameter is the name that you use in the
-- @serviceConnectConfiguration@ of a service. The name can include up to
-- 64 characters. The characters can include lowercase letters, numbers,
-- underscores (_), and hyphens (-). The name can\'t start with a hyphen.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-connect.html Service Connect>
-- in the /Amazon Elastic Container Service Developer Guide/.
portMapping_name :: Lens.Lens' PortMapping (Prelude.Maybe Prelude.Text)
portMapping_name = Lens.lens (\PortMapping' {name} -> name) (\s@PortMapping' {} a -> s {name = a} :: PortMapping)

-- | The protocol used for the port mapping. Valid values are @tcp@ and
-- @udp@. The default is @tcp@.
portMapping_protocol :: Lens.Lens' PortMapping (Prelude.Maybe TransportProtocol)
portMapping_protocol = Lens.lens (\PortMapping' {protocol} -> protocol) (\s@PortMapping' {} a -> s {protocol = a} :: PortMapping)

instance Data.FromJSON PortMapping where
  parseJSON =
    Data.withObject
      "PortMapping"
      ( \x ->
          PortMapping'
            Prelude.<$> (x Data..:? "appProtocol")
            Prelude.<*> (x Data..:? "containerPort")
            Prelude.<*> (x Data..:? "containerPortRange")
            Prelude.<*> (x Data..:? "hostPort")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "protocol")
      )

instance Prelude.Hashable PortMapping where
  hashWithSalt _salt PortMapping' {..} =
    _salt
      `Prelude.hashWithSalt` appProtocol
      `Prelude.hashWithSalt` containerPort
      `Prelude.hashWithSalt` containerPortRange
      `Prelude.hashWithSalt` hostPort
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` protocol

instance Prelude.NFData PortMapping where
  rnf PortMapping' {..} =
    Prelude.rnf appProtocol `Prelude.seq`
      Prelude.rnf containerPort `Prelude.seq`
        Prelude.rnf containerPortRange `Prelude.seq`
          Prelude.rnf hostPort `Prelude.seq`
            Prelude.rnf name `Prelude.seq`
              Prelude.rnf protocol

instance Data.ToJSON PortMapping where
  toJSON PortMapping' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("appProtocol" Data..=) Prelude.<$> appProtocol,
            ("containerPort" Data..=) Prelude.<$> containerPort,
            ("containerPortRange" Data..=)
              Prelude.<$> containerPortRange,
            ("hostPort" Data..=) Prelude.<$> hostPort,
            ("name" Data..=) Prelude.<$> name,
            ("protocol" Data..=) Prelude.<$> protocol
          ]
      )
