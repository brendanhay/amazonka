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
-- Module      : Amazonka.ECS.Types.NetworkBinding
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.NetworkBinding where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECS.Types.TransportProtocol
import qualified Amazonka.Prelude as Prelude

-- | Details on the network bindings between a container and its host
-- container instance. After a task reaches the @RUNNING@ status, manual
-- and automatic host and container port assignments are visible in the
-- @networkBindings@ section of DescribeTasks API responses.
--
-- /See:/ 'newNetworkBinding' smart constructor.
data NetworkBinding = NetworkBinding'
  { -- | The IP address that the container is bound to on the container instance.
    bindIP :: Prelude.Maybe Prelude.Text,
    -- | The port number on the container that\'s used with the network binding.
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
    -- | The port number on the host that\'s used with the network binding.
    hostPort :: Prelude.Maybe Prelude.Int,
    -- | The port number range on the host that\'s used with the network binding.
    -- This is assigned is assigned by Docker and delivered by the Amazon ECS
    -- agent.
    hostPortRange :: Prelude.Maybe Prelude.Text,
    -- | The protocol used for the network binding.
    protocol :: Prelude.Maybe TransportProtocol
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetworkBinding' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bindIP', 'networkBinding_bindIP' - The IP address that the container is bound to on the container instance.
--
-- 'containerPort', 'networkBinding_containerPort' - The port number on the container that\'s used with the network binding.
--
-- 'containerPortRange', 'networkBinding_containerPortRange' - The port number range on the container that\'s bound to the dynamically
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
-- 'hostPort', 'networkBinding_hostPort' - The port number on the host that\'s used with the network binding.
--
-- 'hostPortRange', 'networkBinding_hostPortRange' - The port number range on the host that\'s used with the network binding.
-- This is assigned is assigned by Docker and delivered by the Amazon ECS
-- agent.
--
-- 'protocol', 'networkBinding_protocol' - The protocol used for the network binding.
newNetworkBinding ::
  NetworkBinding
newNetworkBinding =
  NetworkBinding'
    { bindIP = Prelude.Nothing,
      containerPort = Prelude.Nothing,
      containerPortRange = Prelude.Nothing,
      hostPort = Prelude.Nothing,
      hostPortRange = Prelude.Nothing,
      protocol = Prelude.Nothing
    }

-- | The IP address that the container is bound to on the container instance.
networkBinding_bindIP :: Lens.Lens' NetworkBinding (Prelude.Maybe Prelude.Text)
networkBinding_bindIP = Lens.lens (\NetworkBinding' {bindIP} -> bindIP) (\s@NetworkBinding' {} a -> s {bindIP = a} :: NetworkBinding)

-- | The port number on the container that\'s used with the network binding.
networkBinding_containerPort :: Lens.Lens' NetworkBinding (Prelude.Maybe Prelude.Int)
networkBinding_containerPort = Lens.lens (\NetworkBinding' {containerPort} -> containerPort) (\s@NetworkBinding' {} a -> s {containerPort = a} :: NetworkBinding)

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
networkBinding_containerPortRange :: Lens.Lens' NetworkBinding (Prelude.Maybe Prelude.Text)
networkBinding_containerPortRange = Lens.lens (\NetworkBinding' {containerPortRange} -> containerPortRange) (\s@NetworkBinding' {} a -> s {containerPortRange = a} :: NetworkBinding)

-- | The port number on the host that\'s used with the network binding.
networkBinding_hostPort :: Lens.Lens' NetworkBinding (Prelude.Maybe Prelude.Int)
networkBinding_hostPort = Lens.lens (\NetworkBinding' {hostPort} -> hostPort) (\s@NetworkBinding' {} a -> s {hostPort = a} :: NetworkBinding)

-- | The port number range on the host that\'s used with the network binding.
-- This is assigned is assigned by Docker and delivered by the Amazon ECS
-- agent.
networkBinding_hostPortRange :: Lens.Lens' NetworkBinding (Prelude.Maybe Prelude.Text)
networkBinding_hostPortRange = Lens.lens (\NetworkBinding' {hostPortRange} -> hostPortRange) (\s@NetworkBinding' {} a -> s {hostPortRange = a} :: NetworkBinding)

-- | The protocol used for the network binding.
networkBinding_protocol :: Lens.Lens' NetworkBinding (Prelude.Maybe TransportProtocol)
networkBinding_protocol = Lens.lens (\NetworkBinding' {protocol} -> protocol) (\s@NetworkBinding' {} a -> s {protocol = a} :: NetworkBinding)

instance Data.FromJSON NetworkBinding where
  parseJSON =
    Data.withObject
      "NetworkBinding"
      ( \x ->
          NetworkBinding'
            Prelude.<$> (x Data..:? "bindIP")
            Prelude.<*> (x Data..:? "containerPort")
            Prelude.<*> (x Data..:? "containerPortRange")
            Prelude.<*> (x Data..:? "hostPort")
            Prelude.<*> (x Data..:? "hostPortRange")
            Prelude.<*> (x Data..:? "protocol")
      )

instance Prelude.Hashable NetworkBinding where
  hashWithSalt _salt NetworkBinding' {..} =
    _salt
      `Prelude.hashWithSalt` bindIP
      `Prelude.hashWithSalt` containerPort
      `Prelude.hashWithSalt` containerPortRange
      `Prelude.hashWithSalt` hostPort
      `Prelude.hashWithSalt` hostPortRange
      `Prelude.hashWithSalt` protocol

instance Prelude.NFData NetworkBinding where
  rnf NetworkBinding' {..} =
    Prelude.rnf bindIP
      `Prelude.seq` Prelude.rnf containerPort
      `Prelude.seq` Prelude.rnf containerPortRange
      `Prelude.seq` Prelude.rnf hostPort
      `Prelude.seq` Prelude.rnf hostPortRange
      `Prelude.seq` Prelude.rnf protocol

instance Data.ToJSON NetworkBinding where
  toJSON NetworkBinding' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("bindIP" Data..=) Prelude.<$> bindIP,
            ("containerPort" Data..=) Prelude.<$> containerPort,
            ("containerPortRange" Data..=)
              Prelude.<$> containerPortRange,
            ("hostPort" Data..=) Prelude.<$> hostPort,
            ("hostPortRange" Data..=) Prelude.<$> hostPortRange,
            ("protocol" Data..=) Prelude.<$> protocol
          ]
      )
