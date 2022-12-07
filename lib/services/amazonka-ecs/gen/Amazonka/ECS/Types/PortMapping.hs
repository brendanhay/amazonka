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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.PortMapping where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
  { -- | The port number on the container that\'s bound to the user-specified or
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
    -- | The port number on the container instance to reserve for your container.
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
-- 'hostPort', 'portMapping_hostPort' - The port number on the container instance to reserve for your container.
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
-- 'protocol', 'portMapping_protocol' - The protocol used for the port mapping. Valid values are @tcp@ and
-- @udp@. The default is @tcp@.
newPortMapping ::
  PortMapping
newPortMapping =
  PortMapping'
    { containerPort = Prelude.Nothing,
      hostPort = Prelude.Nothing,
      protocol = Prelude.Nothing
    }

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

-- | The port number on the container instance to reserve for your container.
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
            Prelude.<$> (x Data..:? "containerPort")
            Prelude.<*> (x Data..:? "hostPort")
            Prelude.<*> (x Data..:? "protocol")
      )

instance Prelude.Hashable PortMapping where
  hashWithSalt _salt PortMapping' {..} =
    _salt `Prelude.hashWithSalt` containerPort
      `Prelude.hashWithSalt` hostPort
      `Prelude.hashWithSalt` protocol

instance Prelude.NFData PortMapping where
  rnf PortMapping' {..} =
    Prelude.rnf containerPort
      `Prelude.seq` Prelude.rnf hostPort
      `Prelude.seq` Prelude.rnf protocol

instance Data.ToJSON PortMapping where
  toJSON PortMapping' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("containerPort" Data..=) Prelude.<$> containerPort,
            ("hostPort" Data..=) Prelude.<$> hostPort,
            ("protocol" Data..=) Prelude.<$> protocol
          ]
      )
