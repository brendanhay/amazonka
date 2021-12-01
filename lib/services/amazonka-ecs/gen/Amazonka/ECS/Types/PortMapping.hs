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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.PortMapping where

import qualified Amazonka.Core as Core
import Amazonka.ECS.Types.TransportProtocol
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Port mappings allow containers to access ports on the host container
-- instance to send or receive traffic. Port mappings are specified as part
-- of the container definition.
--
-- If you are using containers in a task with the @awsvpc@ or @host@
-- network mode, exposed ports should be specified using @containerPort@.
-- The @hostPort@ can be left blank or it must be the same value as the
-- @containerPort@.
--
-- You cannot expose the same container port for multiple protocols. An
-- error will be returned if this is attempted
--
-- After a task reaches the @RUNNING@ status, manual and automatic host and
-- container port assignments are visible in the @networkBindings@ section
-- of DescribeTasks API responses.
--
-- /See:/ 'newPortMapping' smart constructor.
data PortMapping = PortMapping'
  { -- | The protocol used for the port mapping. Valid values are @tcp@ and
    -- @udp@. The default is @tcp@.
    protocol :: Prelude.Maybe TransportProtocol,
    -- | The port number on the container instance to reserve for your container.
    --
    -- If you are using containers in a task with the @awsvpc@ or @host@
    -- network mode, the @hostPort@ can either be left blank or set to the same
    -- value as the @containerPort@.
    --
    -- If you are using containers in a task with the @bridge@ network mode,
    -- you can specify a non-reserved host port for your container port
    -- mapping, or you can omit the @hostPort@ (or set it to @0@) while
    -- specifying a @containerPort@ and your container automatically receives a
    -- port in the ephemeral port range for your container instance operating
    -- system and Docker version.
    --
    -- The default ephemeral port range for Docker version 1.6.0 and later is
    -- listed on the instance under
    -- @\/proc\/sys\/net\/ipv4\/ip_local_port_range@. If this kernel parameter
    -- is unavailable, the default ephemeral port range from 49153 through
    -- 65535 is used. Do not attempt to specify a host port in the ephemeral
    -- port range as these are reserved for automatic assignment. In general,
    -- ports below 32768 are outside of the ephemeral port range.
    --
    -- The default ephemeral port range from 49153 through 65535 is always used
    -- for Docker versions before 1.6.0.
    --
    -- The default reserved ports are 22 for SSH, the Docker ports 2375 and
    -- 2376, and the Amazon ECS container agent ports 51678-51680. Any host
    -- port that was previously specified in a running task is also reserved
    -- while the task is running (after a task stops, the host port is
    -- released). The current reserved ports are displayed in the
    -- @remainingResources@ of DescribeContainerInstances output. A container
    -- instance can have up to 100 reserved ports at a time, including the
    -- default reserved ports. Automatically assigned ports don\'t count toward
    -- the 100 reserved ports limit.
    hostPort :: Prelude.Maybe Prelude.Int,
    -- | The port number on the container that is bound to the user-specified or
    -- automatically assigned host port.
    --
    -- If you are using containers in a task with the @awsvpc@ or @host@
    -- network mode, exposed ports should be specified using @containerPort@.
    --
    -- If you are using containers in a task with the @bridge@ network mode and
    -- you specify a container port and not a host port, your container
    -- automatically receives a host port in the ephemeral port range. For more
    -- information, see @hostPort@. Port mappings that are automatically
    -- assigned in this way do not count toward the 100 reserved ports limit of
    -- a container instance.
    containerPort :: Prelude.Maybe Prelude.Int
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
-- 'protocol', 'portMapping_protocol' - The protocol used for the port mapping. Valid values are @tcp@ and
-- @udp@. The default is @tcp@.
--
-- 'hostPort', 'portMapping_hostPort' - The port number on the container instance to reserve for your container.
--
-- If you are using containers in a task with the @awsvpc@ or @host@
-- network mode, the @hostPort@ can either be left blank or set to the same
-- value as the @containerPort@.
--
-- If you are using containers in a task with the @bridge@ network mode,
-- you can specify a non-reserved host port for your container port
-- mapping, or you can omit the @hostPort@ (or set it to @0@) while
-- specifying a @containerPort@ and your container automatically receives a
-- port in the ephemeral port range for your container instance operating
-- system and Docker version.
--
-- The default ephemeral port range for Docker version 1.6.0 and later is
-- listed on the instance under
-- @\/proc\/sys\/net\/ipv4\/ip_local_port_range@. If this kernel parameter
-- is unavailable, the default ephemeral port range from 49153 through
-- 65535 is used. Do not attempt to specify a host port in the ephemeral
-- port range as these are reserved for automatic assignment. In general,
-- ports below 32768 are outside of the ephemeral port range.
--
-- The default ephemeral port range from 49153 through 65535 is always used
-- for Docker versions before 1.6.0.
--
-- The default reserved ports are 22 for SSH, the Docker ports 2375 and
-- 2376, and the Amazon ECS container agent ports 51678-51680. Any host
-- port that was previously specified in a running task is also reserved
-- while the task is running (after a task stops, the host port is
-- released). The current reserved ports are displayed in the
-- @remainingResources@ of DescribeContainerInstances output. A container
-- instance can have up to 100 reserved ports at a time, including the
-- default reserved ports. Automatically assigned ports don\'t count toward
-- the 100 reserved ports limit.
--
-- 'containerPort', 'portMapping_containerPort' - The port number on the container that is bound to the user-specified or
-- automatically assigned host port.
--
-- If you are using containers in a task with the @awsvpc@ or @host@
-- network mode, exposed ports should be specified using @containerPort@.
--
-- If you are using containers in a task with the @bridge@ network mode and
-- you specify a container port and not a host port, your container
-- automatically receives a host port in the ephemeral port range. For more
-- information, see @hostPort@. Port mappings that are automatically
-- assigned in this way do not count toward the 100 reserved ports limit of
-- a container instance.
newPortMapping ::
  PortMapping
newPortMapping =
  PortMapping'
    { protocol = Prelude.Nothing,
      hostPort = Prelude.Nothing,
      containerPort = Prelude.Nothing
    }

-- | The protocol used for the port mapping. Valid values are @tcp@ and
-- @udp@. The default is @tcp@.
portMapping_protocol :: Lens.Lens' PortMapping (Prelude.Maybe TransportProtocol)
portMapping_protocol = Lens.lens (\PortMapping' {protocol} -> protocol) (\s@PortMapping' {} a -> s {protocol = a} :: PortMapping)

-- | The port number on the container instance to reserve for your container.
--
-- If you are using containers in a task with the @awsvpc@ or @host@
-- network mode, the @hostPort@ can either be left blank or set to the same
-- value as the @containerPort@.
--
-- If you are using containers in a task with the @bridge@ network mode,
-- you can specify a non-reserved host port for your container port
-- mapping, or you can omit the @hostPort@ (or set it to @0@) while
-- specifying a @containerPort@ and your container automatically receives a
-- port in the ephemeral port range for your container instance operating
-- system and Docker version.
--
-- The default ephemeral port range for Docker version 1.6.0 and later is
-- listed on the instance under
-- @\/proc\/sys\/net\/ipv4\/ip_local_port_range@. If this kernel parameter
-- is unavailable, the default ephemeral port range from 49153 through
-- 65535 is used. Do not attempt to specify a host port in the ephemeral
-- port range as these are reserved for automatic assignment. In general,
-- ports below 32768 are outside of the ephemeral port range.
--
-- The default ephemeral port range from 49153 through 65535 is always used
-- for Docker versions before 1.6.0.
--
-- The default reserved ports are 22 for SSH, the Docker ports 2375 and
-- 2376, and the Amazon ECS container agent ports 51678-51680. Any host
-- port that was previously specified in a running task is also reserved
-- while the task is running (after a task stops, the host port is
-- released). The current reserved ports are displayed in the
-- @remainingResources@ of DescribeContainerInstances output. A container
-- instance can have up to 100 reserved ports at a time, including the
-- default reserved ports. Automatically assigned ports don\'t count toward
-- the 100 reserved ports limit.
portMapping_hostPort :: Lens.Lens' PortMapping (Prelude.Maybe Prelude.Int)
portMapping_hostPort = Lens.lens (\PortMapping' {hostPort} -> hostPort) (\s@PortMapping' {} a -> s {hostPort = a} :: PortMapping)

-- | The port number on the container that is bound to the user-specified or
-- automatically assigned host port.
--
-- If you are using containers in a task with the @awsvpc@ or @host@
-- network mode, exposed ports should be specified using @containerPort@.
--
-- If you are using containers in a task with the @bridge@ network mode and
-- you specify a container port and not a host port, your container
-- automatically receives a host port in the ephemeral port range. For more
-- information, see @hostPort@. Port mappings that are automatically
-- assigned in this way do not count toward the 100 reserved ports limit of
-- a container instance.
portMapping_containerPort :: Lens.Lens' PortMapping (Prelude.Maybe Prelude.Int)
portMapping_containerPort = Lens.lens (\PortMapping' {containerPort} -> containerPort) (\s@PortMapping' {} a -> s {containerPort = a} :: PortMapping)

instance Core.FromJSON PortMapping where
  parseJSON =
    Core.withObject
      "PortMapping"
      ( \x ->
          PortMapping'
            Prelude.<$> (x Core..:? "protocol")
            Prelude.<*> (x Core..:? "hostPort")
            Prelude.<*> (x Core..:? "containerPort")
      )

instance Prelude.Hashable PortMapping where
  hashWithSalt salt' PortMapping' {..} =
    salt' `Prelude.hashWithSalt` containerPort
      `Prelude.hashWithSalt` hostPort
      `Prelude.hashWithSalt` protocol

instance Prelude.NFData PortMapping where
  rnf PortMapping' {..} =
    Prelude.rnf protocol
      `Prelude.seq` Prelude.rnf containerPort
      `Prelude.seq` Prelude.rnf hostPort

instance Core.ToJSON PortMapping where
  toJSON PortMapping' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("protocol" Core..=) Prelude.<$> protocol,
            ("hostPort" Core..=) Prelude.<$> hostPort,
            ("containerPort" Core..=) Prelude.<$> containerPort
          ]
      )
