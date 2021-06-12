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
-- Module      : Network.AWS.ECS.Types.NetworkBinding
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.NetworkBinding where

import qualified Network.AWS.Core as Core
import Network.AWS.ECS.Types.TransportProtocol
import qualified Network.AWS.Lens as Lens

-- | Details on the network bindings between a container and its host
-- container instance. After a task reaches the @RUNNING@ status, manual
-- and automatic host and container port assignments are visible in the
-- @networkBindings@ section of DescribeTasks API responses.
--
-- /See:/ 'newNetworkBinding' smart constructor.
data NetworkBinding = NetworkBinding'
  { -- | The port number on the host that is used with the network binding.
    hostPort :: Core.Maybe Core.Int,
    -- | The IP address that the container is bound to on the container instance.
    bindIP :: Core.Maybe Core.Text,
    -- | The protocol used for the network binding.
    protocol :: Core.Maybe TransportProtocol,
    -- | The port number on the container that is used with the network binding.
    containerPort :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'NetworkBinding' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hostPort', 'networkBinding_hostPort' - The port number on the host that is used with the network binding.
--
-- 'bindIP', 'networkBinding_bindIP' - The IP address that the container is bound to on the container instance.
--
-- 'protocol', 'networkBinding_protocol' - The protocol used for the network binding.
--
-- 'containerPort', 'networkBinding_containerPort' - The port number on the container that is used with the network binding.
newNetworkBinding ::
  NetworkBinding
newNetworkBinding =
  NetworkBinding'
    { hostPort = Core.Nothing,
      bindIP = Core.Nothing,
      protocol = Core.Nothing,
      containerPort = Core.Nothing
    }

-- | The port number on the host that is used with the network binding.
networkBinding_hostPort :: Lens.Lens' NetworkBinding (Core.Maybe Core.Int)
networkBinding_hostPort = Lens.lens (\NetworkBinding' {hostPort} -> hostPort) (\s@NetworkBinding' {} a -> s {hostPort = a} :: NetworkBinding)

-- | The IP address that the container is bound to on the container instance.
networkBinding_bindIP :: Lens.Lens' NetworkBinding (Core.Maybe Core.Text)
networkBinding_bindIP = Lens.lens (\NetworkBinding' {bindIP} -> bindIP) (\s@NetworkBinding' {} a -> s {bindIP = a} :: NetworkBinding)

-- | The protocol used for the network binding.
networkBinding_protocol :: Lens.Lens' NetworkBinding (Core.Maybe TransportProtocol)
networkBinding_protocol = Lens.lens (\NetworkBinding' {protocol} -> protocol) (\s@NetworkBinding' {} a -> s {protocol = a} :: NetworkBinding)

-- | The port number on the container that is used with the network binding.
networkBinding_containerPort :: Lens.Lens' NetworkBinding (Core.Maybe Core.Int)
networkBinding_containerPort = Lens.lens (\NetworkBinding' {containerPort} -> containerPort) (\s@NetworkBinding' {} a -> s {containerPort = a} :: NetworkBinding)

instance Core.FromJSON NetworkBinding where
  parseJSON =
    Core.withObject
      "NetworkBinding"
      ( \x ->
          NetworkBinding'
            Core.<$> (x Core..:? "hostPort")
            Core.<*> (x Core..:? "bindIP")
            Core.<*> (x Core..:? "protocol")
            Core.<*> (x Core..:? "containerPort")
      )

instance Core.Hashable NetworkBinding

instance Core.NFData NetworkBinding

instance Core.ToJSON NetworkBinding where
  toJSON NetworkBinding' {..} =
    Core.object
      ( Core.catMaybes
          [ ("hostPort" Core..=) Core.<$> hostPort,
            ("bindIP" Core..=) Core.<$> bindIP,
            ("protocol" Core..=) Core.<$> protocol,
            ("containerPort" Core..=) Core.<$> containerPort
          ]
      )
