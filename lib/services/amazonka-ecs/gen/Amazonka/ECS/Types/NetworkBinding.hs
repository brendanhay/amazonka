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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.NetworkBinding where

import qualified Amazonka.Core as Core
import Amazonka.ECS.Types.TransportProtocol
import qualified Amazonka.Lens as Lens
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
    -- | The protocol used for the network binding.
    protocol :: Prelude.Maybe TransportProtocol,
    -- | The port number on the host that is used with the network binding.
    hostPort :: Prelude.Maybe Prelude.Int,
    -- | The port number on the container that is used with the network binding.
    containerPort :: Prelude.Maybe Prelude.Int
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
-- 'protocol', 'networkBinding_protocol' - The protocol used for the network binding.
--
-- 'hostPort', 'networkBinding_hostPort' - The port number on the host that is used with the network binding.
--
-- 'containerPort', 'networkBinding_containerPort' - The port number on the container that is used with the network binding.
newNetworkBinding ::
  NetworkBinding
newNetworkBinding =
  NetworkBinding'
    { bindIP = Prelude.Nothing,
      protocol = Prelude.Nothing,
      hostPort = Prelude.Nothing,
      containerPort = Prelude.Nothing
    }

-- | The IP address that the container is bound to on the container instance.
networkBinding_bindIP :: Lens.Lens' NetworkBinding (Prelude.Maybe Prelude.Text)
networkBinding_bindIP = Lens.lens (\NetworkBinding' {bindIP} -> bindIP) (\s@NetworkBinding' {} a -> s {bindIP = a} :: NetworkBinding)

-- | The protocol used for the network binding.
networkBinding_protocol :: Lens.Lens' NetworkBinding (Prelude.Maybe TransportProtocol)
networkBinding_protocol = Lens.lens (\NetworkBinding' {protocol} -> protocol) (\s@NetworkBinding' {} a -> s {protocol = a} :: NetworkBinding)

-- | The port number on the host that is used with the network binding.
networkBinding_hostPort :: Lens.Lens' NetworkBinding (Prelude.Maybe Prelude.Int)
networkBinding_hostPort = Lens.lens (\NetworkBinding' {hostPort} -> hostPort) (\s@NetworkBinding' {} a -> s {hostPort = a} :: NetworkBinding)

-- | The port number on the container that is used with the network binding.
networkBinding_containerPort :: Lens.Lens' NetworkBinding (Prelude.Maybe Prelude.Int)
networkBinding_containerPort = Lens.lens (\NetworkBinding' {containerPort} -> containerPort) (\s@NetworkBinding' {} a -> s {containerPort = a} :: NetworkBinding)

instance Core.FromJSON NetworkBinding where
  parseJSON =
    Core.withObject
      "NetworkBinding"
      ( \x ->
          NetworkBinding'
            Prelude.<$> (x Core..:? "bindIP")
            Prelude.<*> (x Core..:? "protocol")
            Prelude.<*> (x Core..:? "hostPort")
            Prelude.<*> (x Core..:? "containerPort")
      )

instance Prelude.Hashable NetworkBinding where
  hashWithSalt _salt NetworkBinding' {..} =
    _salt `Prelude.hashWithSalt` bindIP
      `Prelude.hashWithSalt` protocol
      `Prelude.hashWithSalt` hostPort
      `Prelude.hashWithSalt` containerPort

instance Prelude.NFData NetworkBinding where
  rnf NetworkBinding' {..} =
    Prelude.rnf bindIP
      `Prelude.seq` Prelude.rnf protocol
      `Prelude.seq` Prelude.rnf hostPort
      `Prelude.seq` Prelude.rnf containerPort

instance Core.ToJSON NetworkBinding where
  toJSON NetworkBinding' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("bindIP" Core..=) Prelude.<$> bindIP,
            ("protocol" Core..=) Prelude.<$> protocol,
            ("hostPort" Core..=) Prelude.<$> hostPort,
            ("containerPort" Core..=) Prelude.<$> containerPort
          ]
      )
