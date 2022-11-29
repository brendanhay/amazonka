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
-- Module      : Amazonka.AppMesh.Types.VirtualNodeHttpConnectionPool
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.VirtualNodeHttpConnectionPool where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that represents a type of connection pool.
--
-- /See:/ 'newVirtualNodeHttpConnectionPool' smart constructor.
data VirtualNodeHttpConnectionPool = VirtualNodeHttpConnectionPool'
  { -- | Number of overflowing requests after @max_connections@ Envoy will queue
    -- to upstream cluster.
    maxPendingRequests :: Prelude.Maybe Prelude.Natural,
    -- | Maximum number of outbound TCP connections Envoy can establish
    -- concurrently with all hosts in upstream cluster.
    maxConnections :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VirtualNodeHttpConnectionPool' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxPendingRequests', 'virtualNodeHttpConnectionPool_maxPendingRequests' - Number of overflowing requests after @max_connections@ Envoy will queue
-- to upstream cluster.
--
-- 'maxConnections', 'virtualNodeHttpConnectionPool_maxConnections' - Maximum number of outbound TCP connections Envoy can establish
-- concurrently with all hosts in upstream cluster.
newVirtualNodeHttpConnectionPool ::
  -- | 'maxConnections'
  Prelude.Natural ->
  VirtualNodeHttpConnectionPool
newVirtualNodeHttpConnectionPool pMaxConnections_ =
  VirtualNodeHttpConnectionPool'
    { maxPendingRequests =
        Prelude.Nothing,
      maxConnections = pMaxConnections_
    }

-- | Number of overflowing requests after @max_connections@ Envoy will queue
-- to upstream cluster.
virtualNodeHttpConnectionPool_maxPendingRequests :: Lens.Lens' VirtualNodeHttpConnectionPool (Prelude.Maybe Prelude.Natural)
virtualNodeHttpConnectionPool_maxPendingRequests = Lens.lens (\VirtualNodeHttpConnectionPool' {maxPendingRequests} -> maxPendingRequests) (\s@VirtualNodeHttpConnectionPool' {} a -> s {maxPendingRequests = a} :: VirtualNodeHttpConnectionPool)

-- | Maximum number of outbound TCP connections Envoy can establish
-- concurrently with all hosts in upstream cluster.
virtualNodeHttpConnectionPool_maxConnections :: Lens.Lens' VirtualNodeHttpConnectionPool Prelude.Natural
virtualNodeHttpConnectionPool_maxConnections = Lens.lens (\VirtualNodeHttpConnectionPool' {maxConnections} -> maxConnections) (\s@VirtualNodeHttpConnectionPool' {} a -> s {maxConnections = a} :: VirtualNodeHttpConnectionPool)

instance Core.FromJSON VirtualNodeHttpConnectionPool where
  parseJSON =
    Core.withObject
      "VirtualNodeHttpConnectionPool"
      ( \x ->
          VirtualNodeHttpConnectionPool'
            Prelude.<$> (x Core..:? "maxPendingRequests")
            Prelude.<*> (x Core..: "maxConnections")
      )

instance
  Prelude.Hashable
    VirtualNodeHttpConnectionPool
  where
  hashWithSalt _salt VirtualNodeHttpConnectionPool' {..} =
    _salt `Prelude.hashWithSalt` maxPendingRequests
      `Prelude.hashWithSalt` maxConnections

instance Prelude.NFData VirtualNodeHttpConnectionPool where
  rnf VirtualNodeHttpConnectionPool' {..} =
    Prelude.rnf maxPendingRequests
      `Prelude.seq` Prelude.rnf maxConnections

instance Core.ToJSON VirtualNodeHttpConnectionPool where
  toJSON VirtualNodeHttpConnectionPool' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("maxPendingRequests" Core..=)
              Prelude.<$> maxPendingRequests,
            Prelude.Just
              ("maxConnections" Core..= maxConnections)
          ]
      )
