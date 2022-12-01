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
-- Module      : Amazonka.AppMesh.Types.VirtualGatewayHttpConnectionPool
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.VirtualGatewayHttpConnectionPool where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that represents a type of connection pool.
--
-- /See:/ 'newVirtualGatewayHttpConnectionPool' smart constructor.
data VirtualGatewayHttpConnectionPool = VirtualGatewayHttpConnectionPool'
  { -- | Number of overflowing requests after @max_connections@ Envoy will queue
    -- to upstream cluster.
    maxPendingRequests :: Prelude.Maybe Prelude.Natural,
    -- | Maximum number of outbound TCP connections Envoy can establish
    -- concurrently with all hosts in upstream cluster.
    maxConnections :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VirtualGatewayHttpConnectionPool' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxPendingRequests', 'virtualGatewayHttpConnectionPool_maxPendingRequests' - Number of overflowing requests after @max_connections@ Envoy will queue
-- to upstream cluster.
--
-- 'maxConnections', 'virtualGatewayHttpConnectionPool_maxConnections' - Maximum number of outbound TCP connections Envoy can establish
-- concurrently with all hosts in upstream cluster.
newVirtualGatewayHttpConnectionPool ::
  -- | 'maxConnections'
  Prelude.Natural ->
  VirtualGatewayHttpConnectionPool
newVirtualGatewayHttpConnectionPool pMaxConnections_ =
  VirtualGatewayHttpConnectionPool'
    { maxPendingRequests =
        Prelude.Nothing,
      maxConnections = pMaxConnections_
    }

-- | Number of overflowing requests after @max_connections@ Envoy will queue
-- to upstream cluster.
virtualGatewayHttpConnectionPool_maxPendingRequests :: Lens.Lens' VirtualGatewayHttpConnectionPool (Prelude.Maybe Prelude.Natural)
virtualGatewayHttpConnectionPool_maxPendingRequests = Lens.lens (\VirtualGatewayHttpConnectionPool' {maxPendingRequests} -> maxPendingRequests) (\s@VirtualGatewayHttpConnectionPool' {} a -> s {maxPendingRequests = a} :: VirtualGatewayHttpConnectionPool)

-- | Maximum number of outbound TCP connections Envoy can establish
-- concurrently with all hosts in upstream cluster.
virtualGatewayHttpConnectionPool_maxConnections :: Lens.Lens' VirtualGatewayHttpConnectionPool Prelude.Natural
virtualGatewayHttpConnectionPool_maxConnections = Lens.lens (\VirtualGatewayHttpConnectionPool' {maxConnections} -> maxConnections) (\s@VirtualGatewayHttpConnectionPool' {} a -> s {maxConnections = a} :: VirtualGatewayHttpConnectionPool)

instance
  Core.FromJSON
    VirtualGatewayHttpConnectionPool
  where
  parseJSON =
    Core.withObject
      "VirtualGatewayHttpConnectionPool"
      ( \x ->
          VirtualGatewayHttpConnectionPool'
            Prelude.<$> (x Core..:? "maxPendingRequests")
            Prelude.<*> (x Core..: "maxConnections")
      )

instance
  Prelude.Hashable
    VirtualGatewayHttpConnectionPool
  where
  hashWithSalt
    _salt
    VirtualGatewayHttpConnectionPool' {..} =
      _salt `Prelude.hashWithSalt` maxPendingRequests
        `Prelude.hashWithSalt` maxConnections

instance
  Prelude.NFData
    VirtualGatewayHttpConnectionPool
  where
  rnf VirtualGatewayHttpConnectionPool' {..} =
    Prelude.rnf maxPendingRequests
      `Prelude.seq` Prelude.rnf maxConnections

instance Core.ToJSON VirtualGatewayHttpConnectionPool where
  toJSON VirtualGatewayHttpConnectionPool' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("maxPendingRequests" Core..=)
              Prelude.<$> maxPendingRequests,
            Prelude.Just
              ("maxConnections" Core..= maxConnections)
          ]
      )
