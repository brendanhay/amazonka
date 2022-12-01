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
-- Module      : Amazonka.AppMesh.Types.VirtualGatewayHttp2ConnectionPool
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.VirtualGatewayHttp2ConnectionPool where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that represents a type of connection pool.
--
-- /See:/ 'newVirtualGatewayHttp2ConnectionPool' smart constructor.
data VirtualGatewayHttp2ConnectionPool = VirtualGatewayHttp2ConnectionPool'
  { -- | Maximum number of inflight requests Envoy can concurrently support
    -- across hosts in upstream cluster.
    maxRequests :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VirtualGatewayHttp2ConnectionPool' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxRequests', 'virtualGatewayHttp2ConnectionPool_maxRequests' - Maximum number of inflight requests Envoy can concurrently support
-- across hosts in upstream cluster.
newVirtualGatewayHttp2ConnectionPool ::
  -- | 'maxRequests'
  Prelude.Natural ->
  VirtualGatewayHttp2ConnectionPool
newVirtualGatewayHttp2ConnectionPool pMaxRequests_ =
  VirtualGatewayHttp2ConnectionPool'
    { maxRequests =
        pMaxRequests_
    }

-- | Maximum number of inflight requests Envoy can concurrently support
-- across hosts in upstream cluster.
virtualGatewayHttp2ConnectionPool_maxRequests :: Lens.Lens' VirtualGatewayHttp2ConnectionPool Prelude.Natural
virtualGatewayHttp2ConnectionPool_maxRequests = Lens.lens (\VirtualGatewayHttp2ConnectionPool' {maxRequests} -> maxRequests) (\s@VirtualGatewayHttp2ConnectionPool' {} a -> s {maxRequests = a} :: VirtualGatewayHttp2ConnectionPool)

instance
  Core.FromJSON
    VirtualGatewayHttp2ConnectionPool
  where
  parseJSON =
    Core.withObject
      "VirtualGatewayHttp2ConnectionPool"
      ( \x ->
          VirtualGatewayHttp2ConnectionPool'
            Prelude.<$> (x Core..: "maxRequests")
      )

instance
  Prelude.Hashable
    VirtualGatewayHttp2ConnectionPool
  where
  hashWithSalt
    _salt
    VirtualGatewayHttp2ConnectionPool' {..} =
      _salt `Prelude.hashWithSalt` maxRequests

instance
  Prelude.NFData
    VirtualGatewayHttp2ConnectionPool
  where
  rnf VirtualGatewayHttp2ConnectionPool' {..} =
    Prelude.rnf maxRequests

instance
  Core.ToJSON
    VirtualGatewayHttp2ConnectionPool
  where
  toJSON VirtualGatewayHttp2ConnectionPool' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("maxRequests" Core..= maxRequests)]
      )
