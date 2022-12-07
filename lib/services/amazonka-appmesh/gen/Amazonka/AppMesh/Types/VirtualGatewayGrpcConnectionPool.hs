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
-- Module      : Amazonka.AppMesh.Types.VirtualGatewayGrpcConnectionPool
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.VirtualGatewayGrpcConnectionPool where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents a type of connection pool.
--
-- /See:/ 'newVirtualGatewayGrpcConnectionPool' smart constructor.
data VirtualGatewayGrpcConnectionPool = VirtualGatewayGrpcConnectionPool'
  { -- | Maximum number of inflight requests Envoy can concurrently support
    -- across hosts in upstream cluster.
    maxRequests :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VirtualGatewayGrpcConnectionPool' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxRequests', 'virtualGatewayGrpcConnectionPool_maxRequests' - Maximum number of inflight requests Envoy can concurrently support
-- across hosts in upstream cluster.
newVirtualGatewayGrpcConnectionPool ::
  -- | 'maxRequests'
  Prelude.Natural ->
  VirtualGatewayGrpcConnectionPool
newVirtualGatewayGrpcConnectionPool pMaxRequests_ =
  VirtualGatewayGrpcConnectionPool'
    { maxRequests =
        pMaxRequests_
    }

-- | Maximum number of inflight requests Envoy can concurrently support
-- across hosts in upstream cluster.
virtualGatewayGrpcConnectionPool_maxRequests :: Lens.Lens' VirtualGatewayGrpcConnectionPool Prelude.Natural
virtualGatewayGrpcConnectionPool_maxRequests = Lens.lens (\VirtualGatewayGrpcConnectionPool' {maxRequests} -> maxRequests) (\s@VirtualGatewayGrpcConnectionPool' {} a -> s {maxRequests = a} :: VirtualGatewayGrpcConnectionPool)

instance
  Data.FromJSON
    VirtualGatewayGrpcConnectionPool
  where
  parseJSON =
    Data.withObject
      "VirtualGatewayGrpcConnectionPool"
      ( \x ->
          VirtualGatewayGrpcConnectionPool'
            Prelude.<$> (x Data..: "maxRequests")
      )

instance
  Prelude.Hashable
    VirtualGatewayGrpcConnectionPool
  where
  hashWithSalt
    _salt
    VirtualGatewayGrpcConnectionPool' {..} =
      _salt `Prelude.hashWithSalt` maxRequests

instance
  Prelude.NFData
    VirtualGatewayGrpcConnectionPool
  where
  rnf VirtualGatewayGrpcConnectionPool' {..} =
    Prelude.rnf maxRequests

instance Data.ToJSON VirtualGatewayGrpcConnectionPool where
  toJSON VirtualGatewayGrpcConnectionPool' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("maxRequests" Data..= maxRequests)]
      )
