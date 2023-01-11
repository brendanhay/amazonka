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
-- Module      : Amazonka.AppMesh.Types.VirtualNodeGrpcConnectionPool
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.VirtualNodeGrpcConnectionPool where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents a type of connection pool.
--
-- /See:/ 'newVirtualNodeGrpcConnectionPool' smart constructor.
data VirtualNodeGrpcConnectionPool = VirtualNodeGrpcConnectionPool'
  { -- | Maximum number of inflight requests Envoy can concurrently support
    -- across hosts in upstream cluster.
    maxRequests :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VirtualNodeGrpcConnectionPool' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxRequests', 'virtualNodeGrpcConnectionPool_maxRequests' - Maximum number of inflight requests Envoy can concurrently support
-- across hosts in upstream cluster.
newVirtualNodeGrpcConnectionPool ::
  -- | 'maxRequests'
  Prelude.Natural ->
  VirtualNodeGrpcConnectionPool
newVirtualNodeGrpcConnectionPool pMaxRequests_ =
  VirtualNodeGrpcConnectionPool'
    { maxRequests =
        pMaxRequests_
    }

-- | Maximum number of inflight requests Envoy can concurrently support
-- across hosts in upstream cluster.
virtualNodeGrpcConnectionPool_maxRequests :: Lens.Lens' VirtualNodeGrpcConnectionPool Prelude.Natural
virtualNodeGrpcConnectionPool_maxRequests = Lens.lens (\VirtualNodeGrpcConnectionPool' {maxRequests} -> maxRequests) (\s@VirtualNodeGrpcConnectionPool' {} a -> s {maxRequests = a} :: VirtualNodeGrpcConnectionPool)

instance Data.FromJSON VirtualNodeGrpcConnectionPool where
  parseJSON =
    Data.withObject
      "VirtualNodeGrpcConnectionPool"
      ( \x ->
          VirtualNodeGrpcConnectionPool'
            Prelude.<$> (x Data..: "maxRequests")
      )

instance
  Prelude.Hashable
    VirtualNodeGrpcConnectionPool
  where
  hashWithSalt _salt VirtualNodeGrpcConnectionPool' {..} =
    _salt `Prelude.hashWithSalt` maxRequests

instance Prelude.NFData VirtualNodeGrpcConnectionPool where
  rnf VirtualNodeGrpcConnectionPool' {..} =
    Prelude.rnf maxRequests

instance Data.ToJSON VirtualNodeGrpcConnectionPool where
  toJSON VirtualNodeGrpcConnectionPool' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("maxRequests" Data..= maxRequests)]
      )
