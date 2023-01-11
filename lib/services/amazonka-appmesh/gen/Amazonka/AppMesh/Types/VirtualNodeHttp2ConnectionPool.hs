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
-- Module      : Amazonka.AppMesh.Types.VirtualNodeHttp2ConnectionPool
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.VirtualNodeHttp2ConnectionPool where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents a type of connection pool.
--
-- /See:/ 'newVirtualNodeHttp2ConnectionPool' smart constructor.
data VirtualNodeHttp2ConnectionPool = VirtualNodeHttp2ConnectionPool'
  { -- | Maximum number of inflight requests Envoy can concurrently support
    -- across hosts in upstream cluster.
    maxRequests :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VirtualNodeHttp2ConnectionPool' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxRequests', 'virtualNodeHttp2ConnectionPool_maxRequests' - Maximum number of inflight requests Envoy can concurrently support
-- across hosts in upstream cluster.
newVirtualNodeHttp2ConnectionPool ::
  -- | 'maxRequests'
  Prelude.Natural ->
  VirtualNodeHttp2ConnectionPool
newVirtualNodeHttp2ConnectionPool pMaxRequests_ =
  VirtualNodeHttp2ConnectionPool'
    { maxRequests =
        pMaxRequests_
    }

-- | Maximum number of inflight requests Envoy can concurrently support
-- across hosts in upstream cluster.
virtualNodeHttp2ConnectionPool_maxRequests :: Lens.Lens' VirtualNodeHttp2ConnectionPool Prelude.Natural
virtualNodeHttp2ConnectionPool_maxRequests = Lens.lens (\VirtualNodeHttp2ConnectionPool' {maxRequests} -> maxRequests) (\s@VirtualNodeHttp2ConnectionPool' {} a -> s {maxRequests = a} :: VirtualNodeHttp2ConnectionPool)

instance Data.FromJSON VirtualNodeHttp2ConnectionPool where
  parseJSON =
    Data.withObject
      "VirtualNodeHttp2ConnectionPool"
      ( \x ->
          VirtualNodeHttp2ConnectionPool'
            Prelude.<$> (x Data..: "maxRequests")
      )

instance
  Prelude.Hashable
    VirtualNodeHttp2ConnectionPool
  where
  hashWithSalt
    _salt
    VirtualNodeHttp2ConnectionPool' {..} =
      _salt `Prelude.hashWithSalt` maxRequests

instance
  Prelude.NFData
    VirtualNodeHttp2ConnectionPool
  where
  rnf VirtualNodeHttp2ConnectionPool' {..} =
    Prelude.rnf maxRequests

instance Data.ToJSON VirtualNodeHttp2ConnectionPool where
  toJSON VirtualNodeHttp2ConnectionPool' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("maxRequests" Data..= maxRequests)]
      )
