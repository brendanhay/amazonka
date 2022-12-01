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
-- Module      : Amazonka.AppMesh.Types.VirtualNodeConnectionPool
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.VirtualNodeConnectionPool where

import Amazonka.AppMesh.Types.VirtualNodeGrpcConnectionPool
import Amazonka.AppMesh.Types.VirtualNodeHttp2ConnectionPool
import Amazonka.AppMesh.Types.VirtualNodeHttpConnectionPool
import Amazonka.AppMesh.Types.VirtualNodeTcpConnectionPool
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that represents the type of virtual node connection pool.
--
-- Only one protocol is used at a time and should be the same protocol as
-- the one chosen under port mapping.
--
-- If not present the default value for @maxPendingRequests@ is
-- @2147483647@.
--
-- /See:/ 'newVirtualNodeConnectionPool' smart constructor.
data VirtualNodeConnectionPool = VirtualNodeConnectionPool'
  { -- | An object that represents a type of connection pool.
    http :: Prelude.Maybe VirtualNodeHttpConnectionPool,
    -- | An object that represents a type of connection pool.
    http2 :: Prelude.Maybe VirtualNodeHttp2ConnectionPool,
    -- | An object that represents a type of connection pool.
    tcp :: Prelude.Maybe VirtualNodeTcpConnectionPool,
    -- | An object that represents a type of connection pool.
    grpc :: Prelude.Maybe VirtualNodeGrpcConnectionPool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VirtualNodeConnectionPool' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'http', 'virtualNodeConnectionPool_http' - An object that represents a type of connection pool.
--
-- 'http2', 'virtualNodeConnectionPool_http2' - An object that represents a type of connection pool.
--
-- 'tcp', 'virtualNodeConnectionPool_tcp' - An object that represents a type of connection pool.
--
-- 'grpc', 'virtualNodeConnectionPool_grpc' - An object that represents a type of connection pool.
newVirtualNodeConnectionPool ::
  VirtualNodeConnectionPool
newVirtualNodeConnectionPool =
  VirtualNodeConnectionPool'
    { http = Prelude.Nothing,
      http2 = Prelude.Nothing,
      tcp = Prelude.Nothing,
      grpc = Prelude.Nothing
    }

-- | An object that represents a type of connection pool.
virtualNodeConnectionPool_http :: Lens.Lens' VirtualNodeConnectionPool (Prelude.Maybe VirtualNodeHttpConnectionPool)
virtualNodeConnectionPool_http = Lens.lens (\VirtualNodeConnectionPool' {http} -> http) (\s@VirtualNodeConnectionPool' {} a -> s {http = a} :: VirtualNodeConnectionPool)

-- | An object that represents a type of connection pool.
virtualNodeConnectionPool_http2 :: Lens.Lens' VirtualNodeConnectionPool (Prelude.Maybe VirtualNodeHttp2ConnectionPool)
virtualNodeConnectionPool_http2 = Lens.lens (\VirtualNodeConnectionPool' {http2} -> http2) (\s@VirtualNodeConnectionPool' {} a -> s {http2 = a} :: VirtualNodeConnectionPool)

-- | An object that represents a type of connection pool.
virtualNodeConnectionPool_tcp :: Lens.Lens' VirtualNodeConnectionPool (Prelude.Maybe VirtualNodeTcpConnectionPool)
virtualNodeConnectionPool_tcp = Lens.lens (\VirtualNodeConnectionPool' {tcp} -> tcp) (\s@VirtualNodeConnectionPool' {} a -> s {tcp = a} :: VirtualNodeConnectionPool)

-- | An object that represents a type of connection pool.
virtualNodeConnectionPool_grpc :: Lens.Lens' VirtualNodeConnectionPool (Prelude.Maybe VirtualNodeGrpcConnectionPool)
virtualNodeConnectionPool_grpc = Lens.lens (\VirtualNodeConnectionPool' {grpc} -> grpc) (\s@VirtualNodeConnectionPool' {} a -> s {grpc = a} :: VirtualNodeConnectionPool)

instance Core.FromJSON VirtualNodeConnectionPool where
  parseJSON =
    Core.withObject
      "VirtualNodeConnectionPool"
      ( \x ->
          VirtualNodeConnectionPool'
            Prelude.<$> (x Core..:? "http")
            Prelude.<*> (x Core..:? "http2")
            Prelude.<*> (x Core..:? "tcp")
            Prelude.<*> (x Core..:? "grpc")
      )

instance Prelude.Hashable VirtualNodeConnectionPool where
  hashWithSalt _salt VirtualNodeConnectionPool' {..} =
    _salt `Prelude.hashWithSalt` http
      `Prelude.hashWithSalt` http2
      `Prelude.hashWithSalt` tcp
      `Prelude.hashWithSalt` grpc

instance Prelude.NFData VirtualNodeConnectionPool where
  rnf VirtualNodeConnectionPool' {..} =
    Prelude.rnf http
      `Prelude.seq` Prelude.rnf http2
      `Prelude.seq` Prelude.rnf tcp
      `Prelude.seq` Prelude.rnf grpc

instance Core.ToJSON VirtualNodeConnectionPool where
  toJSON VirtualNodeConnectionPool' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("http" Core..=) Prelude.<$> http,
            ("http2" Core..=) Prelude.<$> http2,
            ("tcp" Core..=) Prelude.<$> tcp,
            ("grpc" Core..=) Prelude.<$> grpc
          ]
      )
