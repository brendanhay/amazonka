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
-- Module      : Amazonka.AppMesh.Types.VirtualNodeTcpConnectionPool
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.VirtualNodeTcpConnectionPool where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents a type of connection pool.
--
-- /See:/ 'newVirtualNodeTcpConnectionPool' smart constructor.
data VirtualNodeTcpConnectionPool = VirtualNodeTcpConnectionPool'
  { -- | Maximum number of outbound TCP connections Envoy can establish
    -- concurrently with all hosts in upstream cluster.
    maxConnections :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VirtualNodeTcpConnectionPool' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxConnections', 'virtualNodeTcpConnectionPool_maxConnections' - Maximum number of outbound TCP connections Envoy can establish
-- concurrently with all hosts in upstream cluster.
newVirtualNodeTcpConnectionPool ::
  -- | 'maxConnections'
  Prelude.Natural ->
  VirtualNodeTcpConnectionPool
newVirtualNodeTcpConnectionPool pMaxConnections_ =
  VirtualNodeTcpConnectionPool'
    { maxConnections =
        pMaxConnections_
    }

-- | Maximum number of outbound TCP connections Envoy can establish
-- concurrently with all hosts in upstream cluster.
virtualNodeTcpConnectionPool_maxConnections :: Lens.Lens' VirtualNodeTcpConnectionPool Prelude.Natural
virtualNodeTcpConnectionPool_maxConnections = Lens.lens (\VirtualNodeTcpConnectionPool' {maxConnections} -> maxConnections) (\s@VirtualNodeTcpConnectionPool' {} a -> s {maxConnections = a} :: VirtualNodeTcpConnectionPool)

instance Data.FromJSON VirtualNodeTcpConnectionPool where
  parseJSON =
    Data.withObject
      "VirtualNodeTcpConnectionPool"
      ( \x ->
          VirtualNodeTcpConnectionPool'
            Prelude.<$> (x Data..: "maxConnections")
      )

instance
  Prelude.Hashable
    VirtualNodeTcpConnectionPool
  where
  hashWithSalt _salt VirtualNodeTcpConnectionPool' {..} =
    _salt `Prelude.hashWithSalt` maxConnections

instance Prelude.NFData VirtualNodeTcpConnectionPool where
  rnf VirtualNodeTcpConnectionPool' {..} =
    Prelude.rnf maxConnections

instance Data.ToJSON VirtualNodeTcpConnectionPool where
  toJSON VirtualNodeTcpConnectionPool' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("maxConnections" Data..= maxConnections)
          ]
      )
