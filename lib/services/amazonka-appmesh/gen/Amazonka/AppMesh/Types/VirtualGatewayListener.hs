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
-- Module      : Amazonka.AppMesh.Types.VirtualGatewayListener
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.VirtualGatewayListener where

import Amazonka.AppMesh.Types.VirtualGatewayConnectionPool
import Amazonka.AppMesh.Types.VirtualGatewayHealthCheckPolicy
import Amazonka.AppMesh.Types.VirtualGatewayListenerTls
import Amazonka.AppMesh.Types.VirtualGatewayPortMapping
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents a listener for a virtual gateway.
--
-- /See:/ 'newVirtualGatewayListener' smart constructor.
data VirtualGatewayListener = VirtualGatewayListener'
  { -- | The connection pool information for the virtual gateway listener.
    connectionPool :: Prelude.Maybe VirtualGatewayConnectionPool,
    -- | The health check information for the listener.
    healthCheck :: Prelude.Maybe VirtualGatewayHealthCheckPolicy,
    -- | A reference to an object that represents the Transport Layer Security
    -- (TLS) properties for the listener.
    tls :: Prelude.Maybe VirtualGatewayListenerTls,
    -- | The port mapping information for the listener.
    portMapping :: VirtualGatewayPortMapping
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VirtualGatewayListener' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectionPool', 'virtualGatewayListener_connectionPool' - The connection pool information for the virtual gateway listener.
--
-- 'healthCheck', 'virtualGatewayListener_healthCheck' - The health check information for the listener.
--
-- 'tls', 'virtualGatewayListener_tls' - A reference to an object that represents the Transport Layer Security
-- (TLS) properties for the listener.
--
-- 'portMapping', 'virtualGatewayListener_portMapping' - The port mapping information for the listener.
newVirtualGatewayListener ::
  -- | 'portMapping'
  VirtualGatewayPortMapping ->
  VirtualGatewayListener
newVirtualGatewayListener pPortMapping_ =
  VirtualGatewayListener'
    { connectionPool =
        Prelude.Nothing,
      healthCheck = Prelude.Nothing,
      tls = Prelude.Nothing,
      portMapping = pPortMapping_
    }

-- | The connection pool information for the virtual gateway listener.
virtualGatewayListener_connectionPool :: Lens.Lens' VirtualGatewayListener (Prelude.Maybe VirtualGatewayConnectionPool)
virtualGatewayListener_connectionPool = Lens.lens (\VirtualGatewayListener' {connectionPool} -> connectionPool) (\s@VirtualGatewayListener' {} a -> s {connectionPool = a} :: VirtualGatewayListener)

-- | The health check information for the listener.
virtualGatewayListener_healthCheck :: Lens.Lens' VirtualGatewayListener (Prelude.Maybe VirtualGatewayHealthCheckPolicy)
virtualGatewayListener_healthCheck = Lens.lens (\VirtualGatewayListener' {healthCheck} -> healthCheck) (\s@VirtualGatewayListener' {} a -> s {healthCheck = a} :: VirtualGatewayListener)

-- | A reference to an object that represents the Transport Layer Security
-- (TLS) properties for the listener.
virtualGatewayListener_tls :: Lens.Lens' VirtualGatewayListener (Prelude.Maybe VirtualGatewayListenerTls)
virtualGatewayListener_tls = Lens.lens (\VirtualGatewayListener' {tls} -> tls) (\s@VirtualGatewayListener' {} a -> s {tls = a} :: VirtualGatewayListener)

-- | The port mapping information for the listener.
virtualGatewayListener_portMapping :: Lens.Lens' VirtualGatewayListener VirtualGatewayPortMapping
virtualGatewayListener_portMapping = Lens.lens (\VirtualGatewayListener' {portMapping} -> portMapping) (\s@VirtualGatewayListener' {} a -> s {portMapping = a} :: VirtualGatewayListener)

instance Data.FromJSON VirtualGatewayListener where
  parseJSON =
    Data.withObject
      "VirtualGatewayListener"
      ( \x ->
          VirtualGatewayListener'
            Prelude.<$> (x Data..:? "connectionPool")
            Prelude.<*> (x Data..:? "healthCheck")
            Prelude.<*> (x Data..:? "tls")
            Prelude.<*> (x Data..: "portMapping")
      )

instance Prelude.Hashable VirtualGatewayListener where
  hashWithSalt _salt VirtualGatewayListener' {..} =
    _salt
      `Prelude.hashWithSalt` connectionPool
      `Prelude.hashWithSalt` healthCheck
      `Prelude.hashWithSalt` tls
      `Prelude.hashWithSalt` portMapping

instance Prelude.NFData VirtualGatewayListener where
  rnf VirtualGatewayListener' {..} =
    Prelude.rnf connectionPool `Prelude.seq`
      Prelude.rnf healthCheck `Prelude.seq`
        Prelude.rnf tls `Prelude.seq`
          Prelude.rnf portMapping

instance Data.ToJSON VirtualGatewayListener where
  toJSON VirtualGatewayListener' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("connectionPool" Data..=)
              Prelude.<$> connectionPool,
            ("healthCheck" Data..=) Prelude.<$> healthCheck,
            ("tls" Data..=) Prelude.<$> tls,
            Prelude.Just ("portMapping" Data..= portMapping)
          ]
      )
