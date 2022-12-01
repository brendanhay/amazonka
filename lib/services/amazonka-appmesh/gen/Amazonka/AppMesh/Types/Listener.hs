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
-- Module      : Amazonka.AppMesh.Types.Listener
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.Listener where

import Amazonka.AppMesh.Types.HealthCheckPolicy
import Amazonka.AppMesh.Types.ListenerTimeout
import Amazonka.AppMesh.Types.ListenerTls
import Amazonka.AppMesh.Types.OutlierDetection
import Amazonka.AppMesh.Types.PortMapping
import Amazonka.AppMesh.Types.VirtualNodeConnectionPool
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that represents a listener for a virtual node.
--
-- /See:/ 'newListener' smart constructor.
data Listener = Listener'
  { -- | An object that represents timeouts for different protocols.
    timeout :: Prelude.Maybe ListenerTimeout,
    -- | The health check information for the listener.
    healthCheck :: Prelude.Maybe HealthCheckPolicy,
    -- | A reference to an object that represents the Transport Layer Security
    -- (TLS) properties for a listener.
    tls :: Prelude.Maybe ListenerTls,
    -- | The connection pool information for the listener.
    connectionPool :: Prelude.Maybe VirtualNodeConnectionPool,
    -- | The outlier detection information for the listener.
    outlierDetection :: Prelude.Maybe OutlierDetection,
    -- | The port mapping information for the listener.
    portMapping :: PortMapping
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Listener' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timeout', 'listener_timeout' - An object that represents timeouts for different protocols.
--
-- 'healthCheck', 'listener_healthCheck' - The health check information for the listener.
--
-- 'tls', 'listener_tls' - A reference to an object that represents the Transport Layer Security
-- (TLS) properties for a listener.
--
-- 'connectionPool', 'listener_connectionPool' - The connection pool information for the listener.
--
-- 'outlierDetection', 'listener_outlierDetection' - The outlier detection information for the listener.
--
-- 'portMapping', 'listener_portMapping' - The port mapping information for the listener.
newListener ::
  -- | 'portMapping'
  PortMapping ->
  Listener
newListener pPortMapping_ =
  Listener'
    { timeout = Prelude.Nothing,
      healthCheck = Prelude.Nothing,
      tls = Prelude.Nothing,
      connectionPool = Prelude.Nothing,
      outlierDetection = Prelude.Nothing,
      portMapping = pPortMapping_
    }

-- | An object that represents timeouts for different protocols.
listener_timeout :: Lens.Lens' Listener (Prelude.Maybe ListenerTimeout)
listener_timeout = Lens.lens (\Listener' {timeout} -> timeout) (\s@Listener' {} a -> s {timeout = a} :: Listener)

-- | The health check information for the listener.
listener_healthCheck :: Lens.Lens' Listener (Prelude.Maybe HealthCheckPolicy)
listener_healthCheck = Lens.lens (\Listener' {healthCheck} -> healthCheck) (\s@Listener' {} a -> s {healthCheck = a} :: Listener)

-- | A reference to an object that represents the Transport Layer Security
-- (TLS) properties for a listener.
listener_tls :: Lens.Lens' Listener (Prelude.Maybe ListenerTls)
listener_tls = Lens.lens (\Listener' {tls} -> tls) (\s@Listener' {} a -> s {tls = a} :: Listener)

-- | The connection pool information for the listener.
listener_connectionPool :: Lens.Lens' Listener (Prelude.Maybe VirtualNodeConnectionPool)
listener_connectionPool = Lens.lens (\Listener' {connectionPool} -> connectionPool) (\s@Listener' {} a -> s {connectionPool = a} :: Listener)

-- | The outlier detection information for the listener.
listener_outlierDetection :: Lens.Lens' Listener (Prelude.Maybe OutlierDetection)
listener_outlierDetection = Lens.lens (\Listener' {outlierDetection} -> outlierDetection) (\s@Listener' {} a -> s {outlierDetection = a} :: Listener)

-- | The port mapping information for the listener.
listener_portMapping :: Lens.Lens' Listener PortMapping
listener_portMapping = Lens.lens (\Listener' {portMapping} -> portMapping) (\s@Listener' {} a -> s {portMapping = a} :: Listener)

instance Core.FromJSON Listener where
  parseJSON =
    Core.withObject
      "Listener"
      ( \x ->
          Listener'
            Prelude.<$> (x Core..:? "timeout")
            Prelude.<*> (x Core..:? "healthCheck")
            Prelude.<*> (x Core..:? "tls")
            Prelude.<*> (x Core..:? "connectionPool")
            Prelude.<*> (x Core..:? "outlierDetection")
            Prelude.<*> (x Core..: "portMapping")
      )

instance Prelude.Hashable Listener where
  hashWithSalt _salt Listener' {..} =
    _salt `Prelude.hashWithSalt` timeout
      `Prelude.hashWithSalt` healthCheck
      `Prelude.hashWithSalt` tls
      `Prelude.hashWithSalt` connectionPool
      `Prelude.hashWithSalt` outlierDetection
      `Prelude.hashWithSalt` portMapping

instance Prelude.NFData Listener where
  rnf Listener' {..} =
    Prelude.rnf timeout
      `Prelude.seq` Prelude.rnf healthCheck
      `Prelude.seq` Prelude.rnf tls
      `Prelude.seq` Prelude.rnf connectionPool
      `Prelude.seq` Prelude.rnf outlierDetection
      `Prelude.seq` Prelude.rnf portMapping

instance Core.ToJSON Listener where
  toJSON Listener' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("timeout" Core..=) Prelude.<$> timeout,
            ("healthCheck" Core..=) Prelude.<$> healthCheck,
            ("tls" Core..=) Prelude.<$> tls,
            ("connectionPool" Core..=)
              Prelude.<$> connectionPool,
            ("outlierDetection" Core..=)
              Prelude.<$> outlierDetection,
            Prelude.Just ("portMapping" Core..= portMapping)
          ]
      )
