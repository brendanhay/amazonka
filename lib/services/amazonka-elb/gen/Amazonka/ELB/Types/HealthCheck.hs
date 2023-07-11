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
-- Module      : Amazonka.ELB.Types.HealthCheck
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ELB.Types.HealthCheck where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ELB.Internal
import qualified Amazonka.Prelude as Prelude

-- | Information about a health check.
--
-- /See:/ 'newHealthCheck' smart constructor.
data HealthCheck = HealthCheck'
  { -- | The instance being checked. The protocol is either TCP, HTTP, HTTPS, or
    -- SSL. The range of valid ports is one (1) through 65535.
    --
    -- TCP is the default, specified as a TCP: port pair, for example
    -- \"TCP:5000\". In this case, a health check simply attempts to open a TCP
    -- connection to the instance on the specified port. Failure to connect
    -- within the configured timeout is considered unhealthy.
    --
    -- SSL is also specified as SSL: port pair, for example, SSL:5000.
    --
    -- For HTTP\/HTTPS, you must include a ping path in the string. HTTP is
    -- specified as a HTTP:port;\/;PathToPing; grouping, for example
    -- \"HTTP:80\/weather\/us\/wa\/seattle\". In this case, a HTTP GET request
    -- is issued to the instance on the given port and path. Any answer other
    -- than \"200 OK\" within the timeout period is considered unhealthy.
    --
    -- The total length of the HTTP ping target must be 1024 16-bit Unicode
    -- characters or less.
    target :: Prelude.Text,
    -- | The approximate interval, in seconds, between health checks of an
    -- individual instance.
    interval :: Prelude.Natural,
    -- | The amount of time, in seconds, during which no response means a failed
    -- health check.
    --
    -- This value must be less than the @Interval@ value.
    timeout :: Prelude.Natural,
    -- | The number of consecutive health check failures required before moving
    -- the instance to the @Unhealthy@ state.
    unhealthyThreshold :: Prelude.Natural,
    -- | The number of consecutive health checks successes required before moving
    -- the instance to the @Healthy@ state.
    healthyThreshold :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HealthCheck' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'target', 'healthCheck_target' - The instance being checked. The protocol is either TCP, HTTP, HTTPS, or
-- SSL. The range of valid ports is one (1) through 65535.
--
-- TCP is the default, specified as a TCP: port pair, for example
-- \"TCP:5000\". In this case, a health check simply attempts to open a TCP
-- connection to the instance on the specified port. Failure to connect
-- within the configured timeout is considered unhealthy.
--
-- SSL is also specified as SSL: port pair, for example, SSL:5000.
--
-- For HTTP\/HTTPS, you must include a ping path in the string. HTTP is
-- specified as a HTTP:port;\/;PathToPing; grouping, for example
-- \"HTTP:80\/weather\/us\/wa\/seattle\". In this case, a HTTP GET request
-- is issued to the instance on the given port and path. Any answer other
-- than \"200 OK\" within the timeout period is considered unhealthy.
--
-- The total length of the HTTP ping target must be 1024 16-bit Unicode
-- characters or less.
--
-- 'interval', 'healthCheck_interval' - The approximate interval, in seconds, between health checks of an
-- individual instance.
--
-- 'timeout', 'healthCheck_timeout' - The amount of time, in seconds, during which no response means a failed
-- health check.
--
-- This value must be less than the @Interval@ value.
--
-- 'unhealthyThreshold', 'healthCheck_unhealthyThreshold' - The number of consecutive health check failures required before moving
-- the instance to the @Unhealthy@ state.
--
-- 'healthyThreshold', 'healthCheck_healthyThreshold' - The number of consecutive health checks successes required before moving
-- the instance to the @Healthy@ state.
newHealthCheck ::
  -- | 'target'
  Prelude.Text ->
  -- | 'interval'
  Prelude.Natural ->
  -- | 'timeout'
  Prelude.Natural ->
  -- | 'unhealthyThreshold'
  Prelude.Natural ->
  -- | 'healthyThreshold'
  Prelude.Natural ->
  HealthCheck
newHealthCheck
  pTarget_
  pInterval_
  pTimeout_
  pUnhealthyThreshold_
  pHealthyThreshold_ =
    HealthCheck'
      { target = pTarget_,
        interval = pInterval_,
        timeout = pTimeout_,
        unhealthyThreshold = pUnhealthyThreshold_,
        healthyThreshold = pHealthyThreshold_
      }

-- | The instance being checked. The protocol is either TCP, HTTP, HTTPS, or
-- SSL. The range of valid ports is one (1) through 65535.
--
-- TCP is the default, specified as a TCP: port pair, for example
-- \"TCP:5000\". In this case, a health check simply attempts to open a TCP
-- connection to the instance on the specified port. Failure to connect
-- within the configured timeout is considered unhealthy.
--
-- SSL is also specified as SSL: port pair, for example, SSL:5000.
--
-- For HTTP\/HTTPS, you must include a ping path in the string. HTTP is
-- specified as a HTTP:port;\/;PathToPing; grouping, for example
-- \"HTTP:80\/weather\/us\/wa\/seattle\". In this case, a HTTP GET request
-- is issued to the instance on the given port and path. Any answer other
-- than \"200 OK\" within the timeout period is considered unhealthy.
--
-- The total length of the HTTP ping target must be 1024 16-bit Unicode
-- characters or less.
healthCheck_target :: Lens.Lens' HealthCheck Prelude.Text
healthCheck_target = Lens.lens (\HealthCheck' {target} -> target) (\s@HealthCheck' {} a -> s {target = a} :: HealthCheck)

-- | The approximate interval, in seconds, between health checks of an
-- individual instance.
healthCheck_interval :: Lens.Lens' HealthCheck Prelude.Natural
healthCheck_interval = Lens.lens (\HealthCheck' {interval} -> interval) (\s@HealthCheck' {} a -> s {interval = a} :: HealthCheck)

-- | The amount of time, in seconds, during which no response means a failed
-- health check.
--
-- This value must be less than the @Interval@ value.
healthCheck_timeout :: Lens.Lens' HealthCheck Prelude.Natural
healthCheck_timeout = Lens.lens (\HealthCheck' {timeout} -> timeout) (\s@HealthCheck' {} a -> s {timeout = a} :: HealthCheck)

-- | The number of consecutive health check failures required before moving
-- the instance to the @Unhealthy@ state.
healthCheck_unhealthyThreshold :: Lens.Lens' HealthCheck Prelude.Natural
healthCheck_unhealthyThreshold = Lens.lens (\HealthCheck' {unhealthyThreshold} -> unhealthyThreshold) (\s@HealthCheck' {} a -> s {unhealthyThreshold = a} :: HealthCheck)

-- | The number of consecutive health checks successes required before moving
-- the instance to the @Healthy@ state.
healthCheck_healthyThreshold :: Lens.Lens' HealthCheck Prelude.Natural
healthCheck_healthyThreshold = Lens.lens (\HealthCheck' {healthyThreshold} -> healthyThreshold) (\s@HealthCheck' {} a -> s {healthyThreshold = a} :: HealthCheck)

instance Data.FromXML HealthCheck where
  parseXML x =
    HealthCheck'
      Prelude.<$> (x Data..@ "Target")
      Prelude.<*> (x Data..@ "Interval")
      Prelude.<*> (x Data..@ "Timeout")
      Prelude.<*> (x Data..@ "UnhealthyThreshold")
      Prelude.<*> (x Data..@ "HealthyThreshold")

instance Prelude.Hashable HealthCheck where
  hashWithSalt _salt HealthCheck' {..} =
    _salt
      `Prelude.hashWithSalt` target
      `Prelude.hashWithSalt` interval
      `Prelude.hashWithSalt` timeout
      `Prelude.hashWithSalt` unhealthyThreshold
      `Prelude.hashWithSalt` healthyThreshold

instance Prelude.NFData HealthCheck where
  rnf HealthCheck' {..} =
    Prelude.rnf target
      `Prelude.seq` Prelude.rnf interval
      `Prelude.seq` Prelude.rnf timeout
      `Prelude.seq` Prelude.rnf unhealthyThreshold
      `Prelude.seq` Prelude.rnf healthyThreshold

instance Data.ToQuery HealthCheck where
  toQuery HealthCheck' {..} =
    Prelude.mconcat
      [ "Target" Data.=: target,
        "Interval" Data.=: interval,
        "Timeout" Data.=: timeout,
        "UnhealthyThreshold" Data.=: unhealthyThreshold,
        "HealthyThreshold" Data.=: healthyThreshold
      ]
