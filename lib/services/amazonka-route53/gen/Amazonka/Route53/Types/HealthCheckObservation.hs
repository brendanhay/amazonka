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
-- Module      : Amazonka.Route53.Types.HealthCheckObservation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53.Types.HealthCheckObservation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53.Internal
import Amazonka.Route53.Types.HealthCheckRegion
import Amazonka.Route53.Types.StatusReport

-- | A complex type that contains the last failure reason as reported by one
-- Amazon Route 53 health checker.
--
-- /See:/ 'newHealthCheckObservation' smart constructor.
data HealthCheckObservation = HealthCheckObservation'
  { -- | A complex type that contains the last failure reason as reported by one
    -- Amazon Route 53 health checker and the time of the failed health check.
    statusReport :: Prelude.Maybe StatusReport,
    -- | The region of the Amazon Route 53 health checker that provided the
    -- status in @StatusReport@.
    region :: Prelude.Maybe HealthCheckRegion,
    -- | The IP address of the Amazon Route 53 health checker that provided the
    -- failure reason in @StatusReport@.
    iPAddress :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HealthCheckObservation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statusReport', 'healthCheckObservation_statusReport' - A complex type that contains the last failure reason as reported by one
-- Amazon Route 53 health checker and the time of the failed health check.
--
-- 'region', 'healthCheckObservation_region' - The region of the Amazon Route 53 health checker that provided the
-- status in @StatusReport@.
--
-- 'iPAddress', 'healthCheckObservation_iPAddress' - The IP address of the Amazon Route 53 health checker that provided the
-- failure reason in @StatusReport@.
newHealthCheckObservation ::
  HealthCheckObservation
newHealthCheckObservation =
  HealthCheckObservation'
    { statusReport =
        Prelude.Nothing,
      region = Prelude.Nothing,
      iPAddress = Prelude.Nothing
    }

-- | A complex type that contains the last failure reason as reported by one
-- Amazon Route 53 health checker and the time of the failed health check.
healthCheckObservation_statusReport :: Lens.Lens' HealthCheckObservation (Prelude.Maybe StatusReport)
healthCheckObservation_statusReport = Lens.lens (\HealthCheckObservation' {statusReport} -> statusReport) (\s@HealthCheckObservation' {} a -> s {statusReport = a} :: HealthCheckObservation)

-- | The region of the Amazon Route 53 health checker that provided the
-- status in @StatusReport@.
healthCheckObservation_region :: Lens.Lens' HealthCheckObservation (Prelude.Maybe HealthCheckRegion)
healthCheckObservation_region = Lens.lens (\HealthCheckObservation' {region} -> region) (\s@HealthCheckObservation' {} a -> s {region = a} :: HealthCheckObservation)

-- | The IP address of the Amazon Route 53 health checker that provided the
-- failure reason in @StatusReport@.
healthCheckObservation_iPAddress :: Lens.Lens' HealthCheckObservation (Prelude.Maybe Prelude.Text)
healthCheckObservation_iPAddress = Lens.lens (\HealthCheckObservation' {iPAddress} -> iPAddress) (\s@HealthCheckObservation' {} a -> s {iPAddress = a} :: HealthCheckObservation)

instance Data.FromXML HealthCheckObservation where
  parseXML x =
    HealthCheckObservation'
      Prelude.<$> (x Data..@? "StatusReport")
      Prelude.<*> (x Data..@? "Region")
      Prelude.<*> (x Data..@? "IPAddress")

instance Prelude.Hashable HealthCheckObservation where
  hashWithSalt _salt HealthCheckObservation' {..} =
    _salt `Prelude.hashWithSalt` statusReport
      `Prelude.hashWithSalt` region
      `Prelude.hashWithSalt` iPAddress

instance Prelude.NFData HealthCheckObservation where
  rnf HealthCheckObservation' {..} =
    Prelude.rnf statusReport
      `Prelude.seq` Prelude.rnf region
      `Prelude.seq` Prelude.rnf iPAddress
