{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Route53.Types.HealthCheckObservation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.HealthCheckObservation where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Route53.Internal
import Network.AWS.Route53.Types.HealthCheckRegion
import Network.AWS.Route53.Types.StatusReport

-- | A complex type that contains the last failure reason as reported by one
-- Amazon Route 53 health checker.
--
-- /See:/ 'newHealthCheckObservation' smart constructor.
data HealthCheckObservation = HealthCheckObservation'
  { -- | The IP address of the Amazon Route 53 health checker that provided the
    -- failure reason in @StatusReport@.
    iPAddress :: Prelude.Maybe Prelude.Text,
    -- | The region of the Amazon Route 53 health checker that provided the
    -- status in @StatusReport@.
    region :: Prelude.Maybe HealthCheckRegion,
    -- | A complex type that contains the last failure reason as reported by one
    -- Amazon Route 53 health checker and the time of the failed health check.
    statusReport :: Prelude.Maybe StatusReport
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'HealthCheckObservation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'iPAddress', 'healthCheckObservation_iPAddress' - The IP address of the Amazon Route 53 health checker that provided the
-- failure reason in @StatusReport@.
--
-- 'region', 'healthCheckObservation_region' - The region of the Amazon Route 53 health checker that provided the
-- status in @StatusReport@.
--
-- 'statusReport', 'healthCheckObservation_statusReport' - A complex type that contains the last failure reason as reported by one
-- Amazon Route 53 health checker and the time of the failed health check.
newHealthCheckObservation ::
  HealthCheckObservation
newHealthCheckObservation =
  HealthCheckObservation'
    { iPAddress =
        Prelude.Nothing,
      region = Prelude.Nothing,
      statusReport = Prelude.Nothing
    }

-- | The IP address of the Amazon Route 53 health checker that provided the
-- failure reason in @StatusReport@.
healthCheckObservation_iPAddress :: Lens.Lens' HealthCheckObservation (Prelude.Maybe Prelude.Text)
healthCheckObservation_iPAddress = Lens.lens (\HealthCheckObservation' {iPAddress} -> iPAddress) (\s@HealthCheckObservation' {} a -> s {iPAddress = a} :: HealthCheckObservation)

-- | The region of the Amazon Route 53 health checker that provided the
-- status in @StatusReport@.
healthCheckObservation_region :: Lens.Lens' HealthCheckObservation (Prelude.Maybe HealthCheckRegion)
healthCheckObservation_region = Lens.lens (\HealthCheckObservation' {region} -> region) (\s@HealthCheckObservation' {} a -> s {region = a} :: HealthCheckObservation)

-- | A complex type that contains the last failure reason as reported by one
-- Amazon Route 53 health checker and the time of the failed health check.
healthCheckObservation_statusReport :: Lens.Lens' HealthCheckObservation (Prelude.Maybe StatusReport)
healthCheckObservation_statusReport = Lens.lens (\HealthCheckObservation' {statusReport} -> statusReport) (\s@HealthCheckObservation' {} a -> s {statusReport = a} :: HealthCheckObservation)

instance Prelude.FromXML HealthCheckObservation where
  parseXML x =
    HealthCheckObservation'
      Prelude.<$> (x Prelude..@? "IPAddress")
      Prelude.<*> (x Prelude..@? "Region")
      Prelude.<*> (x Prelude..@? "StatusReport")

instance Prelude.Hashable HealthCheckObservation

instance Prelude.NFData HealthCheckObservation
