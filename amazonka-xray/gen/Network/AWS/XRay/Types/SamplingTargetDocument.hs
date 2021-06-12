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
-- Module      : Network.AWS.XRay.Types.SamplingTargetDocument
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.SamplingTargetDocument where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Temporary changes to a sampling rule configuration. To meet the global
-- sampling target for a rule, X-Ray calculates a new reservoir for each
-- service based on the recent sampling results of all services that called
-- GetSamplingTargets.
--
-- /See:/ 'newSamplingTargetDocument' smart constructor.
data SamplingTargetDocument = SamplingTargetDocument'
  { -- | The number of requests per second that X-Ray allocated for this service.
    reservoirQuota :: Core.Maybe Core.Int,
    -- | The percentage of matching requests to instrument, after the reservoir
    -- is exhausted.
    fixedRate :: Core.Maybe Core.Double,
    -- | The name of the sampling rule.
    ruleName :: Core.Maybe Core.Text,
    -- | When the reservoir quota expires.
    reservoirQuotaTTL :: Core.Maybe Core.POSIX,
    -- | The number of seconds for the service to wait before getting sampling
    -- targets again.
    interval :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SamplingTargetDocument' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reservoirQuota', 'samplingTargetDocument_reservoirQuota' - The number of requests per second that X-Ray allocated for this service.
--
-- 'fixedRate', 'samplingTargetDocument_fixedRate' - The percentage of matching requests to instrument, after the reservoir
-- is exhausted.
--
-- 'ruleName', 'samplingTargetDocument_ruleName' - The name of the sampling rule.
--
-- 'reservoirQuotaTTL', 'samplingTargetDocument_reservoirQuotaTTL' - When the reservoir quota expires.
--
-- 'interval', 'samplingTargetDocument_interval' - The number of seconds for the service to wait before getting sampling
-- targets again.
newSamplingTargetDocument ::
  SamplingTargetDocument
newSamplingTargetDocument =
  SamplingTargetDocument'
    { reservoirQuota =
        Core.Nothing,
      fixedRate = Core.Nothing,
      ruleName = Core.Nothing,
      reservoirQuotaTTL = Core.Nothing,
      interval = Core.Nothing
    }

-- | The number of requests per second that X-Ray allocated for this service.
samplingTargetDocument_reservoirQuota :: Lens.Lens' SamplingTargetDocument (Core.Maybe Core.Int)
samplingTargetDocument_reservoirQuota = Lens.lens (\SamplingTargetDocument' {reservoirQuota} -> reservoirQuota) (\s@SamplingTargetDocument' {} a -> s {reservoirQuota = a} :: SamplingTargetDocument)

-- | The percentage of matching requests to instrument, after the reservoir
-- is exhausted.
samplingTargetDocument_fixedRate :: Lens.Lens' SamplingTargetDocument (Core.Maybe Core.Double)
samplingTargetDocument_fixedRate = Lens.lens (\SamplingTargetDocument' {fixedRate} -> fixedRate) (\s@SamplingTargetDocument' {} a -> s {fixedRate = a} :: SamplingTargetDocument)

-- | The name of the sampling rule.
samplingTargetDocument_ruleName :: Lens.Lens' SamplingTargetDocument (Core.Maybe Core.Text)
samplingTargetDocument_ruleName = Lens.lens (\SamplingTargetDocument' {ruleName} -> ruleName) (\s@SamplingTargetDocument' {} a -> s {ruleName = a} :: SamplingTargetDocument)

-- | When the reservoir quota expires.
samplingTargetDocument_reservoirQuotaTTL :: Lens.Lens' SamplingTargetDocument (Core.Maybe Core.UTCTime)
samplingTargetDocument_reservoirQuotaTTL = Lens.lens (\SamplingTargetDocument' {reservoirQuotaTTL} -> reservoirQuotaTTL) (\s@SamplingTargetDocument' {} a -> s {reservoirQuotaTTL = a} :: SamplingTargetDocument) Core.. Lens.mapping Core._Time

-- | The number of seconds for the service to wait before getting sampling
-- targets again.
samplingTargetDocument_interval :: Lens.Lens' SamplingTargetDocument (Core.Maybe Core.Int)
samplingTargetDocument_interval = Lens.lens (\SamplingTargetDocument' {interval} -> interval) (\s@SamplingTargetDocument' {} a -> s {interval = a} :: SamplingTargetDocument)

instance Core.FromJSON SamplingTargetDocument where
  parseJSON =
    Core.withObject
      "SamplingTargetDocument"
      ( \x ->
          SamplingTargetDocument'
            Core.<$> (x Core..:? "ReservoirQuota")
            Core.<*> (x Core..:? "FixedRate")
            Core.<*> (x Core..:? "RuleName")
            Core.<*> (x Core..:? "ReservoirQuotaTTL")
            Core.<*> (x Core..:? "Interval")
      )

instance Core.Hashable SamplingTargetDocument

instance Core.NFData SamplingTargetDocument
