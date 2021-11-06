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
-- Module      : Amazonka.XRay.Types.SamplingTargetDocument
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.XRay.Types.SamplingTargetDocument where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Temporary changes to a sampling rule configuration. To meet the global
-- sampling target for a rule, X-Ray calculates a new reservoir for each
-- service based on the recent sampling results of all services that called
-- <https://docs.aws.amazon.com/xray/latest/api/API_GetSamplingTargets.html GetSamplingTargets>.
--
-- /See:/ 'newSamplingTargetDocument' smart constructor.
data SamplingTargetDocument = SamplingTargetDocument'
  { -- | The number of requests per second that X-Ray allocated for this service.
    reservoirQuota :: Prelude.Maybe Prelude.Int,
    -- | The name of the sampling rule.
    ruleName :: Prelude.Maybe Prelude.Text,
    -- | The percentage of matching requests to instrument, after the reservoir
    -- is exhausted.
    fixedRate :: Prelude.Maybe Prelude.Double,
    -- | The number of seconds for the service to wait before getting sampling
    -- targets again.
    interval :: Prelude.Maybe Prelude.Int,
    -- | When the reservoir quota expires.
    reservoirQuotaTTL :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'ruleName', 'samplingTargetDocument_ruleName' - The name of the sampling rule.
--
-- 'fixedRate', 'samplingTargetDocument_fixedRate' - The percentage of matching requests to instrument, after the reservoir
-- is exhausted.
--
-- 'interval', 'samplingTargetDocument_interval' - The number of seconds for the service to wait before getting sampling
-- targets again.
--
-- 'reservoirQuotaTTL', 'samplingTargetDocument_reservoirQuotaTTL' - When the reservoir quota expires.
newSamplingTargetDocument ::
  SamplingTargetDocument
newSamplingTargetDocument =
  SamplingTargetDocument'
    { reservoirQuota =
        Prelude.Nothing,
      ruleName = Prelude.Nothing,
      fixedRate = Prelude.Nothing,
      interval = Prelude.Nothing,
      reservoirQuotaTTL = Prelude.Nothing
    }

-- | The number of requests per second that X-Ray allocated for this service.
samplingTargetDocument_reservoirQuota :: Lens.Lens' SamplingTargetDocument (Prelude.Maybe Prelude.Int)
samplingTargetDocument_reservoirQuota = Lens.lens (\SamplingTargetDocument' {reservoirQuota} -> reservoirQuota) (\s@SamplingTargetDocument' {} a -> s {reservoirQuota = a} :: SamplingTargetDocument)

-- | The name of the sampling rule.
samplingTargetDocument_ruleName :: Lens.Lens' SamplingTargetDocument (Prelude.Maybe Prelude.Text)
samplingTargetDocument_ruleName = Lens.lens (\SamplingTargetDocument' {ruleName} -> ruleName) (\s@SamplingTargetDocument' {} a -> s {ruleName = a} :: SamplingTargetDocument)

-- | The percentage of matching requests to instrument, after the reservoir
-- is exhausted.
samplingTargetDocument_fixedRate :: Lens.Lens' SamplingTargetDocument (Prelude.Maybe Prelude.Double)
samplingTargetDocument_fixedRate = Lens.lens (\SamplingTargetDocument' {fixedRate} -> fixedRate) (\s@SamplingTargetDocument' {} a -> s {fixedRate = a} :: SamplingTargetDocument)

-- | The number of seconds for the service to wait before getting sampling
-- targets again.
samplingTargetDocument_interval :: Lens.Lens' SamplingTargetDocument (Prelude.Maybe Prelude.Int)
samplingTargetDocument_interval = Lens.lens (\SamplingTargetDocument' {interval} -> interval) (\s@SamplingTargetDocument' {} a -> s {interval = a} :: SamplingTargetDocument)

-- | When the reservoir quota expires.
samplingTargetDocument_reservoirQuotaTTL :: Lens.Lens' SamplingTargetDocument (Prelude.Maybe Prelude.UTCTime)
samplingTargetDocument_reservoirQuotaTTL = Lens.lens (\SamplingTargetDocument' {reservoirQuotaTTL} -> reservoirQuotaTTL) (\s@SamplingTargetDocument' {} a -> s {reservoirQuotaTTL = a} :: SamplingTargetDocument) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON SamplingTargetDocument where
  parseJSON =
    Core.withObject
      "SamplingTargetDocument"
      ( \x ->
          SamplingTargetDocument'
            Prelude.<$> (x Core..:? "ReservoirQuota")
            Prelude.<*> (x Core..:? "RuleName")
            Prelude.<*> (x Core..:? "FixedRate")
            Prelude.<*> (x Core..:? "Interval")
            Prelude.<*> (x Core..:? "ReservoirQuotaTTL")
      )

instance Prelude.Hashable SamplingTargetDocument

instance Prelude.NFData SamplingTargetDocument
