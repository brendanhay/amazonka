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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.XRay.Types.SamplingTargetDocument where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Temporary changes to a sampling rule configuration. To meet the global
-- sampling target for a rule, X-Ray calculates a new reservoir for each
-- service based on the recent sampling results of all services that called
-- <https://docs.aws.amazon.com/xray/latest/api/API_GetSamplingTargets.html GetSamplingTargets>.
--
-- /See:/ 'newSamplingTargetDocument' smart constructor.
data SamplingTargetDocument = SamplingTargetDocument'
  { -- | The percentage of matching requests to instrument, after the reservoir
    -- is exhausted.
    fixedRate :: Prelude.Maybe Prelude.Double,
    -- | The number of seconds for the service to wait before getting sampling
    -- targets again.
    interval :: Prelude.Maybe Prelude.Int,
    -- | The number of requests per second that X-Ray allocated for this service.
    reservoirQuota :: Prelude.Maybe Prelude.Int,
    -- | When the reservoir quota expires.
    reservoirQuotaTTL :: Prelude.Maybe Data.POSIX,
    -- | The name of the sampling rule.
    ruleName :: Prelude.Maybe Prelude.Text
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
-- 'fixedRate', 'samplingTargetDocument_fixedRate' - The percentage of matching requests to instrument, after the reservoir
-- is exhausted.
--
-- 'interval', 'samplingTargetDocument_interval' - The number of seconds for the service to wait before getting sampling
-- targets again.
--
-- 'reservoirQuota', 'samplingTargetDocument_reservoirQuota' - The number of requests per second that X-Ray allocated for this service.
--
-- 'reservoirQuotaTTL', 'samplingTargetDocument_reservoirQuotaTTL' - When the reservoir quota expires.
--
-- 'ruleName', 'samplingTargetDocument_ruleName' - The name of the sampling rule.
newSamplingTargetDocument ::
  SamplingTargetDocument
newSamplingTargetDocument =
  SamplingTargetDocument'
    { fixedRate =
        Prelude.Nothing,
      interval = Prelude.Nothing,
      reservoirQuota = Prelude.Nothing,
      reservoirQuotaTTL = Prelude.Nothing,
      ruleName = Prelude.Nothing
    }

-- | The percentage of matching requests to instrument, after the reservoir
-- is exhausted.
samplingTargetDocument_fixedRate :: Lens.Lens' SamplingTargetDocument (Prelude.Maybe Prelude.Double)
samplingTargetDocument_fixedRate = Lens.lens (\SamplingTargetDocument' {fixedRate} -> fixedRate) (\s@SamplingTargetDocument' {} a -> s {fixedRate = a} :: SamplingTargetDocument)

-- | The number of seconds for the service to wait before getting sampling
-- targets again.
samplingTargetDocument_interval :: Lens.Lens' SamplingTargetDocument (Prelude.Maybe Prelude.Int)
samplingTargetDocument_interval = Lens.lens (\SamplingTargetDocument' {interval} -> interval) (\s@SamplingTargetDocument' {} a -> s {interval = a} :: SamplingTargetDocument)

-- | The number of requests per second that X-Ray allocated for this service.
samplingTargetDocument_reservoirQuota :: Lens.Lens' SamplingTargetDocument (Prelude.Maybe Prelude.Int)
samplingTargetDocument_reservoirQuota = Lens.lens (\SamplingTargetDocument' {reservoirQuota} -> reservoirQuota) (\s@SamplingTargetDocument' {} a -> s {reservoirQuota = a} :: SamplingTargetDocument)

-- | When the reservoir quota expires.
samplingTargetDocument_reservoirQuotaTTL :: Lens.Lens' SamplingTargetDocument (Prelude.Maybe Prelude.UTCTime)
samplingTargetDocument_reservoirQuotaTTL = Lens.lens (\SamplingTargetDocument' {reservoirQuotaTTL} -> reservoirQuotaTTL) (\s@SamplingTargetDocument' {} a -> s {reservoirQuotaTTL = a} :: SamplingTargetDocument) Prelude.. Lens.mapping Data._Time

-- | The name of the sampling rule.
samplingTargetDocument_ruleName :: Lens.Lens' SamplingTargetDocument (Prelude.Maybe Prelude.Text)
samplingTargetDocument_ruleName = Lens.lens (\SamplingTargetDocument' {ruleName} -> ruleName) (\s@SamplingTargetDocument' {} a -> s {ruleName = a} :: SamplingTargetDocument)

instance Data.FromJSON SamplingTargetDocument where
  parseJSON =
    Data.withObject
      "SamplingTargetDocument"
      ( \x ->
          SamplingTargetDocument'
            Prelude.<$> (x Data..:? "FixedRate")
            Prelude.<*> (x Data..:? "Interval")
            Prelude.<*> (x Data..:? "ReservoirQuota")
            Prelude.<*> (x Data..:? "ReservoirQuotaTTL")
            Prelude.<*> (x Data..:? "RuleName")
      )

instance Prelude.Hashable SamplingTargetDocument where
  hashWithSalt _salt SamplingTargetDocument' {..} =
    _salt `Prelude.hashWithSalt` fixedRate
      `Prelude.hashWithSalt` interval
      `Prelude.hashWithSalt` reservoirQuota
      `Prelude.hashWithSalt` reservoirQuotaTTL
      `Prelude.hashWithSalt` ruleName

instance Prelude.NFData SamplingTargetDocument where
  rnf SamplingTargetDocument' {..} =
    Prelude.rnf fixedRate
      `Prelude.seq` Prelude.rnf interval
      `Prelude.seq` Prelude.rnf reservoirQuota
      `Prelude.seq` Prelude.rnf reservoirQuotaTTL
      `Prelude.seq` Prelude.rnf ruleName
