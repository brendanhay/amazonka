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
-- Module      : Amazonka.FraudDetector.Types.ATIMetricDataPoint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.ATIMetricDataPoint where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The Account Takeover Insights (ATI) model performance metrics data
-- points.
--
-- /See:/ 'newATIMetricDataPoint' smart constructor.
data ATIMetricDataPoint = ATIMetricDataPoint'
  { -- | The anomaly discovery rate. This metric quantifies the percentage of
    -- anomalies that can be detected by the model at the selected score
    -- threshold. A lower score threshold increases the percentage of anomalies
    -- captured by the model, but would also require challenging a larger
    -- percentage of login events, leading to a higher customer friction.
    adr :: Prelude.Maybe Prelude.Double,
    -- | The account takeover discovery rate. This metric quantifies the
    -- percentage of account compromise events that can be detected by the
    -- model at the selected score threshold. This metric is only available if
    -- 50 or more entities with at-least one labeled account takeover event is
    -- present in the ingested dataset.
    atodr :: Prelude.Maybe Prelude.Double,
    -- | The challenge rate. This indicates the percentage of login events that
    -- the model recommends to challenge such as one-time password,
    -- multi-factor authentication, and investigations.
    cr :: Prelude.Maybe Prelude.Double,
    -- | The model\'s threshold that specifies an acceptable fraud capture rate.
    -- For example, a threshold of 500 means any model score 500 or above is
    -- labeled as fraud.
    threshold :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ATIMetricDataPoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'adr', 'aTIMetricDataPoint_adr' - The anomaly discovery rate. This metric quantifies the percentage of
-- anomalies that can be detected by the model at the selected score
-- threshold. A lower score threshold increases the percentage of anomalies
-- captured by the model, but would also require challenging a larger
-- percentage of login events, leading to a higher customer friction.
--
-- 'atodr', 'aTIMetricDataPoint_atodr' - The account takeover discovery rate. This metric quantifies the
-- percentage of account compromise events that can be detected by the
-- model at the selected score threshold. This metric is only available if
-- 50 or more entities with at-least one labeled account takeover event is
-- present in the ingested dataset.
--
-- 'cr', 'aTIMetricDataPoint_cr' - The challenge rate. This indicates the percentage of login events that
-- the model recommends to challenge such as one-time password,
-- multi-factor authentication, and investigations.
--
-- 'threshold', 'aTIMetricDataPoint_threshold' - The model\'s threshold that specifies an acceptable fraud capture rate.
-- For example, a threshold of 500 means any model score 500 or above is
-- labeled as fraud.
newATIMetricDataPoint ::
  ATIMetricDataPoint
newATIMetricDataPoint =
  ATIMetricDataPoint'
    { adr = Prelude.Nothing,
      atodr = Prelude.Nothing,
      cr = Prelude.Nothing,
      threshold = Prelude.Nothing
    }

-- | The anomaly discovery rate. This metric quantifies the percentage of
-- anomalies that can be detected by the model at the selected score
-- threshold. A lower score threshold increases the percentage of anomalies
-- captured by the model, but would also require challenging a larger
-- percentage of login events, leading to a higher customer friction.
aTIMetricDataPoint_adr :: Lens.Lens' ATIMetricDataPoint (Prelude.Maybe Prelude.Double)
aTIMetricDataPoint_adr = Lens.lens (\ATIMetricDataPoint' {adr} -> adr) (\s@ATIMetricDataPoint' {} a -> s {adr = a} :: ATIMetricDataPoint)

-- | The account takeover discovery rate. This metric quantifies the
-- percentage of account compromise events that can be detected by the
-- model at the selected score threshold. This metric is only available if
-- 50 or more entities with at-least one labeled account takeover event is
-- present in the ingested dataset.
aTIMetricDataPoint_atodr :: Lens.Lens' ATIMetricDataPoint (Prelude.Maybe Prelude.Double)
aTIMetricDataPoint_atodr = Lens.lens (\ATIMetricDataPoint' {atodr} -> atodr) (\s@ATIMetricDataPoint' {} a -> s {atodr = a} :: ATIMetricDataPoint)

-- | The challenge rate. This indicates the percentage of login events that
-- the model recommends to challenge such as one-time password,
-- multi-factor authentication, and investigations.
aTIMetricDataPoint_cr :: Lens.Lens' ATIMetricDataPoint (Prelude.Maybe Prelude.Double)
aTIMetricDataPoint_cr = Lens.lens (\ATIMetricDataPoint' {cr} -> cr) (\s@ATIMetricDataPoint' {} a -> s {cr = a} :: ATIMetricDataPoint)

-- | The model\'s threshold that specifies an acceptable fraud capture rate.
-- For example, a threshold of 500 means any model score 500 or above is
-- labeled as fraud.
aTIMetricDataPoint_threshold :: Lens.Lens' ATIMetricDataPoint (Prelude.Maybe Prelude.Double)
aTIMetricDataPoint_threshold = Lens.lens (\ATIMetricDataPoint' {threshold} -> threshold) (\s@ATIMetricDataPoint' {} a -> s {threshold = a} :: ATIMetricDataPoint)

instance Data.FromJSON ATIMetricDataPoint where
  parseJSON =
    Data.withObject
      "ATIMetricDataPoint"
      ( \x ->
          ATIMetricDataPoint'
            Prelude.<$> (x Data..:? "adr")
            Prelude.<*> (x Data..:? "atodr")
            Prelude.<*> (x Data..:? "cr")
            Prelude.<*> (x Data..:? "threshold")
      )

instance Prelude.Hashable ATIMetricDataPoint where
  hashWithSalt _salt ATIMetricDataPoint' {..} =
    _salt
      `Prelude.hashWithSalt` adr
      `Prelude.hashWithSalt` atodr
      `Prelude.hashWithSalt` cr
      `Prelude.hashWithSalt` threshold

instance Prelude.NFData ATIMetricDataPoint where
  rnf ATIMetricDataPoint' {..} =
    Prelude.rnf adr
      `Prelude.seq` Prelude.rnf atodr
      `Prelude.seq` Prelude.rnf cr
      `Prelude.seq` Prelude.rnf threshold
