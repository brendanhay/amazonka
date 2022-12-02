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
-- Module      : Amazonka.FraudDetector.Types.OFIMetricDataPoint
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.OFIMetricDataPoint where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The Online Fraud Insights (OFI) model performance metrics data points.
--
-- /See:/ 'newOFIMetricDataPoint' smart constructor.
data OFIMetricDataPoint = OFIMetricDataPoint'
  { -- | The true positive rate. This is the percentage of total fraud the model
    -- detects. Also known as capture rate.
    tpr :: Prelude.Maybe Prelude.Double,
    -- | The false positive rate. This is the percentage of total legitimate
    -- events that are incorrectly predicted as fraud.
    fpr :: Prelude.Maybe Prelude.Double,
    -- | The percentage of fraud events correctly predicted as fraudulent as
    -- compared to all events predicted as fraudulent.
    precision :: Prelude.Maybe Prelude.Double,
    -- | The model threshold that specifies an acceptable fraud capture rate. For
    -- example, a threshold of 500 means any model score 500 or above is
    -- labeled as fraud.
    threshold :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OFIMetricDataPoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tpr', 'oFIMetricDataPoint_tpr' - The true positive rate. This is the percentage of total fraud the model
-- detects. Also known as capture rate.
--
-- 'fpr', 'oFIMetricDataPoint_fpr' - The false positive rate. This is the percentage of total legitimate
-- events that are incorrectly predicted as fraud.
--
-- 'precision', 'oFIMetricDataPoint_precision' - The percentage of fraud events correctly predicted as fraudulent as
-- compared to all events predicted as fraudulent.
--
-- 'threshold', 'oFIMetricDataPoint_threshold' - The model threshold that specifies an acceptable fraud capture rate. For
-- example, a threshold of 500 means any model score 500 or above is
-- labeled as fraud.
newOFIMetricDataPoint ::
  OFIMetricDataPoint
newOFIMetricDataPoint =
  OFIMetricDataPoint'
    { tpr = Prelude.Nothing,
      fpr = Prelude.Nothing,
      precision = Prelude.Nothing,
      threshold = Prelude.Nothing
    }

-- | The true positive rate. This is the percentage of total fraud the model
-- detects. Also known as capture rate.
oFIMetricDataPoint_tpr :: Lens.Lens' OFIMetricDataPoint (Prelude.Maybe Prelude.Double)
oFIMetricDataPoint_tpr = Lens.lens (\OFIMetricDataPoint' {tpr} -> tpr) (\s@OFIMetricDataPoint' {} a -> s {tpr = a} :: OFIMetricDataPoint)

-- | The false positive rate. This is the percentage of total legitimate
-- events that are incorrectly predicted as fraud.
oFIMetricDataPoint_fpr :: Lens.Lens' OFIMetricDataPoint (Prelude.Maybe Prelude.Double)
oFIMetricDataPoint_fpr = Lens.lens (\OFIMetricDataPoint' {fpr} -> fpr) (\s@OFIMetricDataPoint' {} a -> s {fpr = a} :: OFIMetricDataPoint)

-- | The percentage of fraud events correctly predicted as fraudulent as
-- compared to all events predicted as fraudulent.
oFIMetricDataPoint_precision :: Lens.Lens' OFIMetricDataPoint (Prelude.Maybe Prelude.Double)
oFIMetricDataPoint_precision = Lens.lens (\OFIMetricDataPoint' {precision} -> precision) (\s@OFIMetricDataPoint' {} a -> s {precision = a} :: OFIMetricDataPoint)

-- | The model threshold that specifies an acceptable fraud capture rate. For
-- example, a threshold of 500 means any model score 500 or above is
-- labeled as fraud.
oFIMetricDataPoint_threshold :: Lens.Lens' OFIMetricDataPoint (Prelude.Maybe Prelude.Double)
oFIMetricDataPoint_threshold = Lens.lens (\OFIMetricDataPoint' {threshold} -> threshold) (\s@OFIMetricDataPoint' {} a -> s {threshold = a} :: OFIMetricDataPoint)

instance Data.FromJSON OFIMetricDataPoint where
  parseJSON =
    Data.withObject
      "OFIMetricDataPoint"
      ( \x ->
          OFIMetricDataPoint'
            Prelude.<$> (x Data..:? "tpr")
            Prelude.<*> (x Data..:? "fpr")
            Prelude.<*> (x Data..:? "precision")
            Prelude.<*> (x Data..:? "threshold")
      )

instance Prelude.Hashable OFIMetricDataPoint where
  hashWithSalt _salt OFIMetricDataPoint' {..} =
    _salt `Prelude.hashWithSalt` tpr
      `Prelude.hashWithSalt` fpr
      `Prelude.hashWithSalt` precision
      `Prelude.hashWithSalt` threshold

instance Prelude.NFData OFIMetricDataPoint where
  rnf OFIMetricDataPoint' {..} =
    Prelude.rnf tpr
      `Prelude.seq` Prelude.rnf fpr
      `Prelude.seq` Prelude.rnf precision
      `Prelude.seq` Prelude.rnf threshold
