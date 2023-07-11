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
-- Module      : Amazonka.FraudDetector.Types.TFIMetricDataPoint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.TFIMetricDataPoint where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The performance metrics data points for Transaction Fraud Insights (TFI)
-- model.
--
-- /See:/ 'newTFIMetricDataPoint' smart constructor.
data TFIMetricDataPoint = TFIMetricDataPoint'
  { -- | The false positive rate. This is the percentage of total legitimate
    -- events that are incorrectly predicted as fraud.
    fpr :: Prelude.Maybe Prelude.Double,
    -- | The percentage of fraud events correctly predicted as fraudulent as
    -- compared to all events predicted as fraudulent.
    precision :: Prelude.Maybe Prelude.Double,
    -- | The model threshold that specifies an acceptable fraud capture rate. For
    -- example, a threshold of 500 means any model score 500 or above is
    -- labeled as fraud.
    threshold :: Prelude.Maybe Prelude.Double,
    -- | The true positive rate. This is the percentage of total fraud the model
    -- detects. Also known as capture rate.
    tpr :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TFIMetricDataPoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fpr', 'tFIMetricDataPoint_fpr' - The false positive rate. This is the percentage of total legitimate
-- events that are incorrectly predicted as fraud.
--
-- 'precision', 'tFIMetricDataPoint_precision' - The percentage of fraud events correctly predicted as fraudulent as
-- compared to all events predicted as fraudulent.
--
-- 'threshold', 'tFIMetricDataPoint_threshold' - The model threshold that specifies an acceptable fraud capture rate. For
-- example, a threshold of 500 means any model score 500 or above is
-- labeled as fraud.
--
-- 'tpr', 'tFIMetricDataPoint_tpr' - The true positive rate. This is the percentage of total fraud the model
-- detects. Also known as capture rate.
newTFIMetricDataPoint ::
  TFIMetricDataPoint
newTFIMetricDataPoint =
  TFIMetricDataPoint'
    { fpr = Prelude.Nothing,
      precision = Prelude.Nothing,
      threshold = Prelude.Nothing,
      tpr = Prelude.Nothing
    }

-- | The false positive rate. This is the percentage of total legitimate
-- events that are incorrectly predicted as fraud.
tFIMetricDataPoint_fpr :: Lens.Lens' TFIMetricDataPoint (Prelude.Maybe Prelude.Double)
tFIMetricDataPoint_fpr = Lens.lens (\TFIMetricDataPoint' {fpr} -> fpr) (\s@TFIMetricDataPoint' {} a -> s {fpr = a} :: TFIMetricDataPoint)

-- | The percentage of fraud events correctly predicted as fraudulent as
-- compared to all events predicted as fraudulent.
tFIMetricDataPoint_precision :: Lens.Lens' TFIMetricDataPoint (Prelude.Maybe Prelude.Double)
tFIMetricDataPoint_precision = Lens.lens (\TFIMetricDataPoint' {precision} -> precision) (\s@TFIMetricDataPoint' {} a -> s {precision = a} :: TFIMetricDataPoint)

-- | The model threshold that specifies an acceptable fraud capture rate. For
-- example, a threshold of 500 means any model score 500 or above is
-- labeled as fraud.
tFIMetricDataPoint_threshold :: Lens.Lens' TFIMetricDataPoint (Prelude.Maybe Prelude.Double)
tFIMetricDataPoint_threshold = Lens.lens (\TFIMetricDataPoint' {threshold} -> threshold) (\s@TFIMetricDataPoint' {} a -> s {threshold = a} :: TFIMetricDataPoint)

-- | The true positive rate. This is the percentage of total fraud the model
-- detects. Also known as capture rate.
tFIMetricDataPoint_tpr :: Lens.Lens' TFIMetricDataPoint (Prelude.Maybe Prelude.Double)
tFIMetricDataPoint_tpr = Lens.lens (\TFIMetricDataPoint' {tpr} -> tpr) (\s@TFIMetricDataPoint' {} a -> s {tpr = a} :: TFIMetricDataPoint)

instance Data.FromJSON TFIMetricDataPoint where
  parseJSON =
    Data.withObject
      "TFIMetricDataPoint"
      ( \x ->
          TFIMetricDataPoint'
            Prelude.<$> (x Data..:? "fpr")
            Prelude.<*> (x Data..:? "precision")
            Prelude.<*> (x Data..:? "threshold")
            Prelude.<*> (x Data..:? "tpr")
      )

instance Prelude.Hashable TFIMetricDataPoint where
  hashWithSalt _salt TFIMetricDataPoint' {..} =
    _salt
      `Prelude.hashWithSalt` fpr
      `Prelude.hashWithSalt` precision
      `Prelude.hashWithSalt` threshold
      `Prelude.hashWithSalt` tpr

instance Prelude.NFData TFIMetricDataPoint where
  rnf TFIMetricDataPoint' {..} =
    Prelude.rnf fpr
      `Prelude.seq` Prelude.rnf precision
      `Prelude.seq` Prelude.rnf threshold
      `Prelude.seq` Prelude.rnf tpr
