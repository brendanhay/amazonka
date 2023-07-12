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
-- Module      : Amazonka.FraudDetector.Types.TrainingMetricsV2
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.TrainingMetricsV2 where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FraudDetector.Types.ATITrainingMetricsValue
import Amazonka.FraudDetector.Types.OFITrainingMetricsValue
import Amazonka.FraudDetector.Types.TFITrainingMetricsValue
import qualified Amazonka.Prelude as Prelude

-- | The training metrics details.
--
-- /See:/ 'newTrainingMetricsV2' smart constructor.
data TrainingMetricsV2 = TrainingMetricsV2'
  { -- | The Account Takeover Insights (ATI) model training metric details.
    ati :: Prelude.Maybe ATITrainingMetricsValue,
    -- | The Online Fraud Insights (OFI) model training metric details.
    ofi :: Prelude.Maybe OFITrainingMetricsValue,
    -- | The Transaction Fraud Insights (TFI) model training metric details.
    tfi :: Prelude.Maybe TFITrainingMetricsValue
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TrainingMetricsV2' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ati', 'trainingMetricsV2_ati' - The Account Takeover Insights (ATI) model training metric details.
--
-- 'ofi', 'trainingMetricsV2_ofi' - The Online Fraud Insights (OFI) model training metric details.
--
-- 'tfi', 'trainingMetricsV2_tfi' - The Transaction Fraud Insights (TFI) model training metric details.
newTrainingMetricsV2 ::
  TrainingMetricsV2
newTrainingMetricsV2 =
  TrainingMetricsV2'
    { ati = Prelude.Nothing,
      ofi = Prelude.Nothing,
      tfi = Prelude.Nothing
    }

-- | The Account Takeover Insights (ATI) model training metric details.
trainingMetricsV2_ati :: Lens.Lens' TrainingMetricsV2 (Prelude.Maybe ATITrainingMetricsValue)
trainingMetricsV2_ati = Lens.lens (\TrainingMetricsV2' {ati} -> ati) (\s@TrainingMetricsV2' {} a -> s {ati = a} :: TrainingMetricsV2)

-- | The Online Fraud Insights (OFI) model training metric details.
trainingMetricsV2_ofi :: Lens.Lens' TrainingMetricsV2 (Prelude.Maybe OFITrainingMetricsValue)
trainingMetricsV2_ofi = Lens.lens (\TrainingMetricsV2' {ofi} -> ofi) (\s@TrainingMetricsV2' {} a -> s {ofi = a} :: TrainingMetricsV2)

-- | The Transaction Fraud Insights (TFI) model training metric details.
trainingMetricsV2_tfi :: Lens.Lens' TrainingMetricsV2 (Prelude.Maybe TFITrainingMetricsValue)
trainingMetricsV2_tfi = Lens.lens (\TrainingMetricsV2' {tfi} -> tfi) (\s@TrainingMetricsV2' {} a -> s {tfi = a} :: TrainingMetricsV2)

instance Data.FromJSON TrainingMetricsV2 where
  parseJSON =
    Data.withObject
      "TrainingMetricsV2"
      ( \x ->
          TrainingMetricsV2'
            Prelude.<$> (x Data..:? "ati")
            Prelude.<*> (x Data..:? "ofi")
            Prelude.<*> (x Data..:? "tfi")
      )

instance Prelude.Hashable TrainingMetricsV2 where
  hashWithSalt _salt TrainingMetricsV2' {..} =
    _salt
      `Prelude.hashWithSalt` ati
      `Prelude.hashWithSalt` ofi
      `Prelude.hashWithSalt` tfi

instance Prelude.NFData TrainingMetricsV2 where
  rnf TrainingMetricsV2' {..} =
    Prelude.rnf ati
      `Prelude.seq` Prelude.rnf ofi
      `Prelude.seq` Prelude.rnf tfi
