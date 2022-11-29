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
-- Module      : Amazonka.FraudDetector.Types.TFIModelPerformance
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.TFIModelPerformance where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The Transaction Fraud Insights (TFI) model performance score.
--
-- /See:/ 'newTFIModelPerformance' smart constructor.
data TFIModelPerformance = TFIModelPerformance'
  { -- | The area under the curve (auc). This summarizes the total positive rate
    -- (tpr) and false positive rate (FPR) across all possible model score
    -- thresholds.
    auc :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TFIModelPerformance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'auc', 'tFIModelPerformance_auc' - The area under the curve (auc). This summarizes the total positive rate
-- (tpr) and false positive rate (FPR) across all possible model score
-- thresholds.
newTFIModelPerformance ::
  TFIModelPerformance
newTFIModelPerformance =
  TFIModelPerformance' {auc = Prelude.Nothing}

-- | The area under the curve (auc). This summarizes the total positive rate
-- (tpr) and false positive rate (FPR) across all possible model score
-- thresholds.
tFIModelPerformance_auc :: Lens.Lens' TFIModelPerformance (Prelude.Maybe Prelude.Double)
tFIModelPerformance_auc = Lens.lens (\TFIModelPerformance' {auc} -> auc) (\s@TFIModelPerformance' {} a -> s {auc = a} :: TFIModelPerformance)

instance Core.FromJSON TFIModelPerformance where
  parseJSON =
    Core.withObject
      "TFIModelPerformance"
      ( \x ->
          TFIModelPerformance' Prelude.<$> (x Core..:? "auc")
      )

instance Prelude.Hashable TFIModelPerformance where
  hashWithSalt _salt TFIModelPerformance' {..} =
    _salt `Prelude.hashWithSalt` auc

instance Prelude.NFData TFIModelPerformance where
  rnf TFIModelPerformance' {..} = Prelude.rnf auc
