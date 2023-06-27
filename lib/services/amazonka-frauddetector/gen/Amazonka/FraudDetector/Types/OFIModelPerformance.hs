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
-- Module      : Amazonka.FraudDetector.Types.OFIModelPerformance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.OFIModelPerformance where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FraudDetector.Types.UncertaintyRange
import qualified Amazonka.Prelude as Prelude

-- | The Online Fraud Insights (OFI) model performance score.
--
-- /See:/ 'newOFIModelPerformance' smart constructor.
data OFIModelPerformance = OFIModelPerformance'
  { -- | The area under the curve (auc). This summarizes the total positive rate
    -- (tpr) and false positive rate (FPR) across all possible model score
    -- thresholds.
    auc :: Prelude.Maybe Prelude.Double,
    -- | Indicates the range of area under curve (auc) expected from the OFI
    -- model. A range greater than 0.1 indicates higher model uncertainity.
    uncertaintyRange :: Prelude.Maybe UncertaintyRange
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OFIModelPerformance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'auc', 'oFIModelPerformance_auc' - The area under the curve (auc). This summarizes the total positive rate
-- (tpr) and false positive rate (FPR) across all possible model score
-- thresholds.
--
-- 'uncertaintyRange', 'oFIModelPerformance_uncertaintyRange' - Indicates the range of area under curve (auc) expected from the OFI
-- model. A range greater than 0.1 indicates higher model uncertainity.
newOFIModelPerformance ::
  OFIModelPerformance
newOFIModelPerformance =
  OFIModelPerformance'
    { auc = Prelude.Nothing,
      uncertaintyRange = Prelude.Nothing
    }

-- | The area under the curve (auc). This summarizes the total positive rate
-- (tpr) and false positive rate (FPR) across all possible model score
-- thresholds.
oFIModelPerformance_auc :: Lens.Lens' OFIModelPerformance (Prelude.Maybe Prelude.Double)
oFIModelPerformance_auc = Lens.lens (\OFIModelPerformance' {auc} -> auc) (\s@OFIModelPerformance' {} a -> s {auc = a} :: OFIModelPerformance)

-- | Indicates the range of area under curve (auc) expected from the OFI
-- model. A range greater than 0.1 indicates higher model uncertainity.
oFIModelPerformance_uncertaintyRange :: Lens.Lens' OFIModelPerformance (Prelude.Maybe UncertaintyRange)
oFIModelPerformance_uncertaintyRange = Lens.lens (\OFIModelPerformance' {uncertaintyRange} -> uncertaintyRange) (\s@OFIModelPerformance' {} a -> s {uncertaintyRange = a} :: OFIModelPerformance)

instance Data.FromJSON OFIModelPerformance where
  parseJSON =
    Data.withObject
      "OFIModelPerformance"
      ( \x ->
          OFIModelPerformance'
            Prelude.<$> (x Data..:? "auc")
            Prelude.<*> (x Data..:? "uncertaintyRange")
      )

instance Prelude.Hashable OFIModelPerformance where
  hashWithSalt _salt OFIModelPerformance' {..} =
    _salt
      `Prelude.hashWithSalt` auc
      `Prelude.hashWithSalt` uncertaintyRange

instance Prelude.NFData OFIModelPerformance where
  rnf OFIModelPerformance' {..} =
    Prelude.rnf auc
      `Prelude.seq` Prelude.rnf uncertaintyRange
