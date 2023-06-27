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
-- Module      : Amazonka.WellArchitected.Types.LensMetric
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types.LensMetric where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WellArchitected.Types.PillarMetric
import Amazonka.WellArchitected.Types.Risk

-- | A metric for a particular lens in a workload.
--
-- /See:/ 'newLensMetric' smart constructor.
data LensMetric = LensMetric'
  { -- | The lens ARN.
    lensArn :: Prelude.Maybe Prelude.Text,
    -- | The metrics for the pillars in a lens.
    pillars :: Prelude.Maybe [PillarMetric],
    riskCounts :: Prelude.Maybe (Prelude.HashMap Risk Prelude.Natural)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LensMetric' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lensArn', 'lensMetric_lensArn' - The lens ARN.
--
-- 'pillars', 'lensMetric_pillars' - The metrics for the pillars in a lens.
--
-- 'riskCounts', 'lensMetric_riskCounts' - Undocumented member.
newLensMetric ::
  LensMetric
newLensMetric =
  LensMetric'
    { lensArn = Prelude.Nothing,
      pillars = Prelude.Nothing,
      riskCounts = Prelude.Nothing
    }

-- | The lens ARN.
lensMetric_lensArn :: Lens.Lens' LensMetric (Prelude.Maybe Prelude.Text)
lensMetric_lensArn = Lens.lens (\LensMetric' {lensArn} -> lensArn) (\s@LensMetric' {} a -> s {lensArn = a} :: LensMetric)

-- | The metrics for the pillars in a lens.
lensMetric_pillars :: Lens.Lens' LensMetric (Prelude.Maybe [PillarMetric])
lensMetric_pillars = Lens.lens (\LensMetric' {pillars} -> pillars) (\s@LensMetric' {} a -> s {pillars = a} :: LensMetric) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
lensMetric_riskCounts :: Lens.Lens' LensMetric (Prelude.Maybe (Prelude.HashMap Risk Prelude.Natural))
lensMetric_riskCounts = Lens.lens (\LensMetric' {riskCounts} -> riskCounts) (\s@LensMetric' {} a -> s {riskCounts = a} :: LensMetric) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON LensMetric where
  parseJSON =
    Data.withObject
      "LensMetric"
      ( \x ->
          LensMetric'
            Prelude.<$> (x Data..:? "LensArn")
            Prelude.<*> (x Data..:? "Pillars" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "RiskCounts" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable LensMetric where
  hashWithSalt _salt LensMetric' {..} =
    _salt
      `Prelude.hashWithSalt` lensArn
      `Prelude.hashWithSalt` pillars
      `Prelude.hashWithSalt` riskCounts

instance Prelude.NFData LensMetric where
  rnf LensMetric' {..} =
    Prelude.rnf lensArn
      `Prelude.seq` Prelude.rnf pillars
      `Prelude.seq` Prelude.rnf riskCounts
