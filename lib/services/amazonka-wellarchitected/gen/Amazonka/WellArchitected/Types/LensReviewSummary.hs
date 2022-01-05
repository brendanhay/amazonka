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
-- Module      : Amazonka.WellArchitected.Types.LensReviewSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types.LensReviewSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.WellArchitected.Types.LensStatus
import Amazonka.WellArchitected.Types.Risk

-- | A lens review summary of a workload.
--
-- /See:/ 'newLensReviewSummary' smart constructor.
data LensReviewSummary = LensReviewSummary'
  { lensAlias :: Prelude.Maybe Prelude.Text,
    riskCounts :: Prelude.Maybe (Prelude.HashMap Risk Prelude.Natural),
    lensName :: Prelude.Maybe Prelude.Text,
    updatedAt :: Prelude.Maybe Core.POSIX,
    -- | The status of the lens.
    lensStatus :: Prelude.Maybe LensStatus,
    -- | The version of the lens.
    lensVersion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LensReviewSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lensAlias', 'lensReviewSummary_lensAlias' - Undocumented member.
--
-- 'riskCounts', 'lensReviewSummary_riskCounts' - Undocumented member.
--
-- 'lensName', 'lensReviewSummary_lensName' - Undocumented member.
--
-- 'updatedAt', 'lensReviewSummary_updatedAt' - Undocumented member.
--
-- 'lensStatus', 'lensReviewSummary_lensStatus' - The status of the lens.
--
-- 'lensVersion', 'lensReviewSummary_lensVersion' - The version of the lens.
newLensReviewSummary ::
  LensReviewSummary
newLensReviewSummary =
  LensReviewSummary'
    { lensAlias = Prelude.Nothing,
      riskCounts = Prelude.Nothing,
      lensName = Prelude.Nothing,
      updatedAt = Prelude.Nothing,
      lensStatus = Prelude.Nothing,
      lensVersion = Prelude.Nothing
    }

-- | Undocumented member.
lensReviewSummary_lensAlias :: Lens.Lens' LensReviewSummary (Prelude.Maybe Prelude.Text)
lensReviewSummary_lensAlias = Lens.lens (\LensReviewSummary' {lensAlias} -> lensAlias) (\s@LensReviewSummary' {} a -> s {lensAlias = a} :: LensReviewSummary)

-- | Undocumented member.
lensReviewSummary_riskCounts :: Lens.Lens' LensReviewSummary (Prelude.Maybe (Prelude.HashMap Risk Prelude.Natural))
lensReviewSummary_riskCounts = Lens.lens (\LensReviewSummary' {riskCounts} -> riskCounts) (\s@LensReviewSummary' {} a -> s {riskCounts = a} :: LensReviewSummary) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
lensReviewSummary_lensName :: Lens.Lens' LensReviewSummary (Prelude.Maybe Prelude.Text)
lensReviewSummary_lensName = Lens.lens (\LensReviewSummary' {lensName} -> lensName) (\s@LensReviewSummary' {} a -> s {lensName = a} :: LensReviewSummary)

-- | Undocumented member.
lensReviewSummary_updatedAt :: Lens.Lens' LensReviewSummary (Prelude.Maybe Prelude.UTCTime)
lensReviewSummary_updatedAt = Lens.lens (\LensReviewSummary' {updatedAt} -> updatedAt) (\s@LensReviewSummary' {} a -> s {updatedAt = a} :: LensReviewSummary) Prelude.. Lens.mapping Core._Time

-- | The status of the lens.
lensReviewSummary_lensStatus :: Lens.Lens' LensReviewSummary (Prelude.Maybe LensStatus)
lensReviewSummary_lensStatus = Lens.lens (\LensReviewSummary' {lensStatus} -> lensStatus) (\s@LensReviewSummary' {} a -> s {lensStatus = a} :: LensReviewSummary)

-- | The version of the lens.
lensReviewSummary_lensVersion :: Lens.Lens' LensReviewSummary (Prelude.Maybe Prelude.Text)
lensReviewSummary_lensVersion = Lens.lens (\LensReviewSummary' {lensVersion} -> lensVersion) (\s@LensReviewSummary' {} a -> s {lensVersion = a} :: LensReviewSummary)

instance Core.FromJSON LensReviewSummary where
  parseJSON =
    Core.withObject
      "LensReviewSummary"
      ( \x ->
          LensReviewSummary'
            Prelude.<$> (x Core..:? "LensAlias")
            Prelude.<*> (x Core..:? "RiskCounts" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "LensName")
            Prelude.<*> (x Core..:? "UpdatedAt")
            Prelude.<*> (x Core..:? "LensStatus")
            Prelude.<*> (x Core..:? "LensVersion")
      )

instance Prelude.Hashable LensReviewSummary where
  hashWithSalt _salt LensReviewSummary' {..} =
    _salt `Prelude.hashWithSalt` lensAlias
      `Prelude.hashWithSalt` riskCounts
      `Prelude.hashWithSalt` lensName
      `Prelude.hashWithSalt` updatedAt
      `Prelude.hashWithSalt` lensStatus
      `Prelude.hashWithSalt` lensVersion

instance Prelude.NFData LensReviewSummary where
  rnf LensReviewSummary' {..} =
    Prelude.rnf lensAlias
      `Prelude.seq` Prelude.rnf riskCounts
      `Prelude.seq` Prelude.rnf lensName
      `Prelude.seq` Prelude.rnf updatedAt
      `Prelude.seq` Prelude.rnf lensStatus
      `Prelude.seq` Prelude.rnf lensVersion
