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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types.LensReviewSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WellArchitected.Types.LensStatus
import Amazonka.WellArchitected.Types.Risk

-- | A lens review summary of a workload.
--
-- /See:/ 'newLensReviewSummary' smart constructor.
data LensReviewSummary = LensReviewSummary'
  { -- | The ARN for the lens.
    lensArn :: Prelude.Maybe Prelude.Text,
    riskCounts :: Prelude.Maybe (Prelude.HashMap Risk Prelude.Natural),
    lensAlias :: Prelude.Maybe Prelude.Text,
    -- | The version of the lens.
    lensVersion :: Prelude.Maybe Prelude.Text,
    lensName :: Prelude.Maybe Prelude.Text,
    -- | The status of the lens.
    lensStatus :: Prelude.Maybe LensStatus,
    updatedAt :: Prelude.Maybe Data.POSIX
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
-- 'lensArn', 'lensReviewSummary_lensArn' - The ARN for the lens.
--
-- 'riskCounts', 'lensReviewSummary_riskCounts' - Undocumented member.
--
-- 'lensAlias', 'lensReviewSummary_lensAlias' - Undocumented member.
--
-- 'lensVersion', 'lensReviewSummary_lensVersion' - The version of the lens.
--
-- 'lensName', 'lensReviewSummary_lensName' - Undocumented member.
--
-- 'lensStatus', 'lensReviewSummary_lensStatus' - The status of the lens.
--
-- 'updatedAt', 'lensReviewSummary_updatedAt' - Undocumented member.
newLensReviewSummary ::
  LensReviewSummary
newLensReviewSummary =
  LensReviewSummary'
    { lensArn = Prelude.Nothing,
      riskCounts = Prelude.Nothing,
      lensAlias = Prelude.Nothing,
      lensVersion = Prelude.Nothing,
      lensName = Prelude.Nothing,
      lensStatus = Prelude.Nothing,
      updatedAt = Prelude.Nothing
    }

-- | The ARN for the lens.
lensReviewSummary_lensArn :: Lens.Lens' LensReviewSummary (Prelude.Maybe Prelude.Text)
lensReviewSummary_lensArn = Lens.lens (\LensReviewSummary' {lensArn} -> lensArn) (\s@LensReviewSummary' {} a -> s {lensArn = a} :: LensReviewSummary)

-- | Undocumented member.
lensReviewSummary_riskCounts :: Lens.Lens' LensReviewSummary (Prelude.Maybe (Prelude.HashMap Risk Prelude.Natural))
lensReviewSummary_riskCounts = Lens.lens (\LensReviewSummary' {riskCounts} -> riskCounts) (\s@LensReviewSummary' {} a -> s {riskCounts = a} :: LensReviewSummary) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
lensReviewSummary_lensAlias :: Lens.Lens' LensReviewSummary (Prelude.Maybe Prelude.Text)
lensReviewSummary_lensAlias = Lens.lens (\LensReviewSummary' {lensAlias} -> lensAlias) (\s@LensReviewSummary' {} a -> s {lensAlias = a} :: LensReviewSummary)

-- | The version of the lens.
lensReviewSummary_lensVersion :: Lens.Lens' LensReviewSummary (Prelude.Maybe Prelude.Text)
lensReviewSummary_lensVersion = Lens.lens (\LensReviewSummary' {lensVersion} -> lensVersion) (\s@LensReviewSummary' {} a -> s {lensVersion = a} :: LensReviewSummary)

-- | Undocumented member.
lensReviewSummary_lensName :: Lens.Lens' LensReviewSummary (Prelude.Maybe Prelude.Text)
lensReviewSummary_lensName = Lens.lens (\LensReviewSummary' {lensName} -> lensName) (\s@LensReviewSummary' {} a -> s {lensName = a} :: LensReviewSummary)

-- | The status of the lens.
lensReviewSummary_lensStatus :: Lens.Lens' LensReviewSummary (Prelude.Maybe LensStatus)
lensReviewSummary_lensStatus = Lens.lens (\LensReviewSummary' {lensStatus} -> lensStatus) (\s@LensReviewSummary' {} a -> s {lensStatus = a} :: LensReviewSummary)

-- | Undocumented member.
lensReviewSummary_updatedAt :: Lens.Lens' LensReviewSummary (Prelude.Maybe Prelude.UTCTime)
lensReviewSummary_updatedAt = Lens.lens (\LensReviewSummary' {updatedAt} -> updatedAt) (\s@LensReviewSummary' {} a -> s {updatedAt = a} :: LensReviewSummary) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON LensReviewSummary where
  parseJSON =
    Data.withObject
      "LensReviewSummary"
      ( \x ->
          LensReviewSummary'
            Prelude.<$> (x Data..:? "LensArn")
            Prelude.<*> (x Data..:? "RiskCounts" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "LensAlias")
            Prelude.<*> (x Data..:? "LensVersion")
            Prelude.<*> (x Data..:? "LensName")
            Prelude.<*> (x Data..:? "LensStatus")
            Prelude.<*> (x Data..:? "UpdatedAt")
      )

instance Prelude.Hashable LensReviewSummary where
  hashWithSalt _salt LensReviewSummary' {..} =
    _salt `Prelude.hashWithSalt` lensArn
      `Prelude.hashWithSalt` riskCounts
      `Prelude.hashWithSalt` lensAlias
      `Prelude.hashWithSalt` lensVersion
      `Prelude.hashWithSalt` lensName
      `Prelude.hashWithSalt` lensStatus
      `Prelude.hashWithSalt` updatedAt

instance Prelude.NFData LensReviewSummary where
  rnf LensReviewSummary' {..} =
    Prelude.rnf lensArn
      `Prelude.seq` Prelude.rnf riskCounts
      `Prelude.seq` Prelude.rnf lensAlias
      `Prelude.seq` Prelude.rnf lensVersion
      `Prelude.seq` Prelude.rnf lensName
      `Prelude.seq` Prelude.rnf lensStatus
      `Prelude.seq` Prelude.rnf updatedAt
