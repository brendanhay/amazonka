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
-- Module      : Amazonka.WellArchitected.Types.LensReview
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types.LensReview where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.WellArchitected.Types.LensStatus
import Amazonka.WellArchitected.Types.PillarReviewSummary
import Amazonka.WellArchitected.Types.Risk

-- | A lens review of a question.
--
-- /See:/ 'newLensReview' smart constructor.
data LensReview = LensReview'
  { nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ARN for the lens.
    lensArn :: Prelude.Maybe Prelude.Text,
    riskCounts :: Prelude.Maybe (Prelude.HashMap Risk Prelude.Natural),
    lensAlias :: Prelude.Maybe Prelude.Text,
    -- | The version of the lens.
    lensVersion :: Prelude.Maybe Prelude.Text,
    lensName :: Prelude.Maybe Prelude.Text,
    pillarReviewSummaries :: Prelude.Maybe [PillarReviewSummary],
    notes :: Prelude.Maybe Prelude.Text,
    -- | The status of the lens.
    lensStatus :: Prelude.Maybe LensStatus,
    updatedAt :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LensReview' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'lensReview_nextToken' - Undocumented member.
--
-- 'lensArn', 'lensReview_lensArn' - The ARN for the lens.
--
-- 'riskCounts', 'lensReview_riskCounts' - Undocumented member.
--
-- 'lensAlias', 'lensReview_lensAlias' - Undocumented member.
--
-- 'lensVersion', 'lensReview_lensVersion' - The version of the lens.
--
-- 'lensName', 'lensReview_lensName' - Undocumented member.
--
-- 'pillarReviewSummaries', 'lensReview_pillarReviewSummaries' - Undocumented member.
--
-- 'notes', 'lensReview_notes' - Undocumented member.
--
-- 'lensStatus', 'lensReview_lensStatus' - The status of the lens.
--
-- 'updatedAt', 'lensReview_updatedAt' - Undocumented member.
newLensReview ::
  LensReview
newLensReview =
  LensReview'
    { nextToken = Prelude.Nothing,
      lensArn = Prelude.Nothing,
      riskCounts = Prelude.Nothing,
      lensAlias = Prelude.Nothing,
      lensVersion = Prelude.Nothing,
      lensName = Prelude.Nothing,
      pillarReviewSummaries = Prelude.Nothing,
      notes = Prelude.Nothing,
      lensStatus = Prelude.Nothing,
      updatedAt = Prelude.Nothing
    }

-- | Undocumented member.
lensReview_nextToken :: Lens.Lens' LensReview (Prelude.Maybe Prelude.Text)
lensReview_nextToken = Lens.lens (\LensReview' {nextToken} -> nextToken) (\s@LensReview' {} a -> s {nextToken = a} :: LensReview)

-- | The ARN for the lens.
lensReview_lensArn :: Lens.Lens' LensReview (Prelude.Maybe Prelude.Text)
lensReview_lensArn = Lens.lens (\LensReview' {lensArn} -> lensArn) (\s@LensReview' {} a -> s {lensArn = a} :: LensReview)

-- | Undocumented member.
lensReview_riskCounts :: Lens.Lens' LensReview (Prelude.Maybe (Prelude.HashMap Risk Prelude.Natural))
lensReview_riskCounts = Lens.lens (\LensReview' {riskCounts} -> riskCounts) (\s@LensReview' {} a -> s {riskCounts = a} :: LensReview) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
lensReview_lensAlias :: Lens.Lens' LensReview (Prelude.Maybe Prelude.Text)
lensReview_lensAlias = Lens.lens (\LensReview' {lensAlias} -> lensAlias) (\s@LensReview' {} a -> s {lensAlias = a} :: LensReview)

-- | The version of the lens.
lensReview_lensVersion :: Lens.Lens' LensReview (Prelude.Maybe Prelude.Text)
lensReview_lensVersion = Lens.lens (\LensReview' {lensVersion} -> lensVersion) (\s@LensReview' {} a -> s {lensVersion = a} :: LensReview)

-- | Undocumented member.
lensReview_lensName :: Lens.Lens' LensReview (Prelude.Maybe Prelude.Text)
lensReview_lensName = Lens.lens (\LensReview' {lensName} -> lensName) (\s@LensReview' {} a -> s {lensName = a} :: LensReview)

-- | Undocumented member.
lensReview_pillarReviewSummaries :: Lens.Lens' LensReview (Prelude.Maybe [PillarReviewSummary])
lensReview_pillarReviewSummaries = Lens.lens (\LensReview' {pillarReviewSummaries} -> pillarReviewSummaries) (\s@LensReview' {} a -> s {pillarReviewSummaries = a} :: LensReview) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
lensReview_notes :: Lens.Lens' LensReview (Prelude.Maybe Prelude.Text)
lensReview_notes = Lens.lens (\LensReview' {notes} -> notes) (\s@LensReview' {} a -> s {notes = a} :: LensReview)

-- | The status of the lens.
lensReview_lensStatus :: Lens.Lens' LensReview (Prelude.Maybe LensStatus)
lensReview_lensStatus = Lens.lens (\LensReview' {lensStatus} -> lensStatus) (\s@LensReview' {} a -> s {lensStatus = a} :: LensReview)

-- | Undocumented member.
lensReview_updatedAt :: Lens.Lens' LensReview (Prelude.Maybe Prelude.UTCTime)
lensReview_updatedAt = Lens.lens (\LensReview' {updatedAt} -> updatedAt) (\s@LensReview' {} a -> s {updatedAt = a} :: LensReview) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON LensReview where
  parseJSON =
    Core.withObject
      "LensReview"
      ( \x ->
          LensReview'
            Prelude.<$> (x Core..:? "NextToken")
            Prelude.<*> (x Core..:? "LensArn")
            Prelude.<*> (x Core..:? "RiskCounts" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "LensAlias")
            Prelude.<*> (x Core..:? "LensVersion")
            Prelude.<*> (x Core..:? "LensName")
            Prelude.<*> ( x Core..:? "PillarReviewSummaries"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "Notes")
            Prelude.<*> (x Core..:? "LensStatus")
            Prelude.<*> (x Core..:? "UpdatedAt")
      )

instance Prelude.Hashable LensReview where
  hashWithSalt _salt LensReview' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` lensArn
      `Prelude.hashWithSalt` riskCounts
      `Prelude.hashWithSalt` lensAlias
      `Prelude.hashWithSalt` lensVersion
      `Prelude.hashWithSalt` lensName
      `Prelude.hashWithSalt` pillarReviewSummaries
      `Prelude.hashWithSalt` notes
      `Prelude.hashWithSalt` lensStatus
      `Prelude.hashWithSalt` updatedAt

instance Prelude.NFData LensReview where
  rnf LensReview' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf lensArn
      `Prelude.seq` Prelude.rnf riskCounts
      `Prelude.seq` Prelude.rnf lensAlias
      `Prelude.seq` Prelude.rnf lensVersion
      `Prelude.seq` Prelude.rnf lensName
      `Prelude.seq` Prelude.rnf pillarReviewSummaries
      `Prelude.seq` Prelude.rnf notes
      `Prelude.seq` Prelude.rnf lensStatus
      `Prelude.seq` Prelude.rnf updatedAt
