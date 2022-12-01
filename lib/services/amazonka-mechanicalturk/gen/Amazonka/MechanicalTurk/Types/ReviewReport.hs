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
-- Module      : Amazonka.MechanicalTurk.Types.ReviewReport
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MechanicalTurk.Types.ReviewReport where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MechanicalTurk.Types.ReviewActionDetail
import Amazonka.MechanicalTurk.Types.ReviewResultDetail
import qualified Amazonka.Prelude as Prelude

-- | Contains both ReviewResult and ReviewAction elements for a particular
-- HIT.
--
-- /See:/ 'newReviewReport' smart constructor.
data ReviewReport = ReviewReport'
  { -- | A list of ReviewResults objects for each action specified in the Review
    -- Policy.
    reviewResults :: Prelude.Maybe [ReviewResultDetail],
    -- | A list of ReviewAction objects for each action specified in the Review
    -- Policy.
    reviewActions :: Prelude.Maybe [ReviewActionDetail]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReviewReport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reviewResults', 'reviewReport_reviewResults' - A list of ReviewResults objects for each action specified in the Review
-- Policy.
--
-- 'reviewActions', 'reviewReport_reviewActions' - A list of ReviewAction objects for each action specified in the Review
-- Policy.
newReviewReport ::
  ReviewReport
newReviewReport =
  ReviewReport'
    { reviewResults = Prelude.Nothing,
      reviewActions = Prelude.Nothing
    }

-- | A list of ReviewResults objects for each action specified in the Review
-- Policy.
reviewReport_reviewResults :: Lens.Lens' ReviewReport (Prelude.Maybe [ReviewResultDetail])
reviewReport_reviewResults = Lens.lens (\ReviewReport' {reviewResults} -> reviewResults) (\s@ReviewReport' {} a -> s {reviewResults = a} :: ReviewReport) Prelude.. Lens.mapping Lens.coerced

-- | A list of ReviewAction objects for each action specified in the Review
-- Policy.
reviewReport_reviewActions :: Lens.Lens' ReviewReport (Prelude.Maybe [ReviewActionDetail])
reviewReport_reviewActions = Lens.lens (\ReviewReport' {reviewActions} -> reviewActions) (\s@ReviewReport' {} a -> s {reviewActions = a} :: ReviewReport) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON ReviewReport where
  parseJSON =
    Core.withObject
      "ReviewReport"
      ( \x ->
          ReviewReport'
            Prelude.<$> (x Core..:? "ReviewResults" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "ReviewActions" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable ReviewReport where
  hashWithSalt _salt ReviewReport' {..} =
    _salt `Prelude.hashWithSalt` reviewResults
      `Prelude.hashWithSalt` reviewActions

instance Prelude.NFData ReviewReport where
  rnf ReviewReport' {..} =
    Prelude.rnf reviewResults
      `Prelude.seq` Prelude.rnf reviewActions
