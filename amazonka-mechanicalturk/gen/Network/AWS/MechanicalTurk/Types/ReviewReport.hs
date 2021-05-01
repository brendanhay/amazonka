{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.MechanicalTurk.Types.ReviewReport
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.ReviewReport where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types.ReviewActionDetail
import Network.AWS.MechanicalTurk.Types.ReviewResultDetail
import qualified Network.AWS.Prelude as Prelude

-- | Contains both ReviewResult and ReviewAction elements for a particular
-- HIT.
--
-- /See:/ 'newReviewReport' smart constructor.
data ReviewReport = ReviewReport'
  { -- | A list of ReviewAction objects for each action specified in the Review
    -- Policy.
    reviewActions :: Prelude.Maybe [ReviewActionDetail],
    -- | A list of ReviewResults objects for each action specified in the Review
    -- Policy.
    reviewResults :: Prelude.Maybe [ReviewResultDetail]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ReviewReport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reviewActions', 'reviewReport_reviewActions' - A list of ReviewAction objects for each action specified in the Review
-- Policy.
--
-- 'reviewResults', 'reviewReport_reviewResults' - A list of ReviewResults objects for each action specified in the Review
-- Policy.
newReviewReport ::
  ReviewReport
newReviewReport =
  ReviewReport'
    { reviewActions = Prelude.Nothing,
      reviewResults = Prelude.Nothing
    }

-- | A list of ReviewAction objects for each action specified in the Review
-- Policy.
reviewReport_reviewActions :: Lens.Lens' ReviewReport (Prelude.Maybe [ReviewActionDetail])
reviewReport_reviewActions = Lens.lens (\ReviewReport' {reviewActions} -> reviewActions) (\s@ReviewReport' {} a -> s {reviewActions = a} :: ReviewReport) Prelude.. Lens.mapping Prelude._Coerce

-- | A list of ReviewResults objects for each action specified in the Review
-- Policy.
reviewReport_reviewResults :: Lens.Lens' ReviewReport (Prelude.Maybe [ReviewResultDetail])
reviewReport_reviewResults = Lens.lens (\ReviewReport' {reviewResults} -> reviewResults) (\s@ReviewReport' {} a -> s {reviewResults = a} :: ReviewReport) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON ReviewReport where
  parseJSON =
    Prelude.withObject
      "ReviewReport"
      ( \x ->
          ReviewReport'
            Prelude.<$> ( x Prelude..:? "ReviewActions"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> ( x Prelude..:? "ReviewResults"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable ReviewReport

instance Prelude.NFData ReviewReport
