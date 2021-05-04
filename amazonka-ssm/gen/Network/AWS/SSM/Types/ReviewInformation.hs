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
-- Module      : Network.AWS.SSM.Types.ReviewInformation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ReviewInformation where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SSM.Types.ReviewStatus

-- | Information about the result of a document review request.
--
-- /See:/ 'newReviewInformation' smart constructor.
data ReviewInformation = ReviewInformation'
  { -- | The current status of the document review request.
    status :: Prelude.Maybe ReviewStatus,
    -- | The time that the reviewer took action on the document review request.
    reviewedTime :: Prelude.Maybe Prelude.POSIX,
    -- | The reviewer assigned to take action on the document review request.
    reviewer :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ReviewInformation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'reviewInformation_status' - The current status of the document review request.
--
-- 'reviewedTime', 'reviewInformation_reviewedTime' - The time that the reviewer took action on the document review request.
--
-- 'reviewer', 'reviewInformation_reviewer' - The reviewer assigned to take action on the document review request.
newReviewInformation ::
  ReviewInformation
newReviewInformation =
  ReviewInformation'
    { status = Prelude.Nothing,
      reviewedTime = Prelude.Nothing,
      reviewer = Prelude.Nothing
    }

-- | The current status of the document review request.
reviewInformation_status :: Lens.Lens' ReviewInformation (Prelude.Maybe ReviewStatus)
reviewInformation_status = Lens.lens (\ReviewInformation' {status} -> status) (\s@ReviewInformation' {} a -> s {status = a} :: ReviewInformation)

-- | The time that the reviewer took action on the document review request.
reviewInformation_reviewedTime :: Lens.Lens' ReviewInformation (Prelude.Maybe Prelude.UTCTime)
reviewInformation_reviewedTime = Lens.lens (\ReviewInformation' {reviewedTime} -> reviewedTime) (\s@ReviewInformation' {} a -> s {reviewedTime = a} :: ReviewInformation) Prelude.. Lens.mapping Prelude._Time

-- | The reviewer assigned to take action on the document review request.
reviewInformation_reviewer :: Lens.Lens' ReviewInformation (Prelude.Maybe Prelude.Text)
reviewInformation_reviewer = Lens.lens (\ReviewInformation' {reviewer} -> reviewer) (\s@ReviewInformation' {} a -> s {reviewer = a} :: ReviewInformation)

instance Prelude.FromJSON ReviewInformation where
  parseJSON =
    Prelude.withObject
      "ReviewInformation"
      ( \x ->
          ReviewInformation'
            Prelude.<$> (x Prelude..:? "Status")
            Prelude.<*> (x Prelude..:? "ReviewedTime")
            Prelude.<*> (x Prelude..:? "Reviewer")
      )

instance Prelude.Hashable ReviewInformation

instance Prelude.NFData ReviewInformation
