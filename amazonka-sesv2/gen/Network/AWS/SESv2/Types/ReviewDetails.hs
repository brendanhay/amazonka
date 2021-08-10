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
-- Module      : Network.AWS.SESv2.Types.ReviewDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SESv2.Types.ReviewDetails where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SESv2.Types.ReviewStatus

-- | An object that contains information about your account details review.
--
-- /See:/ 'newReviewDetails' smart constructor.
data ReviewDetails = ReviewDetails'
  { -- | The status of the latest review of your account. The status can be one
    -- of the following:
    --
    -- -   @PENDING@ – We have received your appeal and are in the process of
    --     reviewing it.
    --
    -- -   @GRANTED@ – Your appeal has been reviewed and your production access
    --     has been granted.
    --
    -- -   @DENIED@ – Your appeal has been reviewed and your production access
    --     has been denied.
    --
    -- -   @FAILED@ – An internal error occurred and we didn\'t receive your
    --     appeal. You can submit your appeal again.
    status :: Prelude.Maybe ReviewStatus,
    -- | The associated support center case ID (if any).
    caseId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReviewDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'reviewDetails_status' - The status of the latest review of your account. The status can be one
-- of the following:
--
-- -   @PENDING@ – We have received your appeal and are in the process of
--     reviewing it.
--
-- -   @GRANTED@ – Your appeal has been reviewed and your production access
--     has been granted.
--
-- -   @DENIED@ – Your appeal has been reviewed and your production access
--     has been denied.
--
-- -   @FAILED@ – An internal error occurred and we didn\'t receive your
--     appeal. You can submit your appeal again.
--
-- 'caseId', 'reviewDetails_caseId' - The associated support center case ID (if any).
newReviewDetails ::
  ReviewDetails
newReviewDetails =
  ReviewDetails'
    { status = Prelude.Nothing,
      caseId = Prelude.Nothing
    }

-- | The status of the latest review of your account. The status can be one
-- of the following:
--
-- -   @PENDING@ – We have received your appeal and are in the process of
--     reviewing it.
--
-- -   @GRANTED@ – Your appeal has been reviewed and your production access
--     has been granted.
--
-- -   @DENIED@ – Your appeal has been reviewed and your production access
--     has been denied.
--
-- -   @FAILED@ – An internal error occurred and we didn\'t receive your
--     appeal. You can submit your appeal again.
reviewDetails_status :: Lens.Lens' ReviewDetails (Prelude.Maybe ReviewStatus)
reviewDetails_status = Lens.lens (\ReviewDetails' {status} -> status) (\s@ReviewDetails' {} a -> s {status = a} :: ReviewDetails)

-- | The associated support center case ID (if any).
reviewDetails_caseId :: Lens.Lens' ReviewDetails (Prelude.Maybe Prelude.Text)
reviewDetails_caseId = Lens.lens (\ReviewDetails' {caseId} -> caseId) (\s@ReviewDetails' {} a -> s {caseId = a} :: ReviewDetails)

instance Core.FromJSON ReviewDetails where
  parseJSON =
    Core.withObject
      "ReviewDetails"
      ( \x ->
          ReviewDetails'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "CaseId")
      )

instance Prelude.Hashable ReviewDetails

instance Prelude.NFData ReviewDetails
