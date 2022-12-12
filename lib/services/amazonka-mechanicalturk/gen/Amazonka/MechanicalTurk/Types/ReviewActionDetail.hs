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
-- Module      : Amazonka.MechanicalTurk.Types.ReviewActionDetail
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MechanicalTurk.Types.ReviewActionDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MechanicalTurk.Types.ReviewActionStatus
import qualified Amazonka.Prelude as Prelude

-- | Both the AssignmentReviewReport and the HITReviewReport elements
-- contains the ReviewActionDetail data structure. This structure is
-- returned multiple times for each action specified in the Review Policy.
--
-- /See:/ 'newReviewActionDetail' smart constructor.
data ReviewActionDetail = ReviewActionDetail'
  { -- | The unique identifier for the action.
    actionId :: Prelude.Maybe Prelude.Text,
    -- | The nature of the action itself. The Review Policy is responsible for
    -- examining the HIT and Assignments, emitting results, and deciding which
    -- other actions will be necessary.
    actionName :: Prelude.Maybe Prelude.Text,
    -- | The date when the action was completed.
    completeTime :: Prelude.Maybe Data.POSIX,
    -- | Present only when the Results have a FAILED Status.
    errorCode :: Prelude.Maybe Prelude.Text,
    -- | A description of the outcome of the review.
    result :: Prelude.Maybe Prelude.Text,
    -- | The current disposition of the action: INTENDED, SUCCEEDED, FAILED, or
    -- CANCELLED.
    status :: Prelude.Maybe ReviewActionStatus,
    -- | The specific HITId or AssignmentID targeted by the action.
    targetId :: Prelude.Maybe Prelude.Text,
    -- | The type of object in TargetId.
    targetType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReviewActionDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actionId', 'reviewActionDetail_actionId' - The unique identifier for the action.
--
-- 'actionName', 'reviewActionDetail_actionName' - The nature of the action itself. The Review Policy is responsible for
-- examining the HIT and Assignments, emitting results, and deciding which
-- other actions will be necessary.
--
-- 'completeTime', 'reviewActionDetail_completeTime' - The date when the action was completed.
--
-- 'errorCode', 'reviewActionDetail_errorCode' - Present only when the Results have a FAILED Status.
--
-- 'result', 'reviewActionDetail_result' - A description of the outcome of the review.
--
-- 'status', 'reviewActionDetail_status' - The current disposition of the action: INTENDED, SUCCEEDED, FAILED, or
-- CANCELLED.
--
-- 'targetId', 'reviewActionDetail_targetId' - The specific HITId or AssignmentID targeted by the action.
--
-- 'targetType', 'reviewActionDetail_targetType' - The type of object in TargetId.
newReviewActionDetail ::
  ReviewActionDetail
newReviewActionDetail =
  ReviewActionDetail'
    { actionId = Prelude.Nothing,
      actionName = Prelude.Nothing,
      completeTime = Prelude.Nothing,
      errorCode = Prelude.Nothing,
      result = Prelude.Nothing,
      status = Prelude.Nothing,
      targetId = Prelude.Nothing,
      targetType = Prelude.Nothing
    }

-- | The unique identifier for the action.
reviewActionDetail_actionId :: Lens.Lens' ReviewActionDetail (Prelude.Maybe Prelude.Text)
reviewActionDetail_actionId = Lens.lens (\ReviewActionDetail' {actionId} -> actionId) (\s@ReviewActionDetail' {} a -> s {actionId = a} :: ReviewActionDetail)

-- | The nature of the action itself. The Review Policy is responsible for
-- examining the HIT and Assignments, emitting results, and deciding which
-- other actions will be necessary.
reviewActionDetail_actionName :: Lens.Lens' ReviewActionDetail (Prelude.Maybe Prelude.Text)
reviewActionDetail_actionName = Lens.lens (\ReviewActionDetail' {actionName} -> actionName) (\s@ReviewActionDetail' {} a -> s {actionName = a} :: ReviewActionDetail)

-- | The date when the action was completed.
reviewActionDetail_completeTime :: Lens.Lens' ReviewActionDetail (Prelude.Maybe Prelude.UTCTime)
reviewActionDetail_completeTime = Lens.lens (\ReviewActionDetail' {completeTime} -> completeTime) (\s@ReviewActionDetail' {} a -> s {completeTime = a} :: ReviewActionDetail) Prelude.. Lens.mapping Data._Time

-- | Present only when the Results have a FAILED Status.
reviewActionDetail_errorCode :: Lens.Lens' ReviewActionDetail (Prelude.Maybe Prelude.Text)
reviewActionDetail_errorCode = Lens.lens (\ReviewActionDetail' {errorCode} -> errorCode) (\s@ReviewActionDetail' {} a -> s {errorCode = a} :: ReviewActionDetail)

-- | A description of the outcome of the review.
reviewActionDetail_result :: Lens.Lens' ReviewActionDetail (Prelude.Maybe Prelude.Text)
reviewActionDetail_result = Lens.lens (\ReviewActionDetail' {result} -> result) (\s@ReviewActionDetail' {} a -> s {result = a} :: ReviewActionDetail)

-- | The current disposition of the action: INTENDED, SUCCEEDED, FAILED, or
-- CANCELLED.
reviewActionDetail_status :: Lens.Lens' ReviewActionDetail (Prelude.Maybe ReviewActionStatus)
reviewActionDetail_status = Lens.lens (\ReviewActionDetail' {status} -> status) (\s@ReviewActionDetail' {} a -> s {status = a} :: ReviewActionDetail)

-- | The specific HITId or AssignmentID targeted by the action.
reviewActionDetail_targetId :: Lens.Lens' ReviewActionDetail (Prelude.Maybe Prelude.Text)
reviewActionDetail_targetId = Lens.lens (\ReviewActionDetail' {targetId} -> targetId) (\s@ReviewActionDetail' {} a -> s {targetId = a} :: ReviewActionDetail)

-- | The type of object in TargetId.
reviewActionDetail_targetType :: Lens.Lens' ReviewActionDetail (Prelude.Maybe Prelude.Text)
reviewActionDetail_targetType = Lens.lens (\ReviewActionDetail' {targetType} -> targetType) (\s@ReviewActionDetail' {} a -> s {targetType = a} :: ReviewActionDetail)

instance Data.FromJSON ReviewActionDetail where
  parseJSON =
    Data.withObject
      "ReviewActionDetail"
      ( \x ->
          ReviewActionDetail'
            Prelude.<$> (x Data..:? "ActionId")
            Prelude.<*> (x Data..:? "ActionName")
            Prelude.<*> (x Data..:? "CompleteTime")
            Prelude.<*> (x Data..:? "ErrorCode")
            Prelude.<*> (x Data..:? "Result")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "TargetId")
            Prelude.<*> (x Data..:? "TargetType")
      )

instance Prelude.Hashable ReviewActionDetail where
  hashWithSalt _salt ReviewActionDetail' {..} =
    _salt `Prelude.hashWithSalt` actionId
      `Prelude.hashWithSalt` actionName
      `Prelude.hashWithSalt` completeTime
      `Prelude.hashWithSalt` errorCode
      `Prelude.hashWithSalt` result
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` targetId
      `Prelude.hashWithSalt` targetType

instance Prelude.NFData ReviewActionDetail where
  rnf ReviewActionDetail' {..} =
    Prelude.rnf actionId
      `Prelude.seq` Prelude.rnf actionName
      `Prelude.seq` Prelude.rnf completeTime
      `Prelude.seq` Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf result
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf targetId
      `Prelude.seq` Prelude.rnf targetType
