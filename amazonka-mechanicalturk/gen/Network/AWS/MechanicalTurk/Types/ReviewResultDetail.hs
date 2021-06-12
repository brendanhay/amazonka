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
-- Module      : Network.AWS.MechanicalTurk.Types.ReviewResultDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.ReviewResultDetail where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | This data structure is returned multiple times for each result specified
-- in the Review Policy.
--
-- /See:/ 'newReviewResultDetail' smart constructor.
data ReviewResultDetail = ReviewResultDetail'
  { -- | Key identifies the particular piece of reviewed information.
    key :: Core.Maybe Core.Text,
    -- | The type of the object from the SubjectId field.
    subjectType :: Core.Maybe Core.Text,
    -- | The HITID or AssignmentId about which this result was taken. Note that
    -- HIT-level Review Policies will often emit results about both the HIT
    -- itself and its Assignments, while Assignment-level review policies
    -- generally only emit results about the Assignment itself.
    subjectId :: Core.Maybe Core.Text,
    -- | A unique identifier of the Review action result.
    actionId :: Core.Maybe Core.Text,
    -- | The values of Key provided by the review policies you have selected.
    value :: Core.Maybe Core.Text,
    -- | Specifies the QuestionId the result is describing. Depending on whether
    -- the TargetType is a HIT or Assignment this results could specify
    -- multiple values. If TargetType is HIT and QuestionId is absent, then the
    -- result describes results of the HIT, including the HIT agreement score.
    -- If ObjectType is Assignment and QuestionId is absent, then the result
    -- describes the Worker\'s performance on the HIT.
    questionId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ReviewResultDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'reviewResultDetail_key' - Key identifies the particular piece of reviewed information.
--
-- 'subjectType', 'reviewResultDetail_subjectType' - The type of the object from the SubjectId field.
--
-- 'subjectId', 'reviewResultDetail_subjectId' - The HITID or AssignmentId about which this result was taken. Note that
-- HIT-level Review Policies will often emit results about both the HIT
-- itself and its Assignments, while Assignment-level review policies
-- generally only emit results about the Assignment itself.
--
-- 'actionId', 'reviewResultDetail_actionId' - A unique identifier of the Review action result.
--
-- 'value', 'reviewResultDetail_value' - The values of Key provided by the review policies you have selected.
--
-- 'questionId', 'reviewResultDetail_questionId' - Specifies the QuestionId the result is describing. Depending on whether
-- the TargetType is a HIT or Assignment this results could specify
-- multiple values. If TargetType is HIT and QuestionId is absent, then the
-- result describes results of the HIT, including the HIT agreement score.
-- If ObjectType is Assignment and QuestionId is absent, then the result
-- describes the Worker\'s performance on the HIT.
newReviewResultDetail ::
  ReviewResultDetail
newReviewResultDetail =
  ReviewResultDetail'
    { key = Core.Nothing,
      subjectType = Core.Nothing,
      subjectId = Core.Nothing,
      actionId = Core.Nothing,
      value = Core.Nothing,
      questionId = Core.Nothing
    }

-- | Key identifies the particular piece of reviewed information.
reviewResultDetail_key :: Lens.Lens' ReviewResultDetail (Core.Maybe Core.Text)
reviewResultDetail_key = Lens.lens (\ReviewResultDetail' {key} -> key) (\s@ReviewResultDetail' {} a -> s {key = a} :: ReviewResultDetail)

-- | The type of the object from the SubjectId field.
reviewResultDetail_subjectType :: Lens.Lens' ReviewResultDetail (Core.Maybe Core.Text)
reviewResultDetail_subjectType = Lens.lens (\ReviewResultDetail' {subjectType} -> subjectType) (\s@ReviewResultDetail' {} a -> s {subjectType = a} :: ReviewResultDetail)

-- | The HITID or AssignmentId about which this result was taken. Note that
-- HIT-level Review Policies will often emit results about both the HIT
-- itself and its Assignments, while Assignment-level review policies
-- generally only emit results about the Assignment itself.
reviewResultDetail_subjectId :: Lens.Lens' ReviewResultDetail (Core.Maybe Core.Text)
reviewResultDetail_subjectId = Lens.lens (\ReviewResultDetail' {subjectId} -> subjectId) (\s@ReviewResultDetail' {} a -> s {subjectId = a} :: ReviewResultDetail)

-- | A unique identifier of the Review action result.
reviewResultDetail_actionId :: Lens.Lens' ReviewResultDetail (Core.Maybe Core.Text)
reviewResultDetail_actionId = Lens.lens (\ReviewResultDetail' {actionId} -> actionId) (\s@ReviewResultDetail' {} a -> s {actionId = a} :: ReviewResultDetail)

-- | The values of Key provided by the review policies you have selected.
reviewResultDetail_value :: Lens.Lens' ReviewResultDetail (Core.Maybe Core.Text)
reviewResultDetail_value = Lens.lens (\ReviewResultDetail' {value} -> value) (\s@ReviewResultDetail' {} a -> s {value = a} :: ReviewResultDetail)

-- | Specifies the QuestionId the result is describing. Depending on whether
-- the TargetType is a HIT or Assignment this results could specify
-- multiple values. If TargetType is HIT and QuestionId is absent, then the
-- result describes results of the HIT, including the HIT agreement score.
-- If ObjectType is Assignment and QuestionId is absent, then the result
-- describes the Worker\'s performance on the HIT.
reviewResultDetail_questionId :: Lens.Lens' ReviewResultDetail (Core.Maybe Core.Text)
reviewResultDetail_questionId = Lens.lens (\ReviewResultDetail' {questionId} -> questionId) (\s@ReviewResultDetail' {} a -> s {questionId = a} :: ReviewResultDetail)

instance Core.FromJSON ReviewResultDetail where
  parseJSON =
    Core.withObject
      "ReviewResultDetail"
      ( \x ->
          ReviewResultDetail'
            Core.<$> (x Core..:? "Key")
            Core.<*> (x Core..:? "SubjectType")
            Core.<*> (x Core..:? "SubjectId")
            Core.<*> (x Core..:? "ActionId")
            Core.<*> (x Core..:? "Value")
            Core.<*> (x Core..:? "QuestionId")
      )

instance Core.Hashable ReviewResultDetail

instance Core.NFData ReviewResultDetail
