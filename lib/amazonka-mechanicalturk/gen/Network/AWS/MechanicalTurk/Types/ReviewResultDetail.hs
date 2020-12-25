{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.ReviewResultDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.ReviewResultDetail
  ( ReviewResultDetail (..),

    -- * Smart constructor
    mkReviewResultDetail,

    -- * Lenses
    rrdActionId,
    rrdKey,
    rrdQuestionId,
    rrdSubjectId,
    rrdSubjectType,
    rrdValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MechanicalTurk.Types.ActionId as Types
import qualified Network.AWS.MechanicalTurk.Types.Key as Types
import qualified Network.AWS.MechanicalTurk.Types.QuestionId as Types
import qualified Network.AWS.MechanicalTurk.Types.SubjectId as Types
import qualified Network.AWS.MechanicalTurk.Types.SubjectType as Types
import qualified Network.AWS.MechanicalTurk.Types.Value as Types
import qualified Network.AWS.Prelude as Core

-- | This data structure is returned multiple times for each result specified in the Review Policy.
--
-- /See:/ 'mkReviewResultDetail' smart constructor.
data ReviewResultDetail = ReviewResultDetail'
  { -- | A unique identifier of the Review action result.
    actionId :: Core.Maybe Types.ActionId,
    -- | Key identifies the particular piece of reviewed information.
    key :: Core.Maybe Types.Key,
    -- | Specifies the QuestionId the result is describing. Depending on whether the TargetType is a HIT or Assignment this results could specify multiple values. If TargetType is HIT and QuestionId is absent, then the result describes results of the HIT, including the HIT agreement score. If ObjectType is Assignment and QuestionId is absent, then the result describes the Worker's performance on the HIT.
    questionId :: Core.Maybe Types.QuestionId,
    -- | The HITID or AssignmentId about which this result was taken. Note that HIT-level Review Policies will often emit results about both the HIT itself and its Assignments, while Assignment-level review policies generally only emit results about the Assignment itself.
    subjectId :: Core.Maybe Types.SubjectId,
    -- | The type of the object from the SubjectId field.
    subjectType :: Core.Maybe Types.SubjectType,
    -- | The values of Key provided by the review policies you have selected.
    value :: Core.Maybe Types.Value
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReviewResultDetail' value with any optional fields omitted.
mkReviewResultDetail ::
  ReviewResultDetail
mkReviewResultDetail =
  ReviewResultDetail'
    { actionId = Core.Nothing,
      key = Core.Nothing,
      questionId = Core.Nothing,
      subjectId = Core.Nothing,
      subjectType = Core.Nothing,
      value = Core.Nothing
    }

-- | A unique identifier of the Review action result.
--
-- /Note:/ Consider using 'actionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrdActionId :: Lens.Lens' ReviewResultDetail (Core.Maybe Types.ActionId)
rrdActionId = Lens.field @"actionId"
{-# DEPRECATED rrdActionId "Use generic-lens or generic-optics with 'actionId' instead." #-}

-- | Key identifies the particular piece of reviewed information.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrdKey :: Lens.Lens' ReviewResultDetail (Core.Maybe Types.Key)
rrdKey = Lens.field @"key"
{-# DEPRECATED rrdKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | Specifies the QuestionId the result is describing. Depending on whether the TargetType is a HIT or Assignment this results could specify multiple values. If TargetType is HIT and QuestionId is absent, then the result describes results of the HIT, including the HIT agreement score. If ObjectType is Assignment and QuestionId is absent, then the result describes the Worker's performance on the HIT.
--
-- /Note:/ Consider using 'questionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrdQuestionId :: Lens.Lens' ReviewResultDetail (Core.Maybe Types.QuestionId)
rrdQuestionId = Lens.field @"questionId"
{-# DEPRECATED rrdQuestionId "Use generic-lens or generic-optics with 'questionId' instead." #-}

-- | The HITID or AssignmentId about which this result was taken. Note that HIT-level Review Policies will often emit results about both the HIT itself and its Assignments, while Assignment-level review policies generally only emit results about the Assignment itself.
--
-- /Note:/ Consider using 'subjectId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrdSubjectId :: Lens.Lens' ReviewResultDetail (Core.Maybe Types.SubjectId)
rrdSubjectId = Lens.field @"subjectId"
{-# DEPRECATED rrdSubjectId "Use generic-lens or generic-optics with 'subjectId' instead." #-}

-- | The type of the object from the SubjectId field.
--
-- /Note:/ Consider using 'subjectType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrdSubjectType :: Lens.Lens' ReviewResultDetail (Core.Maybe Types.SubjectType)
rrdSubjectType = Lens.field @"subjectType"
{-# DEPRECATED rrdSubjectType "Use generic-lens or generic-optics with 'subjectType' instead." #-}

-- | The values of Key provided by the review policies you have selected.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrdValue :: Lens.Lens' ReviewResultDetail (Core.Maybe Types.Value)
rrdValue = Lens.field @"value"
{-# DEPRECATED rrdValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Core.FromJSON ReviewResultDetail where
  parseJSON =
    Core.withObject "ReviewResultDetail" Core.$
      \x ->
        ReviewResultDetail'
          Core.<$> (x Core..:? "ActionId")
          Core.<*> (x Core..:? "Key")
          Core.<*> (x Core..:? "QuestionId")
          Core.<*> (x Core..:? "SubjectId")
          Core.<*> (x Core..:? "SubjectType")
          Core.<*> (x Core..:? "Value")
