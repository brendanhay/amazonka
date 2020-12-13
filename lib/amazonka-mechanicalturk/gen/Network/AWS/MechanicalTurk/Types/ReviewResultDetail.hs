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
    rrdValue,
    rrdActionId,
    rrdSubjectType,
    rrdKey,
    rrdQuestionId,
    rrdSubjectId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | This data structure is returned multiple times for each result specified in the Review Policy.
--
-- /See:/ 'mkReviewResultDetail' smart constructor.
data ReviewResultDetail = ReviewResultDetail'
  { -- | The values of Key provided by the review policies you have selected.
    value :: Lude.Maybe Lude.Text,
    -- | A unique identifier of the Review action result.
    actionId :: Lude.Maybe Lude.Text,
    -- | The type of the object from the SubjectId field.
    subjectType :: Lude.Maybe Lude.Text,
    -- | Key identifies the particular piece of reviewed information.
    key :: Lude.Maybe Lude.Text,
    -- | Specifies the QuestionId the result is describing. Depending on whether the TargetType is a HIT or Assignment this results could specify multiple values. If TargetType is HIT and QuestionId is absent, then the result describes results of the HIT, including the HIT agreement score. If ObjectType is Assignment and QuestionId is absent, then the result describes the Worker's performance on the HIT.
    questionId :: Lude.Maybe Lude.Text,
    -- | The HITID or AssignmentId about which this result was taken. Note that HIT-level Review Policies will often emit results about both the HIT itself and its Assignments, while Assignment-level review policies generally only emit results about the Assignment itself.
    subjectId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReviewResultDetail' with the minimum fields required to make a request.
--
-- * 'value' - The values of Key provided by the review policies you have selected.
-- * 'actionId' - A unique identifier of the Review action result.
-- * 'subjectType' - The type of the object from the SubjectId field.
-- * 'key' - Key identifies the particular piece of reviewed information.
-- * 'questionId' - Specifies the QuestionId the result is describing. Depending on whether the TargetType is a HIT or Assignment this results could specify multiple values. If TargetType is HIT and QuestionId is absent, then the result describes results of the HIT, including the HIT agreement score. If ObjectType is Assignment and QuestionId is absent, then the result describes the Worker's performance on the HIT.
-- * 'subjectId' - The HITID or AssignmentId about which this result was taken. Note that HIT-level Review Policies will often emit results about both the HIT itself and its Assignments, while Assignment-level review policies generally only emit results about the Assignment itself.
mkReviewResultDetail ::
  ReviewResultDetail
mkReviewResultDetail =
  ReviewResultDetail'
    { value = Lude.Nothing,
      actionId = Lude.Nothing,
      subjectType = Lude.Nothing,
      key = Lude.Nothing,
      questionId = Lude.Nothing,
      subjectId = Lude.Nothing
    }

-- | The values of Key provided by the review policies you have selected.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrdValue :: Lens.Lens' ReviewResultDetail (Lude.Maybe Lude.Text)
rrdValue = Lens.lens (value :: ReviewResultDetail -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: ReviewResultDetail)
{-# DEPRECATED rrdValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | A unique identifier of the Review action result.
--
-- /Note:/ Consider using 'actionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrdActionId :: Lens.Lens' ReviewResultDetail (Lude.Maybe Lude.Text)
rrdActionId = Lens.lens (actionId :: ReviewResultDetail -> Lude.Maybe Lude.Text) (\s a -> s {actionId = a} :: ReviewResultDetail)
{-# DEPRECATED rrdActionId "Use generic-lens or generic-optics with 'actionId' instead." #-}

-- | The type of the object from the SubjectId field.
--
-- /Note:/ Consider using 'subjectType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrdSubjectType :: Lens.Lens' ReviewResultDetail (Lude.Maybe Lude.Text)
rrdSubjectType = Lens.lens (subjectType :: ReviewResultDetail -> Lude.Maybe Lude.Text) (\s a -> s {subjectType = a} :: ReviewResultDetail)
{-# DEPRECATED rrdSubjectType "Use generic-lens or generic-optics with 'subjectType' instead." #-}

-- | Key identifies the particular piece of reviewed information.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrdKey :: Lens.Lens' ReviewResultDetail (Lude.Maybe Lude.Text)
rrdKey = Lens.lens (key :: ReviewResultDetail -> Lude.Maybe Lude.Text) (\s a -> s {key = a} :: ReviewResultDetail)
{-# DEPRECATED rrdKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | Specifies the QuestionId the result is describing. Depending on whether the TargetType is a HIT or Assignment this results could specify multiple values. If TargetType is HIT and QuestionId is absent, then the result describes results of the HIT, including the HIT agreement score. If ObjectType is Assignment and QuestionId is absent, then the result describes the Worker's performance on the HIT.
--
-- /Note:/ Consider using 'questionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrdQuestionId :: Lens.Lens' ReviewResultDetail (Lude.Maybe Lude.Text)
rrdQuestionId = Lens.lens (questionId :: ReviewResultDetail -> Lude.Maybe Lude.Text) (\s a -> s {questionId = a} :: ReviewResultDetail)
{-# DEPRECATED rrdQuestionId "Use generic-lens or generic-optics with 'questionId' instead." #-}

-- | The HITID or AssignmentId about which this result was taken. Note that HIT-level Review Policies will often emit results about both the HIT itself and its Assignments, while Assignment-level review policies generally only emit results about the Assignment itself.
--
-- /Note:/ Consider using 'subjectId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrdSubjectId :: Lens.Lens' ReviewResultDetail (Lude.Maybe Lude.Text)
rrdSubjectId = Lens.lens (subjectId :: ReviewResultDetail -> Lude.Maybe Lude.Text) (\s a -> s {subjectId = a} :: ReviewResultDetail)
{-# DEPRECATED rrdSubjectId "Use generic-lens or generic-optics with 'subjectId' instead." #-}

instance Lude.FromJSON ReviewResultDetail where
  parseJSON =
    Lude.withObject
      "ReviewResultDetail"
      ( \x ->
          ReviewResultDetail'
            Lude.<$> (x Lude..:? "Value")
            Lude.<*> (x Lude..:? "ActionId")
            Lude.<*> (x Lude..:? "SubjectType")
            Lude.<*> (x Lude..:? "Key")
            Lude.<*> (x Lude..:? "QuestionId")
            Lude.<*> (x Lude..:? "SubjectId")
      )
