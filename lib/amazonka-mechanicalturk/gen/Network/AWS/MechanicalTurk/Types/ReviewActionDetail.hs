{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.ReviewActionDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.ReviewActionDetail
  ( ReviewActionDetail (..),

    -- * Smart constructor
    mkReviewActionDetail,

    -- * Lenses
    radStatus,
    radTargetId,
    radActionId,
    radTargetType,
    radResult,
    radActionName,
    radCompleteTime,
    radErrorCode,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types.ReviewActionStatus
import qualified Network.AWS.Prelude as Lude

-- | Both the AssignmentReviewReport and the HITReviewReport elements contains the ReviewActionDetail data structure. This structure is returned multiple times for each action specified in the Review Policy.
--
-- /See:/ 'mkReviewActionDetail' smart constructor.
data ReviewActionDetail = ReviewActionDetail'
  { -- | The current disposition of the action: INTENDED, SUCCEEDED, FAILED, or CANCELLED.
    status :: Lude.Maybe ReviewActionStatus,
    -- | The specific HITId or AssignmentID targeted by the action.
    targetId :: Lude.Maybe Lude.Text,
    -- | The unique identifier for the action.
    actionId :: Lude.Maybe Lude.Text,
    -- | The type of object in TargetId.
    targetType :: Lude.Maybe Lude.Text,
    -- | A description of the outcome of the review.
    result :: Lude.Maybe Lude.Text,
    -- | The nature of the action itself. The Review Policy is responsible for examining the HIT and Assignments, emitting results, and deciding which other actions will be necessary.
    actionName :: Lude.Maybe Lude.Text,
    -- | The date when the action was completed.
    completeTime :: Lude.Maybe Lude.Timestamp,
    -- | Present only when the Results have a FAILED Status.
    errorCode :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReviewActionDetail' with the minimum fields required to make a request.
--
-- * 'status' - The current disposition of the action: INTENDED, SUCCEEDED, FAILED, or CANCELLED.
-- * 'targetId' - The specific HITId or AssignmentID targeted by the action.
-- * 'actionId' - The unique identifier for the action.
-- * 'targetType' - The type of object in TargetId.
-- * 'result' - A description of the outcome of the review.
-- * 'actionName' - The nature of the action itself. The Review Policy is responsible for examining the HIT and Assignments, emitting results, and deciding which other actions will be necessary.
-- * 'completeTime' - The date when the action was completed.
-- * 'errorCode' - Present only when the Results have a FAILED Status.
mkReviewActionDetail ::
  ReviewActionDetail
mkReviewActionDetail =
  ReviewActionDetail'
    { status = Lude.Nothing,
      targetId = Lude.Nothing,
      actionId = Lude.Nothing,
      targetType = Lude.Nothing,
      result = Lude.Nothing,
      actionName = Lude.Nothing,
      completeTime = Lude.Nothing,
      errorCode = Lude.Nothing
    }

-- | The current disposition of the action: INTENDED, SUCCEEDED, FAILED, or CANCELLED.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
radStatus :: Lens.Lens' ReviewActionDetail (Lude.Maybe ReviewActionStatus)
radStatus = Lens.lens (status :: ReviewActionDetail -> Lude.Maybe ReviewActionStatus) (\s a -> s {status = a} :: ReviewActionDetail)
{-# DEPRECATED radStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The specific HITId or AssignmentID targeted by the action.
--
-- /Note:/ Consider using 'targetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
radTargetId :: Lens.Lens' ReviewActionDetail (Lude.Maybe Lude.Text)
radTargetId = Lens.lens (targetId :: ReviewActionDetail -> Lude.Maybe Lude.Text) (\s a -> s {targetId = a} :: ReviewActionDetail)
{-# DEPRECATED radTargetId "Use generic-lens or generic-optics with 'targetId' instead." #-}

-- | The unique identifier for the action.
--
-- /Note:/ Consider using 'actionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
radActionId :: Lens.Lens' ReviewActionDetail (Lude.Maybe Lude.Text)
radActionId = Lens.lens (actionId :: ReviewActionDetail -> Lude.Maybe Lude.Text) (\s a -> s {actionId = a} :: ReviewActionDetail)
{-# DEPRECATED radActionId "Use generic-lens or generic-optics with 'actionId' instead." #-}

-- | The type of object in TargetId.
--
-- /Note:/ Consider using 'targetType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
radTargetType :: Lens.Lens' ReviewActionDetail (Lude.Maybe Lude.Text)
radTargetType = Lens.lens (targetType :: ReviewActionDetail -> Lude.Maybe Lude.Text) (\s a -> s {targetType = a} :: ReviewActionDetail)
{-# DEPRECATED radTargetType "Use generic-lens or generic-optics with 'targetType' instead." #-}

-- | A description of the outcome of the review.
--
-- /Note:/ Consider using 'result' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
radResult :: Lens.Lens' ReviewActionDetail (Lude.Maybe Lude.Text)
radResult = Lens.lens (result :: ReviewActionDetail -> Lude.Maybe Lude.Text) (\s a -> s {result = a} :: ReviewActionDetail)
{-# DEPRECATED radResult "Use generic-lens or generic-optics with 'result' instead." #-}

-- | The nature of the action itself. The Review Policy is responsible for examining the HIT and Assignments, emitting results, and deciding which other actions will be necessary.
--
-- /Note:/ Consider using 'actionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
radActionName :: Lens.Lens' ReviewActionDetail (Lude.Maybe Lude.Text)
radActionName = Lens.lens (actionName :: ReviewActionDetail -> Lude.Maybe Lude.Text) (\s a -> s {actionName = a} :: ReviewActionDetail)
{-# DEPRECATED radActionName "Use generic-lens or generic-optics with 'actionName' instead." #-}

-- | The date when the action was completed.
--
-- /Note:/ Consider using 'completeTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
radCompleteTime :: Lens.Lens' ReviewActionDetail (Lude.Maybe Lude.Timestamp)
radCompleteTime = Lens.lens (completeTime :: ReviewActionDetail -> Lude.Maybe Lude.Timestamp) (\s a -> s {completeTime = a} :: ReviewActionDetail)
{-# DEPRECATED radCompleteTime "Use generic-lens or generic-optics with 'completeTime' instead." #-}

-- | Present only when the Results have a FAILED Status.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
radErrorCode :: Lens.Lens' ReviewActionDetail (Lude.Maybe Lude.Text)
radErrorCode = Lens.lens (errorCode :: ReviewActionDetail -> Lude.Maybe Lude.Text) (\s a -> s {errorCode = a} :: ReviewActionDetail)
{-# DEPRECATED radErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

instance Lude.FromJSON ReviewActionDetail where
  parseJSON =
    Lude.withObject
      "ReviewActionDetail"
      ( \x ->
          ReviewActionDetail'
            Lude.<$> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "TargetId")
            Lude.<*> (x Lude..:? "ActionId")
            Lude.<*> (x Lude..:? "TargetType")
            Lude.<*> (x Lude..:? "Result")
            Lude.<*> (x Lude..:? "ActionName")
            Lude.<*> (x Lude..:? "CompleteTime")
            Lude.<*> (x Lude..:? "ErrorCode")
      )
