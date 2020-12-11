{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.UpdateFindingsFeedback
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Marks the specified GuardDuty findings as useful or not useful.
module Network.AWS.GuardDuty.UpdateFindingsFeedback
  ( -- * Creating a request
    UpdateFindingsFeedback (..),
    mkUpdateFindingsFeedback,

    -- ** Request lenses
    uffComments,
    uffDetectorId,
    uffFindingIds,
    uffFeedback,

    -- * Destructuring the response
    UpdateFindingsFeedbackResponse (..),
    mkUpdateFindingsFeedbackResponse,

    -- ** Response lenses
    uffrsResponseStatus,
  )
where

import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateFindingsFeedback' smart constructor.
data UpdateFindingsFeedback = UpdateFindingsFeedback'
  { comments ::
      Lude.Maybe Lude.Text,
    detectorId :: Lude.Text,
    findingIds :: [Lude.Text],
    feedback :: Feedback
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateFindingsFeedback' with the minimum fields required to make a request.
--
-- * 'comments' - Additional feedback about the GuardDuty findings.
-- * 'detectorId' - The ID of the detector associated with the findings to update feedback for.
-- * 'feedback' - The feedback for the finding.
-- * 'findingIds' - The IDs of the findings that you want to mark as useful or not useful.
mkUpdateFindingsFeedback ::
  -- | 'detectorId'
  Lude.Text ->
  -- | 'feedback'
  Feedback ->
  UpdateFindingsFeedback
mkUpdateFindingsFeedback pDetectorId_ pFeedback_ =
  UpdateFindingsFeedback'
    { comments = Lude.Nothing,
      detectorId = pDetectorId_,
      findingIds = Lude.mempty,
      feedback = pFeedback_
    }

-- | Additional feedback about the GuardDuty findings.
--
-- /Note:/ Consider using 'comments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uffComments :: Lens.Lens' UpdateFindingsFeedback (Lude.Maybe Lude.Text)
uffComments = Lens.lens (comments :: UpdateFindingsFeedback -> Lude.Maybe Lude.Text) (\s a -> s {comments = a} :: UpdateFindingsFeedback)
{-# DEPRECATED uffComments "Use generic-lens or generic-optics with 'comments' instead." #-}

-- | The ID of the detector associated with the findings to update feedback for.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uffDetectorId :: Lens.Lens' UpdateFindingsFeedback Lude.Text
uffDetectorId = Lens.lens (detectorId :: UpdateFindingsFeedback -> Lude.Text) (\s a -> s {detectorId = a} :: UpdateFindingsFeedback)
{-# DEPRECATED uffDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

-- | The IDs of the findings that you want to mark as useful or not useful.
--
-- /Note:/ Consider using 'findingIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uffFindingIds :: Lens.Lens' UpdateFindingsFeedback [Lude.Text]
uffFindingIds = Lens.lens (findingIds :: UpdateFindingsFeedback -> [Lude.Text]) (\s a -> s {findingIds = a} :: UpdateFindingsFeedback)
{-# DEPRECATED uffFindingIds "Use generic-lens or generic-optics with 'findingIds' instead." #-}

-- | The feedback for the finding.
--
-- /Note:/ Consider using 'feedback' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uffFeedback :: Lens.Lens' UpdateFindingsFeedback Feedback
uffFeedback = Lens.lens (feedback :: UpdateFindingsFeedback -> Feedback) (\s a -> s {feedback = a} :: UpdateFindingsFeedback)
{-# DEPRECATED uffFeedback "Use generic-lens or generic-optics with 'feedback' instead." #-}

instance Lude.AWSRequest UpdateFindingsFeedback where
  type Rs UpdateFindingsFeedback = UpdateFindingsFeedbackResponse
  request = Req.postJSON guardDutyService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateFindingsFeedbackResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateFindingsFeedback where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateFindingsFeedback where
  toJSON UpdateFindingsFeedback' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("comments" Lude..=) Lude.<$> comments,
            Lude.Just ("findingIds" Lude..= findingIds),
            Lude.Just ("feedback" Lude..= feedback)
          ]
      )

instance Lude.ToPath UpdateFindingsFeedback where
  toPath UpdateFindingsFeedback' {..} =
    Lude.mconcat
      ["/detector/", Lude.toBS detectorId, "/findings/feedback"]

instance Lude.ToQuery UpdateFindingsFeedback where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateFindingsFeedbackResponse' smart constructor.
newtype UpdateFindingsFeedbackResponse = UpdateFindingsFeedbackResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateFindingsFeedbackResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateFindingsFeedbackResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateFindingsFeedbackResponse
mkUpdateFindingsFeedbackResponse pResponseStatus_ =
  UpdateFindingsFeedbackResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uffrsResponseStatus :: Lens.Lens' UpdateFindingsFeedbackResponse Lude.Int
uffrsResponseStatus = Lens.lens (responseStatus :: UpdateFindingsFeedbackResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateFindingsFeedbackResponse)
{-# DEPRECATED uffrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
