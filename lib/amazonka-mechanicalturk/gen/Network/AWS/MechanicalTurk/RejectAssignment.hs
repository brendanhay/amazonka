{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.RejectAssignment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @RejectAssignment@ operation rejects the results of a completed assignment.
--
-- You can include an optional feedback message with the rejection, which the Worker can see in the Status section of the web site. When you include a feedback message with the rejection, it helps the Worker understand why the assignment was rejected, and can improve the quality of the results the Worker submits in the future.
-- Only the Requester who created the HIT can reject an assignment for the HIT.
module Network.AWS.MechanicalTurk.RejectAssignment
  ( -- * Creating a request
    RejectAssignment (..),
    mkRejectAssignment,

    -- ** Request lenses
    raAssignmentId,
    raRequesterFeedback,

    -- * Destructuring the response
    RejectAssignmentResponse (..),
    mkRejectAssignmentResponse,

    -- ** Response lenses
    rarsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRejectAssignment' smart constructor.
data RejectAssignment = RejectAssignment'
  { assignmentId ::
      Lude.Text,
    requesterFeedback :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RejectAssignment' with the minimum fields required to make a request.
--
-- * 'assignmentId' - The ID of the assignment. The assignment must correspond to a HIT created by the Requester.
-- * 'requesterFeedback' - A message for the Worker, which the Worker can see in the Status section of the web site.
mkRejectAssignment ::
  -- | 'assignmentId'
  Lude.Text ->
  -- | 'requesterFeedback'
  Lude.Text ->
  RejectAssignment
mkRejectAssignment pAssignmentId_ pRequesterFeedback_ =
  RejectAssignment'
    { assignmentId = pAssignmentId_,
      requesterFeedback = pRequesterFeedback_
    }

-- | The ID of the assignment. The assignment must correspond to a HIT created by the Requester.
--
-- /Note:/ Consider using 'assignmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raAssignmentId :: Lens.Lens' RejectAssignment Lude.Text
raAssignmentId = Lens.lens (assignmentId :: RejectAssignment -> Lude.Text) (\s a -> s {assignmentId = a} :: RejectAssignment)
{-# DEPRECATED raAssignmentId "Use generic-lens or generic-optics with 'assignmentId' instead." #-}

-- | A message for the Worker, which the Worker can see in the Status section of the web site.
--
-- /Note:/ Consider using 'requesterFeedback' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raRequesterFeedback :: Lens.Lens' RejectAssignment Lude.Text
raRequesterFeedback = Lens.lens (requesterFeedback :: RejectAssignment -> Lude.Text) (\s a -> s {requesterFeedback = a} :: RejectAssignment)
{-# DEPRECATED raRequesterFeedback "Use generic-lens or generic-optics with 'requesterFeedback' instead." #-}

instance Lude.AWSRequest RejectAssignment where
  type Rs RejectAssignment = RejectAssignmentResponse
  request = Req.postJSON mechanicalTurkService
  response =
    Res.receiveEmpty
      ( \s h x ->
          RejectAssignmentResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RejectAssignment where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "MTurkRequesterServiceV20170117.RejectAssignment" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RejectAssignment where
  toJSON RejectAssignment' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("AssignmentId" Lude..= assignmentId),
            Lude.Just ("RequesterFeedback" Lude..= requesterFeedback)
          ]
      )

instance Lude.ToPath RejectAssignment where
  toPath = Lude.const "/"

instance Lude.ToQuery RejectAssignment where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRejectAssignmentResponse' smart constructor.
newtype RejectAssignmentResponse = RejectAssignmentResponse'
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

-- | Creates a value of 'RejectAssignmentResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkRejectAssignmentResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RejectAssignmentResponse
mkRejectAssignmentResponse pResponseStatus_ =
  RejectAssignmentResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rarsResponseStatus :: Lens.Lens' RejectAssignmentResponse Lude.Int
rarsResponseStatus = Lens.lens (responseStatus :: RejectAssignmentResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RejectAssignmentResponse)
{-# DEPRECATED rarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
