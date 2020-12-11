{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.ApproveAssignment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @ApproveAssignment@ operation approves the results of a completed assignment.
--
-- Approving an assignment initiates two payments from the Requester's Amazon.com account
--
--     * The Worker who submitted the results is paid the reward specified in the HIT.
--
--
--     * Amazon Mechanical Turk fees are debited.
--
--
-- If the Requester's account does not have adequate funds for these payments, the call to ApproveAssignment returns an exception, and the approval is not processed. You can include an optional feedback message with the approval, which the Worker can see in the Status section of the web site.
-- You can also call this operation for assignments that were previous rejected and approve them by explicitly overriding the previous rejection. This only works on rejected assignments that were submitted within the previous 30 days and only if the assignment's related HIT has not been deleted.
module Network.AWS.MechanicalTurk.ApproveAssignment
  ( -- * Creating a request
    ApproveAssignment (..),
    mkApproveAssignment,

    -- ** Request lenses
    aaOverrideRejection,
    aaRequesterFeedback,
    aaAssignmentId,

    -- * Destructuring the response
    ApproveAssignmentResponse (..),
    mkApproveAssignmentResponse,

    -- ** Response lenses
    aarsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkApproveAssignment' smart constructor.
data ApproveAssignment = ApproveAssignment'
  { overrideRejection ::
      Lude.Maybe Lude.Bool,
    requesterFeedback :: Lude.Maybe Lude.Text,
    assignmentId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ApproveAssignment' with the minimum fields required to make a request.
--
-- * 'assignmentId' - The ID of the assignment. The assignment must correspond to a HIT created by the Requester.
-- * 'overrideRejection' - A flag indicating that an assignment should be approved even if it was previously rejected. Defaults to @False@ .
-- * 'requesterFeedback' - A message for the Worker, which the Worker can see in the Status section of the web site.
mkApproveAssignment ::
  -- | 'assignmentId'
  Lude.Text ->
  ApproveAssignment
mkApproveAssignment pAssignmentId_ =
  ApproveAssignment'
    { overrideRejection = Lude.Nothing,
      requesterFeedback = Lude.Nothing,
      assignmentId = pAssignmentId_
    }

-- | A flag indicating that an assignment should be approved even if it was previously rejected. Defaults to @False@ .
--
-- /Note:/ Consider using 'overrideRejection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaOverrideRejection :: Lens.Lens' ApproveAssignment (Lude.Maybe Lude.Bool)
aaOverrideRejection = Lens.lens (overrideRejection :: ApproveAssignment -> Lude.Maybe Lude.Bool) (\s a -> s {overrideRejection = a} :: ApproveAssignment)
{-# DEPRECATED aaOverrideRejection "Use generic-lens or generic-optics with 'overrideRejection' instead." #-}

-- | A message for the Worker, which the Worker can see in the Status section of the web site.
--
-- /Note:/ Consider using 'requesterFeedback' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaRequesterFeedback :: Lens.Lens' ApproveAssignment (Lude.Maybe Lude.Text)
aaRequesterFeedback = Lens.lens (requesterFeedback :: ApproveAssignment -> Lude.Maybe Lude.Text) (\s a -> s {requesterFeedback = a} :: ApproveAssignment)
{-# DEPRECATED aaRequesterFeedback "Use generic-lens or generic-optics with 'requesterFeedback' instead." #-}

-- | The ID of the assignment. The assignment must correspond to a HIT created by the Requester.
--
-- /Note:/ Consider using 'assignmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaAssignmentId :: Lens.Lens' ApproveAssignment Lude.Text
aaAssignmentId = Lens.lens (assignmentId :: ApproveAssignment -> Lude.Text) (\s a -> s {assignmentId = a} :: ApproveAssignment)
{-# DEPRECATED aaAssignmentId "Use generic-lens or generic-optics with 'assignmentId' instead." #-}

instance Lude.AWSRequest ApproveAssignment where
  type Rs ApproveAssignment = ApproveAssignmentResponse
  request = Req.postJSON mechanicalTurkService
  response =
    Res.receiveEmpty
      ( \s h x ->
          ApproveAssignmentResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ApproveAssignment where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "MTurkRequesterServiceV20170117.ApproveAssignment" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ApproveAssignment where
  toJSON ApproveAssignment' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("OverrideRejection" Lude..=) Lude.<$> overrideRejection,
            ("RequesterFeedback" Lude..=) Lude.<$> requesterFeedback,
            Lude.Just ("AssignmentId" Lude..= assignmentId)
          ]
      )

instance Lude.ToPath ApproveAssignment where
  toPath = Lude.const "/"

instance Lude.ToQuery ApproveAssignment where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkApproveAssignmentResponse' smart constructor.
newtype ApproveAssignmentResponse = ApproveAssignmentResponse'
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

-- | Creates a value of 'ApproveAssignmentResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkApproveAssignmentResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ApproveAssignmentResponse
mkApproveAssignmentResponse pResponseStatus_ =
  ApproveAssignmentResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aarsResponseStatus :: Lens.Lens' ApproveAssignmentResponse Lude.Int
aarsResponseStatus = Lens.lens (responseStatus :: ApproveAssignmentResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ApproveAssignmentResponse)
{-# DEPRECATED aarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
