{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.Assignment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.Assignment
  ( Assignment (..),

    -- * Smart constructor
    mkAssignment,

    -- * Lenses
    aAcceptTime,
    aAnswer,
    aAssignmentStatus,
    aRequesterFeedback,
    aDeadline,
    aApprovalTime,
    aRejectionTime,
    aAutoApprovalTime,
    aHITId,
    aWorkerId,
    aAssignmentId,
    aSubmitTime,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types.AssignmentStatus
import qualified Network.AWS.Prelude as Lude

-- | The Assignment data structure represents a single assignment of a HIT to a Worker. The assignment tracks the Worker's efforts to complete the HIT, and contains the results for later retrieval.
--
-- /See:/ 'mkAssignment' smart constructor.
data Assignment = Assignment'
  { acceptTime ::
      Lude.Maybe Lude.Timestamp,
    answer :: Lude.Maybe Lude.Text,
    assignmentStatus :: Lude.Maybe AssignmentStatus,
    requesterFeedback :: Lude.Maybe Lude.Text,
    deadline :: Lude.Maybe Lude.Timestamp,
    approvalTime :: Lude.Maybe Lude.Timestamp,
    rejectionTime :: Lude.Maybe Lude.Timestamp,
    autoApprovalTime :: Lude.Maybe Lude.Timestamp,
    hITId :: Lude.Maybe Lude.Text,
    workerId :: Lude.Maybe Lude.Text,
    assignmentId :: Lude.Maybe Lude.Text,
    submitTime :: Lude.Maybe Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Assignment' with the minimum fields required to make a request.
--
-- * 'acceptTime' - The date and time the Worker accepted the assignment.
-- * 'answer' - The Worker's answers submitted for the HIT contained in a QuestionFormAnswers document, if the Worker provides an answer. If the Worker does not provide any answers, Answer may contain a QuestionFormAnswers document, or Answer may be empty.
-- * 'approvalTime' - If the Worker has submitted results and the Requester has approved the results, ApprovalTime is the date and time the Requester approved the results. This value is omitted from the assignment if the Requester has not yet approved the results.
-- * 'assignmentId' - A unique identifier for the assignment.
-- * 'assignmentStatus' - The status of the assignment.
-- * 'autoApprovalTime' - If results have been submitted, AutoApprovalTime is the date and time the results of the assignment results are considered Approved automatically if they have not already been explicitly approved or rejected by the Requester. This value is derived from the auto-approval delay specified by the Requester in the HIT. This value is omitted from the assignment if the Worker has not yet submitted results.
-- * 'deadline' - The date and time of the deadline for the assignment. This value is derived from the deadline specification for the HIT and the date and time the Worker accepted the HIT.
-- * 'hITId' - The ID of the HIT.
-- * 'rejectionTime' - If the Worker has submitted results and the Requester has rejected the results, RejectionTime is the date and time the Requester rejected the results.
-- * 'requesterFeedback' - The feedback string included with the call to the ApproveAssignment operation or the RejectAssignment operation, if the Requester approved or rejected the assignment and specified feedback.
-- * 'submitTime' - If the Worker has submitted results, SubmitTime is the date and time the assignment was submitted. This value is omitted from the assignment if the Worker has not yet submitted results.
-- * 'workerId' - The ID of the Worker who accepted the HIT.
mkAssignment ::
  Assignment
mkAssignment =
  Assignment'
    { acceptTime = Lude.Nothing,
      answer = Lude.Nothing,
      assignmentStatus = Lude.Nothing,
      requesterFeedback = Lude.Nothing,
      deadline = Lude.Nothing,
      approvalTime = Lude.Nothing,
      rejectionTime = Lude.Nothing,
      autoApprovalTime = Lude.Nothing,
      hITId = Lude.Nothing,
      workerId = Lude.Nothing,
      assignmentId = Lude.Nothing,
      submitTime = Lude.Nothing
    }

-- | The date and time the Worker accepted the assignment.
--
-- /Note:/ Consider using 'acceptTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAcceptTime :: Lens.Lens' Assignment (Lude.Maybe Lude.Timestamp)
aAcceptTime = Lens.lens (acceptTime :: Assignment -> Lude.Maybe Lude.Timestamp) (\s a -> s {acceptTime = a} :: Assignment)
{-# DEPRECATED aAcceptTime "Use generic-lens or generic-optics with 'acceptTime' instead." #-}

-- | The Worker's answers submitted for the HIT contained in a QuestionFormAnswers document, if the Worker provides an answer. If the Worker does not provide any answers, Answer may contain a QuestionFormAnswers document, or Answer may be empty.
--
-- /Note:/ Consider using 'answer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAnswer :: Lens.Lens' Assignment (Lude.Maybe Lude.Text)
aAnswer = Lens.lens (answer :: Assignment -> Lude.Maybe Lude.Text) (\s a -> s {answer = a} :: Assignment)
{-# DEPRECATED aAnswer "Use generic-lens or generic-optics with 'answer' instead." #-}

-- | The status of the assignment.
--
-- /Note:/ Consider using 'assignmentStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAssignmentStatus :: Lens.Lens' Assignment (Lude.Maybe AssignmentStatus)
aAssignmentStatus = Lens.lens (assignmentStatus :: Assignment -> Lude.Maybe AssignmentStatus) (\s a -> s {assignmentStatus = a} :: Assignment)
{-# DEPRECATED aAssignmentStatus "Use generic-lens or generic-optics with 'assignmentStatus' instead." #-}

-- | The feedback string included with the call to the ApproveAssignment operation or the RejectAssignment operation, if the Requester approved or rejected the assignment and specified feedback.
--
-- /Note:/ Consider using 'requesterFeedback' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aRequesterFeedback :: Lens.Lens' Assignment (Lude.Maybe Lude.Text)
aRequesterFeedback = Lens.lens (requesterFeedback :: Assignment -> Lude.Maybe Lude.Text) (\s a -> s {requesterFeedback = a} :: Assignment)
{-# DEPRECATED aRequesterFeedback "Use generic-lens or generic-optics with 'requesterFeedback' instead." #-}

-- | The date and time of the deadline for the assignment. This value is derived from the deadline specification for the HIT and the date and time the Worker accepted the HIT.
--
-- /Note:/ Consider using 'deadline' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aDeadline :: Lens.Lens' Assignment (Lude.Maybe Lude.Timestamp)
aDeadline = Lens.lens (deadline :: Assignment -> Lude.Maybe Lude.Timestamp) (\s a -> s {deadline = a} :: Assignment)
{-# DEPRECATED aDeadline "Use generic-lens or generic-optics with 'deadline' instead." #-}

-- | If the Worker has submitted results and the Requester has approved the results, ApprovalTime is the date and time the Requester approved the results. This value is omitted from the assignment if the Requester has not yet approved the results.
--
-- /Note:/ Consider using 'approvalTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aApprovalTime :: Lens.Lens' Assignment (Lude.Maybe Lude.Timestamp)
aApprovalTime = Lens.lens (approvalTime :: Assignment -> Lude.Maybe Lude.Timestamp) (\s a -> s {approvalTime = a} :: Assignment)
{-# DEPRECATED aApprovalTime "Use generic-lens or generic-optics with 'approvalTime' instead." #-}

-- | If the Worker has submitted results and the Requester has rejected the results, RejectionTime is the date and time the Requester rejected the results.
--
-- /Note:/ Consider using 'rejectionTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aRejectionTime :: Lens.Lens' Assignment (Lude.Maybe Lude.Timestamp)
aRejectionTime = Lens.lens (rejectionTime :: Assignment -> Lude.Maybe Lude.Timestamp) (\s a -> s {rejectionTime = a} :: Assignment)
{-# DEPRECATED aRejectionTime "Use generic-lens or generic-optics with 'rejectionTime' instead." #-}

-- | If results have been submitted, AutoApprovalTime is the date and time the results of the assignment results are considered Approved automatically if they have not already been explicitly approved or rejected by the Requester. This value is derived from the auto-approval delay specified by the Requester in the HIT. This value is omitted from the assignment if the Worker has not yet submitted results.
--
-- /Note:/ Consider using 'autoApprovalTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAutoApprovalTime :: Lens.Lens' Assignment (Lude.Maybe Lude.Timestamp)
aAutoApprovalTime = Lens.lens (autoApprovalTime :: Assignment -> Lude.Maybe Lude.Timestamp) (\s a -> s {autoApprovalTime = a} :: Assignment)
{-# DEPRECATED aAutoApprovalTime "Use generic-lens or generic-optics with 'autoApprovalTime' instead." #-}

-- | The ID of the HIT.
--
-- /Note:/ Consider using 'hITId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aHITId :: Lens.Lens' Assignment (Lude.Maybe Lude.Text)
aHITId = Lens.lens (hITId :: Assignment -> Lude.Maybe Lude.Text) (\s a -> s {hITId = a} :: Assignment)
{-# DEPRECATED aHITId "Use generic-lens or generic-optics with 'hITId' instead." #-}

-- | The ID of the Worker who accepted the HIT.
--
-- /Note:/ Consider using 'workerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aWorkerId :: Lens.Lens' Assignment (Lude.Maybe Lude.Text)
aWorkerId = Lens.lens (workerId :: Assignment -> Lude.Maybe Lude.Text) (\s a -> s {workerId = a} :: Assignment)
{-# DEPRECATED aWorkerId "Use generic-lens or generic-optics with 'workerId' instead." #-}

-- | A unique identifier for the assignment.
--
-- /Note:/ Consider using 'assignmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAssignmentId :: Lens.Lens' Assignment (Lude.Maybe Lude.Text)
aAssignmentId = Lens.lens (assignmentId :: Assignment -> Lude.Maybe Lude.Text) (\s a -> s {assignmentId = a} :: Assignment)
{-# DEPRECATED aAssignmentId "Use generic-lens or generic-optics with 'assignmentId' instead." #-}

-- | If the Worker has submitted results, SubmitTime is the date and time the assignment was submitted. This value is omitted from the assignment if the Worker has not yet submitted results.
--
-- /Note:/ Consider using 'submitTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aSubmitTime :: Lens.Lens' Assignment (Lude.Maybe Lude.Timestamp)
aSubmitTime = Lens.lens (submitTime :: Assignment -> Lude.Maybe Lude.Timestamp) (\s a -> s {submitTime = a} :: Assignment)
{-# DEPRECATED aSubmitTime "Use generic-lens or generic-optics with 'submitTime' instead." #-}

instance Lude.FromJSON Assignment where
  parseJSON =
    Lude.withObject
      "Assignment"
      ( \x ->
          Assignment'
            Lude.<$> (x Lude..:? "AcceptTime")
            Lude.<*> (x Lude..:? "Answer")
            Lude.<*> (x Lude..:? "AssignmentStatus")
            Lude.<*> (x Lude..:? "RequesterFeedback")
            Lude.<*> (x Lude..:? "Deadline")
            Lude.<*> (x Lude..:? "ApprovalTime")
            Lude.<*> (x Lude..:? "RejectionTime")
            Lude.<*> (x Lude..:? "AutoApprovalTime")
            Lude.<*> (x Lude..:? "HITId")
            Lude.<*> (x Lude..:? "WorkerId")
            Lude.<*> (x Lude..:? "AssignmentId")
            Lude.<*> (x Lude..:? "SubmitTime")
      )
