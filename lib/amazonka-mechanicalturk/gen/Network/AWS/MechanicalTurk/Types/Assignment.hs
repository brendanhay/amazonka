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
    aApprovalTime,
    aAssignmentId,
    aAssignmentStatus,
    aAutoApprovalTime,
    aDeadline,
    aHITId,
    aRejectionTime,
    aRequesterFeedback,
    aSubmitTime,
    aWorkerId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MechanicalTurk.Types.AssignmentId as Types
import qualified Network.AWS.MechanicalTurk.Types.AssignmentStatus as Types
import qualified Network.AWS.MechanicalTurk.Types.CustomerId as Types
import qualified Network.AWS.MechanicalTurk.Types.HITId as Types
import qualified Network.AWS.MechanicalTurk.Types.String as Types
import qualified Network.AWS.Prelude as Core

-- | The Assignment data structure represents a single assignment of a HIT to a Worker. The assignment tracks the Worker's efforts to complete the HIT, and contains the results for later retrieval.
--
-- /See:/ 'mkAssignment' smart constructor.
data Assignment = Assignment'
  { -- | The date and time the Worker accepted the assignment.
    acceptTime :: Core.Maybe Core.NominalDiffTime,
    -- | The Worker's answers submitted for the HIT contained in a QuestionFormAnswers document, if the Worker provides an answer. If the Worker does not provide any answers, Answer may contain a QuestionFormAnswers document, or Answer may be empty.
    answer :: Core.Maybe Types.String,
    -- | If the Worker has submitted results and the Requester has approved the results, ApprovalTime is the date and time the Requester approved the results. This value is omitted from the assignment if the Requester has not yet approved the results.
    approvalTime :: Core.Maybe Core.NominalDiffTime,
    -- | A unique identifier for the assignment.
    assignmentId :: Core.Maybe Types.AssignmentId,
    -- | The status of the assignment.
    assignmentStatus :: Core.Maybe Types.AssignmentStatus,
    -- | If results have been submitted, AutoApprovalTime is the date and time the results of the assignment results are considered Approved automatically if they have not already been explicitly approved or rejected by the Requester. This value is derived from the auto-approval delay specified by the Requester in the HIT. This value is omitted from the assignment if the Worker has not yet submitted results.
    autoApprovalTime :: Core.Maybe Core.NominalDiffTime,
    -- | The date and time of the deadline for the assignment. This value is derived from the deadline specification for the HIT and the date and time the Worker accepted the HIT.
    deadline :: Core.Maybe Core.NominalDiffTime,
    -- | The ID of the HIT.
    hITId :: Core.Maybe Types.HITId,
    -- | If the Worker has submitted results and the Requester has rejected the results, RejectionTime is the date and time the Requester rejected the results.
    rejectionTime :: Core.Maybe Core.NominalDiffTime,
    -- | The feedback string included with the call to the ApproveAssignment operation or the RejectAssignment operation, if the Requester approved or rejected the assignment and specified feedback.
    requesterFeedback :: Core.Maybe Types.String,
    -- | If the Worker has submitted results, SubmitTime is the date and time the assignment was submitted. This value is omitted from the assignment if the Worker has not yet submitted results.
    submitTime :: Core.Maybe Core.NominalDiffTime,
    -- | The ID of the Worker who accepted the HIT.
    workerId :: Core.Maybe Types.CustomerId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Assignment' value with any optional fields omitted.
mkAssignment ::
  Assignment
mkAssignment =
  Assignment'
    { acceptTime = Core.Nothing,
      answer = Core.Nothing,
      approvalTime = Core.Nothing,
      assignmentId = Core.Nothing,
      assignmentStatus = Core.Nothing,
      autoApprovalTime = Core.Nothing,
      deadline = Core.Nothing,
      hITId = Core.Nothing,
      rejectionTime = Core.Nothing,
      requesterFeedback = Core.Nothing,
      submitTime = Core.Nothing,
      workerId = Core.Nothing
    }

-- | The date and time the Worker accepted the assignment.
--
-- /Note:/ Consider using 'acceptTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAcceptTime :: Lens.Lens' Assignment (Core.Maybe Core.NominalDiffTime)
aAcceptTime = Lens.field @"acceptTime"
{-# DEPRECATED aAcceptTime "Use generic-lens or generic-optics with 'acceptTime' instead." #-}

-- | The Worker's answers submitted for the HIT contained in a QuestionFormAnswers document, if the Worker provides an answer. If the Worker does not provide any answers, Answer may contain a QuestionFormAnswers document, or Answer may be empty.
--
-- /Note:/ Consider using 'answer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAnswer :: Lens.Lens' Assignment (Core.Maybe Types.String)
aAnswer = Lens.field @"answer"
{-# DEPRECATED aAnswer "Use generic-lens or generic-optics with 'answer' instead." #-}

-- | If the Worker has submitted results and the Requester has approved the results, ApprovalTime is the date and time the Requester approved the results. This value is omitted from the assignment if the Requester has not yet approved the results.
--
-- /Note:/ Consider using 'approvalTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aApprovalTime :: Lens.Lens' Assignment (Core.Maybe Core.NominalDiffTime)
aApprovalTime = Lens.field @"approvalTime"
{-# DEPRECATED aApprovalTime "Use generic-lens or generic-optics with 'approvalTime' instead." #-}

-- | A unique identifier for the assignment.
--
-- /Note:/ Consider using 'assignmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAssignmentId :: Lens.Lens' Assignment (Core.Maybe Types.AssignmentId)
aAssignmentId = Lens.field @"assignmentId"
{-# DEPRECATED aAssignmentId "Use generic-lens or generic-optics with 'assignmentId' instead." #-}

-- | The status of the assignment.
--
-- /Note:/ Consider using 'assignmentStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAssignmentStatus :: Lens.Lens' Assignment (Core.Maybe Types.AssignmentStatus)
aAssignmentStatus = Lens.field @"assignmentStatus"
{-# DEPRECATED aAssignmentStatus "Use generic-lens or generic-optics with 'assignmentStatus' instead." #-}

-- | If results have been submitted, AutoApprovalTime is the date and time the results of the assignment results are considered Approved automatically if they have not already been explicitly approved or rejected by the Requester. This value is derived from the auto-approval delay specified by the Requester in the HIT. This value is omitted from the assignment if the Worker has not yet submitted results.
--
-- /Note:/ Consider using 'autoApprovalTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAutoApprovalTime :: Lens.Lens' Assignment (Core.Maybe Core.NominalDiffTime)
aAutoApprovalTime = Lens.field @"autoApprovalTime"
{-# DEPRECATED aAutoApprovalTime "Use generic-lens or generic-optics with 'autoApprovalTime' instead." #-}

-- | The date and time of the deadline for the assignment. This value is derived from the deadline specification for the HIT and the date and time the Worker accepted the HIT.
--
-- /Note:/ Consider using 'deadline' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aDeadline :: Lens.Lens' Assignment (Core.Maybe Core.NominalDiffTime)
aDeadline = Lens.field @"deadline"
{-# DEPRECATED aDeadline "Use generic-lens or generic-optics with 'deadline' instead." #-}

-- | The ID of the HIT.
--
-- /Note:/ Consider using 'hITId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aHITId :: Lens.Lens' Assignment (Core.Maybe Types.HITId)
aHITId = Lens.field @"hITId"
{-# DEPRECATED aHITId "Use generic-lens or generic-optics with 'hITId' instead." #-}

-- | If the Worker has submitted results and the Requester has rejected the results, RejectionTime is the date and time the Requester rejected the results.
--
-- /Note:/ Consider using 'rejectionTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aRejectionTime :: Lens.Lens' Assignment (Core.Maybe Core.NominalDiffTime)
aRejectionTime = Lens.field @"rejectionTime"
{-# DEPRECATED aRejectionTime "Use generic-lens or generic-optics with 'rejectionTime' instead." #-}

-- | The feedback string included with the call to the ApproveAssignment operation or the RejectAssignment operation, if the Requester approved or rejected the assignment and specified feedback.
--
-- /Note:/ Consider using 'requesterFeedback' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aRequesterFeedback :: Lens.Lens' Assignment (Core.Maybe Types.String)
aRequesterFeedback = Lens.field @"requesterFeedback"
{-# DEPRECATED aRequesterFeedback "Use generic-lens or generic-optics with 'requesterFeedback' instead." #-}

-- | If the Worker has submitted results, SubmitTime is the date and time the assignment was submitted. This value is omitted from the assignment if the Worker has not yet submitted results.
--
-- /Note:/ Consider using 'submitTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aSubmitTime :: Lens.Lens' Assignment (Core.Maybe Core.NominalDiffTime)
aSubmitTime = Lens.field @"submitTime"
{-# DEPRECATED aSubmitTime "Use generic-lens or generic-optics with 'submitTime' instead." #-}

-- | The ID of the Worker who accepted the HIT.
--
-- /Note:/ Consider using 'workerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aWorkerId :: Lens.Lens' Assignment (Core.Maybe Types.CustomerId)
aWorkerId = Lens.field @"workerId"
{-# DEPRECATED aWorkerId "Use generic-lens or generic-optics with 'workerId' instead." #-}

instance Core.FromJSON Assignment where
  parseJSON =
    Core.withObject "Assignment" Core.$
      \x ->
        Assignment'
          Core.<$> (x Core..:? "AcceptTime")
          Core.<*> (x Core..:? "Answer")
          Core.<*> (x Core..:? "ApprovalTime")
          Core.<*> (x Core..:? "AssignmentId")
          Core.<*> (x Core..:? "AssignmentStatus")
          Core.<*> (x Core..:? "AutoApprovalTime")
          Core.<*> (x Core..:? "Deadline")
          Core.<*> (x Core..:? "HITId")
          Core.<*> (x Core..:? "RejectionTime")
          Core.<*> (x Core..:? "RequesterFeedback")
          Core.<*> (x Core..:? "SubmitTime")
          Core.<*> (x Core..:? "WorkerId")
