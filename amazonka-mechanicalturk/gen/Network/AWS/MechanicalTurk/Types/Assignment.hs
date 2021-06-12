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
-- Module      : Network.AWS.MechanicalTurk.Types.Assignment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.Assignment where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types.AssignmentStatus

-- | The Assignment data structure represents a single assignment of a HIT to
-- a Worker. The assignment tracks the Worker\'s efforts to complete the
-- HIT, and contains the results for later retrieval.
--
-- /See:/ 'newAssignment' smart constructor.
data Assignment = Assignment'
  { -- | The feedback string included with the call to the ApproveAssignment
    -- operation or the RejectAssignment operation, if the Requester approved
    -- or rejected the assignment and specified feedback.
    requesterFeedback :: Core.Maybe Core.Text,
    -- | A unique identifier for the assignment.
    assignmentId :: Core.Maybe Core.Text,
    -- | The ID of the HIT.
    hITId :: Core.Maybe Core.Text,
    -- | If results have been submitted, AutoApprovalTime is the date and time
    -- the results of the assignment results are considered Approved
    -- automatically if they have not already been explicitly approved or
    -- rejected by the Requester. This value is derived from the auto-approval
    -- delay specified by the Requester in the HIT. This value is omitted from
    -- the assignment if the Worker has not yet submitted results.
    autoApprovalTime :: Core.Maybe Core.POSIX,
    -- | If the Worker has submitted results and the Requester has rejected the
    -- results, RejectionTime is the date and time the Requester rejected the
    -- results.
    rejectionTime :: Core.Maybe Core.POSIX,
    -- | The status of the assignment.
    assignmentStatus :: Core.Maybe AssignmentStatus,
    -- | The Worker\'s answers submitted for the HIT contained in a
    -- QuestionFormAnswers document, if the Worker provides an answer. If the
    -- Worker does not provide any answers, Answer may contain a
    -- QuestionFormAnswers document, or Answer may be empty.
    answer :: Core.Maybe Core.Text,
    -- | If the Worker has submitted results, SubmitTime is the date and time the
    -- assignment was submitted. This value is omitted from the assignment if
    -- the Worker has not yet submitted results.
    submitTime :: Core.Maybe Core.POSIX,
    -- | The ID of the Worker who accepted the HIT.
    workerId :: Core.Maybe Core.Text,
    -- | The date and time the Worker accepted the assignment.
    acceptTime :: Core.Maybe Core.POSIX,
    -- | If the Worker has submitted results and the Requester has approved the
    -- results, ApprovalTime is the date and time the Requester approved the
    -- results. This value is omitted from the assignment if the Requester has
    -- not yet approved the results.
    approvalTime :: Core.Maybe Core.POSIX,
    -- | The date and time of the deadline for the assignment. This value is
    -- derived from the deadline specification for the HIT and the date and
    -- time the Worker accepted the HIT.
    deadline :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Assignment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requesterFeedback', 'assignment_requesterFeedback' - The feedback string included with the call to the ApproveAssignment
-- operation or the RejectAssignment operation, if the Requester approved
-- or rejected the assignment and specified feedback.
--
-- 'assignmentId', 'assignment_assignmentId' - A unique identifier for the assignment.
--
-- 'hITId', 'assignment_hITId' - The ID of the HIT.
--
-- 'autoApprovalTime', 'assignment_autoApprovalTime' - If results have been submitted, AutoApprovalTime is the date and time
-- the results of the assignment results are considered Approved
-- automatically if they have not already been explicitly approved or
-- rejected by the Requester. This value is derived from the auto-approval
-- delay specified by the Requester in the HIT. This value is omitted from
-- the assignment if the Worker has not yet submitted results.
--
-- 'rejectionTime', 'assignment_rejectionTime' - If the Worker has submitted results and the Requester has rejected the
-- results, RejectionTime is the date and time the Requester rejected the
-- results.
--
-- 'assignmentStatus', 'assignment_assignmentStatus' - The status of the assignment.
--
-- 'answer', 'assignment_answer' - The Worker\'s answers submitted for the HIT contained in a
-- QuestionFormAnswers document, if the Worker provides an answer. If the
-- Worker does not provide any answers, Answer may contain a
-- QuestionFormAnswers document, or Answer may be empty.
--
-- 'submitTime', 'assignment_submitTime' - If the Worker has submitted results, SubmitTime is the date and time the
-- assignment was submitted. This value is omitted from the assignment if
-- the Worker has not yet submitted results.
--
-- 'workerId', 'assignment_workerId' - The ID of the Worker who accepted the HIT.
--
-- 'acceptTime', 'assignment_acceptTime' - The date and time the Worker accepted the assignment.
--
-- 'approvalTime', 'assignment_approvalTime' - If the Worker has submitted results and the Requester has approved the
-- results, ApprovalTime is the date and time the Requester approved the
-- results. This value is omitted from the assignment if the Requester has
-- not yet approved the results.
--
-- 'deadline', 'assignment_deadline' - The date and time of the deadline for the assignment. This value is
-- derived from the deadline specification for the HIT and the date and
-- time the Worker accepted the HIT.
newAssignment ::
  Assignment
newAssignment =
  Assignment'
    { requesterFeedback = Core.Nothing,
      assignmentId = Core.Nothing,
      hITId = Core.Nothing,
      autoApprovalTime = Core.Nothing,
      rejectionTime = Core.Nothing,
      assignmentStatus = Core.Nothing,
      answer = Core.Nothing,
      submitTime = Core.Nothing,
      workerId = Core.Nothing,
      acceptTime = Core.Nothing,
      approvalTime = Core.Nothing,
      deadline = Core.Nothing
    }

-- | The feedback string included with the call to the ApproveAssignment
-- operation or the RejectAssignment operation, if the Requester approved
-- or rejected the assignment and specified feedback.
assignment_requesterFeedback :: Lens.Lens' Assignment (Core.Maybe Core.Text)
assignment_requesterFeedback = Lens.lens (\Assignment' {requesterFeedback} -> requesterFeedback) (\s@Assignment' {} a -> s {requesterFeedback = a} :: Assignment)

-- | A unique identifier for the assignment.
assignment_assignmentId :: Lens.Lens' Assignment (Core.Maybe Core.Text)
assignment_assignmentId = Lens.lens (\Assignment' {assignmentId} -> assignmentId) (\s@Assignment' {} a -> s {assignmentId = a} :: Assignment)

-- | The ID of the HIT.
assignment_hITId :: Lens.Lens' Assignment (Core.Maybe Core.Text)
assignment_hITId = Lens.lens (\Assignment' {hITId} -> hITId) (\s@Assignment' {} a -> s {hITId = a} :: Assignment)

-- | If results have been submitted, AutoApprovalTime is the date and time
-- the results of the assignment results are considered Approved
-- automatically if they have not already been explicitly approved or
-- rejected by the Requester. This value is derived from the auto-approval
-- delay specified by the Requester in the HIT. This value is omitted from
-- the assignment if the Worker has not yet submitted results.
assignment_autoApprovalTime :: Lens.Lens' Assignment (Core.Maybe Core.UTCTime)
assignment_autoApprovalTime = Lens.lens (\Assignment' {autoApprovalTime} -> autoApprovalTime) (\s@Assignment' {} a -> s {autoApprovalTime = a} :: Assignment) Core.. Lens.mapping Core._Time

-- | If the Worker has submitted results and the Requester has rejected the
-- results, RejectionTime is the date and time the Requester rejected the
-- results.
assignment_rejectionTime :: Lens.Lens' Assignment (Core.Maybe Core.UTCTime)
assignment_rejectionTime = Lens.lens (\Assignment' {rejectionTime} -> rejectionTime) (\s@Assignment' {} a -> s {rejectionTime = a} :: Assignment) Core.. Lens.mapping Core._Time

-- | The status of the assignment.
assignment_assignmentStatus :: Lens.Lens' Assignment (Core.Maybe AssignmentStatus)
assignment_assignmentStatus = Lens.lens (\Assignment' {assignmentStatus} -> assignmentStatus) (\s@Assignment' {} a -> s {assignmentStatus = a} :: Assignment)

-- | The Worker\'s answers submitted for the HIT contained in a
-- QuestionFormAnswers document, if the Worker provides an answer. If the
-- Worker does not provide any answers, Answer may contain a
-- QuestionFormAnswers document, or Answer may be empty.
assignment_answer :: Lens.Lens' Assignment (Core.Maybe Core.Text)
assignment_answer = Lens.lens (\Assignment' {answer} -> answer) (\s@Assignment' {} a -> s {answer = a} :: Assignment)

-- | If the Worker has submitted results, SubmitTime is the date and time the
-- assignment was submitted. This value is omitted from the assignment if
-- the Worker has not yet submitted results.
assignment_submitTime :: Lens.Lens' Assignment (Core.Maybe Core.UTCTime)
assignment_submitTime = Lens.lens (\Assignment' {submitTime} -> submitTime) (\s@Assignment' {} a -> s {submitTime = a} :: Assignment) Core.. Lens.mapping Core._Time

-- | The ID of the Worker who accepted the HIT.
assignment_workerId :: Lens.Lens' Assignment (Core.Maybe Core.Text)
assignment_workerId = Lens.lens (\Assignment' {workerId} -> workerId) (\s@Assignment' {} a -> s {workerId = a} :: Assignment)

-- | The date and time the Worker accepted the assignment.
assignment_acceptTime :: Lens.Lens' Assignment (Core.Maybe Core.UTCTime)
assignment_acceptTime = Lens.lens (\Assignment' {acceptTime} -> acceptTime) (\s@Assignment' {} a -> s {acceptTime = a} :: Assignment) Core.. Lens.mapping Core._Time

-- | If the Worker has submitted results and the Requester has approved the
-- results, ApprovalTime is the date and time the Requester approved the
-- results. This value is omitted from the assignment if the Requester has
-- not yet approved the results.
assignment_approvalTime :: Lens.Lens' Assignment (Core.Maybe Core.UTCTime)
assignment_approvalTime = Lens.lens (\Assignment' {approvalTime} -> approvalTime) (\s@Assignment' {} a -> s {approvalTime = a} :: Assignment) Core.. Lens.mapping Core._Time

-- | The date and time of the deadline for the assignment. This value is
-- derived from the deadline specification for the HIT and the date and
-- time the Worker accepted the HIT.
assignment_deadline :: Lens.Lens' Assignment (Core.Maybe Core.UTCTime)
assignment_deadline = Lens.lens (\Assignment' {deadline} -> deadline) (\s@Assignment' {} a -> s {deadline = a} :: Assignment) Core.. Lens.mapping Core._Time

instance Core.FromJSON Assignment where
  parseJSON =
    Core.withObject
      "Assignment"
      ( \x ->
          Assignment'
            Core.<$> (x Core..:? "RequesterFeedback")
            Core.<*> (x Core..:? "AssignmentId")
            Core.<*> (x Core..:? "HITId")
            Core.<*> (x Core..:? "AutoApprovalTime")
            Core.<*> (x Core..:? "RejectionTime")
            Core.<*> (x Core..:? "AssignmentStatus")
            Core.<*> (x Core..:? "Answer")
            Core.<*> (x Core..:? "SubmitTime")
            Core.<*> (x Core..:? "WorkerId")
            Core.<*> (x Core..:? "AcceptTime")
            Core.<*> (x Core..:? "ApprovalTime")
            Core.<*> (x Core..:? "Deadline")
      )

instance Core.Hashable Assignment

instance Core.NFData Assignment
