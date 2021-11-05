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
-- Module      : Amazonka.MechanicalTurk.Types.Assignment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MechanicalTurk.Types.Assignment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MechanicalTurk.Types.AssignmentStatus
import qualified Amazonka.Prelude as Prelude

-- | The Assignment data structure represents a single assignment of a HIT to
-- a Worker. The assignment tracks the Worker\'s efforts to complete the
-- HIT, and contains the results for later retrieval.
--
-- /See:/ 'newAssignment' smart constructor.
data Assignment = Assignment'
  { -- | The date and time the Worker accepted the assignment.
    acceptTime :: Prelude.Maybe Core.POSIX,
    -- | The Worker\'s answers submitted for the HIT contained in a
    -- QuestionFormAnswers document, if the Worker provides an answer. If the
    -- Worker does not provide any answers, Answer may contain a
    -- QuestionFormAnswers document, or Answer may be empty.
    answer :: Prelude.Maybe Prelude.Text,
    -- | The status of the assignment.
    assignmentStatus :: Prelude.Maybe AssignmentStatus,
    -- | The feedback string included with the call to the ApproveAssignment
    -- operation or the RejectAssignment operation, if the Requester approved
    -- or rejected the assignment and specified feedback.
    requesterFeedback :: Prelude.Maybe Prelude.Text,
    -- | The date and time of the deadline for the assignment. This value is
    -- derived from the deadline specification for the HIT and the date and
    -- time the Worker accepted the HIT.
    deadline :: Prelude.Maybe Core.POSIX,
    -- | If the Worker has submitted results and the Requester has approved the
    -- results, ApprovalTime is the date and time the Requester approved the
    -- results. This value is omitted from the assignment if the Requester has
    -- not yet approved the results.
    approvalTime :: Prelude.Maybe Core.POSIX,
    -- | If the Worker has submitted results and the Requester has rejected the
    -- results, RejectionTime is the date and time the Requester rejected the
    -- results.
    rejectionTime :: Prelude.Maybe Core.POSIX,
    -- | If results have been submitted, AutoApprovalTime is the date and time
    -- the results of the assignment results are considered Approved
    -- automatically if they have not already been explicitly approved or
    -- rejected by the Requester. This value is derived from the auto-approval
    -- delay specified by the Requester in the HIT. This value is omitted from
    -- the assignment if the Worker has not yet submitted results.
    autoApprovalTime :: Prelude.Maybe Core.POSIX,
    -- | The ID of the HIT.
    hITId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Worker who accepted the HIT.
    workerId :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the assignment.
    assignmentId :: Prelude.Maybe Prelude.Text,
    -- | If the Worker has submitted results, SubmitTime is the date and time the
    -- assignment was submitted. This value is omitted from the assignment if
    -- the Worker has not yet submitted results.
    submitTime :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Assignment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceptTime', 'assignment_acceptTime' - The date and time the Worker accepted the assignment.
--
-- 'answer', 'assignment_answer' - The Worker\'s answers submitted for the HIT contained in a
-- QuestionFormAnswers document, if the Worker provides an answer. If the
-- Worker does not provide any answers, Answer may contain a
-- QuestionFormAnswers document, or Answer may be empty.
--
-- 'assignmentStatus', 'assignment_assignmentStatus' - The status of the assignment.
--
-- 'requesterFeedback', 'assignment_requesterFeedback' - The feedback string included with the call to the ApproveAssignment
-- operation or the RejectAssignment operation, if the Requester approved
-- or rejected the assignment and specified feedback.
--
-- 'deadline', 'assignment_deadline' - The date and time of the deadline for the assignment. This value is
-- derived from the deadline specification for the HIT and the date and
-- time the Worker accepted the HIT.
--
-- 'approvalTime', 'assignment_approvalTime' - If the Worker has submitted results and the Requester has approved the
-- results, ApprovalTime is the date and time the Requester approved the
-- results. This value is omitted from the assignment if the Requester has
-- not yet approved the results.
--
-- 'rejectionTime', 'assignment_rejectionTime' - If the Worker has submitted results and the Requester has rejected the
-- results, RejectionTime is the date and time the Requester rejected the
-- results.
--
-- 'autoApprovalTime', 'assignment_autoApprovalTime' - If results have been submitted, AutoApprovalTime is the date and time
-- the results of the assignment results are considered Approved
-- automatically if they have not already been explicitly approved or
-- rejected by the Requester. This value is derived from the auto-approval
-- delay specified by the Requester in the HIT. This value is omitted from
-- the assignment if the Worker has not yet submitted results.
--
-- 'hITId', 'assignment_hITId' - The ID of the HIT.
--
-- 'workerId', 'assignment_workerId' - The ID of the Worker who accepted the HIT.
--
-- 'assignmentId', 'assignment_assignmentId' - A unique identifier for the assignment.
--
-- 'submitTime', 'assignment_submitTime' - If the Worker has submitted results, SubmitTime is the date and time the
-- assignment was submitted. This value is omitted from the assignment if
-- the Worker has not yet submitted results.
newAssignment ::
  Assignment
newAssignment =
  Assignment'
    { acceptTime = Prelude.Nothing,
      answer = Prelude.Nothing,
      assignmentStatus = Prelude.Nothing,
      requesterFeedback = Prelude.Nothing,
      deadline = Prelude.Nothing,
      approvalTime = Prelude.Nothing,
      rejectionTime = Prelude.Nothing,
      autoApprovalTime = Prelude.Nothing,
      hITId = Prelude.Nothing,
      workerId = Prelude.Nothing,
      assignmentId = Prelude.Nothing,
      submitTime = Prelude.Nothing
    }

-- | The date and time the Worker accepted the assignment.
assignment_acceptTime :: Lens.Lens' Assignment (Prelude.Maybe Prelude.UTCTime)
assignment_acceptTime = Lens.lens (\Assignment' {acceptTime} -> acceptTime) (\s@Assignment' {} a -> s {acceptTime = a} :: Assignment) Prelude.. Lens.mapping Core._Time

-- | The Worker\'s answers submitted for the HIT contained in a
-- QuestionFormAnswers document, if the Worker provides an answer. If the
-- Worker does not provide any answers, Answer may contain a
-- QuestionFormAnswers document, or Answer may be empty.
assignment_answer :: Lens.Lens' Assignment (Prelude.Maybe Prelude.Text)
assignment_answer = Lens.lens (\Assignment' {answer} -> answer) (\s@Assignment' {} a -> s {answer = a} :: Assignment)

-- | The status of the assignment.
assignment_assignmentStatus :: Lens.Lens' Assignment (Prelude.Maybe AssignmentStatus)
assignment_assignmentStatus = Lens.lens (\Assignment' {assignmentStatus} -> assignmentStatus) (\s@Assignment' {} a -> s {assignmentStatus = a} :: Assignment)

-- | The feedback string included with the call to the ApproveAssignment
-- operation or the RejectAssignment operation, if the Requester approved
-- or rejected the assignment and specified feedback.
assignment_requesterFeedback :: Lens.Lens' Assignment (Prelude.Maybe Prelude.Text)
assignment_requesterFeedback = Lens.lens (\Assignment' {requesterFeedback} -> requesterFeedback) (\s@Assignment' {} a -> s {requesterFeedback = a} :: Assignment)

-- | The date and time of the deadline for the assignment. This value is
-- derived from the deadline specification for the HIT and the date and
-- time the Worker accepted the HIT.
assignment_deadline :: Lens.Lens' Assignment (Prelude.Maybe Prelude.UTCTime)
assignment_deadline = Lens.lens (\Assignment' {deadline} -> deadline) (\s@Assignment' {} a -> s {deadline = a} :: Assignment) Prelude.. Lens.mapping Core._Time

-- | If the Worker has submitted results and the Requester has approved the
-- results, ApprovalTime is the date and time the Requester approved the
-- results. This value is omitted from the assignment if the Requester has
-- not yet approved the results.
assignment_approvalTime :: Lens.Lens' Assignment (Prelude.Maybe Prelude.UTCTime)
assignment_approvalTime = Lens.lens (\Assignment' {approvalTime} -> approvalTime) (\s@Assignment' {} a -> s {approvalTime = a} :: Assignment) Prelude.. Lens.mapping Core._Time

-- | If the Worker has submitted results and the Requester has rejected the
-- results, RejectionTime is the date and time the Requester rejected the
-- results.
assignment_rejectionTime :: Lens.Lens' Assignment (Prelude.Maybe Prelude.UTCTime)
assignment_rejectionTime = Lens.lens (\Assignment' {rejectionTime} -> rejectionTime) (\s@Assignment' {} a -> s {rejectionTime = a} :: Assignment) Prelude.. Lens.mapping Core._Time

-- | If results have been submitted, AutoApprovalTime is the date and time
-- the results of the assignment results are considered Approved
-- automatically if they have not already been explicitly approved or
-- rejected by the Requester. This value is derived from the auto-approval
-- delay specified by the Requester in the HIT. This value is omitted from
-- the assignment if the Worker has not yet submitted results.
assignment_autoApprovalTime :: Lens.Lens' Assignment (Prelude.Maybe Prelude.UTCTime)
assignment_autoApprovalTime = Lens.lens (\Assignment' {autoApprovalTime} -> autoApprovalTime) (\s@Assignment' {} a -> s {autoApprovalTime = a} :: Assignment) Prelude.. Lens.mapping Core._Time

-- | The ID of the HIT.
assignment_hITId :: Lens.Lens' Assignment (Prelude.Maybe Prelude.Text)
assignment_hITId = Lens.lens (\Assignment' {hITId} -> hITId) (\s@Assignment' {} a -> s {hITId = a} :: Assignment)

-- | The ID of the Worker who accepted the HIT.
assignment_workerId :: Lens.Lens' Assignment (Prelude.Maybe Prelude.Text)
assignment_workerId = Lens.lens (\Assignment' {workerId} -> workerId) (\s@Assignment' {} a -> s {workerId = a} :: Assignment)

-- | A unique identifier for the assignment.
assignment_assignmentId :: Lens.Lens' Assignment (Prelude.Maybe Prelude.Text)
assignment_assignmentId = Lens.lens (\Assignment' {assignmentId} -> assignmentId) (\s@Assignment' {} a -> s {assignmentId = a} :: Assignment)

-- | If the Worker has submitted results, SubmitTime is the date and time the
-- assignment was submitted. This value is omitted from the assignment if
-- the Worker has not yet submitted results.
assignment_submitTime :: Lens.Lens' Assignment (Prelude.Maybe Prelude.UTCTime)
assignment_submitTime = Lens.lens (\Assignment' {submitTime} -> submitTime) (\s@Assignment' {} a -> s {submitTime = a} :: Assignment) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON Assignment where
  parseJSON =
    Core.withObject
      "Assignment"
      ( \x ->
          Assignment'
            Prelude.<$> (x Core..:? "AcceptTime")
            Prelude.<*> (x Core..:? "Answer")
            Prelude.<*> (x Core..:? "AssignmentStatus")
            Prelude.<*> (x Core..:? "RequesterFeedback")
            Prelude.<*> (x Core..:? "Deadline")
            Prelude.<*> (x Core..:? "ApprovalTime")
            Prelude.<*> (x Core..:? "RejectionTime")
            Prelude.<*> (x Core..:? "AutoApprovalTime")
            Prelude.<*> (x Core..:? "HITId")
            Prelude.<*> (x Core..:? "WorkerId")
            Prelude.<*> (x Core..:? "AssignmentId")
            Prelude.<*> (x Core..:? "SubmitTime")
      )

instance Prelude.Hashable Assignment

instance Prelude.NFData Assignment
