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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MechanicalTurk.Types.Assignment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MechanicalTurk.Types.AssignmentStatus
import qualified Amazonka.Prelude as Prelude

-- | The Assignment data structure represents a single assignment of a HIT to
-- a Worker. The assignment tracks the Worker\'s efforts to complete the
-- HIT, and contains the results for later retrieval.
--
-- /See:/ 'newAssignment' smart constructor.
data Assignment = Assignment'
  { -- | The date and time the Worker accepted the assignment.
    acceptTime :: Prelude.Maybe Data.POSIX,
    -- | The Worker\'s answers submitted for the HIT contained in a
    -- QuestionFormAnswers document, if the Worker provides an answer. If the
    -- Worker does not provide any answers, Answer may contain a
    -- QuestionFormAnswers document, or Answer may be empty.
    answer :: Prelude.Maybe Prelude.Text,
    -- | If the Worker has submitted results and the Requester has approved the
    -- results, ApprovalTime is the date and time the Requester approved the
    -- results. This value is omitted from the assignment if the Requester has
    -- not yet approved the results.
    approvalTime :: Prelude.Maybe Data.POSIX,
    -- | A unique identifier for the assignment.
    assignmentId :: Prelude.Maybe Prelude.Text,
    -- | The status of the assignment.
    assignmentStatus :: Prelude.Maybe AssignmentStatus,
    -- | If results have been submitted, AutoApprovalTime is the date and time
    -- the results of the assignment results are considered Approved
    -- automatically if they have not already been explicitly approved or
    -- rejected by the Requester. This value is derived from the auto-approval
    -- delay specified by the Requester in the HIT. This value is omitted from
    -- the assignment if the Worker has not yet submitted results.
    autoApprovalTime :: Prelude.Maybe Data.POSIX,
    -- | The date and time of the deadline for the assignment. This value is
    -- derived from the deadline specification for the HIT and the date and
    -- time the Worker accepted the HIT.
    deadline :: Prelude.Maybe Data.POSIX,
    -- | The ID of the HIT.
    hITId :: Prelude.Maybe Prelude.Text,
    -- | If the Worker has submitted results and the Requester has rejected the
    -- results, RejectionTime is the date and time the Requester rejected the
    -- results.
    rejectionTime :: Prelude.Maybe Data.POSIX,
    -- | The feedback string included with the call to the ApproveAssignment
    -- operation or the RejectAssignment operation, if the Requester approved
    -- or rejected the assignment and specified feedback.
    requesterFeedback :: Prelude.Maybe Prelude.Text,
    -- | If the Worker has submitted results, SubmitTime is the date and time the
    -- assignment was submitted. This value is omitted from the assignment if
    -- the Worker has not yet submitted results.
    submitTime :: Prelude.Maybe Data.POSIX,
    -- | The ID of the Worker who accepted the HIT.
    workerId :: Prelude.Maybe Prelude.Text
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
-- 'approvalTime', 'assignment_approvalTime' - If the Worker has submitted results and the Requester has approved the
-- results, ApprovalTime is the date and time the Requester approved the
-- results. This value is omitted from the assignment if the Requester has
-- not yet approved the results.
--
-- 'assignmentId', 'assignment_assignmentId' - A unique identifier for the assignment.
--
-- 'assignmentStatus', 'assignment_assignmentStatus' - The status of the assignment.
--
-- 'autoApprovalTime', 'assignment_autoApprovalTime' - If results have been submitted, AutoApprovalTime is the date and time
-- the results of the assignment results are considered Approved
-- automatically if they have not already been explicitly approved or
-- rejected by the Requester. This value is derived from the auto-approval
-- delay specified by the Requester in the HIT. This value is omitted from
-- the assignment if the Worker has not yet submitted results.
--
-- 'deadline', 'assignment_deadline' - The date and time of the deadline for the assignment. This value is
-- derived from the deadline specification for the HIT and the date and
-- time the Worker accepted the HIT.
--
-- 'hITId', 'assignment_hITId' - The ID of the HIT.
--
-- 'rejectionTime', 'assignment_rejectionTime' - If the Worker has submitted results and the Requester has rejected the
-- results, RejectionTime is the date and time the Requester rejected the
-- results.
--
-- 'requesterFeedback', 'assignment_requesterFeedback' - The feedback string included with the call to the ApproveAssignment
-- operation or the RejectAssignment operation, if the Requester approved
-- or rejected the assignment and specified feedback.
--
-- 'submitTime', 'assignment_submitTime' - If the Worker has submitted results, SubmitTime is the date and time the
-- assignment was submitted. This value is omitted from the assignment if
-- the Worker has not yet submitted results.
--
-- 'workerId', 'assignment_workerId' - The ID of the Worker who accepted the HIT.
newAssignment ::
  Assignment
newAssignment =
  Assignment'
    { acceptTime = Prelude.Nothing,
      answer = Prelude.Nothing,
      approvalTime = Prelude.Nothing,
      assignmentId = Prelude.Nothing,
      assignmentStatus = Prelude.Nothing,
      autoApprovalTime = Prelude.Nothing,
      deadline = Prelude.Nothing,
      hITId = Prelude.Nothing,
      rejectionTime = Prelude.Nothing,
      requesterFeedback = Prelude.Nothing,
      submitTime = Prelude.Nothing,
      workerId = Prelude.Nothing
    }

-- | The date and time the Worker accepted the assignment.
assignment_acceptTime :: Lens.Lens' Assignment (Prelude.Maybe Prelude.UTCTime)
assignment_acceptTime = Lens.lens (\Assignment' {acceptTime} -> acceptTime) (\s@Assignment' {} a -> s {acceptTime = a} :: Assignment) Prelude.. Lens.mapping Data._Time

-- | The Worker\'s answers submitted for the HIT contained in a
-- QuestionFormAnswers document, if the Worker provides an answer. If the
-- Worker does not provide any answers, Answer may contain a
-- QuestionFormAnswers document, or Answer may be empty.
assignment_answer :: Lens.Lens' Assignment (Prelude.Maybe Prelude.Text)
assignment_answer = Lens.lens (\Assignment' {answer} -> answer) (\s@Assignment' {} a -> s {answer = a} :: Assignment)

-- | If the Worker has submitted results and the Requester has approved the
-- results, ApprovalTime is the date and time the Requester approved the
-- results. This value is omitted from the assignment if the Requester has
-- not yet approved the results.
assignment_approvalTime :: Lens.Lens' Assignment (Prelude.Maybe Prelude.UTCTime)
assignment_approvalTime = Lens.lens (\Assignment' {approvalTime} -> approvalTime) (\s@Assignment' {} a -> s {approvalTime = a} :: Assignment) Prelude.. Lens.mapping Data._Time

-- | A unique identifier for the assignment.
assignment_assignmentId :: Lens.Lens' Assignment (Prelude.Maybe Prelude.Text)
assignment_assignmentId = Lens.lens (\Assignment' {assignmentId} -> assignmentId) (\s@Assignment' {} a -> s {assignmentId = a} :: Assignment)

-- | The status of the assignment.
assignment_assignmentStatus :: Lens.Lens' Assignment (Prelude.Maybe AssignmentStatus)
assignment_assignmentStatus = Lens.lens (\Assignment' {assignmentStatus} -> assignmentStatus) (\s@Assignment' {} a -> s {assignmentStatus = a} :: Assignment)

-- | If results have been submitted, AutoApprovalTime is the date and time
-- the results of the assignment results are considered Approved
-- automatically if they have not already been explicitly approved or
-- rejected by the Requester. This value is derived from the auto-approval
-- delay specified by the Requester in the HIT. This value is omitted from
-- the assignment if the Worker has not yet submitted results.
assignment_autoApprovalTime :: Lens.Lens' Assignment (Prelude.Maybe Prelude.UTCTime)
assignment_autoApprovalTime = Lens.lens (\Assignment' {autoApprovalTime} -> autoApprovalTime) (\s@Assignment' {} a -> s {autoApprovalTime = a} :: Assignment) Prelude.. Lens.mapping Data._Time

-- | The date and time of the deadline for the assignment. This value is
-- derived from the deadline specification for the HIT and the date and
-- time the Worker accepted the HIT.
assignment_deadline :: Lens.Lens' Assignment (Prelude.Maybe Prelude.UTCTime)
assignment_deadline = Lens.lens (\Assignment' {deadline} -> deadline) (\s@Assignment' {} a -> s {deadline = a} :: Assignment) Prelude.. Lens.mapping Data._Time

-- | The ID of the HIT.
assignment_hITId :: Lens.Lens' Assignment (Prelude.Maybe Prelude.Text)
assignment_hITId = Lens.lens (\Assignment' {hITId} -> hITId) (\s@Assignment' {} a -> s {hITId = a} :: Assignment)

-- | If the Worker has submitted results and the Requester has rejected the
-- results, RejectionTime is the date and time the Requester rejected the
-- results.
assignment_rejectionTime :: Lens.Lens' Assignment (Prelude.Maybe Prelude.UTCTime)
assignment_rejectionTime = Lens.lens (\Assignment' {rejectionTime} -> rejectionTime) (\s@Assignment' {} a -> s {rejectionTime = a} :: Assignment) Prelude.. Lens.mapping Data._Time

-- | The feedback string included with the call to the ApproveAssignment
-- operation or the RejectAssignment operation, if the Requester approved
-- or rejected the assignment and specified feedback.
assignment_requesterFeedback :: Lens.Lens' Assignment (Prelude.Maybe Prelude.Text)
assignment_requesterFeedback = Lens.lens (\Assignment' {requesterFeedback} -> requesterFeedback) (\s@Assignment' {} a -> s {requesterFeedback = a} :: Assignment)

-- | If the Worker has submitted results, SubmitTime is the date and time the
-- assignment was submitted. This value is omitted from the assignment if
-- the Worker has not yet submitted results.
assignment_submitTime :: Lens.Lens' Assignment (Prelude.Maybe Prelude.UTCTime)
assignment_submitTime = Lens.lens (\Assignment' {submitTime} -> submitTime) (\s@Assignment' {} a -> s {submitTime = a} :: Assignment) Prelude.. Lens.mapping Data._Time

-- | The ID of the Worker who accepted the HIT.
assignment_workerId :: Lens.Lens' Assignment (Prelude.Maybe Prelude.Text)
assignment_workerId = Lens.lens (\Assignment' {workerId} -> workerId) (\s@Assignment' {} a -> s {workerId = a} :: Assignment)

instance Data.FromJSON Assignment where
  parseJSON =
    Data.withObject
      "Assignment"
      ( \x ->
          Assignment'
            Prelude.<$> (x Data..:? "AcceptTime")
            Prelude.<*> (x Data..:? "Answer")
            Prelude.<*> (x Data..:? "ApprovalTime")
            Prelude.<*> (x Data..:? "AssignmentId")
            Prelude.<*> (x Data..:? "AssignmentStatus")
            Prelude.<*> (x Data..:? "AutoApprovalTime")
            Prelude.<*> (x Data..:? "Deadline")
            Prelude.<*> (x Data..:? "HITId")
            Prelude.<*> (x Data..:? "RejectionTime")
            Prelude.<*> (x Data..:? "RequesterFeedback")
            Prelude.<*> (x Data..:? "SubmitTime")
            Prelude.<*> (x Data..:? "WorkerId")
      )

instance Prelude.Hashable Assignment where
  hashWithSalt _salt Assignment' {..} =
    _salt
      `Prelude.hashWithSalt` acceptTime
      `Prelude.hashWithSalt` answer
      `Prelude.hashWithSalt` approvalTime
      `Prelude.hashWithSalt` assignmentId
      `Prelude.hashWithSalt` assignmentStatus
      `Prelude.hashWithSalt` autoApprovalTime
      `Prelude.hashWithSalt` deadline
      `Prelude.hashWithSalt` hITId
      `Prelude.hashWithSalt` rejectionTime
      `Prelude.hashWithSalt` requesterFeedback
      `Prelude.hashWithSalt` submitTime
      `Prelude.hashWithSalt` workerId

instance Prelude.NFData Assignment where
  rnf Assignment' {..} =
    Prelude.rnf acceptTime
      `Prelude.seq` Prelude.rnf answer
      `Prelude.seq` Prelude.rnf approvalTime
      `Prelude.seq` Prelude.rnf assignmentId
      `Prelude.seq` Prelude.rnf assignmentStatus
      `Prelude.seq` Prelude.rnf autoApprovalTime
      `Prelude.seq` Prelude.rnf deadline
      `Prelude.seq` Prelude.rnf hITId
      `Prelude.seq` Prelude.rnf rejectionTime
      `Prelude.seq` Prelude.rnf requesterFeedback
      `Prelude.seq` Prelude.rnf submitTime
      `Prelude.seq` Prelude.rnf workerId
