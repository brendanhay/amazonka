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
-- Module      : Amazonka.MechanicalTurk.Types.HIT
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MechanicalTurk.Types.HIT where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MechanicalTurk.Types.HITReviewStatus
import Amazonka.MechanicalTurk.Types.HITStatus
import Amazonka.MechanicalTurk.Types.QualificationRequirement
import qualified Amazonka.Prelude as Prelude

-- | The HIT data structure represents a single HIT, including all the
-- information necessary for a Worker to accept and complete the HIT.
--
-- /See:/ 'newHIT' smart constructor.
data HIT = HIT'
  { -- | The length of time, in seconds, that a Worker has to complete the HIT
    -- after accepting it.
    assignmentDurationInSeconds :: Prelude.Maybe Prelude.Integer,
    -- | The amount of time, in seconds, after the Worker submits an assignment
    -- for the HIT that the results are automatically approved by Amazon
    -- Mechanical Turk. This is the amount of time the Requester has to reject
    -- an assignment submitted by a Worker before the assignment is
    -- auto-approved and the Worker is paid.
    autoApprovalDelayInSeconds :: Prelude.Maybe Prelude.Integer,
    -- | The date and time the HIT was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | A general description of the HIT.
    description :: Prelude.Maybe Prelude.Text,
    -- | The date and time the HIT expires.
    expiration :: Prelude.Maybe Data.POSIX,
    -- | The ID of the HIT Group of this HIT.
    hITGroupId :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the HIT.
    hITId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the HIT Layout of this HIT.
    hITLayoutId :: Prelude.Maybe Prelude.Text,
    -- | Indicates the review status of the HIT. Valid Values are NotReviewed |
    -- MarkedForReview | ReviewedAppropriate | ReviewedInappropriate.
    hITReviewStatus :: Prelude.Maybe HITReviewStatus,
    -- | The status of the HIT and its assignments. Valid Values are Assignable |
    -- Unassignable | Reviewable | Reviewing | Disposed.
    hITStatus :: Prelude.Maybe HITStatus,
    -- | The ID of the HIT type of this HIT
    hITTypeId :: Prelude.Maybe Prelude.Text,
    -- | One or more words or phrases that describe the HIT, separated by commas.
    -- Search terms similar to the keywords of a HIT are more likely to have
    -- the HIT in the search results.
    keywords :: Prelude.Maybe Prelude.Text,
    -- | The number of times the HIT can be accepted and completed before the HIT
    -- becomes unavailable.
    maxAssignments :: Prelude.Maybe Prelude.Int,
    -- | The number of assignments for this HIT that are available for Workers to
    -- accept.
    numberOfAssignmentsAvailable :: Prelude.Maybe Prelude.Int,
    -- | The number of assignments for this HIT that have been approved or
    -- rejected.
    numberOfAssignmentsCompleted :: Prelude.Maybe Prelude.Int,
    -- | The number of assignments for this HIT that are being previewed or have
    -- been accepted by Workers, but have not yet been submitted, returned, or
    -- abandoned.
    numberOfAssignmentsPending :: Prelude.Maybe Prelude.Int,
    -- | Conditions that a Worker\'s Qualifications must meet in order to accept
    -- the HIT. A HIT can have between zero and ten Qualification requirements.
    -- All requirements must be met in order for a Worker to accept the HIT.
    -- Additionally, other actions can be restricted using the @ActionsGuarded@
    -- field on each @QualificationRequirement@ structure.
    qualificationRequirements :: Prelude.Maybe [QualificationRequirement],
    -- | The data the Worker completing the HIT uses produce the results. This is
    -- either either a QuestionForm, HTMLQuestion or an ExternalQuestion data
    -- structure.
    question :: Prelude.Maybe Prelude.Text,
    -- | An arbitrary data field the Requester who created the HIT can use. This
    -- field is visible only to the creator of the HIT.
    requesterAnnotation :: Prelude.Maybe Prelude.Text,
    reward :: Prelude.Maybe Prelude.Text,
    -- | The title of the HIT.
    title :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HIT' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assignmentDurationInSeconds', 'hit_assignmentDurationInSeconds' - The length of time, in seconds, that a Worker has to complete the HIT
-- after accepting it.
--
-- 'autoApprovalDelayInSeconds', 'hit_autoApprovalDelayInSeconds' - The amount of time, in seconds, after the Worker submits an assignment
-- for the HIT that the results are automatically approved by Amazon
-- Mechanical Turk. This is the amount of time the Requester has to reject
-- an assignment submitted by a Worker before the assignment is
-- auto-approved and the Worker is paid.
--
-- 'creationTime', 'hit_creationTime' - The date and time the HIT was created.
--
-- 'description', 'hit_description' - A general description of the HIT.
--
-- 'expiration', 'hit_expiration' - The date and time the HIT expires.
--
-- 'hITGroupId', 'hit_hITGroupId' - The ID of the HIT Group of this HIT.
--
-- 'hITId', 'hit_hITId' - A unique identifier for the HIT.
--
-- 'hITLayoutId', 'hit_hITLayoutId' - The ID of the HIT Layout of this HIT.
--
-- 'hITReviewStatus', 'hit_hITReviewStatus' - Indicates the review status of the HIT. Valid Values are NotReviewed |
-- MarkedForReview | ReviewedAppropriate | ReviewedInappropriate.
--
-- 'hITStatus', 'hit_hITStatus' - The status of the HIT and its assignments. Valid Values are Assignable |
-- Unassignable | Reviewable | Reviewing | Disposed.
--
-- 'hITTypeId', 'hit_hITTypeId' - The ID of the HIT type of this HIT
--
-- 'keywords', 'hit_keywords' - One or more words or phrases that describe the HIT, separated by commas.
-- Search terms similar to the keywords of a HIT are more likely to have
-- the HIT in the search results.
--
-- 'maxAssignments', 'hit_maxAssignments' - The number of times the HIT can be accepted and completed before the HIT
-- becomes unavailable.
--
-- 'numberOfAssignmentsAvailable', 'hit_numberOfAssignmentsAvailable' - The number of assignments for this HIT that are available for Workers to
-- accept.
--
-- 'numberOfAssignmentsCompleted', 'hit_numberOfAssignmentsCompleted' - The number of assignments for this HIT that have been approved or
-- rejected.
--
-- 'numberOfAssignmentsPending', 'hit_numberOfAssignmentsPending' - The number of assignments for this HIT that are being previewed or have
-- been accepted by Workers, but have not yet been submitted, returned, or
-- abandoned.
--
-- 'qualificationRequirements', 'hit_qualificationRequirements' - Conditions that a Worker\'s Qualifications must meet in order to accept
-- the HIT. A HIT can have between zero and ten Qualification requirements.
-- All requirements must be met in order for a Worker to accept the HIT.
-- Additionally, other actions can be restricted using the @ActionsGuarded@
-- field on each @QualificationRequirement@ structure.
--
-- 'question', 'hit_question' - The data the Worker completing the HIT uses produce the results. This is
-- either either a QuestionForm, HTMLQuestion or an ExternalQuestion data
-- structure.
--
-- 'requesterAnnotation', 'hit_requesterAnnotation' - An arbitrary data field the Requester who created the HIT can use. This
-- field is visible only to the creator of the HIT.
--
-- 'reward', 'hit_reward' - Undocumented member.
--
-- 'title', 'hit_title' - The title of the HIT.
newHIT ::
  HIT
newHIT =
  HIT'
    { assignmentDurationInSeconds = Prelude.Nothing,
      autoApprovalDelayInSeconds = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      description = Prelude.Nothing,
      expiration = Prelude.Nothing,
      hITGroupId = Prelude.Nothing,
      hITId = Prelude.Nothing,
      hITLayoutId = Prelude.Nothing,
      hITReviewStatus = Prelude.Nothing,
      hITStatus = Prelude.Nothing,
      hITTypeId = Prelude.Nothing,
      keywords = Prelude.Nothing,
      maxAssignments = Prelude.Nothing,
      numberOfAssignmentsAvailable = Prelude.Nothing,
      numberOfAssignmentsCompleted = Prelude.Nothing,
      numberOfAssignmentsPending = Prelude.Nothing,
      qualificationRequirements = Prelude.Nothing,
      question = Prelude.Nothing,
      requesterAnnotation = Prelude.Nothing,
      reward = Prelude.Nothing,
      title = Prelude.Nothing
    }

-- | The length of time, in seconds, that a Worker has to complete the HIT
-- after accepting it.
hit_assignmentDurationInSeconds :: Lens.Lens' HIT (Prelude.Maybe Prelude.Integer)
hit_assignmentDurationInSeconds = Lens.lens (\HIT' {assignmentDurationInSeconds} -> assignmentDurationInSeconds) (\s@HIT' {} a -> s {assignmentDurationInSeconds = a} :: HIT)

-- | The amount of time, in seconds, after the Worker submits an assignment
-- for the HIT that the results are automatically approved by Amazon
-- Mechanical Turk. This is the amount of time the Requester has to reject
-- an assignment submitted by a Worker before the assignment is
-- auto-approved and the Worker is paid.
hit_autoApprovalDelayInSeconds :: Lens.Lens' HIT (Prelude.Maybe Prelude.Integer)
hit_autoApprovalDelayInSeconds = Lens.lens (\HIT' {autoApprovalDelayInSeconds} -> autoApprovalDelayInSeconds) (\s@HIT' {} a -> s {autoApprovalDelayInSeconds = a} :: HIT)

-- | The date and time the HIT was created.
hit_creationTime :: Lens.Lens' HIT (Prelude.Maybe Prelude.UTCTime)
hit_creationTime = Lens.lens (\HIT' {creationTime} -> creationTime) (\s@HIT' {} a -> s {creationTime = a} :: HIT) Prelude.. Lens.mapping Data._Time

-- | A general description of the HIT.
hit_description :: Lens.Lens' HIT (Prelude.Maybe Prelude.Text)
hit_description = Lens.lens (\HIT' {description} -> description) (\s@HIT' {} a -> s {description = a} :: HIT)

-- | The date and time the HIT expires.
hit_expiration :: Lens.Lens' HIT (Prelude.Maybe Prelude.UTCTime)
hit_expiration = Lens.lens (\HIT' {expiration} -> expiration) (\s@HIT' {} a -> s {expiration = a} :: HIT) Prelude.. Lens.mapping Data._Time

-- | The ID of the HIT Group of this HIT.
hit_hITGroupId :: Lens.Lens' HIT (Prelude.Maybe Prelude.Text)
hit_hITGroupId = Lens.lens (\HIT' {hITGroupId} -> hITGroupId) (\s@HIT' {} a -> s {hITGroupId = a} :: HIT)

-- | A unique identifier for the HIT.
hit_hITId :: Lens.Lens' HIT (Prelude.Maybe Prelude.Text)
hit_hITId = Lens.lens (\HIT' {hITId} -> hITId) (\s@HIT' {} a -> s {hITId = a} :: HIT)

-- | The ID of the HIT Layout of this HIT.
hit_hITLayoutId :: Lens.Lens' HIT (Prelude.Maybe Prelude.Text)
hit_hITLayoutId = Lens.lens (\HIT' {hITLayoutId} -> hITLayoutId) (\s@HIT' {} a -> s {hITLayoutId = a} :: HIT)

-- | Indicates the review status of the HIT. Valid Values are NotReviewed |
-- MarkedForReview | ReviewedAppropriate | ReviewedInappropriate.
hit_hITReviewStatus :: Lens.Lens' HIT (Prelude.Maybe HITReviewStatus)
hit_hITReviewStatus = Lens.lens (\HIT' {hITReviewStatus} -> hITReviewStatus) (\s@HIT' {} a -> s {hITReviewStatus = a} :: HIT)

-- | The status of the HIT and its assignments. Valid Values are Assignable |
-- Unassignable | Reviewable | Reviewing | Disposed.
hit_hITStatus :: Lens.Lens' HIT (Prelude.Maybe HITStatus)
hit_hITStatus = Lens.lens (\HIT' {hITStatus} -> hITStatus) (\s@HIT' {} a -> s {hITStatus = a} :: HIT)

-- | The ID of the HIT type of this HIT
hit_hITTypeId :: Lens.Lens' HIT (Prelude.Maybe Prelude.Text)
hit_hITTypeId = Lens.lens (\HIT' {hITTypeId} -> hITTypeId) (\s@HIT' {} a -> s {hITTypeId = a} :: HIT)

-- | One or more words or phrases that describe the HIT, separated by commas.
-- Search terms similar to the keywords of a HIT are more likely to have
-- the HIT in the search results.
hit_keywords :: Lens.Lens' HIT (Prelude.Maybe Prelude.Text)
hit_keywords = Lens.lens (\HIT' {keywords} -> keywords) (\s@HIT' {} a -> s {keywords = a} :: HIT)

-- | The number of times the HIT can be accepted and completed before the HIT
-- becomes unavailable.
hit_maxAssignments :: Lens.Lens' HIT (Prelude.Maybe Prelude.Int)
hit_maxAssignments = Lens.lens (\HIT' {maxAssignments} -> maxAssignments) (\s@HIT' {} a -> s {maxAssignments = a} :: HIT)

-- | The number of assignments for this HIT that are available for Workers to
-- accept.
hit_numberOfAssignmentsAvailable :: Lens.Lens' HIT (Prelude.Maybe Prelude.Int)
hit_numberOfAssignmentsAvailable = Lens.lens (\HIT' {numberOfAssignmentsAvailable} -> numberOfAssignmentsAvailable) (\s@HIT' {} a -> s {numberOfAssignmentsAvailable = a} :: HIT)

-- | The number of assignments for this HIT that have been approved or
-- rejected.
hit_numberOfAssignmentsCompleted :: Lens.Lens' HIT (Prelude.Maybe Prelude.Int)
hit_numberOfAssignmentsCompleted = Lens.lens (\HIT' {numberOfAssignmentsCompleted} -> numberOfAssignmentsCompleted) (\s@HIT' {} a -> s {numberOfAssignmentsCompleted = a} :: HIT)

-- | The number of assignments for this HIT that are being previewed or have
-- been accepted by Workers, but have not yet been submitted, returned, or
-- abandoned.
hit_numberOfAssignmentsPending :: Lens.Lens' HIT (Prelude.Maybe Prelude.Int)
hit_numberOfAssignmentsPending = Lens.lens (\HIT' {numberOfAssignmentsPending} -> numberOfAssignmentsPending) (\s@HIT' {} a -> s {numberOfAssignmentsPending = a} :: HIT)

-- | Conditions that a Worker\'s Qualifications must meet in order to accept
-- the HIT. A HIT can have between zero and ten Qualification requirements.
-- All requirements must be met in order for a Worker to accept the HIT.
-- Additionally, other actions can be restricted using the @ActionsGuarded@
-- field on each @QualificationRequirement@ structure.
hit_qualificationRequirements :: Lens.Lens' HIT (Prelude.Maybe [QualificationRequirement])
hit_qualificationRequirements = Lens.lens (\HIT' {qualificationRequirements} -> qualificationRequirements) (\s@HIT' {} a -> s {qualificationRequirements = a} :: HIT) Prelude.. Lens.mapping Lens.coerced

-- | The data the Worker completing the HIT uses produce the results. This is
-- either either a QuestionForm, HTMLQuestion or an ExternalQuestion data
-- structure.
hit_question :: Lens.Lens' HIT (Prelude.Maybe Prelude.Text)
hit_question = Lens.lens (\HIT' {question} -> question) (\s@HIT' {} a -> s {question = a} :: HIT)

-- | An arbitrary data field the Requester who created the HIT can use. This
-- field is visible only to the creator of the HIT.
hit_requesterAnnotation :: Lens.Lens' HIT (Prelude.Maybe Prelude.Text)
hit_requesterAnnotation = Lens.lens (\HIT' {requesterAnnotation} -> requesterAnnotation) (\s@HIT' {} a -> s {requesterAnnotation = a} :: HIT)

-- | Undocumented member.
hit_reward :: Lens.Lens' HIT (Prelude.Maybe Prelude.Text)
hit_reward = Lens.lens (\HIT' {reward} -> reward) (\s@HIT' {} a -> s {reward = a} :: HIT)

-- | The title of the HIT.
hit_title :: Lens.Lens' HIT (Prelude.Maybe Prelude.Text)
hit_title = Lens.lens (\HIT' {title} -> title) (\s@HIT' {} a -> s {title = a} :: HIT)

instance Data.FromJSON HIT where
  parseJSON =
    Data.withObject
      "HIT"
      ( \x ->
          HIT'
            Prelude.<$> (x Data..:? "AssignmentDurationInSeconds")
            Prelude.<*> (x Data..:? "AutoApprovalDelayInSeconds")
            Prelude.<*> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "Expiration")
            Prelude.<*> (x Data..:? "HITGroupId")
            Prelude.<*> (x Data..:? "HITId")
            Prelude.<*> (x Data..:? "HITLayoutId")
            Prelude.<*> (x Data..:? "HITReviewStatus")
            Prelude.<*> (x Data..:? "HITStatus")
            Prelude.<*> (x Data..:? "HITTypeId")
            Prelude.<*> (x Data..:? "Keywords")
            Prelude.<*> (x Data..:? "MaxAssignments")
            Prelude.<*> (x Data..:? "NumberOfAssignmentsAvailable")
            Prelude.<*> (x Data..:? "NumberOfAssignmentsCompleted")
            Prelude.<*> (x Data..:? "NumberOfAssignmentsPending")
            Prelude.<*> ( x
                            Data..:? "QualificationRequirements"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Question")
            Prelude.<*> (x Data..:? "RequesterAnnotation")
            Prelude.<*> (x Data..:? "Reward")
            Prelude.<*> (x Data..:? "Title")
      )

instance Prelude.Hashable HIT where
  hashWithSalt _salt HIT' {..} =
    _salt
      `Prelude.hashWithSalt` assignmentDurationInSeconds
      `Prelude.hashWithSalt` autoApprovalDelayInSeconds
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` expiration
      `Prelude.hashWithSalt` hITGroupId
      `Prelude.hashWithSalt` hITId
      `Prelude.hashWithSalt` hITLayoutId
      `Prelude.hashWithSalt` hITReviewStatus
      `Prelude.hashWithSalt` hITStatus
      `Prelude.hashWithSalt` hITTypeId
      `Prelude.hashWithSalt` keywords
      `Prelude.hashWithSalt` maxAssignments
      `Prelude.hashWithSalt` numberOfAssignmentsAvailable
      `Prelude.hashWithSalt` numberOfAssignmentsCompleted
      `Prelude.hashWithSalt` numberOfAssignmentsPending
      `Prelude.hashWithSalt` qualificationRequirements
      `Prelude.hashWithSalt` question
      `Prelude.hashWithSalt` requesterAnnotation
      `Prelude.hashWithSalt` reward
      `Prelude.hashWithSalt` title

instance Prelude.NFData HIT where
  rnf HIT' {..} =
    Prelude.rnf assignmentDurationInSeconds
      `Prelude.seq` Prelude.rnf autoApprovalDelayInSeconds
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf expiration
      `Prelude.seq` Prelude.rnf hITGroupId
      `Prelude.seq` Prelude.rnf hITId
      `Prelude.seq` Prelude.rnf hITLayoutId
      `Prelude.seq` Prelude.rnf hITReviewStatus
      `Prelude.seq` Prelude.rnf hITStatus
      `Prelude.seq` Prelude.rnf hITTypeId
      `Prelude.seq` Prelude.rnf keywords
      `Prelude.seq` Prelude.rnf maxAssignments
      `Prelude.seq` Prelude.rnf numberOfAssignmentsAvailable
      `Prelude.seq` Prelude.rnf numberOfAssignmentsCompleted
      `Prelude.seq` Prelude.rnf numberOfAssignmentsPending
      `Prelude.seq` Prelude.rnf
        qualificationRequirements
      `Prelude.seq` Prelude.rnf question
      `Prelude.seq` Prelude.rnf requesterAnnotation
      `Prelude.seq` Prelude.rnf reward
      `Prelude.seq` Prelude.rnf title
