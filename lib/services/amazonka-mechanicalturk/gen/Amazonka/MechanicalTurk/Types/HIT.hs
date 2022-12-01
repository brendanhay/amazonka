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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MechanicalTurk.Types.HIT where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MechanicalTurk.Types.HITReviewStatus
import Amazonka.MechanicalTurk.Types.HITStatus
import Amazonka.MechanicalTurk.Types.QualificationRequirement
import qualified Amazonka.Prelude as Prelude

-- | The HIT data structure represents a single HIT, including all the
-- information necessary for a Worker to accept and complete the HIT.
--
-- /See:/ 'newHIT' smart constructor.
data HIT = HIT'
  { -- | The ID of the HIT Group of this HIT.
    hITGroupId :: Prelude.Maybe Prelude.Text,
    -- | The date and time the HIT expires.
    expiration :: Prelude.Maybe Core.POSIX,
    -- | The number of assignments for this HIT that are available for Workers to
    -- accept.
    numberOfAssignmentsAvailable :: Prelude.Maybe Prelude.Int,
    -- | The status of the HIT and its assignments. Valid Values are Assignable |
    -- Unassignable | Reviewable | Reviewing | Disposed.
    hITStatus :: Prelude.Maybe HITStatus,
    -- | The number of times the HIT can be accepted and completed before the HIT
    -- becomes unavailable.
    maxAssignments :: Prelude.Maybe Prelude.Int,
    -- | One or more words or phrases that describe the HIT, separated by commas.
    -- Search terms similar to the keywords of a HIT are more likely to have
    -- the HIT in the search results.
    keywords :: Prelude.Maybe Prelude.Text,
    -- | The number of assignments for this HIT that are being previewed or have
    -- been accepted by Workers, but have not yet been submitted, returned, or
    -- abandoned.
    numberOfAssignmentsPending :: Prelude.Maybe Prelude.Int,
    -- | A general description of the HIT.
    description :: Prelude.Maybe Prelude.Text,
    -- | The length of time, in seconds, that a Worker has to complete the HIT
    -- after accepting it.
    assignmentDurationInSeconds :: Prelude.Maybe Prelude.Integer,
    -- | The data the Worker completing the HIT uses produce the results. This is
    -- either either a QuestionForm, HTMLQuestion or an ExternalQuestion data
    -- structure.
    question :: Prelude.Maybe Prelude.Text,
    -- | The title of the HIT.
    title :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the HIT.
    hITId :: Prelude.Maybe Prelude.Text,
    -- | The amount of time, in seconds, after the Worker submits an assignment
    -- for the HIT that the results are automatically approved by Amazon
    -- Mechanical Turk. This is the amount of time the Requester has to reject
    -- an assignment submitted by a Worker before the assignment is
    -- auto-approved and the Worker is paid.
    autoApprovalDelayInSeconds :: Prelude.Maybe Prelude.Integer,
    reward :: Prelude.Maybe Prelude.Text,
    -- | Conditions that a Worker\'s Qualifications must meet in order to accept
    -- the HIT. A HIT can have between zero and ten Qualification requirements.
    -- All requirements must be met in order for a Worker to accept the HIT.
    -- Additionally, other actions can be restricted using the @ActionsGuarded@
    -- field on each @QualificationRequirement@ structure.
    qualificationRequirements :: Prelude.Maybe [QualificationRequirement],
    -- | The ID of the HIT type of this HIT
    hITTypeId :: Prelude.Maybe Prelude.Text,
    -- | The date and time the HIT was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | Indicates the review status of the HIT. Valid Values are NotReviewed |
    -- MarkedForReview | ReviewedAppropriate | ReviewedInappropriate.
    hITReviewStatus :: Prelude.Maybe HITReviewStatus,
    -- | The ID of the HIT Layout of this HIT.
    hITLayoutId :: Prelude.Maybe Prelude.Text,
    -- | The number of assignments for this HIT that have been approved or
    -- rejected.
    numberOfAssignmentsCompleted :: Prelude.Maybe Prelude.Int,
    -- | An arbitrary data field the Requester who created the HIT can use. This
    -- field is visible only to the creator of the HIT.
    requesterAnnotation :: Prelude.Maybe Prelude.Text
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
-- 'hITGroupId', 'hit_hITGroupId' - The ID of the HIT Group of this HIT.
--
-- 'expiration', 'hit_expiration' - The date and time the HIT expires.
--
-- 'numberOfAssignmentsAvailable', 'hit_numberOfAssignmentsAvailable' - The number of assignments for this HIT that are available for Workers to
-- accept.
--
-- 'hITStatus', 'hit_hITStatus' - The status of the HIT and its assignments. Valid Values are Assignable |
-- Unassignable | Reviewable | Reviewing | Disposed.
--
-- 'maxAssignments', 'hit_maxAssignments' - The number of times the HIT can be accepted and completed before the HIT
-- becomes unavailable.
--
-- 'keywords', 'hit_keywords' - One or more words or phrases that describe the HIT, separated by commas.
-- Search terms similar to the keywords of a HIT are more likely to have
-- the HIT in the search results.
--
-- 'numberOfAssignmentsPending', 'hit_numberOfAssignmentsPending' - The number of assignments for this HIT that are being previewed or have
-- been accepted by Workers, but have not yet been submitted, returned, or
-- abandoned.
--
-- 'description', 'hit_description' - A general description of the HIT.
--
-- 'assignmentDurationInSeconds', 'hit_assignmentDurationInSeconds' - The length of time, in seconds, that a Worker has to complete the HIT
-- after accepting it.
--
-- 'question', 'hit_question' - The data the Worker completing the HIT uses produce the results. This is
-- either either a QuestionForm, HTMLQuestion or an ExternalQuestion data
-- structure.
--
-- 'title', 'hit_title' - The title of the HIT.
--
-- 'hITId', 'hit_hITId' - A unique identifier for the HIT.
--
-- 'autoApprovalDelayInSeconds', 'hit_autoApprovalDelayInSeconds' - The amount of time, in seconds, after the Worker submits an assignment
-- for the HIT that the results are automatically approved by Amazon
-- Mechanical Turk. This is the amount of time the Requester has to reject
-- an assignment submitted by a Worker before the assignment is
-- auto-approved and the Worker is paid.
--
-- 'reward', 'hit_reward' - Undocumented member.
--
-- 'qualificationRequirements', 'hit_qualificationRequirements' - Conditions that a Worker\'s Qualifications must meet in order to accept
-- the HIT. A HIT can have between zero and ten Qualification requirements.
-- All requirements must be met in order for a Worker to accept the HIT.
-- Additionally, other actions can be restricted using the @ActionsGuarded@
-- field on each @QualificationRequirement@ structure.
--
-- 'hITTypeId', 'hit_hITTypeId' - The ID of the HIT type of this HIT
--
-- 'creationTime', 'hit_creationTime' - The date and time the HIT was created.
--
-- 'hITReviewStatus', 'hit_hITReviewStatus' - Indicates the review status of the HIT. Valid Values are NotReviewed |
-- MarkedForReview | ReviewedAppropriate | ReviewedInappropriate.
--
-- 'hITLayoutId', 'hit_hITLayoutId' - The ID of the HIT Layout of this HIT.
--
-- 'numberOfAssignmentsCompleted', 'hit_numberOfAssignmentsCompleted' - The number of assignments for this HIT that have been approved or
-- rejected.
--
-- 'requesterAnnotation', 'hit_requesterAnnotation' - An arbitrary data field the Requester who created the HIT can use. This
-- field is visible only to the creator of the HIT.
newHIT ::
  HIT
newHIT =
  HIT'
    { hITGroupId = Prelude.Nothing,
      expiration = Prelude.Nothing,
      numberOfAssignmentsAvailable = Prelude.Nothing,
      hITStatus = Prelude.Nothing,
      maxAssignments = Prelude.Nothing,
      keywords = Prelude.Nothing,
      numberOfAssignmentsPending = Prelude.Nothing,
      description = Prelude.Nothing,
      assignmentDurationInSeconds = Prelude.Nothing,
      question = Prelude.Nothing,
      title = Prelude.Nothing,
      hITId = Prelude.Nothing,
      autoApprovalDelayInSeconds = Prelude.Nothing,
      reward = Prelude.Nothing,
      qualificationRequirements = Prelude.Nothing,
      hITTypeId = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      hITReviewStatus = Prelude.Nothing,
      hITLayoutId = Prelude.Nothing,
      numberOfAssignmentsCompleted = Prelude.Nothing,
      requesterAnnotation = Prelude.Nothing
    }

-- | The ID of the HIT Group of this HIT.
hit_hITGroupId :: Lens.Lens' HIT (Prelude.Maybe Prelude.Text)
hit_hITGroupId = Lens.lens (\HIT' {hITGroupId} -> hITGroupId) (\s@HIT' {} a -> s {hITGroupId = a} :: HIT)

-- | The date and time the HIT expires.
hit_expiration :: Lens.Lens' HIT (Prelude.Maybe Prelude.UTCTime)
hit_expiration = Lens.lens (\HIT' {expiration} -> expiration) (\s@HIT' {} a -> s {expiration = a} :: HIT) Prelude.. Lens.mapping Core._Time

-- | The number of assignments for this HIT that are available for Workers to
-- accept.
hit_numberOfAssignmentsAvailable :: Lens.Lens' HIT (Prelude.Maybe Prelude.Int)
hit_numberOfAssignmentsAvailable = Lens.lens (\HIT' {numberOfAssignmentsAvailable} -> numberOfAssignmentsAvailable) (\s@HIT' {} a -> s {numberOfAssignmentsAvailable = a} :: HIT)

-- | The status of the HIT and its assignments. Valid Values are Assignable |
-- Unassignable | Reviewable | Reviewing | Disposed.
hit_hITStatus :: Lens.Lens' HIT (Prelude.Maybe HITStatus)
hit_hITStatus = Lens.lens (\HIT' {hITStatus} -> hITStatus) (\s@HIT' {} a -> s {hITStatus = a} :: HIT)

-- | The number of times the HIT can be accepted and completed before the HIT
-- becomes unavailable.
hit_maxAssignments :: Lens.Lens' HIT (Prelude.Maybe Prelude.Int)
hit_maxAssignments = Lens.lens (\HIT' {maxAssignments} -> maxAssignments) (\s@HIT' {} a -> s {maxAssignments = a} :: HIT)

-- | One or more words or phrases that describe the HIT, separated by commas.
-- Search terms similar to the keywords of a HIT are more likely to have
-- the HIT in the search results.
hit_keywords :: Lens.Lens' HIT (Prelude.Maybe Prelude.Text)
hit_keywords = Lens.lens (\HIT' {keywords} -> keywords) (\s@HIT' {} a -> s {keywords = a} :: HIT)

-- | The number of assignments for this HIT that are being previewed or have
-- been accepted by Workers, but have not yet been submitted, returned, or
-- abandoned.
hit_numberOfAssignmentsPending :: Lens.Lens' HIT (Prelude.Maybe Prelude.Int)
hit_numberOfAssignmentsPending = Lens.lens (\HIT' {numberOfAssignmentsPending} -> numberOfAssignmentsPending) (\s@HIT' {} a -> s {numberOfAssignmentsPending = a} :: HIT)

-- | A general description of the HIT.
hit_description :: Lens.Lens' HIT (Prelude.Maybe Prelude.Text)
hit_description = Lens.lens (\HIT' {description} -> description) (\s@HIT' {} a -> s {description = a} :: HIT)

-- | The length of time, in seconds, that a Worker has to complete the HIT
-- after accepting it.
hit_assignmentDurationInSeconds :: Lens.Lens' HIT (Prelude.Maybe Prelude.Integer)
hit_assignmentDurationInSeconds = Lens.lens (\HIT' {assignmentDurationInSeconds} -> assignmentDurationInSeconds) (\s@HIT' {} a -> s {assignmentDurationInSeconds = a} :: HIT)

-- | The data the Worker completing the HIT uses produce the results. This is
-- either either a QuestionForm, HTMLQuestion or an ExternalQuestion data
-- structure.
hit_question :: Lens.Lens' HIT (Prelude.Maybe Prelude.Text)
hit_question = Lens.lens (\HIT' {question} -> question) (\s@HIT' {} a -> s {question = a} :: HIT)

-- | The title of the HIT.
hit_title :: Lens.Lens' HIT (Prelude.Maybe Prelude.Text)
hit_title = Lens.lens (\HIT' {title} -> title) (\s@HIT' {} a -> s {title = a} :: HIT)

-- | A unique identifier for the HIT.
hit_hITId :: Lens.Lens' HIT (Prelude.Maybe Prelude.Text)
hit_hITId = Lens.lens (\HIT' {hITId} -> hITId) (\s@HIT' {} a -> s {hITId = a} :: HIT)

-- | The amount of time, in seconds, after the Worker submits an assignment
-- for the HIT that the results are automatically approved by Amazon
-- Mechanical Turk. This is the amount of time the Requester has to reject
-- an assignment submitted by a Worker before the assignment is
-- auto-approved and the Worker is paid.
hit_autoApprovalDelayInSeconds :: Lens.Lens' HIT (Prelude.Maybe Prelude.Integer)
hit_autoApprovalDelayInSeconds = Lens.lens (\HIT' {autoApprovalDelayInSeconds} -> autoApprovalDelayInSeconds) (\s@HIT' {} a -> s {autoApprovalDelayInSeconds = a} :: HIT)

-- | Undocumented member.
hit_reward :: Lens.Lens' HIT (Prelude.Maybe Prelude.Text)
hit_reward = Lens.lens (\HIT' {reward} -> reward) (\s@HIT' {} a -> s {reward = a} :: HIT)

-- | Conditions that a Worker\'s Qualifications must meet in order to accept
-- the HIT. A HIT can have between zero and ten Qualification requirements.
-- All requirements must be met in order for a Worker to accept the HIT.
-- Additionally, other actions can be restricted using the @ActionsGuarded@
-- field on each @QualificationRequirement@ structure.
hit_qualificationRequirements :: Lens.Lens' HIT (Prelude.Maybe [QualificationRequirement])
hit_qualificationRequirements = Lens.lens (\HIT' {qualificationRequirements} -> qualificationRequirements) (\s@HIT' {} a -> s {qualificationRequirements = a} :: HIT) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the HIT type of this HIT
hit_hITTypeId :: Lens.Lens' HIT (Prelude.Maybe Prelude.Text)
hit_hITTypeId = Lens.lens (\HIT' {hITTypeId} -> hITTypeId) (\s@HIT' {} a -> s {hITTypeId = a} :: HIT)

-- | The date and time the HIT was created.
hit_creationTime :: Lens.Lens' HIT (Prelude.Maybe Prelude.UTCTime)
hit_creationTime = Lens.lens (\HIT' {creationTime} -> creationTime) (\s@HIT' {} a -> s {creationTime = a} :: HIT) Prelude.. Lens.mapping Core._Time

-- | Indicates the review status of the HIT. Valid Values are NotReviewed |
-- MarkedForReview | ReviewedAppropriate | ReviewedInappropriate.
hit_hITReviewStatus :: Lens.Lens' HIT (Prelude.Maybe HITReviewStatus)
hit_hITReviewStatus = Lens.lens (\HIT' {hITReviewStatus} -> hITReviewStatus) (\s@HIT' {} a -> s {hITReviewStatus = a} :: HIT)

-- | The ID of the HIT Layout of this HIT.
hit_hITLayoutId :: Lens.Lens' HIT (Prelude.Maybe Prelude.Text)
hit_hITLayoutId = Lens.lens (\HIT' {hITLayoutId} -> hITLayoutId) (\s@HIT' {} a -> s {hITLayoutId = a} :: HIT)

-- | The number of assignments for this HIT that have been approved or
-- rejected.
hit_numberOfAssignmentsCompleted :: Lens.Lens' HIT (Prelude.Maybe Prelude.Int)
hit_numberOfAssignmentsCompleted = Lens.lens (\HIT' {numberOfAssignmentsCompleted} -> numberOfAssignmentsCompleted) (\s@HIT' {} a -> s {numberOfAssignmentsCompleted = a} :: HIT)

-- | An arbitrary data field the Requester who created the HIT can use. This
-- field is visible only to the creator of the HIT.
hit_requesterAnnotation :: Lens.Lens' HIT (Prelude.Maybe Prelude.Text)
hit_requesterAnnotation = Lens.lens (\HIT' {requesterAnnotation} -> requesterAnnotation) (\s@HIT' {} a -> s {requesterAnnotation = a} :: HIT)

instance Core.FromJSON HIT where
  parseJSON =
    Core.withObject
      "HIT"
      ( \x ->
          HIT'
            Prelude.<$> (x Core..:? "HITGroupId")
            Prelude.<*> (x Core..:? "Expiration")
            Prelude.<*> (x Core..:? "NumberOfAssignmentsAvailable")
            Prelude.<*> (x Core..:? "HITStatus")
            Prelude.<*> (x Core..:? "MaxAssignments")
            Prelude.<*> (x Core..:? "Keywords")
            Prelude.<*> (x Core..:? "NumberOfAssignmentsPending")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "AssignmentDurationInSeconds")
            Prelude.<*> (x Core..:? "Question")
            Prelude.<*> (x Core..:? "Title")
            Prelude.<*> (x Core..:? "HITId")
            Prelude.<*> (x Core..:? "AutoApprovalDelayInSeconds")
            Prelude.<*> (x Core..:? "Reward")
            Prelude.<*> ( x Core..:? "QualificationRequirements"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "HITTypeId")
            Prelude.<*> (x Core..:? "CreationTime")
            Prelude.<*> (x Core..:? "HITReviewStatus")
            Prelude.<*> (x Core..:? "HITLayoutId")
            Prelude.<*> (x Core..:? "NumberOfAssignmentsCompleted")
            Prelude.<*> (x Core..:? "RequesterAnnotation")
      )

instance Prelude.Hashable HIT where
  hashWithSalt _salt HIT' {..} =
    _salt `Prelude.hashWithSalt` hITGroupId
      `Prelude.hashWithSalt` expiration
      `Prelude.hashWithSalt` numberOfAssignmentsAvailable
      `Prelude.hashWithSalt` hITStatus
      `Prelude.hashWithSalt` maxAssignments
      `Prelude.hashWithSalt` keywords
      `Prelude.hashWithSalt` numberOfAssignmentsPending
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` assignmentDurationInSeconds
      `Prelude.hashWithSalt` question
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` hITId
      `Prelude.hashWithSalt` autoApprovalDelayInSeconds
      `Prelude.hashWithSalt` reward
      `Prelude.hashWithSalt` qualificationRequirements
      `Prelude.hashWithSalt` hITTypeId
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` hITReviewStatus
      `Prelude.hashWithSalt` hITLayoutId
      `Prelude.hashWithSalt` numberOfAssignmentsCompleted
      `Prelude.hashWithSalt` requesterAnnotation

instance Prelude.NFData HIT where
  rnf HIT' {..} =
    Prelude.rnf hITGroupId
      `Prelude.seq` Prelude.rnf expiration
      `Prelude.seq` Prelude.rnf numberOfAssignmentsAvailable
      `Prelude.seq` Prelude.rnf hITStatus
      `Prelude.seq` Prelude.rnf maxAssignments
      `Prelude.seq` Prelude.rnf keywords
      `Prelude.seq` Prelude.rnf numberOfAssignmentsPending
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf assignmentDurationInSeconds
      `Prelude.seq` Prelude.rnf question
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf hITId
      `Prelude.seq` Prelude.rnf autoApprovalDelayInSeconds
      `Prelude.seq` Prelude.rnf reward
      `Prelude.seq` Prelude.rnf qualificationRequirements
      `Prelude.seq` Prelude.rnf hITTypeId
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf hITReviewStatus
      `Prelude.seq` Prelude.rnf hITLayoutId
      `Prelude.seq` Prelude.rnf
        numberOfAssignmentsCompleted
      `Prelude.seq` Prelude.rnf
        requesterAnnotation
