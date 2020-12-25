{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.HIT
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.HIT
  ( HIT (..),

    -- * Smart constructor
    mkHIT,

    -- * Lenses
    hitAssignmentDurationInSeconds,
    hitAutoApprovalDelayInSeconds,
    hitCreationTime,
    hitDescription,
    hitExpiration,
    hitHITGroupId,
    hitHITId,
    hitHITLayoutId,
    hitHITReviewStatus,
    hitHITStatus,
    hitHITTypeId,
    hitKeywords,
    hitMaxAssignments,
    hitNumberOfAssignmentsAvailable,
    hitNumberOfAssignmentsCompleted,
    hitNumberOfAssignmentsPending,
    hitQualificationRequirements,
    hitQuestion,
    hitRequesterAnnotation,
    hitReward,
    hitTitle,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MechanicalTurk.Types.HITGroupId as Types
import qualified Network.AWS.MechanicalTurk.Types.HITId as Types
import qualified Network.AWS.MechanicalTurk.Types.HITLayoutId as Types
import qualified Network.AWS.MechanicalTurk.Types.HITReviewStatus as Types
import qualified Network.AWS.MechanicalTurk.Types.HITStatus as Types
import qualified Network.AWS.MechanicalTurk.Types.HITTypeId as Types
import qualified Network.AWS.MechanicalTurk.Types.QualificationRequirement as Types
import qualified Network.AWS.MechanicalTurk.Types.Reward as Types
import qualified Network.AWS.MechanicalTurk.Types.String as Types
import qualified Network.AWS.Prelude as Core

-- | The HIT data structure represents a single HIT, including all the information necessary for a Worker to accept and complete the HIT.
--
-- /See:/ 'mkHIT' smart constructor.
data HIT = HIT'
  { -- | The length of time, in seconds, that a Worker has to complete the HIT after accepting it.
    assignmentDurationInSeconds :: Core.Maybe Core.Integer,
    -- | The amount of time, in seconds, after the Worker submits an assignment for the HIT that the results are automatically approved by Amazon Mechanical Turk. This is the amount of time the Requester has to reject an assignment submitted by a Worker before the assignment is auto-approved and the Worker is paid.
    autoApprovalDelayInSeconds :: Core.Maybe Core.Integer,
    -- | The date and time the HIT was created.
    creationTime :: Core.Maybe Core.NominalDiffTime,
    -- | A general description of the HIT.
    description :: Core.Maybe Types.String,
    -- | The date and time the HIT expires.
    expiration :: Core.Maybe Core.NominalDiffTime,
    -- | The ID of the HIT Group of this HIT.
    hITGroupId :: Core.Maybe Types.HITGroupId,
    -- | A unique identifier for the HIT.
    hITId :: Core.Maybe Types.HITId,
    -- | The ID of the HIT Layout of this HIT.
    hITLayoutId :: Core.Maybe Types.HITLayoutId,
    -- | Indicates the review status of the HIT. Valid Values are NotReviewed | MarkedForReview | ReviewedAppropriate | ReviewedInappropriate.
    hITReviewStatus :: Core.Maybe Types.HITReviewStatus,
    -- | The status of the HIT and its assignments. Valid Values are Assignable | Unassignable | Reviewable | Reviewing | Disposed.
    hITStatus :: Core.Maybe Types.HITStatus,
    -- | The ID of the HIT type of this HIT
    hITTypeId :: Core.Maybe Types.HITTypeId,
    -- | One or more words or phrases that describe the HIT, separated by commas. Search terms similar to the keywords of a HIT are more likely to have the HIT in the search results.
    keywords :: Core.Maybe Types.String,
    -- | The number of times the HIT can be accepted and completed before the HIT becomes unavailable.
    maxAssignments :: Core.Maybe Core.Int,
    -- | The number of assignments for this HIT that are available for Workers to accept.
    numberOfAssignmentsAvailable :: Core.Maybe Core.Int,
    -- | The number of assignments for this HIT that have been approved or rejected.
    numberOfAssignmentsCompleted :: Core.Maybe Core.Int,
    -- | The number of assignments for this HIT that are being previewed or have been accepted by Workers, but have not yet been submitted, returned, or abandoned.
    numberOfAssignmentsPending :: Core.Maybe Core.Int,
    -- | Conditions that a Worker's Qualifications must meet in order to accept the HIT. A HIT can have between zero and ten Qualification requirements. All requirements must be met in order for a Worker to accept the HIT. Additionally, other actions can be restricted using the @ActionsGuarded@ field on each @QualificationRequirement@ structure.
    qualificationRequirements :: Core.Maybe [Types.QualificationRequirement],
    -- | The data the Worker completing the HIT uses produce the results. This is either either a QuestionForm, HTMLQuestion or an ExternalQuestion data structure.
    question :: Core.Maybe Types.String,
    -- | An arbitrary data field the Requester who created the HIT can use. This field is visible only to the creator of the HIT.
    requesterAnnotation :: Core.Maybe Types.String,
    reward :: Core.Maybe Types.Reward,
    -- | The title of the HIT.
    title :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'HIT' value with any optional fields omitted.
mkHIT ::
  HIT
mkHIT =
  HIT'
    { assignmentDurationInSeconds = Core.Nothing,
      autoApprovalDelayInSeconds = Core.Nothing,
      creationTime = Core.Nothing,
      description = Core.Nothing,
      expiration = Core.Nothing,
      hITGroupId = Core.Nothing,
      hITId = Core.Nothing,
      hITLayoutId = Core.Nothing,
      hITReviewStatus = Core.Nothing,
      hITStatus = Core.Nothing,
      hITTypeId = Core.Nothing,
      keywords = Core.Nothing,
      maxAssignments = Core.Nothing,
      numberOfAssignmentsAvailable = Core.Nothing,
      numberOfAssignmentsCompleted = Core.Nothing,
      numberOfAssignmentsPending = Core.Nothing,
      qualificationRequirements = Core.Nothing,
      question = Core.Nothing,
      requesterAnnotation = Core.Nothing,
      reward = Core.Nothing,
      title = Core.Nothing
    }

-- | The length of time, in seconds, that a Worker has to complete the HIT after accepting it.
--
-- /Note:/ Consider using 'assignmentDurationInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hitAssignmentDurationInSeconds :: Lens.Lens' HIT (Core.Maybe Core.Integer)
hitAssignmentDurationInSeconds = Lens.field @"assignmentDurationInSeconds"
{-# DEPRECATED hitAssignmentDurationInSeconds "Use generic-lens or generic-optics with 'assignmentDurationInSeconds' instead." #-}

-- | The amount of time, in seconds, after the Worker submits an assignment for the HIT that the results are automatically approved by Amazon Mechanical Turk. This is the amount of time the Requester has to reject an assignment submitted by a Worker before the assignment is auto-approved and the Worker is paid.
--
-- /Note:/ Consider using 'autoApprovalDelayInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hitAutoApprovalDelayInSeconds :: Lens.Lens' HIT (Core.Maybe Core.Integer)
hitAutoApprovalDelayInSeconds = Lens.field @"autoApprovalDelayInSeconds"
{-# DEPRECATED hitAutoApprovalDelayInSeconds "Use generic-lens or generic-optics with 'autoApprovalDelayInSeconds' instead." #-}

-- | The date and time the HIT was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hitCreationTime :: Lens.Lens' HIT (Core.Maybe Core.NominalDiffTime)
hitCreationTime = Lens.field @"creationTime"
{-# DEPRECATED hitCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | A general description of the HIT.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hitDescription :: Lens.Lens' HIT (Core.Maybe Types.String)
hitDescription = Lens.field @"description"
{-# DEPRECATED hitDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The date and time the HIT expires.
--
-- /Note:/ Consider using 'expiration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hitExpiration :: Lens.Lens' HIT (Core.Maybe Core.NominalDiffTime)
hitExpiration = Lens.field @"expiration"
{-# DEPRECATED hitExpiration "Use generic-lens or generic-optics with 'expiration' instead." #-}

-- | The ID of the HIT Group of this HIT.
--
-- /Note:/ Consider using 'hITGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hitHITGroupId :: Lens.Lens' HIT (Core.Maybe Types.HITGroupId)
hitHITGroupId = Lens.field @"hITGroupId"
{-# DEPRECATED hitHITGroupId "Use generic-lens or generic-optics with 'hITGroupId' instead." #-}

-- | A unique identifier for the HIT.
--
-- /Note:/ Consider using 'hITId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hitHITId :: Lens.Lens' HIT (Core.Maybe Types.HITId)
hitHITId = Lens.field @"hITId"
{-# DEPRECATED hitHITId "Use generic-lens or generic-optics with 'hITId' instead." #-}

-- | The ID of the HIT Layout of this HIT.
--
-- /Note:/ Consider using 'hITLayoutId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hitHITLayoutId :: Lens.Lens' HIT (Core.Maybe Types.HITLayoutId)
hitHITLayoutId = Lens.field @"hITLayoutId"
{-# DEPRECATED hitHITLayoutId "Use generic-lens or generic-optics with 'hITLayoutId' instead." #-}

-- | Indicates the review status of the HIT. Valid Values are NotReviewed | MarkedForReview | ReviewedAppropriate | ReviewedInappropriate.
--
-- /Note:/ Consider using 'hITReviewStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hitHITReviewStatus :: Lens.Lens' HIT (Core.Maybe Types.HITReviewStatus)
hitHITReviewStatus = Lens.field @"hITReviewStatus"
{-# DEPRECATED hitHITReviewStatus "Use generic-lens or generic-optics with 'hITReviewStatus' instead." #-}

-- | The status of the HIT and its assignments. Valid Values are Assignable | Unassignable | Reviewable | Reviewing | Disposed.
--
-- /Note:/ Consider using 'hITStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hitHITStatus :: Lens.Lens' HIT (Core.Maybe Types.HITStatus)
hitHITStatus = Lens.field @"hITStatus"
{-# DEPRECATED hitHITStatus "Use generic-lens or generic-optics with 'hITStatus' instead." #-}

-- | The ID of the HIT type of this HIT
--
-- /Note:/ Consider using 'hITTypeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hitHITTypeId :: Lens.Lens' HIT (Core.Maybe Types.HITTypeId)
hitHITTypeId = Lens.field @"hITTypeId"
{-# DEPRECATED hitHITTypeId "Use generic-lens or generic-optics with 'hITTypeId' instead." #-}

-- | One or more words or phrases that describe the HIT, separated by commas. Search terms similar to the keywords of a HIT are more likely to have the HIT in the search results.
--
-- /Note:/ Consider using 'keywords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hitKeywords :: Lens.Lens' HIT (Core.Maybe Types.String)
hitKeywords = Lens.field @"keywords"
{-# DEPRECATED hitKeywords "Use generic-lens or generic-optics with 'keywords' instead." #-}

-- | The number of times the HIT can be accepted and completed before the HIT becomes unavailable.
--
-- /Note:/ Consider using 'maxAssignments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hitMaxAssignments :: Lens.Lens' HIT (Core.Maybe Core.Int)
hitMaxAssignments = Lens.field @"maxAssignments"
{-# DEPRECATED hitMaxAssignments "Use generic-lens or generic-optics with 'maxAssignments' instead." #-}

-- | The number of assignments for this HIT that are available for Workers to accept.
--
-- /Note:/ Consider using 'numberOfAssignmentsAvailable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hitNumberOfAssignmentsAvailable :: Lens.Lens' HIT (Core.Maybe Core.Int)
hitNumberOfAssignmentsAvailable = Lens.field @"numberOfAssignmentsAvailable"
{-# DEPRECATED hitNumberOfAssignmentsAvailable "Use generic-lens or generic-optics with 'numberOfAssignmentsAvailable' instead." #-}

-- | The number of assignments for this HIT that have been approved or rejected.
--
-- /Note:/ Consider using 'numberOfAssignmentsCompleted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hitNumberOfAssignmentsCompleted :: Lens.Lens' HIT (Core.Maybe Core.Int)
hitNumberOfAssignmentsCompleted = Lens.field @"numberOfAssignmentsCompleted"
{-# DEPRECATED hitNumberOfAssignmentsCompleted "Use generic-lens or generic-optics with 'numberOfAssignmentsCompleted' instead." #-}

-- | The number of assignments for this HIT that are being previewed or have been accepted by Workers, but have not yet been submitted, returned, or abandoned.
--
-- /Note:/ Consider using 'numberOfAssignmentsPending' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hitNumberOfAssignmentsPending :: Lens.Lens' HIT (Core.Maybe Core.Int)
hitNumberOfAssignmentsPending = Lens.field @"numberOfAssignmentsPending"
{-# DEPRECATED hitNumberOfAssignmentsPending "Use generic-lens or generic-optics with 'numberOfAssignmentsPending' instead." #-}

-- | Conditions that a Worker's Qualifications must meet in order to accept the HIT. A HIT can have between zero and ten Qualification requirements. All requirements must be met in order for a Worker to accept the HIT. Additionally, other actions can be restricted using the @ActionsGuarded@ field on each @QualificationRequirement@ structure.
--
-- /Note:/ Consider using 'qualificationRequirements' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hitQualificationRequirements :: Lens.Lens' HIT (Core.Maybe [Types.QualificationRequirement])
hitQualificationRequirements = Lens.field @"qualificationRequirements"
{-# DEPRECATED hitQualificationRequirements "Use generic-lens or generic-optics with 'qualificationRequirements' instead." #-}

-- | The data the Worker completing the HIT uses produce the results. This is either either a QuestionForm, HTMLQuestion or an ExternalQuestion data structure.
--
-- /Note:/ Consider using 'question' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hitQuestion :: Lens.Lens' HIT (Core.Maybe Types.String)
hitQuestion = Lens.field @"question"
{-# DEPRECATED hitQuestion "Use generic-lens or generic-optics with 'question' instead." #-}

-- | An arbitrary data field the Requester who created the HIT can use. This field is visible only to the creator of the HIT.
--
-- /Note:/ Consider using 'requesterAnnotation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hitRequesterAnnotation :: Lens.Lens' HIT (Core.Maybe Types.String)
hitRequesterAnnotation = Lens.field @"requesterAnnotation"
{-# DEPRECATED hitRequesterAnnotation "Use generic-lens or generic-optics with 'requesterAnnotation' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'reward' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hitReward :: Lens.Lens' HIT (Core.Maybe Types.Reward)
hitReward = Lens.field @"reward"
{-# DEPRECATED hitReward "Use generic-lens or generic-optics with 'reward' instead." #-}

-- | The title of the HIT.
--
-- /Note:/ Consider using 'title' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hitTitle :: Lens.Lens' HIT (Core.Maybe Types.String)
hitTitle = Lens.field @"title"
{-# DEPRECATED hitTitle "Use generic-lens or generic-optics with 'title' instead." #-}

instance Core.FromJSON HIT where
  parseJSON =
    Core.withObject "HIT" Core.$
      \x ->
        HIT'
          Core.<$> (x Core..:? "AssignmentDurationInSeconds")
          Core.<*> (x Core..:? "AutoApprovalDelayInSeconds")
          Core.<*> (x Core..:? "CreationTime")
          Core.<*> (x Core..:? "Description")
          Core.<*> (x Core..:? "Expiration")
          Core.<*> (x Core..:? "HITGroupId")
          Core.<*> (x Core..:? "HITId")
          Core.<*> (x Core..:? "HITLayoutId")
          Core.<*> (x Core..:? "HITReviewStatus")
          Core.<*> (x Core..:? "HITStatus")
          Core.<*> (x Core..:? "HITTypeId")
          Core.<*> (x Core..:? "Keywords")
          Core.<*> (x Core..:? "MaxAssignments")
          Core.<*> (x Core..:? "NumberOfAssignmentsAvailable")
          Core.<*> (x Core..:? "NumberOfAssignmentsCompleted")
          Core.<*> (x Core..:? "NumberOfAssignmentsPending")
          Core.<*> (x Core..:? "QualificationRequirements")
          Core.<*> (x Core..:? "Question")
          Core.<*> (x Core..:? "RequesterAnnotation")
          Core.<*> (x Core..:? "Reward")
          Core.<*> (x Core..:? "Title")
