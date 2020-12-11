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
    hitCreationTime,
    hitHITGroupId,
    hitNumberOfAssignmentsPending,
    hitHITTypeId,
    hitExpiration,
    hitAutoApprovalDelayInSeconds,
    hitRequesterAnnotation,
    hitHITStatus,
    hitMaxAssignments,
    hitNumberOfAssignmentsCompleted,
    hitReward,
    hitKeywords,
    hitHITLayoutId,
    hitQualificationRequirements,
    hitTitle,
    hitHITId,
    hitHITReviewStatus,
    hitNumberOfAssignmentsAvailable,
    hitDescription,
    hitQuestion,
    hitAssignmentDurationInSeconds,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types.HITReviewStatus
import Network.AWS.MechanicalTurk.Types.HITStatus
import Network.AWS.MechanicalTurk.Types.QualificationRequirement
import qualified Network.AWS.Prelude as Lude

-- | The HIT data structure represents a single HIT, including all the information necessary for a Worker to accept and complete the HIT.
--
-- /See:/ 'mkHIT' smart constructor.
data HIT = HIT'
  { creationTime :: Lude.Maybe Lude.Timestamp,
    hITGroupId :: Lude.Maybe Lude.Text,
    numberOfAssignmentsPending :: Lude.Maybe Lude.Int,
    hITTypeId :: Lude.Maybe Lude.Text,
    expiration :: Lude.Maybe Lude.Timestamp,
    autoApprovalDelayInSeconds :: Lude.Maybe Lude.Integer,
    requesterAnnotation :: Lude.Maybe Lude.Text,
    hITStatus :: Lude.Maybe HITStatus,
    maxAssignments :: Lude.Maybe Lude.Int,
    numberOfAssignmentsCompleted :: Lude.Maybe Lude.Int,
    reward :: Lude.Maybe Lude.Text,
    keywords :: Lude.Maybe Lude.Text,
    hITLayoutId :: Lude.Maybe Lude.Text,
    qualificationRequirements :: Lude.Maybe [QualificationRequirement],
    title :: Lude.Maybe Lude.Text,
    hITId :: Lude.Maybe Lude.Text,
    hITReviewStatus :: Lude.Maybe HITReviewStatus,
    numberOfAssignmentsAvailable :: Lude.Maybe Lude.Int,
    description :: Lude.Maybe Lude.Text,
    question :: Lude.Maybe Lude.Text,
    assignmentDurationInSeconds :: Lude.Maybe Lude.Integer
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HIT' with the minimum fields required to make a request.
--
-- * 'assignmentDurationInSeconds' - The length of time, in seconds, that a Worker has to complete the HIT after accepting it.
-- * 'autoApprovalDelayInSeconds' - The amount of time, in seconds, after the Worker submits an assignment for the HIT that the results are automatically approved by Amazon Mechanical Turk. This is the amount of time the Requester has to reject an assignment submitted by a Worker before the assignment is auto-approved and the Worker is paid.
-- * 'creationTime' - The date and time the HIT was created.
-- * 'description' - A general description of the HIT.
-- * 'expiration' - The date and time the HIT expires.
-- * 'hITGroupId' - The ID of the HIT Group of this HIT.
-- * 'hITId' - A unique identifier for the HIT.
-- * 'hITLayoutId' - The ID of the HIT Layout of this HIT.
-- * 'hITReviewStatus' - Indicates the review status of the HIT. Valid Values are NotReviewed | MarkedForReview | ReviewedAppropriate | ReviewedInappropriate.
-- * 'hITStatus' - The status of the HIT and its assignments. Valid Values are Assignable | Unassignable | Reviewable | Reviewing | Disposed.
-- * 'hITTypeId' - The ID of the HIT type of this HIT
-- * 'keywords' - One or more words or phrases that describe the HIT, separated by commas. Search terms similar to the keywords of a HIT are more likely to have the HIT in the search results.
-- * 'maxAssignments' - The number of times the HIT can be accepted and completed before the HIT becomes unavailable.
-- * 'numberOfAssignmentsAvailable' - The number of assignments for this HIT that are available for Workers to accept.
-- * 'numberOfAssignmentsCompleted' - The number of assignments for this HIT that have been approved or rejected.
-- * 'numberOfAssignmentsPending' - The number of assignments for this HIT that are being previewed or have been accepted by Workers, but have not yet been submitted, returned, or abandoned.
-- * 'qualificationRequirements' - Conditions that a Worker's Qualifications must meet in order to accept the HIT. A HIT can have between zero and ten Qualification requirements. All requirements must be met in order for a Worker to accept the HIT. Additionally, other actions can be restricted using the @ActionsGuarded@ field on each @QualificationRequirement@ structure.
-- * 'question' - The data the Worker completing the HIT uses produce the results. This is either either a QuestionForm, HTMLQuestion or an ExternalQuestion data structure.
-- * 'requesterAnnotation' - An arbitrary data field the Requester who created the HIT can use. This field is visible only to the creator of the HIT.
-- * 'reward' - Undocumented field.
-- * 'title' - The title of the HIT.
mkHIT ::
  HIT
mkHIT =
  HIT'
    { creationTime = Lude.Nothing,
      hITGroupId = Lude.Nothing,
      numberOfAssignmentsPending = Lude.Nothing,
      hITTypeId = Lude.Nothing,
      expiration = Lude.Nothing,
      autoApprovalDelayInSeconds = Lude.Nothing,
      requesterAnnotation = Lude.Nothing,
      hITStatus = Lude.Nothing,
      maxAssignments = Lude.Nothing,
      numberOfAssignmentsCompleted = Lude.Nothing,
      reward = Lude.Nothing,
      keywords = Lude.Nothing,
      hITLayoutId = Lude.Nothing,
      qualificationRequirements = Lude.Nothing,
      title = Lude.Nothing,
      hITId = Lude.Nothing,
      hITReviewStatus = Lude.Nothing,
      numberOfAssignmentsAvailable = Lude.Nothing,
      description = Lude.Nothing,
      question = Lude.Nothing,
      assignmentDurationInSeconds = Lude.Nothing
    }

-- | The date and time the HIT was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hitCreationTime :: Lens.Lens' HIT (Lude.Maybe Lude.Timestamp)
hitCreationTime = Lens.lens (creationTime :: HIT -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: HIT)
{-# DEPRECATED hitCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The ID of the HIT Group of this HIT.
--
-- /Note:/ Consider using 'hITGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hitHITGroupId :: Lens.Lens' HIT (Lude.Maybe Lude.Text)
hitHITGroupId = Lens.lens (hITGroupId :: HIT -> Lude.Maybe Lude.Text) (\s a -> s {hITGroupId = a} :: HIT)
{-# DEPRECATED hitHITGroupId "Use generic-lens or generic-optics with 'hITGroupId' instead." #-}

-- | The number of assignments for this HIT that are being previewed or have been accepted by Workers, but have not yet been submitted, returned, or abandoned.
--
-- /Note:/ Consider using 'numberOfAssignmentsPending' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hitNumberOfAssignmentsPending :: Lens.Lens' HIT (Lude.Maybe Lude.Int)
hitNumberOfAssignmentsPending = Lens.lens (numberOfAssignmentsPending :: HIT -> Lude.Maybe Lude.Int) (\s a -> s {numberOfAssignmentsPending = a} :: HIT)
{-# DEPRECATED hitNumberOfAssignmentsPending "Use generic-lens or generic-optics with 'numberOfAssignmentsPending' instead." #-}

-- | The ID of the HIT type of this HIT
--
-- /Note:/ Consider using 'hITTypeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hitHITTypeId :: Lens.Lens' HIT (Lude.Maybe Lude.Text)
hitHITTypeId = Lens.lens (hITTypeId :: HIT -> Lude.Maybe Lude.Text) (\s a -> s {hITTypeId = a} :: HIT)
{-# DEPRECATED hitHITTypeId "Use generic-lens or generic-optics with 'hITTypeId' instead." #-}

-- | The date and time the HIT expires.
--
-- /Note:/ Consider using 'expiration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hitExpiration :: Lens.Lens' HIT (Lude.Maybe Lude.Timestamp)
hitExpiration = Lens.lens (expiration :: HIT -> Lude.Maybe Lude.Timestamp) (\s a -> s {expiration = a} :: HIT)
{-# DEPRECATED hitExpiration "Use generic-lens or generic-optics with 'expiration' instead." #-}

-- | The amount of time, in seconds, after the Worker submits an assignment for the HIT that the results are automatically approved by Amazon Mechanical Turk. This is the amount of time the Requester has to reject an assignment submitted by a Worker before the assignment is auto-approved and the Worker is paid.
--
-- /Note:/ Consider using 'autoApprovalDelayInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hitAutoApprovalDelayInSeconds :: Lens.Lens' HIT (Lude.Maybe Lude.Integer)
hitAutoApprovalDelayInSeconds = Lens.lens (autoApprovalDelayInSeconds :: HIT -> Lude.Maybe Lude.Integer) (\s a -> s {autoApprovalDelayInSeconds = a} :: HIT)
{-# DEPRECATED hitAutoApprovalDelayInSeconds "Use generic-lens or generic-optics with 'autoApprovalDelayInSeconds' instead." #-}

-- | An arbitrary data field the Requester who created the HIT can use. This field is visible only to the creator of the HIT.
--
-- /Note:/ Consider using 'requesterAnnotation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hitRequesterAnnotation :: Lens.Lens' HIT (Lude.Maybe Lude.Text)
hitRequesterAnnotation = Lens.lens (requesterAnnotation :: HIT -> Lude.Maybe Lude.Text) (\s a -> s {requesterAnnotation = a} :: HIT)
{-# DEPRECATED hitRequesterAnnotation "Use generic-lens or generic-optics with 'requesterAnnotation' instead." #-}

-- | The status of the HIT and its assignments. Valid Values are Assignable | Unassignable | Reviewable | Reviewing | Disposed.
--
-- /Note:/ Consider using 'hITStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hitHITStatus :: Lens.Lens' HIT (Lude.Maybe HITStatus)
hitHITStatus = Lens.lens (hITStatus :: HIT -> Lude.Maybe HITStatus) (\s a -> s {hITStatus = a} :: HIT)
{-# DEPRECATED hitHITStatus "Use generic-lens or generic-optics with 'hITStatus' instead." #-}

-- | The number of times the HIT can be accepted and completed before the HIT becomes unavailable.
--
-- /Note:/ Consider using 'maxAssignments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hitMaxAssignments :: Lens.Lens' HIT (Lude.Maybe Lude.Int)
hitMaxAssignments = Lens.lens (maxAssignments :: HIT -> Lude.Maybe Lude.Int) (\s a -> s {maxAssignments = a} :: HIT)
{-# DEPRECATED hitMaxAssignments "Use generic-lens or generic-optics with 'maxAssignments' instead." #-}

-- | The number of assignments for this HIT that have been approved or rejected.
--
-- /Note:/ Consider using 'numberOfAssignmentsCompleted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hitNumberOfAssignmentsCompleted :: Lens.Lens' HIT (Lude.Maybe Lude.Int)
hitNumberOfAssignmentsCompleted = Lens.lens (numberOfAssignmentsCompleted :: HIT -> Lude.Maybe Lude.Int) (\s a -> s {numberOfAssignmentsCompleted = a} :: HIT)
{-# DEPRECATED hitNumberOfAssignmentsCompleted "Use generic-lens or generic-optics with 'numberOfAssignmentsCompleted' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'reward' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hitReward :: Lens.Lens' HIT (Lude.Maybe Lude.Text)
hitReward = Lens.lens (reward :: HIT -> Lude.Maybe Lude.Text) (\s a -> s {reward = a} :: HIT)
{-# DEPRECATED hitReward "Use generic-lens or generic-optics with 'reward' instead." #-}

-- | One or more words or phrases that describe the HIT, separated by commas. Search terms similar to the keywords of a HIT are more likely to have the HIT in the search results.
--
-- /Note:/ Consider using 'keywords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hitKeywords :: Lens.Lens' HIT (Lude.Maybe Lude.Text)
hitKeywords = Lens.lens (keywords :: HIT -> Lude.Maybe Lude.Text) (\s a -> s {keywords = a} :: HIT)
{-# DEPRECATED hitKeywords "Use generic-lens or generic-optics with 'keywords' instead." #-}

-- | The ID of the HIT Layout of this HIT.
--
-- /Note:/ Consider using 'hITLayoutId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hitHITLayoutId :: Lens.Lens' HIT (Lude.Maybe Lude.Text)
hitHITLayoutId = Lens.lens (hITLayoutId :: HIT -> Lude.Maybe Lude.Text) (\s a -> s {hITLayoutId = a} :: HIT)
{-# DEPRECATED hitHITLayoutId "Use generic-lens or generic-optics with 'hITLayoutId' instead." #-}

-- | Conditions that a Worker's Qualifications must meet in order to accept the HIT. A HIT can have between zero and ten Qualification requirements. All requirements must be met in order for a Worker to accept the HIT. Additionally, other actions can be restricted using the @ActionsGuarded@ field on each @QualificationRequirement@ structure.
--
-- /Note:/ Consider using 'qualificationRequirements' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hitQualificationRequirements :: Lens.Lens' HIT (Lude.Maybe [QualificationRequirement])
hitQualificationRequirements = Lens.lens (qualificationRequirements :: HIT -> Lude.Maybe [QualificationRequirement]) (\s a -> s {qualificationRequirements = a} :: HIT)
{-# DEPRECATED hitQualificationRequirements "Use generic-lens or generic-optics with 'qualificationRequirements' instead." #-}

-- | The title of the HIT.
--
-- /Note:/ Consider using 'title' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hitTitle :: Lens.Lens' HIT (Lude.Maybe Lude.Text)
hitTitle = Lens.lens (title :: HIT -> Lude.Maybe Lude.Text) (\s a -> s {title = a} :: HIT)
{-# DEPRECATED hitTitle "Use generic-lens or generic-optics with 'title' instead." #-}

-- | A unique identifier for the HIT.
--
-- /Note:/ Consider using 'hITId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hitHITId :: Lens.Lens' HIT (Lude.Maybe Lude.Text)
hitHITId = Lens.lens (hITId :: HIT -> Lude.Maybe Lude.Text) (\s a -> s {hITId = a} :: HIT)
{-# DEPRECATED hitHITId "Use generic-lens or generic-optics with 'hITId' instead." #-}

-- | Indicates the review status of the HIT. Valid Values are NotReviewed | MarkedForReview | ReviewedAppropriate | ReviewedInappropriate.
--
-- /Note:/ Consider using 'hITReviewStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hitHITReviewStatus :: Lens.Lens' HIT (Lude.Maybe HITReviewStatus)
hitHITReviewStatus = Lens.lens (hITReviewStatus :: HIT -> Lude.Maybe HITReviewStatus) (\s a -> s {hITReviewStatus = a} :: HIT)
{-# DEPRECATED hitHITReviewStatus "Use generic-lens or generic-optics with 'hITReviewStatus' instead." #-}

-- | The number of assignments for this HIT that are available for Workers to accept.
--
-- /Note:/ Consider using 'numberOfAssignmentsAvailable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hitNumberOfAssignmentsAvailable :: Lens.Lens' HIT (Lude.Maybe Lude.Int)
hitNumberOfAssignmentsAvailable = Lens.lens (numberOfAssignmentsAvailable :: HIT -> Lude.Maybe Lude.Int) (\s a -> s {numberOfAssignmentsAvailable = a} :: HIT)
{-# DEPRECATED hitNumberOfAssignmentsAvailable "Use generic-lens or generic-optics with 'numberOfAssignmentsAvailable' instead." #-}

-- | A general description of the HIT.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hitDescription :: Lens.Lens' HIT (Lude.Maybe Lude.Text)
hitDescription = Lens.lens (description :: HIT -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: HIT)
{-# DEPRECATED hitDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The data the Worker completing the HIT uses produce the results. This is either either a QuestionForm, HTMLQuestion or an ExternalQuestion data structure.
--
-- /Note:/ Consider using 'question' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hitQuestion :: Lens.Lens' HIT (Lude.Maybe Lude.Text)
hitQuestion = Lens.lens (question :: HIT -> Lude.Maybe Lude.Text) (\s a -> s {question = a} :: HIT)
{-# DEPRECATED hitQuestion "Use generic-lens or generic-optics with 'question' instead." #-}

-- | The length of time, in seconds, that a Worker has to complete the HIT after accepting it.
--
-- /Note:/ Consider using 'assignmentDurationInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hitAssignmentDurationInSeconds :: Lens.Lens' HIT (Lude.Maybe Lude.Integer)
hitAssignmentDurationInSeconds = Lens.lens (assignmentDurationInSeconds :: HIT -> Lude.Maybe Lude.Integer) (\s a -> s {assignmentDurationInSeconds = a} :: HIT)
{-# DEPRECATED hitAssignmentDurationInSeconds "Use generic-lens or generic-optics with 'assignmentDurationInSeconds' instead." #-}

instance Lude.FromJSON HIT where
  parseJSON =
    Lude.withObject
      "HIT"
      ( \x ->
          HIT'
            Lude.<$> (x Lude..:? "CreationTime")
            Lude.<*> (x Lude..:? "HITGroupId")
            Lude.<*> (x Lude..:? "NumberOfAssignmentsPending")
            Lude.<*> (x Lude..:? "HITTypeId")
            Lude.<*> (x Lude..:? "Expiration")
            Lude.<*> (x Lude..:? "AutoApprovalDelayInSeconds")
            Lude.<*> (x Lude..:? "RequesterAnnotation")
            Lude.<*> (x Lude..:? "HITStatus")
            Lude.<*> (x Lude..:? "MaxAssignments")
            Lude.<*> (x Lude..:? "NumberOfAssignmentsCompleted")
            Lude.<*> (x Lude..:? "Reward")
            Lude.<*> (x Lude..:? "Keywords")
            Lude.<*> (x Lude..:? "HITLayoutId")
            Lude.<*> (x Lude..:? "QualificationRequirements" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Title")
            Lude.<*> (x Lude..:? "HITId")
            Lude.<*> (x Lude..:? "HITReviewStatus")
            Lude.<*> (x Lude..:? "NumberOfAssignmentsAvailable")
            Lude.<*> (x Lude..:? "Description")
            Lude.<*> (x Lude..:? "Question")
            Lude.<*> (x Lude..:? "AssignmentDurationInSeconds")
      )
