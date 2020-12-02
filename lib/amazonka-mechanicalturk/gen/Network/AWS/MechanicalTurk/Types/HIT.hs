{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.HIT
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.HIT where

import Network.AWS.Lens
import Network.AWS.MechanicalTurk.Types.HITReviewStatus
import Network.AWS.MechanicalTurk.Types.HITStatus
import Network.AWS.MechanicalTurk.Types.QualificationRequirement
import Network.AWS.Prelude

-- | The HIT data structure represents a single HIT, including all the information necessary for a Worker to accept and complete the HIT.
--
--
--
-- /See:/ 'hIT' smart constructor.
data HIT = HIT'
  { _hitCreationTime :: !(Maybe POSIX),
    _hitHITGroupId :: !(Maybe Text),
    _hitNumberOfAssignmentsPending :: !(Maybe Int),
    _hitHITTypeId :: !(Maybe Text),
    _hitExpiration :: !(Maybe POSIX),
    _hitAutoApprovalDelayInSeconds :: !(Maybe Integer),
    _hitRequesterAnnotation :: !(Maybe Text),
    _hitHITStatus :: !(Maybe HITStatus),
    _hitMaxAssignments :: !(Maybe Int),
    _hitNumberOfAssignmentsCompleted :: !(Maybe Int),
    _hitReward :: !(Maybe Text),
    _hitKeywords :: !(Maybe Text),
    _hitHITLayoutId :: !(Maybe Text),
    _hitQualificationRequirements ::
      !(Maybe [QualificationRequirement]),
    _hitTitle :: !(Maybe Text),
    _hitHITId :: !(Maybe Text),
    _hitHITReviewStatus :: !(Maybe HITReviewStatus),
    _hitNumberOfAssignmentsAvailable :: !(Maybe Int),
    _hitDescription :: !(Maybe Text),
    _hitQuestion :: !(Maybe Text),
    _hitAssignmentDurationInSeconds :: !(Maybe Integer)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HIT' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hitCreationTime' - The date and time the HIT was created.
--
-- * 'hitHITGroupId' - The ID of the HIT Group of this HIT.
--
-- * 'hitNumberOfAssignmentsPending' - The number of assignments for this HIT that are being previewed or have been accepted by Workers, but have not yet been submitted, returned, or abandoned.
--
-- * 'hitHITTypeId' - The ID of the HIT type of this HIT
--
-- * 'hitExpiration' - The date and time the HIT expires.
--
-- * 'hitAutoApprovalDelayInSeconds' - The amount of time, in seconds, after the Worker submits an assignment for the HIT that the results are automatically approved by Amazon Mechanical Turk. This is the amount of time the Requester has to reject an assignment submitted by a Worker before the assignment is auto-approved and the Worker is paid.
--
-- * 'hitRequesterAnnotation' - An arbitrary data field the Requester who created the HIT can use. This field is visible only to the creator of the HIT.
--
-- * 'hitHITStatus' - The status of the HIT and its assignments. Valid Values are Assignable | Unassignable | Reviewable | Reviewing | Disposed.
--
-- * 'hitMaxAssignments' - The number of times the HIT can be accepted and completed before the HIT becomes unavailable.
--
-- * 'hitNumberOfAssignmentsCompleted' - The number of assignments for this HIT that have been approved or rejected.
--
-- * 'hitReward' - Undocumented member.
--
-- * 'hitKeywords' - One or more words or phrases that describe the HIT, separated by commas. Search terms similar to the keywords of a HIT are more likely to have the HIT in the search results.
--
-- * 'hitHITLayoutId' - The ID of the HIT Layout of this HIT.
--
-- * 'hitQualificationRequirements' - Conditions that a Worker's Qualifications must meet in order to accept the HIT. A HIT can have between zero and ten Qualification requirements. All requirements must be met in order for a Worker to accept the HIT. Additionally, other actions can be restricted using the @ActionsGuarded@ field on each @QualificationRequirement@ structure.
--
-- * 'hitTitle' - The title of the HIT.
--
-- * 'hitHITId' - A unique identifier for the HIT.
--
-- * 'hitHITReviewStatus' - Indicates the review status of the HIT. Valid Values are NotReviewed | MarkedForReview | ReviewedAppropriate | ReviewedInappropriate.
--
-- * 'hitNumberOfAssignmentsAvailable' - The number of assignments for this HIT that are available for Workers to accept.
--
-- * 'hitDescription' - A general description of the HIT.
--
-- * 'hitQuestion' - The data the Worker completing the HIT uses produce the results. This is either either a QuestionForm, HTMLQuestion or an ExternalQuestion data structure.
--
-- * 'hitAssignmentDurationInSeconds' - The length of time, in seconds, that a Worker has to complete the HIT after accepting it.
hIT ::
  HIT
hIT =
  HIT'
    { _hitCreationTime = Nothing,
      _hitHITGroupId = Nothing,
      _hitNumberOfAssignmentsPending = Nothing,
      _hitHITTypeId = Nothing,
      _hitExpiration = Nothing,
      _hitAutoApprovalDelayInSeconds = Nothing,
      _hitRequesterAnnotation = Nothing,
      _hitHITStatus = Nothing,
      _hitMaxAssignments = Nothing,
      _hitNumberOfAssignmentsCompleted = Nothing,
      _hitReward = Nothing,
      _hitKeywords = Nothing,
      _hitHITLayoutId = Nothing,
      _hitQualificationRequirements = Nothing,
      _hitTitle = Nothing,
      _hitHITId = Nothing,
      _hitHITReviewStatus = Nothing,
      _hitNumberOfAssignmentsAvailable = Nothing,
      _hitDescription = Nothing,
      _hitQuestion = Nothing,
      _hitAssignmentDurationInSeconds = Nothing
    }

-- | The date and time the HIT was created.
hitCreationTime :: Lens' HIT (Maybe UTCTime)
hitCreationTime = lens _hitCreationTime (\s a -> s {_hitCreationTime = a}) . mapping _Time

-- | The ID of the HIT Group of this HIT.
hitHITGroupId :: Lens' HIT (Maybe Text)
hitHITGroupId = lens _hitHITGroupId (\s a -> s {_hitHITGroupId = a})

-- | The number of assignments for this HIT that are being previewed or have been accepted by Workers, but have not yet been submitted, returned, or abandoned.
hitNumberOfAssignmentsPending :: Lens' HIT (Maybe Int)
hitNumberOfAssignmentsPending = lens _hitNumberOfAssignmentsPending (\s a -> s {_hitNumberOfAssignmentsPending = a})

-- | The ID of the HIT type of this HIT
hitHITTypeId :: Lens' HIT (Maybe Text)
hitHITTypeId = lens _hitHITTypeId (\s a -> s {_hitHITTypeId = a})

-- | The date and time the HIT expires.
hitExpiration :: Lens' HIT (Maybe UTCTime)
hitExpiration = lens _hitExpiration (\s a -> s {_hitExpiration = a}) . mapping _Time

-- | The amount of time, in seconds, after the Worker submits an assignment for the HIT that the results are automatically approved by Amazon Mechanical Turk. This is the amount of time the Requester has to reject an assignment submitted by a Worker before the assignment is auto-approved and the Worker is paid.
hitAutoApprovalDelayInSeconds :: Lens' HIT (Maybe Integer)
hitAutoApprovalDelayInSeconds = lens _hitAutoApprovalDelayInSeconds (\s a -> s {_hitAutoApprovalDelayInSeconds = a})

-- | An arbitrary data field the Requester who created the HIT can use. This field is visible only to the creator of the HIT.
hitRequesterAnnotation :: Lens' HIT (Maybe Text)
hitRequesterAnnotation = lens _hitRequesterAnnotation (\s a -> s {_hitRequesterAnnotation = a})

-- | The status of the HIT and its assignments. Valid Values are Assignable | Unassignable | Reviewable | Reviewing | Disposed.
hitHITStatus :: Lens' HIT (Maybe HITStatus)
hitHITStatus = lens _hitHITStatus (\s a -> s {_hitHITStatus = a})

-- | The number of times the HIT can be accepted and completed before the HIT becomes unavailable.
hitMaxAssignments :: Lens' HIT (Maybe Int)
hitMaxAssignments = lens _hitMaxAssignments (\s a -> s {_hitMaxAssignments = a})

-- | The number of assignments for this HIT that have been approved or rejected.
hitNumberOfAssignmentsCompleted :: Lens' HIT (Maybe Int)
hitNumberOfAssignmentsCompleted = lens _hitNumberOfAssignmentsCompleted (\s a -> s {_hitNumberOfAssignmentsCompleted = a})

-- | Undocumented member.
hitReward :: Lens' HIT (Maybe Text)
hitReward = lens _hitReward (\s a -> s {_hitReward = a})

-- | One or more words or phrases that describe the HIT, separated by commas. Search terms similar to the keywords of a HIT are more likely to have the HIT in the search results.
hitKeywords :: Lens' HIT (Maybe Text)
hitKeywords = lens _hitKeywords (\s a -> s {_hitKeywords = a})

-- | The ID of the HIT Layout of this HIT.
hitHITLayoutId :: Lens' HIT (Maybe Text)
hitHITLayoutId = lens _hitHITLayoutId (\s a -> s {_hitHITLayoutId = a})

-- | Conditions that a Worker's Qualifications must meet in order to accept the HIT. A HIT can have between zero and ten Qualification requirements. All requirements must be met in order for a Worker to accept the HIT. Additionally, other actions can be restricted using the @ActionsGuarded@ field on each @QualificationRequirement@ structure.
hitQualificationRequirements :: Lens' HIT [QualificationRequirement]
hitQualificationRequirements = lens _hitQualificationRequirements (\s a -> s {_hitQualificationRequirements = a}) . _Default . _Coerce

-- | The title of the HIT.
hitTitle :: Lens' HIT (Maybe Text)
hitTitle = lens _hitTitle (\s a -> s {_hitTitle = a})

-- | A unique identifier for the HIT.
hitHITId :: Lens' HIT (Maybe Text)
hitHITId = lens _hitHITId (\s a -> s {_hitHITId = a})

-- | Indicates the review status of the HIT. Valid Values are NotReviewed | MarkedForReview | ReviewedAppropriate | ReviewedInappropriate.
hitHITReviewStatus :: Lens' HIT (Maybe HITReviewStatus)
hitHITReviewStatus = lens _hitHITReviewStatus (\s a -> s {_hitHITReviewStatus = a})

-- | The number of assignments for this HIT that are available for Workers to accept.
hitNumberOfAssignmentsAvailable :: Lens' HIT (Maybe Int)
hitNumberOfAssignmentsAvailable = lens _hitNumberOfAssignmentsAvailable (\s a -> s {_hitNumberOfAssignmentsAvailable = a})

-- | A general description of the HIT.
hitDescription :: Lens' HIT (Maybe Text)
hitDescription = lens _hitDescription (\s a -> s {_hitDescription = a})

-- | The data the Worker completing the HIT uses produce the results. This is either either a QuestionForm, HTMLQuestion or an ExternalQuestion data structure.
hitQuestion :: Lens' HIT (Maybe Text)
hitQuestion = lens _hitQuestion (\s a -> s {_hitQuestion = a})

-- | The length of time, in seconds, that a Worker has to complete the HIT after accepting it.
hitAssignmentDurationInSeconds :: Lens' HIT (Maybe Integer)
hitAssignmentDurationInSeconds = lens _hitAssignmentDurationInSeconds (\s a -> s {_hitAssignmentDurationInSeconds = a})

instance FromJSON HIT where
  parseJSON =
    withObject
      "HIT"
      ( \x ->
          HIT'
            <$> (x .:? "CreationTime")
            <*> (x .:? "HITGroupId")
            <*> (x .:? "NumberOfAssignmentsPending")
            <*> (x .:? "HITTypeId")
            <*> (x .:? "Expiration")
            <*> (x .:? "AutoApprovalDelayInSeconds")
            <*> (x .:? "RequesterAnnotation")
            <*> (x .:? "HITStatus")
            <*> (x .:? "MaxAssignments")
            <*> (x .:? "NumberOfAssignmentsCompleted")
            <*> (x .:? "Reward")
            <*> (x .:? "Keywords")
            <*> (x .:? "HITLayoutId")
            <*> (x .:? "QualificationRequirements" .!= mempty)
            <*> (x .:? "Title")
            <*> (x .:? "HITId")
            <*> (x .:? "HITReviewStatus")
            <*> (x .:? "NumberOfAssignmentsAvailable")
            <*> (x .:? "Description")
            <*> (x .:? "Question")
            <*> (x .:? "AssignmentDurationInSeconds")
      )

instance Hashable HIT

instance NFData HIT
