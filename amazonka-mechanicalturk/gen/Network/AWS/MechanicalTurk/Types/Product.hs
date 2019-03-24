{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MechanicalTurk.Types.Product where

import Network.AWS.Lens
import Network.AWS.MechanicalTurk.Types.Sum
import Network.AWS.Prelude

-- | The Assignment data structure represents a single assignment of a HIT to a Worker. The assignment tracks the Worker's efforts to complete the HIT, and contains the results for later retrieval.
--
--
--
-- /See:/ 'assignment' smart constructor.
data Assignment = Assignment'
  { _aAcceptTime        :: !(Maybe POSIX)
  , _aAnswer            :: !(Maybe Text)
  , _aAssignmentStatus  :: !(Maybe AssignmentStatus)
  , _aRequesterFeedback :: !(Maybe Text)
  , _aDeadline          :: !(Maybe POSIX)
  , _aApprovalTime      :: !(Maybe POSIX)
  , _aRejectionTime     :: !(Maybe POSIX)
  , _aAutoApprovalTime  :: !(Maybe POSIX)
  , _aHITId             :: !(Maybe Text)
  , _aWorkerId          :: !(Maybe Text)
  , _aAssignmentId      :: !(Maybe Text)
  , _aSubmitTime        :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Assignment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aAcceptTime' - The date and time the Worker accepted the assignment.
--
-- * 'aAnswer' - The Worker's answers submitted for the HIT contained in a QuestionFormAnswers document, if the Worker provides an answer. If the Worker does not provide any answers, Answer may contain a QuestionFormAnswers document, or Answer may be empty.
--
-- * 'aAssignmentStatus' - The status of the assignment.
--
-- * 'aRequesterFeedback' - The feedback string included with the call to the ApproveAssignment operation or the RejectAssignment operation, if the Requester approved or rejected the assignment and specified feedback.
--
-- * 'aDeadline' - The date and time of the deadline for the assignment. This value is derived from the deadline specification for the HIT and the date and time the Worker accepted the HIT.
--
-- * 'aApprovalTime' - If the Worker has submitted results and the Requester has approved the results, ApprovalTime is the date and time the Requester approved the results. This value is omitted from the assignment if the Requester has not yet approved the results.
--
-- * 'aRejectionTime' - If the Worker has submitted results and the Requester has rejected the results, RejectionTime is the date and time the Requester rejected the results.
--
-- * 'aAutoApprovalTime' - If results have been submitted, AutoApprovalTime is the date and time the results of the assignment results are considered Approved automatically if they have not already been explicitly approved or rejected by the Requester. This value is derived from the auto-approval delay specified by the Requester in the HIT. This value is omitted from the assignment if the Worker has not yet submitted results.
--
-- * 'aHITId' - The ID of the HIT.
--
-- * 'aWorkerId' - The ID of the Worker who accepted the HIT.
--
-- * 'aAssignmentId' - A unique identifier for the assignment.
--
-- * 'aSubmitTime' - If the Worker has submitted results, SubmitTime is the date and time the assignment was submitted. This value is omitted from the assignment if the Worker has not yet submitted results.
assignment
    :: Assignment
assignment =
  Assignment'
    { _aAcceptTime = Nothing
    , _aAnswer = Nothing
    , _aAssignmentStatus = Nothing
    , _aRequesterFeedback = Nothing
    , _aDeadline = Nothing
    , _aApprovalTime = Nothing
    , _aRejectionTime = Nothing
    , _aAutoApprovalTime = Nothing
    , _aHITId = Nothing
    , _aWorkerId = Nothing
    , _aAssignmentId = Nothing
    , _aSubmitTime = Nothing
    }


-- | The date and time the Worker accepted the assignment.
aAcceptTime :: Lens' Assignment (Maybe UTCTime)
aAcceptTime = lens _aAcceptTime (\ s a -> s{_aAcceptTime = a}) . mapping _Time

-- | The Worker's answers submitted for the HIT contained in a QuestionFormAnswers document, if the Worker provides an answer. If the Worker does not provide any answers, Answer may contain a QuestionFormAnswers document, or Answer may be empty.
aAnswer :: Lens' Assignment (Maybe Text)
aAnswer = lens _aAnswer (\ s a -> s{_aAnswer = a})

-- | The status of the assignment.
aAssignmentStatus :: Lens' Assignment (Maybe AssignmentStatus)
aAssignmentStatus = lens _aAssignmentStatus (\ s a -> s{_aAssignmentStatus = a})

-- | The feedback string included with the call to the ApproveAssignment operation or the RejectAssignment operation, if the Requester approved or rejected the assignment and specified feedback.
aRequesterFeedback :: Lens' Assignment (Maybe Text)
aRequesterFeedback = lens _aRequesterFeedback (\ s a -> s{_aRequesterFeedback = a})

-- | The date and time of the deadline for the assignment. This value is derived from the deadline specification for the HIT and the date and time the Worker accepted the HIT.
aDeadline :: Lens' Assignment (Maybe UTCTime)
aDeadline = lens _aDeadline (\ s a -> s{_aDeadline = a}) . mapping _Time

-- | If the Worker has submitted results and the Requester has approved the results, ApprovalTime is the date and time the Requester approved the results. This value is omitted from the assignment if the Requester has not yet approved the results.
aApprovalTime :: Lens' Assignment (Maybe UTCTime)
aApprovalTime = lens _aApprovalTime (\ s a -> s{_aApprovalTime = a}) . mapping _Time

-- | If the Worker has submitted results and the Requester has rejected the results, RejectionTime is the date and time the Requester rejected the results.
aRejectionTime :: Lens' Assignment (Maybe UTCTime)
aRejectionTime = lens _aRejectionTime (\ s a -> s{_aRejectionTime = a}) . mapping _Time

-- | If results have been submitted, AutoApprovalTime is the date and time the results of the assignment results are considered Approved automatically if they have not already been explicitly approved or rejected by the Requester. This value is derived from the auto-approval delay specified by the Requester in the HIT. This value is omitted from the assignment if the Worker has not yet submitted results.
aAutoApprovalTime :: Lens' Assignment (Maybe UTCTime)
aAutoApprovalTime = lens _aAutoApprovalTime (\ s a -> s{_aAutoApprovalTime = a}) . mapping _Time

-- | The ID of the HIT.
aHITId :: Lens' Assignment (Maybe Text)
aHITId = lens _aHITId (\ s a -> s{_aHITId = a})

-- | The ID of the Worker who accepted the HIT.
aWorkerId :: Lens' Assignment (Maybe Text)
aWorkerId = lens _aWorkerId (\ s a -> s{_aWorkerId = a})

-- | A unique identifier for the assignment.
aAssignmentId :: Lens' Assignment (Maybe Text)
aAssignmentId = lens _aAssignmentId (\ s a -> s{_aAssignmentId = a})

-- | If the Worker has submitted results, SubmitTime is the date and time the assignment was submitted. This value is omitted from the assignment if the Worker has not yet submitted results.
aSubmitTime :: Lens' Assignment (Maybe UTCTime)
aSubmitTime = lens _aSubmitTime (\ s a -> s{_aSubmitTime = a}) . mapping _Time

instance FromJSON Assignment where
        parseJSON
          = withObject "Assignment"
              (\ x ->
                 Assignment' <$>
                   (x .:? "AcceptTime") <*> (x .:? "Answer") <*>
                     (x .:? "AssignmentStatus")
                     <*> (x .:? "RequesterFeedback")
                     <*> (x .:? "Deadline")
                     <*> (x .:? "ApprovalTime")
                     <*> (x .:? "RejectionTime")
                     <*> (x .:? "AutoApprovalTime")
                     <*> (x .:? "HITId")
                     <*> (x .:? "WorkerId")
                     <*> (x .:? "AssignmentId")
                     <*> (x .:? "SubmitTime"))

instance Hashable Assignment where

instance NFData Assignment where

-- | An object representing a Bonus payment paid to a Worker.
--
--
--
-- /See:/ 'bonusPayment' smart constructor.
data BonusPayment = BonusPayment'
  { _bpReason       :: !(Maybe Text)
  , _bpGrantTime    :: !(Maybe POSIX)
  , _bpWorkerId     :: !(Maybe Text)
  , _bpAssignmentId :: !(Maybe Text)
  , _bpBonusAmount  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BonusPayment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bpReason' - The Reason text given when the bonus was granted, if any.
--
-- * 'bpGrantTime' - The date and time of when the bonus was granted.
--
-- * 'bpWorkerId' - The ID of the Worker to whom the bonus was paid.
--
-- * 'bpAssignmentId' - The ID of the assignment associated with this bonus payment.
--
-- * 'bpBonusAmount' - Undocumented member.
bonusPayment
    :: BonusPayment
bonusPayment =
  BonusPayment'
    { _bpReason = Nothing
    , _bpGrantTime = Nothing
    , _bpWorkerId = Nothing
    , _bpAssignmentId = Nothing
    , _bpBonusAmount = Nothing
    }


-- | The Reason text given when the bonus was granted, if any.
bpReason :: Lens' BonusPayment (Maybe Text)
bpReason = lens _bpReason (\ s a -> s{_bpReason = a})

-- | The date and time of when the bonus was granted.
bpGrantTime :: Lens' BonusPayment (Maybe UTCTime)
bpGrantTime = lens _bpGrantTime (\ s a -> s{_bpGrantTime = a}) . mapping _Time

-- | The ID of the Worker to whom the bonus was paid.
bpWorkerId :: Lens' BonusPayment (Maybe Text)
bpWorkerId = lens _bpWorkerId (\ s a -> s{_bpWorkerId = a})

-- | The ID of the assignment associated with this bonus payment.
bpAssignmentId :: Lens' BonusPayment (Maybe Text)
bpAssignmentId = lens _bpAssignmentId (\ s a -> s{_bpAssignmentId = a})

-- | Undocumented member.
bpBonusAmount :: Lens' BonusPayment (Maybe Text)
bpBonusAmount = lens _bpBonusAmount (\ s a -> s{_bpBonusAmount = a})

instance FromJSON BonusPayment where
        parseJSON
          = withObject "BonusPayment"
              (\ x ->
                 BonusPayment' <$>
                   (x .:? "Reason") <*> (x .:? "GrantTime") <*>
                     (x .:? "WorkerId")
                     <*> (x .:? "AssignmentId")
                     <*> (x .:? "BonusAmount"))

instance Hashable BonusPayment where

instance NFData BonusPayment where

-- | The HIT data structure represents a single HIT, including all the information necessary for a Worker to accept and complete the HIT.
--
--
--
-- /See:/ 'hIT' smart constructor.
data HIT = HIT'
  { _hitCreationTime                 :: !(Maybe POSIX)
  , _hitHITGroupId                   :: !(Maybe Text)
  , _hitNumberOfAssignmentsPending   :: !(Maybe Int)
  , _hitHITTypeId                    :: !(Maybe Text)
  , _hitExpiration                   :: !(Maybe POSIX)
  , _hitAutoApprovalDelayInSeconds   :: !(Maybe Integer)
  , _hitRequesterAnnotation          :: !(Maybe Text)
  , _hitHITStatus                    :: !(Maybe HITStatus)
  , _hitMaxAssignments               :: !(Maybe Int)
  , _hitNumberOfAssignmentsCompleted :: !(Maybe Int)
  , _hitReward                       :: !(Maybe Text)
  , _hitKeywords                     :: !(Maybe Text)
  , _hitHITLayoutId                  :: !(Maybe Text)
  , _hitQualificationRequirements    :: !(Maybe [QualificationRequirement])
  , _hitTitle                        :: !(Maybe Text)
  , _hitHITId                        :: !(Maybe Text)
  , _hitHITReviewStatus              :: !(Maybe HITReviewStatus)
  , _hitNumberOfAssignmentsAvailable :: !(Maybe Int)
  , _hitDescription                  :: !(Maybe Text)
  , _hitQuestion                     :: !(Maybe Text)
  , _hitAssignmentDurationInSeconds  :: !(Maybe Integer)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


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
hIT
    :: HIT
hIT =
  HIT'
    { _hitCreationTime = Nothing
    , _hitHITGroupId = Nothing
    , _hitNumberOfAssignmentsPending = Nothing
    , _hitHITTypeId = Nothing
    , _hitExpiration = Nothing
    , _hitAutoApprovalDelayInSeconds = Nothing
    , _hitRequesterAnnotation = Nothing
    , _hitHITStatus = Nothing
    , _hitMaxAssignments = Nothing
    , _hitNumberOfAssignmentsCompleted = Nothing
    , _hitReward = Nothing
    , _hitKeywords = Nothing
    , _hitHITLayoutId = Nothing
    , _hitQualificationRequirements = Nothing
    , _hitTitle = Nothing
    , _hitHITId = Nothing
    , _hitHITReviewStatus = Nothing
    , _hitNumberOfAssignmentsAvailable = Nothing
    , _hitDescription = Nothing
    , _hitQuestion = Nothing
    , _hitAssignmentDurationInSeconds = Nothing
    }


-- | The date and time the HIT was created.
hitCreationTime :: Lens' HIT (Maybe UTCTime)
hitCreationTime = lens _hitCreationTime (\ s a -> s{_hitCreationTime = a}) . mapping _Time

-- | The ID of the HIT Group of this HIT.
hitHITGroupId :: Lens' HIT (Maybe Text)
hitHITGroupId = lens _hitHITGroupId (\ s a -> s{_hitHITGroupId = a})

-- | The number of assignments for this HIT that are being previewed or have been accepted by Workers, but have not yet been submitted, returned, or abandoned.
hitNumberOfAssignmentsPending :: Lens' HIT (Maybe Int)
hitNumberOfAssignmentsPending = lens _hitNumberOfAssignmentsPending (\ s a -> s{_hitNumberOfAssignmentsPending = a})

-- | The ID of the HIT type of this HIT
hitHITTypeId :: Lens' HIT (Maybe Text)
hitHITTypeId = lens _hitHITTypeId (\ s a -> s{_hitHITTypeId = a})

-- | The date and time the HIT expires.
hitExpiration :: Lens' HIT (Maybe UTCTime)
hitExpiration = lens _hitExpiration (\ s a -> s{_hitExpiration = a}) . mapping _Time

-- | The amount of time, in seconds, after the Worker submits an assignment for the HIT that the results are automatically approved by Amazon Mechanical Turk. This is the amount of time the Requester has to reject an assignment submitted by a Worker before the assignment is auto-approved and the Worker is paid.
hitAutoApprovalDelayInSeconds :: Lens' HIT (Maybe Integer)
hitAutoApprovalDelayInSeconds = lens _hitAutoApprovalDelayInSeconds (\ s a -> s{_hitAutoApprovalDelayInSeconds = a})

-- | An arbitrary data field the Requester who created the HIT can use. This field is visible only to the creator of the HIT.
hitRequesterAnnotation :: Lens' HIT (Maybe Text)
hitRequesterAnnotation = lens _hitRequesterAnnotation (\ s a -> s{_hitRequesterAnnotation = a})

-- | The status of the HIT and its assignments. Valid Values are Assignable | Unassignable | Reviewable | Reviewing | Disposed.
hitHITStatus :: Lens' HIT (Maybe HITStatus)
hitHITStatus = lens _hitHITStatus (\ s a -> s{_hitHITStatus = a})

-- | The number of times the HIT can be accepted and completed before the HIT becomes unavailable.
hitMaxAssignments :: Lens' HIT (Maybe Int)
hitMaxAssignments = lens _hitMaxAssignments (\ s a -> s{_hitMaxAssignments = a})

-- | The number of assignments for this HIT that have been approved or rejected.
hitNumberOfAssignmentsCompleted :: Lens' HIT (Maybe Int)
hitNumberOfAssignmentsCompleted = lens _hitNumberOfAssignmentsCompleted (\ s a -> s{_hitNumberOfAssignmentsCompleted = a})

-- | Undocumented member.
hitReward :: Lens' HIT (Maybe Text)
hitReward = lens _hitReward (\ s a -> s{_hitReward = a})

-- | One or more words or phrases that describe the HIT, separated by commas. Search terms similar to the keywords of a HIT are more likely to have the HIT in the search results.
hitKeywords :: Lens' HIT (Maybe Text)
hitKeywords = lens _hitKeywords (\ s a -> s{_hitKeywords = a})

-- | The ID of the HIT Layout of this HIT.
hitHITLayoutId :: Lens' HIT (Maybe Text)
hitHITLayoutId = lens _hitHITLayoutId (\ s a -> s{_hitHITLayoutId = a})

-- | Conditions that a Worker's Qualifications must meet in order to accept the HIT. A HIT can have between zero and ten Qualification requirements. All requirements must be met in order for a Worker to accept the HIT. Additionally, other actions can be restricted using the @ActionsGuarded@ field on each @QualificationRequirement@ structure.
hitQualificationRequirements :: Lens' HIT [QualificationRequirement]
hitQualificationRequirements = lens _hitQualificationRequirements (\ s a -> s{_hitQualificationRequirements = a}) . _Default . _Coerce

-- | The title of the HIT.
hitTitle :: Lens' HIT (Maybe Text)
hitTitle = lens _hitTitle (\ s a -> s{_hitTitle = a})

-- | A unique identifier for the HIT.
hitHITId :: Lens' HIT (Maybe Text)
hitHITId = lens _hitHITId (\ s a -> s{_hitHITId = a})

-- | Indicates the review status of the HIT. Valid Values are NotReviewed | MarkedForReview | ReviewedAppropriate | ReviewedInappropriate.
hitHITReviewStatus :: Lens' HIT (Maybe HITReviewStatus)
hitHITReviewStatus = lens _hitHITReviewStatus (\ s a -> s{_hitHITReviewStatus = a})

-- | The number of assignments for this HIT that are available for Workers to accept.
hitNumberOfAssignmentsAvailable :: Lens' HIT (Maybe Int)
hitNumberOfAssignmentsAvailable = lens _hitNumberOfAssignmentsAvailable (\ s a -> s{_hitNumberOfAssignmentsAvailable = a})

-- | A general description of the HIT.
hitDescription :: Lens' HIT (Maybe Text)
hitDescription = lens _hitDescription (\ s a -> s{_hitDescription = a})

-- | The data the Worker completing the HIT uses produce the results. This is either either a QuestionForm, HTMLQuestion or an ExternalQuestion data structure.
hitQuestion :: Lens' HIT (Maybe Text)
hitQuestion = lens _hitQuestion (\ s a -> s{_hitQuestion = a})

-- | The length of time, in seconds, that a Worker has to complete the HIT after accepting it.
hitAssignmentDurationInSeconds :: Lens' HIT (Maybe Integer)
hitAssignmentDurationInSeconds = lens _hitAssignmentDurationInSeconds (\ s a -> s{_hitAssignmentDurationInSeconds = a})

instance FromJSON HIT where
        parseJSON
          = withObject "HIT"
              (\ x ->
                 HIT' <$>
                   (x .:? "CreationTime") <*> (x .:? "HITGroupId") <*>
                     (x .:? "NumberOfAssignmentsPending")
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
                     <*> (x .:? "AssignmentDurationInSeconds"))

instance Hashable HIT where

instance NFData HIT where

-- | The HITLayoutParameter data structure defines parameter values used with a HITLayout. A HITLayout is a reusable Amazon Mechanical Turk project template used to provide Human Intelligence Task (HIT) question data for CreateHIT.
--
--
--
-- /See:/ 'hITLayoutParameter' smart constructor.
data HITLayoutParameter = HITLayoutParameter'
  { _hitlpName  :: !Text
  , _hitlpValue :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'HITLayoutParameter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hitlpName' - The name of the parameter in the HITLayout.
--
-- * 'hitlpValue' - The value substituted for the parameter referenced in the HITLayout.
hITLayoutParameter
    :: Text -- ^ 'hitlpName'
    -> Text -- ^ 'hitlpValue'
    -> HITLayoutParameter
hITLayoutParameter pName_ pValue_ =
  HITLayoutParameter' {_hitlpName = pName_, _hitlpValue = pValue_}


-- | The name of the parameter in the HITLayout.
hitlpName :: Lens' HITLayoutParameter Text
hitlpName = lens _hitlpName (\ s a -> s{_hitlpName = a})

-- | The value substituted for the parameter referenced in the HITLayout.
hitlpValue :: Lens' HITLayoutParameter Text
hitlpValue = lens _hitlpValue (\ s a -> s{_hitlpValue = a})

instance Hashable HITLayoutParameter where

instance NFData HITLayoutParameter where

instance ToJSON HITLayoutParameter where
        toJSON HITLayoutParameter'{..}
          = object
              (catMaybes
                 [Just ("Name" .= _hitlpName),
                  Just ("Value" .= _hitlpValue)])

-- | The Locale data structure represents a geographical region or location.
--
--
--
-- /See:/ 'locale' smart constructor.
data Locale = Locale'
  { _lSubdivision :: !(Maybe Text)
  , _lCountry     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Locale' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lSubdivision' - The state or subdivision of the locale. A valid ISO 3166-2 subdivision code. For example, the code WA refers to the state of Washington.
--
-- * 'lCountry' - The country of the locale. Must be a valid ISO 3166 country code. For example, the code US refers to the United States of America.
locale
    :: Text -- ^ 'lCountry'
    -> Locale
locale pCountry_ = Locale' {_lSubdivision = Nothing, _lCountry = pCountry_}


-- | The state or subdivision of the locale. A valid ISO 3166-2 subdivision code. For example, the code WA refers to the state of Washington.
lSubdivision :: Lens' Locale (Maybe Text)
lSubdivision = lens _lSubdivision (\ s a -> s{_lSubdivision = a})

-- | The country of the locale. Must be a valid ISO 3166 country code. For example, the code US refers to the United States of America.
lCountry :: Lens' Locale Text
lCountry = lens _lCountry (\ s a -> s{_lCountry = a})

instance FromJSON Locale where
        parseJSON
          = withObject "Locale"
              (\ x ->
                 Locale' <$>
                   (x .:? "Subdivision") <*> (x .: "Country"))

instance Hashable Locale where

instance NFData Locale where

instance ToJSON Locale where
        toJSON Locale'{..}
          = object
              (catMaybes
                 [("Subdivision" .=) <$> _lSubdivision,
                  Just ("Country" .= _lCountry)])

-- | The NotificationSpecification data structure describes a HIT event notification for a HIT type.
--
--
--
-- /See:/ 'notificationSpecification' smart constructor.
data NotificationSpecification = NotificationSpecification'
  { _nsDestination :: !Text
  , _nsTransport   :: !NotificationTransport
  , _nsVersion     :: !Text
  , _nsEventTypes  :: ![EventType]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'NotificationSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nsDestination' - The target for notification messages. The Destination
