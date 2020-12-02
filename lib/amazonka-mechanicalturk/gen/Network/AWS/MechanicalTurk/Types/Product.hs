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
-- * 'nsDestination' - The target for notification messages. The Destination’s format is determined by the specified Transport:      * When Transport is Email, the Destination is your email address.     * When Transport is SQS, the Destination is your queue URL.     * When Transport is SNS, the Destination is the ARN of your topic.
--
-- * 'nsTransport' - The method Amazon Mechanical Turk uses to send the notification. Valid Values: Email | SQS | SNS.
--
-- * 'nsVersion' - The version of the Notification API to use. Valid value is 2006-05-05.
--
-- * 'nsEventTypes' - The list of events that should cause notifications to be sent. Valid Values: AssignmentAccepted | AssignmentAbandoned | AssignmentReturned | AssignmentSubmitted | AssignmentRejected | AssignmentApproved | HITCreated | HITExtended | HITDisposed | HITReviewable | HITExpired | Ping. The Ping event is only valid for the SendTestEventNotification operation.
notificationSpecification
    :: Text -- ^ 'nsDestination'
    -> NotificationTransport -- ^ 'nsTransport'
    -> Text -- ^ 'nsVersion'
    -> NotificationSpecification
notificationSpecification pDestination_ pTransport_ pVersion_ =
  NotificationSpecification'
    { _nsDestination = pDestination_
    , _nsTransport = pTransport_
    , _nsVersion = pVersion_
    , _nsEventTypes = mempty
    }


-- | The target for notification messages. The Destination’s format is determined by the specified Transport:      * When Transport is Email, the Destination is your email address.     * When Transport is SQS, the Destination is your queue URL.     * When Transport is SNS, the Destination is the ARN of your topic.
nsDestination :: Lens' NotificationSpecification Text
nsDestination = lens _nsDestination (\ s a -> s{_nsDestination = a})

-- | The method Amazon Mechanical Turk uses to send the notification. Valid Values: Email | SQS | SNS.
nsTransport :: Lens' NotificationSpecification NotificationTransport
nsTransport = lens _nsTransport (\ s a -> s{_nsTransport = a})

-- | The version of the Notification API to use. Valid value is 2006-05-05.
nsVersion :: Lens' NotificationSpecification Text
nsVersion = lens _nsVersion (\ s a -> s{_nsVersion = a})

-- | The list of events that should cause notifications to be sent. Valid Values: AssignmentAccepted | AssignmentAbandoned | AssignmentReturned | AssignmentSubmitted | AssignmentRejected | AssignmentApproved | HITCreated | HITExtended | HITDisposed | HITReviewable | HITExpired | Ping. The Ping event is only valid for the SendTestEventNotification operation.
nsEventTypes :: Lens' NotificationSpecification [EventType]
nsEventTypes = lens _nsEventTypes (\ s a -> s{_nsEventTypes = a}) . _Coerce

instance Hashable NotificationSpecification where

instance NFData NotificationSpecification where

instance ToJSON NotificationSpecification where
        toJSON NotificationSpecification'{..}
          = object
              (catMaybes
                 [Just ("Destination" .= _nsDestination),
                  Just ("Transport" .= _nsTransport),
                  Just ("Version" .= _nsVersion),
                  Just ("EventTypes" .= _nsEventTypes)])

-- | When MTurk encounters an issue with notifying the Workers you specified, it returns back this object with failure details.
--
--
--
-- /See:/ 'notifyWorkersFailureStatus' smart constructor.
data NotifyWorkersFailureStatus = NotifyWorkersFailureStatus'
  { _nwfsNotifyWorkersFailureMessage :: !(Maybe Text)
  , _nwfsNotifyWorkersFailureCode    :: !(Maybe NotifyWorkersFailureCode)
  , _nwfsWorkerId                    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'NotifyWorkersFailureStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nwfsNotifyWorkersFailureMessage' - A message detailing the reason the Worker could not be notified.
--
-- * 'nwfsNotifyWorkersFailureCode' - Encoded value for the failure type.
--
-- * 'nwfsWorkerId' - The ID of the Worker.
notifyWorkersFailureStatus
    :: NotifyWorkersFailureStatus
notifyWorkersFailureStatus =
  NotifyWorkersFailureStatus'
    { _nwfsNotifyWorkersFailureMessage = Nothing
    , _nwfsNotifyWorkersFailureCode = Nothing
    , _nwfsWorkerId = Nothing
    }


-- | A message detailing the reason the Worker could not be notified.
nwfsNotifyWorkersFailureMessage :: Lens' NotifyWorkersFailureStatus (Maybe Text)
nwfsNotifyWorkersFailureMessage = lens _nwfsNotifyWorkersFailureMessage (\ s a -> s{_nwfsNotifyWorkersFailureMessage = a})

-- | Encoded value for the failure type.
nwfsNotifyWorkersFailureCode :: Lens' NotifyWorkersFailureStatus (Maybe NotifyWorkersFailureCode)
nwfsNotifyWorkersFailureCode = lens _nwfsNotifyWorkersFailureCode (\ s a -> s{_nwfsNotifyWorkersFailureCode = a})

-- | The ID of the Worker.
nwfsWorkerId :: Lens' NotifyWorkersFailureStatus (Maybe Text)
nwfsWorkerId = lens _nwfsWorkerId (\ s a -> s{_nwfsWorkerId = a})

instance FromJSON NotifyWorkersFailureStatus where
        parseJSON
          = withObject "NotifyWorkersFailureStatus"
              (\ x ->
                 NotifyWorkersFailureStatus' <$>
                   (x .:? "NotifyWorkersFailureMessage") <*>
                     (x .:? "NotifyWorkersFailureCode")
                     <*> (x .:? "WorkerId"))

instance Hashable NotifyWorkersFailureStatus where

instance NFData NotifyWorkersFailureStatus where

-- | This data structure is the data type for the AnswerKey parameter of the ScoreMyKnownAnswers/2011-09-01 Review Policy.
--
--
--
-- /See:/ 'parameterMapEntry' smart constructor.
data ParameterMapEntry = ParameterMapEntry'
  { _pmeValues :: !(Maybe [Text])
  , _pmeKey    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ParameterMapEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pmeValues' - The list of answers to the question specified in the MapEntry Key element. The Worker must match all values in order for the answer to be scored correctly.
--
-- * 'pmeKey' - The QuestionID from the HIT that is used to identify which question requires Mechanical Turk to score as part of the ScoreMyKnownAnswers/2011-09-01 Review Policy.
parameterMapEntry
    :: ParameterMapEntry
parameterMapEntry = ParameterMapEntry' {_pmeValues = Nothing, _pmeKey = Nothing}


-- | The list of answers to the question specified in the MapEntry Key element. The Worker must match all values in order for the answer to be scored correctly.
pmeValues :: Lens' ParameterMapEntry [Text]
pmeValues = lens _pmeValues (\ s a -> s{_pmeValues = a}) . _Default . _Coerce

-- | The QuestionID from the HIT that is used to identify which question requires Mechanical Turk to score as part of the ScoreMyKnownAnswers/2011-09-01 Review Policy.
pmeKey :: Lens' ParameterMapEntry (Maybe Text)
pmeKey = lens _pmeKey (\ s a -> s{_pmeKey = a})

instance FromJSON ParameterMapEntry where
        parseJSON
          = withObject "ParameterMapEntry"
              (\ x ->
                 ParameterMapEntry' <$>
                   (x .:? "Values" .!= mempty) <*> (x .:? "Key"))

instance Hashable ParameterMapEntry where

instance NFData ParameterMapEntry where

instance ToJSON ParameterMapEntry where
        toJSON ParameterMapEntry'{..}
          = object
              (catMaybes
                 [("Values" .=) <$> _pmeValues,
                  ("Key" .=) <$> _pmeKey])

-- | Name of the parameter from the Review policy.
--
--
--
-- /See:/ 'policyParameter' smart constructor.
data PolicyParameter = PolicyParameter'
  { _ppValues     :: !(Maybe [Text])
  , _ppMapEntries :: !(Maybe [ParameterMapEntry])
  , _ppKey        :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PolicyParameter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ppValues' - The list of values of the Parameter
--
-- * 'ppMapEntries' - List of ParameterMapEntry objects.
--
-- * 'ppKey' - Name of the parameter from the list of Review Polices.
policyParameter
    :: PolicyParameter
policyParameter =
  PolicyParameter'
    {_ppValues = Nothing, _ppMapEntries = Nothing, _ppKey = Nothing}


-- | The list of values of the Parameter
ppValues :: Lens' PolicyParameter [Text]
ppValues = lens _ppValues (\ s a -> s{_ppValues = a}) . _Default . _Coerce

-- | List of ParameterMapEntry objects.
ppMapEntries :: Lens' PolicyParameter [ParameterMapEntry]
ppMapEntries = lens _ppMapEntries (\ s a -> s{_ppMapEntries = a}) . _Default . _Coerce

-- | Name of the parameter from the list of Review Polices.
ppKey :: Lens' PolicyParameter (Maybe Text)
ppKey = lens _ppKey (\ s a -> s{_ppKey = a})

instance FromJSON PolicyParameter where
        parseJSON
          = withObject "PolicyParameter"
              (\ x ->
                 PolicyParameter' <$>
                   (x .:? "Values" .!= mempty) <*>
                     (x .:? "MapEntries" .!= mempty)
                     <*> (x .:? "Key"))

instance Hashable PolicyParameter where

instance NFData PolicyParameter where

instance ToJSON PolicyParameter where
        toJSON PolicyParameter'{..}
          = object
              (catMaybes
                 [("Values" .=) <$> _ppValues,
                  ("MapEntries" .=) <$> _ppMapEntries,
                  ("Key" .=) <$> _ppKey])

-- | The Qualification data structure represents a Qualification assigned to a user, including the Qualification type and the value (score).
--
--
--
-- /See:/ 'qualification' smart constructor.
data Qualification = Qualification'
  { _qStatus              :: !(Maybe QualificationStatus)
  , _qIntegerValue        :: !(Maybe Int)
  , _qLocaleValue         :: !(Maybe Locale)
  , _qQualificationTypeId :: !(Maybe Text)
  , _qGrantTime           :: !(Maybe POSIX)
  , _qWorkerId            :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Qualification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'qStatus' - The status of the Qualification. Valid values are Granted | Revoked.
--
-- * 'qIntegerValue' - The value (score) of the Qualification, if the Qualification has an integer value.
--
-- * 'qLocaleValue' - Undocumented member.
--
-- * 'qQualificationTypeId' - The ID of the Qualification type for the Qualification.
--
-- * 'qGrantTime' - The date and time the Qualification was granted to the Worker. If the Worker's Qualification was revoked, and then re-granted based on a new Qualification request, GrantTime is the date and time of the last call to the AcceptQualificationRequest operation.
--
-- * 'qWorkerId' - The ID of the Worker who possesses the Qualification.
qualification
    :: Qualification
qualification =
  Qualification'
    { _qStatus = Nothing
    , _qIntegerValue = Nothing
    , _qLocaleValue = Nothing
    , _qQualificationTypeId = Nothing
    , _qGrantTime = Nothing
    , _qWorkerId = Nothing
    }


-- | The status of the Qualification. Valid values are Granted | Revoked.
qStatus :: Lens' Qualification (Maybe QualificationStatus)
qStatus = lens _qStatus (\ s a -> s{_qStatus = a})

-- | The value (score) of the Qualification, if the Qualification has an integer value.
qIntegerValue :: Lens' Qualification (Maybe Int)
qIntegerValue = lens _qIntegerValue (\ s a -> s{_qIntegerValue = a})

-- | Undocumented member.
qLocaleValue :: Lens' Qualification (Maybe Locale)
qLocaleValue = lens _qLocaleValue (\ s a -> s{_qLocaleValue = a})

-- | The ID of the Qualification type for the Qualification.
qQualificationTypeId :: Lens' Qualification (Maybe Text)
qQualificationTypeId = lens _qQualificationTypeId (\ s a -> s{_qQualificationTypeId = a})

-- | The date and time the Qualification was granted to the Worker. If the Worker's Qualification was revoked, and then re-granted based on a new Qualification request, GrantTime is the date and time of the last call to the AcceptQualificationRequest operation.
qGrantTime :: Lens' Qualification (Maybe UTCTime)
qGrantTime = lens _qGrantTime (\ s a -> s{_qGrantTime = a}) . mapping _Time

-- | The ID of the Worker who possesses the Qualification.
qWorkerId :: Lens' Qualification (Maybe Text)
qWorkerId = lens _qWorkerId (\ s a -> s{_qWorkerId = a})

instance FromJSON Qualification where
        parseJSON
          = withObject "Qualification"
              (\ x ->
                 Qualification' <$>
                   (x .:? "Status") <*> (x .:? "IntegerValue") <*>
                     (x .:? "LocaleValue")
                     <*> (x .:? "QualificationTypeId")
                     <*> (x .:? "GrantTime")
                     <*> (x .:? "WorkerId"))

instance Hashable Qualification where

instance NFData Qualification where

-- | The QualificationRequest data structure represents a request a Worker has made for a Qualification.
--
--
--
-- /See:/ 'qualificationRequest' smart constructor.
data QualificationRequest = QualificationRequest'
  { _quaQualificationRequestId :: !(Maybe Text)
  , _quaTest                   :: !(Maybe Text)
  , _quaQualificationTypeId    :: !(Maybe Text)
  , _quaAnswer                 :: !(Maybe Text)
  , _quaWorkerId               :: !(Maybe Text)
  , _quaSubmitTime             :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'QualificationRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'quaQualificationRequestId' - The ID of the Qualification request, a unique identifier generated when the request was submitted.
--
-- * 'quaTest' - The contents of the Qualification test that was presented to the Worker, if the type has a test and the Worker has submitted answers. This value is identical to the QuestionForm associated with the Qualification type at the time the Worker requests the Qualification.
--
-- * 'quaQualificationTypeId' - The ID of the Qualification type the Worker is requesting, as returned by the CreateQualificationType operation.
--
-- * 'quaAnswer' - The Worker's answers for the Qualification type's test contained in a QuestionFormAnswers document, if the type has a test and the Worker has submitted answers. If the Worker does not provide any answers, Answer may be empty.
--
-- * 'quaWorkerId' - The ID of the Worker requesting the Qualification.
--
-- * 'quaSubmitTime' - The date and time the Qualification request had a status of Submitted. This is either the time the Worker submitted answers for a Qualification test, or the time the Worker requested the Qualification if the Qualification type does not have a test.
qualificationRequest
    :: QualificationRequest
qualificationRequest =
  QualificationRequest'
    { _quaQualificationRequestId = Nothing
    , _quaTest = Nothing
    , _quaQualificationTypeId = Nothing
    , _quaAnswer = Nothing
    , _quaWorkerId = Nothing
    , _quaSubmitTime = Nothing
    }


-- | The ID of the Qualification request, a unique identifier generated when the request was submitted.
quaQualificationRequestId :: Lens' QualificationRequest (Maybe Text)
quaQualificationRequestId = lens _quaQualificationRequestId (\ s a -> s{_quaQualificationRequestId = a})

-- | The contents of the Qualification test that was presented to the Worker, if the type has a test and the Worker has submitted answers. This value is identical to the QuestionForm associated with the Qualification type at the time the Worker requests the Qualification.
quaTest :: Lens' QualificationRequest (Maybe Text)
quaTest = lens _quaTest (\ s a -> s{_quaTest = a})

-- | The ID of the Qualification type the Worker is requesting, as returned by the CreateQualificationType operation.
quaQualificationTypeId :: Lens' QualificationRequest (Maybe Text)
quaQualificationTypeId = lens _quaQualificationTypeId (\ s a -> s{_quaQualificationTypeId = a})

-- | The Worker's answers for the Qualification type's test contained in a QuestionFormAnswers document, if the type has a test and the Worker has submitted answers. If the Worker does not provide any answers, Answer may be empty.
quaAnswer :: Lens' QualificationRequest (Maybe Text)
quaAnswer = lens _quaAnswer (\ s a -> s{_quaAnswer = a})

-- | The ID of the Worker requesting the Qualification.
quaWorkerId :: Lens' QualificationRequest (Maybe Text)
quaWorkerId = lens _quaWorkerId (\ s a -> s{_quaWorkerId = a})

-- | The date and time the Qualification request had a status of Submitted. This is either the time the Worker submitted answers for a Qualification test, or the time the Worker requested the Qualification if the Qualification type does not have a test.
quaSubmitTime :: Lens' QualificationRequest (Maybe UTCTime)
quaSubmitTime = lens _quaSubmitTime (\ s a -> s{_quaSubmitTime = a}) . mapping _Time

instance FromJSON QualificationRequest where
        parseJSON
          = withObject "QualificationRequest"
              (\ x ->
                 QualificationRequest' <$>
                   (x .:? "QualificationRequestId") <*> (x .:? "Test")
                     <*> (x .:? "QualificationTypeId")
                     <*> (x .:? "Answer")
                     <*> (x .:? "WorkerId")
                     <*> (x .:? "SubmitTime"))

instance Hashable QualificationRequest where

instance NFData QualificationRequest where

-- | The QualificationRequirement data structure describes a Qualification that a Worker must have before the Worker is allowed to accept a HIT. A requirement may optionally state that a Worker must have the Qualification in order to preview the HIT, or see the HIT in search results.
--
--
--
-- /See:/ 'qualificationRequirement' smart constructor.
data QualificationRequirement = QualificationRequirement'
  { _qrLocaleValues        :: !(Maybe [Locale])
  , _qrActionsGuarded      :: !(Maybe HITAccessActions)
  , _qrRequiredToPreview   :: !(Maybe Bool)
  , _qrIntegerValues       :: !(Maybe [Int])
  , _qrQualificationTypeId :: !Text
  , _qrComparator          :: !Comparator
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'QualificationRequirement' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'qrLocaleValues' - The locale value to compare against the Qualification's value. The local value must be a valid ISO 3166 country code or supports ISO 3166-2 subdivisions. LocaleValue can only be used with a Worker_Locale QualificationType ID. LocaleValue can only be used with the EqualTo, NotEqualTo, In, and NotIn comparators. You must only use a single LocaleValue element when using the EqualTo or NotEqualTo comparators. When performing a set comparison by using the In or the NotIn comparator, you can use up to 30 LocaleValue elements in a QualificationRequirement data structure.
--
-- * 'qrActionsGuarded' - Setting this attribute prevents Workers whose Qualifications do not meet this QualificationRequirement from taking the specified action. Valid arguments include "Accept" (Worker cannot accept the HIT, but can preview the HIT and see it in their search results), "PreviewAndAccept" (Worker cannot accept or preview the HIT, but can see the HIT in their search results), and "DiscoverPreviewAndAccept" (Worker cannot accept, preview, or see the HIT in their search results). It's possible for you to create a HIT with multiple QualificationRequirements (which can have different values for the ActionGuarded attribute). In this case, the Worker is only permitted to perform an action when they have met all QualificationRequirements guarding the action. The actions in the order of least restrictive to most restrictive are Discover, Preview and Accept. For example, if a Worker meets all QualificationRequirements that are set to DiscoverPreviewAndAccept, but do not meet all requirements that are set with PreviewAndAccept, then the Worker will be able to Discover, i.e. see the HIT in their search result, but will not be able to Preview or Accept the HIT. ActionsGuarded should not be used in combination with the @RequiredToPreview@ field.
--
-- * 'qrRequiredToPreview' - DEPRECATED: Use the @ActionsGuarded@ field instead. If RequiredToPreview is true, the question data for the HIT will not be shown when a Worker whose Qualifications do not meet this requirement tries to preview the HIT. That is, a Worker's Qualifications must meet all of the requirements for which RequiredToPreview is true in order to preview the HIT. If a Worker meets all of the requirements where RequiredToPreview is true (or if there are no such requirements), but does not meet all of the requirements for the HIT, the Worker will be allowed to preview the HIT's question data, but will not be allowed to accept and complete the HIT. The default is false. This should not be used in combination with the @ActionsGuarded@ field.
--
-- * 'qrIntegerValues' - The integer value to compare against the Qualification's value. IntegerValue must not be present if Comparator is Exists or DoesNotExist. IntegerValue can only be used if the Qualification type has an integer value; it cannot be used with the Worker_Locale QualificationType ID. When performing a set comparison by using the In or the NotIn comparator, you can use up to 15 IntegerValue elements in a QualificationRequirement data structure.
--
-- * 'qrQualificationTypeId' - The ID of the Qualification type for the requirement.
--
-- * 'qrComparator' - The kind of comparison to make against a Qualification's value. You can compare a Qualification's value to an IntegerValue to see if it is LessThan, LessThanOrEqualTo, GreaterThan, GreaterThanOrEqualTo, EqualTo, or NotEqualTo the IntegerValue. You can compare it to a LocaleValue to see if it is EqualTo, or NotEqualTo the LocaleValue. You can check to see if the value is In or NotIn a set of IntegerValue or LocaleValue values. Lastly, a Qualification requirement can also test if a Qualification Exists or DoesNotExist in the user's profile, regardless of its value.
qualificationRequirement
    :: Text -- ^ 'qrQualificationTypeId'
    -> Comparator -- ^ 'qrComparator'
    -> QualificationRequirement
qualificationRequirement pQualificationTypeId_ pComparator_ =
  QualificationRequirement'
    { _qrLocaleValues = Nothing
    , _qrActionsGuarded = Nothing
    , _qrRequiredToPreview = Nothing
    , _qrIntegerValues = Nothing
    , _qrQualificationTypeId = pQualificationTypeId_
    , _qrComparator = pComparator_
    }


-- | The locale value to compare against the Qualification's value. The local value must be a valid ISO 3166 country code or supports ISO 3166-2 subdivisions. LocaleValue can only be used with a Worker_Locale QualificationType ID. LocaleValue can only be used with the EqualTo, NotEqualTo, In, and NotIn comparators. You must only use a single LocaleValue element when using the EqualTo or NotEqualTo comparators. When performing a set comparison by using the In or the NotIn comparator, you can use up to 30 LocaleValue elements in a QualificationRequirement data structure.
qrLocaleValues :: Lens' QualificationRequirement [Locale]
qrLocaleValues = lens _qrLocaleValues (\ s a -> s{_qrLocaleValues = a}) . _Default . _Coerce

-- | Setting this attribute prevents Workers whose Qualifications do not meet this QualificationRequirement from taking the specified action. Valid arguments include "Accept" (Worker cannot accept the HIT, but can preview the HIT and see it in their search results), "PreviewAndAccept" (Worker cannot accept or preview the HIT, but can see the HIT in their search results), and "DiscoverPreviewAndAccept" (Worker cannot accept, preview, or see the HIT in their search results). It's possible for you to create a HIT with multiple QualificationRequirements (which can have different values for the ActionGuarded attribute). In this case, the Worker is only permitted to perform an action when they have met all QualificationRequirements guarding the action. The actions in the order of least restrictive to most restrictive are Discover, Preview and Accept. For example, if a Worker meets all QualificationRequirements that are set to DiscoverPreviewAndAccept, but do not meet all requirements that are set with PreviewAndAccept, then the Worker will be able to Discover, i.e. see the HIT in their search result, but will not be able to Preview or Accept the HIT. ActionsGuarded should not be used in combination with the @RequiredToPreview@ field.
qrActionsGuarded :: Lens' QualificationRequirement (Maybe HITAccessActions)
qrActionsGuarded = lens _qrActionsGuarded (\ s a -> s{_qrActionsGuarded = a})

-- | DEPRECATED: Use the @ActionsGuarded@ field instead. If RequiredToPreview is true, the question data for the HIT will not be shown when a Worker whose Qualifications do not meet this requirement tries to preview the HIT. That is, a Worker's Qualifications must meet all of the requirements for which RequiredToPreview is true in order to preview the HIT. If a Worker meets all of the requirements where RequiredToPreview is true (or if there are no such requirements), but does not meet all of the requirements for the HIT, the Worker will be allowed to preview the HIT's question data, but will not be allowed to accept and complete the HIT. The default is false. This should not be used in combination with the @ActionsGuarded@ field.
qrRequiredToPreview :: Lens' QualificationRequirement (Maybe Bool)
qrRequiredToPreview = lens _qrRequiredToPreview (\ s a -> s{_qrRequiredToPreview = a})

-- | The integer value to compare against the Qualification's value. IntegerValue must not be present if Comparator is Exists or DoesNotExist. IntegerValue can only be used if the Qualification type has an integer value; it cannot be used with the Worker_Locale QualificationType ID. When performing a set comparison by using the In or the NotIn comparator, you can use up to 15 IntegerValue elements in a QualificationRequirement data structure.
qrIntegerValues :: Lens' QualificationRequirement [Int]
qrIntegerValues = lens _qrIntegerValues (\ s a -> s{_qrIntegerValues = a}) . _Default . _Coerce

-- | The ID of the Qualification type for the requirement.
qrQualificationTypeId :: Lens' QualificationRequirement Text
qrQualificationTypeId = lens _qrQualificationTypeId (\ s a -> s{_qrQualificationTypeId = a})

-- | The kind of comparison to make against a Qualification's value. You can compare a Qualification's value to an IntegerValue to see if it is LessThan, LessThanOrEqualTo, GreaterThan, GreaterThanOrEqualTo, EqualTo, or NotEqualTo the IntegerValue. You can compare it to a LocaleValue to see if it is EqualTo, or NotEqualTo the LocaleValue. You can check to see if the value is In or NotIn a set of IntegerValue or LocaleValue values. Lastly, a Qualification requirement can also test if a Qualification Exists or DoesNotExist in the user's profile, regardless of its value.
qrComparator :: Lens' QualificationRequirement Comparator
qrComparator = lens _qrComparator (\ s a -> s{_qrComparator = a})

instance FromJSON QualificationRequirement where
        parseJSON
          = withObject "QualificationRequirement"
              (\ x ->
                 QualificationRequirement' <$>
                   (x .:? "LocaleValues" .!= mempty) <*>
                     (x .:? "ActionsGuarded")
                     <*> (x .:? "RequiredToPreview")
                     <*> (x .:? "IntegerValues" .!= mempty)
                     <*> (x .: "QualificationTypeId")
                     <*> (x .: "Comparator"))

instance Hashable QualificationRequirement where

instance NFData QualificationRequirement where

instance ToJSON QualificationRequirement where
        toJSON QualificationRequirement'{..}
          = object
              (catMaybes
                 [("LocaleValues" .=) <$> _qrLocaleValues,
                  ("ActionsGuarded" .=) <$> _qrActionsGuarded,
                  ("RequiredToPreview" .=) <$> _qrRequiredToPreview,
                  ("IntegerValues" .=) <$> _qrIntegerValues,
                  Just
                    ("QualificationTypeId" .= _qrQualificationTypeId),
                  Just ("Comparator" .= _qrComparator)])

-- | The QualificationType data structure represents a Qualification type, a description of a property of a Worker that must match the requirements of a HIT for the Worker to be able to accept the HIT. The type also describes how a Worker can obtain a Qualification of that type, such as through a Qualification test.
--
--
--
-- /See:/ 'qualificationType' smart constructor.
data QualificationType = QualificationType'
  { _qtCreationTime            :: !(Maybe POSIX)
  , _qtTestDurationInSeconds   :: !(Maybe Integer)
  , _qtQualificationTypeStatus :: !(Maybe QualificationTypeStatus)
  , _qtAnswerKey               :: !(Maybe Text)
  , _qtTest                    :: !(Maybe Text)
  , _qtQualificationTypeId     :: !(Maybe Text)
  , _qtName                    :: !(Maybe Text)
  , _qtKeywords                :: !(Maybe Text)
  , _qtAutoGranted             :: !(Maybe Bool)
  , _qtAutoGrantedValue        :: !(Maybe Int)
  , _qtDescription             :: !(Maybe Text)
  , _qtIsRequestable           :: !(Maybe Bool)
  , _qtRetryDelayInSeconds     :: !(Maybe Integer)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'QualificationType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'qtCreationTime' - The date and time the Qualification type was created.
--
-- * 'qtTestDurationInSeconds' - The amount of time, in seconds, given to a Worker to complete the Qualification test, beginning from the time the Worker requests the Qualification.
--
-- * 'qtQualificationTypeStatus' - The status of the Qualification type. A Qualification type's status determines if users can apply to receive a Qualification of this type, and if HITs can be created with requirements based on this type. Valid values are Active | Inactive.
--
-- * 'qtAnswerKey' - The answers to the Qualification test specified in the Test parameter.
--
-- * 'qtTest' - The questions for a Qualification test associated with this Qualification type that a user can take to obtain a Qualification of this type. This parameter must be specified if AnswerKey is present. A Qualification type cannot have both a specified Test parameter and an AutoGranted value of true.
--
-- * 'qtQualificationTypeId' - A unique identifier for the Qualification type. A Qualification type is given a Qualification type ID when you call the CreateQualificationType operation.
--
-- * 'qtName' - The name of the Qualification type. The type name is used to identify the type, and to find the type using a Qualification type search.
--
-- * 'qtKeywords' - One or more words or phrases that describe theQualification type, separated by commas. The Keywords make the type easier to find using a search.
--
-- * 'qtAutoGranted' - Specifies that requests for the Qualification type are granted immediately, without prompting the Worker with a Qualification test. Valid values are True | False.
--
-- * 'qtAutoGrantedValue' - The Qualification integer value to use for automatically granted Qualifications, if AutoGranted is true. This is 1 by default.
--
-- * 'qtDescription' - A long description for the Qualification type.
--
-- * 'qtIsRequestable' - Specifies whether the Qualification type is one that a user can request through the Amazon Mechanical Turk web site, such as by taking a Qualification test. This value is False for Qualifications assigned automatically by the system. Valid values are True | False.
--
-- * 'qtRetryDelayInSeconds' - The amount of time, in seconds, Workers must wait after taking the Qualification test before they can take it again. Workers can take a Qualification test multiple times if they were not granted the Qualification from a previous attempt, or if the test offers a gradient score and they want a better score. If not specified, retries are disabled and Workers can request a Qualification only once.
qualificationType
    :: QualificationType
qualificationType =
  QualificationType'
    { _qtCreationTime = Nothing
    , _qtTestDurationInSeconds = Nothing
    , _qtQualificationTypeStatus = Nothing
    , _qtAnswerKey = Nothing
    , _qtTest = Nothing
    , _qtQualificationTypeId = Nothing
    , _qtName = Nothing
    , _qtKeywords = Nothing
    , _qtAutoGranted = Nothing
    , _qtAutoGrantedValue = Nothing
    , _qtDescription = Nothing
    , _qtIsRequestable = Nothing
    , _qtRetryDelayInSeconds = Nothing
    }


-- | The date and time the Qualification type was created.
qtCreationTime :: Lens' QualificationType (Maybe UTCTime)
qtCreationTime = lens _qtCreationTime (\ s a -> s{_qtCreationTime = a}) . mapping _Time

-- | The amount of time, in seconds, given to a Worker to complete the Qualification test, beginning from the time the Worker requests the Qualification.
qtTestDurationInSeconds :: Lens' QualificationType (Maybe Integer)
qtTestDurationInSeconds = lens _qtTestDurationInSeconds (\ s a -> s{_qtTestDurationInSeconds = a})

-- | The status of the Qualification type. A Qualification type's status determines if users can apply to receive a Qualification of this type, and if HITs can be created with requirements based on this type. Valid values are Active | Inactive.
qtQualificationTypeStatus :: Lens' QualificationType (Maybe QualificationTypeStatus)
qtQualificationTypeStatus = lens _qtQualificationTypeStatus (\ s a -> s{_qtQualificationTypeStatus = a})

-- | The answers to the Qualification test specified in the Test parameter.
qtAnswerKey :: Lens' QualificationType (Maybe Text)
qtAnswerKey = lens _qtAnswerKey (\ s a -> s{_qtAnswerKey = a})

-- | The questions for a Qualification test associated with this Qualification type that a user can take to obtain a Qualification of this type. This parameter must be specified if AnswerKey is present. A Qualification type cannot have both a specified Test parameter and an AutoGranted value of true.
qtTest :: Lens' QualificationType (Maybe Text)
qtTest = lens _qtTest (\ s a -> s{_qtTest = a})

-- | A unique identifier for the Qualification type. A Qualification type is given a Qualification type ID when you call the CreateQualificationType operation.
qtQualificationTypeId :: Lens' QualificationType (Maybe Text)
qtQualificationTypeId = lens _qtQualificationTypeId (\ s a -> s{_qtQualificationTypeId = a})

-- | The name of the Qualification type. The type name is used to identify the type, and to find the type using a Qualification type search.
qtName :: Lens' QualificationType (Maybe Text)
qtName = lens _qtName (\ s a -> s{_qtName = a})

-- | One or more words or phrases that describe theQualification type, separated by commas. The Keywords make the type easier to find using a search.
qtKeywords :: Lens' QualificationType (Maybe Text)
qtKeywords = lens _qtKeywords (\ s a -> s{_qtKeywords = a})

-- | Specifies that requests for the Qualification type are granted immediately, without prompting the Worker with a Qualification test. Valid values are True | False.
qtAutoGranted :: Lens' QualificationType (Maybe Bool)
qtAutoGranted = lens _qtAutoGranted (\ s a -> s{_qtAutoGranted = a})

-- | The Qualification integer value to use for automatically granted Qualifications, if AutoGranted is true. This is 1 by default.
qtAutoGrantedValue :: Lens' QualificationType (Maybe Int)
qtAutoGrantedValue = lens _qtAutoGrantedValue (\ s a -> s{_qtAutoGrantedValue = a})

-- | A long description for the Qualification type.
qtDescription :: Lens' QualificationType (Maybe Text)
qtDescription = lens _qtDescription (\ s a -> s{_qtDescription = a})

-- | Specifies whether the Qualification type is one that a user can request through the Amazon Mechanical Turk web site, such as by taking a Qualification test. This value is False for Qualifications assigned automatically by the system. Valid values are True | False.
qtIsRequestable :: Lens' QualificationType (Maybe Bool)
qtIsRequestable = lens _qtIsRequestable (\ s a -> s{_qtIsRequestable = a})

-- | The amount of time, in seconds, Workers must wait after taking the Qualification test before they can take it again. Workers can take a Qualification test multiple times if they were not granted the Qualification from a previous attempt, or if the test offers a gradient score and they want a better score. If not specified, retries are disabled and Workers can request a Qualification only once.
qtRetryDelayInSeconds :: Lens' QualificationType (Maybe Integer)
qtRetryDelayInSeconds = lens _qtRetryDelayInSeconds (\ s a -> s{_qtRetryDelayInSeconds = a})

instance FromJSON QualificationType where
        parseJSON
          = withObject "QualificationType"
              (\ x ->
                 QualificationType' <$>
                   (x .:? "CreationTime") <*>
                     (x .:? "TestDurationInSeconds")
                     <*> (x .:? "QualificationTypeStatus")
                     <*> (x .:? "AnswerKey")
                     <*> (x .:? "Test")
                     <*> (x .:? "QualificationTypeId")
                     <*> (x .:? "Name")
                     <*> (x .:? "Keywords")
                     <*> (x .:? "AutoGranted")
                     <*> (x .:? "AutoGrantedValue")
                     <*> (x .:? "Description")
                     <*> (x .:? "IsRequestable")
                     <*> (x .:? "RetryDelayInSeconds"))

instance Hashable QualificationType where

instance NFData QualificationType where

-- | Both the AssignmentReviewReport and the HITReviewReport elements contains the ReviewActionDetail data structure. This structure is returned multiple times for each action specified in the Review Policy.
--
--
--
-- /See:/ 'reviewActionDetail' smart constructor.
data ReviewActionDetail = ReviewActionDetail'
  { _radStatus       :: !(Maybe ReviewActionStatus)
  , _radTargetId     :: !(Maybe Text)
  , _radActionId     :: !(Maybe Text)
  , _radTargetType   :: !(Maybe Text)
  , _radResult       :: !(Maybe Text)
  , _radActionName   :: !(Maybe Text)
  , _radCompleteTime :: !(Maybe POSIX)
  , _radErrorCode    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReviewActionDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'radStatus' - The current disposition of the action: INTENDED, SUCCEEDED, FAILED, or CANCELLED.
--
-- * 'radTargetId' - The specific HITId or AssignmentID targeted by the action.
--
-- * 'radActionId' - The unique identifier for the action.
--
-- * 'radTargetType' - The type of object in TargetId.
--
-- * 'radResult' - A description of the outcome of the review.
--
-- * 'radActionName' - The nature of the action itself. The Review Policy is responsible for examining the HIT and Assignments, emitting results, and deciding which other actions will be necessary.
--
-- * 'radCompleteTime' - The date when the action was completed.
--
-- * 'radErrorCode' - Present only when the Results have a FAILED Status.
reviewActionDetail
    :: ReviewActionDetail
reviewActionDetail =
  ReviewActionDetail'
    { _radStatus = Nothing
    , _radTargetId = Nothing
    , _radActionId = Nothing
    , _radTargetType = Nothing
    , _radResult = Nothing
    , _radActionName = Nothing
    , _radCompleteTime = Nothing
    , _radErrorCode = Nothing
    }


-- | The current disposition of the action: INTENDED, SUCCEEDED, FAILED, or CANCELLED.
radStatus :: Lens' ReviewActionDetail (Maybe ReviewActionStatus)
radStatus = lens _radStatus (\ s a -> s{_radStatus = a})

-- | The specific HITId or AssignmentID targeted by the action.
radTargetId :: Lens' ReviewActionDetail (Maybe Text)
radTargetId = lens _radTargetId (\ s a -> s{_radTargetId = a})

-- | The unique identifier for the action.
radActionId :: Lens' ReviewActionDetail (Maybe Text)
radActionId = lens _radActionId (\ s a -> s{_radActionId = a})

-- | The type of object in TargetId.
radTargetType :: Lens' ReviewActionDetail (Maybe Text)
radTargetType = lens _radTargetType (\ s a -> s{_radTargetType = a})

-- | A description of the outcome of the review.
radResult :: Lens' ReviewActionDetail (Maybe Text)
radResult = lens _radResult (\ s a -> s{_radResult = a})

-- | The nature of the action itself. The Review Policy is responsible for examining the HIT and Assignments, emitting results, and deciding which other actions will be necessary.
radActionName :: Lens' ReviewActionDetail (Maybe Text)
radActionName = lens _radActionName (\ s a -> s{_radActionName = a})

-- | The date when the action was completed.
radCompleteTime :: Lens' ReviewActionDetail (Maybe UTCTime)
radCompleteTime = lens _radCompleteTime (\ s a -> s{_radCompleteTime = a}) . mapping _Time

-- | Present only when the Results have a FAILED Status.
radErrorCode :: Lens' ReviewActionDetail (Maybe Text)
radErrorCode = lens _radErrorCode (\ s a -> s{_radErrorCode = a})

instance FromJSON ReviewActionDetail where
        parseJSON
          = withObject "ReviewActionDetail"
              (\ x ->
                 ReviewActionDetail' <$>
                   (x .:? "Status") <*> (x .:? "TargetId") <*>
                     (x .:? "ActionId")
                     <*> (x .:? "TargetType")
                     <*> (x .:? "Result")
                     <*> (x .:? "ActionName")
                     <*> (x .:? "CompleteTime")
                     <*> (x .:? "ErrorCode"))

instance Hashable ReviewActionDetail where

instance NFData ReviewActionDetail where

-- | HIT Review Policy data structures represent HIT review policies, which you specify when you create a HIT.
--
--
--
-- /See:/ 'reviewPolicy' smart constructor.
data ReviewPolicy = ReviewPolicy'
  { _rpParameters :: !(Maybe [PolicyParameter])
  , _rpPolicyName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReviewPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rpParameters' - Name of the parameter from the Review policy.
--
-- * 'rpPolicyName' - Name of a Review Policy: SimplePlurality/2011-09-01 or ScoreMyKnownAnswers/2011-09-01
reviewPolicy
    :: Text -- ^ 'rpPolicyName'
    -> ReviewPolicy
reviewPolicy pPolicyName_ =
  ReviewPolicy' {_rpParameters = Nothing, _rpPolicyName = pPolicyName_}


-- | Name of the parameter from the Review policy.
rpParameters :: Lens' ReviewPolicy [PolicyParameter]
rpParameters = lens _rpParameters (\ s a -> s{_rpParameters = a}) . _Default . _Coerce

-- | Name of a Review Policy: SimplePlurality/2011-09-01 or ScoreMyKnownAnswers/2011-09-01
rpPolicyName :: Lens' ReviewPolicy Text
rpPolicyName = lens _rpPolicyName (\ s a -> s{_rpPolicyName = a})

instance FromJSON ReviewPolicy where
        parseJSON
          = withObject "ReviewPolicy"
              (\ x ->
                 ReviewPolicy' <$>
                   (x .:? "Parameters" .!= mempty) <*>
                     (x .: "PolicyName"))

instance Hashable ReviewPolicy where

instance NFData ReviewPolicy where

instance ToJSON ReviewPolicy where
        toJSON ReviewPolicy'{..}
          = object
              (catMaybes
                 [("Parameters" .=) <$> _rpParameters,
                  Just ("PolicyName" .= _rpPolicyName)])

-- | Contains both ReviewResult and ReviewAction elements for a particular HIT.
--
--
--
-- /See:/ 'reviewReport' smart constructor.
data ReviewReport = ReviewReport'
  { _rrReviewActions :: !(Maybe [ReviewActionDetail])
  , _rrReviewResults :: !(Maybe [ReviewResultDetail])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReviewReport' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrReviewActions' - A list of ReviewAction objects for each action specified in the Review Policy.
--
-- * 'rrReviewResults' - A list of ReviewResults objects for each action specified in the Review Policy.
reviewReport
    :: ReviewReport
reviewReport =
  ReviewReport' {_rrReviewActions = Nothing, _rrReviewResults = Nothing}


-- | A list of ReviewAction objects for each action specified in the Review Policy.
rrReviewActions :: Lens' ReviewReport [ReviewActionDetail]
rrReviewActions = lens _rrReviewActions (\ s a -> s{_rrReviewActions = a}) . _Default . _Coerce

-- | A list of ReviewResults objects for each action specified in the Review Policy.
rrReviewResults :: Lens' ReviewReport [ReviewResultDetail]
rrReviewResults = lens _rrReviewResults (\ s a -> s{_rrReviewResults = a}) . _Default . _Coerce

instance FromJSON ReviewReport where
        parseJSON
          = withObject "ReviewReport"
              (\ x ->
                 ReviewReport' <$>
                   (x .:? "ReviewActions" .!= mempty) <*>
                     (x .:? "ReviewResults" .!= mempty))

instance Hashable ReviewReport where

instance NFData ReviewReport where

-- | This data structure is returned multiple times for each result specified in the Review Policy.
--
--
--
-- /See:/ 'reviewResultDetail' smart constructor.
data ReviewResultDetail = ReviewResultDetail'
  { _rrdValue       :: !(Maybe Text)
  , _rrdActionId    :: !(Maybe Text)
  , _rrdSubjectType :: !(Maybe Text)
  , _rrdKey         :: !(Maybe Text)
  , _rrdQuestionId  :: !(Maybe Text)
  , _rrdSubjectId   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReviewResultDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrdValue' - The values of Key provided by the review policies you have selected.
--
-- * 'rrdActionId' - A unique identifier of the Review action result.
--
-- * 'rrdSubjectType' - The type of the object from the SubjectId field.
--
-- * 'rrdKey' - Key identifies the particular piece of reviewed information.
--
-- * 'rrdQuestionId' - Specifies the QuestionId the result is describing. Depending on whether the TargetType is a HIT or Assignment this results could specify multiple values. If TargetType is HIT and QuestionId is absent, then the result describes results of the HIT, including the HIT agreement score. If ObjectType is Assignment and QuestionId is absent, then the result describes the Worker's performance on the HIT.
--
-- * 'rrdSubjectId' - The HITID or AssignmentId about which this result was taken. Note that HIT-level Review Policies will often emit results about both the HIT itself and its Assignments, while Assignment-level review policies generally only emit results about the Assignment itself.
reviewResultDetail
    :: ReviewResultDetail
reviewResultDetail =
  ReviewResultDetail'
    { _rrdValue = Nothing
    , _rrdActionId = Nothing
    , _rrdSubjectType = Nothing
    , _rrdKey = Nothing
    , _rrdQuestionId = Nothing
    , _rrdSubjectId = Nothing
    }


-- | The values of Key provided by the review policies you have selected.
rrdValue :: Lens' ReviewResultDetail (Maybe Text)
rrdValue = lens _rrdValue (\ s a -> s{_rrdValue = a})

-- | A unique identifier of the Review action result.
rrdActionId :: Lens' ReviewResultDetail (Maybe Text)
rrdActionId = lens _rrdActionId (\ s a -> s{_rrdActionId = a})

-- | The type of the object from the SubjectId field.
rrdSubjectType :: Lens' ReviewResultDetail (Maybe Text)
rrdSubjectType = lens _rrdSubjectType (\ s a -> s{_rrdSubjectType = a})

-- | Key identifies the particular piece of reviewed information.
rrdKey :: Lens' ReviewResultDetail (Maybe Text)
rrdKey = lens _rrdKey (\ s a -> s{_rrdKey = a})

-- | Specifies the QuestionId the result is describing. Depending on whether the TargetType is a HIT or Assignment this results could specify multiple values. If TargetType is HIT and QuestionId is absent, then the result describes results of the HIT, including the HIT agreement score. If ObjectType is Assignment and QuestionId is absent, then the result describes the Worker's performance on the HIT.
rrdQuestionId :: Lens' ReviewResultDetail (Maybe Text)
rrdQuestionId = lens _rrdQuestionId (\ s a -> s{_rrdQuestionId = a})

-- | The HITID or AssignmentId about which this result was taken. Note that HIT-level Review Policies will often emit results about both the HIT itself and its Assignments, while Assignment-level review policies generally only emit results about the Assignment itself.
rrdSubjectId :: Lens' ReviewResultDetail (Maybe Text)
rrdSubjectId = lens _rrdSubjectId (\ s a -> s{_rrdSubjectId = a})

instance FromJSON ReviewResultDetail where
        parseJSON
          = withObject "ReviewResultDetail"
              (\ x ->
                 ReviewResultDetail' <$>
                   (x .:? "Value") <*> (x .:? "ActionId") <*>
                     (x .:? "SubjectType")
                     <*> (x .:? "Key")
                     <*> (x .:? "QuestionId")
                     <*> (x .:? "SubjectId"))

instance Hashable ReviewResultDetail where

instance NFData ReviewResultDetail where

-- | The WorkerBlock data structure represents a Worker who has been blocked. It has two elements: the WorkerId and the Reason for the block.
--
--
--
-- /See:/ 'workerBlock' smart constructor.
data WorkerBlock = WorkerBlock'
  { _wbReason   :: !(Maybe Text)
  , _wbWorkerId :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'WorkerBlock' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wbReason' - A message explaining the reason the Worker was blocked.
--
-- * 'wbWorkerId' - The ID of the Worker who accepted the HIT.
workerBlock
    :: WorkerBlock
workerBlock = WorkerBlock' {_wbReason = Nothing, _wbWorkerId = Nothing}


-- | A message explaining the reason the Worker was blocked.
wbReason :: Lens' WorkerBlock (Maybe Text)
wbReason = lens _wbReason (\ s a -> s{_wbReason = a})

-- | The ID of the Worker who accepted the HIT.
wbWorkerId :: Lens' WorkerBlock (Maybe Text)
wbWorkerId = lens _wbWorkerId (\ s a -> s{_wbWorkerId = a})

instance FromJSON WorkerBlock where
        parseJSON
          = withObject "WorkerBlock"
              (\ x ->
                 WorkerBlock' <$>
                   (x .:? "Reason") <*> (x .:? "WorkerId"))

instance Hashable WorkerBlock where

instance NFData WorkerBlock where
