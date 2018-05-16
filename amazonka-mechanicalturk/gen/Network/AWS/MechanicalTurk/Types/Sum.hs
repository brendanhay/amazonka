{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MechanicalTurk.Types.Sum where

import Network.AWS.Prelude

data AssignmentStatus
  = Approved
  | Rejected
  | Submitted
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AssignmentStatus where
    parser = takeLowerText >>= \case
        "approved" -> pure Approved
        "rejected" -> pure Rejected
        "submitted" -> pure Submitted
        e -> fromTextError $ "Failure parsing AssignmentStatus from value: '" <> e
           <> "'. Accepted values: approved, rejected, submitted"

instance ToText AssignmentStatus where
    toText = \case
        Approved -> "Approved"
        Rejected -> "Rejected"
        Submitted -> "Submitted"

instance Hashable     AssignmentStatus
instance NFData       AssignmentStatus
instance ToByteString AssignmentStatus
instance ToQuery      AssignmentStatus
instance ToHeader     AssignmentStatus

instance ToJSON AssignmentStatus where
    toJSON = toJSONText

instance FromJSON AssignmentStatus where
    parseJSON = parseJSONText "AssignmentStatus"

data Comparator
  = DoesNotExist
  | EqualTo
  | Exists
  | GreaterThan
  | GreaterThanOrEqualTo
  | IN
  | LessThan
  | LessThanOrEqualTo
  | NotEqualTo
  | NotIn
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Comparator where
    parser = takeLowerText >>= \case
        "doesnotexist" -> pure DoesNotExist
        "equalto" -> pure EqualTo
        "exists" -> pure Exists
        "greaterthan" -> pure GreaterThan
        "greaterthanorequalto" -> pure GreaterThanOrEqualTo
        "in" -> pure IN
        "lessthan" -> pure LessThan
        "lessthanorequalto" -> pure LessThanOrEqualTo
        "notequalto" -> pure NotEqualTo
        "notin" -> pure NotIn
        e -> fromTextError $ "Failure parsing Comparator from value: '" <> e
           <> "'. Accepted values: doesnotexist, equalto, exists, greaterthan, greaterthanorequalto, in, lessthan, lessthanorequalto, notequalto, notin"

instance ToText Comparator where
    toText = \case
        DoesNotExist -> "DoesNotExist"
        EqualTo -> "EqualTo"
        Exists -> "Exists"
        GreaterThan -> "GreaterThan"
        GreaterThanOrEqualTo -> "GreaterThanOrEqualTo"
        IN -> "In"
        LessThan -> "LessThan"
        LessThanOrEqualTo -> "LessThanOrEqualTo"
        NotEqualTo -> "NotEqualTo"
        NotIn -> "NotIn"

instance Hashable     Comparator
instance NFData       Comparator
instance ToByteString Comparator
instance ToQuery      Comparator
instance ToHeader     Comparator

instance ToJSON Comparator where
    toJSON = toJSONText

instance FromJSON Comparator where
    parseJSON = parseJSONText "Comparator"

data EventType
  = AssignmentAbandoned
  | AssignmentAccepted
  | AssignmentApproved
  | AssignmentRejected
  | AssignmentReturned
  | AssignmentSubmitted
  | HITCreated
  | HITDisposed
  | HITExpired
  | HITExtended
  | HITReviewable
  | Ping
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText EventType where
    parser = takeLowerText >>= \case
        "assignmentabandoned" -> pure AssignmentAbandoned
        "assignmentaccepted" -> pure AssignmentAccepted
        "assignmentapproved" -> pure AssignmentApproved
        "assignmentrejected" -> pure AssignmentRejected
        "assignmentreturned" -> pure AssignmentReturned
        "assignmentsubmitted" -> pure AssignmentSubmitted
        "hitcreated" -> pure HITCreated
        "hitdisposed" -> pure HITDisposed
        "hitexpired" -> pure HITExpired
        "hitextended" -> pure HITExtended
        "hitreviewable" -> pure HITReviewable
        "ping" -> pure Ping
        e -> fromTextError $ "Failure parsing EventType from value: '" <> e
           <> "'. Accepted values: assignmentabandoned, assignmentaccepted, assignmentapproved, assignmentrejected, assignmentreturned, assignmentsubmitted, hitcreated, hitdisposed, hitexpired, hitextended, hitreviewable, ping"

instance ToText EventType where
    toText = \case
        AssignmentAbandoned -> "AssignmentAbandoned"
        AssignmentAccepted -> "AssignmentAccepted"
        AssignmentApproved -> "AssignmentApproved"
        AssignmentRejected -> "AssignmentRejected"
        AssignmentReturned -> "AssignmentReturned"
        AssignmentSubmitted -> "AssignmentSubmitted"
        HITCreated -> "HITCreated"
        HITDisposed -> "HITDisposed"
        HITExpired -> "HITExpired"
        HITExtended -> "HITExtended"
        HITReviewable -> "HITReviewable"
        Ping -> "Ping"

instance Hashable     EventType
instance NFData       EventType
instance ToByteString EventType
instance ToQuery      EventType
instance ToHeader     EventType

instance ToJSON EventType where
    toJSON = toJSONText

data HITAccessActions
  = Accept
  | DiscoverPreviewAndAccept
  | PreviewAndAccept
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText HITAccessActions where
    parser = takeLowerText >>= \case
        "accept" -> pure Accept
        "discoverpreviewandaccept" -> pure DiscoverPreviewAndAccept
        "previewandaccept" -> pure PreviewAndAccept
        e -> fromTextError $ "Failure parsing HITAccessActions from value: '" <> e
           <> "'. Accepted values: accept, discoverpreviewandaccept, previewandaccept"

instance ToText HITAccessActions where
    toText = \case
        Accept -> "Accept"
        DiscoverPreviewAndAccept -> "DiscoverPreviewAndAccept"
        PreviewAndAccept -> "PreviewAndAccept"

instance Hashable     HITAccessActions
instance NFData       HITAccessActions
instance ToByteString HITAccessActions
instance ToQuery      HITAccessActions
instance ToHeader     HITAccessActions

instance ToJSON HITAccessActions where
    toJSON = toJSONText

instance FromJSON HITAccessActions where
    parseJSON = parseJSONText "HITAccessActions"

data HITReviewStatus
  = MarkedForReview
  | NotReviewed
  | ReviewedAppropriate
  | ReviewedInappropriate
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText HITReviewStatus where
    parser = takeLowerText >>= \case
        "markedforreview" -> pure MarkedForReview
        "notreviewed" -> pure NotReviewed
        "reviewedappropriate" -> pure ReviewedAppropriate
        "reviewedinappropriate" -> pure ReviewedInappropriate
        e -> fromTextError $ "Failure parsing HITReviewStatus from value: '" <> e
           <> "'. Accepted values: markedforreview, notreviewed, reviewedappropriate, reviewedinappropriate"

instance ToText HITReviewStatus where
    toText = \case
        MarkedForReview -> "MarkedForReview"
        NotReviewed -> "NotReviewed"
        ReviewedAppropriate -> "ReviewedAppropriate"
        ReviewedInappropriate -> "ReviewedInappropriate"

instance Hashable     HITReviewStatus
instance NFData       HITReviewStatus
instance ToByteString HITReviewStatus
instance ToQuery      HITReviewStatus
instance ToHeader     HITReviewStatus

instance FromJSON HITReviewStatus where
    parseJSON = parseJSONText "HITReviewStatus"

data HITStatus
  = HITSAssignable
  | HITSDisposed
  | HITSReviewable
  | HITSReviewing
  | HITSUnassignable
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText HITStatus where
    parser = takeLowerText >>= \case
        "assignable" -> pure HITSAssignable
        "disposed" -> pure HITSDisposed
        "reviewable" -> pure HITSReviewable
        "reviewing" -> pure HITSReviewing
        "unassignable" -> pure HITSUnassignable
        e -> fromTextError $ "Failure parsing HITStatus from value: '" <> e
           <> "'. Accepted values: assignable, disposed, reviewable, reviewing, unassignable"

instance ToText HITStatus where
    toText = \case
        HITSAssignable -> "Assignable"
        HITSDisposed -> "Disposed"
        HITSReviewable -> "Reviewable"
        HITSReviewing -> "Reviewing"
        HITSUnassignable -> "Unassignable"

instance Hashable     HITStatus
instance NFData       HITStatus
instance ToByteString HITStatus
instance ToQuery      HITStatus
instance ToHeader     HITStatus

instance FromJSON HITStatus where
    parseJSON = parseJSONText "HITStatus"

data NotificationTransport
  = Email
  | SNS
  | Sqs
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText NotificationTransport where
    parser = takeLowerText >>= \case
        "email" -> pure Email
        "sns" -> pure SNS
        "sqs" -> pure Sqs
        e -> fromTextError $ "Failure parsing NotificationTransport from value: '" <> e
           <> "'. Accepted values: email, sns, sqs"

instance ToText NotificationTransport where
    toText = \case
        Email -> "Email"
        SNS -> "SNS"
        Sqs -> "SQS"

instance Hashable     NotificationTransport
instance NFData       NotificationTransport
instance ToByteString NotificationTransport
instance ToQuery      NotificationTransport
instance ToHeader     NotificationTransport

instance ToJSON NotificationTransport where
    toJSON = toJSONText

data NotifyWorkersFailureCode
  = HardFailure
  | SoftFailure
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText NotifyWorkersFailureCode where
    parser = takeLowerText >>= \case
        "hardfailure" -> pure HardFailure
        "softfailure" -> pure SoftFailure
        e -> fromTextError $ "Failure parsing NotifyWorkersFailureCode from value: '" <> e
           <> "'. Accepted values: hardfailure, softfailure"

instance ToText NotifyWorkersFailureCode where
    toText = \case
        HardFailure -> "HardFailure"
        SoftFailure -> "SoftFailure"

instance Hashable     NotifyWorkersFailureCode
instance NFData       NotifyWorkersFailureCode
instance ToByteString NotifyWorkersFailureCode
instance ToQuery      NotifyWorkersFailureCode
instance ToHeader     NotifyWorkersFailureCode

instance FromJSON NotifyWorkersFailureCode where
    parseJSON = parseJSONText "NotifyWorkersFailureCode"

data QualificationStatus
  = Granted
  | Revoked
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText QualificationStatus where
    parser = takeLowerText >>= \case
        "granted" -> pure Granted
        "revoked" -> pure Revoked
        e -> fromTextError $ "Failure parsing QualificationStatus from value: '" <> e
           <> "'. Accepted values: granted, revoked"

instance ToText QualificationStatus where
    toText = \case
        Granted -> "Granted"
        Revoked -> "Revoked"

instance Hashable     QualificationStatus
instance NFData       QualificationStatus
instance ToByteString QualificationStatus
instance ToQuery      QualificationStatus
instance ToHeader     QualificationStatus

instance ToJSON QualificationStatus where
    toJSON = toJSONText

instance FromJSON QualificationStatus where
    parseJSON = parseJSONText "QualificationStatus"

data QualificationTypeStatus
  = Active
  | Inactive
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText QualificationTypeStatus where
    parser = takeLowerText >>= \case
        "active" -> pure Active
        "inactive" -> pure Inactive
        e -> fromTextError $ "Failure parsing QualificationTypeStatus from value: '" <> e
           <> "'. Accepted values: active, inactive"

instance ToText QualificationTypeStatus where
    toText = \case
        Active -> "Active"
        Inactive -> "Inactive"

instance Hashable     QualificationTypeStatus
instance NFData       QualificationTypeStatus
instance ToByteString QualificationTypeStatus
instance ToQuery      QualificationTypeStatus
instance ToHeader     QualificationTypeStatus

instance ToJSON QualificationTypeStatus where
    toJSON = toJSONText

instance FromJSON QualificationTypeStatus where
    parseJSON = parseJSONText "QualificationTypeStatus"

data ReviewActionStatus
  = Cancelled
  | Failed
  | Intended
  | Succeeded
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ReviewActionStatus where
    parser = takeLowerText >>= \case
        "cancelled" -> pure Cancelled
        "failed" -> pure Failed
        "intended" -> pure Intended
        "succeeded" -> pure Succeeded
        e -> fromTextError $ "Failure parsing ReviewActionStatus from value: '" <> e
           <> "'. Accepted values: cancelled, failed, intended, succeeded"

instance ToText ReviewActionStatus where
    toText = \case
        Cancelled -> "Cancelled"
        Failed -> "Failed"
        Intended -> "Intended"
        Succeeded -> "Succeeded"

instance Hashable     ReviewActionStatus
instance NFData       ReviewActionStatus
instance ToByteString ReviewActionStatus
instance ToQuery      ReviewActionStatus
instance ToHeader     ReviewActionStatus

instance FromJSON ReviewActionStatus where
    parseJSON = parseJSONText "ReviewActionStatus"

data ReviewPolicyLevel
  = Assignment
  | Hit
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ReviewPolicyLevel where
    parser = takeLowerText >>= \case
        "assignment" -> pure Assignment
        "hit" -> pure Hit
        e -> fromTextError $ "Failure parsing ReviewPolicyLevel from value: '" <> e
           <> "'. Accepted values: assignment, hit"

instance ToText ReviewPolicyLevel where
    toText = \case
        Assignment -> "Assignment"
        Hit -> "HIT"

instance Hashable     ReviewPolicyLevel
instance NFData       ReviewPolicyLevel
instance ToByteString ReviewPolicyLevel
instance ToQuery      ReviewPolicyLevel
instance ToHeader     ReviewPolicyLevel

instance ToJSON ReviewPolicyLevel where
    toJSON = toJSONText

data ReviewableHITStatus
  = Reviewable
  | Reviewing
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ReviewableHITStatus where
    parser = takeLowerText >>= \case
        "reviewable" -> pure Reviewable
        "reviewing" -> pure Reviewing
        e -> fromTextError $ "Failure parsing ReviewableHITStatus from value: '" <> e
           <> "'. Accepted values: reviewable, reviewing"

instance ToText ReviewableHITStatus where
    toText = \case
        Reviewable -> "Reviewable"
        Reviewing -> "Reviewing"

instance Hashable     ReviewableHITStatus
instance NFData       ReviewableHITStatus
instance ToByteString ReviewableHITStatus
instance ToQuery      ReviewableHITStatus
instance ToHeader     ReviewableHITStatus

instance ToJSON ReviewableHITStatus where
    toJSON = toJSONText
