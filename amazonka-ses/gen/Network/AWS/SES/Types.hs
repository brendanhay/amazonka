{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SES.Types
    (
    -- * Service Configuration
      ses

    -- * Errors
    , _InvalidConfigurationSetException
    , _InvalidSNSDestinationException
    , _CannotDeleteException
    , _RuleDoesNotExistException
    , _MessageRejected
    , _RuleSetDoesNotExistException
    , _MailFromDomainNotVerifiedException
    , _InvalidFirehoseDestinationException
    , _ConfigurationSetAlreadyExistsException
    , _EventDestinationDoesNotExistException
    , _InvalidCloudWatchDestinationException
    , _InvalidLambdaFunctionException
    , _ConfigurationSetDoesNotExistException
    , _InvalidPolicyException
    , _InvalidS3ConfigurationException
    , _InvalidSNSTopicException
    , _EventDestinationAlreadyExistsException
    , _AlreadyExistsException
    , _LimitExceededException

    -- * BehaviorOnMXFailure
    , BehaviorOnMXFailure (..)

    -- * BounceType
    , BounceType (..)

    -- * ConfigurationSetAttribute
    , ConfigurationSetAttribute (..)

    -- * CustomMailFromStatus
    , CustomMailFromStatus (..)

    -- * DimensionValueSource
    , DimensionValueSource (..)

    -- * DsnAction
    , DsnAction (..)

    -- * EventType
    , EventType (..)

    -- * IdentityType
    , IdentityType (..)

    -- * InvocationType
    , InvocationType (..)

    -- * NotificationType
    , NotificationType (..)

    -- * ReceiptFilterPolicy
    , ReceiptFilterPolicy (..)

    -- * SNSActionEncoding
    , SNSActionEncoding (..)

    -- * StopScope
    , StopScope (..)

    -- * TLSPolicy
    , TLSPolicy (..)

    -- * VerificationStatus
    , VerificationStatus (..)

    -- * AddHeaderAction
    , AddHeaderAction
    , addHeaderAction
    , ahaHeaderName
    , ahaHeaderValue

    -- * Body
    , Body
    , body
    , bText
    , bHTML

    -- * BounceAction
    , BounceAction
    , bounceAction
    , baTopicARN
    , baStatusCode
    , baSmtpReplyCode
    , baMessage
    , baSender

    -- * BouncedRecipientInfo
    , BouncedRecipientInfo
    , bouncedRecipientInfo
    , briBounceType
    , briRecipientDsnFields
    , briRecipientARN
    , briRecipient

    -- * CloudWatchDestination
    , CloudWatchDestination
    , cloudWatchDestination
    , cwdDimensionConfigurations

    -- * CloudWatchDimensionConfiguration
    , CloudWatchDimensionConfiguration
    , cloudWatchDimensionConfiguration
    , cwdcDimensionName
    , cwdcDimensionValueSource
    , cwdcDefaultDimensionValue

    -- * ConfigurationSet
    , ConfigurationSet
    , configurationSet
    , csName

    -- * Content
    , Content
    , content
    , cCharset
    , cData

    -- * Destination
    , Destination
    , destination
    , dBCCAddresses
    , dCCAddresses
    , dToAddresses

    -- * EventDestination
    , EventDestination
    , eventDestination
    , edEnabled
    , edKinesisFirehoseDestination
    , edCloudWatchDestination
    , edSNSDestination
    , edName
    , edMatchingEventTypes

    -- * ExtensionField
    , ExtensionField
    , extensionField
    , efName
    , efValue

    -- * IdentityDkimAttributes
    , IdentityDkimAttributes
    , identityDkimAttributes
    , idaDkimTokens
    , idaDkimEnabled
    , idaDkimVerificationStatus

    -- * IdentityMailFromDomainAttributes
    , IdentityMailFromDomainAttributes
    , identityMailFromDomainAttributes
    , imfdaMailFromDomain
    , imfdaMailFromDomainStatus
    , imfdaBehaviorOnMXFailure

    -- * IdentityNotificationAttributes
    , IdentityNotificationAttributes
    , identityNotificationAttributes
    , inaHeadersInDeliveryNotificationsEnabled
    , inaHeadersInComplaintNotificationsEnabled
    , inaHeadersInBounceNotificationsEnabled
    , inaBounceTopic
    , inaComplaintTopic
    , inaDeliveryTopic
    , inaForwardingEnabled

    -- * IdentityVerificationAttributes
    , IdentityVerificationAttributes
    , identityVerificationAttributes
    , ivaVerificationToken
    , ivaVerificationStatus

    -- * KinesisFirehoseDestination
    , KinesisFirehoseDestination
    , kinesisFirehoseDestination
    , kfdIAMRoleARN
    , kfdDeliveryStreamARN

    -- * LambdaAction
    , LambdaAction
    , lambdaAction
    , laInvocationType
    , laTopicARN
    , laFunctionARN

    -- * Message
    , Message
    , message
    , mSubject
    , mBody

    -- * MessageDsn
    , MessageDsn
    , messageDsn
    , mdArrivalDate
    , mdExtensionFields
    , mdReportingMta

    -- * MessageTag
    , MessageTag
    , messageTag
    , mtName
    , mtValue

    -- * RawMessage
    , RawMessage
    , rawMessage
    , rmData

    -- * ReceiptAction
    , ReceiptAction
    , receiptAction
    , raAddHeaderAction
    , raSNSAction
    , raWorkmailAction
    , raBounceAction
    , raLambdaAction
    , raStopAction
    , raS3Action

    -- * ReceiptFilter
    , ReceiptFilter
    , receiptFilter
    , rfName
    , rfIPFilter

    -- * ReceiptIPFilter
    , ReceiptIPFilter
    , receiptIPFilter
    , rifPolicy
    , rifCidr

    -- * ReceiptRule
    , ReceiptRule
    , receiptRule
    , rrScanEnabled
    , rrEnabled
    , rrActions
    , rrRecipients
    , rrTLSPolicy
    , rrName

    -- * ReceiptRuleSetMetadata
    , ReceiptRuleSetMetadata
    , receiptRuleSetMetadata
    , rrsmName
    , rrsmCreatedTimestamp

    -- * RecipientDsnFields
    , RecipientDsnFields
    , recipientDsnFields
    , rdfDiagnosticCode
    , rdfRemoteMta
    , rdfFinalRecipient
    , rdfExtensionFields
    , rdfLastAttemptDate
    , rdfAction
    , rdfStatus

    -- * S3Action
    , S3Action
    , s3Action
    , s3KMSKeyARN
    , s3TopicARN
    , s3ObjectKeyPrefix
    , s3BucketName

    -- * SNSAction
    , SNSAction
    , snsAction
    , saEncoding
    , saTopicARN

    -- * SNSDestination
    , SNSDestination
    , snsDestination
    , sdTopicARN

    -- * SendDataPoint
    , SendDataPoint
    , sendDataPoint
    , sdpRejects
    , sdpComplaints
    , sdpDeliveryAttempts
    , sdpBounces
    , sdpTimestamp

    -- * StopAction
    , StopAction
    , stopAction
    , sTopicARN
    , sScope

    -- * WorkmailAction
    , WorkmailAction
    , workmailAction
    , waTopicARN
    , waOrganizationARN
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.SES.Types.Product
import           Network.AWS.SES.Types.Sum
import           Network.AWS.Sign.V4

-- | API version @2010-12-01@ of the Amazon Simple Email Service SDK configuration.
ses :: Service
ses =
    Service
    { _svcAbbrev = "SES"
    , _svcSigner = v4
    , _svcPrefix = "email"
    , _svcVersion = "2010-12-01"
    , _svcEndpoint = defaultEndpoint ses
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseXMLError "SES"
    , _svcRetry = retry
    }
  where
    retry =
        Exponential
        { _retryBase = 5.0e-2
        , _retryGrowth = 2
        , _retryAttempts = 5
        , _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
          Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
          Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing

-- | Indicates that the configuration set is invalid. See the error message for details.
--
--
_InvalidConfigurationSetException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidConfigurationSetException =
    _MatchServiceError ses "InvalidConfigurationSet" . hasStatus 400

-- | Indicates that the Amazon Simple Notification Service (Amazon SNS) destination is invalid. See the error message for details.
--
--
_InvalidSNSDestinationException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidSNSDestinationException =
    _MatchServiceError ses "InvalidSNSDestination" . hasStatus 400

-- | Indicates that the delete operation could not be completed.
--
--
_CannotDeleteException :: AsError a => Getting (First ServiceError) a ServiceError
_CannotDeleteException = _MatchServiceError ses "CannotDelete" . hasStatus 400

-- | Indicates that the provided receipt rule does not exist.
--
--
_RuleDoesNotExistException :: AsError a => Getting (First ServiceError) a ServiceError
_RuleDoesNotExistException =
    _MatchServiceError ses "RuleDoesNotExist" . hasStatus 400

-- | Indicates that the action failed, and the message could not be sent. Check the error stack for more information about what caused the error.
--
--
_MessageRejected :: AsError a => Getting (First ServiceError) a ServiceError
_MessageRejected = _MatchServiceError ses "MessageRejected" . hasStatus 400

-- | Indicates that the provided receipt rule set does not exist.
--
--
_RuleSetDoesNotExistException :: AsError a => Getting (First ServiceError) a ServiceError
_RuleSetDoesNotExistException =
    _MatchServiceError ses "RuleSetDoesNotExist" . hasStatus 400

-- | Indicates that the message could not be sent because Amazon SES could not read the MX record required to use the specified MAIL FROM domain. For information about editing the custom MAIL FROM domain settings for an identity, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/mail-from-edit.html Amazon SES Developer Guide> .
--
--
_MailFromDomainNotVerifiedException :: AsError a => Getting (First ServiceError) a ServiceError
_MailFromDomainNotVerifiedException =
    _MatchServiceError ses "MailFromDomainNotVerifiedException" . hasStatus 400

-- | Indicates that the Amazon Kinesis Firehose destination is invalid. See the error message for details.
--
--
_InvalidFirehoseDestinationException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidFirehoseDestinationException =
    _MatchServiceError ses "InvalidFirehoseDestination" . hasStatus 400

-- | Indicates that the configuration set could not be created because of a naming conflict.
--
--
_ConfigurationSetAlreadyExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_ConfigurationSetAlreadyExistsException =
    _MatchServiceError ses "ConfigurationSetAlreadyExists" . hasStatus 400

-- | Indicates that the event destination does not exist.
--
--
_EventDestinationDoesNotExistException :: AsError a => Getting (First ServiceError) a ServiceError
_EventDestinationDoesNotExistException =
    _MatchServiceError ses "EventDestinationDoesNotExist" . hasStatus 400

-- | Indicates that the Amazon CloudWatch destination is invalid. See the error message for details.
--
--
_InvalidCloudWatchDestinationException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidCloudWatchDestinationException =
    _MatchServiceError ses "InvalidCloudWatchDestination" . hasStatus 400

-- | Indicates that the provided AWS Lambda function is invalid, or that Amazon SES could not execute the provided function, possibly due to permissions issues. For information about giving permissions, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-permissions.html Amazon SES Developer Guide> .
--
--
_InvalidLambdaFunctionException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidLambdaFunctionException =
    _MatchServiceError ses "InvalidLambdaFunction" . hasStatus 400

-- | Indicates that the configuration set does not exist.
--
--
_ConfigurationSetDoesNotExistException :: AsError a => Getting (First ServiceError) a ServiceError
_ConfigurationSetDoesNotExistException =
    _MatchServiceError ses "ConfigurationSetDoesNotExist" . hasStatus 400

-- | Indicates that the provided policy is invalid. Check the error stack for more information about what caused the error.
--
--
_InvalidPolicyException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidPolicyException =
    _MatchServiceError ses "InvalidPolicy" . hasStatus 400

-- | Indicates that the provided Amazon S3 bucket or AWS KMS encryption key is invalid, or that Amazon SES could not publish to the bucket, possibly due to permissions issues. For information about giving permissions, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-permissions.html Amazon SES Developer Guide> .
--
--
_InvalidS3ConfigurationException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidS3ConfigurationException =
    _MatchServiceError ses "InvalidS3Configuration" . hasStatus 400

-- | Indicates that the provided Amazon SNS topic is invalid, or that Amazon SES could not publish to the topic, possibly due to permissions issues. For information about giving permissions, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-permissions.html Amazon SES Developer Guide> .
--
--
_InvalidSNSTopicException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidSNSTopicException =
    _MatchServiceError ses "InvalidSnsTopic" . hasStatus 400

-- | Indicates that the event destination could not be created because of a naming conflict.
--
--
_EventDestinationAlreadyExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_EventDestinationAlreadyExistsException =
    _MatchServiceError ses "EventDestinationAlreadyExists" . hasStatus 400

-- | Indicates that a resource could not be created because of a naming conflict.
--
--
_AlreadyExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_AlreadyExistsException =
    _MatchServiceError ses "AlreadyExists" . hasStatus 400

-- | Indicates that a resource could not be created because of service limits. For a list of Amazon SES limits, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/limits.html Amazon SES Developer Guide> .
--
--
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException =
    _MatchServiceError ses "LimitExceeded" . hasStatus 400
