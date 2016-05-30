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
    , _CannotDeleteException
    , _RuleDoesNotExistException
    , _MessageRejected
    , _RuleSetDoesNotExistException
    , _MailFromDomainNotVerifiedException
    , _InvalidLambdaFunctionException
    , _InvalidPolicyException
    , _InvalidS3ConfigurationException
    , _InvalidSNSTopicException
    , _AlreadyExistsException
    , _LimitExceededException

    -- * BehaviorOnMXFailure
    , BehaviorOnMXFailure (..)

    -- * BounceType
    , BounceType (..)

    -- * CustomMailFromStatus
    , CustomMailFromStatus (..)

    -- * DsnAction
    , DsnAction (..)

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
    , inaBounceTopic
    , inaComplaintTopic
    , inaDeliveryTopic
    , inaForwardingEnabled

    -- * IdentityVerificationAttributes
    , IdentityVerificationAttributes
    , identityVerificationAttributes
    , ivaVerificationToken
    , ivaVerificationStatus

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
    , rifCIdR

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

-- | API version '2010-12-01' of the Amazon Simple Email Service SDK configuration.
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

-- | Indicates that the delete operation could not be completed.
_CannotDeleteException :: AsError a => Getting (First ServiceError) a ServiceError
_CannotDeleteException = _ServiceError . hasStatus 400 . hasCode "CannotDelete"

-- | Indicates that the provided receipt rule does not exist.
_RuleDoesNotExistException :: AsError a => Getting (First ServiceError) a ServiceError
_RuleDoesNotExistException =
    _ServiceError . hasStatus 400 . hasCode "RuleDoesNotExist"

-- | Indicates that the action failed, and the message could not be sent.
-- Check the error stack for more information about what caused the error.
_MessageRejected :: AsError a => Getting (First ServiceError) a ServiceError
_MessageRejected = _ServiceError . hasStatus 400 . hasCode "MessageRejected"

-- | Indicates that the provided receipt rule set does not exist.
_RuleSetDoesNotExistException :: AsError a => Getting (First ServiceError) a ServiceError
_RuleSetDoesNotExistException =
    _ServiceError . hasStatus 400 . hasCode "RuleSetDoesNotExist"

-- | Indicates that the message could not be sent because Amazon SES could
-- not read the MX record required to use the specified MAIL FROM domain.
-- For information about editing the custom MAIL FROM domain settings for
-- an identity, see the
-- <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/mail-from-edit.html Amazon SES Developer Guide>.
_MailFromDomainNotVerifiedException :: AsError a => Getting (First ServiceError) a ServiceError
_MailFromDomainNotVerifiedException =
    _ServiceError .
    hasStatus 400 . hasCode "MailFromDomainNotVerifiedException"

-- | Indicates that the provided AWS Lambda function is invalid, or that
-- Amazon SES could not execute the provided function, possibly due to
-- permissions issues. For information about giving permissions, see the
-- <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-permissions.html Amazon SES Developer Guide>.
_InvalidLambdaFunctionException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidLambdaFunctionException =
    _ServiceError . hasStatus 400 . hasCode "InvalidLambdaFunction"

-- | Indicates that the provided policy is invalid. Check the error stack for
-- more information about what caused the error.
_InvalidPolicyException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidPolicyException =
    _ServiceError . hasStatus 400 . hasCode "InvalidPolicy"

-- | Indicates that the provided Amazon S3 bucket or AWS KMS encryption key
-- is invalid, or that Amazon SES could not publish to the bucket, possibly
-- due to permissions issues. For information about giving permissions, see
-- the
-- <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-permissions.html Amazon SES Developer Guide>.
_InvalidS3ConfigurationException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidS3ConfigurationException =
    _ServiceError . hasStatus 400 . hasCode "InvalidS3Configuration"

-- | Indicates that the provided Amazon SNS topic is invalid, or that Amazon
-- SES could not publish to the topic, possibly due to permissions issues.
-- For information about giving permissions, see the
-- <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-permissions.html Amazon SES Developer Guide>.
_InvalidSNSTopicException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidSNSTopicException =
    _ServiceError . hasStatus 400 . hasCode "InvalidSnsTopic"

-- | Indicates that a resource could not be created due to a naming conflict.
_AlreadyExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_AlreadyExistsException =
    _ServiceError . hasStatus 400 . hasCode "AlreadyExists"

-- | Indicates that a resource could not be created due to service limits.
-- For a list of Amazon SES limits, see the
-- <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/limits.html Amazon SES Developer Guide>.
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException =
    _ServiceError . hasStatus 400 . hasCode "LimitExceeded"
