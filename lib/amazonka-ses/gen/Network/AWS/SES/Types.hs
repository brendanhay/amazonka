-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SES.Types
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    , _InvalidConfigurationSetException
    , _InvalidSNSDestinationException
    , _TemplateDoesNotExistException
    , _ConfigurationSetSendingPausedException
    , _CannotDeleteException
    , _ProductionAccessNotGrantedException
    , _RuleDoesNotExistException
    , _MessageRejected
    , _InvalidRenderingParameterException
    , _MissingRenderingAttributeException
    , _FromEmailAddressNotVerifiedException
    , _RuleSetDoesNotExistException
    , _MailFromDomainNotVerifiedException
    , _InvalidFirehoseDestinationException
    , _ConfigurationSetAlreadyExistsException
    , _CustomVerificationEmailInvalidContentException
    , _InvalidTrackingOptionsException
    , _AccountSendingPausedException
    , _EventDestinationDoesNotExistException
    , _CustomVerificationEmailTemplateAlreadyExistsException
    , _CustomVerificationEmailTemplateDoesNotExistException
    , _InvalidCloudWatchDestinationException
    , _InvalidDeliveryOptionsException
    , _InvalidLambdaFunctionException
    , _TrackingOptionsDoesNotExistException
    , _InvalidTemplateException
    , _ConfigurationSetDoesNotExistException
    , _InvalidPolicyException
    , _InvalidS3ConfigurationException
    , _TrackingOptionsAlreadyExistsException
    , _InvalidSnsTopicException
    , _EventDestinationAlreadyExistsException
    , _AlreadyExistsException
    , _LimitExceededException

    -- * Subject
    , Subject (..)

    -- * BulkEmailDestinationStatus
    , BulkEmailDestinationStatus (..)
    , mkBulkEmailDestinationStatus
    , bedsError
    , bedsMessageId
    , bedsStatus

    -- * ReceiptFilterPolicy
    , ReceiptFilterPolicy (..)

    -- * ExtensionFieldName
    , ExtensionFieldName (..)

    -- * Destination
    , Destination (..)
    , mkDestination
    , dBccAddresses
    , dCcAddresses
    , dToAddresses

    -- * HeaderValue
    , HeaderValue (..)

    -- * TemplateName
    , TemplateName (..)

    -- * ReceiptRuleSetName
    , ReceiptRuleSetName (..)

    -- * PolicyName
    , PolicyName (..)

    -- * S3KeyPrefix
    , S3KeyPrefix (..)

    -- * DiagnosticCode
    , DiagnosticCode (..)

    -- * StopScope
    , StopScope (..)

    -- * VerificationToken
    , VerificationToken (..)

    -- * IdentityDkimAttributes
    , IdentityDkimAttributes (..)
    , mkIdentityDkimAttributes
    , idaDkimEnabled
    , idaDkimVerificationStatus
    , idaDkimTokens

    -- * TextPart
    , TextPart (..)

    -- * MessageDsn
    , MessageDsn (..)
    , mkMessageDsn
    , mdReportingMta
    , mdArrivalDate
    , mdExtensionFields

    -- * ReceiptRuleSetMetadata
    , ReceiptRuleSetMetadata (..)
    , mkReceiptRuleSetMetadata
    , rrsmCreatedTimestamp
    , rrsmName

    -- * ReceiptRuleName
    , ReceiptRuleName (..)

    -- * KinesisFirehoseDestination
    , KinesisFirehoseDestination (..)
    , mkKinesisFirehoseDestination
    , kfdIAMRoleARN
    , kfdDeliveryStreamARN

    -- * TemplateMetadata
    , TemplateMetadata (..)
    , mkTemplateMetadata
    , tmCreatedTimestamp
    , tmName

    -- * DsnStatus
    , DsnStatus (..)

    -- * ConfigurationSetName
    , ConfigurationSetName (..)

    -- * BounceSmtpReplyCode
    , BounceSmtpReplyCode (..)

    -- * AddHeaderAction
    , AddHeaderAction (..)
    , mkAddHeaderAction
    , ahaHeaderName
    , ahaHeaderValue

    -- * BulkEmailDestination
    , BulkEmailDestination (..)
    , mkBulkEmailDestination
    , bedDestination
    , bedReplacementTags
    , bedReplacementTemplateData

    -- * SNSAction
    , SNSAction (..)
    , mkSNSAction
    , snsaTopicArn
    , snsaEncoding

    -- * CustomMailFromStatus
    , CustomMailFromStatus (..)

    -- * Body
    , Body (..)
    , mkBody
    , bHtml
    , bText

    -- * EventDestination
    , EventDestination (..)
    , mkEventDestination
    , edName
    , edMatchingEventTypes
    , edCloudWatchDestination
    , edEnabled
    , edKinesisFirehoseDestination
    , edSNSDestination

    -- * CloudWatchDimensionConfiguration
    , CloudWatchDimensionConfiguration (..)
    , mkCloudWatchDimensionConfiguration
    , cwdcDimensionName
    , cwdcDimensionValueSource
    , cwdcDefaultDimensionValue

    -- * DefaultDimensionValue
    , DefaultDimensionValue (..)

    -- * HeaderName
    , HeaderName (..)

    -- * Domain
    , Domain (..)

    -- * InvocationType
    , InvocationType (..)

    -- * FromAddress
    , FromAddress (..)

    -- * BulkEmailStatus
    , BulkEmailStatus (..)

    -- * IdentityVerificationAttributes
    , IdentityVerificationAttributes (..)
    , mkIdentityVerificationAttributes
    , ivaVerificationStatus
    , ivaVerificationToken

    -- * Error
    , Error (..)

    -- * SendDataPoint
    , SendDataPoint (..)
    , mkSendDataPoint
    , sdpBounces
    , sdpComplaints
    , sdpDeliveryAttempts
    , sdpRejects
    , sdpTimestamp

    -- * ExtensionFieldValue
    , ExtensionFieldValue (..)

    -- * SubjectPart
    , SubjectPart (..)

    -- * ReceiptFilterName
    , ReceiptFilterName (..)

    -- * Explanation
    , Explanation (..)

    -- * Address
    , Address (..)

    -- * ConfigurationSetAttribute
    , ConfigurationSetAttribute (..)

    -- * WorkmailAction
    , WorkmailAction (..)
    , mkWorkmailAction
    , waOrganizationArn
    , waTopicArn

    -- * IdentityType
    , IdentityType (..)

    -- * MessageTagName
    , MessageTagName (..)

    -- * DimensionValueSource
    , DimensionValueSource (..)

    -- * IdentityMailFromDomainAttributes
    , IdentityMailFromDomainAttributes (..)
    , mkIdentityMailFromDomainAttributes
    , imfdaMailFromDomain
    , imfdaMailFromDomainStatus
    , imfdaBehaviorOnMXFailure

    -- * BounceAction
    , BounceAction (..)
    , mkBounceAction
    , baSmtpReplyCode
    , baMessage
    , baSender
    , baStatusCode
    , baTopicArn

    -- * LambdaAction
    , LambdaAction (..)
    , mkLambdaAction
    , laFunctionArn
    , laInvocationType
    , laTopicArn

    -- * FailureRedirectionURL
    , FailureRedirectionURL (..)

    -- * MessageTag
    , MessageTag (..)
    , mkMessageTag
    , mtName
    , mtValue

    -- * Content
    , Content (..)
    , mkContent
    , cData
    , cCharset

    -- * BouncedRecipientInfo
    , BouncedRecipientInfo (..)
    , mkBouncedRecipientInfo
    , briRecipient
    , briBounceType
    , briRecipientArn
    , briRecipientDsnFields

    -- * CloudWatchDestination
    , CloudWatchDestination (..)
    , mkCloudWatchDestination
    , cwdDimensionConfigurations

    -- * IdentityNotificationAttributes
    , IdentityNotificationAttributes (..)
    , mkIdentityNotificationAttributes
    , inaBounceTopic
    , inaComplaintTopic
    , inaDeliveryTopic
    , inaForwardingEnabled
    , inaHeadersInBounceNotificationsEnabled
    , inaHeadersInComplaintNotificationsEnabled
    , inaHeadersInDeliveryNotificationsEnabled

    -- * RemoteMta
    , RemoteMta (..)

    -- * NextToken
    , NextToken (..)

    -- * Cidr
    , Cidr (..)

    -- * ReceiptFilter
    , ReceiptFilter (..)
    , mkReceiptFilter
    , rfName
    , rfIpFilter

    -- * EventType
    , EventType (..)

    -- * DeliveryOptions
    , DeliveryOptions (..)
    , mkDeliveryOptions
    , doTlsPolicy

    -- * DsnAction
    , DsnAction (..)

    -- * EventDestinationName
    , EventDestinationName (..)

    -- * TrackingOptions
    , TrackingOptions (..)
    , mkTrackingOptions
    , toCustomRedirectDomain

    -- * DimensionName
    , DimensionName (..)

    -- * RawMessage
    , RawMessage (..)
    , mkRawMessage
    , rmData

    -- * ReceiptRule
    , ReceiptRule (..)
    , mkReceiptRule
    , rrName
    , rrActions
    , rrEnabled
    , rrRecipients
    , rrScanEnabled
    , rrTlsPolicy

    -- * SNSDestination
    , SNSDestination (..)
    , mkSNSDestination
    , snsdTopicARN

    -- * HtmlPart
    , HtmlPart (..)

    -- * Charset
    , Charset (..)

    -- * SuccessRedirectionURL
    , SuccessRedirectionURL (..)

    -- * ReceiptAction
    , ReceiptAction (..)
    , mkReceiptAction
    , raAddHeaderAction
    , raBounceAction
    , raLambdaAction
    , raS3Action
    , raSNSAction
    , raStopAction
    , raWorkmailAction

    -- * ConfigurationSet
    , ConfigurationSet (..)
    , mkConfigurationSet
    , csName

    -- * ReceiptIpFilter
    , ReceiptIpFilter (..)
    , mkReceiptIpFilter
    , rifPolicy
    , rifCidr

    -- * NotificationType
    , NotificationType (..)

    -- * VerificationStatus
    , VerificationStatus (..)

    -- * Policy
    , Policy (..)

    -- * BehaviorOnMXFailure
    , BehaviorOnMXFailure (..)

    -- * Template
    , Template (..)
    , mkTemplate
    , tTemplateName
    , tHtmlPart
    , tSubjectPart
    , tTextPart

    -- * TemplateData
    , TemplateData (..)

    -- * BounceType
    , BounceType (..)

    -- * ReputationOptions
    , ReputationOptions (..)
    , mkReputationOptions
    , roLastFreshStart
    , roReputationMetricsEnabled
    , roSendingEnabled

    -- * BounceMessage
    , BounceMessage (..)

    -- * ExtensionField
    , ExtensionField (..)
    , mkExtensionField
    , efName
    , efValue

    -- * Message
    , Message (..)
    , mkMessage
    , mSubject
    , mBody

    -- * AmazonResourceName
    , AmazonResourceName (..)

    -- * RecipientDsnFields
    , RecipientDsnFields (..)
    , mkRecipientDsnFields
    , rdfAction
    , rdfStatus
    , rdfDiagnosticCode
    , rdfExtensionFields
    , rdfFinalRecipient
    , rdfLastAttemptDate
    , rdfRemoteMta

    -- * SNSActionEncoding
    , SNSActionEncoding (..)

    -- * TlsPolicy
    , TlsPolicy (..)

    -- * Recipient
    , Recipient (..)

    -- * RenderedTemplate
    , RenderedTemplate (..)

    -- * ReportingMta
    , ReportingMta (..)

    -- * CustomVerificationEmailTemplate
    , CustomVerificationEmailTemplate (..)
    , mkCustomVerificationEmailTemplate
    , cvetFailureRedirectionURL
    , cvetFromEmailAddress
    , cvetSuccessRedirectionURL
    , cvetTemplateName
    , cvetTemplateSubject

    -- * Identity
    , Identity (..)

    -- * StopAction
    , StopAction (..)
    , mkStopAction
    , sScope
    , sTopicArn

    -- * S3Action
    , S3Action (..)
    , mkS3Action
    , saBucketName
    , saKmsKeyArn
    , saObjectKeyPrefix
    , saTopicArn

    -- * MessageId
    , MessageId (..)

    -- * CustomRedirectDomain
    , CustomRedirectDomain (..)

    -- * TemplateContent
    , TemplateContent (..)

    -- * RuleSetName
    , RuleSetName (..)

    -- * Name
    , Name (..)

    -- * MailFromDomain
    , MailFromDomain (..)

    -- * Source
    , Source (..)

    -- * DefaultTemplateData
    , DefaultTemplateData (..)

    -- * ReturnPath
    , ReturnPath (..)

    -- * ReturnPathArn
    , ReturnPathArn (..)

    -- * SourceArn
    , SourceArn (..)

    -- * TemplateArn
    , TemplateArn (..)

    -- * FromArn
    , FromArn (..)

    -- * IAMRoleARN
    , IAMRoleARN (..)

    -- * DeliveryStreamARN
    , DeliveryStreamARN (..)

    -- * OriginalMessageId
    , OriginalMessageId (..)

    -- * BounceSender
    , BounceSender (..)

    -- * BounceSenderArn
    , BounceSenderArn (..)

    -- * ReplacementTemplateData
    , ReplacementTemplateData (..)

    -- * TopicArn
    , TopicArn (..)

    -- * OrganizationArn
    , OrganizationArn (..)

    -- * StatusCode
    , StatusCode (..)

    -- * FunctionArn
    , FunctionArn (..)

    -- * Value
    , Value (..)

    -- * Data
    , Data (..)

    -- * RecipientArn
    , RecipientArn (..)

    -- * SnsTopic
    , SnsTopic (..)

    -- * BounceTopic
    , BounceTopic (..)

    -- * ComplaintTopic
    , ComplaintTopic (..)

    -- * DeliveryTopic
    , DeliveryTopic (..)

    -- * TopicARN
    , TopicARN (..)

    -- * BucketName
    , BucketName (..)
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign
import Network.AWS.SES.Types.Subject
  
  
import Network.AWS.SES.Types.BulkEmailDestinationStatus
  
  
  
import Network.AWS.SES.Types.ReceiptFilterPolicy
  
import Network.AWS.SES.Types.ExtensionFieldName
  
import Network.AWS.SES.Types.Destination
  
import Network.AWS.SES.Types.HeaderValue
  
import Network.AWS.SES.Types.TemplateName
  
  
  
import Network.AWS.SES.Types.ReceiptRuleSetName
  
import Network.AWS.SES.Types.PolicyName
  
import Network.AWS.SES.Types.S3KeyPrefix
  
  
import Network.AWS.SES.Types.DiagnosticCode
  
import Network.AWS.SES.Types.StopScope
  
import Network.AWS.SES.Types.VerificationToken
  
import Network.AWS.SES.Types.IdentityDkimAttributes
  
import Network.AWS.SES.Types.TextPart
  
  
import Network.AWS.SES.Types.MessageDsn
  
import Network.AWS.SES.Types.ReceiptRuleSetMetadata
  
  
import Network.AWS.SES.Types.ReceiptRuleName
  
import Network.AWS.SES.Types.KinesisFirehoseDestination
  
  
import Network.AWS.SES.Types.TemplateMetadata
  
import Network.AWS.SES.Types.DsnStatus
  
  
import Network.AWS.SES.Types.ConfigurationSetName
  
import Network.AWS.SES.Types.BounceSmtpReplyCode
  
import Network.AWS.SES.Types.AddHeaderAction
  
import Network.AWS.SES.Types.BulkEmailDestination
  
import Network.AWS.SES.Types.SNSAction
  
import Network.AWS.SES.Types.CustomMailFromStatus
  
import Network.AWS.SES.Types.Body
  
  
import Network.AWS.SES.Types.EventDestination
  
import Network.AWS.SES.Types.CloudWatchDimensionConfiguration
  
import Network.AWS.SES.Types.DefaultDimensionValue
  
import Network.AWS.SES.Types.HeaderName
  
import Network.AWS.SES.Types.Domain
  
import Network.AWS.SES.Types.InvocationType
  
import Network.AWS.SES.Types.FromAddress
  
import Network.AWS.SES.Types.BulkEmailStatus
  
import Network.AWS.SES.Types.IdentityVerificationAttributes
  
import Network.AWS.SES.Types.Error
  
import Network.AWS.SES.Types.SendDataPoint
  
import Network.AWS.SES.Types.ExtensionFieldValue
  
  
import Network.AWS.SES.Types.SubjectPart
  
import Network.AWS.SES.Types.ReceiptFilterName
  
import Network.AWS.SES.Types.Explanation
  
  
  
  
import Network.AWS.SES.Types.Address
  
import Network.AWS.SES.Types.ConfigurationSetAttribute
  
import Network.AWS.SES.Types.WorkmailAction
  
import Network.AWS.SES.Types.IdentityType
  
import Network.AWS.SES.Types.MessageTagName
  
import Network.AWS.SES.Types.DimensionValueSource
  
import Network.AWS.SES.Types.IdentityMailFromDomainAttributes
  
import Network.AWS.SES.Types.BounceAction
  
import Network.AWS.SES.Types.LambdaAction
  
import Network.AWS.SES.Types.FailureRedirectionURL
  
import Network.AWS.SES.Types.MessageTag
  
import Network.AWS.SES.Types.Content
  
import Network.AWS.SES.Types.BouncedRecipientInfo
  
  
  
import Network.AWS.SES.Types.CloudWatchDestination
  
  
  
import Network.AWS.SES.Types.IdentityNotificationAttributes
  
import Network.AWS.SES.Types.RemoteMta
  
import Network.AWS.SES.Types.NextToken
  
import Network.AWS.SES.Types.Cidr
  
import Network.AWS.SES.Types.ReceiptFilter
  
import Network.AWS.SES.Types.EventType
  
import Network.AWS.SES.Types.DeliveryOptions
  
import Network.AWS.SES.Types.DsnAction
  
import Network.AWS.SES.Types.EventDestinationName
  
import Network.AWS.SES.Types.TrackingOptions
  
  
import Network.AWS.SES.Types.DimensionName
  
import Network.AWS.SES.Types.RawMessage
  
import Network.AWS.SES.Types.ReceiptRule
  
import Network.AWS.SES.Types.SNSDestination
  
  
import Network.AWS.SES.Types.HtmlPart
  
  
import Network.AWS.SES.Types.Charset
  
import Network.AWS.SES.Types.SuccessRedirectionURL
  
import Network.AWS.SES.Types.ReceiptAction
  
import Network.AWS.SES.Types.ConfigurationSet
  
  
import Network.AWS.SES.Types.ReceiptIpFilter
  
import Network.AWS.SES.Types.NotificationType
  
import Network.AWS.SES.Types.VerificationStatus
  
import Network.AWS.SES.Types.Policy
  
import Network.AWS.SES.Types.BehaviorOnMXFailure
  
import Network.AWS.SES.Types.Template
  
import Network.AWS.SES.Types.TemplateData
  
import Network.AWS.SES.Types.BounceType
  
  
import Network.AWS.SES.Types.ReputationOptions
  
import Network.AWS.SES.Types.BounceMessage
  
import Network.AWS.SES.Types.ExtensionField
  
import Network.AWS.SES.Types.Message
  
  
import Network.AWS.SES.Types.AmazonResourceName
  
import Network.AWS.SES.Types.RecipientDsnFields
  
import Network.AWS.SES.Types.SNSActionEncoding
  
import Network.AWS.SES.Types.TlsPolicy
  
import Network.AWS.SES.Types.Recipient
  
import Network.AWS.SES.Types.RenderedTemplate
  
import Network.AWS.SES.Types.ReportingMta
  
import Network.AWS.SES.Types.CustomVerificationEmailTemplate
  
import Network.AWS.SES.Types.Identity
  
import Network.AWS.SES.Types.StopAction
  
  
  
import Network.AWS.SES.Types.S3Action
  
  
  
import Network.AWS.SES.Types.MessageId
  
  
import Network.AWS.SES.Types.CustomRedirectDomain
  
  
import Network.AWS.SES.Types.TemplateContent
  
  
  
  
import Network.AWS.SES.Types.RuleSetName
  
import Network.AWS.SES.Types.Name
  
import Network.AWS.SES.Types.MailFromDomain
  
import Network.AWS.SES.Types.Source
  
import Network.AWS.SES.Types.DefaultTemplateData
  
import Network.AWS.SES.Types.ReturnPath
  
import Network.AWS.SES.Types.ReturnPathArn
  
import Network.AWS.SES.Types.SourceArn
  
import Network.AWS.SES.Types.TemplateArn
  
import Network.AWS.SES.Types.FromArn
  
import Network.AWS.SES.Types.IAMRoleARN
  
import Network.AWS.SES.Types.DeliveryStreamARN
  
import Network.AWS.SES.Types.OriginalMessageId
  
import Network.AWS.SES.Types.BounceSender
  
import Network.AWS.SES.Types.BounceSenderArn
  
import Network.AWS.SES.Types.ReplacementTemplateData
  
import Network.AWS.SES.Types.TopicArn
  
import Network.AWS.SES.Types.OrganizationArn
  
import Network.AWS.SES.Types.StatusCode
  
import Network.AWS.SES.Types.FunctionArn
  
import Network.AWS.SES.Types.Value
  
import Network.AWS.SES.Types.Data
  
import Network.AWS.SES.Types.RecipientArn
  
import Network.AWS.SES.Types.SnsTopic
  
import Network.AWS.SES.Types.BounceTopic
  
import Network.AWS.SES.Types.ComplaintTopic
  
import Network.AWS.SES.Types.DeliveryTopic
  
import Network.AWS.SES.Types.TopicARN
  
import Network.AWS.SES.Types.BucketName
  

-- | API version @2010-12-01@ of the Amazon Simple Email Service SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig
  = Core.Service{Core._svcAbbrev = "SES", Core._svcSigner = Sign.v4,
                 Core._svcPrefix = "email", Core._svcVersion = "2010-12-01",
                 Core._svcTimeout = Core.Just 70,
                 Core._svcCheck = Core.statusSuccess, Core._svcRetry = retry,
                 Core._svcError = Core.parseXMLError "SES",
                 Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig}
  where retry
          = Core.Exponential{Core._retryBase = 5.0e-2, Core._retryGrowth = 2,
                             Core._retryAttempts = 5, Core._retryCheck = check}
        check e
          | Lens.has
              (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttled_exception"
          | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
          | Lens.has
              (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttling_exception"
          | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e
            = Core.Just "throttling"
          | Lens.has
              (Core.hasCode "ProvisionedThroughputExceededException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "throughput_exceeded"
          | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
          | Lens.has
              (Core.hasCode "RequestThrottledException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "request_throttled_exception"
          | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
          | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
          | Lens.has (Core.hasStatus 500) e =
            Core.Just "general_server_error"
          | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
          | Core.otherwise = Core.Nothing

-- | Indicates that the configuration set is invalid. See the error message for details.
_InvalidConfigurationSetException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidConfigurationSetException
  = Core._MatchServiceError mkServiceConfig "InvalidConfigurationSet"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidConfigurationSetException #-}
{-# DEPRECATED _InvalidConfigurationSetException "Use generic-lens or generic-optics instead"  #-}

-- | Indicates that the Amazon Simple Notification Service (Amazon SNS) destination is invalid. See the error message for details.
_InvalidSNSDestinationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidSNSDestinationException
  = Core._MatchServiceError mkServiceConfig "InvalidSNSDestination"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidSNSDestinationException #-}
{-# DEPRECATED _InvalidSNSDestinationException "Use generic-lens or generic-optics instead"  #-}

-- | Indicates that the Template object you specified does not exist in your Amazon SES account.
_TemplateDoesNotExistException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TemplateDoesNotExistException
  = Core._MatchServiceError mkServiceConfig "TemplateDoesNotExist"
      Core.. Core.hasStatues 400
{-# INLINEABLE _TemplateDoesNotExistException #-}
{-# DEPRECATED _TemplateDoesNotExistException "Use generic-lens or generic-optics instead"  #-}

-- | Indicates that email sending is disabled for the configuration set.
--
-- You can enable or disable email sending for a configuration set using 'UpdateConfigurationSetSendingEnabled' .
_ConfigurationSetSendingPausedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConfigurationSetSendingPausedException
  = Core._MatchServiceError mkServiceConfig
      "ConfigurationSetSendingPausedException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _ConfigurationSetSendingPausedException #-}
{-# DEPRECATED _ConfigurationSetSendingPausedException "Use generic-lens or generic-optics instead"  #-}

-- | Indicates that the delete operation could not be completed.
_CannotDeleteException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CannotDeleteException
  = Core._MatchServiceError mkServiceConfig "CannotDelete" Core..
      Core.hasStatues 400
{-# INLINEABLE _CannotDeleteException #-}
{-# DEPRECATED _CannotDeleteException "Use generic-lens or generic-optics instead"  #-}

-- | Indicates that the account has not been granted production access.
_ProductionAccessNotGrantedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ProductionAccessNotGrantedException
  = Core._MatchServiceError mkServiceConfig
      "ProductionAccessNotGranted"
      Core.. Core.hasStatues 400
{-# INLINEABLE _ProductionAccessNotGrantedException #-}
{-# DEPRECATED _ProductionAccessNotGrantedException "Use generic-lens or generic-optics instead"  #-}

-- | Indicates that the provided receipt rule does not exist.
_RuleDoesNotExistException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_RuleDoesNotExistException
  = Core._MatchServiceError mkServiceConfig "RuleDoesNotExist" Core..
      Core.hasStatues 400
{-# INLINEABLE _RuleDoesNotExistException #-}
{-# DEPRECATED _RuleDoesNotExistException "Use generic-lens or generic-optics instead"  #-}

-- | Indicates that the action failed, and the message could not be sent. Check the error stack for more information about what caused the error.
_MessageRejected :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MessageRejected
  = Core._MatchServiceError mkServiceConfig "MessageRejected" Core..
      Core.hasStatues 400
{-# INLINEABLE _MessageRejected #-}
{-# DEPRECATED _MessageRejected "Use generic-lens or generic-optics instead"  #-}

-- | Indicates that one or more of the replacement values you provided is invalid. This error may occur when the TemplateData object contains invalid JSON.
_InvalidRenderingParameterException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidRenderingParameterException
  = Core._MatchServiceError mkServiceConfig
      "InvalidRenderingParameter"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidRenderingParameterException #-}
{-# DEPRECATED _InvalidRenderingParameterException "Use generic-lens or generic-optics instead"  #-}

-- | Indicates that one or more of the replacement values for the specified template was not specified. Ensure that the TemplateData object contains references to all of the replacement tags in the specified template.
_MissingRenderingAttributeException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MissingRenderingAttributeException
  = Core._MatchServiceError mkServiceConfig
      "MissingRenderingAttribute"
      Core.. Core.hasStatues 400
{-# INLINEABLE _MissingRenderingAttributeException #-}
{-# DEPRECATED _MissingRenderingAttributeException "Use generic-lens or generic-optics instead"  #-}

-- | Indicates that the sender address specified for a custom verification email is not verified, and is therefore not eligible to send the custom verification email. 
_FromEmailAddressNotVerifiedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_FromEmailAddressNotVerifiedException
  = Core._MatchServiceError mkServiceConfig
      "FromEmailAddressNotVerified"
      Core.. Core.hasStatues 400
{-# INLINEABLE _FromEmailAddressNotVerifiedException #-}
{-# DEPRECATED _FromEmailAddressNotVerifiedException "Use generic-lens or generic-optics instead"  #-}

-- | Indicates that the provided receipt rule set does not exist.
_RuleSetDoesNotExistException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_RuleSetDoesNotExistException
  = Core._MatchServiceError mkServiceConfig "RuleSetDoesNotExist"
      Core.. Core.hasStatues 400
{-# INLINEABLE _RuleSetDoesNotExistException #-}
{-# DEPRECATED _RuleSetDoesNotExistException "Use generic-lens or generic-optics instead"  #-}

-- | Indicates that the message could not be sent because Amazon SES could not read the MX record required to use the specified MAIL FROM domain. For information about editing the custom MAIL FROM domain settings for an identity, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/mail-from-edit.html Amazon SES Developer Guide> .
_MailFromDomainNotVerifiedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MailFromDomainNotVerifiedException
  = Core._MatchServiceError mkServiceConfig
      "MailFromDomainNotVerifiedException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _MailFromDomainNotVerifiedException #-}
{-# DEPRECATED _MailFromDomainNotVerifiedException "Use generic-lens or generic-optics instead"  #-}

-- | Indicates that the Amazon Kinesis Firehose destination is invalid. See the error message for details.
_InvalidFirehoseDestinationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidFirehoseDestinationException
  = Core._MatchServiceError mkServiceConfig
      "InvalidFirehoseDestination"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidFirehoseDestinationException #-}
{-# DEPRECATED _InvalidFirehoseDestinationException "Use generic-lens or generic-optics instead"  #-}

-- | Indicates that the configuration set could not be created because of a naming conflict.
_ConfigurationSetAlreadyExistsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConfigurationSetAlreadyExistsException
  = Core._MatchServiceError mkServiceConfig
      "ConfigurationSetAlreadyExists"
      Core.. Core.hasStatues 400
{-# INLINEABLE _ConfigurationSetAlreadyExistsException #-}
{-# DEPRECATED _ConfigurationSetAlreadyExistsException "Use generic-lens or generic-optics instead"  #-}

-- | Indicates that custom verification email template provided content is invalid.
_CustomVerificationEmailInvalidContentException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CustomVerificationEmailInvalidContentException
  = Core._MatchServiceError mkServiceConfig
      "CustomVerificationEmailInvalidContent"
      Core.. Core.hasStatues 400
{-# INLINEABLE _CustomVerificationEmailInvalidContentException #-}
{-# DEPRECATED _CustomVerificationEmailInvalidContentException "Use generic-lens or generic-optics instead"  #-}

-- | Indicates that the custom domain to be used for open and click tracking redirects is invalid. This error appears most often in the following situations:
--
--
--     * When the tracking domain you specified is not verified in Amazon SES.
--
--
--     * When the tracking domain you specified is not a valid domain or subdomain.
--
--
_InvalidTrackingOptionsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidTrackingOptionsException
  = Core._MatchServiceError mkServiceConfig "InvalidTrackingOptions"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidTrackingOptionsException #-}
{-# DEPRECATED _InvalidTrackingOptionsException "Use generic-lens or generic-optics instead"  #-}

-- | Indicates that email sending is disabled for your entire Amazon SES account.
--
-- You can enable or disable email sending for your Amazon SES account using 'UpdateAccountSendingEnabled' .
_AccountSendingPausedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AccountSendingPausedException
  = Core._MatchServiceError mkServiceConfig
      "AccountSendingPausedException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _AccountSendingPausedException #-}
{-# DEPRECATED _AccountSendingPausedException "Use generic-lens or generic-optics instead"  #-}

-- | Indicates that the event destination does not exist.
_EventDestinationDoesNotExistException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_EventDestinationDoesNotExistException
  = Core._MatchServiceError mkServiceConfig
      "EventDestinationDoesNotExist"
      Core.. Core.hasStatues 400
{-# INLINEABLE _EventDestinationDoesNotExistException #-}
{-# DEPRECATED _EventDestinationDoesNotExistException "Use generic-lens or generic-optics instead"  #-}

-- | Indicates that a custom verification email template with the name you specified already exists.
_CustomVerificationEmailTemplateAlreadyExistsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CustomVerificationEmailTemplateAlreadyExistsException
  = Core._MatchServiceError mkServiceConfig
      "CustomVerificationEmailTemplateAlreadyExists"
      Core.. Core.hasStatues 400
{-# INLINEABLE _CustomVerificationEmailTemplateAlreadyExistsException #-}
{-# DEPRECATED _CustomVerificationEmailTemplateAlreadyExistsException "Use generic-lens or generic-optics instead"  #-}

-- | Indicates that a custom verification email template with the name you specified does not exist.
_CustomVerificationEmailTemplateDoesNotExistException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CustomVerificationEmailTemplateDoesNotExistException
  = Core._MatchServiceError mkServiceConfig
      "CustomVerificationEmailTemplateDoesNotExist"
      Core.. Core.hasStatues 400
{-# INLINEABLE _CustomVerificationEmailTemplateDoesNotExistException #-}
{-# DEPRECATED _CustomVerificationEmailTemplateDoesNotExistException "Use generic-lens or generic-optics instead"  #-}

-- | Indicates that the Amazon CloudWatch destination is invalid. See the error message for details.
_InvalidCloudWatchDestinationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidCloudWatchDestinationException
  = Core._MatchServiceError mkServiceConfig
      "InvalidCloudWatchDestination"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidCloudWatchDestinationException #-}
{-# DEPRECATED _InvalidCloudWatchDestinationException "Use generic-lens or generic-optics instead"  #-}

-- | Indicates that provided delivery option is invalid.
_InvalidDeliveryOptionsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidDeliveryOptionsException
  = Core._MatchServiceError mkServiceConfig "InvalidDeliveryOptions"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidDeliveryOptionsException #-}
{-# DEPRECATED _InvalidDeliveryOptionsException "Use generic-lens or generic-optics instead"  #-}

-- | Indicates that the provided AWS Lambda function is invalid, or that Amazon SES could not execute the provided function, possibly due to permissions issues. For information about giving permissions, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-permissions.html Amazon SES Developer Guide> .
_InvalidLambdaFunctionException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidLambdaFunctionException
  = Core._MatchServiceError mkServiceConfig "InvalidLambdaFunction"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidLambdaFunctionException #-}
{-# DEPRECATED _InvalidLambdaFunctionException "Use generic-lens or generic-optics instead"  #-}

-- | Indicates that the TrackingOptions object you specified does not exist.
_TrackingOptionsDoesNotExistException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TrackingOptionsDoesNotExistException
  = Core._MatchServiceError mkServiceConfig
      "TrackingOptionsDoesNotExistException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _TrackingOptionsDoesNotExistException #-}
{-# DEPRECATED _TrackingOptionsDoesNotExistException "Use generic-lens or generic-optics instead"  #-}

-- | Indicates that the template that you specified could not be rendered. This issue may occur when a template refers to a partial that does not exist.
_InvalidTemplateException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidTemplateException
  = Core._MatchServiceError mkServiceConfig "InvalidTemplate" Core..
      Core.hasStatues 400
{-# INLINEABLE _InvalidTemplateException #-}
{-# DEPRECATED _InvalidTemplateException "Use generic-lens or generic-optics instead"  #-}

-- | Indicates that the configuration set does not exist.
_ConfigurationSetDoesNotExistException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConfigurationSetDoesNotExistException
  = Core._MatchServiceError mkServiceConfig
      "ConfigurationSetDoesNotExist"
      Core.. Core.hasStatues 400
{-# INLINEABLE _ConfigurationSetDoesNotExistException #-}
{-# DEPRECATED _ConfigurationSetDoesNotExistException "Use generic-lens or generic-optics instead"  #-}

-- | Indicates that the provided policy is invalid. Check the error stack for more information about what caused the error.
_InvalidPolicyException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidPolicyException
  = Core._MatchServiceError mkServiceConfig "InvalidPolicy" Core..
      Core.hasStatues 400
{-# INLINEABLE _InvalidPolicyException #-}
{-# DEPRECATED _InvalidPolicyException "Use generic-lens or generic-optics instead"  #-}

-- | Indicates that the provided Amazon S3 bucket or AWS KMS encryption key is invalid, or that Amazon SES could not publish to the bucket, possibly due to permissions issues. For information about giving permissions, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-permissions.html Amazon SES Developer Guide> .
_InvalidS3ConfigurationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidS3ConfigurationException
  = Core._MatchServiceError mkServiceConfig "InvalidS3Configuration"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidS3ConfigurationException #-}
{-# DEPRECATED _InvalidS3ConfigurationException "Use generic-lens or generic-optics instead"  #-}

-- | Indicates that the configuration set you specified already contains a TrackingOptions object.
_TrackingOptionsAlreadyExistsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TrackingOptionsAlreadyExistsException
  = Core._MatchServiceError mkServiceConfig
      "TrackingOptionsAlreadyExistsException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _TrackingOptionsAlreadyExistsException #-}
{-# DEPRECATED _TrackingOptionsAlreadyExistsException "Use generic-lens or generic-optics instead"  #-}

-- | Indicates that the provided Amazon SNS topic is invalid, or that Amazon SES could not publish to the topic, possibly due to permissions issues. For information about giving permissions, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-permissions.html Amazon SES Developer Guide> .
_InvalidSnsTopicException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidSnsTopicException
  = Core._MatchServiceError mkServiceConfig "InvalidSnsTopic" Core..
      Core.hasStatues 400
{-# INLINEABLE _InvalidSnsTopicException #-}
{-# DEPRECATED _InvalidSnsTopicException "Use generic-lens or generic-optics instead"  #-}

-- | Indicates that the event destination could not be created because of a naming conflict.
_EventDestinationAlreadyExistsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_EventDestinationAlreadyExistsException
  = Core._MatchServiceError mkServiceConfig
      "EventDestinationAlreadyExists"
      Core.. Core.hasStatues 400
{-# INLINEABLE _EventDestinationAlreadyExistsException #-}
{-# DEPRECATED _EventDestinationAlreadyExistsException "Use generic-lens or generic-optics instead"  #-}

-- | Indicates that a resource could not be created because of a naming conflict.
_AlreadyExistsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AlreadyExistsException
  = Core._MatchServiceError mkServiceConfig "AlreadyExists" Core..
      Core.hasStatues 400
{-# INLINEABLE _AlreadyExistsException #-}
{-# DEPRECATED _AlreadyExistsException "Use generic-lens or generic-optics instead"  #-}

-- | Indicates that a resource could not be created because of service limits. For a list of Amazon SES limits, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/limits.html Amazon SES Developer Guide> .
_LimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededException
  = Core._MatchServiceError mkServiceConfig "LimitExceeded" Core..
      Core.hasStatues 400
{-# INLINEABLE _LimitExceededException #-}
{-# DEPRECATED _LimitExceededException "Use generic-lens or generic-optics instead"  #-}
