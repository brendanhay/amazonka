-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types
  ( -- * Service configuration
    sesService,

    -- * Errors

    -- * BehaviorOnMXFailure
    BehaviorOnMXFailure (..),

    -- * BounceType
    BounceType (..),

    -- * BulkEmailStatus
    BulkEmailStatus (..),

    -- * ConfigurationSetAttribute
    ConfigurationSetAttribute (..),

    -- * CustomMailFromStatus
    CustomMailFromStatus (..),

    -- * DimensionValueSource
    DimensionValueSource (..),

    -- * DsnAction
    DsnAction (..),

    -- * EventType
    EventType (..),

    -- * IdentityType
    IdentityType (..),

    -- * InvocationType
    InvocationType (..),

    -- * NotificationType
    NotificationType (..),

    -- * ReceiptFilterPolicy
    ReceiptFilterPolicy (..),

    -- * SNSActionEncoding
    SNSActionEncoding (..),

    -- * StopScope
    StopScope (..),

    -- * TLSPolicy
    TLSPolicy (..),

    -- * VerificationStatus
    VerificationStatus (..),

    -- * AddHeaderAction
    AddHeaderAction (..),
    mkAddHeaderAction,
    ahaHeaderName,
    ahaHeaderValue,

    -- * Body
    Body (..),
    mkBody,
    bText,
    bHTML,

    -- * BounceAction
    BounceAction (..),
    mkBounceAction,
    baTopicARN,
    baStatusCode,
    baSmtpReplyCode,
    baMessage,
    baSender,

    -- * BouncedRecipientInfo
    BouncedRecipientInfo (..),
    mkBouncedRecipientInfo,
    briBounceType,
    briRecipientDsnFields,
    briRecipientARN,
    briRecipient,

    -- * BulkEmailDestination
    BulkEmailDestination (..),
    mkBulkEmailDestination,
    bedReplacementTemplateData,
    bedReplacementTags,
    bedDestination,

    -- * BulkEmailDestinationStatus
    BulkEmailDestinationStatus (..),
    mkBulkEmailDestinationStatus,
    bedsStatus,
    bedsError,
    bedsMessageId,

    -- * CloudWatchDestination
    CloudWatchDestination (..),
    mkCloudWatchDestination,
    cwdDimensionConfigurations,

    -- * CloudWatchDimensionConfiguration
    CloudWatchDimensionConfiguration (..),
    mkCloudWatchDimensionConfiguration,
    cwdcDimensionName,
    cwdcDimensionValueSource,
    cwdcDefaultDimensionValue,

    -- * ConfigurationSet
    ConfigurationSet (..),
    mkConfigurationSet,
    csName,

    -- * Content
    Content (..),
    mkContent,
    cCharset,
    cData,

    -- * CustomVerificationEmailTemplate
    CustomVerificationEmailTemplate (..),
    mkCustomVerificationEmailTemplate,
    cvetFromEmailAddress,
    cvetTemplateName,
    cvetFailureRedirectionURL,
    cvetTemplateSubject,
    cvetSuccessRedirectionURL,

    -- * DeliveryOptions
    DeliveryOptions (..),
    mkDeliveryOptions,
    doTLSPolicy,

    -- * Destination
    Destination (..),
    mkDestination,
    dBCCAddresses,
    dCCAddresses,
    dToAddresses,

    -- * EventDestination
    EventDestination (..),
    mkEventDestination,
    edEnabled,
    edKinesisFirehoseDestination,
    edCloudWatchDestination,
    edSNSDestination,
    edName,
    edMatchingEventTypes,

    -- * ExtensionField
    ExtensionField (..),
    mkExtensionField,
    efName,
    efValue,

    -- * IdentityDkimAttributes
    IdentityDkimAttributes (..),
    mkIdentityDkimAttributes,
    idaDkimTokens,
    idaDkimEnabled,
    idaDkimVerificationStatus,

    -- * IdentityMailFromDomainAttributes
    IdentityMailFromDomainAttributes (..),
    mkIdentityMailFromDomainAttributes,
    imfdaMailFromDomain,
    imfdaMailFromDomainStatus,
    imfdaBehaviorOnMXFailure,

    -- * IdentityNotificationAttributes
    IdentityNotificationAttributes (..),
    mkIdentityNotificationAttributes,
    inaHeadersInDeliveryNotificationsEnabled,
    inaHeadersInComplaintNotificationsEnabled,
    inaHeadersInBounceNotificationsEnabled,
    inaBounceTopic,
    inaComplaintTopic,
    inaDeliveryTopic,
    inaForwardingEnabled,

    -- * IdentityVerificationAttributes
    IdentityVerificationAttributes (..),
    mkIdentityVerificationAttributes,
    ivaVerificationToken,
    ivaVerificationStatus,

    -- * KinesisFirehoseDestination
    KinesisFirehoseDestination (..),
    mkKinesisFirehoseDestination,
    kfdIAMRoleARN,
    kfdDeliveryStreamARN,

    -- * LambdaAction
    LambdaAction (..),
    mkLambdaAction,
    laInvocationType,
    laTopicARN,
    laFunctionARN,

    -- * Message
    Message (..),
    mkMessage,
    mSubject,
    mBody,

    -- * MessageDsn
    MessageDsn (..),
    mkMessageDsn,
    mdArrivalDate,
    mdExtensionFields,
    mdReportingMta,

    -- * MessageTag
    MessageTag (..),
    mkMessageTag,
    mtName,
    mtValue,

    -- * RawMessage
    RawMessage (..),
    mkRawMessage,
    rmData,

    -- * ReceiptAction
    ReceiptAction (..),
    mkReceiptAction,
    raAddHeaderAction,
    raSNSAction,
    raWorkmailAction,
    raBounceAction,
    raLambdaAction,
    raStopAction,
    raS3Action,

    -- * ReceiptFilter
    ReceiptFilter (..),
    mkReceiptFilter,
    rfName,
    rfIPFilter,

    -- * ReceiptIPFilter
    ReceiptIPFilter (..),
    mkReceiptIPFilter,
    rifPolicy,
    rifCidr,

    -- * ReceiptRule
    ReceiptRule (..),
    mkReceiptRule,
    rrScanEnabled,
    rrEnabled,
    rrActions,
    rrRecipients,
    rrTLSPolicy,
    rrName,

    -- * ReceiptRuleSetMetadata
    ReceiptRuleSetMetadata (..),
    mkReceiptRuleSetMetadata,
    rrsmName,
    rrsmCreatedTimestamp,

    -- * RecipientDsnFields
    RecipientDsnFields (..),
    mkRecipientDsnFields,
    rdfDiagnosticCode,
    rdfRemoteMta,
    rdfFinalRecipient,
    rdfExtensionFields,
    rdfLastAttemptDate,
    rdfAction,
    rdfStatus,

    -- * ReputationOptions
    ReputationOptions (..),
    mkReputationOptions,
    roLastFreshStart,
    roReputationMetricsEnabled,
    roSendingEnabled,

    -- * S3Action
    S3Action (..),
    mkS3Action,
    s3KMSKeyARN,
    s3TopicARN,
    s3ObjectKeyPrefix,
    s3BucketName,

    -- * SNSAction
    SNSAction (..),
    mkSNSAction,
    saEncoding,
    saTopicARN,

    -- * SNSDestination
    SNSDestination (..),
    mkSNSDestination,
    sdTopicARN,

    -- * SendDataPoint
    SendDataPoint (..),
    mkSendDataPoint,
    sdpRejects,
    sdpComplaints,
    sdpDeliveryAttempts,
    sdpBounces,
    sdpTimestamp,

    -- * StopAction
    StopAction (..),
    mkStopAction,
    sTopicARN,
    sScope,

    -- * Template
    Template (..),
    mkTemplate,
    tTextPart,
    tSubjectPart,
    tHTMLPart,
    tTemplateName,

    -- * TemplateMetadata
    TemplateMetadata (..),
    mkTemplateMetadata,
    tmName,
    tmCreatedTimestamp,

    -- * TrackingOptions
    TrackingOptions (..),
    mkTrackingOptions,
    toCustomRedirectDomain,

    -- * WorkmailAction
    WorkmailAction (..),
    mkWorkmailAction,
    waTopicARN,
    waOrganizationARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SES.Types.AddHeaderAction
import Network.AWS.SES.Types.BehaviorOnMXFailure
import Network.AWS.SES.Types.Body
import Network.AWS.SES.Types.BounceAction
import Network.AWS.SES.Types.BounceType
import Network.AWS.SES.Types.BouncedRecipientInfo
import Network.AWS.SES.Types.BulkEmailDestination
import Network.AWS.SES.Types.BulkEmailDestinationStatus
import Network.AWS.SES.Types.BulkEmailStatus
import Network.AWS.SES.Types.CloudWatchDestination
import Network.AWS.SES.Types.CloudWatchDimensionConfiguration
import Network.AWS.SES.Types.ConfigurationSet
import Network.AWS.SES.Types.ConfigurationSetAttribute
import Network.AWS.SES.Types.Content
import Network.AWS.SES.Types.CustomMailFromStatus
import Network.AWS.SES.Types.CustomVerificationEmailTemplate
import Network.AWS.SES.Types.DeliveryOptions
import Network.AWS.SES.Types.Destination
import Network.AWS.SES.Types.DimensionValueSource
import Network.AWS.SES.Types.DsnAction
import Network.AWS.SES.Types.EventDestination
import Network.AWS.SES.Types.EventType
import Network.AWS.SES.Types.ExtensionField
import Network.AWS.SES.Types.IdentityDkimAttributes
import Network.AWS.SES.Types.IdentityMailFromDomainAttributes
import Network.AWS.SES.Types.IdentityNotificationAttributes
import Network.AWS.SES.Types.IdentityType
import Network.AWS.SES.Types.IdentityVerificationAttributes
import Network.AWS.SES.Types.InvocationType
import Network.AWS.SES.Types.KinesisFirehoseDestination
import Network.AWS.SES.Types.LambdaAction
import Network.AWS.SES.Types.Message
import Network.AWS.SES.Types.MessageDsn
import Network.AWS.SES.Types.MessageTag
import Network.AWS.SES.Types.NotificationType
import Network.AWS.SES.Types.RawMessage
import Network.AWS.SES.Types.ReceiptAction
import Network.AWS.SES.Types.ReceiptFilter
import Network.AWS.SES.Types.ReceiptFilterPolicy
import Network.AWS.SES.Types.ReceiptIPFilter
import Network.AWS.SES.Types.ReceiptRule
import Network.AWS.SES.Types.ReceiptRuleSetMetadata
import Network.AWS.SES.Types.RecipientDsnFields
import Network.AWS.SES.Types.ReputationOptions
import Network.AWS.SES.Types.S3Action
import Network.AWS.SES.Types.SNSAction
import Network.AWS.SES.Types.SNSActionEncoding
import Network.AWS.SES.Types.SNSDestination
import Network.AWS.SES.Types.SendDataPoint
import Network.AWS.SES.Types.StopAction
import Network.AWS.SES.Types.StopScope
import Network.AWS.SES.Types.TLSPolicy
import Network.AWS.SES.Types.Template
import Network.AWS.SES.Types.TemplateMetadata
import Network.AWS.SES.Types.TrackingOptions
import Network.AWS.SES.Types.VerificationStatus
import Network.AWS.SES.Types.WorkmailAction
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2010-12-01@ of the Amazon Simple Email Service SDK configuration.
sesService :: Lude.Service
sesService =
  Lude.Service
    { Lude._svcAbbrev = "SES",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "email",
      Lude._svcVersion = "2010-12-01",
      Lude._svcEndpoint = Lude.defaultEndpoint sesService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseXMLError "SES",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing
