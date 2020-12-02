{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types
  ( -- * Service Configuration
    ses,

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
    AddHeaderAction,
    addHeaderAction,
    ahaHeaderName,
    ahaHeaderValue,

    -- * Body
    Body,
    body,
    bText,
    bHTML,

    -- * BounceAction
    BounceAction,
    bounceAction,
    baTopicARN,
    baStatusCode,
    baSmtpReplyCode,
    baMessage,
    baSender,

    -- * BouncedRecipientInfo
    BouncedRecipientInfo,
    bouncedRecipientInfo,
    briBounceType,
    briRecipientDsnFields,
    briRecipientARN,
    briRecipient,

    -- * BulkEmailDestination
    BulkEmailDestination,
    bulkEmailDestination,
    bedReplacementTemplateData,
    bedReplacementTags,
    bedDestination,

    -- * BulkEmailDestinationStatus
    BulkEmailDestinationStatus,
    bulkEmailDestinationStatus,
    bedsStatus,
    bedsError,
    bedsMessageId,

    -- * CloudWatchDestination
    CloudWatchDestination,
    cloudWatchDestination,
    cwdDimensionConfigurations,

    -- * CloudWatchDimensionConfiguration
    CloudWatchDimensionConfiguration,
    cloudWatchDimensionConfiguration,
    cwdcDimensionName,
    cwdcDimensionValueSource,
    cwdcDefaultDimensionValue,

    -- * ConfigurationSet
    ConfigurationSet,
    configurationSet,
    csName,

    -- * Content
    Content,
    content,
    cCharset,
    cData,

    -- * CustomVerificationEmailTemplate
    CustomVerificationEmailTemplate,
    customVerificationEmailTemplate,
    cvetFromEmailAddress,
    cvetTemplateName,
    cvetFailureRedirectionURL,
    cvetTemplateSubject,
    cvetSuccessRedirectionURL,

    -- * DeliveryOptions
    DeliveryOptions,
    deliveryOptions,
    doTLSPolicy,

    -- * Destination
    Destination,
    destination,
    dBCCAddresses,
    dCCAddresses,
    dToAddresses,

    -- * EventDestination
    EventDestination,
    eventDestination,
    edEnabled,
    edKinesisFirehoseDestination,
    edCloudWatchDestination,
    edSNSDestination,
    edName,
    edMatchingEventTypes,

    -- * ExtensionField
    ExtensionField,
    extensionField,
    efName,
    efValue,

    -- * IdentityDkimAttributes
    IdentityDkimAttributes,
    identityDkimAttributes,
    idaDkimTokens,
    idaDkimEnabled,
    idaDkimVerificationStatus,

    -- * IdentityMailFromDomainAttributes
    IdentityMailFromDomainAttributes,
    identityMailFromDomainAttributes,
    imfdaMailFromDomain,
    imfdaMailFromDomainStatus,
    imfdaBehaviorOnMXFailure,

    -- * IdentityNotificationAttributes
    IdentityNotificationAttributes,
    identityNotificationAttributes,
    inaHeadersInDeliveryNotificationsEnabled,
    inaHeadersInComplaintNotificationsEnabled,
    inaHeadersInBounceNotificationsEnabled,
    inaBounceTopic,
    inaComplaintTopic,
    inaDeliveryTopic,
    inaForwardingEnabled,

    -- * IdentityVerificationAttributes
    IdentityVerificationAttributes,
    identityVerificationAttributes,
    ivaVerificationToken,
    ivaVerificationStatus,

    -- * KinesisFirehoseDestination
    KinesisFirehoseDestination,
    kinesisFirehoseDestination,
    kfdIAMRoleARN,
    kfdDeliveryStreamARN,

    -- * LambdaAction
    LambdaAction,
    lambdaAction,
    laInvocationType,
    laTopicARN,
    laFunctionARN,

    -- * Message
    Message,
    message,
    mSubject,
    mBody,

    -- * MessageDsn
    MessageDsn,
    messageDsn,
    mdArrivalDate,
    mdExtensionFields,
    mdReportingMta,

    -- * MessageTag
    MessageTag,
    messageTag,
    mtName,
    mtValue,

    -- * RawMessage
    RawMessage,
    rawMessage,
    rmData,

    -- * ReceiptAction
    ReceiptAction,
    receiptAction,
    raAddHeaderAction,
    raSNSAction,
    raWorkmailAction,
    raBounceAction,
    raLambdaAction,
    raStopAction,
    raS3Action,

    -- * ReceiptFilter
    ReceiptFilter,
    receiptFilter,
    rfName,
    rfIPFilter,

    -- * ReceiptIPFilter
    ReceiptIPFilter,
    receiptIPFilter,
    rifPolicy,
    rifCidr,

    -- * ReceiptRule
    ReceiptRule,
    receiptRule,
    rrScanEnabled,
    rrEnabled,
    rrActions,
    rrRecipients,
    rrTLSPolicy,
    rrName,

    -- * ReceiptRuleSetMetadata
    ReceiptRuleSetMetadata,
    receiptRuleSetMetadata,
    rrsmName,
    rrsmCreatedTimestamp,

    -- * RecipientDsnFields
    RecipientDsnFields,
    recipientDsnFields,
    rdfDiagnosticCode,
    rdfRemoteMta,
    rdfFinalRecipient,
    rdfExtensionFields,
    rdfLastAttemptDate,
    rdfAction,
    rdfStatus,

    -- * ReputationOptions
    ReputationOptions,
    reputationOptions,
    roLastFreshStart,
    roReputationMetricsEnabled,
    roSendingEnabled,

    -- * S3Action
    S3Action,
    s3Action,
    s3KMSKeyARN,
    s3TopicARN,
    s3ObjectKeyPrefix,
    s3BucketName,

    -- * SNSAction
    SNSAction,
    snsAction,
    saEncoding,
    saTopicARN,

    -- * SNSDestination
    SNSDestination,
    snsDestination,
    sdTopicARN,

    -- * SendDataPoint
    SendDataPoint,
    sendDataPoint,
    sdpRejects,
    sdpComplaints,
    sdpDeliveryAttempts,
    sdpBounces,
    sdpTimestamp,

    -- * StopAction
    StopAction,
    stopAction,
    sTopicARN,
    sScope,

    -- * Template
    Template,
    template,
    tTextPart,
    tSubjectPart,
    tHTMLPart,
    tTemplateName,

    -- * TemplateMetadata
    TemplateMetadata,
    templateMetadata,
    tmName,
    tmCreatedTimestamp,

    -- * TrackingOptions
    TrackingOptions,
    trackingOptions,
    toCustomRedirectDomain,

    -- * WorkmailAction
    WorkmailAction,
    workmailAction,
    waTopicARN,
    waOrganizationARN,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
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
import Network.AWS.Sign.V4

-- | API version @2010-12-01@ of the Amazon Simple Email Service SDK configuration.
ses :: Service
ses =
  Service
    { _svcAbbrev = "SES",
      _svcSigner = v4,
      _svcPrefix = "email",
      _svcVersion = "2010-12-01",
      _svcEndpoint = defaultEndpoint ses,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseXMLError "SES",
      _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2,
          _retryGrowth = 2,
          _retryAttempts = 5,
          _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has
          (hasCode "ProvisionedThroughputExceededException" . hasStatus 400)
          e =
        Just "throughput_exceeded"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing
