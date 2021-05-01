{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ConfigurationSetSendingPausedException,
    _CustomVerificationEmailTemplateAlreadyExistsException,
    _InvalidConfigurationSetException,
    _AccountSendingPausedException,
    _EventDestinationDoesNotExistException,
    _InvalidSNSDestinationException,
    _CustomVerificationEmailInvalidContentException,
    _InvalidTemplateException,
    _InvalidPolicyException,
    _ConfigurationSetAlreadyExistsException,
    _MailFromDomainNotVerifiedException,
    _FromEmailAddressNotVerifiedException,
    _RuleSetDoesNotExistException,
    _MessageRejected,
    _InvalidDeliveryOptionsException,
    _InvalidCloudWatchDestinationException,
    _CannotDeleteException,
    _TemplateDoesNotExistException,
    _LimitExceededException,
    _InvalidTrackingOptionsException,
    _InvalidSnsTopicException,
    _EventDestinationAlreadyExistsException,
    _AlreadyExistsException,
    _InvalidS3ConfigurationException,
    _ConfigurationSetDoesNotExistException,
    _TrackingOptionsAlreadyExistsException,
    _TrackingOptionsDoesNotExistException,
    _InvalidFirehoseDestinationException,
    _InvalidLambdaFunctionException,
    _MissingRenderingAttributeException,
    _InvalidRenderingParameterException,
    _CustomVerificationEmailTemplateDoesNotExistException,
    _ProductionAccessNotGrantedException,
    _RuleDoesNotExistException,

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

    -- * TlsPolicy
    TlsPolicy (..),

    -- * VerificationStatus
    VerificationStatus (..),

    -- * AddHeaderAction
    AddHeaderAction (..),
    newAddHeaderAction,
    addHeaderAction_headerName,
    addHeaderAction_headerValue,

    -- * Body
    Body (..),
    newBody,
    body_html,
    body_text,

    -- * BounceAction
    BounceAction (..),
    newBounceAction,
    bounceAction_topicArn,
    bounceAction_statusCode,
    bounceAction_smtpReplyCode,
    bounceAction_message,
    bounceAction_sender,

    -- * BouncedRecipientInfo
    BouncedRecipientInfo (..),
    newBouncedRecipientInfo,
    bouncedRecipientInfo_recipientArn,
    bouncedRecipientInfo_recipientDsnFields,
    bouncedRecipientInfo_bounceType,
    bouncedRecipientInfo_recipient,

    -- * BulkEmailDestination
    BulkEmailDestination (..),
    newBulkEmailDestination,
    bulkEmailDestination_replacementTags,
    bulkEmailDestination_replacementTemplateData,
    bulkEmailDestination_destination,

    -- * BulkEmailDestinationStatus
    BulkEmailDestinationStatus (..),
    newBulkEmailDestinationStatus,
    bulkEmailDestinationStatus_status,
    bulkEmailDestinationStatus_messageId,
    bulkEmailDestinationStatus_error,

    -- * CloudWatchDestination
    CloudWatchDestination (..),
    newCloudWatchDestination,
    cloudWatchDestination_dimensionConfigurations,

    -- * CloudWatchDimensionConfiguration
    CloudWatchDimensionConfiguration (..),
    newCloudWatchDimensionConfiguration,
    cloudWatchDimensionConfiguration_dimensionName,
    cloudWatchDimensionConfiguration_dimensionValueSource,
    cloudWatchDimensionConfiguration_defaultDimensionValue,

    -- * ConfigurationSet
    ConfigurationSet (..),
    newConfigurationSet,
    configurationSet_name,

    -- * Content
    Content (..),
    newContent,
    content_charset,
    content_data,

    -- * CustomVerificationEmailTemplate
    CustomVerificationEmailTemplate (..),
    newCustomVerificationEmailTemplate,
    customVerificationEmailTemplate_templateName,
    customVerificationEmailTemplate_templateSubject,
    customVerificationEmailTemplate_fromEmailAddress,
    customVerificationEmailTemplate_successRedirectionURL,
    customVerificationEmailTemplate_failureRedirectionURL,

    -- * DeliveryOptions
    DeliveryOptions (..),
    newDeliveryOptions,
    deliveryOptions_tlsPolicy,

    -- * Destination
    Destination (..),
    newDestination,
    destination_toAddresses,
    destination_ccAddresses,
    destination_bccAddresses,

    -- * EventDestination
    EventDestination (..),
    newEventDestination,
    eventDestination_cloudWatchDestination,
    eventDestination_enabled,
    eventDestination_kinesisFirehoseDestination,
    eventDestination_sNSDestination,
    eventDestination_name,
    eventDestination_matchingEventTypes,

    -- * ExtensionField
    ExtensionField (..),
    newExtensionField,
    extensionField_name,
    extensionField_value,

    -- * IdentityDkimAttributes
    IdentityDkimAttributes (..),
    newIdentityDkimAttributes,
    identityDkimAttributes_dkimTokens,
    identityDkimAttributes_dkimEnabled,
    identityDkimAttributes_dkimVerificationStatus,

    -- * IdentityMailFromDomainAttributes
    IdentityMailFromDomainAttributes (..),
    newIdentityMailFromDomainAttributes,
    identityMailFromDomainAttributes_mailFromDomain,
    identityMailFromDomainAttributes_mailFromDomainStatus,
    identityMailFromDomainAttributes_behaviorOnMXFailure,

    -- * IdentityNotificationAttributes
    IdentityNotificationAttributes (..),
    newIdentityNotificationAttributes,
    identityNotificationAttributes_headersInComplaintNotificationsEnabled,
    identityNotificationAttributes_headersInDeliveryNotificationsEnabled,
    identityNotificationAttributes_headersInBounceNotificationsEnabled,
    identityNotificationAttributes_bounceTopic,
    identityNotificationAttributes_complaintTopic,
    identityNotificationAttributes_deliveryTopic,
    identityNotificationAttributes_forwardingEnabled,

    -- * IdentityVerificationAttributes
    IdentityVerificationAttributes (..),
    newIdentityVerificationAttributes,
    identityVerificationAttributes_verificationToken,
    identityVerificationAttributes_verificationStatus,

    -- * KinesisFirehoseDestination
    KinesisFirehoseDestination (..),
    newKinesisFirehoseDestination,
    kinesisFirehoseDestination_iAMRoleARN,
    kinesisFirehoseDestination_deliveryStreamARN,

    -- * LambdaAction
    LambdaAction (..),
    newLambdaAction,
    lambdaAction_invocationType,
    lambdaAction_topicArn,
    lambdaAction_functionArn,

    -- * Message
    Message (..),
    newMessage,
    message_subject,
    message_body,

    -- * MessageDsn
    MessageDsn (..),
    newMessageDsn,
    messageDsn_extensionFields,
    messageDsn_arrivalDate,
    messageDsn_reportingMta,

    -- * MessageTag
    MessageTag (..),
    newMessageTag,
    messageTag_name,
    messageTag_value,

    -- * RawMessage
    RawMessage (..),
    newRawMessage,
    rawMessage_data,

    -- * ReceiptAction
    ReceiptAction (..),
    newReceiptAction,
    receiptAction_lambdaAction,
    receiptAction_stopAction,
    receiptAction_s3Action,
    receiptAction_bounceAction,
    receiptAction_workmailAction,
    receiptAction_addHeaderAction,
    receiptAction_sNSAction,

    -- * ReceiptFilter
    ReceiptFilter (..),
    newReceiptFilter,
    receiptFilter_name,
    receiptFilter_ipFilter,

    -- * ReceiptIpFilter
    ReceiptIpFilter (..),
    newReceiptIpFilter,
    receiptIpFilter_policy,
    receiptIpFilter_cidr,

    -- * ReceiptRule
    ReceiptRule (..),
    newReceiptRule,
    receiptRule_tlsPolicy,
    receiptRule_enabled,
    receiptRule_actions,
    receiptRule_recipients,
    receiptRule_scanEnabled,
    receiptRule_name,

    -- * ReceiptRuleSetMetadata
    ReceiptRuleSetMetadata (..),
    newReceiptRuleSetMetadata,
    receiptRuleSetMetadata_createdTimestamp,
    receiptRuleSetMetadata_name,

    -- * RecipientDsnFields
    RecipientDsnFields (..),
    newRecipientDsnFields,
    recipientDsnFields_remoteMta,
    recipientDsnFields_lastAttemptDate,
    recipientDsnFields_extensionFields,
    recipientDsnFields_diagnosticCode,
    recipientDsnFields_finalRecipient,
    recipientDsnFields_action,
    recipientDsnFields_status,

    -- * ReputationOptions
    ReputationOptions (..),
    newReputationOptions,
    reputationOptions_reputationMetricsEnabled,
    reputationOptions_lastFreshStart,
    reputationOptions_sendingEnabled,

    -- * S3Action
    S3Action (..),
    newS3Action,
    s3Action_objectKeyPrefix,
    s3Action_kmsKeyArn,
    s3Action_topicArn,
    s3Action_bucketName,

    -- * SNSAction
    SNSAction (..),
    newSNSAction,
    sNSAction_encoding,
    sNSAction_topicArn,

    -- * SNSDestination
    SNSDestination (..),
    newSNSDestination,
    sNSDestination_topicARN,

    -- * SendDataPoint
    SendDataPoint (..),
    newSendDataPoint,
    sendDataPoint_bounces,
    sendDataPoint_complaints,
    sendDataPoint_rejects,
    sendDataPoint_timestamp,
    sendDataPoint_deliveryAttempts,

    -- * StopAction
    StopAction (..),
    newStopAction,
    stopAction_topicArn,
    stopAction_scope,

    -- * Template
    Template (..),
    newTemplate,
    template_textPart,
    template_subjectPart,
    template_htmlPart,
    template_templateName,

    -- * TemplateMetadata
    TemplateMetadata (..),
    newTemplateMetadata,
    templateMetadata_createdTimestamp,
    templateMetadata_name,

    -- * TrackingOptions
    TrackingOptions (..),
    newTrackingOptions,
    trackingOptions_customRedirectDomain,

    -- * WorkmailAction
    WorkmailAction (..),
    newWorkmailAction,
    workmailAction_topicArn,
    workmailAction_organizationArn,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
import Network.AWS.SES.Types.ReceiptIpFilter
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
import Network.AWS.SES.Types.Template
import Network.AWS.SES.Types.TemplateMetadata
import Network.AWS.SES.Types.TlsPolicy
import Network.AWS.SES.Types.TrackingOptions
import Network.AWS.SES.Types.VerificationStatus
import Network.AWS.SES.Types.WorkmailAction
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2010-12-01@ of the Amazon Simple Email Service SDK configuration.
defaultService :: Prelude.Service
defaultService =
  Prelude.Service
    { Prelude._svcAbbrev = "SES",
      Prelude._svcSigner = Sign.v4,
      Prelude._svcEndpointPrefix = "email",
      Prelude._svcSigningName = "ses",
      Prelude._svcVersion = "2010-12-01",
      Prelude._svcEndpoint =
        Prelude.defaultEndpoint defaultService,
      Prelude._svcTimeout = Prelude.Just 70,
      Prelude._svcCheck = Prelude.statusSuccess,
      Prelude._svcError = Prelude.parseXMLError "SES",
      Prelude._svcRetry = retry
    }
  where
    retry =
      Prelude.Exponential
        { Prelude._retryBase = 5.0e-2,
          Prelude._retryGrowth = 2,
          Prelude._retryAttempts = 5,
          Prelude._retryCheck = check
        }
    check e
      | Lens.has (Prelude.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Prelude.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Prelude.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Prelude.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Prelude.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Prelude.hasCode "RequestThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Prelude.hasCode "ThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Prelude.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Prelude.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Prelude.hasCode "ThrottlingException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Prelude.hasCode "Throttling"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing

-- | Indicates that email sending is disabled for the configuration set.
--
-- You can enable or disable email sending for a configuration set using
-- UpdateConfigurationSetSendingEnabled.
_ConfigurationSetSendingPausedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ConfigurationSetSendingPausedException =
  Prelude._MatchServiceError
    defaultService
    "ConfigurationSetSendingPausedException"
    Prelude.. Prelude.hasStatus 400

-- | Indicates that a custom verification email template with the name you
-- specified already exists.
_CustomVerificationEmailTemplateAlreadyExistsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_CustomVerificationEmailTemplateAlreadyExistsException =
  Prelude._MatchServiceError
    defaultService
    "CustomVerificationEmailTemplateAlreadyExists"
    Prelude.. Prelude.hasStatus 400

-- | Indicates that the configuration set is invalid. See the error message
-- for details.
_InvalidConfigurationSetException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidConfigurationSetException =
  Prelude._MatchServiceError
    defaultService
    "InvalidConfigurationSet"
    Prelude.. Prelude.hasStatus 400

-- | Indicates that email sending is disabled for your entire Amazon SES
-- account.
--
-- You can enable or disable email sending for your Amazon SES account
-- using UpdateAccountSendingEnabled.
_AccountSendingPausedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_AccountSendingPausedException =
  Prelude._MatchServiceError
    defaultService
    "AccountSendingPausedException"
    Prelude.. Prelude.hasStatus 400

-- | Indicates that the event destination does not exist.
_EventDestinationDoesNotExistException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_EventDestinationDoesNotExistException =
  Prelude._MatchServiceError
    defaultService
    "EventDestinationDoesNotExist"
    Prelude.. Prelude.hasStatus 400

-- | Indicates that the Amazon Simple Notification Service (Amazon SNS)
-- destination is invalid. See the error message for details.
_InvalidSNSDestinationException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidSNSDestinationException =
  Prelude._MatchServiceError
    defaultService
    "InvalidSNSDestination"
    Prelude.. Prelude.hasStatus 400

-- | Indicates that custom verification email template provided content is
-- invalid.
_CustomVerificationEmailInvalidContentException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_CustomVerificationEmailInvalidContentException =
  Prelude._MatchServiceError
    defaultService
    "CustomVerificationEmailInvalidContent"
    Prelude.. Prelude.hasStatus 400

-- | Indicates that the template that you specified could not be rendered.
-- This issue may occur when a template refers to a partial that does not
-- exist.
_InvalidTemplateException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidTemplateException =
  Prelude._MatchServiceError
    defaultService
    "InvalidTemplate"
    Prelude.. Prelude.hasStatus 400

-- | Indicates that the provided policy is invalid. Check the error stack for
-- more information about what caused the error.
_InvalidPolicyException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidPolicyException =
  Prelude._MatchServiceError
    defaultService
    "InvalidPolicy"
    Prelude.. Prelude.hasStatus 400

-- | Indicates that the configuration set could not be created because of a
-- naming conflict.
_ConfigurationSetAlreadyExistsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ConfigurationSetAlreadyExistsException =
  Prelude._MatchServiceError
    defaultService
    "ConfigurationSetAlreadyExists"
    Prelude.. Prelude.hasStatus 400

-- | Indicates that the message could not be sent because Amazon SES could
-- not read the MX record required to use the specified MAIL FROM domain.
-- For information about editing the custom MAIL FROM domain settings for
-- an identity, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/mail-from-edit.html Amazon SES Developer Guide>.
_MailFromDomainNotVerifiedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_MailFromDomainNotVerifiedException =
  Prelude._MatchServiceError
    defaultService
    "MailFromDomainNotVerifiedException"
    Prelude.. Prelude.hasStatus 400

-- | Indicates that the sender address specified for a custom verification
-- email is not verified, and is therefore not eligible to send the custom
-- verification email.
_FromEmailAddressNotVerifiedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_FromEmailAddressNotVerifiedException =
  Prelude._MatchServiceError
    defaultService
    "FromEmailAddressNotVerified"
    Prelude.. Prelude.hasStatus 400

-- | Indicates that the provided receipt rule set does not exist.
_RuleSetDoesNotExistException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_RuleSetDoesNotExistException =
  Prelude._MatchServiceError
    defaultService
    "RuleSetDoesNotExist"
    Prelude.. Prelude.hasStatus 400

-- | Indicates that the action failed, and the message could not be sent.
-- Check the error stack for more information about what caused the error.
_MessageRejected :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_MessageRejected =
  Prelude._MatchServiceError
    defaultService
    "MessageRejected"
    Prelude.. Prelude.hasStatus 400

-- | Indicates that provided delivery option is invalid.
_InvalidDeliveryOptionsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidDeliveryOptionsException =
  Prelude._MatchServiceError
    defaultService
    "InvalidDeliveryOptions"
    Prelude.. Prelude.hasStatus 400

-- | Indicates that the Amazon CloudWatch destination is invalid. See the
-- error message for details.
_InvalidCloudWatchDestinationException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidCloudWatchDestinationException =
  Prelude._MatchServiceError
    defaultService
    "InvalidCloudWatchDestination"
    Prelude.. Prelude.hasStatus 400

-- | Indicates that the delete operation could not be completed.
_CannotDeleteException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_CannotDeleteException =
  Prelude._MatchServiceError
    defaultService
    "CannotDelete"
    Prelude.. Prelude.hasStatus 400

-- | Indicates that the Template object you specified does not exist in your
-- Amazon SES account.
_TemplateDoesNotExistException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TemplateDoesNotExistException =
  Prelude._MatchServiceError
    defaultService
    "TemplateDoesNotExist"
    Prelude.. Prelude.hasStatus 400

-- | Indicates that a resource could not be created because of service
-- limits. For a list of Amazon SES limits, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/limits.html Amazon SES Developer Guide>.
_LimitExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_LimitExceededException =
  Prelude._MatchServiceError
    defaultService
    "LimitExceeded"
    Prelude.. Prelude.hasStatus 400

-- | Indicates that the custom domain to be used for open and click tracking
-- redirects is invalid. This error appears most often in the following
-- situations:
--
-- -   When the tracking domain you specified is not verified in Amazon
--     SES.
--
-- -   When the tracking domain you specified is not a valid domain or
--     subdomain.
_InvalidTrackingOptionsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidTrackingOptionsException =
  Prelude._MatchServiceError
    defaultService
    "InvalidTrackingOptions"
    Prelude.. Prelude.hasStatus 400

-- | Indicates that the provided Amazon SNS topic is invalid, or that Amazon
-- SES could not publish to the topic, possibly due to permissions issues.
-- For information about giving permissions, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-permissions.html Amazon SES Developer Guide>.
_InvalidSnsTopicException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidSnsTopicException =
  Prelude._MatchServiceError
    defaultService
    "InvalidSnsTopic"
    Prelude.. Prelude.hasStatus 400

-- | Indicates that the event destination could not be created because of a
-- naming conflict.
_EventDestinationAlreadyExistsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_EventDestinationAlreadyExistsException =
  Prelude._MatchServiceError
    defaultService
    "EventDestinationAlreadyExists"
    Prelude.. Prelude.hasStatus 400

-- | Indicates that a resource could not be created because of a naming
-- conflict.
_AlreadyExistsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_AlreadyExistsException =
  Prelude._MatchServiceError
    defaultService
    "AlreadyExists"
    Prelude.. Prelude.hasStatus 400

-- | Indicates that the provided Amazon S3 bucket or AWS KMS encryption key
-- is invalid, or that Amazon SES could not publish to the bucket, possibly
-- due to permissions issues. For information about giving permissions, see
-- the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-permissions.html Amazon SES Developer Guide>.
_InvalidS3ConfigurationException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidS3ConfigurationException =
  Prelude._MatchServiceError
    defaultService
    "InvalidS3Configuration"
    Prelude.. Prelude.hasStatus 400

-- | Indicates that the configuration set does not exist.
_ConfigurationSetDoesNotExistException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ConfigurationSetDoesNotExistException =
  Prelude._MatchServiceError
    defaultService
    "ConfigurationSetDoesNotExist"
    Prelude.. Prelude.hasStatus 400

-- | Indicates that the configuration set you specified already contains a
-- TrackingOptions object.
_TrackingOptionsAlreadyExistsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TrackingOptionsAlreadyExistsException =
  Prelude._MatchServiceError
    defaultService
    "TrackingOptionsAlreadyExistsException"
    Prelude.. Prelude.hasStatus 400

-- | Indicates that the TrackingOptions object you specified does not exist.
_TrackingOptionsDoesNotExistException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TrackingOptionsDoesNotExistException =
  Prelude._MatchServiceError
    defaultService
    "TrackingOptionsDoesNotExistException"
    Prelude.. Prelude.hasStatus 400

-- | Indicates that the Amazon Kinesis Firehose destination is invalid. See
-- the error message for details.
_InvalidFirehoseDestinationException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidFirehoseDestinationException =
  Prelude._MatchServiceError
    defaultService
    "InvalidFirehoseDestination"
    Prelude.. Prelude.hasStatus 400

-- | Indicates that the provided AWS Lambda function is invalid, or that
-- Amazon SES could not execute the provided function, possibly due to
-- permissions issues. For information about giving permissions, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-permissions.html Amazon SES Developer Guide>.
_InvalidLambdaFunctionException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidLambdaFunctionException =
  Prelude._MatchServiceError
    defaultService
    "InvalidLambdaFunction"
    Prelude.. Prelude.hasStatus 400

-- | Indicates that one or more of the replacement values for the specified
-- template was not specified. Ensure that the TemplateData object contains
-- references to all of the replacement tags in the specified template.
_MissingRenderingAttributeException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_MissingRenderingAttributeException =
  Prelude._MatchServiceError
    defaultService
    "MissingRenderingAttribute"
    Prelude.. Prelude.hasStatus 400

-- | Indicates that one or more of the replacement values you provided is
-- invalid. This error may occur when the TemplateData object contains
-- invalid JSON.
_InvalidRenderingParameterException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidRenderingParameterException =
  Prelude._MatchServiceError
    defaultService
    "InvalidRenderingParameter"
    Prelude.. Prelude.hasStatus 400

-- | Indicates that a custom verification email template with the name you
-- specified does not exist.
_CustomVerificationEmailTemplateDoesNotExistException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_CustomVerificationEmailTemplateDoesNotExistException =
  Prelude._MatchServiceError
    defaultService
    "CustomVerificationEmailTemplateDoesNotExist"
    Prelude.. Prelude.hasStatus 400

-- | Indicates that the account has not been granted production access.
_ProductionAccessNotGrantedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ProductionAccessNotGrantedException =
  Prelude._MatchServiceError
    defaultService
    "ProductionAccessNotGranted"
    Prelude.. Prelude.hasStatus 400

-- | Indicates that the provided receipt rule does not exist.
_RuleDoesNotExistException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_RuleDoesNotExistException =
  Prelude._MatchServiceError
    defaultService
    "RuleDoesNotExist"
    Prelude.. Prelude.hasStatus 400
