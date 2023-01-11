{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SES.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SES.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccountSendingPausedException,
    _AlreadyExistsException,
    _CannotDeleteException,
    _ConfigurationSetAlreadyExistsException,
    _ConfigurationSetDoesNotExistException,
    _ConfigurationSetSendingPausedException,
    _CustomVerificationEmailInvalidContentException,
    _CustomVerificationEmailTemplateAlreadyExistsException,
    _CustomVerificationEmailTemplateDoesNotExistException,
    _EventDestinationAlreadyExistsException,
    _EventDestinationDoesNotExistException,
    _FromEmailAddressNotVerifiedException,
    _InvalidCloudWatchDestinationException,
    _InvalidConfigurationSetException,
    _InvalidDeliveryOptionsException,
    _InvalidFirehoseDestinationException,
    _InvalidLambdaFunctionException,
    _InvalidPolicyException,
    _InvalidRenderingParameterException,
    _InvalidS3ConfigurationException,
    _InvalidSNSDestinationException,
    _InvalidSnsTopicException,
    _InvalidTemplateException,
    _InvalidTrackingOptionsException,
    _LimitExceededException,
    _MailFromDomainNotVerifiedException,
    _MessageRejected,
    _MissingRenderingAttributeException,
    _ProductionAccessNotGrantedException,
    _RuleDoesNotExistException,
    _RuleSetDoesNotExistException,
    _TemplateDoesNotExistException,
    _TrackingOptionsAlreadyExistsException,
    _TrackingOptionsDoesNotExistException,

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
    bounceAction_statusCode,
    bounceAction_topicArn,
    bounceAction_smtpReplyCode,
    bounceAction_message,
    bounceAction_sender,

    -- * BouncedRecipientInfo
    BouncedRecipientInfo (..),
    newBouncedRecipientInfo,
    bouncedRecipientInfo_bounceType,
    bouncedRecipientInfo_recipientArn,
    bouncedRecipientInfo_recipientDsnFields,
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
    bulkEmailDestinationStatus_error,
    bulkEmailDestinationStatus_messageId,
    bulkEmailDestinationStatus_status,

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
    customVerificationEmailTemplate_failureRedirectionURL,
    customVerificationEmailTemplate_fromEmailAddress,
    customVerificationEmailTemplate_successRedirectionURL,
    customVerificationEmailTemplate_templateName,
    customVerificationEmailTemplate_templateSubject,

    -- * DeliveryOptions
    DeliveryOptions (..),
    newDeliveryOptions,
    deliveryOptions_tlsPolicy,

    -- * Destination
    Destination (..),
    newDestination,
    destination_bccAddresses,
    destination_ccAddresses,
    destination_toAddresses,

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
    identityNotificationAttributes_headersInBounceNotificationsEnabled,
    identityNotificationAttributes_headersInComplaintNotificationsEnabled,
    identityNotificationAttributes_headersInDeliveryNotificationsEnabled,
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
    messageDsn_arrivalDate,
    messageDsn_extensionFields,
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
    receiptAction_addHeaderAction,
    receiptAction_bounceAction,
    receiptAction_lambdaAction,
    receiptAction_s3Action,
    receiptAction_sNSAction,
    receiptAction_stopAction,
    receiptAction_workmailAction,

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
    receiptRule_actions,
    receiptRule_enabled,
    receiptRule_recipients,
    receiptRule_scanEnabled,
    receiptRule_tlsPolicy,
    receiptRule_name,

    -- * ReceiptRuleSetMetadata
    ReceiptRuleSetMetadata (..),
    newReceiptRuleSetMetadata,
    receiptRuleSetMetadata_createdTimestamp,
    receiptRuleSetMetadata_name,

    -- * RecipientDsnFields
    RecipientDsnFields (..),
    newRecipientDsnFields,
    recipientDsnFields_diagnosticCode,
    recipientDsnFields_extensionFields,
    recipientDsnFields_finalRecipient,
    recipientDsnFields_lastAttemptDate,
    recipientDsnFields_remoteMta,
    recipientDsnFields_action,
    recipientDsnFields_status,

    -- * ReputationOptions
    ReputationOptions (..),
    newReputationOptions,
    reputationOptions_lastFreshStart,
    reputationOptions_reputationMetricsEnabled,
    reputationOptions_sendingEnabled,

    -- * S3Action
    S3Action (..),
    newS3Action,
    s3Action_kmsKeyArn,
    s3Action_objectKeyPrefix,
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
    sendDataPoint_deliveryAttempts,
    sendDataPoint_rejects,
    sendDataPoint_timestamp,

    -- * StopAction
    StopAction (..),
    newStopAction,
    stopAction_topicArn,
    stopAction_scope,

    -- * Template
    Template (..),
    newTemplate,
    template_htmlPart,
    template_subjectPart,
    template_textPart,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SES.Types.AddHeaderAction
import Amazonka.SES.Types.BehaviorOnMXFailure
import Amazonka.SES.Types.Body
import Amazonka.SES.Types.BounceAction
import Amazonka.SES.Types.BounceType
import Amazonka.SES.Types.BouncedRecipientInfo
import Amazonka.SES.Types.BulkEmailDestination
import Amazonka.SES.Types.BulkEmailDestinationStatus
import Amazonka.SES.Types.BulkEmailStatus
import Amazonka.SES.Types.CloudWatchDestination
import Amazonka.SES.Types.CloudWatchDimensionConfiguration
import Amazonka.SES.Types.ConfigurationSet
import Amazonka.SES.Types.ConfigurationSetAttribute
import Amazonka.SES.Types.Content
import Amazonka.SES.Types.CustomMailFromStatus
import Amazonka.SES.Types.CustomVerificationEmailTemplate
import Amazonka.SES.Types.DeliveryOptions
import Amazonka.SES.Types.Destination
import Amazonka.SES.Types.DimensionValueSource
import Amazonka.SES.Types.DsnAction
import Amazonka.SES.Types.EventDestination
import Amazonka.SES.Types.EventType
import Amazonka.SES.Types.ExtensionField
import Amazonka.SES.Types.IdentityDkimAttributes
import Amazonka.SES.Types.IdentityMailFromDomainAttributes
import Amazonka.SES.Types.IdentityNotificationAttributes
import Amazonka.SES.Types.IdentityType
import Amazonka.SES.Types.IdentityVerificationAttributes
import Amazonka.SES.Types.InvocationType
import Amazonka.SES.Types.KinesisFirehoseDestination
import Amazonka.SES.Types.LambdaAction
import Amazonka.SES.Types.Message
import Amazonka.SES.Types.MessageDsn
import Amazonka.SES.Types.MessageTag
import Amazonka.SES.Types.NotificationType
import Amazonka.SES.Types.RawMessage
import Amazonka.SES.Types.ReceiptAction
import Amazonka.SES.Types.ReceiptFilter
import Amazonka.SES.Types.ReceiptFilterPolicy
import Amazonka.SES.Types.ReceiptIpFilter
import Amazonka.SES.Types.ReceiptRule
import Amazonka.SES.Types.ReceiptRuleSetMetadata
import Amazonka.SES.Types.RecipientDsnFields
import Amazonka.SES.Types.ReputationOptions
import Amazonka.SES.Types.S3Action
import Amazonka.SES.Types.SNSAction
import Amazonka.SES.Types.SNSActionEncoding
import Amazonka.SES.Types.SNSDestination
import Amazonka.SES.Types.SendDataPoint
import Amazonka.SES.Types.StopAction
import Amazonka.SES.Types.StopScope
import Amazonka.SES.Types.Template
import Amazonka.SES.Types.TemplateMetadata
import Amazonka.SES.Types.TlsPolicy
import Amazonka.SES.Types.TrackingOptions
import Amazonka.SES.Types.VerificationStatus
import Amazonka.SES.Types.WorkmailAction
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2010-12-01@ of the Amazon Simple Email Service SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "SES",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "email",
      Core.signingName = "ses",
      Core.version = "2010-12-01",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseXMLError "SES",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | Indicates that email sending is disabled for your entire Amazon SES
-- account.
--
-- You can enable or disable email sending for your Amazon SES account
-- using UpdateAccountSendingEnabled.
_AccountSendingPausedException :: Core.AsError a => Lens.Fold a Core.ServiceError
_AccountSendingPausedException =
  Core._MatchServiceError
    defaultService
    "AccountSendingPausedException"
    Prelude.. Core.hasStatus 400

-- | Indicates that a resource could not be created because of a naming
-- conflict.
_AlreadyExistsException :: Core.AsError a => Lens.Fold a Core.ServiceError
_AlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "AlreadyExists"
    Prelude.. Core.hasStatus 400

-- | Indicates that the delete operation could not be completed.
_CannotDeleteException :: Core.AsError a => Lens.Fold a Core.ServiceError
_CannotDeleteException =
  Core._MatchServiceError
    defaultService
    "CannotDelete"
    Prelude.. Core.hasStatus 400

-- | Indicates that the configuration set could not be created because of a
-- naming conflict.
_ConfigurationSetAlreadyExistsException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ConfigurationSetAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ConfigurationSetAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | Indicates that the configuration set does not exist.
_ConfigurationSetDoesNotExistException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ConfigurationSetDoesNotExistException =
  Core._MatchServiceError
    defaultService
    "ConfigurationSetDoesNotExist"
    Prelude.. Core.hasStatus 400

-- | Indicates that email sending is disabled for the configuration set.
--
-- You can enable or disable email sending for a configuration set using
-- UpdateConfigurationSetSendingEnabled.
_ConfigurationSetSendingPausedException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ConfigurationSetSendingPausedException =
  Core._MatchServiceError
    defaultService
    "ConfigurationSetSendingPausedException"
    Prelude.. Core.hasStatus 400

-- | Indicates that custom verification email template provided content is
-- invalid.
_CustomVerificationEmailInvalidContentException :: Core.AsError a => Lens.Fold a Core.ServiceError
_CustomVerificationEmailInvalidContentException =
  Core._MatchServiceError
    defaultService
    "CustomVerificationEmailInvalidContent"
    Prelude.. Core.hasStatus 400

-- | Indicates that a custom verification email template with the name you
-- specified already exists.
_CustomVerificationEmailTemplateAlreadyExistsException :: Core.AsError a => Lens.Fold a Core.ServiceError
_CustomVerificationEmailTemplateAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "CustomVerificationEmailTemplateAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | Indicates that a custom verification email template with the name you
-- specified does not exist.
_CustomVerificationEmailTemplateDoesNotExistException :: Core.AsError a => Lens.Fold a Core.ServiceError
_CustomVerificationEmailTemplateDoesNotExistException =
  Core._MatchServiceError
    defaultService
    "CustomVerificationEmailTemplateDoesNotExist"
    Prelude.. Core.hasStatus 400

-- | Indicates that the event destination could not be created because of a
-- naming conflict.
_EventDestinationAlreadyExistsException :: Core.AsError a => Lens.Fold a Core.ServiceError
_EventDestinationAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "EventDestinationAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | Indicates that the event destination does not exist.
_EventDestinationDoesNotExistException :: Core.AsError a => Lens.Fold a Core.ServiceError
_EventDestinationDoesNotExistException =
  Core._MatchServiceError
    defaultService
    "EventDestinationDoesNotExist"
    Prelude.. Core.hasStatus 400

-- | Indicates that the sender address specified for a custom verification
-- email is not verified, and is therefore not eligible to send the custom
-- verification email.
_FromEmailAddressNotVerifiedException :: Core.AsError a => Lens.Fold a Core.ServiceError
_FromEmailAddressNotVerifiedException =
  Core._MatchServiceError
    defaultService
    "FromEmailAddressNotVerified"
    Prelude.. Core.hasStatus 400

-- | Indicates that the Amazon CloudWatch destination is invalid. See the
-- error message for details.
_InvalidCloudWatchDestinationException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidCloudWatchDestinationException =
  Core._MatchServiceError
    defaultService
    "InvalidCloudWatchDestination"
    Prelude.. Core.hasStatus 400

-- | Indicates that the configuration set is invalid. See the error message
-- for details.
_InvalidConfigurationSetException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidConfigurationSetException =
  Core._MatchServiceError
    defaultService
    "InvalidConfigurationSet"
    Prelude.. Core.hasStatus 400

-- | Indicates that provided delivery option is invalid.
_InvalidDeliveryOptionsException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidDeliveryOptionsException =
  Core._MatchServiceError
    defaultService
    "InvalidDeliveryOptions"
    Prelude.. Core.hasStatus 400

-- | Indicates that the Amazon Kinesis Firehose destination is invalid. See
-- the error message for details.
_InvalidFirehoseDestinationException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidFirehoseDestinationException =
  Core._MatchServiceError
    defaultService
    "InvalidFirehoseDestination"
    Prelude.. Core.hasStatus 400

-- | Indicates that the provided AWS Lambda function is invalid, or that
-- Amazon SES could not execute the provided function, possibly due to
-- permissions issues. For information about giving permissions, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-permissions.html Amazon SES Developer Guide>.
_InvalidLambdaFunctionException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidLambdaFunctionException =
  Core._MatchServiceError
    defaultService
    "InvalidLambdaFunction"
    Prelude.. Core.hasStatus 400

-- | Indicates that the provided policy is invalid. Check the error stack for
-- more information about what caused the error.
_InvalidPolicyException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidPolicyException =
  Core._MatchServiceError
    defaultService
    "InvalidPolicy"
    Prelude.. Core.hasStatus 400

-- | Indicates that one or more of the replacement values you provided is
-- invalid. This error may occur when the TemplateData object contains
-- invalid JSON.
_InvalidRenderingParameterException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidRenderingParameterException =
  Core._MatchServiceError
    defaultService
    "InvalidRenderingParameter"
    Prelude.. Core.hasStatus 400

-- | Indicates that the provided Amazon S3 bucket or AWS KMS encryption key
-- is invalid, or that Amazon SES could not publish to the bucket, possibly
-- due to permissions issues. For information about giving permissions, see
-- the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-permissions.html Amazon SES Developer Guide>.
_InvalidS3ConfigurationException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidS3ConfigurationException =
  Core._MatchServiceError
    defaultService
    "InvalidS3Configuration"
    Prelude.. Core.hasStatus 400

-- | Indicates that the Amazon Simple Notification Service (Amazon SNS)
-- destination is invalid. See the error message for details.
_InvalidSNSDestinationException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidSNSDestinationException =
  Core._MatchServiceError
    defaultService
    "InvalidSNSDestination"
    Prelude.. Core.hasStatus 400

-- | Indicates that the provided Amazon SNS topic is invalid, or that Amazon
-- SES could not publish to the topic, possibly due to permissions issues.
-- For information about giving permissions, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-permissions.html Amazon SES Developer Guide>.
_InvalidSnsTopicException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidSnsTopicException =
  Core._MatchServiceError
    defaultService
    "InvalidSnsTopic"
    Prelude.. Core.hasStatus 400

-- | Indicates that the template that you specified could not be rendered.
-- This issue may occur when a template refers to a partial that does not
-- exist.
_InvalidTemplateException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidTemplateException =
  Core._MatchServiceError
    defaultService
    "InvalidTemplate"
    Prelude.. Core.hasStatus 400

-- | Indicates that the custom domain to be used for open and click tracking
-- redirects is invalid. This error appears most often in the following
-- situations:
--
-- -   When the tracking domain you specified is not verified in Amazon
--     SES.
--
-- -   When the tracking domain you specified is not a valid domain or
--     subdomain.
_InvalidTrackingOptionsException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidTrackingOptionsException =
  Core._MatchServiceError
    defaultService
    "InvalidTrackingOptions"
    Prelude.. Core.hasStatus 400

-- | Indicates that a resource could not be created because of service
-- limits. For a list of Amazon SES limits, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/limits.html Amazon SES Developer Guide>.
_LimitExceededException :: Core.AsError a => Lens.Fold a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceeded"
    Prelude.. Core.hasStatus 400

-- | Indicates that the message could not be sent because Amazon SES could
-- not read the MX record required to use the specified MAIL FROM domain.
-- For information about editing the custom MAIL FROM domain settings for
-- an identity, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/mail-from-edit.html Amazon SES Developer Guide>.
_MailFromDomainNotVerifiedException :: Core.AsError a => Lens.Fold a Core.ServiceError
_MailFromDomainNotVerifiedException =
  Core._MatchServiceError
    defaultService
    "MailFromDomainNotVerifiedException"
    Prelude.. Core.hasStatus 400

-- | Indicates that the action failed, and the message could not be sent.
-- Check the error stack for more information about what caused the error.
_MessageRejected :: Core.AsError a => Lens.Fold a Core.ServiceError
_MessageRejected =
  Core._MatchServiceError
    defaultService
    "MessageRejected"
    Prelude.. Core.hasStatus 400

-- | Indicates that one or more of the replacement values for the specified
-- template was not specified. Ensure that the TemplateData object contains
-- references to all of the replacement tags in the specified template.
_MissingRenderingAttributeException :: Core.AsError a => Lens.Fold a Core.ServiceError
_MissingRenderingAttributeException =
  Core._MatchServiceError
    defaultService
    "MissingRenderingAttribute"
    Prelude.. Core.hasStatus 400

-- | Indicates that the account has not been granted production access.
_ProductionAccessNotGrantedException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ProductionAccessNotGrantedException =
  Core._MatchServiceError
    defaultService
    "ProductionAccessNotGranted"
    Prelude.. Core.hasStatus 400

-- | Indicates that the provided receipt rule does not exist.
_RuleDoesNotExistException :: Core.AsError a => Lens.Fold a Core.ServiceError
_RuleDoesNotExistException =
  Core._MatchServiceError
    defaultService
    "RuleDoesNotExist"
    Prelude.. Core.hasStatus 400

-- | Indicates that the provided receipt rule set does not exist.
_RuleSetDoesNotExistException :: Core.AsError a => Lens.Fold a Core.ServiceError
_RuleSetDoesNotExistException =
  Core._MatchServiceError
    defaultService
    "RuleSetDoesNotExist"
    Prelude.. Core.hasStatus 400

-- | Indicates that the Template object you specified does not exist in your
-- Amazon SES account.
_TemplateDoesNotExistException :: Core.AsError a => Lens.Fold a Core.ServiceError
_TemplateDoesNotExistException =
  Core._MatchServiceError
    defaultService
    "TemplateDoesNotExist"
    Prelude.. Core.hasStatus 400

-- | Indicates that the configuration set you specified already contains a
-- TrackingOptions object.
_TrackingOptionsAlreadyExistsException :: Core.AsError a => Lens.Fold a Core.ServiceError
_TrackingOptionsAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "TrackingOptionsAlreadyExistsException"
    Prelude.. Core.hasStatus 400

-- | Indicates that the TrackingOptions object you specified does not exist.
_TrackingOptionsDoesNotExistException :: Core.AsError a => Lens.Fold a Core.ServiceError
_TrackingOptionsDoesNotExistException =
  Core._MatchServiceError
    defaultService
    "TrackingOptionsDoesNotExistException"
    Prelude.. Core.hasStatus 400
