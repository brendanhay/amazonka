{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SESV2.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SESV2.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _MessageRejected,
    _MailFromDomainNotVerifiedException,
    _ConflictException,
    _NotFoundException,
    _TooManyRequestsException,
    _ConcurrentModificationException,
    _InvalidNextTokenException,
    _AccountSuspendedException,
    _SendingPausedException,
    _BadRequestException,
    _AlreadyExistsException,
    _LimitExceededException,

    -- * BehaviorOnMxFailure
    BehaviorOnMxFailure (..),

    -- * BulkEmailStatus
    BulkEmailStatus (..),

    -- * ContactLanguage
    ContactLanguage (..),

    -- * ContactListImportAction
    ContactListImportAction (..),

    -- * DataFormat
    DataFormat (..),

    -- * DeliverabilityDashboardAccountStatus
    DeliverabilityDashboardAccountStatus (..),

    -- * DeliverabilityTestStatus
    DeliverabilityTestStatus (..),

    -- * DimensionValueSource
    DimensionValueSource (..),

    -- * DkimSigningAttributesOrigin
    DkimSigningAttributesOrigin (..),

    -- * DkimSigningKeyLength
    DkimSigningKeyLength (..),

    -- * DkimStatus
    DkimStatus (..),

    -- * EventType
    EventType (..),

    -- * IdentityType
    IdentityType (..),

    -- * ImportDestinationType
    ImportDestinationType (..),

    -- * JobStatus
    JobStatus (..),

    -- * MailFromDomainStatus
    MailFromDomainStatus (..),

    -- * MailType
    MailType (..),

    -- * ReviewStatus
    ReviewStatus (..),

    -- * SubscriptionStatus
    SubscriptionStatus (..),

    -- * SuppressionListImportAction
    SuppressionListImportAction (..),

    -- * SuppressionListReason
    SuppressionListReason (..),

    -- * TlsPolicy
    TlsPolicy (..),

    -- * WarmupStatus
    WarmupStatus (..),

    -- * AccountDetails
    AccountDetails (..),
    newAccountDetails,
    accountDetails_reviewDetails,
    accountDetails_mailType,
    accountDetails_useCaseDescription,
    accountDetails_contactLanguage,
    accountDetails_additionalContactEmailAddresses,
    accountDetails_websiteURL,

    -- * BlacklistEntry
    BlacklistEntry (..),
    newBlacklistEntry,
    blacklistEntry_listingTime,
    blacklistEntry_rblName,
    blacklistEntry_description,

    -- * Body
    Body (..),
    newBody,
    body_text,
    body_html,

    -- * BulkEmailContent
    BulkEmailContent (..),
    newBulkEmailContent,
    bulkEmailContent_template,

    -- * BulkEmailEntry
    BulkEmailEntry (..),
    newBulkEmailEntry,
    bulkEmailEntry_replacementEmailContent,
    bulkEmailEntry_replacementTags,
    bulkEmailEntry_destination,

    -- * BulkEmailEntryResult
    BulkEmailEntryResult (..),
    newBulkEmailEntryResult,
    bulkEmailEntryResult_status,
    bulkEmailEntryResult_error,
    bulkEmailEntryResult_messageId,

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

    -- * Contact
    Contact (..),
    newContact,
    contact_unsubscribeAll,
    contact_topicDefaultPreferences,
    contact_emailAddress,
    contact_lastUpdatedTimestamp,
    contact_topicPreferences,

    -- * ContactList
    ContactList (..),
    newContactList,
    contactList_contactListName,
    contactList_lastUpdatedTimestamp,

    -- * ContactListDestination
    ContactListDestination (..),
    newContactListDestination,
    contactListDestination_contactListName,
    contactListDestination_contactListImportAction,

    -- * Content
    Content (..),
    newContent,
    content_charset,
    content_data,

    -- * CustomVerificationEmailTemplateMetadata
    CustomVerificationEmailTemplateMetadata (..),
    newCustomVerificationEmailTemplateMetadata,
    customVerificationEmailTemplateMetadata_fromEmailAddress,
    customVerificationEmailTemplateMetadata_templateName,
    customVerificationEmailTemplateMetadata_failureRedirectionURL,
    customVerificationEmailTemplateMetadata_templateSubject,
    customVerificationEmailTemplateMetadata_successRedirectionURL,

    -- * DailyVolume
    DailyVolume (..),
    newDailyVolume,
    dailyVolume_domainIspPlacements,
    dailyVolume_startDate,
    dailyVolume_volumeStatistics,

    -- * DedicatedIp
    DedicatedIp (..),
    newDedicatedIp,
    dedicatedIp_poolName,
    dedicatedIp_ip,
    dedicatedIp_warmupStatus,
    dedicatedIp_warmupPercentage,

    -- * DeliverabilityTestReport
    DeliverabilityTestReport (..),
    newDeliverabilityTestReport,
    deliverabilityTestReport_subject,
    deliverabilityTestReport_fromEmailAddress,
    deliverabilityTestReport_createDate,
    deliverabilityTestReport_reportId,
    deliverabilityTestReport_reportName,
    deliverabilityTestReport_deliverabilityTestStatus,

    -- * DeliveryOptions
    DeliveryOptions (..),
    newDeliveryOptions,
    deliveryOptions_sendingPoolName,
    deliveryOptions_tlsPolicy,

    -- * Destination
    Destination (..),
    newDestination,
    destination_bccAddresses,
    destination_ccAddresses,
    destination_toAddresses,

    -- * DkimAttributes
    DkimAttributes (..),
    newDkimAttributes,
    dkimAttributes_status,
    dkimAttributes_nextSigningKeyLength,
    dkimAttributes_tokens,
    dkimAttributes_signingEnabled,
    dkimAttributes_currentSigningKeyLength,
    dkimAttributes_lastKeyGenerationTimestamp,
    dkimAttributes_signingAttributesOrigin,

    -- * DkimSigningAttributes
    DkimSigningAttributes (..),
    newDkimSigningAttributes,
    dkimSigningAttributes_nextSigningKeyLength,
    dkimSigningAttributes_domainSigningPrivateKey,
    dkimSigningAttributes_domainSigningSelector,

    -- * DomainDeliverabilityCampaign
    DomainDeliverabilityCampaign (..),
    newDomainDeliverabilityCampaign,
    domainDeliverabilityCampaign_spamCount,
    domainDeliverabilityCampaign_subject,
    domainDeliverabilityCampaign_esps,
    domainDeliverabilityCampaign_fromAddress,
    domainDeliverabilityCampaign_deleteRate,
    domainDeliverabilityCampaign_campaignId,
    domainDeliverabilityCampaign_sendingIps,
    domainDeliverabilityCampaign_firstSeenDateTime,
    domainDeliverabilityCampaign_inboxCount,
    domainDeliverabilityCampaign_readDeleteRate,
    domainDeliverabilityCampaign_projectedVolume,
    domainDeliverabilityCampaign_imageUrl,
    domainDeliverabilityCampaign_readRate,
    domainDeliverabilityCampaign_lastSeenDateTime,

    -- * DomainDeliverabilityTrackingOption
    DomainDeliverabilityTrackingOption (..),
    newDomainDeliverabilityTrackingOption,
    domainDeliverabilityTrackingOption_domain,
    domainDeliverabilityTrackingOption_subscriptionStartDate,
    domainDeliverabilityTrackingOption_inboxPlacementTrackingOption,

    -- * DomainIspPlacement
    DomainIspPlacement (..),
    newDomainIspPlacement,
    domainIspPlacement_spamPercentage,
    domainIspPlacement_inboxRawCount,
    domainIspPlacement_ispName,
    domainIspPlacement_inboxPercentage,
    domainIspPlacement_spamRawCount,

    -- * EmailContent
    EmailContent (..),
    newEmailContent,
    emailContent_raw,
    emailContent_simple,
    emailContent_template,

    -- * EmailTemplateContent
    EmailTemplateContent (..),
    newEmailTemplateContent,
    emailTemplateContent_subject,
    emailTemplateContent_text,
    emailTemplateContent_html,

    -- * EmailTemplateMetadata
    EmailTemplateMetadata (..),
    newEmailTemplateMetadata,
    emailTemplateMetadata_templateName,
    emailTemplateMetadata_createdTimestamp,

    -- * EventDestination
    EventDestination (..),
    newEventDestination,
    eventDestination_pinpointDestination,
    eventDestination_enabled,
    eventDestination_kinesisFirehoseDestination,
    eventDestination_cloudWatchDestination,
    eventDestination_snsDestination,
    eventDestination_name,
    eventDestination_matchingEventTypes,

    -- * EventDestinationDefinition
    EventDestinationDefinition (..),
    newEventDestinationDefinition,
    eventDestinationDefinition_matchingEventTypes,
    eventDestinationDefinition_pinpointDestination,
    eventDestinationDefinition_enabled,
    eventDestinationDefinition_kinesisFirehoseDestination,
    eventDestinationDefinition_cloudWatchDestination,
    eventDestinationDefinition_snsDestination,

    -- * FailureInfo
    FailureInfo (..),
    newFailureInfo,
    failureInfo_failedRecordsS3Url,
    failureInfo_errorMessage,

    -- * IdentityInfo
    IdentityInfo (..),
    newIdentityInfo,
    identityInfo_identityType,
    identityInfo_identityName,
    identityInfo_sendingEnabled,

    -- * ImportDataSource
    ImportDataSource (..),
    newImportDataSource,
    importDataSource_s3Url,
    importDataSource_dataFormat,

    -- * ImportDestination
    ImportDestination (..),
    newImportDestination,
    importDestination_suppressionListDestination,
    importDestination_contactListDestination,

    -- * ImportJobSummary
    ImportJobSummary (..),
    newImportJobSummary,
    importJobSummary_jobId,
    importJobSummary_importDestination,
    importJobSummary_jobStatus,
    importJobSummary_createdTimestamp,

    -- * InboxPlacementTrackingOption
    InboxPlacementTrackingOption (..),
    newInboxPlacementTrackingOption,
    inboxPlacementTrackingOption_trackedIsps,
    inboxPlacementTrackingOption_global,

    -- * IspPlacement
    IspPlacement (..),
    newIspPlacement,
    ispPlacement_placementStatistics,
    ispPlacement_ispName,

    -- * KinesisFirehoseDestination
    KinesisFirehoseDestination (..),
    newKinesisFirehoseDestination,
    kinesisFirehoseDestination_iamRoleArn,
    kinesisFirehoseDestination_deliveryStreamArn,

    -- * ListContactsFilter
    ListContactsFilter (..),
    newListContactsFilter,
    listContactsFilter_filteredStatus,
    listContactsFilter_topicFilter,

    -- * ListManagementOptions
    ListManagementOptions (..),
    newListManagementOptions,
    listManagementOptions_topicName,
    listManagementOptions_contactListName,

    -- * MailFromAttributes
    MailFromAttributes (..),
    newMailFromAttributes,
    mailFromAttributes_mailFromDomain,
    mailFromAttributes_mailFromDomainStatus,
    mailFromAttributes_behaviorOnMxFailure,

    -- * Message
    Message (..),
    newMessage,
    message_subject,
    message_body,

    -- * MessageTag
    MessageTag (..),
    newMessageTag,
    messageTag_name,
    messageTag_value,

    -- * OverallVolume
    OverallVolume (..),
    newOverallVolume,
    overallVolume_domainIspPlacements,
    overallVolume_volumeStatistics,
    overallVolume_readRatePercent,

    -- * PinpointDestination
    PinpointDestination (..),
    newPinpointDestination,
    pinpointDestination_applicationArn,

    -- * PlacementStatistics
    PlacementStatistics (..),
    newPlacementStatistics,
    placementStatistics_missingPercentage,
    placementStatistics_spamPercentage,
    placementStatistics_spfPercentage,
    placementStatistics_dkimPercentage,
    placementStatistics_inboxPercentage,

    -- * RawMessage
    RawMessage (..),
    newRawMessage,
    rawMessage_data,

    -- * ReplacementEmailContent
    ReplacementEmailContent (..),
    newReplacementEmailContent,
    replacementEmailContent_replacementTemplate,

    -- * ReplacementTemplate
    ReplacementTemplate (..),
    newReplacementTemplate,
    replacementTemplate_replacementTemplateData,

    -- * ReputationOptions
    ReputationOptions (..),
    newReputationOptions,
    reputationOptions_lastFreshStart,
    reputationOptions_reputationMetricsEnabled,

    -- * ReviewDetails
    ReviewDetails (..),
    newReviewDetails,
    reviewDetails_status,
    reviewDetails_caseId,

    -- * SendQuota
    SendQuota (..),
    newSendQuota,
    sendQuota_maxSendRate,
    sendQuota_sentLast24Hours,
    sendQuota_max24HourSend,

    -- * SendingOptions
    SendingOptions (..),
    newSendingOptions,
    sendingOptions_sendingEnabled,

    -- * SnsDestination
    SnsDestination (..),
    newSnsDestination,
    snsDestination_topicArn,

    -- * SuppressedDestination
    SuppressedDestination (..),
    newSuppressedDestination,
    suppressedDestination_attributes,
    suppressedDestination_emailAddress,
    suppressedDestination_reason,
    suppressedDestination_lastUpdateTime,

    -- * SuppressedDestinationAttributes
    SuppressedDestinationAttributes (..),
    newSuppressedDestinationAttributes,
    suppressedDestinationAttributes_feedbackId,
    suppressedDestinationAttributes_messageId,

    -- * SuppressedDestinationSummary
    SuppressedDestinationSummary (..),
    newSuppressedDestinationSummary,
    suppressedDestinationSummary_emailAddress,
    suppressedDestinationSummary_reason,
    suppressedDestinationSummary_lastUpdateTime,

    -- * SuppressionAttributes
    SuppressionAttributes (..),
    newSuppressionAttributes,
    suppressionAttributes_suppressedReasons,

    -- * SuppressionListDestination
    SuppressionListDestination (..),
    newSuppressionListDestination,
    suppressionListDestination_suppressionListImportAction,

    -- * SuppressionOptions
    SuppressionOptions (..),
    newSuppressionOptions,
    suppressionOptions_suppressedReasons,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * Template
    Template (..),
    newTemplate,
    template_templateName,
    template_templateArn,
    template_templateData,

    -- * Topic
    Topic (..),
    newTopic,
    topic_description,
    topic_topicName,
    topic_displayName,
    topic_defaultSubscriptionStatus,

    -- * TopicFilter
    TopicFilter (..),
    newTopicFilter,
    topicFilter_topicName,
    topicFilter_useDefaultIfPreferenceUnavailable,

    -- * TopicPreference
    TopicPreference (..),
    newTopicPreference,
    topicPreference_topicName,
    topicPreference_subscriptionStatus,

    -- * TrackingOptions
    TrackingOptions (..),
    newTrackingOptions,
    trackingOptions_customRedirectDomain,

    -- * VolumeStatistics
    VolumeStatistics (..),
    newVolumeStatistics,
    volumeStatistics_inboxRawCount,
    volumeStatistics_projectedSpam,
    volumeStatistics_projectedInbox,
    volumeStatistics_spamRawCount,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SESV2.Types.AccountDetails
import Amazonka.SESV2.Types.BehaviorOnMxFailure
import Amazonka.SESV2.Types.BlacklistEntry
import Amazonka.SESV2.Types.Body
import Amazonka.SESV2.Types.BulkEmailContent
import Amazonka.SESV2.Types.BulkEmailEntry
import Amazonka.SESV2.Types.BulkEmailEntryResult
import Amazonka.SESV2.Types.BulkEmailStatus
import Amazonka.SESV2.Types.CloudWatchDestination
import Amazonka.SESV2.Types.CloudWatchDimensionConfiguration
import Amazonka.SESV2.Types.Contact
import Amazonka.SESV2.Types.ContactLanguage
import Amazonka.SESV2.Types.ContactList
import Amazonka.SESV2.Types.ContactListDestination
import Amazonka.SESV2.Types.ContactListImportAction
import Amazonka.SESV2.Types.Content
import Amazonka.SESV2.Types.CustomVerificationEmailTemplateMetadata
import Amazonka.SESV2.Types.DailyVolume
import Amazonka.SESV2.Types.DataFormat
import Amazonka.SESV2.Types.DedicatedIp
import Amazonka.SESV2.Types.DeliverabilityDashboardAccountStatus
import Amazonka.SESV2.Types.DeliverabilityTestReport
import Amazonka.SESV2.Types.DeliverabilityTestStatus
import Amazonka.SESV2.Types.DeliveryOptions
import Amazonka.SESV2.Types.Destination
import Amazonka.SESV2.Types.DimensionValueSource
import Amazonka.SESV2.Types.DkimAttributes
import Amazonka.SESV2.Types.DkimSigningAttributes
import Amazonka.SESV2.Types.DkimSigningAttributesOrigin
import Amazonka.SESV2.Types.DkimSigningKeyLength
import Amazonka.SESV2.Types.DkimStatus
import Amazonka.SESV2.Types.DomainDeliverabilityCampaign
import Amazonka.SESV2.Types.DomainDeliverabilityTrackingOption
import Amazonka.SESV2.Types.DomainIspPlacement
import Amazonka.SESV2.Types.EmailContent
import Amazonka.SESV2.Types.EmailTemplateContent
import Amazonka.SESV2.Types.EmailTemplateMetadata
import Amazonka.SESV2.Types.EventDestination
import Amazonka.SESV2.Types.EventDestinationDefinition
import Amazonka.SESV2.Types.EventType
import Amazonka.SESV2.Types.FailureInfo
import Amazonka.SESV2.Types.IdentityInfo
import Amazonka.SESV2.Types.IdentityType
import Amazonka.SESV2.Types.ImportDataSource
import Amazonka.SESV2.Types.ImportDestination
import Amazonka.SESV2.Types.ImportDestinationType
import Amazonka.SESV2.Types.ImportJobSummary
import Amazonka.SESV2.Types.InboxPlacementTrackingOption
import Amazonka.SESV2.Types.IspPlacement
import Amazonka.SESV2.Types.JobStatus
import Amazonka.SESV2.Types.KinesisFirehoseDestination
import Amazonka.SESV2.Types.ListContactsFilter
import Amazonka.SESV2.Types.ListManagementOptions
import Amazonka.SESV2.Types.MailFromAttributes
import Amazonka.SESV2.Types.MailFromDomainStatus
import Amazonka.SESV2.Types.MailType
import Amazonka.SESV2.Types.Message
import Amazonka.SESV2.Types.MessageTag
import Amazonka.SESV2.Types.OverallVolume
import Amazonka.SESV2.Types.PinpointDestination
import Amazonka.SESV2.Types.PlacementStatistics
import Amazonka.SESV2.Types.RawMessage
import Amazonka.SESV2.Types.ReplacementEmailContent
import Amazonka.SESV2.Types.ReplacementTemplate
import Amazonka.SESV2.Types.ReputationOptions
import Amazonka.SESV2.Types.ReviewDetails
import Amazonka.SESV2.Types.ReviewStatus
import Amazonka.SESV2.Types.SendQuota
import Amazonka.SESV2.Types.SendingOptions
import Amazonka.SESV2.Types.SnsDestination
import Amazonka.SESV2.Types.SubscriptionStatus
import Amazonka.SESV2.Types.SuppressedDestination
import Amazonka.SESV2.Types.SuppressedDestinationAttributes
import Amazonka.SESV2.Types.SuppressedDestinationSummary
import Amazonka.SESV2.Types.SuppressionAttributes
import Amazonka.SESV2.Types.SuppressionListDestination
import Amazonka.SESV2.Types.SuppressionListImportAction
import Amazonka.SESV2.Types.SuppressionListReason
import Amazonka.SESV2.Types.SuppressionOptions
import Amazonka.SESV2.Types.Tag
import Amazonka.SESV2.Types.Template
import Amazonka.SESV2.Types.TlsPolicy
import Amazonka.SESV2.Types.Topic
import Amazonka.SESV2.Types.TopicFilter
import Amazonka.SESV2.Types.TopicPreference
import Amazonka.SESV2.Types.TrackingOptions
import Amazonka.SESV2.Types.VolumeStatistics
import Amazonka.SESV2.Types.WarmupStatus
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2019-09-27@ of the Amazon Simple Email Service SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "SESV2",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "email",
      Core._serviceSigningName = "ses",
      Core._serviceVersion = "2019-09-27",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "SESV2",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | The message can\'t be sent because it contains invalid content.
_MessageRejected :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MessageRejected =
  Core._MatchServiceError
    defaultService
    "MessageRejected"
    Prelude.. Core.hasStatus 400

-- | The message can\'t be sent because the sending domain isn\'t verified.
_MailFromDomainNotVerifiedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MailFromDomainNotVerifiedException =
  Core._MatchServiceError
    defaultService
    "MailFromDomainNotVerifiedException"
    Prelude.. Core.hasStatus 400

-- | If there is already an ongoing account details update under review.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | The resource you attempted to access doesn\'t exist.
_NotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError
    defaultService
    "NotFoundException"
    Prelude.. Core.hasStatus 404

-- | Too many requests have been made to the operation.
_TooManyRequestsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyRequestsException =
  Core._MatchServiceError
    defaultService
    "TooManyRequestsException"
    Prelude.. Core.hasStatus 429

-- | The resource is being modified by another operation or thread.
_ConcurrentModificationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConcurrentModificationException =
  Core._MatchServiceError
    defaultService
    "ConcurrentModificationException"
    Prelude.. Core.hasStatus 500

-- | The specified request includes an invalid or expired token.
_InvalidNextTokenException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidNextTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidNextTokenException"
    Prelude.. Core.hasStatus 400

-- | The message can\'t be sent because the account\'s ability to send email
-- has been permanently restricted.
_AccountSuspendedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccountSuspendedException =
  Core._MatchServiceError
    defaultService
    "AccountSuspendedException"
    Prelude.. Core.hasStatus 400

-- | The message can\'t be sent because the account\'s ability to send email
-- is currently paused.
_SendingPausedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SendingPausedException =
  Core._MatchServiceError
    defaultService
    "SendingPausedException"
    Prelude.. Core.hasStatus 400

-- | The input you provided is invalid.
_BadRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError
    defaultService
    "BadRequestException"
    Prelude.. Core.hasStatus 400

-- | The resource specified in your request already exists.
_AlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "AlreadyExistsException"
    Prelude.. Core.hasStatus 400

-- | There are too many instances of the specified resource type.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"
    Prelude.. Core.hasStatus 400
