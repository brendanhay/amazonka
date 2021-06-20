{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SESv2.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SESv2.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _NotFoundException,
    _BadRequestException,
    _MailFromDomainNotVerifiedException,
    _SendingPausedException,
    _MessageRejected,
    _ConcurrentModificationException,
    _InvalidNextTokenException,
    _LimitExceededException,
    _ConflictException,
    _AlreadyExistsException,
    _AccountSuspendedException,
    _TooManyRequestsException,

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
    accountDetails_useCaseDescription,
    accountDetails_mailType,
    accountDetails_contactLanguage,
    accountDetails_websiteURL,
    accountDetails_reviewDetails,
    accountDetails_additionalContactEmailAddresses,

    -- * BlacklistEntry
    BlacklistEntry (..),
    newBlacklistEntry,
    blacklistEntry_rblName,
    blacklistEntry_listingTime,
    blacklistEntry_description,

    -- * Body
    Body (..),
    newBody,
    body_html,
    body_text,

    -- * BulkEmailContent
    BulkEmailContent (..),
    newBulkEmailContent,
    bulkEmailContent_template,

    -- * BulkEmailEntry
    BulkEmailEntry (..),
    newBulkEmailEntry,
    bulkEmailEntry_replacementTags,
    bulkEmailEntry_replacementEmailContent,
    bulkEmailEntry_destination,

    -- * BulkEmailEntryResult
    BulkEmailEntryResult (..),
    newBulkEmailEntryResult,
    bulkEmailEntryResult_status,
    bulkEmailEntryResult_messageId,
    bulkEmailEntryResult_error,

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
    contact_topicPreferences,
    contact_lastUpdatedTimestamp,
    contact_emailAddress,

    -- * ContactList
    ContactList (..),
    newContactList,
    contactList_lastUpdatedTimestamp,
    contactList_contactListName,

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
    customVerificationEmailTemplateMetadata_templateName,
    customVerificationEmailTemplateMetadata_templateSubject,
    customVerificationEmailTemplateMetadata_fromEmailAddress,
    customVerificationEmailTemplateMetadata_successRedirectionURL,
    customVerificationEmailTemplateMetadata_failureRedirectionURL,

    -- * DailyVolume
    DailyVolume (..),
    newDailyVolume,
    dailyVolume_startDate,
    dailyVolume_volumeStatistics,
    dailyVolume_domainIspPlacements,

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
    deliverabilityTestReport_fromEmailAddress,
    deliverabilityTestReport_reportName,
    deliverabilityTestReport_reportId,
    deliverabilityTestReport_createDate,
    deliverabilityTestReport_deliverabilityTestStatus,
    deliverabilityTestReport_subject,

    -- * DeliveryOptions
    DeliveryOptions (..),
    newDeliveryOptions,
    deliveryOptions_sendingPoolName,
    deliveryOptions_tlsPolicy,

    -- * Destination
    Destination (..),
    newDestination,
    destination_toAddresses,
    destination_ccAddresses,
    destination_bccAddresses,

    -- * DkimAttributes
    DkimAttributes (..),
    newDkimAttributes,
    dkimAttributes_status,
    dkimAttributes_tokens,
    dkimAttributes_signingAttributesOrigin,
    dkimAttributes_signingEnabled,

    -- * DkimSigningAttributes
    DkimSigningAttributes (..),
    newDkimSigningAttributes,
    dkimSigningAttributes_domainSigningSelector,
    dkimSigningAttributes_domainSigningPrivateKey,

    -- * DomainDeliverabilityCampaign
    DomainDeliverabilityCampaign (..),
    newDomainDeliverabilityCampaign,
    domainDeliverabilityCampaign_projectedVolume,
    domainDeliverabilityCampaign_readDeleteRate,
    domainDeliverabilityCampaign_inboxCount,
    domainDeliverabilityCampaign_firstSeenDateTime,
    domainDeliverabilityCampaign_lastSeenDateTime,
    domainDeliverabilityCampaign_deleteRate,
    domainDeliverabilityCampaign_campaignId,
    domainDeliverabilityCampaign_spamCount,
    domainDeliverabilityCampaign_imageUrl,
    domainDeliverabilityCampaign_subject,
    domainDeliverabilityCampaign_sendingIps,
    domainDeliverabilityCampaign_fromAddress,
    domainDeliverabilityCampaign_readRate,
    domainDeliverabilityCampaign_esps,

    -- * DomainDeliverabilityTrackingOption
    DomainDeliverabilityTrackingOption (..),
    newDomainDeliverabilityTrackingOption,
    domainDeliverabilityTrackingOption_subscriptionStartDate,
    domainDeliverabilityTrackingOption_domain,
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
    emailTemplateContent_html,
    emailTemplateContent_subject,
    emailTemplateContent_text,

    -- * EmailTemplateMetadata
    EmailTemplateMetadata (..),
    newEmailTemplateMetadata,
    emailTemplateMetadata_templateName,
    emailTemplateMetadata_createdTimestamp,

    -- * EventDestination
    EventDestination (..),
    newEventDestination,
    eventDestination_cloudWatchDestination,
    eventDestination_enabled,
    eventDestination_pinpointDestination,
    eventDestination_kinesisFirehoseDestination,
    eventDestination_snsDestination,
    eventDestination_name,
    eventDestination_matchingEventTypes,

    -- * EventDestinationDefinition
    EventDestinationDefinition (..),
    newEventDestinationDefinition,
    eventDestinationDefinition_matchingEventTypes,
    eventDestinationDefinition_cloudWatchDestination,
    eventDestinationDefinition_enabled,
    eventDestinationDefinition_pinpointDestination,
    eventDestinationDefinition_kinesisFirehoseDestination,
    eventDestinationDefinition_snsDestination,

    -- * FailureInfo
    FailureInfo (..),
    newFailureInfo,
    failureInfo_failedRecordsS3Url,
    failureInfo_errorMessage,

    -- * IdentityInfo
    IdentityInfo (..),
    newIdentityInfo,
    identityInfo_identityName,
    identityInfo_sendingEnabled,
    identityInfo_identityType,

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
    importJobSummary_createdTimestamp,
    importJobSummary_jobStatus,
    importJobSummary_importDestination,
    importJobSummary_jobId,

    -- * InboxPlacementTrackingOption
    InboxPlacementTrackingOption (..),
    newInboxPlacementTrackingOption,
    inboxPlacementTrackingOption_trackedIsps,
    inboxPlacementTrackingOption_global,

    -- * IspPlacement
    IspPlacement (..),
    newIspPlacement,
    ispPlacement_ispName,
    ispPlacement_placementStatistics,

    -- * KinesisFirehoseDestination
    KinesisFirehoseDestination (..),
    newKinesisFirehoseDestination,
    kinesisFirehoseDestination_iamRoleArn,
    kinesisFirehoseDestination_deliveryStreamArn,

    -- * ListContactsFilter
    ListContactsFilter (..),
    newListContactsFilter,
    listContactsFilter_topicFilter,
    listContactsFilter_filteredStatus,

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
    overallVolume_readRatePercent,
    overallVolume_volumeStatistics,
    overallVolume_domainIspPlacements,

    -- * PinpointDestination
    PinpointDestination (..),
    newPinpointDestination,
    pinpointDestination_applicationArn,

    -- * PlacementStatistics
    PlacementStatistics (..),
    newPlacementStatistics,
    placementStatistics_spamPercentage,
    placementStatistics_dkimPercentage,
    placementStatistics_spfPercentage,
    placementStatistics_inboxPercentage,
    placementStatistics_missingPercentage,

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
    reputationOptions_reputationMetricsEnabled,
    reputationOptions_lastFreshStart,

    -- * ReviewDetails
    ReviewDetails (..),
    newReviewDetails,
    reviewDetails_status,
    reviewDetails_caseId,

    -- * SendQuota
    SendQuota (..),
    newSendQuota,
    sendQuota_max24HourSend,
    sendQuota_sentLast24Hours,
    sendQuota_maxSendRate,

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
    template_templateData,
    template_templateArn,

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
    volumeStatistics_projectedInbox,
    volumeStatistics_projectedSpam,
    volumeStatistics_spamRawCount,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SESv2.Types.AccountDetails
import Network.AWS.SESv2.Types.BehaviorOnMxFailure
import Network.AWS.SESv2.Types.BlacklistEntry
import Network.AWS.SESv2.Types.Body
import Network.AWS.SESv2.Types.BulkEmailContent
import Network.AWS.SESv2.Types.BulkEmailEntry
import Network.AWS.SESv2.Types.BulkEmailEntryResult
import Network.AWS.SESv2.Types.BulkEmailStatus
import Network.AWS.SESv2.Types.CloudWatchDestination
import Network.AWS.SESv2.Types.CloudWatchDimensionConfiguration
import Network.AWS.SESv2.Types.Contact
import Network.AWS.SESv2.Types.ContactLanguage
import Network.AWS.SESv2.Types.ContactList
import Network.AWS.SESv2.Types.ContactListDestination
import Network.AWS.SESv2.Types.ContactListImportAction
import Network.AWS.SESv2.Types.Content
import Network.AWS.SESv2.Types.CustomVerificationEmailTemplateMetadata
import Network.AWS.SESv2.Types.DailyVolume
import Network.AWS.SESv2.Types.DataFormat
import Network.AWS.SESv2.Types.DedicatedIp
import Network.AWS.SESv2.Types.DeliverabilityDashboardAccountStatus
import Network.AWS.SESv2.Types.DeliverabilityTestReport
import Network.AWS.SESv2.Types.DeliverabilityTestStatus
import Network.AWS.SESv2.Types.DeliveryOptions
import Network.AWS.SESv2.Types.Destination
import Network.AWS.SESv2.Types.DimensionValueSource
import Network.AWS.SESv2.Types.DkimAttributes
import Network.AWS.SESv2.Types.DkimSigningAttributes
import Network.AWS.SESv2.Types.DkimSigningAttributesOrigin
import Network.AWS.SESv2.Types.DkimStatus
import Network.AWS.SESv2.Types.DomainDeliverabilityCampaign
import Network.AWS.SESv2.Types.DomainDeliverabilityTrackingOption
import Network.AWS.SESv2.Types.DomainIspPlacement
import Network.AWS.SESv2.Types.EmailContent
import Network.AWS.SESv2.Types.EmailTemplateContent
import Network.AWS.SESv2.Types.EmailTemplateMetadata
import Network.AWS.SESv2.Types.EventDestination
import Network.AWS.SESv2.Types.EventDestinationDefinition
import Network.AWS.SESv2.Types.EventType
import Network.AWS.SESv2.Types.FailureInfo
import Network.AWS.SESv2.Types.IdentityInfo
import Network.AWS.SESv2.Types.IdentityType
import Network.AWS.SESv2.Types.ImportDataSource
import Network.AWS.SESv2.Types.ImportDestination
import Network.AWS.SESv2.Types.ImportDestinationType
import Network.AWS.SESv2.Types.ImportJobSummary
import Network.AWS.SESv2.Types.InboxPlacementTrackingOption
import Network.AWS.SESv2.Types.IspPlacement
import Network.AWS.SESv2.Types.JobStatus
import Network.AWS.SESv2.Types.KinesisFirehoseDestination
import Network.AWS.SESv2.Types.ListContactsFilter
import Network.AWS.SESv2.Types.ListManagementOptions
import Network.AWS.SESv2.Types.MailFromAttributes
import Network.AWS.SESv2.Types.MailFromDomainStatus
import Network.AWS.SESv2.Types.MailType
import Network.AWS.SESv2.Types.Message
import Network.AWS.SESv2.Types.MessageTag
import Network.AWS.SESv2.Types.OverallVolume
import Network.AWS.SESv2.Types.PinpointDestination
import Network.AWS.SESv2.Types.PlacementStatistics
import Network.AWS.SESv2.Types.RawMessage
import Network.AWS.SESv2.Types.ReplacementEmailContent
import Network.AWS.SESv2.Types.ReplacementTemplate
import Network.AWS.SESv2.Types.ReputationOptions
import Network.AWS.SESv2.Types.ReviewDetails
import Network.AWS.SESv2.Types.ReviewStatus
import Network.AWS.SESv2.Types.SendQuota
import Network.AWS.SESv2.Types.SendingOptions
import Network.AWS.SESv2.Types.SnsDestination
import Network.AWS.SESv2.Types.SubscriptionStatus
import Network.AWS.SESv2.Types.SuppressedDestination
import Network.AWS.SESv2.Types.SuppressedDestinationAttributes
import Network.AWS.SESv2.Types.SuppressedDestinationSummary
import Network.AWS.SESv2.Types.SuppressionAttributes
import Network.AWS.SESv2.Types.SuppressionListDestination
import Network.AWS.SESv2.Types.SuppressionListImportAction
import Network.AWS.SESv2.Types.SuppressionListReason
import Network.AWS.SESv2.Types.SuppressionOptions
import Network.AWS.SESv2.Types.Tag
import Network.AWS.SESv2.Types.Template
import Network.AWS.SESv2.Types.TlsPolicy
import Network.AWS.SESv2.Types.Topic
import Network.AWS.SESv2.Types.TopicFilter
import Network.AWS.SESv2.Types.TopicPreference
import Network.AWS.SESv2.Types.TrackingOptions
import Network.AWS.SESv2.Types.VolumeStatistics
import Network.AWS.SESv2.Types.WarmupStatus
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2019-09-27@ of the Amazon Simple Email Service SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "SESv2",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "email",
      Core._serviceSigningName = "ses",
      Core._serviceVersion = "2019-09-27",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "SESv2",
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
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
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
      | Prelude.otherwise = Prelude.Nothing

-- | The resource you attempted to access doesn\'t exist.
_NotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError
    defaultService
    "NotFoundException"
    Prelude.. Core.hasStatus 404

-- | The input you provided is invalid.
_BadRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError
    defaultService
    "BadRequestException"
    Prelude.. Core.hasStatus 400

-- | The message can\'t be sent because the sending domain isn\'t verified.
_MailFromDomainNotVerifiedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MailFromDomainNotVerifiedException =
  Core._MatchServiceError
    defaultService
    "MailFromDomainNotVerifiedException"
    Prelude.. Core.hasStatus 400

-- | The message can\'t be sent because the account\'s ability to send email
-- is currently paused.
_SendingPausedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SendingPausedException =
  Core._MatchServiceError
    defaultService
    "SendingPausedException"
    Prelude.. Core.hasStatus 400

-- | The message can\'t be sent because it contains invalid content.
_MessageRejected :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MessageRejected =
  Core._MatchServiceError
    defaultService
    "MessageRejected"
    Prelude.. Core.hasStatus 400

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

-- | There are too many instances of the specified resource type.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"
    Prelude.. Core.hasStatus 400

-- | If there is already an ongoing account details update under review.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | The resource specified in your request already exists.
_AlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "AlreadyExistsException"
    Prelude.. Core.hasStatus 400

-- | The message can\'t be sent because the account\'s ability to send email
-- has been permanently restricted.
_AccountSuspendedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccountSuspendedException =
  Core._MatchServiceError
    defaultService
    "AccountSuspendedException"
    Prelude.. Core.hasStatus 400

-- | Too many requests have been made to the operation.
_TooManyRequestsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyRequestsException =
  Core._MatchServiceError
    defaultService
    "TooManyRequestsException"
    Prelude.. Core.hasStatus 429
