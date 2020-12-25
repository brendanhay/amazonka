{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon Pinpoint makes it easy to run targeted campaigns to drive user engagement in mobile apps. Amazon Pinpoint helps you understand user behavior, define which users to target, determine which messages to send, schedule the best time to deliver the messages, and then track the results of your campaign.
--
-- Targeted push notifications based on app usage trends and user behavior have become a popular approach for mobile app user engagement because response rates are often several times higher than tradition email marketing campaigns. By using targeted push notifications, you can increase message relevance and effectiveness, measure engagement, and continually improve your campaigns.
-- Getting started with Amazon Pinpoint is easy. First, AWS Mobile Hub guides you through the process to integrate the AWS Mobile SDK with your app. Next, you define your target segments, campaign message, and specify the delivery schedule. Once your campaign is running, Pinpoint provides metrics so you can run analytics and track the impact of your campaign.
-- With Amazon Pinpoint, there are no upfront setup costs, and no fixed monthly cost. You only pay for the number of users your campaign targets, the messages you send, and the events you collect, so you can start small and scale as your application grows.
module Network.AWS.Pinpoint
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    -- $errors

    -- ** PayloadTooLargeException
    _PayloadTooLargeException,

    -- ** ConflictException
    _ConflictException,

    -- ** ForbiddenException
    _ForbiddenException,

    -- ** NotFoundException
    _NotFoundException,

    -- ** TooManyRequestsException
    _TooManyRequestsException,

    -- ** InternalServerErrorException
    _InternalServerErrorException,

    -- ** MethodNotAllowedException
    _MethodNotAllowedException,

    -- ** BadRequestException
    _BadRequestException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** GetGcmChannel
    module Network.AWS.Pinpoint.GetGcmChannel,

    -- ** GetSegmentImportJobs
    module Network.AWS.Pinpoint.GetSegmentImportJobs,

    -- ** SendMessages
    module Network.AWS.Pinpoint.SendMessages,

    -- ** GetImportJob
    module Network.AWS.Pinpoint.GetImportJob,

    -- ** DeleteSmsTemplate
    module Network.AWS.Pinpoint.DeleteSmsTemplate,

    -- ** UpdateSmsTemplate
    module Network.AWS.Pinpoint.UpdateSmsTemplate,

    -- ** GetApnsVoipSandboxChannel
    module Network.AWS.Pinpoint.GetApnsVoipSandboxChannel,

    -- ** GetSegmentVersions
    module Network.AWS.Pinpoint.GetSegmentVersions,

    -- ** DeleteCampaign
    module Network.AWS.Pinpoint.DeleteCampaign,

    -- ** UpdateCampaign
    module Network.AWS.Pinpoint.UpdateCampaign,

    -- ** GetSegmentVersion
    module Network.AWS.Pinpoint.GetSegmentVersion,

    -- ** DeletePushTemplate
    module Network.AWS.Pinpoint.DeletePushTemplate,

    -- ** UpdatePushTemplate
    module Network.AWS.Pinpoint.UpdatePushTemplate,

    -- ** CreateExportJob
    module Network.AWS.Pinpoint.CreateExportJob,

    -- ** CreateSegment
    module Network.AWS.Pinpoint.CreateSegment,

    -- ** CreateRecommenderConfiguration
    module Network.AWS.Pinpoint.CreateRecommenderConfiguration,

    -- ** CreateVoiceTemplate
    module Network.AWS.Pinpoint.CreateVoiceTemplate,

    -- ** UpdateAdmChannel
    module Network.AWS.Pinpoint.UpdateAdmChannel,

    -- ** DeleteAdmChannel
    module Network.AWS.Pinpoint.DeleteAdmChannel,

    -- ** DeleteRecommenderConfiguration
    module Network.AWS.Pinpoint.DeleteRecommenderConfiguration,

    -- ** UpdateRecommenderConfiguration
    module Network.AWS.Pinpoint.UpdateRecommenderConfiguration,

    -- ** CreatePushTemplate
    module Network.AWS.Pinpoint.CreatePushTemplate,

    -- ** DeleteEndpoint
    module Network.AWS.Pinpoint.DeleteEndpoint,

    -- ** UpdateEndpoint
    module Network.AWS.Pinpoint.UpdateEndpoint,

    -- ** ListTagsForResource
    module Network.AWS.Pinpoint.ListTagsForResource,

    -- ** CreateCampaign
    module Network.AWS.Pinpoint.CreateCampaign,

    -- ** GetEmailTemplate
    module Network.AWS.Pinpoint.GetEmailTemplate,

    -- ** GetExportJob
    module Network.AWS.Pinpoint.GetExportJob,

    -- ** GetEndpoint
    module Network.AWS.Pinpoint.GetEndpoint,

    -- ** GetSegment
    module Network.AWS.Pinpoint.GetSegment,

    -- ** GetRecommenderConfiguration
    module Network.AWS.Pinpoint.GetRecommenderConfiguration,

    -- ** UpdateEndpointsBatch
    module Network.AWS.Pinpoint.UpdateEndpointsBatch,

    -- ** GetAdmChannel
    module Network.AWS.Pinpoint.GetAdmChannel,

    -- ** GetCampaign
    module Network.AWS.Pinpoint.GetCampaign,

    -- ** GetVoiceTemplate
    module Network.AWS.Pinpoint.GetVoiceTemplate,

    -- ** GetPushTemplate
    module Network.AWS.Pinpoint.GetPushTemplate,

    -- ** DeleteUserEndpoints
    module Network.AWS.Pinpoint.DeleteUserEndpoints,

    -- ** CreateEmailTemplate
    module Network.AWS.Pinpoint.CreateEmailTemplate,

    -- ** DeleteApp
    module Network.AWS.Pinpoint.DeleteApp,

    -- ** UpdateApnsVoipSandboxChannel
    module Network.AWS.Pinpoint.UpdateApnsVoipSandboxChannel,

    -- ** DeleteApnsVoipSandboxChannel
    module Network.AWS.Pinpoint.DeleteApnsVoipSandboxChannel,

    -- ** UpdateGcmChannel
    module Network.AWS.Pinpoint.UpdateGcmChannel,

    -- ** DeleteGcmChannel
    module Network.AWS.Pinpoint.DeleteGcmChannel,

    -- ** GetCampaignActivities
    module Network.AWS.Pinpoint.GetCampaignActivities,

    -- ** GetJourneyExecutionMetrics
    module Network.AWS.Pinpoint.GetJourneyExecutionMetrics,

    -- ** UpdateJourneyState
    module Network.AWS.Pinpoint.UpdateJourneyState,

    -- ** GetEventStream
    module Network.AWS.Pinpoint.GetEventStream,

    -- ** GetChannels
    module Network.AWS.Pinpoint.GetChannels,

    -- ** GetJourney
    module Network.AWS.Pinpoint.GetJourney,

    -- ** DeleteEmailChannel
    module Network.AWS.Pinpoint.DeleteEmailChannel,

    -- ** UpdateEmailChannel
    module Network.AWS.Pinpoint.UpdateEmailChannel,

    -- ** GetBaiduChannel
    module Network.AWS.Pinpoint.GetBaiduChannel,

    -- ** DeleteApnsChannel
    module Network.AWS.Pinpoint.DeleteApnsChannel,

    -- ** UpdateApnsChannel
    module Network.AWS.Pinpoint.UpdateApnsChannel,

    -- ** RemoveAttributes
    module Network.AWS.Pinpoint.RemoveAttributes,

    -- ** ListTemplates
    module Network.AWS.Pinpoint.ListTemplates,

    -- ** PutEventStream
    module Network.AWS.Pinpoint.PutEventStream,

    -- ** DeleteEventStream
    module Network.AWS.Pinpoint.DeleteEventStream,

    -- ** GetCampaignVersions
    module Network.AWS.Pinpoint.GetCampaignVersions,

    -- ** DeleteJourney
    module Network.AWS.Pinpoint.DeleteJourney,

    -- ** UpdateJourney
    module Network.AWS.Pinpoint.UpdateJourney,

    -- ** GetCampaignDateRangeKpi
    module Network.AWS.Pinpoint.GetCampaignDateRangeKpi,

    -- ** GetApnsChannel
    module Network.AWS.Pinpoint.GetApnsChannel,

    -- ** UpdateVoiceChannel
    module Network.AWS.Pinpoint.UpdateVoiceChannel,

    -- ** DeleteVoiceChannel
    module Network.AWS.Pinpoint.DeleteVoiceChannel,

    -- ** GetApps
    module Network.AWS.Pinpoint.GetApps,

    -- ** GetApnsSandboxChannel
    module Network.AWS.Pinpoint.GetApnsSandboxChannel,

    -- ** CreateJourney
    module Network.AWS.Pinpoint.CreateJourney,

    -- ** GetUserEndpoints
    module Network.AWS.Pinpoint.GetUserEndpoints,

    -- ** DeleteVoiceTemplate
    module Network.AWS.Pinpoint.DeleteVoiceTemplate,

    -- ** UpdateVoiceTemplate
    module Network.AWS.Pinpoint.UpdateVoiceTemplate,

    -- ** GetImportJobs
    module Network.AWS.Pinpoint.GetImportJobs,

    -- ** GetJourneyDateRangeKpi
    module Network.AWS.Pinpoint.GetJourneyDateRangeKpi,

    -- ** UpdateTemplateActiveVersion
    module Network.AWS.Pinpoint.UpdateTemplateActiveVersion,

    -- ** DeleteSmsChannel
    module Network.AWS.Pinpoint.DeleteSmsChannel,

    -- ** UpdateSmsChannel
    module Network.AWS.Pinpoint.UpdateSmsChannel,

    -- ** GetApp
    module Network.AWS.Pinpoint.GetApp,

    -- ** GetCampaignVersion
    module Network.AWS.Pinpoint.GetCampaignVersion,

    -- ** DeleteSegment
    module Network.AWS.Pinpoint.DeleteSegment,

    -- ** UpdateSegment
    module Network.AWS.Pinpoint.UpdateSegment,

    -- ** GetApplicationDateRangeKpi
    module Network.AWS.Pinpoint.GetApplicationDateRangeKpi,

    -- ** CreateApp
    module Network.AWS.Pinpoint.CreateApp,

    -- ** GetSegmentExportJobs
    module Network.AWS.Pinpoint.GetSegmentExportJobs,

    -- ** DeleteEmailTemplate
    module Network.AWS.Pinpoint.DeleteEmailTemplate,

    -- ** UpdateEmailTemplate
    module Network.AWS.Pinpoint.UpdateEmailTemplate,

    -- ** GetSmsChannel
    module Network.AWS.Pinpoint.GetSmsChannel,

    -- ** TagResource
    module Network.AWS.Pinpoint.TagResource,

    -- ** DeleteApnsSandboxChannel
    module Network.AWS.Pinpoint.DeleteApnsSandboxChannel,

    -- ** UpdateApnsSandboxChannel
    module Network.AWS.Pinpoint.UpdateApnsSandboxChannel,

    -- ** GetCampaigns
    module Network.AWS.Pinpoint.GetCampaigns,

    -- ** GetVoiceChannel
    module Network.AWS.Pinpoint.GetVoiceChannel,

    -- ** UntagResource
    module Network.AWS.Pinpoint.UntagResource,

    -- ** ListTemplateVersions
    module Network.AWS.Pinpoint.ListTemplateVersions,

    -- ** GetSmsTemplate
    module Network.AWS.Pinpoint.GetSmsTemplate,

    -- ** PutEvents
    module Network.AWS.Pinpoint.PutEvents,

    -- ** UpdateApplicationSettings
    module Network.AWS.Pinpoint.UpdateApplicationSettings,

    -- ** GetJourneyExecutionActivityMetrics
    module Network.AWS.Pinpoint.GetJourneyExecutionActivityMetrics,

    -- ** GetSegments
    module Network.AWS.Pinpoint.GetSegments,

    -- ** GetExportJobs
    module Network.AWS.Pinpoint.GetExportJobs,

    -- ** CreateImportJob
    module Network.AWS.Pinpoint.CreateImportJob,

    -- ** GetRecommenderConfigurations
    module Network.AWS.Pinpoint.GetRecommenderConfigurations,

    -- ** DeleteApnsVoipChannel
    module Network.AWS.Pinpoint.DeleteApnsVoipChannel,

    -- ** UpdateApnsVoipChannel
    module Network.AWS.Pinpoint.UpdateApnsVoipChannel,

    -- ** SendUsersMessages
    module Network.AWS.Pinpoint.SendUsersMessages,

    -- ** GetApplicationSettings
    module Network.AWS.Pinpoint.GetApplicationSettings,

    -- ** DeleteBaiduChannel
    module Network.AWS.Pinpoint.DeleteBaiduChannel,

    -- ** UpdateBaiduChannel
    module Network.AWS.Pinpoint.UpdateBaiduChannel,

    -- ** CreateSmsTemplate
    module Network.AWS.Pinpoint.CreateSmsTemplate,

    -- ** PhoneNumberValidate
    module Network.AWS.Pinpoint.PhoneNumberValidate,

    -- ** ListJourneys
    module Network.AWS.Pinpoint.ListJourneys,

    -- ** GetApnsVoipChannel
    module Network.AWS.Pinpoint.GetApnsVoipChannel,

    -- ** GetEmailChannel
    module Network.AWS.Pinpoint.GetEmailChannel,

    -- * Types

    -- ** SegmentBehaviors
    SegmentBehaviors (..),
    mkSegmentBehaviors,
    sbRecency,

    -- ** AttributeDimension
    AttributeDimension (..),
    mkAttributeDimension,
    adValues,
    adAttributeType,

    -- ** APNSVoipChannelResponse
    APNSVoipChannelResponse (..),
    mkAPNSVoipChannelResponse,
    apnsvcrPlatform,
    apnsvcrApplicationId,
    apnsvcrCreationDate,
    apnsvcrDefaultAuthenticationMethod,
    apnsvcrEnabled,
    apnsvcrHasCredential,
    apnsvcrHasTokenKey,
    apnsvcrId,
    apnsvcrIsArchived,
    apnsvcrLastModifiedBy,
    apnsvcrLastModifiedDate,
    apnsvcrVersion,

    -- ** Include
    Include (..),

    -- ** VoiceChannelRequest
    VoiceChannelRequest (..),
    mkVoiceChannelRequest,
    vcrEnabled,

    -- ** SendUsersMessageRequest
    SendUsersMessageRequest (..),
    mkSendUsersMessageRequest,
    sumrMessageConfiguration,
    sumrUsers,
    sumrContext,
    sumrTemplateConfiguration,
    sumrTraceId,

    -- ** SegmentGroup
    SegmentGroup (..),
    mkSegmentGroup,
    sgDimensions,
    sgSourceSegments,
    sgSourceType,
    sgType,

    -- ** EmailChannelResponse
    EmailChannelResponse (..),
    mkEmailChannelResponse,
    ecrPlatform,
    ecrApplicationId,
    ecrConfigurationSet,
    ecrCreationDate,
    ecrEnabled,
    ecrFromAddress,
    ecrHasCredential,
    ecrId,
    ecrIdentity,
    ecrIsArchived,
    ecrLastModifiedBy,
    ecrLastModifiedDate,
    ecrMessagesPerSecond,
    ecrRoleArn,
    ecrVersion,

    -- ** SMSTemplateRequest
    SMSTemplateRequest (..),
    mkSMSTemplateRequest,
    smstrBody,
    smstrDefaultSubstitutions,
    smstrRecommenderId,
    smstrTemplateDescription,
    smstrTags,

    -- ** Frequency
    Frequency (..),

    -- ** Event
    Event (..),
    mkEvent,
    eEventType,
    eTimestamp,
    eAppPackageName,
    eAppTitle,
    eAppVersionCode,
    eAttributes,
    eClientSdkVersion,
    eMetrics,
    eSdkName,
    eSession,

    -- ** CustomDeliveryConfiguration
    CustomDeliveryConfiguration (..),
    mkCustomDeliveryConfiguration,
    cdcDeliveryUri,
    cdcEndpointTypes,

    -- ** CreateApplicationRequest
    CreateApplicationRequest (..),
    mkCreateApplicationRequest,
    carName,
    carTags,

    -- ** APNSPushNotificationTemplate
    APNSPushNotificationTemplate (..),
    mkAPNSPushNotificationTemplate,
    apnspntAction,
    apnspntBody,
    apnspntMediaUrl,
    apnspntRawContent,
    apnspntSound,
    apnspntTitle,
    apnspntUrl,

    -- ** EndpointSendConfiguration
    EndpointSendConfiguration (..),
    mkEndpointSendConfiguration,
    escBodyOverride,
    escContext,
    escRawContent,
    escSubstitutions,
    escTitleOverride,

    -- ** State
    State (..),

    -- ** APNSMessage
    APNSMessage (..),
    mkAPNSMessage,
    apnsmAPNSPushType,
    apnsmAction,
    apnsmBadge,
    apnsmBody,
    apnsmCategory,
    apnsmCollapseId,
    apnsmData,
    apnsmMediaUrl,
    apnsmPreferredAuthenticationMethod,
    apnsmPriority,
    apnsmRawContent,
    apnsmSilentPush,
    apnsmSound,
    apnsmSubstitutions,
    apnsmThreadId,
    apnsmTimeToLive,
    apnsmTitle,
    apnsmUrl,

    -- ** TemplateActiveVersionRequest
    TemplateActiveVersionRequest (..),
    mkTemplateActiveVersionRequest,
    tavrVersion,

    -- ** VoiceTemplateRequest
    VoiceTemplateRequest (..),
    mkVoiceTemplateRequest,
    vtrBody,
    vtrDefaultSubstitutions,
    vtrLanguageCode,
    vtrTemplateDescription,
    vtrVoiceId,
    vtrTags,

    -- ** GCMMessage
    GCMMessage (..),
    mkGCMMessage,
    gcmmAction,
    gcmmBody,
    gcmmCollapseKey,
    gcmmData,
    gcmmIconReference,
    gcmmImageIconUrl,
    gcmmImageUrl,
    gcmmPriority,
    gcmmRawContent,
    gcmmRestrictedPackageName,
    gcmmSilentPush,
    gcmmSmallImageIconUrl,
    gcmmSound,
    gcmmSubstitutions,
    gcmmTimeToLive,
    gcmmTitle,
    gcmmUrl,

    -- ** ImportJobsResponse
    ImportJobsResponse (..),
    mkImportJobsResponse,
    ijrItem,
    ijrNextToken,

    -- ** SetDimension
    SetDimension (..),
    mkSetDimension,
    sdValues,
    sdDimensionType,

    -- ** JourneyDateRangeKpiResponse
    JourneyDateRangeKpiResponse (..),
    mkJourneyDateRangeKpiResponse,
    jdrkrKpiResult,
    jdrkrKpiName,
    jdrkrJourneyId,
    jdrkrEndTime,
    jdrkrStartTime,
    jdrkrApplicationId,
    jdrkrNextToken,

    -- ** PublicEndpoint
    PublicEndpoint (..),
    mkPublicEndpoint,
    peAddress,
    peAttributes,
    peChannelType,
    peDemographic,
    peEffectiveDate,
    peEndpointStatus,
    peLocation,
    peMetrics,
    peOptOut,
    peRequestId,
    peUser,

    -- ** CampaignEventFilter
    CampaignEventFilter (..),
    mkCampaignEventFilter,
    cefFilterType,
    cefDimensions,

    -- ** EmailMessageActivity
    EmailMessageActivity (..),
    mkEmailMessageActivity,
    emaMessageConfig,
    emaNextActivity,
    emaTemplateName,
    emaTemplateVersion,

    -- ** AttributesResource
    AttributesResource (..),
    mkAttributesResource,
    arAttributeType,
    arApplicationId,
    arAttributes,

    -- ** EmailTemplateRequest
    EmailTemplateRequest (..),
    mkEmailTemplateRequest,
    etrDefaultSubstitutions,
    etrHtmlPart,
    etrRecommenderId,
    etrSubject,
    etrTemplateDescription,
    etrTextPart,
    etrTags,

    -- ** SMSChannelResponse
    SMSChannelResponse (..),
    mkSMSChannelResponse,
    smscrPlatform,
    smscrApplicationId,
    smscrCreationDate,
    smscrEnabled,
    smscrHasCredential,
    smscrId,
    smscrIsArchived,
    smscrLastModifiedBy,
    smscrLastModifiedDate,
    smscrPromotionalMessagesPerSecond,
    smscrSenderId,
    smscrShortCode,
    smscrTransactionalMessagesPerSecond,
    smscrVersion,

    -- ** EndpointItemResponse
    EndpointItemResponse (..),
    mkEndpointItemResponse,
    eirMessage,
    eirStatusCode,

    -- ** JourneySchedule
    JourneySchedule (..),
    mkJourneySchedule,
    jsEndTime,
    jsStartTime,
    jsTimezone,

    -- ** SourceType
    SourceType (..),

    -- ** Schedule
    Schedule (..),
    mkSchedule,
    sStartTime,
    sEndTime,
    sEventFilter,
    sFrequency,
    sIsLocalTime,
    sQuietTime,
    sTimezone,

    -- ** SimpleCondition
    SimpleCondition (..),
    mkSimpleCondition,
    scEventCondition,
    scSegmentCondition,
    scSegmentDimensions,

    -- ** RandomSplitEntry
    RandomSplitEntry (..),
    mkRandomSplitEntry,
    rseNextActivity,
    rsePercentage,

    -- ** AndroidPushNotificationTemplate
    AndroidPushNotificationTemplate (..),
    mkAndroidPushNotificationTemplate,
    apntAction,
    apntBody,
    apntImageIconUrl,
    apntImageUrl,
    apntRawContent,
    apntSmallImageIconUrl,
    apntSound,
    apntTitle,
    apntUrl,

    -- ** TemplateType
    TemplateType (..),

    -- ** SegmentDimensions
    SegmentDimensions (..),
    mkSegmentDimensions,
    sdAttributes,
    sdBehavior,
    sdDemographic,
    sdLocation,
    sdMetrics,
    sdUserAttributes,

    -- ** ApplicationDateRangeKpiResponse
    ApplicationDateRangeKpiResponse (..),
    mkApplicationDateRangeKpiResponse,
    adrkrKpiResult,
    adrkrKpiName,
    adrkrEndTime,
    adrkrStartTime,
    adrkrApplicationId,
    adrkrNextToken,

    -- ** JourneyCustomMessage
    JourneyCustomMessage (..),
    mkJourneyCustomMessage,
    jcmData,

    -- ** DeliveryStatus
    DeliveryStatus (..),

    -- ** PushMessageActivity
    PushMessageActivity (..),
    mkPushMessageActivity,
    pmaMessageConfig,
    pmaNextActivity,
    pmaTemplateName,
    pmaTemplateVersion,

    -- ** SegmentReference
    SegmentReference (..),
    mkSegmentReference,
    sId,
    sVersion,

    -- ** CampaignSmsMessage
    CampaignSmsMessage (..),
    mkCampaignSmsMessage,
    csmBody,
    csmMessageType,
    csmSenderId,

    -- ** EventStream
    EventStream (..),
    mkEventStream,
    esApplicationId,
    esRoleArn,
    esDestinationStreamArn,
    esExternalId,
    esLastModifiedDate,
    esLastUpdatedBy,

    -- ** DefaultMessage
    DefaultMessage (..),
    mkDefaultMessage,
    dmBody,
    dmSubstitutions,

    -- ** ImportJobResource
    ImportJobResource (..),
    mkImportJobResource,
    ijrFormat,
    ijrS3Url,
    ijrRoleArn,
    ijrDefineSegment,
    ijrExternalId,
    ijrRegisterEndpoints,
    ijrSegmentId,
    ijrSegmentName,

    -- ** Operator
    Operator (..),

    -- ** GPSPointDimension
    GPSPointDimension (..),
    mkGPSPointDimension,
    gpspdCoordinates,
    gpspdRangeInKilometers,

    -- ** TemplateConfiguration
    TemplateConfiguration (..),
    mkTemplateConfiguration,
    tcEmailTemplate,
    tcPushTemplate,
    tcSMSTemplate,
    tcVoiceTemplate,

    -- ** ChannelResponse
    ChannelResponse (..),
    mkChannelResponse,
    cApplicationId,
    cCreationDate,
    cEnabled,
    cHasCredential,
    cId,
    cIsArchived,
    cLastModifiedBy,
    cLastModifiedDate,
    cVersion,

    -- ** TemplatesResponse
    TemplatesResponse (..),
    mkTemplatesResponse,
    trItem,
    trNextToken,

    -- ** FilterType
    FilterType (..),

    -- ** ActivitiesResponse
    ActivitiesResponse (..),
    mkActivitiesResponse,
    arItem,
    arNextToken,

    -- ** CampaignLimits
    CampaignLimits (..),
    mkCampaignLimits,
    clDaily,
    clMaximumDuration,
    clMessagesPerSecond,
    clTotal,

    -- ** RawEmail
    RawEmail (..),
    mkRawEmail,
    reData,

    -- ** ADMMessage
    ADMMessage (..),
    mkADMMessage,
    admmAction,
    admmBody,
    admmConsolidationKey,
    admmData,
    admmExpiresAfter,
    admmIconReference,
    admmImageIconUrl,
    admmImageUrl,
    admmMD5,
    admmRawContent,
    admmSilentPush,
    admmSmallImageIconUrl,
    admmSound,
    admmSubstitutions,
    admmTitle,
    admmUrl,

    -- ** WriteApplicationSettingsRequest
    WriteApplicationSettingsRequest (..),
    mkWriteApplicationSettingsRequest,
    wasrCampaignHook,
    wasrCloudWatchMetricsEnabled,
    wasrEventTaggingEnabled,
    wasrLimits,
    wasrQuietTime,

    -- ** GCMChannelRequest
    GCMChannelRequest (..),
    mkGCMChannelRequest,
    gApiKey,
    gEnabled,

    -- ** DefinitionFormat
    DefinitionFormat (..),

    -- ** SegmentType
    SegmentType (..),

    -- ** JourneyResponse
    JourneyResponse (..),
    mkJourneyResponse,
    jrName,
    jrId,
    jrApplicationId,
    jrActivities,
    jrCreationDate,
    jrLastModifiedDate,
    jrLimits,
    jrLocalTime,
    jrQuietTime,
    jrRefreshFrequency,
    jrSchedule,
    jrStartActivity,
    jrStartCondition,
    jrState,
    jrTags,

    -- ** ChannelsResponse
    ChannelsResponse (..),
    mkChannelsResponse,
    crChannels,

    -- ** ResultRow
    ResultRow (..),
    mkResultRow,
    rrGroupedBys,
    rrValues,

    -- ** EventsBatch
    EventsBatch (..),
    mkEventsBatch,
    ebEndpoint,
    ebEvents,

    -- ** SMSMessageActivity
    SMSMessageActivity (..),
    mkSMSMessageActivity,
    smsmaMessageConfig,
    smsmaNextActivity,
    smsmaTemplateName,
    smsmaTemplateVersion,

    -- ** APNSVoipSandboxChannelRequest
    APNSVoipSandboxChannelRequest (..),
    mkAPNSVoipSandboxChannelRequest,
    aBundleId,
    aCertificate,
    aDefaultAuthenticationMethod,
    aEnabled,
    aPrivateKey,
    aTeamId,
    aTokenKey,
    aTokenKeyId,

    -- ** MultiConditionalBranch
    MultiConditionalBranch (..),
    mkMultiConditionalBranch,
    mcbCondition,
    mcbNextActivity,

    -- ** ImportJobRequest
    ImportJobRequest (..),
    mkImportJobRequest,
    iFormat,
    iS3Url,
    iRoleArn,
    iDefineSegment,
    iExternalId,
    iRegisterEndpoints,
    iSegmentId,
    iSegmentName,

    -- ** MessageType
    MessageType (..),

    -- ** BaiduChannelResponse
    BaiduChannelResponse (..),
    mkBaiduChannelResponse,
    bcrCredential,
    bcrPlatform,
    bcrApplicationId,
    bcrCreationDate,
    bcrEnabled,
    bcrHasCredential,
    bcrId,
    bcrIsArchived,
    bcrLastModifiedBy,
    bcrLastModifiedDate,
    bcrVersion,

    -- ** Mode
    Mode (..),

    -- ** JourneyLimits
    JourneyLimits (..),
    mkJourneyLimits,
    jlDailyCap,
    jlEndpointReentryCap,
    jlMessagesPerSecond,

    -- ** NumberValidateResponse
    NumberValidateResponse (..),
    mkNumberValidateResponse,
    nvrCarrier,
    nvrCity,
    nvrCleansedPhoneNumberE164,
    nvrCleansedPhoneNumberNational,
    nvrCountry,
    nvrCountryCodeIso2,
    nvrCountryCodeNumeric,
    nvrCounty,
    nvrOriginalCountryCodeIso2,
    nvrOriginalPhoneNumber,
    nvrPhoneType,
    nvrPhoneTypeCode,
    nvrTimezone,
    nvrZipCode,

    -- ** ActivityResponse
    ActivityResponse (..),
    mkActivityResponse,
    aCampaignId,
    aId,
    aApplicationId,
    aEnd,
    aResult,
    aScheduledStart,
    aStart,
    aState,
    aSuccessfulEndpointCount,
    aTimezonesCompletedCount,
    aTimezonesTotalCount,
    aTotalEndpointCount,
    aTreatmentId,

    -- ** Action
    Action (..),

    -- ** EndpointTypesElement
    EndpointTypesElement (..),

    -- ** JourneyExecutionMetricsResponse
    JourneyExecutionMetricsResponse (..),
    mkJourneyExecutionMetricsResponse,
    jemrMetrics,
    jemrJourneyId,
    jemrLastEvaluatedTime,
    jemrApplicationId,

    -- ** EndpointLocation
    EndpointLocation (..),
    mkEndpointLocation,
    elCity,
    elCountry,
    elLatitude,
    elLongitude,
    elPostalCode,
    elRegion,

    -- ** SMSMessage
    SMSMessage (..),
    mkSMSMessage,
    smsmBody,
    smsmKeyword,
    smsmMediaUrl,
    smsmMessageType,
    smsmOriginationNumber,
    smsmSenderId,
    smsmSubstitutions,

    -- ** WaitActivity
    WaitActivity (..),
    mkWaitActivity,
    waNextActivity,
    waWaitTime,

    -- ** ItemResponse
    ItemResponse (..),
    mkItemResponse,
    irEndpointItemResponse,
    irEventsItemResponse,

    -- ** SegmentLocation
    SegmentLocation (..),
    mkSegmentLocation,
    slCountry,
    slGPSPoint,

    -- ** APNSChannelRequest
    APNSChannelRequest (..),
    mkAPNSChannelRequest,
    apnscrfBundleId,
    apnscrfCertificate,
    apnscrfDefaultAuthenticationMethod,
    apnscrfEnabled,
    apnscrfPrivateKey,
    apnscrfTeamId,
    apnscrfTokenKey,
    apnscrfTokenKeyId,

    -- ** TagsModel
    TagsModel (..),
    mkTagsModel,
    tmTags,

    -- ** EventsRequest
    EventsRequest (..),
    mkEventsRequest,
    erBatchItem,

    -- ** JourneySMSMessage
    JourneySMSMessage (..),
    mkJourneySMSMessage,
    jsmsmMessageType,
    jsmsmSenderId,

    -- ** CampaignCustomMessage
    CampaignCustomMessage (..),
    mkCampaignCustomMessage,
    ccmData,

    -- ** CampaignEmailMessage
    CampaignEmailMessage (..),
    mkCampaignEmailMessage,
    cemBody,
    cemFromAddress,
    cemHtmlBody,
    cemTitle,

    -- ** ApplicationResponse
    ApplicationResponse (..),
    mkApplicationResponse,
    arId,
    arArn,
    arName,
    arTags,

    -- ** TemplateVersionResponse
    TemplateVersionResponse (..),
    mkTemplateVersionResponse,
    tvrLastModifiedDate,
    tvrCreationDate,
    tvrTemplateName,
    tvrTemplateType,
    tvrDefaultSubstitutions,
    tvrTemplateDescription,
    tvrVersion,

    -- ** QuietTime
    QuietTime (..),
    mkQuietTime,
    qtEnd,
    qtStart,

    -- ** APNSSandboxChannelResponse
    APNSSandboxChannelResponse (..),
    mkAPNSSandboxChannelResponse,
    apnsscrPlatform,
    apnsscrApplicationId,
    apnsscrCreationDate,
    apnsscrDefaultAuthenticationMethod,
    apnsscrEnabled,
    apnsscrHasCredential,
    apnsscrHasTokenKey,
    apnsscrId,
    apnsscrIsArchived,
    apnsscrLastModifiedBy,
    apnsscrLastModifiedDate,
    apnsscrVersion,

    -- ** ExportJobResource
    ExportJobResource (..),
    mkExportJobResource,
    ejrS3UrlPrefix,
    ejrRoleArn,
    ejrSegmentId,
    ejrSegmentVersion,

    -- ** APNSChannelResponse
    APNSChannelResponse (..),
    mkAPNSChannelResponse,
    apnscrPlatform,
    apnscrApplicationId,
    apnscrCreationDate,
    apnscrDefaultAuthenticationMethod,
    apnscrEnabled,
    apnscrHasCredential,
    apnscrHasTokenKey,
    apnscrId,
    apnscrIsArchived,
    apnscrLastModifiedBy,
    apnscrLastModifiedDate,
    apnscrVersion,

    -- ** MetricDimension
    MetricDimension (..),
    mkMetricDimension,
    mdComparisonOperator,
    mdValue,

    -- ** NumberValidateRequest
    NumberValidateRequest (..),
    mkNumberValidateRequest,
    nvrIsoCountryCode,
    nvrPhoneNumber,

    -- ** EventsResponse
    EventsResponse (..),
    mkEventsResponse,
    erResults,

    -- ** EventFilter
    EventFilter (..),
    mkEventFilter,
    efFilterType,
    efDimensions,

    -- ** WriteJourneyRequest
    WriteJourneyRequest (..),
    mkWriteJourneyRequest,
    wjrName,
    wjrActivities,
    wjrCreationDate,
    wjrLastModifiedDate,
    wjrLimits,
    wjrLocalTime,
    wjrQuietTime,
    wjrRefreshFrequency,
    wjrSchedule,
    wjrStartActivity,
    wjrStartCondition,
    wjrState,

    -- ** HoldoutActivity
    HoldoutActivity (..),
    mkHoldoutActivity,
    haPercentage,
    haNextActivity,

    -- ** SegmentImportResource
    SegmentImportResource (..),
    mkSegmentImportResource,
    sirFormat,
    sirS3Url,
    sirSize,
    sirExternalId,
    sirRoleArn,
    sirChannelCounts,

    -- ** RandomSplitActivity
    RandomSplitActivity (..),
    mkRandomSplitActivity,
    rsaBranches,

    -- ** AttributeType
    AttributeType (..),

    -- ** CampaignDateRangeKpiResponse
    CampaignDateRangeKpiResponse (..),
    mkCampaignDateRangeKpiResponse,
    cdrkrKpiResult,
    cdrkrKpiName,
    cdrkrEndTime,
    cdrkrCampaignId,
    cdrkrStartTime,
    cdrkrApplicationId,
    cdrkrNextToken,

    -- ** EmailMessage
    EmailMessage (..),
    mkEmailMessage,
    emBody,
    emFeedbackForwardingAddress,
    emFromAddress,
    emRawEmail,
    emReplyToAddresses,
    emSimpleEmail,
    emSubstitutions,

    -- ** UpdateAttributesRequest
    UpdateAttributesRequest (..),
    mkUpdateAttributesRequest,
    uarBlacklist,

    -- ** EventStartCondition
    EventStartCondition (..),
    mkEventStartCondition,
    escEventFilter,
    escSegmentId,

    -- ** EventCondition
    EventCondition (..),
    mkEventCondition,
    ecDimensions,
    ecMessageActivity,

    -- ** CreateTemplateMessageBody
    CreateTemplateMessageBody (..),
    mkCreateTemplateMessageBody,
    ctmbArn,
    ctmbMessage,
    ctmbRequestID,

    -- ** JourneyEmailMessage
    JourneyEmailMessage (..),
    mkJourneyEmailMessage,
    jemFromAddress,

    -- ** EventDimensions
    EventDimensions (..),
    mkEventDimensions,
    edAttributes,
    edEventType,
    edMetrics,

    -- ** SMSChannelRequest
    SMSChannelRequest (..),
    mkSMSChannelRequest,
    sEnabled,
    sSenderId,
    sShortCode,

    -- ** WriteSegmentRequest
    WriteSegmentRequest (..),
    mkWriteSegmentRequest,
    wsrDimensions,
    wsrName,
    wsrSegmentGroups,
    wsrTags,

    -- ** EmailTemplateResponse
    EmailTemplateResponse (..),
    mkEmailTemplateResponse,
    eLastModifiedDate,
    eCreationDate,
    eTemplateName,
    eTemplateType,
    eArn,
    eDefaultSubstitutions,
    eHtmlPart,
    eRecommenderId,
    eSubject,
    eTemplateDescription,
    eTextPart,
    eVersion,
    eTags,

    -- ** ADMChannelRequest
    ADMChannelRequest (..),
    mkADMChannelRequest,
    admcrfClientSecret,
    admcrfClientId,
    admcrfEnabled,

    -- ** ExportJobRequest
    ExportJobRequest (..),
    mkExportJobRequest,
    eS3UrlPrefix,
    eRoleArn,
    eSegmentId,
    eSegmentVersion,

    -- ** MessageResult
    MessageResult (..),
    mkMessageResult,
    mrDeliveryStatus,
    mrStatusCode,
    mrMessageId,
    mrStatusMessage,
    mrUpdatedToken,

    -- ** EventItemResponse
    EventItemResponse (..),
    mkEventItemResponse,
    eMessage,
    eStatusCode,

    -- ** EndpointRequest
    EndpointRequest (..),
    mkEndpointRequest,
    erfAddress,
    erfAttributes,
    erfChannelType,
    erfDemographic,
    erfEffectiveDate,
    erfEndpointStatus,
    erfLocation,
    erfMetrics,
    erfOptOut,
    erfRequestId,
    erfUser,

    -- ** ApplicationsResponse
    ApplicationsResponse (..),
    mkApplicationsResponse,
    aItem,
    aNextToken,

    -- ** TemplateVersionsResponse
    TemplateVersionsResponse (..),
    mkTemplateVersionsResponse,
    tvrItem,
    tvrMessage,
    tvrNextToken,
    tvrRequestID,

    -- ** MessageRequest
    MessageRequest (..),
    mkMessageRequest,
    mrMessageConfiguration,
    mrAddresses,
    mrContext,
    mrEndpoints,
    mrTemplateConfiguration,
    mrTraceId,

    -- ** CampaignStatus
    CampaignStatus (..),

    -- ** EndpointUser
    EndpointUser (..),
    mkEndpointUser,
    euUserAttributes,
    euUserId,

    -- ** PushNotificationTemplateRequest
    PushNotificationTemplateRequest (..),
    mkPushNotificationTemplateRequest,
    pADM,
    pAPNS,
    pBaidu,
    pDefault,
    pDefaultSubstitutions,
    pGCM,
    pRecommenderId,
    pTemplateDescription,
    pTags,

    -- ** WriteCampaignRequest
    WriteCampaignRequest (..),
    mkWriteCampaignRequest,
    wcrAdditionalTreatments,
    wcrCustomDeliveryConfiguration,
    wcrDescription,
    wcrHoldoutPercent,
    wcrHook,
    wcrIsPaused,
    wcrLimits,
    wcrMessageConfiguration,
    wcrName,
    wcrSchedule,
    wcrSegmentId,
    wcrSegmentVersion,
    wcrTemplateConfiguration,
    wcrTreatmentDescription,
    wcrTreatmentName,
    wcrTags,

    -- ** SimpleEmail
    SimpleEmail (..),
    mkSimpleEmail,
    seHtmlPart,
    seSubject,
    seTextPart,

    -- ** EndpointBatchItem
    EndpointBatchItem (..),
    mkEndpointBatchItem,
    ebiAddress,
    ebiAttributes,
    ebiChannelType,
    ebiDemographic,
    ebiEffectiveDate,
    ebiEndpointStatus,
    ebiId,
    ebiLocation,
    ebiMetrics,
    ebiOptOut,
    ebiRequestId,
    ebiUser,

    -- ** CustomMessageActivity
    CustomMessageActivity (..),
    mkCustomMessageActivity,
    cmaDeliveryUri,
    cmaEndpointTypes,
    cmaMessageConfig,
    cmaNextActivity,
    cmaTemplateName,
    cmaTemplateVersion,

    -- ** GPSCoordinates
    GPSCoordinates (..),
    mkGPSCoordinates,
    gpscLatitude,
    gpscLongitude,

    -- ** JourneyPushMessage
    JourneyPushMessage (..),
    mkJourneyPushMessage,
    jpmTimeToLive,

    -- ** RecencyType
    RecencyType (..),

    -- ** ApplicationSettingsResource
    ApplicationSettingsResource (..),
    mkApplicationSettingsResource,
    asrApplicationId,
    asrCampaignHook,
    asrLastModifiedDate,
    asrLimits,
    asrQuietTime,

    -- ** RecencyDimension
    RecencyDimension (..),
    mkRecencyDimension,
    rdDuration,
    rdRecencyType,

    -- ** WriteEventStream
    WriteEventStream (..),
    mkWriteEventStream,
    wesRoleArn,
    wesDestinationStreamArn,

    -- ** SegmentDemographics
    SegmentDemographics (..),
    mkSegmentDemographics,
    sdAppVersion,
    sdChannel,
    sdDeviceType,
    sdMake,
    sdModel,
    sdPlatform,

    -- ** VoiceTemplateResponse
    VoiceTemplateResponse (..),
    mkVoiceTemplateResponse,
    vLastModifiedDate,
    vCreationDate,
    vTemplateName,
    vTemplateType,
    vArn,
    vBody,
    vDefaultSubstitutions,
    vLanguageCode,
    vTemplateDescription,
    vVersion,
    vVoiceId,
    vTags,

    -- ** CampaignHook
    CampaignHook (..),
    mkCampaignHook,
    chLambdaFunctionName,
    chMode,
    chWebUrl,

    -- ** VoiceMessage
    VoiceMessage (..),
    mkVoiceMessage,
    vmBody,
    vmLanguageCode,
    vmOriginationNumber,
    vmSubstitutions,
    vmVoiceId,

    -- ** Activity
    Activity (..),
    mkActivity,
    aCUSTOM,
    aConditionalSplit,
    aDescription,
    aEMAIL,
    aHoldout,
    aMultiCondition,
    aPUSH,
    aRandomSplit,
    aSMS,
    aWait,

    -- ** SimpleEmailPart
    SimpleEmailPart (..),
    mkSimpleEmailPart,
    sepCharset,
    sepData,

    -- ** CampaignResponse
    CampaignResponse (..),
    mkCampaignResponse,
    crLastModifiedDate,
    crCreationDate,
    crSegmentId,
    crSegmentVersion,
    crId,
    crArn,
    crApplicationId,
    crAdditionalTreatments,
    crCustomDeliveryConfiguration,
    crDefaultState,
    crDescription,
    crHoldoutPercent,
    crHook,
    crIsPaused,
    crLimits,
    crMessageConfiguration,
    crName,
    crSchedule,
    crState,
    crTemplateConfiguration,
    crTreatmentDescription,
    crTreatmentName,
    crVersion,
    crTags,

    -- ** MessageResponse
    MessageResponse (..),
    mkMessageResponse,
    mrApplicationId,
    mrEndpointResult,
    mrRequestId,
    mrResult,

    -- ** MultiConditionalSplitActivity
    MultiConditionalSplitActivity (..),
    mkMultiConditionalSplitActivity,
    mcsaBranches,
    mcsaDefaultActivity,
    mcsaEvaluationWaitTime,

    -- ** EndpointResponse
    EndpointResponse (..),
    mkEndpointResponse,
    erAddress,
    erApplicationId,
    erAttributes,
    erChannelType,
    erCohortId,
    erCreationDate,
    erDemographic,
    erEffectiveDate,
    erEndpointStatus,
    erId,
    erLocation,
    erMetrics,
    erOptOut,
    erRequestId,
    erUser,

    -- ** ResultRowValue
    ResultRowValue (..),
    mkResultRowValue,
    rrvType,
    rrvValue,
    rrvKey,

    -- ** Type
    Type (..),

    -- ** DefaultPushNotificationTemplate
    DefaultPushNotificationTemplate (..),
    mkDefaultPushNotificationTemplate,
    dpntAction,
    dpntBody,
    dpntSound,
    dpntTitle,
    dpntUrl,

    -- ** ADMChannelResponse
    ADMChannelResponse (..),
    mkADMChannelResponse,
    admcrPlatform,
    admcrApplicationId,
    admcrCreationDate,
    admcrEnabled,
    admcrHasCredential,
    admcrId,
    admcrIsArchived,
    admcrLastModifiedBy,
    admcrLastModifiedDate,
    admcrVersion,

    -- ** Template
    Template (..),
    mkTemplate,
    tName,
    tVersion,

    -- ** JourneysResponse
    JourneysResponse (..),
    mkJourneysResponse,
    jrItem,
    jrNextToken,

    -- ** SegmentResponse
    SegmentResponse (..),
    mkSegmentResponse,
    srSegmentType,
    srCreationDate,
    srId,
    srArn,
    srApplicationId,
    srDimensions,
    srImportDefinition,
    srLastModifiedDate,
    srName,
    srSegmentGroups,
    srVersion,
    srTags,

    -- ** BaiduMessage
    BaiduMessage (..),
    mkBaiduMessage,
    bmAction,
    bmBody,
    bmData,
    bmIconReference,
    bmImageIconUrl,
    bmImageUrl,
    bmRawContent,
    bmSilentPush,
    bmSmallImageIconUrl,
    bmSound,
    bmSubstitutions,
    bmTimeToLive,
    bmTitle,
    bmUrl,

    -- ** RecommenderConfigurationResponse
    RecommenderConfigurationResponse (..),
    mkRecommenderConfigurationResponse,
    rcrRecommendationProviderUri,
    rcrLastModifiedDate,
    rcrCreationDate,
    rcrRecommendationProviderRoleArn,
    rcrId,
    rcrAttributes,
    rcrDescription,
    rcrName,
    rcrRecommendationProviderIdType,
    rcrRecommendationTransformerUri,
    rcrRecommendationsDisplayName,
    rcrRecommendationsPerMessage,

    -- ** DimensionType
    DimensionType (..),

    -- ** SegmentCondition
    SegmentCondition (..),
    mkSegmentCondition,
    scSegmentId,

    -- ** PushNotificationTemplateResponse
    PushNotificationTemplateResponse (..),
    mkPushNotificationTemplateResponse,
    pntrLastModifiedDate,
    pntrCreationDate,
    pntrTemplateType,
    pntrTemplateName,
    pntrADM,
    pntrAPNS,
    pntrArn,
    pntrBaidu,
    pntrDefault,
    pntrDefaultSubstitutions,
    pntrGCM,
    pntrRecommenderId,
    pntrTemplateDescription,
    pntrVersion,
    pntrTags,

    -- ** ExportJobResponse
    ExportJobResponse (..),
    mkExportJobResponse,
    ejrJobStatus,
    ejrCreationDate,
    ejrType,
    ejrDefinition,
    ejrId,
    ejrApplicationId,
    ejrCompletedPieces,
    ejrCompletionDate,
    ejrFailedPieces,
    ejrFailures,
    ejrTotalFailures,
    ejrTotalPieces,
    ejrTotalProcessed,

    -- ** JobStatus
    JobStatus (..),

    -- ** SegmentsResponse
    SegmentsResponse (..),
    mkSegmentsResponse,
    srItem,
    srNextToken,

    -- ** Condition
    Condition (..),
    mkCondition,
    cConditions,
    cOperator,

    -- ** WriteTreatmentResource
    WriteTreatmentResource (..),
    mkWriteTreatmentResource,
    wtrSizePercent,
    wtrCustomDeliveryConfiguration,
    wtrMessageConfiguration,
    wtrSchedule,
    wtrTemplateConfiguration,
    wtrTreatmentDescription,
    wtrTreatmentName,

    -- ** EndpointsResponse
    EndpointsResponse (..),
    mkEndpointsResponse,
    erItem,

    -- ** APNSSandboxChannelRequest
    APNSSandboxChannelRequest (..),
    mkAPNSSandboxChannelRequest,
    apnsscrfBundleId,
    apnsscrfCertificate,
    apnsscrfDefaultAuthenticationMethod,
    apnsscrfEnabled,
    apnsscrfPrivateKey,
    apnsscrfTeamId,
    apnsscrfTokenKey,
    apnsscrfTokenKeyId,

    -- ** Message
    Message (..),
    mkMessage,
    mAction,
    mBody,
    mImageIconUrl,
    mImageSmallIconUrl,
    mImageUrl,
    mJsonBody,
    mMediaUrl,
    mRawContent,
    mSilentPush,
    mTimeToLive,
    mTitle,
    mUrl,

    -- ** DefaultPushNotificationMessage
    DefaultPushNotificationMessage (..),
    mkDefaultPushNotificationMessage,
    dpnmAction,
    dpnmBody,
    dpnmData,
    dpnmSilentPush,
    dpnmSubstitutions,
    dpnmTitle,
    dpnmUrl,

    -- ** StartCondition
    StartCondition (..),
    mkStartCondition,
    scDescription,
    scEventStartCondition,
    scSegmentStartCondition,

    -- ** TemplateResponse
    TemplateResponse (..),
    mkTemplateResponse,
    trLastModifiedDate,
    trCreationDate,
    trTemplateName,
    trTemplateType,
    trArn,
    trDefaultSubstitutions,
    trTemplateDescription,
    trVersion,
    trTags,

    -- ** JourneyExecutionActivityMetricsResponse
    JourneyExecutionActivityMetricsResponse (..),
    mkJourneyExecutionActivityMetricsResponse,
    jeamrMetrics,
    jeamrJourneyId,
    jeamrLastEvaluatedTime,
    jeamrJourneyActivityId,
    jeamrActivityType,
    jeamrApplicationId,

    -- ** ExportJobsResponse
    ExportJobsResponse (..),
    mkExportJobsResponse,
    ejrItem,
    ejrNextToken,

    -- ** ChannelType
    ChannelType (..),

    -- ** MessageConfiguration
    MessageConfiguration (..),
    mkMessageConfiguration,
    mcADMMessage,
    mcAPNSMessage,
    mcBaiduMessage,
    mcCustomMessage,
    mcDefaultMessage,
    mcEmailMessage,
    mcGCMMessage,
    mcSMSMessage,

    -- ** MessageBody
    MessageBody (..),
    mkMessageBody,
    mbMessage,
    mbRequestID,

    -- ** TreatmentResource
    TreatmentResource (..),
    mkTreatmentResource,
    trId,
    trSizePercent,
    trCustomDeliveryConfiguration,
    trMessageConfiguration,
    trSchedule,
    trState,
    trTemplateConfiguration,
    trTreatmentDescription,
    trTreatmentName,

    -- ** CampaignsResponse
    CampaignsResponse (..),
    mkCampaignsResponse,
    crItem,
    crNextToken,

    -- ** VoiceChannelResponse
    VoiceChannelResponse (..),
    mkVoiceChannelResponse,
    vcrfPlatform,
    vcrfApplicationId,
    vcrfCreationDate,
    vcrfEnabled,
    vcrfHasCredential,
    vcrfId,
    vcrfIsArchived,
    vcrfLastModifiedBy,
    vcrfLastModifiedDate,
    vcrfVersion,

    -- ** EndpointDemographic
    EndpointDemographic (..),
    mkEndpointDemographic,
    edAppVersion,
    edLocale,
    edMake,
    edModel,
    edModelVersion,
    edPlatform,
    edPlatformVersion,
    edTimezone,

    -- ** JourneyStateRequest
    JourneyStateRequest (..),
    mkJourneyStateRequest,
    jsrState,

    -- ** SegmentGroupList
    SegmentGroupList (..),
    mkSegmentGroupList,
    sglGroups,
    sglInclude,

    -- ** SMSTemplateResponse
    SMSTemplateResponse (..),
    mkSMSTemplateResponse,
    smstrfLastModifiedDate,
    smstrfCreationDate,
    smstrfTemplateName,
    smstrfTemplateType,
    smstrfArn,
    smstrfBody,
    smstrfDefaultSubstitutions,
    smstrfRecommenderId,
    smstrfTemplateDescription,
    smstrfVersion,
    smstrfTags,

    -- ** APNSVoipChannelRequest
    APNSVoipChannelRequest (..),
    mkAPNSVoipChannelRequest,
    apnsvcrfBundleId,
    apnsvcrfCertificate,
    apnsvcrfDefaultAuthenticationMethod,
    apnsvcrfEnabled,
    apnsvcrfPrivateKey,
    apnsvcrfTeamId,
    apnsvcrfTokenKey,
    apnsvcrfTokenKeyId,

    -- ** EmailChannelRequest
    EmailChannelRequest (..),
    mkEmailChannelRequest,
    ecrfFromAddress,
    ecrfIdentity,
    ecrfConfigurationSet,
    ecrfEnabled,
    ecrfRoleArn,

    -- ** Session
    Session (..),
    mkSession,
    sfStartTimestamp,
    sfId,
    sfDuration,
    sfStopTimestamp,

    -- ** WaitTime
    WaitTime (..),
    mkWaitTime,
    wtWaitFor,
    wtWaitUntil,

    -- ** Duration
    Duration (..),

    -- ** AddressConfiguration
    AddressConfiguration (..),
    mkAddressConfiguration,
    acBodyOverride,
    acChannelType,
    acContext,
    acRawContent,
    acSubstitutions,
    acTitleOverride,

    -- ** EndpointBatchRequest
    EndpointBatchRequest (..),
    mkEndpointBatchRequest,
    ebrItem,

    -- ** SendUsersMessageResponse
    SendUsersMessageResponse (..),
    mkSendUsersMessageResponse,
    sumrApplicationId,
    sumrRequestId,
    sumrResult,

    -- ** BaseKpiResult
    BaseKpiResult (..),
    mkBaseKpiResult,
    bkrRows,

    -- ** DirectMessageConfiguration
    DirectMessageConfiguration (..),
    mkDirectMessageConfiguration,
    dmcADMMessage,
    dmcAPNSMessage,
    dmcBaiduMessage,
    dmcDefaultMessage,
    dmcDefaultPushNotificationMessage,
    dmcEmailMessage,
    dmcGCMMessage,
    dmcSMSMessage,
    dmcVoiceMessage,

    -- ** BaiduChannelRequest
    BaiduChannelRequest (..),
    mkBaiduChannelRequest,
    bSecretKey,
    bApiKey,
    bEnabled,

    -- ** GCMChannelResponse
    GCMChannelResponse (..),
    mkGCMChannelResponse,
    gcmcrCredential,
    gcmcrPlatform,
    gcmcrApplicationId,
    gcmcrCreationDate,
    gcmcrEnabled,
    gcmcrHasCredential,
    gcmcrId,
    gcmcrIsArchived,
    gcmcrLastModifiedBy,
    gcmcrLastModifiedDate,
    gcmcrVersion,

    -- ** EndpointMessageResult
    EndpointMessageResult (..),
    mkEndpointMessageResult,
    emrDeliveryStatus,
    emrStatusCode,
    emrAddress,
    emrMessageId,
    emrStatusMessage,
    emrUpdatedToken,

    -- ** ImportJobResponse
    ImportJobResponse (..),
    mkImportJobResponse,
    ijrJobStatus,
    ijrCreationDate,
    ijrType,
    ijrDefinition,
    ijrId,
    ijrApplicationId,
    ijrCompletedPieces,
    ijrCompletionDate,
    ijrFailedPieces,
    ijrFailures,
    ijrTotalFailures,
    ijrTotalPieces,
    ijrTotalProcessed,

    -- ** APNSVoipSandboxChannelResponse
    APNSVoipSandboxChannelResponse (..),
    mkAPNSVoipSandboxChannelResponse,
    apnsvscrPlatform,
    apnsvscrApplicationId,
    apnsvscrCreationDate,
    apnsvscrDefaultAuthenticationMethod,
    apnsvscrEnabled,
    apnsvscrHasCredential,
    apnsvscrHasTokenKey,
    apnsvscrId,
    apnsvscrIsArchived,
    apnsvscrLastModifiedBy,
    apnsvscrLastModifiedDate,
    apnsvscrVersion,

    -- ** ConditionalSplitActivity
    ConditionalSplitActivity (..),
    mkConditionalSplitActivity,
    csaCondition,
    csaEvaluationWaitTime,
    csaFalseActivity,
    csaTrueActivity,

    -- ** ListRecommenderConfigurationsResponse
    ListRecommenderConfigurationsResponse (..),
    mkListRecommenderConfigurationsResponse,
    lrcrItem,
    lrcrNextToken,

    -- ** CampaignState
    CampaignState (..),
    mkCampaignState,
    csCampaignStatus,

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.UTCTime,
    Lude.NominalDiffTime,
  )
where

import Network.AWS.Pinpoint.CreateApp
import Network.AWS.Pinpoint.CreateCampaign
import Network.AWS.Pinpoint.CreateEmailTemplate
import Network.AWS.Pinpoint.CreateExportJob
import Network.AWS.Pinpoint.CreateImportJob
import Network.AWS.Pinpoint.CreateJourney
import Network.AWS.Pinpoint.CreatePushTemplate
import Network.AWS.Pinpoint.CreateRecommenderConfiguration
import Network.AWS.Pinpoint.CreateSegment
import Network.AWS.Pinpoint.CreateSmsTemplate
import Network.AWS.Pinpoint.CreateVoiceTemplate
import Network.AWS.Pinpoint.DeleteAdmChannel
import Network.AWS.Pinpoint.DeleteApnsChannel
import Network.AWS.Pinpoint.DeleteApnsSandboxChannel
import Network.AWS.Pinpoint.DeleteApnsVoipChannel
import Network.AWS.Pinpoint.DeleteApnsVoipSandboxChannel
import Network.AWS.Pinpoint.DeleteApp
import Network.AWS.Pinpoint.DeleteBaiduChannel
import Network.AWS.Pinpoint.DeleteCampaign
import Network.AWS.Pinpoint.DeleteEmailChannel
import Network.AWS.Pinpoint.DeleteEmailTemplate
import Network.AWS.Pinpoint.DeleteEndpoint
import Network.AWS.Pinpoint.DeleteEventStream
import Network.AWS.Pinpoint.DeleteGcmChannel
import Network.AWS.Pinpoint.DeleteJourney
import Network.AWS.Pinpoint.DeletePushTemplate
import Network.AWS.Pinpoint.DeleteRecommenderConfiguration
import Network.AWS.Pinpoint.DeleteSegment
import Network.AWS.Pinpoint.DeleteSmsChannel
import Network.AWS.Pinpoint.DeleteSmsTemplate
import Network.AWS.Pinpoint.DeleteUserEndpoints
import Network.AWS.Pinpoint.DeleteVoiceChannel
import Network.AWS.Pinpoint.DeleteVoiceTemplate
import Network.AWS.Pinpoint.GetAdmChannel
import Network.AWS.Pinpoint.GetApnsChannel
import Network.AWS.Pinpoint.GetApnsSandboxChannel
import Network.AWS.Pinpoint.GetApnsVoipChannel
import Network.AWS.Pinpoint.GetApnsVoipSandboxChannel
import Network.AWS.Pinpoint.GetApp
import Network.AWS.Pinpoint.GetApplicationDateRangeKpi
import Network.AWS.Pinpoint.GetApplicationSettings
import Network.AWS.Pinpoint.GetApps
import Network.AWS.Pinpoint.GetBaiduChannel
import Network.AWS.Pinpoint.GetCampaign
import Network.AWS.Pinpoint.GetCampaignActivities
import Network.AWS.Pinpoint.GetCampaignDateRangeKpi
import Network.AWS.Pinpoint.GetCampaignVersion
import Network.AWS.Pinpoint.GetCampaignVersions
import Network.AWS.Pinpoint.GetCampaigns
import Network.AWS.Pinpoint.GetChannels
import Network.AWS.Pinpoint.GetEmailChannel
import Network.AWS.Pinpoint.GetEmailTemplate
import Network.AWS.Pinpoint.GetEndpoint
import Network.AWS.Pinpoint.GetEventStream
import Network.AWS.Pinpoint.GetExportJob
import Network.AWS.Pinpoint.GetExportJobs
import Network.AWS.Pinpoint.GetGcmChannel
import Network.AWS.Pinpoint.GetImportJob
import Network.AWS.Pinpoint.GetImportJobs
import Network.AWS.Pinpoint.GetJourney
import Network.AWS.Pinpoint.GetJourneyDateRangeKpi
import Network.AWS.Pinpoint.GetJourneyExecutionActivityMetrics
import Network.AWS.Pinpoint.GetJourneyExecutionMetrics
import Network.AWS.Pinpoint.GetPushTemplate
import Network.AWS.Pinpoint.GetRecommenderConfiguration
import Network.AWS.Pinpoint.GetRecommenderConfigurations
import Network.AWS.Pinpoint.GetSegment
import Network.AWS.Pinpoint.GetSegmentExportJobs
import Network.AWS.Pinpoint.GetSegmentImportJobs
import Network.AWS.Pinpoint.GetSegmentVersion
import Network.AWS.Pinpoint.GetSegmentVersions
import Network.AWS.Pinpoint.GetSegments
import Network.AWS.Pinpoint.GetSmsChannel
import Network.AWS.Pinpoint.GetSmsTemplate
import Network.AWS.Pinpoint.GetUserEndpoints
import Network.AWS.Pinpoint.GetVoiceChannel
import Network.AWS.Pinpoint.GetVoiceTemplate
import Network.AWS.Pinpoint.ListJourneys
import Network.AWS.Pinpoint.ListTagsForResource
import Network.AWS.Pinpoint.ListTemplateVersions
import Network.AWS.Pinpoint.ListTemplates
import Network.AWS.Pinpoint.PhoneNumberValidate
import Network.AWS.Pinpoint.PutEventStream
import Network.AWS.Pinpoint.PutEvents
import Network.AWS.Pinpoint.RemoveAttributes
import Network.AWS.Pinpoint.SendMessages
import Network.AWS.Pinpoint.SendUsersMessages
import Network.AWS.Pinpoint.TagResource
import Network.AWS.Pinpoint.Types
import Network.AWS.Pinpoint.UntagResource
import Network.AWS.Pinpoint.UpdateAdmChannel
import Network.AWS.Pinpoint.UpdateApnsChannel
import Network.AWS.Pinpoint.UpdateApnsSandboxChannel
import Network.AWS.Pinpoint.UpdateApnsVoipChannel
import Network.AWS.Pinpoint.UpdateApnsVoipSandboxChannel
import Network.AWS.Pinpoint.UpdateApplicationSettings
import Network.AWS.Pinpoint.UpdateBaiduChannel
import Network.AWS.Pinpoint.UpdateCampaign
import Network.AWS.Pinpoint.UpdateEmailChannel
import Network.AWS.Pinpoint.UpdateEmailTemplate
import Network.AWS.Pinpoint.UpdateEndpoint
import Network.AWS.Pinpoint.UpdateEndpointsBatch
import Network.AWS.Pinpoint.UpdateGcmChannel
import Network.AWS.Pinpoint.UpdateJourney
import Network.AWS.Pinpoint.UpdateJourneyState
import Network.AWS.Pinpoint.UpdatePushTemplate
import Network.AWS.Pinpoint.UpdateRecommenderConfiguration
import Network.AWS.Pinpoint.UpdateSegment
import Network.AWS.Pinpoint.UpdateSmsChannel
import Network.AWS.Pinpoint.UpdateSmsTemplate
import Network.AWS.Pinpoint.UpdateTemplateActiveVersion
import Network.AWS.Pinpoint.UpdateVoiceChannel
import Network.AWS.Pinpoint.UpdateVoiceTemplate
import Network.AWS.Pinpoint.Waiters
import qualified Network.AWS.Prelude as Lude

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'Pinpoint'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
