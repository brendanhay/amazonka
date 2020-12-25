-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    _PayloadTooLargeException,
    _ConflictException,
    _ForbiddenException,
    _NotFoundException,
    _TooManyRequestsException,
    _InternalServerErrorException,
    _MethodNotAllowedException,
    _BadRequestException,

    -- * SegmentBehaviors
    SegmentBehaviors (..),
    mkSegmentBehaviors,
    sbRecency,

    -- * AttributeDimension
    AttributeDimension (..),
    mkAttributeDimension,
    adValues,
    adAttributeType,

    -- * APNSVoipChannelResponse
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

    -- * Include
    Include (..),

    -- * VoiceChannelRequest
    VoiceChannelRequest (..),
    mkVoiceChannelRequest,
    vcrEnabled,

    -- * SendUsersMessageRequest
    SendUsersMessageRequest (..),
    mkSendUsersMessageRequest,
    sumrMessageConfiguration,
    sumrUsers,
    sumrContext,
    sumrTemplateConfiguration,
    sumrTraceId,

    -- * SegmentGroup
    SegmentGroup (..),
    mkSegmentGroup,
    sgDimensions,
    sgSourceSegments,
    sgSourceType,
    sgType,

    -- * EmailChannelResponse
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

    -- * SMSTemplateRequest
    SMSTemplateRequest (..),
    mkSMSTemplateRequest,
    smstrBody,
    smstrDefaultSubstitutions,
    smstrRecommenderId,
    smstrTemplateDescription,
    smstrTags,

    -- * Frequency
    Frequency (..),

    -- * Event
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

    -- * CustomDeliveryConfiguration
    CustomDeliveryConfiguration (..),
    mkCustomDeliveryConfiguration,
    cdcDeliveryUri,
    cdcEndpointTypes,

    -- * CreateApplicationRequest
    CreateApplicationRequest (..),
    mkCreateApplicationRequest,
    carName,
    carTags,

    -- * APNSPushNotificationTemplate
    APNSPushNotificationTemplate (..),
    mkAPNSPushNotificationTemplate,
    apnspntAction,
    apnspntBody,
    apnspntMediaUrl,
    apnspntRawContent,
    apnspntSound,
    apnspntTitle,
    apnspntUrl,

    -- * EndpointSendConfiguration
    EndpointSendConfiguration (..),
    mkEndpointSendConfiguration,
    escBodyOverride,
    escContext,
    escRawContent,
    escSubstitutions,
    escTitleOverride,

    -- * State
    State (..),

    -- * APNSMessage
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

    -- * TemplateActiveVersionRequest
    TemplateActiveVersionRequest (..),
    mkTemplateActiveVersionRequest,
    tavrVersion,

    -- * VoiceTemplateRequest
    VoiceTemplateRequest (..),
    mkVoiceTemplateRequest,
    vtrBody,
    vtrDefaultSubstitutions,
    vtrLanguageCode,
    vtrTemplateDescription,
    vtrVoiceId,
    vtrTags,

    -- * GCMMessage
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

    -- * ImportJobsResponse
    ImportJobsResponse (..),
    mkImportJobsResponse,
    ijrItem,
    ijrNextToken,

    -- * SetDimension
    SetDimension (..),
    mkSetDimension,
    sdValues,
    sdDimensionType,

    -- * JourneyDateRangeKpiResponse
    JourneyDateRangeKpiResponse (..),
    mkJourneyDateRangeKpiResponse,
    jdrkrKpiResult,
    jdrkrKpiName,
    jdrkrJourneyId,
    jdrkrEndTime,
    jdrkrStartTime,
    jdrkrApplicationId,
    jdrkrNextToken,

    -- * PublicEndpoint
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

    -- * CampaignEventFilter
    CampaignEventFilter (..),
    mkCampaignEventFilter,
    cefFilterType,
    cefDimensions,

    -- * EmailMessageActivity
    EmailMessageActivity (..),
    mkEmailMessageActivity,
    emaMessageConfig,
    emaNextActivity,
    emaTemplateName,
    emaTemplateVersion,

    -- * AttributesResource
    AttributesResource (..),
    mkAttributesResource,
    arAttributeType,
    arApplicationId,
    arAttributes,

    -- * EmailTemplateRequest
    EmailTemplateRequest (..),
    mkEmailTemplateRequest,
    etrDefaultSubstitutions,
    etrHtmlPart,
    etrRecommenderId,
    etrSubject,
    etrTemplateDescription,
    etrTextPart,
    etrTags,

    -- * SMSChannelResponse
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

    -- * EndpointItemResponse
    EndpointItemResponse (..),
    mkEndpointItemResponse,
    eirMessage,
    eirStatusCode,

    -- * JourneySchedule
    JourneySchedule (..),
    mkJourneySchedule,
    jsEndTime,
    jsStartTime,
    jsTimezone,

    -- * SourceType
    SourceType (..),

    -- * Schedule
    Schedule (..),
    mkSchedule,
    sStartTime,
    sEndTime,
    sEventFilter,
    sFrequency,
    sIsLocalTime,
    sQuietTime,
    sTimezone,

    -- * SimpleCondition
    SimpleCondition (..),
    mkSimpleCondition,
    scEventCondition,
    scSegmentCondition,
    scSegmentDimensions,

    -- * RandomSplitEntry
    RandomSplitEntry (..),
    mkRandomSplitEntry,
    rseNextActivity,
    rsePercentage,

    -- * AndroidPushNotificationTemplate
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

    -- * TemplateType
    TemplateType (..),

    -- * SegmentDimensions
    SegmentDimensions (..),
    mkSegmentDimensions,
    sdAttributes,
    sdBehavior,
    sdDemographic,
    sdLocation,
    sdMetrics,
    sdUserAttributes,

    -- * ApplicationDateRangeKpiResponse
    ApplicationDateRangeKpiResponse (..),
    mkApplicationDateRangeKpiResponse,
    adrkrKpiResult,
    adrkrKpiName,
    adrkrEndTime,
    adrkrStartTime,
    adrkrApplicationId,
    adrkrNextToken,

    -- * JourneyCustomMessage
    JourneyCustomMessage (..),
    mkJourneyCustomMessage,
    jcmData,

    -- * DeliveryStatus
    DeliveryStatus (..),

    -- * PushMessageActivity
    PushMessageActivity (..),
    mkPushMessageActivity,
    pmaMessageConfig,
    pmaNextActivity,
    pmaTemplateName,
    pmaTemplateVersion,

    -- * SegmentReference
    SegmentReference (..),
    mkSegmentReference,
    sId,
    sVersion,

    -- * CampaignSmsMessage
    CampaignSmsMessage (..),
    mkCampaignSmsMessage,
    csmBody,
    csmMessageType,
    csmSenderId,

    -- * EventStream
    EventStream (..),
    mkEventStream,
    esApplicationId,
    esRoleArn,
    esDestinationStreamArn,
    esExternalId,
    esLastModifiedDate,
    esLastUpdatedBy,

    -- * DefaultMessage
    DefaultMessage (..),
    mkDefaultMessage,
    dmBody,
    dmSubstitutions,

    -- * ImportJobResource
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

    -- * Operator
    Operator (..),

    -- * GPSPointDimension
    GPSPointDimension (..),
    mkGPSPointDimension,
    gpspdCoordinates,
    gpspdRangeInKilometers,

    -- * TemplateConfiguration
    TemplateConfiguration (..),
    mkTemplateConfiguration,
    tcEmailTemplate,
    tcPushTemplate,
    tcSMSTemplate,
    tcVoiceTemplate,

    -- * ChannelResponse
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

    -- * TemplatesResponse
    TemplatesResponse (..),
    mkTemplatesResponse,
    trItem,
    trNextToken,

    -- * FilterType
    FilterType (..),

    -- * ActivitiesResponse
    ActivitiesResponse (..),
    mkActivitiesResponse,
    arItem,
    arNextToken,

    -- * CampaignLimits
    CampaignLimits (..),
    mkCampaignLimits,
    clDaily,
    clMaximumDuration,
    clMessagesPerSecond,
    clTotal,

    -- * RawEmail
    RawEmail (..),
    mkRawEmail,
    reData,

    -- * ADMMessage
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

    -- * WriteApplicationSettingsRequest
    WriteApplicationSettingsRequest (..),
    mkWriteApplicationSettingsRequest,
    wasrCampaignHook,
    wasrCloudWatchMetricsEnabled,
    wasrEventTaggingEnabled,
    wasrLimits,
    wasrQuietTime,

    -- * GCMChannelRequest
    GCMChannelRequest (..),
    mkGCMChannelRequest,
    gApiKey,
    gEnabled,

    -- * DefinitionFormat
    DefinitionFormat (..),

    -- * SegmentType
    SegmentType (..),

    -- * JourneyResponse
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

    -- * ChannelsResponse
    ChannelsResponse (..),
    mkChannelsResponse,
    crChannels,

    -- * ResultRow
    ResultRow (..),
    mkResultRow,
    rrGroupedBys,
    rrValues,

    -- * EventsBatch
    EventsBatch (..),
    mkEventsBatch,
    ebEndpoint,
    ebEvents,

    -- * SMSMessageActivity
    SMSMessageActivity (..),
    mkSMSMessageActivity,
    smsmaMessageConfig,
    smsmaNextActivity,
    smsmaTemplateName,
    smsmaTemplateVersion,

    -- * APNSVoipSandboxChannelRequest
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

    -- * MultiConditionalBranch
    MultiConditionalBranch (..),
    mkMultiConditionalBranch,
    mcbCondition,
    mcbNextActivity,

    -- * ImportJobRequest
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

    -- * MessageType
    MessageType (..),

    -- * BaiduChannelResponse
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

    -- * Mode
    Mode (..),

    -- * JourneyLimits
    JourneyLimits (..),
    mkJourneyLimits,
    jlDailyCap,
    jlEndpointReentryCap,
    jlMessagesPerSecond,

    -- * NumberValidateResponse
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

    -- * ActivityResponse
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

    -- * Action
    Action (..),

    -- * EndpointTypesElement
    EndpointTypesElement (..),

    -- * JourneyExecutionMetricsResponse
    JourneyExecutionMetricsResponse (..),
    mkJourneyExecutionMetricsResponse,
    jemrMetrics,
    jemrJourneyId,
    jemrLastEvaluatedTime,
    jemrApplicationId,

    -- * EndpointLocation
    EndpointLocation (..),
    mkEndpointLocation,
    elCity,
    elCountry,
    elLatitude,
    elLongitude,
    elPostalCode,
    elRegion,

    -- * SMSMessage
    SMSMessage (..),
    mkSMSMessage,
    smsmBody,
    smsmKeyword,
    smsmMediaUrl,
    smsmMessageType,
    smsmOriginationNumber,
    smsmSenderId,
    smsmSubstitutions,

    -- * WaitActivity
    WaitActivity (..),
    mkWaitActivity,
    waNextActivity,
    waWaitTime,

    -- * ItemResponse
    ItemResponse (..),
    mkItemResponse,
    irEndpointItemResponse,
    irEventsItemResponse,

    -- * SegmentLocation
    SegmentLocation (..),
    mkSegmentLocation,
    slCountry,
    slGPSPoint,

    -- * APNSChannelRequest
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

    -- * TagsModel
    TagsModel (..),
    mkTagsModel,
    tmTags,

    -- * EventsRequest
    EventsRequest (..),
    mkEventsRequest,
    erBatchItem,

    -- * JourneySMSMessage
    JourneySMSMessage (..),
    mkJourneySMSMessage,
    jsmsmMessageType,
    jsmsmSenderId,

    -- * CampaignCustomMessage
    CampaignCustomMessage (..),
    mkCampaignCustomMessage,
    ccmData,

    -- * CampaignEmailMessage
    CampaignEmailMessage (..),
    mkCampaignEmailMessage,
    cemBody,
    cemFromAddress,
    cemHtmlBody,
    cemTitle,

    -- * ApplicationResponse
    ApplicationResponse (..),
    mkApplicationResponse,
    arId,
    arArn,
    arName,
    arTags,

    -- * TemplateVersionResponse
    TemplateVersionResponse (..),
    mkTemplateVersionResponse,
    tvrLastModifiedDate,
    tvrCreationDate,
    tvrTemplateName,
    tvrTemplateType,
    tvrDefaultSubstitutions,
    tvrTemplateDescription,
    tvrVersion,

    -- * QuietTime
    QuietTime (..),
    mkQuietTime,
    qtEnd,
    qtStart,

    -- * APNSSandboxChannelResponse
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

    -- * ExportJobResource
    ExportJobResource (..),
    mkExportJobResource,
    ejrS3UrlPrefix,
    ejrRoleArn,
    ejrSegmentId,
    ejrSegmentVersion,

    -- * APNSChannelResponse
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

    -- * MetricDimension
    MetricDimension (..),
    mkMetricDimension,
    mdComparisonOperator,
    mdValue,

    -- * NumberValidateRequest
    NumberValidateRequest (..),
    mkNumberValidateRequest,
    nvrIsoCountryCode,
    nvrPhoneNumber,

    -- * EventsResponse
    EventsResponse (..),
    mkEventsResponse,
    erResults,

    -- * EventFilter
    EventFilter (..),
    mkEventFilter,
    efFilterType,
    efDimensions,

    -- * WriteJourneyRequest
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

    -- * HoldoutActivity
    HoldoutActivity (..),
    mkHoldoutActivity,
    haPercentage,
    haNextActivity,

    -- * SegmentImportResource
    SegmentImportResource (..),
    mkSegmentImportResource,
    sirFormat,
    sirS3Url,
    sirSize,
    sirExternalId,
    sirRoleArn,
    sirChannelCounts,

    -- * RandomSplitActivity
    RandomSplitActivity (..),
    mkRandomSplitActivity,
    rsaBranches,

    -- * AttributeType
    AttributeType (..),

    -- * CampaignDateRangeKpiResponse
    CampaignDateRangeKpiResponse (..),
    mkCampaignDateRangeKpiResponse,
    cdrkrKpiResult,
    cdrkrKpiName,
    cdrkrEndTime,
    cdrkrCampaignId,
    cdrkrStartTime,
    cdrkrApplicationId,
    cdrkrNextToken,

    -- * EmailMessage
    EmailMessage (..),
    mkEmailMessage,
    emBody,
    emFeedbackForwardingAddress,
    emFromAddress,
    emRawEmail,
    emReplyToAddresses,
    emSimpleEmail,
    emSubstitutions,

    -- * UpdateAttributesRequest
    UpdateAttributesRequest (..),
    mkUpdateAttributesRequest,
    uarBlacklist,

    -- * EventStartCondition
    EventStartCondition (..),
    mkEventStartCondition,
    escEventFilter,
    escSegmentId,

    -- * EventCondition
    EventCondition (..),
    mkEventCondition,
    ecDimensions,
    ecMessageActivity,

    -- * CreateTemplateMessageBody
    CreateTemplateMessageBody (..),
    mkCreateTemplateMessageBody,
    ctmbArn,
    ctmbMessage,
    ctmbRequestID,

    -- * JourneyEmailMessage
    JourneyEmailMessage (..),
    mkJourneyEmailMessage,
    jemFromAddress,

    -- * EventDimensions
    EventDimensions (..),
    mkEventDimensions,
    edAttributes,
    edEventType,
    edMetrics,

    -- * SMSChannelRequest
    SMSChannelRequest (..),
    mkSMSChannelRequest,
    sEnabled,
    sSenderId,
    sShortCode,

    -- * WriteSegmentRequest
    WriteSegmentRequest (..),
    mkWriteSegmentRequest,
    wsrDimensions,
    wsrName,
    wsrSegmentGroups,
    wsrTags,

    -- * EmailTemplateResponse
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

    -- * ADMChannelRequest
    ADMChannelRequest (..),
    mkADMChannelRequest,
    admcrfClientSecret,
    admcrfClientId,
    admcrfEnabled,

    -- * ExportJobRequest
    ExportJobRequest (..),
    mkExportJobRequest,
    eS3UrlPrefix,
    eRoleArn,
    eSegmentId,
    eSegmentVersion,

    -- * MessageResult
    MessageResult (..),
    mkMessageResult,
    mrDeliveryStatus,
    mrStatusCode,
    mrMessageId,
    mrStatusMessage,
    mrUpdatedToken,

    -- * EventItemResponse
    EventItemResponse (..),
    mkEventItemResponse,
    eMessage,
    eStatusCode,

    -- * EndpointRequest
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

    -- * ApplicationsResponse
    ApplicationsResponse (..),
    mkApplicationsResponse,
    aItem,
    aNextToken,

    -- * TemplateVersionsResponse
    TemplateVersionsResponse (..),
    mkTemplateVersionsResponse,
    tvrItem,
    tvrMessage,
    tvrNextToken,
    tvrRequestID,

    -- * MessageRequest
    MessageRequest (..),
    mkMessageRequest,
    mrMessageConfiguration,
    mrAddresses,
    mrContext,
    mrEndpoints,
    mrTemplateConfiguration,
    mrTraceId,

    -- * CampaignStatus
    CampaignStatus (..),

    -- * EndpointUser
    EndpointUser (..),
    mkEndpointUser,
    euUserAttributes,
    euUserId,

    -- * PushNotificationTemplateRequest
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

    -- * WriteCampaignRequest
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

    -- * SimpleEmail
    SimpleEmail (..),
    mkSimpleEmail,
    seHtmlPart,
    seSubject,
    seTextPart,

    -- * EndpointBatchItem
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

    -- * CustomMessageActivity
    CustomMessageActivity (..),
    mkCustomMessageActivity,
    cmaDeliveryUri,
    cmaEndpointTypes,
    cmaMessageConfig,
    cmaNextActivity,
    cmaTemplateName,
    cmaTemplateVersion,

    -- * GPSCoordinates
    GPSCoordinates (..),
    mkGPSCoordinates,
    gpscLatitude,
    gpscLongitude,

    -- * JourneyPushMessage
    JourneyPushMessage (..),
    mkJourneyPushMessage,
    jpmTimeToLive,

    -- * RecencyType
    RecencyType (..),

    -- * ApplicationSettingsResource
    ApplicationSettingsResource (..),
    mkApplicationSettingsResource,
    asrApplicationId,
    asrCampaignHook,
    asrLastModifiedDate,
    asrLimits,
    asrQuietTime,

    -- * RecencyDimension
    RecencyDimension (..),
    mkRecencyDimension,
    rdDuration,
    rdRecencyType,

    -- * WriteEventStream
    WriteEventStream (..),
    mkWriteEventStream,
    wesRoleArn,
    wesDestinationStreamArn,

    -- * SegmentDemographics
    SegmentDemographics (..),
    mkSegmentDemographics,
    sdAppVersion,
    sdChannel,
    sdDeviceType,
    sdMake,
    sdModel,
    sdPlatform,

    -- * VoiceTemplateResponse
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

    -- * CampaignHook
    CampaignHook (..),
    mkCampaignHook,
    chLambdaFunctionName,
    chMode,
    chWebUrl,

    -- * VoiceMessage
    VoiceMessage (..),
    mkVoiceMessage,
    vmBody,
    vmLanguageCode,
    vmOriginationNumber,
    vmSubstitutions,
    vmVoiceId,

    -- * Activity
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

    -- * SimpleEmailPart
    SimpleEmailPart (..),
    mkSimpleEmailPart,
    sepCharset,
    sepData,

    -- * CampaignResponse
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

    -- * MessageResponse
    MessageResponse (..),
    mkMessageResponse,
    mrApplicationId,
    mrEndpointResult,
    mrRequestId,
    mrResult,

    -- * MultiConditionalSplitActivity
    MultiConditionalSplitActivity (..),
    mkMultiConditionalSplitActivity,
    mcsaBranches,
    mcsaDefaultActivity,
    mcsaEvaluationWaitTime,

    -- * EndpointResponse
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

    -- * ResultRowValue
    ResultRowValue (..),
    mkResultRowValue,
    rrvType,
    rrvValue,
    rrvKey,

    -- * Type
    Type (..),

    -- * DefaultPushNotificationTemplate
    DefaultPushNotificationTemplate (..),
    mkDefaultPushNotificationTemplate,
    dpntAction,
    dpntBody,
    dpntSound,
    dpntTitle,
    dpntUrl,

    -- * ADMChannelResponse
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

    -- * Template
    Template (..),
    mkTemplate,
    tName,
    tVersion,

    -- * JourneysResponse
    JourneysResponse (..),
    mkJourneysResponse,
    jrItem,
    jrNextToken,

    -- * SegmentResponse
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

    -- * BaiduMessage
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

    -- * RecommenderConfigurationResponse
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

    -- * DimensionType
    DimensionType (..),

    -- * SegmentCondition
    SegmentCondition (..),
    mkSegmentCondition,
    scSegmentId,

    -- * PushNotificationTemplateResponse
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

    -- * ExportJobResponse
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

    -- * JobStatus
    JobStatus (..),

    -- * SegmentsResponse
    SegmentsResponse (..),
    mkSegmentsResponse,
    srItem,
    srNextToken,

    -- * Condition
    Condition (..),
    mkCondition,
    cConditions,
    cOperator,

    -- * WriteTreatmentResource
    WriteTreatmentResource (..),
    mkWriteTreatmentResource,
    wtrSizePercent,
    wtrCustomDeliveryConfiguration,
    wtrMessageConfiguration,
    wtrSchedule,
    wtrTemplateConfiguration,
    wtrTreatmentDescription,
    wtrTreatmentName,

    -- * EndpointsResponse
    EndpointsResponse (..),
    mkEndpointsResponse,
    erItem,

    -- * APNSSandboxChannelRequest
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

    -- * Message
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

    -- * DefaultPushNotificationMessage
    DefaultPushNotificationMessage (..),
    mkDefaultPushNotificationMessage,
    dpnmAction,
    dpnmBody,
    dpnmData,
    dpnmSilentPush,
    dpnmSubstitutions,
    dpnmTitle,
    dpnmUrl,

    -- * StartCondition
    StartCondition (..),
    mkStartCondition,
    scDescription,
    scEventStartCondition,
    scSegmentStartCondition,

    -- * TemplateResponse
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

    -- * JourneyExecutionActivityMetricsResponse
    JourneyExecutionActivityMetricsResponse (..),
    mkJourneyExecutionActivityMetricsResponse,
    jeamrMetrics,
    jeamrJourneyId,
    jeamrLastEvaluatedTime,
    jeamrJourneyActivityId,
    jeamrActivityType,
    jeamrApplicationId,

    -- * ExportJobsResponse
    ExportJobsResponse (..),
    mkExportJobsResponse,
    ejrItem,
    ejrNextToken,

    -- * ChannelType
    ChannelType (..),

    -- * MessageConfiguration
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

    -- * MessageBody
    MessageBody (..),
    mkMessageBody,
    mbMessage,
    mbRequestID,

    -- * TreatmentResource
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

    -- * CampaignsResponse
    CampaignsResponse (..),
    mkCampaignsResponse,
    crItem,
    crNextToken,

    -- * VoiceChannelResponse
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

    -- * EndpointDemographic
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

    -- * JourneyStateRequest
    JourneyStateRequest (..),
    mkJourneyStateRequest,
    jsrState,

    -- * SegmentGroupList
    SegmentGroupList (..),
    mkSegmentGroupList,
    sglGroups,
    sglInclude,

    -- * SMSTemplateResponse
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

    -- * APNSVoipChannelRequest
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

    -- * EmailChannelRequest
    EmailChannelRequest (..),
    mkEmailChannelRequest,
    ecrfFromAddress,
    ecrfIdentity,
    ecrfConfigurationSet,
    ecrfEnabled,
    ecrfRoleArn,

    -- * Session
    Session (..),
    mkSession,
    sfStartTimestamp,
    sfId,
    sfDuration,
    sfStopTimestamp,

    -- * WaitTime
    WaitTime (..),
    mkWaitTime,
    wtWaitFor,
    wtWaitUntil,

    -- * Duration
    Duration (..),

    -- * AddressConfiguration
    AddressConfiguration (..),
    mkAddressConfiguration,
    acBodyOverride,
    acChannelType,
    acContext,
    acRawContent,
    acSubstitutions,
    acTitleOverride,

    -- * EndpointBatchRequest
    EndpointBatchRequest (..),
    mkEndpointBatchRequest,
    ebrItem,

    -- * SendUsersMessageResponse
    SendUsersMessageResponse (..),
    mkSendUsersMessageResponse,
    sumrApplicationId,
    sumrRequestId,
    sumrResult,

    -- * BaseKpiResult
    BaseKpiResult (..),
    mkBaseKpiResult,
    bkrRows,

    -- * DirectMessageConfiguration
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

    -- * BaiduChannelRequest
    BaiduChannelRequest (..),
    mkBaiduChannelRequest,
    bSecretKey,
    bApiKey,
    bEnabled,

    -- * GCMChannelResponse
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

    -- * EndpointMessageResult
    EndpointMessageResult (..),
    mkEndpointMessageResult,
    emrDeliveryStatus,
    emrStatusCode,
    emrAddress,
    emrMessageId,
    emrStatusMessage,
    emrUpdatedToken,

    -- * ImportJobResponse
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

    -- * APNSVoipSandboxChannelResponse
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

    -- * ConditionalSplitActivity
    ConditionalSplitActivity (..),
    mkConditionalSplitActivity,
    csaCondition,
    csaEvaluationWaitTime,
    csaFalseActivity,
    csaTrueActivity,

    -- * ListRecommenderConfigurationsResponse
    ListRecommenderConfigurationsResponse (..),
    mkListRecommenderConfigurationsResponse,
    lrcrItem,
    lrcrNextToken,

    -- * CampaignState
    CampaignState (..),
    mkCampaignState,
    csCampaignStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.ADMChannelRequest
import Network.AWS.Pinpoint.Types.ADMChannelResponse
import Network.AWS.Pinpoint.Types.ADMMessage
import Network.AWS.Pinpoint.Types.APNSChannelRequest
import Network.AWS.Pinpoint.Types.APNSChannelResponse
import Network.AWS.Pinpoint.Types.APNSMessage
import Network.AWS.Pinpoint.Types.APNSPushNotificationTemplate
import Network.AWS.Pinpoint.Types.APNSSandboxChannelRequest
import Network.AWS.Pinpoint.Types.APNSSandboxChannelResponse
import Network.AWS.Pinpoint.Types.APNSVoipChannelRequest
import Network.AWS.Pinpoint.Types.APNSVoipChannelResponse
import Network.AWS.Pinpoint.Types.APNSVoipSandboxChannelRequest
import Network.AWS.Pinpoint.Types.APNSVoipSandboxChannelResponse
import Network.AWS.Pinpoint.Types.Action
import Network.AWS.Pinpoint.Types.ActivitiesResponse
import Network.AWS.Pinpoint.Types.Activity
import Network.AWS.Pinpoint.Types.ActivityResponse
import Network.AWS.Pinpoint.Types.AddressConfiguration
import Network.AWS.Pinpoint.Types.AndroidPushNotificationTemplate
import Network.AWS.Pinpoint.Types.ApplicationDateRangeKpiResponse
import Network.AWS.Pinpoint.Types.ApplicationResponse
import Network.AWS.Pinpoint.Types.ApplicationSettingsResource
import Network.AWS.Pinpoint.Types.ApplicationsResponse
import Network.AWS.Pinpoint.Types.AttributeDimension
import Network.AWS.Pinpoint.Types.AttributeType
import Network.AWS.Pinpoint.Types.AttributesResource
import Network.AWS.Pinpoint.Types.BaiduChannelRequest
import Network.AWS.Pinpoint.Types.BaiduChannelResponse
import Network.AWS.Pinpoint.Types.BaiduMessage
import Network.AWS.Pinpoint.Types.BaseKpiResult
import Network.AWS.Pinpoint.Types.CampaignCustomMessage
import Network.AWS.Pinpoint.Types.CampaignDateRangeKpiResponse
import Network.AWS.Pinpoint.Types.CampaignEmailMessage
import Network.AWS.Pinpoint.Types.CampaignEventFilter
import Network.AWS.Pinpoint.Types.CampaignHook
import Network.AWS.Pinpoint.Types.CampaignLimits
import Network.AWS.Pinpoint.Types.CampaignResponse
import Network.AWS.Pinpoint.Types.CampaignSmsMessage
import Network.AWS.Pinpoint.Types.CampaignState
import Network.AWS.Pinpoint.Types.CampaignStatus
import Network.AWS.Pinpoint.Types.CampaignsResponse
import Network.AWS.Pinpoint.Types.ChannelResponse
import Network.AWS.Pinpoint.Types.ChannelType
import Network.AWS.Pinpoint.Types.ChannelsResponse
import Network.AWS.Pinpoint.Types.Condition
import Network.AWS.Pinpoint.Types.ConditionalSplitActivity
import Network.AWS.Pinpoint.Types.CreateApplicationRequest
import Network.AWS.Pinpoint.Types.CreateTemplateMessageBody
import Network.AWS.Pinpoint.Types.CustomDeliveryConfiguration
import Network.AWS.Pinpoint.Types.CustomMessageActivity
import Network.AWS.Pinpoint.Types.DefaultMessage
import Network.AWS.Pinpoint.Types.DefaultPushNotificationMessage
import Network.AWS.Pinpoint.Types.DefaultPushNotificationTemplate
import Network.AWS.Pinpoint.Types.DefinitionFormat
import Network.AWS.Pinpoint.Types.DeliveryStatus
import Network.AWS.Pinpoint.Types.DimensionType
import Network.AWS.Pinpoint.Types.DirectMessageConfiguration
import Network.AWS.Pinpoint.Types.Duration
import Network.AWS.Pinpoint.Types.EmailChannelRequest
import Network.AWS.Pinpoint.Types.EmailChannelResponse
import Network.AWS.Pinpoint.Types.EmailMessage
import Network.AWS.Pinpoint.Types.EmailMessageActivity
import Network.AWS.Pinpoint.Types.EmailTemplateRequest
import Network.AWS.Pinpoint.Types.EmailTemplateResponse
import Network.AWS.Pinpoint.Types.EndpointBatchItem
import Network.AWS.Pinpoint.Types.EndpointBatchRequest
import Network.AWS.Pinpoint.Types.EndpointDemographic
import Network.AWS.Pinpoint.Types.EndpointItemResponse
import Network.AWS.Pinpoint.Types.EndpointLocation
import Network.AWS.Pinpoint.Types.EndpointMessageResult
import Network.AWS.Pinpoint.Types.EndpointRequest
import Network.AWS.Pinpoint.Types.EndpointResponse
import Network.AWS.Pinpoint.Types.EndpointSendConfiguration
import Network.AWS.Pinpoint.Types.EndpointTypesElement
import Network.AWS.Pinpoint.Types.EndpointUser
import Network.AWS.Pinpoint.Types.EndpointsResponse
import Network.AWS.Pinpoint.Types.Event
import Network.AWS.Pinpoint.Types.EventCondition
import Network.AWS.Pinpoint.Types.EventDimensions
import Network.AWS.Pinpoint.Types.EventFilter
import Network.AWS.Pinpoint.Types.EventItemResponse
import Network.AWS.Pinpoint.Types.EventStartCondition
import Network.AWS.Pinpoint.Types.EventStream
import Network.AWS.Pinpoint.Types.EventsBatch
import Network.AWS.Pinpoint.Types.EventsRequest
import Network.AWS.Pinpoint.Types.EventsResponse
import Network.AWS.Pinpoint.Types.ExportJobRequest
import Network.AWS.Pinpoint.Types.ExportJobResource
import Network.AWS.Pinpoint.Types.ExportJobResponse
import Network.AWS.Pinpoint.Types.ExportJobsResponse
import Network.AWS.Pinpoint.Types.FilterType
import Network.AWS.Pinpoint.Types.Frequency
import Network.AWS.Pinpoint.Types.GCMChannelRequest
import Network.AWS.Pinpoint.Types.GCMChannelResponse
import Network.AWS.Pinpoint.Types.GCMMessage
import Network.AWS.Pinpoint.Types.GPSCoordinates
import Network.AWS.Pinpoint.Types.GPSPointDimension
import Network.AWS.Pinpoint.Types.HoldoutActivity
import Network.AWS.Pinpoint.Types.ImportJobRequest
import Network.AWS.Pinpoint.Types.ImportJobResource
import Network.AWS.Pinpoint.Types.ImportJobResponse
import Network.AWS.Pinpoint.Types.ImportJobsResponse
import Network.AWS.Pinpoint.Types.Include
import Network.AWS.Pinpoint.Types.ItemResponse
import Network.AWS.Pinpoint.Types.JobStatus
import Network.AWS.Pinpoint.Types.JourneyCustomMessage
import Network.AWS.Pinpoint.Types.JourneyDateRangeKpiResponse
import Network.AWS.Pinpoint.Types.JourneyEmailMessage
import Network.AWS.Pinpoint.Types.JourneyExecutionActivityMetricsResponse
import Network.AWS.Pinpoint.Types.JourneyExecutionMetricsResponse
import Network.AWS.Pinpoint.Types.JourneyLimits
import Network.AWS.Pinpoint.Types.JourneyPushMessage
import Network.AWS.Pinpoint.Types.JourneyResponse
import Network.AWS.Pinpoint.Types.JourneySMSMessage
import Network.AWS.Pinpoint.Types.JourneySchedule
import Network.AWS.Pinpoint.Types.JourneyStateRequest
import Network.AWS.Pinpoint.Types.JourneysResponse
import Network.AWS.Pinpoint.Types.ListRecommenderConfigurationsResponse
import Network.AWS.Pinpoint.Types.Message
import Network.AWS.Pinpoint.Types.MessageBody
import Network.AWS.Pinpoint.Types.MessageConfiguration
import Network.AWS.Pinpoint.Types.MessageRequest
import Network.AWS.Pinpoint.Types.MessageResponse
import Network.AWS.Pinpoint.Types.MessageResult
import Network.AWS.Pinpoint.Types.MessageType
import Network.AWS.Pinpoint.Types.MetricDimension
import Network.AWS.Pinpoint.Types.Mode
import Network.AWS.Pinpoint.Types.MultiConditionalBranch
import Network.AWS.Pinpoint.Types.MultiConditionalSplitActivity
import Network.AWS.Pinpoint.Types.NumberValidateRequest
import Network.AWS.Pinpoint.Types.NumberValidateResponse
import Network.AWS.Pinpoint.Types.Operator
import Network.AWS.Pinpoint.Types.PublicEndpoint
import Network.AWS.Pinpoint.Types.PushMessageActivity
import Network.AWS.Pinpoint.Types.PushNotificationTemplateRequest
import Network.AWS.Pinpoint.Types.PushNotificationTemplateResponse
import Network.AWS.Pinpoint.Types.QuietTime
import Network.AWS.Pinpoint.Types.RandomSplitActivity
import Network.AWS.Pinpoint.Types.RandomSplitEntry
import Network.AWS.Pinpoint.Types.RawEmail
import Network.AWS.Pinpoint.Types.RecencyDimension
import Network.AWS.Pinpoint.Types.RecencyType
import Network.AWS.Pinpoint.Types.RecommenderConfigurationResponse
import Network.AWS.Pinpoint.Types.ResultRow
import Network.AWS.Pinpoint.Types.ResultRowValue
import Network.AWS.Pinpoint.Types.SMSChannelRequest
import Network.AWS.Pinpoint.Types.SMSChannelResponse
import Network.AWS.Pinpoint.Types.SMSMessage
import Network.AWS.Pinpoint.Types.SMSMessageActivity
import Network.AWS.Pinpoint.Types.SMSTemplateRequest
import Network.AWS.Pinpoint.Types.SMSTemplateResponse
import Network.AWS.Pinpoint.Types.Schedule
import Network.AWS.Pinpoint.Types.SegmentBehaviors
import Network.AWS.Pinpoint.Types.SegmentCondition
import Network.AWS.Pinpoint.Types.SegmentDemographics
import Network.AWS.Pinpoint.Types.SegmentDimensions
import Network.AWS.Pinpoint.Types.SegmentGroup
import Network.AWS.Pinpoint.Types.SegmentGroupList
import Network.AWS.Pinpoint.Types.SegmentImportResource
import Network.AWS.Pinpoint.Types.SegmentLocation
import Network.AWS.Pinpoint.Types.SegmentReference
import Network.AWS.Pinpoint.Types.SegmentResponse
import Network.AWS.Pinpoint.Types.SegmentType
import Network.AWS.Pinpoint.Types.SegmentsResponse
import Network.AWS.Pinpoint.Types.SendUsersMessageRequest
import Network.AWS.Pinpoint.Types.SendUsersMessageResponse
import Network.AWS.Pinpoint.Types.Session
import Network.AWS.Pinpoint.Types.SetDimension
import Network.AWS.Pinpoint.Types.SimpleCondition
import Network.AWS.Pinpoint.Types.SimpleEmail
import Network.AWS.Pinpoint.Types.SimpleEmailPart
import Network.AWS.Pinpoint.Types.SourceType
import Network.AWS.Pinpoint.Types.StartCondition
import Network.AWS.Pinpoint.Types.State
import Network.AWS.Pinpoint.Types.TagsModel
import Network.AWS.Pinpoint.Types.Template
import Network.AWS.Pinpoint.Types.TemplateActiveVersionRequest
import Network.AWS.Pinpoint.Types.TemplateConfiguration
import Network.AWS.Pinpoint.Types.TemplateResponse
import Network.AWS.Pinpoint.Types.TemplateType
import Network.AWS.Pinpoint.Types.TemplateVersionResponse
import Network.AWS.Pinpoint.Types.TemplateVersionsResponse
import Network.AWS.Pinpoint.Types.TemplatesResponse
import Network.AWS.Pinpoint.Types.TreatmentResource
import Network.AWS.Pinpoint.Types.Type
import Network.AWS.Pinpoint.Types.UpdateAttributesRequest
import Network.AWS.Pinpoint.Types.VoiceChannelRequest
import Network.AWS.Pinpoint.Types.VoiceChannelResponse
import Network.AWS.Pinpoint.Types.VoiceMessage
import Network.AWS.Pinpoint.Types.VoiceTemplateRequest
import Network.AWS.Pinpoint.Types.VoiceTemplateResponse
import Network.AWS.Pinpoint.Types.WaitActivity
import Network.AWS.Pinpoint.Types.WaitTime
import Network.AWS.Pinpoint.Types.WriteApplicationSettingsRequest
import Network.AWS.Pinpoint.Types.WriteCampaignRequest
import Network.AWS.Pinpoint.Types.WriteEventStream
import Network.AWS.Pinpoint.Types.WriteJourneyRequest
import Network.AWS.Pinpoint.Types.WriteSegmentRequest
import Network.AWS.Pinpoint.Types.WriteTreatmentResource
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2016-12-01@ of the Amazon Pinpoint SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig =
  Core.Service
    { Core._svcAbbrev = "Pinpoint",
      Core._svcSigner = Sign.v4,
      Core._svcPrefix = "pinpoint",
      Core._svcVersion = "2016-12-01",
      Core._svcTimeout = Core.Just 70,
      Core._svcCheck = Core.statusSuccess,
      Core._svcRetry = retry,
      Core._svcError = Core.parseJSONError "Pinpoint",
      Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig
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
          (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
      | Lens.has
          (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling_exception"
      | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e =
        Core.Just "throttling"
      | Lens.has
          ( Core.hasCode "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
      | Core.otherwise = Core.Nothing

-- | Provides information about an API request or response.
_PayloadTooLargeException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PayloadTooLargeException =
  Core._MatchServiceError
    mkServiceConfig
    "PayloadTooLargeException"
    Core.. Core.hasStatues 413
{-# DEPRECATED _PayloadTooLargeException "Use generic-lens or generic-optics instead." #-}

-- | Provides information about an API request or response.
_ConflictException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError mkServiceConfig "ConflictException"
    Core.. Core.hasStatues 409
{-# DEPRECATED _ConflictException "Use generic-lens or generic-optics instead." #-}

-- | Provides information about an API request or response.
_ForbiddenException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ForbiddenException =
  Core._MatchServiceError mkServiceConfig "ForbiddenException"
    Core.. Core.hasStatues 403
{-# DEPRECATED _ForbiddenException "Use generic-lens or generic-optics instead." #-}

-- | Provides information about an API request or response.
_NotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError mkServiceConfig "NotFoundException"
    Core.. Core.hasStatues 404
{-# DEPRECATED _NotFoundException "Use generic-lens or generic-optics instead." #-}

-- | Provides information about an API request or response.
_TooManyRequestsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyRequestsException =
  Core._MatchServiceError
    mkServiceConfig
    "TooManyRequestsException"
    Core.. Core.hasStatues 429
{-# DEPRECATED _TooManyRequestsException "Use generic-lens or generic-optics instead." #-}

-- | Provides information about an API request or response.
_InternalServerErrorException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalServerErrorException =
  Core._MatchServiceError
    mkServiceConfig
    "InternalServerErrorException"
    Core.. Core.hasStatues 500
{-# DEPRECATED _InternalServerErrorException "Use generic-lens or generic-optics instead." #-}

-- | Provides information about an API request or response.
_MethodNotAllowedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MethodNotAllowedException =
  Core._MatchServiceError
    mkServiceConfig
    "MethodNotAllowedException"
    Core.. Core.hasStatues 405
{-# DEPRECATED _MethodNotAllowedException "Use generic-lens or generic-optics instead." #-}

-- | Provides information about an API request or response.
_BadRequestException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError mkServiceConfig "BadRequestException"
    Core.. Core.hasStatues 400
{-# DEPRECATED _BadRequestException "Use generic-lens or generic-optics instead." #-}
