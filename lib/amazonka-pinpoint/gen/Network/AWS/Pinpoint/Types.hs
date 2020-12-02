{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types
  ( -- * Service Configuration
    pinpoint,

    -- * Errors

    -- * Action
    Action (..),

    -- * AttributeType
    AttributeType (..),

    -- * CampaignStatus
    CampaignStatus (..),

    -- * ChannelType
    ChannelType (..),

    -- * DefinitionFormat
    DefinitionFormat (..),

    -- * DeliveryStatus
    DeliveryStatus (..),

    -- * DimensionType
    DimensionType (..),

    -- * Duration
    Duration (..),

    -- * EndpointTypesElement
    EndpointTypesElement (..),

    -- * FilterType
    FilterType (..),

    -- * Frequency
    Frequency (..),

    -- * Include
    Include (..),

    -- * JobStatus
    JobStatus (..),

    -- * MessageType
    MessageType (..),

    -- * Mode
    Mode (..),

    -- * Operator
    Operator (..),

    -- * RecencyType
    RecencyType (..),

    -- * SegmentType
    SegmentType (..),

    -- * SourceType
    SourceType (..),

    -- * State
    State (..),

    -- * TemplateType
    TemplateType (..),

    -- * Type
    Type (..),

    -- * ADMChannelRequest
    ADMChannelRequest,
    aDMChannelRequest,
    admcrEnabled,
    admcrClientSecret,
    admcrClientId,

    -- * ADMChannelResponse
    ADMChannelResponse,
    aDMChannelResponse,
    admcLastModifiedDate,
    admcEnabled,
    admcIsArchived,
    admcApplicationId,
    admcVersion,
    admcId,
    admcCreationDate,
    admcLastModifiedBy,
    admcHasCredential,
    admcPlatform,

    -- * ADMMessage
    ADMMessage,
    aDMMessage,
    admmSubstitutions,
    admmExpiresAfter,
    admmMD5,
    admmSilentPush,
    admmImageIconURL,
    admmRawContent,
    admmData,
    admmSmallImageIconURL,
    admmBody,
    admmURL,
    admmSound,
    admmAction,
    admmImageURL,
    admmConsolidationKey,
    admmTitle,
    admmIconReference,

    -- * APNSChannelRequest
    APNSChannelRequest,
    apnsChannelRequest,
    acrTokenKey,
    acrPrivateKey,
    acrEnabled,
    acrTeamId,
    acrBundleId,
    acrDefaultAuthenticationMethod,
    acrCertificate,
    acrTokenKeyId,

    -- * APNSChannelResponse
    APNSChannelResponse,
    apnsChannelResponse,
    acLastModifiedDate,
    acEnabled,
    acHasTokenKey,
    acDefaultAuthenticationMethod,
    acIsArchived,
    acApplicationId,
    acVersion,
    acId,
    acCreationDate,
    acLastModifiedBy,
    acHasCredential,
    acPlatform,

    -- * APNSMessage
    APNSMessage,
    apnsMessage,
    amSubstitutions,
    amSilentPush,
    amAPNSPushType,
    amPriority,
    amRawContent,
    amData,
    amBody,
    amCategory,
    amTimeToLive,
    amURL,
    amSound,
    amAction,
    amMediaURL,
    amPreferredAuthenticationMethod,
    amBadge,
    amTitle,
    amThreadId,
    amCollapseId,

    -- * APNSPushNotificationTemplate
    APNSPushNotificationTemplate,
    apnsPushNotificationTemplate,
    apntRawContent,
    apntBody,
    apntURL,
    apntSound,
    apntAction,
    apntMediaURL,
    apntTitle,

    -- * APNSSandboxChannelRequest
    APNSSandboxChannelRequest,
    apnsSandboxChannelRequest,
    ascrTokenKey,
    ascrPrivateKey,
    ascrEnabled,
    ascrTeamId,
    ascrBundleId,
    ascrDefaultAuthenticationMethod,
    ascrCertificate,
    ascrTokenKeyId,

    -- * APNSSandboxChannelResponse
    APNSSandboxChannelResponse,
    apnsSandboxChannelResponse,
    ascLastModifiedDate,
    ascEnabled,
    ascHasTokenKey,
    ascDefaultAuthenticationMethod,
    ascIsArchived,
    ascApplicationId,
    ascVersion,
    ascId,
    ascCreationDate,
    ascLastModifiedBy,
    ascHasCredential,
    ascPlatform,

    -- * APNSVoipChannelRequest
    APNSVoipChannelRequest,
    apnsVoipChannelRequest,
    avcrTokenKey,
    avcrPrivateKey,
    avcrEnabled,
    avcrTeamId,
    avcrBundleId,
    avcrDefaultAuthenticationMethod,
    avcrCertificate,
    avcrTokenKeyId,

    -- * APNSVoipChannelResponse
    APNSVoipChannelResponse,
    apnsVoipChannelResponse,
    avcLastModifiedDate,
    avcEnabled,
    avcHasTokenKey,
    avcDefaultAuthenticationMethod,
    avcIsArchived,
    avcApplicationId,
    avcVersion,
    avcId,
    avcCreationDate,
    avcLastModifiedBy,
    avcHasCredential,
    avcPlatform,

    -- * APNSVoipSandboxChannelRequest
    APNSVoipSandboxChannelRequest,
    apnsVoipSandboxChannelRequest,
    avscrTokenKey,
    avscrPrivateKey,
    avscrEnabled,
    avscrTeamId,
    avscrBundleId,
    avscrDefaultAuthenticationMethod,
    avscrCertificate,
    avscrTokenKeyId,

    -- * APNSVoipSandboxChannelResponse
    APNSVoipSandboxChannelResponse,
    apnsVoipSandboxChannelResponse,
    avscLastModifiedDate,
    avscEnabled,
    avscHasTokenKey,
    avscDefaultAuthenticationMethod,
    avscIsArchived,
    avscApplicationId,
    avscVersion,
    avscId,
    avscCreationDate,
    avscLastModifiedBy,
    avscHasCredential,
    avscPlatform,

    -- * ActivitiesResponse
    ActivitiesResponse,
    activitiesResponse,
    aNextToken,
    aItem,

    -- * Activity
    Activity,
    activity,
    aConditionalSplit,
    aEMAIL,
    aMultiCondition,
    aCUSTOM,
    aWait,
    aRandomSplit,
    aHoldout,
    aSMS,
    aPUSH,
    aDescription,

    -- * ActivityResponse
    ActivityResponse,
    activityResponse,
    aState,
    aStart,
    aTimezonesCompletedCount,
    aTimezonesTotalCount,
    aResult,
    aTreatmentId,
    aSuccessfulEndpointCount,
    aEnd,
    aTotalEndpointCount,
    aScheduledStart,
    aCampaignId,
    aId,
    aApplicationId,

    -- * AddressConfiguration
    AddressConfiguration,
    addressConfiguration,
    acSubstitutions,
    acTitleOverride,
    acContext,
    acRawContent,
    acBodyOverride,
    acChannelType,

    -- * AndroidPushNotificationTemplate
    AndroidPushNotificationTemplate,
    androidPushNotificationTemplate,
    aImageIconURL,
    aRawContent,
    aSmallImageIconURL,
    aBody,
    aURL,
    aSound,
    aAction,
    aImageURL,
    aTitle,

    -- * ApplicationDateRangeKpiResponse
    ApplicationDateRangeKpiResponse,
    applicationDateRangeKpiResponse,
    adrkNextToken,
    adrkKpiResult,
    adrkKpiName,
    adrkEndTime,
    adrkStartTime,
    adrkApplicationId,

    -- * ApplicationResponse
    ApplicationResponse,
    applicationResponse,
    appTags,
    appId,
    appARN,
    appName,

    -- * ApplicationSettingsResource
    ApplicationSettingsResource,
    applicationSettingsResource,
    asrLastModifiedDate,
    asrLimits,
    asrQuietTime,
    asrCampaignHook,
    asrApplicationId,

    -- * ApplicationsResponse
    ApplicationsResponse,
    applicationsResponse,
    appNextToken,
    appItem,

    -- * AttributeDimension
    AttributeDimension,
    attributeDimension,
    adAttributeType,
    adValues,

    -- * AttributesResource
    AttributesResource,
    attributesResource,
    arAttributes,
    arAttributeType,
    arApplicationId,

    -- * BaiduChannelRequest
    BaiduChannelRequest,
    baiduChannelRequest,
    bcrEnabled,
    bcrSecretKey,
    bcrAPIKey,

    -- * BaiduChannelResponse
    BaiduChannelResponse,
    baiduChannelResponse,
    bcLastModifiedDate,
    bcEnabled,
    bcIsArchived,
    bcApplicationId,
    bcVersion,
    bcId,
    bcCreationDate,
    bcLastModifiedBy,
    bcHasCredential,
    bcCredential,
    bcPlatform,

    -- * BaiduMessage
    BaiduMessage,
    baiduMessage,
    bmSubstitutions,
    bmSilentPush,
    bmImageIconURL,
    bmRawContent,
    bmData,
    bmSmallImageIconURL,
    bmBody,
    bmTimeToLive,
    bmURL,
    bmSound,
    bmAction,
    bmImageURL,
    bmTitle,
    bmIconReference,

    -- * BaseKpiResult
    BaseKpiResult,
    baseKpiResult,
    bkrRows,

    -- * CampaignCustomMessage
    CampaignCustomMessage,
    campaignCustomMessage,
    ccmData,

    -- * CampaignDateRangeKpiResponse
    CampaignDateRangeKpiResponse,
    campaignDateRangeKpiResponse,
    cdrkNextToken,
    cdrkKpiResult,
    cdrkKpiName,
    cdrkEndTime,
    cdrkCampaignId,
    cdrkStartTime,
    cdrkApplicationId,

    -- * CampaignEmailMessage
    CampaignEmailMessage,
    campaignEmailMessage,
    cemBody,
    cemFromAddress,
    cemHTMLBody,
    cemTitle,

    -- * CampaignEventFilter
    CampaignEventFilter,
    campaignEventFilter,
    cefFilterType,
    cefDimensions,

    -- * CampaignHook
    CampaignHook,
    campaignHook,
    chLambdaFunctionName,
    chMode,
    chWebURL,

    -- * CampaignLimits
    CampaignLimits,
    campaignLimits,
    clMessagesPerSecond,
    clDaily,
    clTotal,
    clMaximumDuration,

    -- * CampaignResponse
    CampaignResponse,
    campaignResponse,
    cCustomDeliveryConfiguration,
    cState,
    cSchedule,
    cTemplateConfiguration,
    cHook,
    cTreatmentName,
    cLimits,
    cIsPaused,
    cDefaultState,
    cName,
    cVersion,
    cHoldoutPercent,
    cTreatmentDescription,
    cMessageConfiguration,
    cDescription,
    cAdditionalTreatments,
    cTags,
    cLastModifiedDate,
    cCreationDate,
    cSegmentId,
    cSegmentVersion,
    cId,
    cARN,
    cApplicationId,

    -- * CampaignSmsMessage
    CampaignSmsMessage,
    campaignSmsMessage,
    csmBody,
    csmMessageType,
    csmSenderId,

    -- * CampaignState
    CampaignState,
    campaignState,
    csCampaignStatus,

    -- * CampaignsResponse
    CampaignsResponse,
    campaignsResponse,
    cNextToken,
    cItem,

    -- * ChannelResponse
    ChannelResponse,
    channelResponse,
    chaLastModifiedDate,
    chaEnabled,
    chaIsArchived,
    chaApplicationId,
    chaVersion,
    chaId,
    chaCreationDate,
    chaLastModifiedBy,
    chaHasCredential,

    -- * ChannelsResponse
    ChannelsResponse,
    channelsResponse,
    cChannels,

    -- * Condition
    Condition,
    condition,
    cOperator,
    cConditions,

    -- * ConditionalSplitActivity
    ConditionalSplitActivity,
    conditionalSplitActivity,
    csaEvaluationWaitTime,
    csaTrueActivity,
    csaFalseActivity,
    csaCondition,

    -- * CreateApplicationRequest
    CreateApplicationRequest,
    createApplicationRequest,
    carTags,
    carName,

    -- * CreateTemplateMessageBody
    CreateTemplateMessageBody,
    createTemplateMessageBody,
    ctmbRequestId,
    ctmbARN,
    ctmbMessage,

    -- * CustomDeliveryConfiguration
    CustomDeliveryConfiguration,
    customDeliveryConfiguration,
    cdcEndpointTypes,
    cdcDeliveryURI,

    -- * CustomMessageActivity
    CustomMessageActivity,
    customMessageActivity,
    cmaTemplateName,
    cmaTemplateVersion,
    cmaEndpointTypes,
    cmaNextActivity,
    cmaDeliveryURI,
    cmaMessageConfig,

    -- * DefaultMessage
    DefaultMessage,
    defaultMessage,
    dmSubstitutions,
    dmBody,

    -- * DefaultPushNotificationMessage
    DefaultPushNotificationMessage,
    defaultPushNotificationMessage,
    dpnmSubstitutions,
    dpnmSilentPush,
    dpnmData,
    dpnmBody,
    dpnmURL,
    dpnmAction,
    dpnmTitle,

    -- * DefaultPushNotificationTemplate
    DefaultPushNotificationTemplate,
    defaultPushNotificationTemplate,
    dpntBody,
    dpntURL,
    dpntSound,
    dpntAction,
    dpntTitle,

    -- * DirectMessageConfiguration
    DirectMessageConfiguration,
    directMessageConfiguration,
    dmcAPNSMessage,
    dmcGCMMessage,
    dmcDefaultMessage,
    dmcADMMessage,
    dmcSMSMessage,
    dmcEmailMessage,
    dmcVoiceMessage,
    dmcBaiduMessage,
    dmcDefaultPushNotificationMessage,

    -- * EmailChannelRequest
    EmailChannelRequest,
    emailChannelRequest,
    ecrEnabled,
    ecrConfigurationSet,
    ecrRoleARN,
    ecrFromAddress,
    ecrIdentity,

    -- * EmailChannelResponse
    EmailChannelResponse,
    emailChannelResponse,
    ecMessagesPerSecond,
    ecLastModifiedDate,
    ecEnabled,
    ecFromAddress,
    ecIsArchived,
    ecApplicationId,
    ecVersion,
    ecConfigurationSet,
    ecId,
    ecCreationDate,
    ecLastModifiedBy,
    ecIdentity,
    ecHasCredential,
    ecRoleARN,
    ecPlatform,

    -- * EmailMessage
    EmailMessage,
    emailMessage,
    emSubstitutions,
    emBody,
    emFromAddress,
    emRawEmail,
    emFeedbackForwardingAddress,
    emSimpleEmail,
    emReplyToAddresses,

    -- * EmailMessageActivity
    EmailMessageActivity,
    emailMessageActivity,
    emaTemplateName,
    emaTemplateVersion,
    emaNextActivity,
    emaMessageConfig,

    -- * EmailTemplateRequest
    EmailTemplateRequest,
    emailTemplateRequest,
    etrSubject,
    etrTextPart,
    etrTemplateDescription,
    etrDefaultSubstitutions,
    etrHTMLPart,
    etrRecommenderId,
    etrTags,

    -- * EmailTemplateResponse
    EmailTemplateResponse,
    emailTemplateResponse,
    etSubject,
    etTextPart,
    etARN,
    etTemplateDescription,
    etDefaultSubstitutions,
    etVersion,
    etHTMLPart,
    etRecommenderId,
    etTags,
    etLastModifiedDate,
    etCreationDate,
    etTemplateName,
    etTemplateType,

    -- * EndpointBatchItem
    EndpointBatchItem,
    endpointBatchItem,
    ebiRequestId,
    ebiMetrics,
    ebiLocation,
    ebiDemographic,
    ebiAddress,
    ebiEffectiveDate,
    ebiUser,
    ebiAttributes,
    ebiEndpointStatus,
    ebiOptOut,
    ebiId,
    ebiChannelType,

    -- * EndpointBatchRequest
    EndpointBatchRequest,
    endpointBatchRequest,
    ebrItem,

    -- * EndpointDemographic
    EndpointDemographic,
    endpointDemographic,
    edPlatform,
    edPlatformVersion,
    edLocale,
    edAppVersion,
    edModel,
    edMake,
    edModelVersion,
    edTimezone,

    -- * EndpointItemResponse
    EndpointItemResponse,
    endpointItemResponse,
    eiMessage,
    eiStatusCode,

    -- * EndpointLocation
    EndpointLocation,
    endpointLocation,
    elPostalCode,
    elLatitude,
    elCountry,
    elCity,
    elRegion,
    elLongitude,

    -- * EndpointMessageResult
    EndpointMessageResult,
    endpointMessageResult,
    emrAddress,
    emrStatusMessage,
    emrUpdatedToken,
    emrMessageId,
    emrDeliveryStatus,
    emrStatusCode,

    -- * EndpointRequest
    EndpointRequest,
    endpointRequest,
    erRequestId,
    erMetrics,
    erLocation,
    erDemographic,
    erAddress,
    erEffectiveDate,
    erUser,
    erAttributes,
    erEndpointStatus,
    erOptOut,
    erChannelType,

    -- * EndpointResponse
    EndpointResponse,
    endpointResponse,
    endRequestId,
    endMetrics,
    endLocation,
    endDemographic,
    endCohortId,
    endAddress,
    endEffectiveDate,
    endUser,
    endApplicationId,
    endAttributes,
    endEndpointStatus,
    endOptOut,
    endId,
    endCreationDate,
    endChannelType,

    -- * EndpointSendConfiguration
    EndpointSendConfiguration,
    endpointSendConfiguration,
    escSubstitutions,
    escTitleOverride,
    escContext,
    escRawContent,
    escBodyOverride,

    -- * EndpointUser
    EndpointUser,
    endpointUser,
    euUserAttributes,
    euUserId,

    -- * EndpointsResponse
    EndpointsResponse,
    endpointsResponse,
    eItem,

    -- * Event
    Event,
    event,
    eClientSDKVersion,
    eMetrics,
    eAppVersionCode,
    eAppTitle,
    eAppPackageName,
    eAttributes,
    eSDKName,
    eSession,
    eEventType,
    eTimestamp,

    -- * EventCondition
    EventCondition,
    eventCondition,
    ecDimensions,
    ecMessageActivity,

    -- * EventDimensions
    EventDimensions,
    eventDimensions,
    edMetrics,
    edEventType,
    edAttributes,

    -- * EventFilter
    EventFilter,
    eventFilter,
    efFilterType,
    efDimensions,

    -- * EventItemResponse
    EventItemResponse,
    eventItemResponse,
    eMessage,
    eStatusCode,

    -- * EventStartCondition
    EventStartCondition,
    eventStartCondition,
    escEventFilter,
    escSegmentId,

    -- * EventStream
    EventStream,
    eventStream,
    esLastUpdatedBy,
    esLastModifiedDate,
    esExternalId,
    esApplicationId,
    esRoleARN,
    esDestinationStreamARN,

    -- * EventsBatch
    EventsBatch,
    eventsBatch,
    ebEndpoint,
    ebEvents,

    -- * EventsRequest
    EventsRequest,
    eventsRequest,
    erBatchItem,

    -- * EventsResponse
    EventsResponse,
    eventsResponse,
    eResults,

    -- * ExportJobRequest
    ExportJobRequest,
    exportJobRequest,
    eSegmentId,
    eSegmentVersion,
    eS3URLPrefix,
    eRoleARN,

    -- * ExportJobResource
    ExportJobResource,
    exportJobResource,
    ejrSegmentId,
    ejrSegmentVersion,
    ejrS3URLPrefix,
    ejrRoleARN,

    -- * ExportJobResponse
    ExportJobResponse,
    exportJobResponse,
    ejCompletedPieces,
    ejFailedPieces,
    ejTotalProcessed,
    ejFailures,
    ejTotalPieces,
    ejCompletionDate,
    ejTotalFailures,
    ejJobStatus,
    ejCreationDate,
    ejType,
    ejDefinition,
    ejId,
    ejApplicationId,

    -- * ExportJobsResponse
    ExportJobsResponse,
    exportJobsResponse,
    ejNextToken,
    ejItem,

    -- * GCMChannelRequest
    GCMChannelRequest,
    gcmChannelRequest,
    gcrEnabled,
    gcrAPIKey,

    -- * GCMChannelResponse
    GCMChannelResponse,
    gcmChannelResponse,
    gcLastModifiedDate,
    gcEnabled,
    gcIsArchived,
    gcApplicationId,
    gcVersion,
    gcId,
    gcCreationDate,
    gcLastModifiedBy,
    gcHasCredential,
    gcCredential,
    gcPlatform,

    -- * GCMMessage
    GCMMessage,
    gcmMessage,
    gmSubstitutions,
    gmSilentPush,
    gmImageIconURL,
    gmPriority,
    gmRawContent,
    gmData,
    gmRestrictedPackageName,
    gmSmallImageIconURL,
    gmBody,
    gmTimeToLive,
    gmURL,
    gmSound,
    gmAction,
    gmCollapseKey,
    gmImageURL,
    gmTitle,
    gmIconReference,

    -- * GPSCoordinates
    GPSCoordinates,
    gPSCoordinates,
    gpscLatitude,
    gpscLongitude,

    -- * GPSPointDimension
    GPSPointDimension,
    gPSPointDimension,
    gpspdRangeInKilometers,
    gpspdCoordinates,

    -- * HoldoutActivity
    HoldoutActivity,
    holdoutActivity,
    haNextActivity,
    haPercentage,

    -- * ImportJobRequest
    ImportJobRequest,
    importJobRequest,
    iSegmentName,
    iDefineSegment,
    iRegisterEndpoints,
    iExternalId,
    iSegmentId,
    iFormat,
    iS3URL,
    iRoleARN,

    -- * ImportJobResource
    ImportJobResource,
    importJobResource,
    ijrSegmentName,
    ijrDefineSegment,
    ijrRegisterEndpoints,
    ijrExternalId,
    ijrSegmentId,
    ijrFormat,
    ijrS3URL,
    ijrRoleARN,

    -- * ImportJobResponse
    ImportJobResponse,
    importJobResponse,
    ijCompletedPieces,
    ijFailedPieces,
    ijTotalProcessed,
    ijFailures,
    ijTotalPieces,
    ijCompletionDate,
    ijTotalFailures,
    ijJobStatus,
    ijCreationDate,
    ijType,
    ijDefinition,
    ijId,
    ijApplicationId,

    -- * ImportJobsResponse
    ImportJobsResponse,
    importJobsResponse,
    ijNextToken,
    ijItem,

    -- * ItemResponse
    ItemResponse,
    itemResponse,
    iEndpointItemResponse,
    iEventsItemResponse,

    -- * JourneyCustomMessage
    JourneyCustomMessage,
    journeyCustomMessage,
    jcmData,

    -- * JourneyDateRangeKpiResponse
    JourneyDateRangeKpiResponse,
    journeyDateRangeKpiResponse,
    jdrkNextToken,
    jdrkKpiResult,
    jdrkKpiName,
    jdrkJourneyId,
    jdrkEndTime,
    jdrkStartTime,
    jdrkApplicationId,

    -- * JourneyEmailMessage
    JourneyEmailMessage,
    journeyEmailMessage,
    jemFromAddress,

    -- * JourneyExecutionActivityMetricsResponse
    JourneyExecutionActivityMetricsResponse,
    journeyExecutionActivityMetricsResponse,
    jeamMetrics,
    jeamJourneyId,
    jeamLastEvaluatedTime,
    jeamJourneyActivityId,
    jeamActivityType,
    jeamApplicationId,

    -- * JourneyExecutionMetricsResponse
    JourneyExecutionMetricsResponse,
    journeyExecutionMetricsResponse,
    jemMetrics,
    jemJourneyId,
    jemLastEvaluatedTime,
    jemApplicationId,

    -- * JourneyLimits
    JourneyLimits,
    journeyLimits,
    jlMessagesPerSecond,
    jlEndpointReentryCap,
    jlDailyCap,

    -- * JourneyPushMessage
    JourneyPushMessage,
    journeyPushMessage,
    jpmTimeToLive,

    -- * JourneyResponse
    JourneyResponse,
    journeyResponse,
    jState,
    jLastModifiedDate,
    jSchedule,
    jLocalTime,
    jActivities,
    jLimits,
    jQuietTime,
    jStartActivity,
    jCreationDate,
    jStartCondition,
    jRefreshFrequency,
    jTags,
    jName,
    jId,
    jApplicationId,

    -- * JourneySMSMessage
    JourneySMSMessage,
    journeySMSMessage,
    jsmsmMessageType,
    jsmsmSenderId,

    -- * JourneySchedule
    JourneySchedule,
    journeySchedule,
    jsStartTime,
    jsEndTime,
    jsTimezone,

    -- * JourneyStateRequest
    JourneyStateRequest,
    journeyStateRequest,
    jsrState,

    -- * JourneysResponse
    JourneysResponse,
    journeysResponse,
    jNextToken,
    jItem,

    -- * ListRecommenderConfigurationsResponse
    ListRecommenderConfigurationsResponse,
    listRecommenderConfigurationsResponse,
    lrcNextToken,
    lrcItem,

    -- * Message
    Message,
    message,
    mSilentPush,
    mImageIconURL,
    mRawContent,
    mBody,
    mTimeToLive,
    mImageSmallIconURL,
    mJSONBody,
    mURL,
    mAction,
    mImageURL,
    mMediaURL,
    mTitle,

    -- * MessageBody
    MessageBody,
    messageBody,
    mbRequestId,
    mbMessage,

    -- * MessageConfiguration
    MessageConfiguration,
    messageConfiguration,
    mcAPNSMessage,
    mcGCMMessage,
    mcDefaultMessage,
    mcCustomMessage,
    mcADMMessage,
    mcSMSMessage,
    mcEmailMessage,
    mcBaiduMessage,

    -- * MessageRequest
    MessageRequest,
    messageRequest,
    mrTraceId,
    mrContext,
    mrAddresses,
    mrTemplateConfiguration,
    mrEndpoints,
    mrMessageConfiguration,

    -- * MessageResponse
    MessageResponse,
    messageResponse,
    mRequestId,
    mResult,
    mEndpointResult,
    mApplicationId,

    -- * MessageResult
    MessageResult,
    messageResult,
    mrStatusMessage,
    mrUpdatedToken,
    mrMessageId,
    mrDeliveryStatus,
    mrStatusCode,

    -- * MetricDimension
    MetricDimension,
    metricDimension,
    mdComparisonOperator,
    mdValue,

    -- * MultiConditionalBranch
    MultiConditionalBranch,
    multiConditionalBranch,
    mcbNextActivity,
    mcbCondition,

    -- * MultiConditionalSplitActivity
    MultiConditionalSplitActivity,
    multiConditionalSplitActivity,
    mcsaBranches,
    mcsaEvaluationWaitTime,
    mcsaDefaultActivity,

    -- * NumberValidateRequest
    NumberValidateRequest,
    numberValidateRequest,
    nvrIsoCountryCode,
    nvrPhoneNumber,

    -- * NumberValidateResponse
    NumberValidateResponse,
    numberValidateResponse,
    nvCarrier,
    nvCounty,
    nvCountry,
    nvCountryCodeNumeric,
    nvZipCode,
    nvOriginalPhoneNumber,
    nvPhoneTypeCode,
    nvPhoneType,
    nvCity,
    nvCountryCodeIso2,
    nvTimezone,
    nvOriginalCountryCodeIso2,
    nvCleansedPhoneNumberNational,
    nvCleansedPhoneNumberE164,

    -- * PublicEndpoint
    PublicEndpoint,
    publicEndpoint,
    peRequestId,
    peMetrics,
    peLocation,
    peDemographic,
    peAddress,
    peEffectiveDate,
    peUser,
    peAttributes,
    peEndpointStatus,
    peOptOut,
    peChannelType,

    -- * PushMessageActivity
    PushMessageActivity,
    pushMessageActivity,
    pmaTemplateName,
    pmaTemplateVersion,
    pmaNextActivity,
    pmaMessageConfig,

    -- * PushNotificationTemplateRequest
    PushNotificationTemplateRequest,
    pushNotificationTemplateRequest,
    pntrDefault,
    pntrTemplateDescription,
    pntrGCM,
    pntrAPNS,
    pntrDefaultSubstitutions,
    pntrADM,
    pntrBaidu,
    pntrRecommenderId,
    pntrTags,

    -- * PushNotificationTemplateResponse
    PushNotificationTemplateResponse,
    pushNotificationTemplateResponse,
    pntARN,
    pntDefault,
    pntTemplateDescription,
    pntGCM,
    pntAPNS,
    pntDefaultSubstitutions,
    pntVersion,
    pntADM,
    pntBaidu,
    pntRecommenderId,
    pntTags,
    pntLastModifiedDate,
    pntCreationDate,
    pntTemplateType,
    pntTemplateName,

    -- * QuietTime
    QuietTime,
    quietTime,
    qtStart,
    qtEnd,

    -- * RandomSplitActivity
    RandomSplitActivity,
    randomSplitActivity,
    rsaBranches,

    -- * RandomSplitEntry
    RandomSplitEntry,
    randomSplitEntry,
    rseNextActivity,
    rsePercentage,

    -- * RawEmail
    RawEmail,
    rawEmail,
    reData,

    -- * RecencyDimension
    RecencyDimension,
    recencyDimension,
    rdDuration,
    rdRecencyType,

    -- * RecommenderConfigurationResponse
    RecommenderConfigurationResponse,
    recommenderConfigurationResponse,
    rcRecommendationTransformerURI,
    rcRecommendationsDisplayName,
    rcRecommendationProviderIdType,
    rcAttributes,
    rcName,
    rcDescription,
    rcRecommendationsPerMessage,
    rcRecommendationProviderURI,
    rcLastModifiedDate,
    rcCreationDate,
    rcRecommendationProviderRoleARN,
    rcId,

    -- * ResultRow
    ResultRow,
    resultRow,
    rrGroupedBys,
    rrValues,

    -- * ResultRowValue
    ResultRowValue,
    resultRowValue,
    rrvType,
    rrvValue,
    rrvKey,

    -- * SMSChannelRequest
    SMSChannelRequest,
    sMSChannelRequest,
    smscrShortCode,
    smscrEnabled,
    smscrSenderId,

    -- * SMSChannelResponse
    SMSChannelResponse,
    sMSChannelResponse,
    smscShortCode,
    smscLastModifiedDate,
    smscEnabled,
    smscSenderId,
    smscTransactionalMessagesPerSecond,
    smscPromotionalMessagesPerSecond,
    smscIsArchived,
    smscApplicationId,
    smscVersion,
    smscId,
    smscCreationDate,
    smscLastModifiedBy,
    smscHasCredential,
    smscPlatform,

    -- * SMSMessage
    SMSMessage,
    sMSMessage,
    smsmSubstitutions,
    smsmOriginationNumber,
    smsmBody,
    smsmMessageType,
    smsmSenderId,
    smsmMediaURL,
    smsmKeyword,

    -- * SMSMessageActivity
    SMSMessageActivity,
    sMSMessageActivity,
    smsmaTemplateName,
    smsmaTemplateVersion,
    smsmaNextActivity,
    smsmaMessageConfig,

    -- * SMSTemplateRequest
    SMSTemplateRequest,
    sMSTemplateRequest,
    smstrBody,
    smstrTemplateDescription,
    smstrDefaultSubstitutions,
    smstrRecommenderId,
    smstrTags,

    -- * SMSTemplateResponse
    SMSTemplateResponse,
    sMSTemplateResponse,
    smstARN,
    smstBody,
    smstTemplateDescription,
    smstDefaultSubstitutions,
    smstVersion,
    smstRecommenderId,
    smstTags,
    smstLastModifiedDate,
    smstCreationDate,
    smstTemplateName,
    smstTemplateType,

    -- * Schedule
    Schedule,
    schedule,
    sFrequency,
    sQuietTime,
    sEventFilter,
    sIsLocalTime,
    sEndTime,
    sTimezone,
    sStartTime,

    -- * SegmentBehaviors
    SegmentBehaviors,
    segmentBehaviors,
    sbRecency,

    -- * SegmentCondition
    SegmentCondition,
    segmentCondition,
    scSegmentId,

    -- * SegmentDemographics
    SegmentDemographics,
    segmentDemographics,
    sdPlatform,
    sdAppVersion,
    sdChannel,
    sdModel,
    sdMake,
    sdDeviceType,

    -- * SegmentDimensions
    SegmentDimensions,
    segmentDimensions,
    sdMetrics,
    sdLocation,
    sdDemographic,
    sdUserAttributes,
    sdBehavior,
    sdAttributes,

    -- * SegmentGroup
    SegmentGroup,
    segmentGroup,
    sgSourceSegments,
    sgSourceType,
    sgType,
    sgDimensions,

    -- * SegmentGroupList
    SegmentGroupList,
    segmentGroupList,
    sglInclude,
    sglGroups,

    -- * SegmentImportResource
    SegmentImportResource,
    segmentImportResource,
    sirChannelCounts,
    sirFormat,
    sirS3URL,
    sirSize,
    sirExternalId,
    sirRoleARN,

    -- * SegmentLocation
    SegmentLocation,
    segmentLocation,
    slCountry,
    slGPSPoint,

    -- * SegmentReference
    SegmentReference,
    segmentReference,
    srVersion,
    srId,

    -- * SegmentResponse
    SegmentResponse,
    segmentResponse,
    sLastModifiedDate,
    sSegmentGroups,
    sName,
    sVersion,
    sImportDefinition,
    sDimensions,
    sTags,
    sSegmentType,
    sCreationDate,
    sId,
    sARN,
    sApplicationId,

    -- * SegmentsResponse
    SegmentsResponse,
    segmentsResponse,
    sNextToken,
    sItem,

    -- * SendUsersMessageRequest
    SendUsersMessageRequest,
    sendUsersMessageRequest,
    sumrTraceId,
    sumrContext,
    sumrTemplateConfiguration,
    sumrMessageConfiguration,
    sumrUsers,

    -- * SendUsersMessageResponse
    SendUsersMessageResponse,
    sendUsersMessageResponse,
    sumRequestId,
    sumResult,
    sumApplicationId,

    -- * Session
    Session,
    session,
    sesStopTimestamp,
    sesDuration,
    sesStartTimestamp,
    sesId,

    -- * SetDimension
    SetDimension,
    setDimension,
    sdDimensionType,
    sdValues,

    -- * SimpleCondition
    SimpleCondition,
    simpleCondition,
    scSegmentDimensions,
    scEventCondition,
    scSegmentCondition,

    -- * SimpleEmail
    SimpleEmail,
    simpleEmail,
    seSubject,
    seTextPart,
    seHTMLPart,

    -- * SimpleEmailPart
    SimpleEmailPart,
    simpleEmailPart,
    sepData,
    sepCharset,

    -- * StartCondition
    StartCondition,
    startCondition,
    scSegmentStartCondition,
    scEventStartCondition,
    scDescription,

    -- * TagsModel
    TagsModel,
    tagsModel,
    tmTags,

    -- * Template
    Template,
    template,
    tName,
    tVersion,

    -- * TemplateActiveVersionRequest
    TemplateActiveVersionRequest,
    templateActiveVersionRequest,
    tavrVersion,

    -- * TemplateConfiguration
    TemplateConfiguration,
    templateConfiguration,
    tcSMSTemplate,
    tcVoiceTemplate,
    tcPushTemplate,
    tcEmailTemplate,

    -- * TemplateResponse
    TemplateResponse,
    templateResponse,
    temARN,
    temTemplateDescription,
    temDefaultSubstitutions,
    temVersion,
    temTags,
    temLastModifiedDate,
    temCreationDate,
    temTemplateName,
    temTemplateType,

    -- * TemplateVersionResponse
    TemplateVersionResponse,
    templateVersionResponse,
    tvTemplateDescription,
    tvDefaultSubstitutions,
    tvVersion,
    tvLastModifiedDate,
    tvCreationDate,
    tvTemplateName,
    tvTemplateType,

    -- * TemplateVersionsResponse
    TemplateVersionsResponse,
    templateVersionsResponse,
    tvRequestId,
    tvNextToken,
    tvMessage,
    tvItem,

    -- * TemplatesResponse
    TemplatesResponse,
    templatesResponse,
    tNextToken,
    tItem,

    -- * TreatmentResource
    TreatmentResource,
    treatmentResource,
    trCustomDeliveryConfiguration,
    trState,
    trSchedule,
    trTemplateConfiguration,
    trTreatmentName,
    trTreatmentDescription,
    trMessageConfiguration,
    trId,
    trSizePercent,

    -- * UpdateAttributesRequest
    UpdateAttributesRequest,
    updateAttributesRequest,
    uarBlacklist,

    -- * VoiceChannelRequest
    VoiceChannelRequest,
    voiceChannelRequest,
    vcrEnabled,

    -- * VoiceChannelResponse
    VoiceChannelResponse,
    voiceChannelResponse,
    vcLastModifiedDate,
    vcEnabled,
    vcIsArchived,
    vcApplicationId,
    vcVersion,
    vcId,
    vcCreationDate,
    vcLastModifiedBy,
    vcHasCredential,
    vcPlatform,

    -- * VoiceMessage
    VoiceMessage,
    voiceMessage,
    vmSubstitutions,
    vmLanguageCode,
    vmOriginationNumber,
    vmBody,
    vmVoiceId,

    -- * VoiceTemplateRequest
    VoiceTemplateRequest,
    voiceTemplateRequest,
    vtrLanguageCode,
    vtrBody,
    vtrTemplateDescription,
    vtrDefaultSubstitutions,
    vtrVoiceId,
    vtrTags,

    -- * VoiceTemplateResponse
    VoiceTemplateResponse,
    voiceTemplateResponse,
    vtLanguageCode,
    vtARN,
    vtBody,
    vtTemplateDescription,
    vtDefaultSubstitutions,
    vtVersion,
    vtVoiceId,
    vtTags,
    vtLastModifiedDate,
    vtCreationDate,
    vtTemplateName,
    vtTemplateType,

    -- * WaitActivity
    WaitActivity,
    waitActivity,
    waNextActivity,
    waWaitTime,

    -- * WaitTime
    WaitTime,
    waitTime,
    wtWaitFor,
    wtWaitUntil,

    -- * WriteApplicationSettingsRequest
    WriteApplicationSettingsRequest,
    writeApplicationSettingsRequest,
    wasrEventTaggingEnabled,
    wasrCloudWatchMetricsEnabled,
    wasrLimits,
    wasrQuietTime,
    wasrCampaignHook,

    -- * WriteCampaignRequest
    WriteCampaignRequest,
    writeCampaignRequest,
    wcrCustomDeliveryConfiguration,
    wcrSchedule,
    wcrTemplateConfiguration,
    wcrHook,
    wcrTreatmentName,
    wcrLimits,
    wcrIsPaused,
    wcrName,
    wcrHoldoutPercent,
    wcrTreatmentDescription,
    wcrMessageConfiguration,
    wcrDescription,
    wcrSegmentId,
    wcrAdditionalTreatments,
    wcrTags,
    wcrSegmentVersion,

    -- * WriteEventStream
    WriteEventStream,
    writeEventStream,
    wesRoleARN,
    wesDestinationStreamARN,

    -- * WriteJourneyRequest
    WriteJourneyRequest,
    writeJourneyRequest,
    wjrState,
    wjrLastModifiedDate,
    wjrSchedule,
    wjrLocalTime,
    wjrActivities,
    wjrLimits,
    wjrQuietTime,
    wjrStartActivity,
    wjrCreationDate,
    wjrStartCondition,
    wjrRefreshFrequency,
    wjrName,

    -- * WriteSegmentRequest
    WriteSegmentRequest,
    writeSegmentRequest,
    wsrSegmentGroups,
    wsrName,
    wsrDimensions,
    wsrTags,

    -- * WriteTreatmentResource
    WriteTreatmentResource,
    writeTreatmentResource,
    wtrCustomDeliveryConfiguration,
    wtrSchedule,
    wtrTemplateConfiguration,
    wtrTreatmentName,
    wtrTreatmentDescription,
    wtrMessageConfiguration,
    wtrSizePercent,
  )
where

import Network.AWS.Lens
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
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2016-12-01@ of the Amazon Pinpoint SDK configuration.
pinpoint :: Service
pinpoint =
  Service
    { _svcAbbrev = "Pinpoint",
      _svcSigner = v4,
      _svcPrefix = "pinpoint",
      _svcVersion = "2016-12-01",
      _svcEndpoint = defaultEndpoint pinpoint,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "Pinpoint",
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
