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
    pinpointService,

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
    ADMChannelRequest (..),
    mkADMChannelRequest,
    admcrEnabled,
    admcrClientSecret,
    admcrClientId,

    -- * ADMChannelResponse
    ADMChannelResponse (..),
    mkADMChannelResponse,
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
    ADMMessage (..),
    mkADMMessage,
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
    APNSChannelRequest (..),
    mkAPNSChannelRequest,
    acrTokenKey,
    acrPrivateKey,
    acrEnabled,
    acrTeamId,
    acrBundleId,
    acrDefaultAuthenticationMethod,
    acrCertificate,
    acrTokenKeyId,

    -- * APNSChannelResponse
    APNSChannelResponse (..),
    mkAPNSChannelResponse,
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
    APNSMessage (..),
    mkAPNSMessage,
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
    APNSPushNotificationTemplate (..),
    mkAPNSPushNotificationTemplate,
    apntRawContent,
    apntBody,
    apntURL,
    apntSound,
    apntAction,
    apntMediaURL,
    apntTitle,

    -- * APNSSandboxChannelRequest
    APNSSandboxChannelRequest (..),
    mkAPNSSandboxChannelRequest,
    ascrTokenKey,
    ascrPrivateKey,
    ascrEnabled,
    ascrTeamId,
    ascrBundleId,
    ascrDefaultAuthenticationMethod,
    ascrCertificate,
    ascrTokenKeyId,

    -- * APNSSandboxChannelResponse
    APNSSandboxChannelResponse (..),
    mkAPNSSandboxChannelResponse,
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
    APNSVoipChannelRequest (..),
    mkAPNSVoipChannelRequest,
    avcrTokenKey,
    avcrPrivateKey,
    avcrEnabled,
    avcrTeamId,
    avcrBundleId,
    avcrDefaultAuthenticationMethod,
    avcrCertificate,
    avcrTokenKeyId,

    -- * APNSVoipChannelResponse
    APNSVoipChannelResponse (..),
    mkAPNSVoipChannelResponse,
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
    APNSVoipSandboxChannelRequest (..),
    mkAPNSVoipSandboxChannelRequest,
    avscrTokenKey,
    avscrPrivateKey,
    avscrEnabled,
    avscrTeamId,
    avscrBundleId,
    avscrDefaultAuthenticationMethod,
    avscrCertificate,
    avscrTokenKeyId,

    -- * APNSVoipSandboxChannelResponse
    APNSVoipSandboxChannelResponse (..),
    mkAPNSVoipSandboxChannelResponse,
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
    ActivitiesResponse (..),
    mkActivitiesResponse,
    aNextToken,
    aItem,

    -- * Activity
    Activity (..),
    mkActivity,
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
    ActivityResponse (..),
    mkActivityResponse,
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
    AddressConfiguration (..),
    mkAddressConfiguration,
    acSubstitutions,
    acTitleOverride,
    acContext,
    acRawContent,
    acBodyOverride,
    acChannelType,

    -- * AndroidPushNotificationTemplate
    AndroidPushNotificationTemplate (..),
    mkAndroidPushNotificationTemplate,
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
    ApplicationDateRangeKpiResponse (..),
    mkApplicationDateRangeKpiResponse,
    adrkNextToken,
    adrkKpiResult,
    adrkKpiName,
    adrkEndTime,
    adrkStartTime,
    adrkApplicationId,

    -- * ApplicationResponse
    ApplicationResponse (..),
    mkApplicationResponse,
    appTags,
    appId,
    appARN,
    appName,

    -- * ApplicationSettingsResource
    ApplicationSettingsResource (..),
    mkApplicationSettingsResource,
    asrLastModifiedDate,
    asrLimits,
    asrQuietTime,
    asrCampaignHook,
    asrApplicationId,

    -- * ApplicationsResponse
    ApplicationsResponse (..),
    mkApplicationsResponse,
    appNextToken,
    appItem,

    -- * AttributeDimension
    AttributeDimension (..),
    mkAttributeDimension,
    adAttributeType,
    adValues,

    -- * AttributesResource
    AttributesResource (..),
    mkAttributesResource,
    arAttributes,
    arAttributeType,
    arApplicationId,

    -- * BaiduChannelRequest
    BaiduChannelRequest (..),
    mkBaiduChannelRequest,
    bcrEnabled,
    bcrSecretKey,
    bcrAPIKey,

    -- * BaiduChannelResponse
    BaiduChannelResponse (..),
    mkBaiduChannelResponse,
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
    BaiduMessage (..),
    mkBaiduMessage,
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
    BaseKpiResult (..),
    mkBaseKpiResult,
    bkrRows,

    -- * CampaignCustomMessage
    CampaignCustomMessage (..),
    mkCampaignCustomMessage,
    ccmData,

    -- * CampaignDateRangeKpiResponse
    CampaignDateRangeKpiResponse (..),
    mkCampaignDateRangeKpiResponse,
    cdrkNextToken,
    cdrkKpiResult,
    cdrkKpiName,
    cdrkEndTime,
    cdrkCampaignId,
    cdrkStartTime,
    cdrkApplicationId,

    -- * CampaignEmailMessage
    CampaignEmailMessage (..),
    mkCampaignEmailMessage,
    cemBody,
    cemFromAddress,
    cemHTMLBody,
    cemTitle,

    -- * CampaignEventFilter
    CampaignEventFilter (..),
    mkCampaignEventFilter,
    cefFilterType,
    cefDimensions,

    -- * CampaignHook
    CampaignHook (..),
    mkCampaignHook,
    chLambdaFunctionName,
    chMode,
    chWebURL,

    -- * CampaignLimits
    CampaignLimits (..),
    mkCampaignLimits,
    clMessagesPerSecond,
    clDaily,
    clTotal,
    clMaximumDuration,

    -- * CampaignResponse
    CampaignResponse (..),
    mkCampaignResponse,
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
    CampaignSmsMessage (..),
    mkCampaignSmsMessage,
    csmBody,
    csmMessageType,
    csmSenderId,

    -- * CampaignState
    CampaignState (..),
    mkCampaignState,
    csCampaignStatus,

    -- * CampaignsResponse
    CampaignsResponse (..),
    mkCampaignsResponse,
    cNextToken,
    cItem,

    -- * ChannelResponse
    ChannelResponse (..),
    mkChannelResponse,
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
    ChannelsResponse (..),
    mkChannelsResponse,
    cChannels,

    -- * Condition
    Condition (..),
    mkCondition,
    cOperator,
    cConditions,

    -- * ConditionalSplitActivity
    ConditionalSplitActivity (..),
    mkConditionalSplitActivity,
    csaEvaluationWaitTime,
    csaTrueActivity,
    csaFalseActivity,
    csaCondition,

    -- * CreateApplicationRequest
    CreateApplicationRequest (..),
    mkCreateApplicationRequest,
    carTags,
    carName,

    -- * CreateTemplateMessageBody
    CreateTemplateMessageBody (..),
    mkCreateTemplateMessageBody,
    ctmbRequestId,
    ctmbARN,
    ctmbMessage,

    -- * CustomDeliveryConfiguration
    CustomDeliveryConfiguration (..),
    mkCustomDeliveryConfiguration,
    cdcEndpointTypes,
    cdcDeliveryURI,

    -- * CustomMessageActivity
    CustomMessageActivity (..),
    mkCustomMessageActivity,
    cmaTemplateName,
    cmaTemplateVersion,
    cmaEndpointTypes,
    cmaNextActivity,
    cmaDeliveryURI,
    cmaMessageConfig,

    -- * DefaultMessage
    DefaultMessage (..),
    mkDefaultMessage,
    dmSubstitutions,
    dmBody,

    -- * DefaultPushNotificationMessage
    DefaultPushNotificationMessage (..),
    mkDefaultPushNotificationMessage,
    dpnmSubstitutions,
    dpnmSilentPush,
    dpnmData,
    dpnmBody,
    dpnmURL,
    dpnmAction,
    dpnmTitle,

    -- * DefaultPushNotificationTemplate
    DefaultPushNotificationTemplate (..),
    mkDefaultPushNotificationTemplate,
    dpntBody,
    dpntURL,
    dpntSound,
    dpntAction,
    dpntTitle,

    -- * DirectMessageConfiguration
    DirectMessageConfiguration (..),
    mkDirectMessageConfiguration,
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
    EmailChannelRequest (..),
    mkEmailChannelRequest,
    ecrEnabled,
    ecrConfigurationSet,
    ecrRoleARN,
    ecrFromAddress,
    ecrIdentity,

    -- * EmailChannelResponse
    EmailChannelResponse (..),
    mkEmailChannelResponse,
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
    EmailMessage (..),
    mkEmailMessage,
    emSubstitutions,
    emBody,
    emFromAddress,
    emRawEmail,
    emFeedbackForwardingAddress,
    emSimpleEmail,
    emReplyToAddresses,

    -- * EmailMessageActivity
    EmailMessageActivity (..),
    mkEmailMessageActivity,
    emaTemplateName,
    emaTemplateVersion,
    emaNextActivity,
    emaMessageConfig,

    -- * EmailTemplateRequest
    EmailTemplateRequest (..),
    mkEmailTemplateRequest,
    etrSubject,
    etrTextPart,
    etrTemplateDescription,
    etrDefaultSubstitutions,
    etrHTMLPart,
    etrRecommenderId,
    etrTags,

    -- * EmailTemplateResponse
    EmailTemplateResponse (..),
    mkEmailTemplateResponse,
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
    EndpointBatchItem (..),
    mkEndpointBatchItem,
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
    EndpointBatchRequest (..),
    mkEndpointBatchRequest,
    ebrItem,

    -- * EndpointDemographic
    EndpointDemographic (..),
    mkEndpointDemographic,
    edPlatform,
    edPlatformVersion,
    edLocale,
    edAppVersion,
    edModel,
    edMake,
    edModelVersion,
    edTimezone,

    -- * EndpointItemResponse
    EndpointItemResponse (..),
    mkEndpointItemResponse,
    eiMessage,
    eiStatusCode,

    -- * EndpointLocation
    EndpointLocation (..),
    mkEndpointLocation,
    elPostalCode,
    elLatitude,
    elCountry,
    elCity,
    elRegion,
    elLongitude,

    -- * EndpointMessageResult
    EndpointMessageResult (..),
    mkEndpointMessageResult,
    emrAddress,
    emrStatusMessage,
    emrUpdatedToken,
    emrMessageId,
    emrDeliveryStatus,
    emrStatusCode,

    -- * EndpointRequest
    EndpointRequest (..),
    mkEndpointRequest,
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
    EndpointResponse (..),
    mkEndpointResponse,
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
    EndpointSendConfiguration (..),
    mkEndpointSendConfiguration,
    escSubstitutions,
    escTitleOverride,
    escContext,
    escRawContent,
    escBodyOverride,

    -- * EndpointUser
    EndpointUser (..),
    mkEndpointUser,
    euUserAttributes,
    euUserId,

    -- * EndpointsResponse
    EndpointsResponse (..),
    mkEndpointsResponse,
    eItem,

    -- * Event
    Event (..),
    mkEvent,
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
    EventCondition (..),
    mkEventCondition,
    ecDimensions,
    ecMessageActivity,

    -- * EventDimensions
    EventDimensions (..),
    mkEventDimensions,
    edMetrics,
    edEventType,
    edAttributes,

    -- * EventFilter
    EventFilter (..),
    mkEventFilter,
    efFilterType,
    efDimensions,

    -- * EventItemResponse
    EventItemResponse (..),
    mkEventItemResponse,
    eMessage,
    eStatusCode,

    -- * EventStartCondition
    EventStartCondition (..),
    mkEventStartCondition,
    escEventFilter,
    escSegmentId,

    -- * EventStream
    EventStream (..),
    mkEventStream,
    esLastUpdatedBy,
    esLastModifiedDate,
    esExternalId,
    esApplicationId,
    esRoleARN,
    esDestinationStreamARN,

    -- * EventsBatch
    EventsBatch (..),
    mkEventsBatch,
    ebEndpoint,
    ebEvents,

    -- * EventsRequest
    EventsRequest (..),
    mkEventsRequest,
    erBatchItem,

    -- * EventsResponse
    EventsResponse (..),
    mkEventsResponse,
    eResults,

    -- * ExportJobRequest
    ExportJobRequest (..),
    mkExportJobRequest,
    eSegmentId,
    eSegmentVersion,
    eS3URLPrefix,
    eRoleARN,

    -- * ExportJobResource
    ExportJobResource (..),
    mkExportJobResource,
    ejrSegmentId,
    ejrSegmentVersion,
    ejrS3URLPrefix,
    ejrRoleARN,

    -- * ExportJobResponse
    ExportJobResponse (..),
    mkExportJobResponse,
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
    ExportJobsResponse (..),
    mkExportJobsResponse,
    ejNextToken,
    ejItem,

    -- * GCMChannelRequest
    GCMChannelRequest (..),
    mkGCMChannelRequest,
    gcrEnabled,
    gcrAPIKey,

    -- * GCMChannelResponse
    GCMChannelResponse (..),
    mkGCMChannelResponse,
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
    GCMMessage (..),
    mkGCMMessage,
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
    GPSCoordinates (..),
    mkGPSCoordinates,
    gpscLatitude,
    gpscLongitude,

    -- * GPSPointDimension
    GPSPointDimension (..),
    mkGPSPointDimension,
    gpspdRangeInKilometers,
    gpspdCoordinates,

    -- * HoldoutActivity
    HoldoutActivity (..),
    mkHoldoutActivity,
    haNextActivity,
    haPercentage,

    -- * ImportJobRequest
    ImportJobRequest (..),
    mkImportJobRequest,
    iSegmentName,
    iDefineSegment,
    iRegisterEndpoints,
    iExternalId,
    iSegmentId,
    iFormat,
    iS3URL,
    iRoleARN,

    -- * ImportJobResource
    ImportJobResource (..),
    mkImportJobResource,
    ijrSegmentName,
    ijrDefineSegment,
    ijrRegisterEndpoints,
    ijrExternalId,
    ijrSegmentId,
    ijrFormat,
    ijrS3URL,
    ijrRoleARN,

    -- * ImportJobResponse
    ImportJobResponse (..),
    mkImportJobResponse,
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
    ImportJobsResponse (..),
    mkImportJobsResponse,
    ijNextToken,
    ijItem,

    -- * ItemResponse
    ItemResponse (..),
    mkItemResponse,
    iEndpointItemResponse,
    iEventsItemResponse,

    -- * JourneyCustomMessage
    JourneyCustomMessage (..),
    mkJourneyCustomMessage,
    jcmData,

    -- * JourneyDateRangeKpiResponse
    JourneyDateRangeKpiResponse (..),
    mkJourneyDateRangeKpiResponse,
    jdrkNextToken,
    jdrkKpiResult,
    jdrkKpiName,
    jdrkJourneyId,
    jdrkEndTime,
    jdrkStartTime,
    jdrkApplicationId,

    -- * JourneyEmailMessage
    JourneyEmailMessage (..),
    mkJourneyEmailMessage,
    jemFromAddress,

    -- * JourneyExecutionActivityMetricsResponse
    JourneyExecutionActivityMetricsResponse (..),
    mkJourneyExecutionActivityMetricsResponse,
    jeamMetrics,
    jeamJourneyId,
    jeamLastEvaluatedTime,
    jeamJourneyActivityId,
    jeamActivityType,
    jeamApplicationId,

    -- * JourneyExecutionMetricsResponse
    JourneyExecutionMetricsResponse (..),
    mkJourneyExecutionMetricsResponse,
    jemMetrics,
    jemJourneyId,
    jemLastEvaluatedTime,
    jemApplicationId,

    -- * JourneyLimits
    JourneyLimits (..),
    mkJourneyLimits,
    jlMessagesPerSecond,
    jlEndpointReentryCap,
    jlDailyCap,

    -- * JourneyPushMessage
    JourneyPushMessage (..),
    mkJourneyPushMessage,
    jpmTimeToLive,

    -- * JourneyResponse
    JourneyResponse (..),
    mkJourneyResponse,
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
    JourneySMSMessage (..),
    mkJourneySMSMessage,
    jsmsmMessageType,
    jsmsmSenderId,

    -- * JourneySchedule
    JourneySchedule (..),
    mkJourneySchedule,
    jsStartTime,
    jsEndTime,
    jsTimezone,

    -- * JourneyStateRequest
    JourneyStateRequest (..),
    mkJourneyStateRequest,
    jsrState,

    -- * JourneysResponse
    JourneysResponse (..),
    mkJourneysResponse,
    jNextToken,
    jItem,

    -- * ListRecommenderConfigurationsResponse
    ListRecommenderConfigurationsResponse (..),
    mkListRecommenderConfigurationsResponse,
    lrcNextToken,
    lrcItem,

    -- * Message
    Message (..),
    mkMessage,
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
    MessageBody (..),
    mkMessageBody,
    mbRequestId,
    mbMessage,

    -- * MessageConfiguration
    MessageConfiguration (..),
    mkMessageConfiguration,
    mcAPNSMessage,
    mcGCMMessage,
    mcDefaultMessage,
    mcCustomMessage,
    mcADMMessage,
    mcSMSMessage,
    mcEmailMessage,
    mcBaiduMessage,

    -- * MessageRequest
    MessageRequest (..),
    mkMessageRequest,
    mrTraceId,
    mrContext,
    mrAddresses,
    mrTemplateConfiguration,
    mrEndpoints,
    mrMessageConfiguration,

    -- * MessageResponse
    MessageResponse (..),
    mkMessageResponse,
    mRequestId,
    mResult,
    mEndpointResult,
    mApplicationId,

    -- * MessageResult
    MessageResult (..),
    mkMessageResult,
    mrStatusMessage,
    mrUpdatedToken,
    mrMessageId,
    mrDeliveryStatus,
    mrStatusCode,

    -- * MetricDimension
    MetricDimension (..),
    mkMetricDimension,
    mdComparisonOperator,
    mdValue,

    -- * MultiConditionalBranch
    MultiConditionalBranch (..),
    mkMultiConditionalBranch,
    mcbNextActivity,
    mcbCondition,

    -- * MultiConditionalSplitActivity
    MultiConditionalSplitActivity (..),
    mkMultiConditionalSplitActivity,
    mcsaBranches,
    mcsaEvaluationWaitTime,
    mcsaDefaultActivity,

    -- * NumberValidateRequest
    NumberValidateRequest (..),
    mkNumberValidateRequest,
    nvrIsoCountryCode,
    nvrPhoneNumber,

    -- * NumberValidateResponse
    NumberValidateResponse (..),
    mkNumberValidateResponse,
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
    PublicEndpoint (..),
    mkPublicEndpoint,
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
    PushMessageActivity (..),
    mkPushMessageActivity,
    pmaTemplateName,
    pmaTemplateVersion,
    pmaNextActivity,
    pmaMessageConfig,

    -- * PushNotificationTemplateRequest
    PushNotificationTemplateRequest (..),
    mkPushNotificationTemplateRequest,
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
    PushNotificationTemplateResponse (..),
    mkPushNotificationTemplateResponse,
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
    QuietTime (..),
    mkQuietTime,
    qtStart,
    qtEnd,

    -- * RandomSplitActivity
    RandomSplitActivity (..),
    mkRandomSplitActivity,
    rsaBranches,

    -- * RandomSplitEntry
    RandomSplitEntry (..),
    mkRandomSplitEntry,
    rseNextActivity,
    rsePercentage,

    -- * RawEmail
    RawEmail (..),
    mkRawEmail,
    reData,

    -- * RecencyDimension
    RecencyDimension (..),
    mkRecencyDimension,
    rdDuration,
    rdRecencyType,

    -- * RecommenderConfigurationResponse
    RecommenderConfigurationResponse (..),
    mkRecommenderConfigurationResponse,
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
    ResultRow (..),
    mkResultRow,
    rrGroupedBys,
    rrValues,

    -- * ResultRowValue
    ResultRowValue (..),
    mkResultRowValue,
    rrvType,
    rrvValue,
    rrvKey,

    -- * SMSChannelRequest
    SMSChannelRequest (..),
    mkSMSChannelRequest,
    smscrShortCode,
    smscrEnabled,
    smscrSenderId,

    -- * SMSChannelResponse
    SMSChannelResponse (..),
    mkSMSChannelResponse,
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
    SMSMessage (..),
    mkSMSMessage,
    smsmSubstitutions,
    smsmOriginationNumber,
    smsmBody,
    smsmMessageType,
    smsmSenderId,
    smsmMediaURL,
    smsmKeyword,

    -- * SMSMessageActivity
    SMSMessageActivity (..),
    mkSMSMessageActivity,
    smsmaTemplateName,
    smsmaTemplateVersion,
    smsmaNextActivity,
    smsmaMessageConfig,

    -- * SMSTemplateRequest
    SMSTemplateRequest (..),
    mkSMSTemplateRequest,
    smstrBody,
    smstrTemplateDescription,
    smstrDefaultSubstitutions,
    smstrRecommenderId,
    smstrTags,

    -- * SMSTemplateResponse
    SMSTemplateResponse (..),
    mkSMSTemplateResponse,
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
    Schedule (..),
    mkSchedule,
    sFrequency,
    sQuietTime,
    sEventFilter,
    sIsLocalTime,
    sEndTime,
    sTimezone,
    sStartTime,

    -- * SegmentBehaviors
    SegmentBehaviors (..),
    mkSegmentBehaviors,
    sbRecency,

    -- * SegmentCondition
    SegmentCondition (..),
    mkSegmentCondition,
    scSegmentId,

    -- * SegmentDemographics
    SegmentDemographics (..),
    mkSegmentDemographics,
    sdPlatform,
    sdAppVersion,
    sdChannel,
    sdModel,
    sdMake,
    sdDeviceType,

    -- * SegmentDimensions
    SegmentDimensions (..),
    mkSegmentDimensions,
    sdMetrics,
    sdLocation,
    sdDemographic,
    sdUserAttributes,
    sdBehavior,
    sdAttributes,

    -- * SegmentGroup
    SegmentGroup (..),
    mkSegmentGroup,
    sgSourceSegments,
    sgSourceType,
    sgType,
    sgDimensions,

    -- * SegmentGroupList
    SegmentGroupList (..),
    mkSegmentGroupList,
    sglInclude,
    sglGroups,

    -- * SegmentImportResource
    SegmentImportResource (..),
    mkSegmentImportResource,
    sirChannelCounts,
    sirFormat,
    sirS3URL,
    sirSize,
    sirExternalId,
    sirRoleARN,

    -- * SegmentLocation
    SegmentLocation (..),
    mkSegmentLocation,
    slCountry,
    slGPSPoint,

    -- * SegmentReference
    SegmentReference (..),
    mkSegmentReference,
    srVersion,
    srId,

    -- * SegmentResponse
    SegmentResponse (..),
    mkSegmentResponse,
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
    SegmentsResponse (..),
    mkSegmentsResponse,
    sNextToken,
    sItem,

    -- * SendUsersMessageRequest
    SendUsersMessageRequest (..),
    mkSendUsersMessageRequest,
    sumrTraceId,
    sumrContext,
    sumrTemplateConfiguration,
    sumrMessageConfiguration,
    sumrUsers,

    -- * SendUsersMessageResponse
    SendUsersMessageResponse (..),
    mkSendUsersMessageResponse,
    sumRequestId,
    sumResult,
    sumApplicationId,

    -- * Session
    Session (..),
    mkSession,
    sesStopTimestamp,
    sesDuration,
    sesStartTimestamp,
    sesId,

    -- * SetDimension
    SetDimension (..),
    mkSetDimension,
    sdDimensionType,
    sdValues,

    -- * SimpleCondition
    SimpleCondition (..),
    mkSimpleCondition,
    scSegmentDimensions,
    scEventCondition,
    scSegmentCondition,

    -- * SimpleEmail
    SimpleEmail (..),
    mkSimpleEmail,
    seSubject,
    seTextPart,
    seHTMLPart,

    -- * SimpleEmailPart
    SimpleEmailPart (..),
    mkSimpleEmailPart,
    sepData,
    sepCharset,

    -- * StartCondition
    StartCondition (..),
    mkStartCondition,
    scSegmentStartCondition,
    scEventStartCondition,
    scDescription,

    -- * TagsModel
    TagsModel (..),
    mkTagsModel,
    tmTags,

    -- * Template
    Template (..),
    mkTemplate,
    tName,
    tVersion,

    -- * TemplateActiveVersionRequest
    TemplateActiveVersionRequest (..),
    mkTemplateActiveVersionRequest,
    tavrVersion,

    -- * TemplateConfiguration
    TemplateConfiguration (..),
    mkTemplateConfiguration,
    tcSMSTemplate,
    tcVoiceTemplate,
    tcPushTemplate,
    tcEmailTemplate,

    -- * TemplateResponse
    TemplateResponse (..),
    mkTemplateResponse,
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
    TemplateVersionResponse (..),
    mkTemplateVersionResponse,
    tvTemplateDescription,
    tvDefaultSubstitutions,
    tvVersion,
    tvLastModifiedDate,
    tvCreationDate,
    tvTemplateName,
    tvTemplateType,

    -- * TemplateVersionsResponse
    TemplateVersionsResponse (..),
    mkTemplateVersionsResponse,
    tvRequestId,
    tvNextToken,
    tvMessage,
    tvItem,

    -- * TemplatesResponse
    TemplatesResponse (..),
    mkTemplatesResponse,
    tNextToken,
    tItem,

    -- * TreatmentResource
    TreatmentResource (..),
    mkTreatmentResource,
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
    UpdateAttributesRequest (..),
    mkUpdateAttributesRequest,
    uarBlacklist,

    -- * VoiceChannelRequest
    VoiceChannelRequest (..),
    mkVoiceChannelRequest,
    vcrEnabled,

    -- * VoiceChannelResponse
    VoiceChannelResponse (..),
    mkVoiceChannelResponse,
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
    VoiceMessage (..),
    mkVoiceMessage,
    vmSubstitutions,
    vmLanguageCode,
    vmOriginationNumber,
    vmBody,
    vmVoiceId,

    -- * VoiceTemplateRequest
    VoiceTemplateRequest (..),
    mkVoiceTemplateRequest,
    vtrLanguageCode,
    vtrBody,
    vtrTemplateDescription,
    vtrDefaultSubstitutions,
    vtrVoiceId,
    vtrTags,

    -- * VoiceTemplateResponse
    VoiceTemplateResponse (..),
    mkVoiceTemplateResponse,
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
    WaitActivity (..),
    mkWaitActivity,
    waNextActivity,
    waWaitTime,

    -- * WaitTime
    WaitTime (..),
    mkWaitTime,
    wtWaitFor,
    wtWaitUntil,

    -- * WriteApplicationSettingsRequest
    WriteApplicationSettingsRequest (..),
    mkWriteApplicationSettingsRequest,
    wasrEventTaggingEnabled,
    wasrCloudWatchMetricsEnabled,
    wasrLimits,
    wasrQuietTime,
    wasrCampaignHook,

    -- * WriteCampaignRequest
    WriteCampaignRequest (..),
    mkWriteCampaignRequest,
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
    WriteEventStream (..),
    mkWriteEventStream,
    wesRoleARN,
    wesDestinationStreamARN,

    -- * WriteJourneyRequest
    WriteJourneyRequest (..),
    mkWriteJourneyRequest,
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
    WriteSegmentRequest (..),
    mkWriteSegmentRequest,
    wsrSegmentGroups,
    wsrName,
    wsrDimensions,
    wsrTags,

    -- * WriteTreatmentResource
    WriteTreatmentResource (..),
    mkWriteTreatmentResource,
    wtrCustomDeliveryConfiguration,
    wtrSchedule,
    wtrTemplateConfiguration,
    wtrTreatmentName,
    wtrTreatmentDescription,
    wtrMessageConfiguration,
    wtrSizePercent,
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
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2016-12-01@ of the Amazon Pinpoint SDK configuration.
pinpointService :: Lude.Service
pinpointService =
  Lude.Service
    { Lude._svcAbbrev = "Pinpoint",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "pinpoint",
      Lude._svcVersion = "2016-12-01",
      Lude._svcEndpoint = Lude.defaultEndpoint pinpointService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "Pinpoint",
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
