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
    admcrClientId,
    admcrClientSecret,
    admcrEnabled,

    -- * ADMChannelResponse
    ADMChannelResponse (..),
    mkADMChannelResponse,
    admcPlatform,
    admcLastModifiedDate,
    admcEnabled,
    admcIsArchived,
    admcApplicationId,
    admcVersion,
    admcId,
    admcCreationDate,
    admcLastModifiedBy,
    admcHasCredential,

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
    acPlatform,
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
    ascPlatform,
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
    avcPlatform,
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
    avscPlatform,
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
    aCampaignId,
    aTimezonesCompletedCount,
    aTimezonesTotalCount,
    aResult,
    aTreatmentId,
    aSuccessfulEndpointCount,
    aEnd,
    aApplicationId,
    aTotalEndpointCount,
    aId,
    aScheduledStart,

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
    adrkKpiName,
    adrkStartTime,
    adrkNextToken,
    adrkApplicationId,
    adrkEndTime,
    adrkKpiResult,

    -- * ApplicationResponse
    ApplicationResponse (..),
    mkApplicationResponse,
    afARN,
    afName,
    afId,
    afTags,

    -- * ApplicationSettingsResource
    ApplicationSettingsResource (..),
    mkApplicationSettingsResource,
    asrLastModifiedDate,
    asrLimits,
    asrQuietTime,
    asrApplicationId,
    asrCampaignHook,

    -- * ApplicationsResponse
    ApplicationsResponse (..),
    mkApplicationsResponse,
    afNextToken,
    afItem,

    -- * AttributeDimension
    AttributeDimension (..),
    mkAttributeDimension,
    adValues,
    adAttributeType,

    -- * AttributesResource
    AttributesResource (..),
    mkAttributesResource,
    arAttributeType,
    arApplicationId,
    arAttributes,

    -- * BaiduChannelRequest
    BaiduChannelRequest (..),
    mkBaiduChannelRequest,
    bcrAPIKey,
    bcrEnabled,
    bcrSecretKey,

    -- * BaiduChannelResponse
    BaiduChannelResponse (..),
    mkBaiduChannelResponse,
    bcPlatform,
    bcLastModifiedDate,
    bcEnabled,
    bcCredential,
    bcIsArchived,
    bcApplicationId,
    bcVersion,
    bcId,
    bcCreationDate,
    bcLastModifiedBy,
    bcHasCredential,

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
    cdrkKpiName,
    cdrkStartTime,
    cdrkCampaignId,
    cdrkNextToken,
    cdrkApplicationId,
    cdrkEndTime,
    cdrkKpiResult,

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
    cLastModifiedDate,
    cARN,
    cSchedule,
    cTemplateConfiguration,
    cHook,
    cTreatmentName,
    cLimits,
    cIsPaused,
    cDefaultState,
    cApplicationId,
    cName,
    cVersion,
    cHoldoutPercent,
    cTreatmentDescription,
    cId,
    cCreationDate,
    cMessageConfiguration,
    cDescription,
    cSegmentId,
    cAdditionalTreatments,
    cTags,
    cSegmentVersion,

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
    cfLastModifiedDate,
    cfEnabled,
    cfIsArchived,
    cfApplicationId,
    cfVersion,
    cfId,
    cfCreationDate,
    cfLastModifiedBy,
    cfHasCredential,

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
    carName,
    carTags,

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
    ecrFromAddress,
    ecrConfigurationSet,
    ecrIdentity,
    ecrRoleARN,

    -- * EmailChannelResponse
    EmailChannelResponse (..),
    mkEmailChannelResponse,
    ecPlatform,
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
    etTemplateName,
    etLastModifiedDate,
    etTextPart,
    etARN,
    etTemplateType,
    etTemplateDescription,
    etDefaultSubstitutions,
    etVersion,
    etHTMLPart,
    etCreationDate,
    etRecommenderId,
    etTags,

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
    emrDeliveryStatus,
    emrAddress,
    emrStatusMessage,
    emrUpdatedToken,
    emrMessageId,
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
    efRequestId,
    efMetrics,
    efLocation,
    efDemographic,
    efCohortId,
    efAddress,
    efEffectiveDate,
    efUser,
    efApplicationId,
    efAttributes,
    efEndpointStatus,
    efOptOut,
    efId,
    efCreationDate,
    efChannelType,

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
    eEventType,
    eAppPackageName,
    eAttributes,
    eSDKName,
    eTimestamp,
    eSession,

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
    esDestinationStreamARN,
    esApplicationId,
    esExternalId,
    esRoleARN,

    -- * EventsBatch
    EventsBatch (..),
    mkEventsBatch,
    ebEvents,
    ebEndpoint,

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
    eS3URLPrefix,
    eSegmentId,
    eRoleARN,
    eSegmentVersion,

    -- * ExportJobResource
    ExportJobResource (..),
    mkExportJobResource,
    ejrS3URLPrefix,
    ejrSegmentId,
    ejrRoleARN,
    ejrSegmentVersion,

    -- * ExportJobResponse
    ExportJobResponse (..),
    mkExportJobResponse,
    ejCompletedPieces,
    ejFailedPieces,
    ejDefinition,
    ejTotalProcessed,
    ejFailures,
    ejTotalPieces,
    ejApplicationId,
    ejId,
    ejCreationDate,
    ejType,
    ejCompletionDate,
    ejJobStatus,
    ejTotalFailures,

    -- * ExportJobsResponse
    ExportJobsResponse (..),
    mkExportJobsResponse,
    ejNextToken,
    ejItem,

    -- * GCMChannelRequest
    GCMChannelRequest (..),
    mkGCMChannelRequest,
    gcrAPIKey,
    gcrEnabled,

    -- * GCMChannelResponse
    GCMChannelResponse (..),
    mkGCMChannelResponse,
    gcmcPlatform,
    gcmcLastModifiedDate,
    gcmcEnabled,
    gcmcCredential,
    gcmcIsArchived,
    gcmcApplicationId,
    gcmcVersion,
    gcmcId,
    gcmcCreationDate,
    gcmcLastModifiedBy,
    gcmcHasCredential,

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
    gpspdCoordinates,
    gpspdRangeInKilometers,

    -- * HoldoutActivity
    HoldoutActivity (..),
    mkHoldoutActivity,
    haNextActivity,
    haPercentage,

    -- * ImportJobRequest
    ImportJobRequest (..),
    mkImportJobRequest,
    iSegmentName,
    iFormat,
    iDefineSegment,
    iRegisterEndpoints,
    iExternalId,
    iS3URL,
    iSegmentId,
    iRoleARN,

    -- * ImportJobResource
    ImportJobResource (..),
    mkImportJobResource,
    ijrSegmentName,
    ijrFormat,
    ijrDefineSegment,
    ijrRegisterEndpoints,
    ijrExternalId,
    ijrS3URL,
    ijrSegmentId,
    ijrRoleARN,

    -- * ImportJobResponse
    ImportJobResponse (..),
    mkImportJobResponse,
    ijCompletedPieces,
    ijFailedPieces,
    ijDefinition,
    ijTotalProcessed,
    ijFailures,
    ijTotalPieces,
    ijApplicationId,
    ijId,
    ijCreationDate,
    ijType,
    ijCompletionDate,
    ijJobStatus,
    ijTotalFailures,

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
    jdrkKpiName,
    jdrkStartTime,
    jdrkNextToken,
    jdrkApplicationId,
    jdrkEndTime,
    jdrkJourneyId,
    jdrkKpiResult,

    -- * JourneyEmailMessage
    JourneyEmailMessage (..),
    mkJourneyEmailMessage,
    jemFromAddress,

    -- * JourneyExecutionActivityMetricsResponse
    JourneyExecutionActivityMetricsResponse (..),
    mkJourneyExecutionActivityMetricsResponse,
    jeamMetrics,
    jeamActivityType,
    jeamLastEvaluatedTime,
    jeamJourneyActivityId,
    jeamApplicationId,
    jeamJourneyId,

    -- * JourneyExecutionMetricsResponse
    JourneyExecutionMetricsResponse (..),
    mkJourneyExecutionMetricsResponse,
    jemMetrics,
    jemLastEvaluatedTime,
    jemApplicationId,
    jemJourneyId,

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
    jApplicationId,
    jName,
    jId,
    jStartActivity,
    jCreationDate,
    jStartCondition,
    jRefreshFrequency,
    jTags,

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
    mApplicationId,
    mEndpointResult,

    -- * MessageResult
    MessageResult (..),
    mkMessageResult,
    mrDeliveryStatus,
    mrStatusMessage,
    mrUpdatedToken,
    mrMessageId,
    mrStatusCode,

    -- * MetricDimension
    MetricDimension (..),
    mkMetricDimension,
    mdValue,
    mdComparisonOperator,

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
    pntTemplateName,
    pntLastModifiedDate,
    pntARN,
    pntTemplateType,
    pntDefault,
    pntTemplateDescription,
    pntGCM,
    pntAPNS,
    pntDefaultSubstitutions,
    pntVersion,
    pntCreationDate,
    pntADM,
    pntBaidu,
    pntRecommenderId,
    pntTags,

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
    rdRecencyType,
    rdDuration,

    -- * RecommenderConfigurationResponse
    RecommenderConfigurationResponse (..),
    mkRecommenderConfigurationResponse,
    rcRecommendationTransformerURI,
    rcRecommendationsDisplayName,
    rcLastModifiedDate,
    rcRecommendationProviderIdType,
    rcRecommendationProviderURI,
    rcAttributes,
    rcName,
    rcId,
    rcCreationDate,
    rcDescription,
    rcRecommendationsPerMessage,
    rcRecommendationProviderRoleARN,

    -- * ResultRow
    ResultRow (..),
    mkResultRow,
    rrValues,
    rrGroupedBys,

    -- * ResultRowValue
    ResultRowValue (..),
    mkResultRowValue,
    rrvValue,
    rrvKey,
    rrvType,

    -- * SMSChannelRequest
    SMSChannelRequest (..),
    mkSMSChannelRequest,
    smscrShortCode,
    smscrEnabled,
    smscrSenderId,

    -- * SMSChannelResponse
    SMSChannelResponse (..),
    mkSMSChannelResponse,
    smscPlatform,
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
    smstTemplateName,
    smstLastModifiedDate,
    smstARN,
    smstTemplateType,
    smstBody,
    smstTemplateDescription,
    smstDefaultSubstitutions,
    smstVersion,
    smstCreationDate,
    smstRecommenderId,
    smstTags,

    -- * Schedule
    Schedule (..),
    mkSchedule,
    sFrequency,
    sStartTime,
    sQuietTime,
    sEventFilter,
    sIsLocalTime,
    sEndTime,
    sTimezone,

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
    sirSize,
    sirFormat,
    sirChannelCounts,
    sirExternalId,
    sirS3URL,
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
    sARN,
    sSegmentType,
    sSegmentGroups,
    sApplicationId,
    sName,
    sVersion,
    sId,
    sCreationDate,
    sImportDefinition,
    sDimensions,
    sTags,

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
    sumrUsers,
    sumrTemplateConfiguration,
    sumrMessageConfiguration,

    -- * SendUsersMessageResponse
    SendUsersMessageResponse (..),
    mkSendUsersMessageResponse,
    sumRequestId,
    sumResult,
    sumApplicationId,

    -- * Session
    Session (..),
    mkSession,
    sfStopTimestamp,
    sfId,
    sfStartTimestamp,
    sfDuration,

    -- * SetDimension
    SetDimension (..),
    mkSetDimension,
    sdValues,
    sdDimensionType,

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
    tfTemplateName,
    tfLastModifiedDate,
    tfARN,
    tfTemplateType,
    tfTemplateDescription,
    tfDefaultSubstitutions,
    tfVersion,
    tfCreationDate,
    tfTags,

    -- * TemplateVersionResponse
    TemplateVersionResponse (..),
    mkTemplateVersionResponse,
    tvTemplateName,
    tvLastModifiedDate,
    tvTemplateType,
    tvTemplateDescription,
    tvDefaultSubstitutions,
    tvVersion,
    tvCreationDate,

    -- * TemplateVersionsResponse
    TemplateVersionsResponse (..),
    mkTemplateVersionsResponse,
    tvRequestId,
    tvNextToken,
    tvItem,
    tvMessage,

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
    trSizePercent,
    trTreatmentDescription,
    trId,
    trMessageConfiguration,

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
    vcPlatform,
    vcLastModifiedDate,
    vcEnabled,
    vcIsArchived,
    vcApplicationId,
    vcVersion,
    vcId,
    vcCreationDate,
    vcLastModifiedBy,
    vcHasCredential,

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
    vtTemplateName,
    vtLastModifiedDate,
    vtLanguageCode,
    vtARN,
    vtTemplateType,
    vtBody,
    vtTemplateDescription,
    vtDefaultSubstitutions,
    vtVersion,
    vtCreationDate,
    vtVoiceId,
    vtTags,

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
    wesDestinationStreamARN,
    wesRoleARN,

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
    wjrName,
    wjrStartActivity,
    wjrCreationDate,
    wjrStartCondition,
    wjrRefreshFrequency,

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
    wtrSizePercent,
    wtrTreatmentDescription,
    wtrMessageConfiguration,
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
