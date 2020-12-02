{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
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
--
-- Targeted push notifications based on app usage trends and user behavior have become a popular approach for mobile app user engagement because response rates are often several times higher than tradition email marketing campaigns. By using targeted push notifications, you can increase message relevance and effectiveness, measure engagement, and continually improve your campaigns.
--
-- Getting started with Amazon Pinpoint is easy. First, AWS Mobile Hub guides you through the process to integrate the AWS Mobile SDK with your app. Next, you define your target segments, campaign message, and specify the delivery schedule. Once your campaign is running, Pinpoint provides metrics so you can run analytics and track the impact of your campaign.
--
-- With Amazon Pinpoint, there are no upfront setup costs, and no fixed monthly cost. You only pay for the number of users your campaign targets, the messages you send, and the events you collect, so you can start small and scale as your application grows.
module Network.AWS.Pinpoint
  ( -- * Service Configuration
    pinpoint,

    -- * Errors
    -- $errors

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** GetGCMChannel
    module Network.AWS.Pinpoint.GetGCMChannel,

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

    -- ** GetAPNSVoipSandboxChannel
    module Network.AWS.Pinpoint.GetAPNSVoipSandboxChannel,

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

    -- ** UpdateADMChannel
    module Network.AWS.Pinpoint.UpdateADMChannel,

    -- ** DeleteADMChannel
    module Network.AWS.Pinpoint.DeleteADMChannel,

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

    -- ** GetADMChannel
    module Network.AWS.Pinpoint.GetADMChannel,

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

    -- ** UpdateAPNSVoipSandboxChannel
    module Network.AWS.Pinpoint.UpdateAPNSVoipSandboxChannel,

    -- ** DeleteAPNSVoipSandboxChannel
    module Network.AWS.Pinpoint.DeleteAPNSVoipSandboxChannel,

    -- ** UpdateGCMChannel
    module Network.AWS.Pinpoint.UpdateGCMChannel,

    -- ** DeleteGCMChannel
    module Network.AWS.Pinpoint.DeleteGCMChannel,

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

    -- ** DeleteAPNSChannel
    module Network.AWS.Pinpoint.DeleteAPNSChannel,

    -- ** UpdateAPNSChannel
    module Network.AWS.Pinpoint.UpdateAPNSChannel,

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

    -- ** GetAPNSChannel
    module Network.AWS.Pinpoint.GetAPNSChannel,

    -- ** UpdateVoiceChannel
    module Network.AWS.Pinpoint.UpdateVoiceChannel,

    -- ** DeleteVoiceChannel
    module Network.AWS.Pinpoint.DeleteVoiceChannel,

    -- ** GetApps
    module Network.AWS.Pinpoint.GetApps,

    -- ** GetAPNSSandboxChannel
    module Network.AWS.Pinpoint.GetAPNSSandboxChannel,

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

    -- ** DeleteAPNSSandboxChannel
    module Network.AWS.Pinpoint.DeleteAPNSSandboxChannel,

    -- ** UpdateAPNSSandboxChannel
    module Network.AWS.Pinpoint.UpdateAPNSSandboxChannel,

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

    -- ** DeleteAPNSVoipChannel
    module Network.AWS.Pinpoint.DeleteAPNSVoipChannel,

    -- ** UpdateAPNSVoipChannel
    module Network.AWS.Pinpoint.UpdateAPNSVoipChannel,

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

    -- ** GetAPNSVoipChannel
    module Network.AWS.Pinpoint.GetAPNSVoipChannel,

    -- ** GetEmailChannel
    module Network.AWS.Pinpoint.GetEmailChannel,

    -- * Types

    -- ** Action
    Action (..),

    -- ** AttributeType
    AttributeType (..),

    -- ** CampaignStatus
    CampaignStatus (..),

    -- ** ChannelType
    ChannelType (..),

    -- ** DefinitionFormat
    DefinitionFormat (..),

    -- ** DeliveryStatus
    DeliveryStatus (..),

    -- ** DimensionType
    DimensionType (..),

    -- ** Duration
    Duration (..),

    -- ** EndpointTypesElement
    EndpointTypesElement (..),

    -- ** FilterType
    FilterType (..),

    -- ** Frequency
    Frequency (..),

    -- ** Include
    Include (..),

    -- ** JobStatus
    JobStatus (..),

    -- ** MessageType
    MessageType (..),

    -- ** Mode
    Mode (..),

    -- ** Operator
    Operator (..),

    -- ** RecencyType
    RecencyType (..),

    -- ** SegmentType
    SegmentType (..),

    -- ** SourceType
    SourceType (..),

    -- ** State
    State (..),

    -- ** TemplateType
    TemplateType (..),

    -- ** Type
    Type (..),

    -- ** ADMChannelRequest
    ADMChannelRequest,
    aDMChannelRequest,
    admcrEnabled,
    admcrClientSecret,
    admcrClientId,

    -- ** ADMChannelResponse
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

    -- ** ADMMessage
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

    -- ** APNSChannelRequest
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

    -- ** APNSChannelResponse
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

    -- ** APNSMessage
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

    -- ** APNSPushNotificationTemplate
    APNSPushNotificationTemplate,
    apnsPushNotificationTemplate,
    apntRawContent,
    apntBody,
    apntURL,
    apntSound,
    apntAction,
    apntMediaURL,
    apntTitle,

    -- ** APNSSandboxChannelRequest
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

    -- ** APNSSandboxChannelResponse
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

    -- ** APNSVoipChannelRequest
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

    -- ** APNSVoipChannelResponse
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

    -- ** APNSVoipSandboxChannelRequest
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

    -- ** APNSVoipSandboxChannelResponse
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

    -- ** ActivitiesResponse
    ActivitiesResponse,
    activitiesResponse,
    aNextToken,
    aItem,

    -- ** Activity
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

    -- ** ActivityResponse
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

    -- ** AddressConfiguration
    AddressConfiguration,
    addressConfiguration,
    acSubstitutions,
    acTitleOverride,
    acContext,
    acRawContent,
    acBodyOverride,
    acChannelType,

    -- ** AndroidPushNotificationTemplate
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

    -- ** ApplicationDateRangeKpiResponse
    ApplicationDateRangeKpiResponse,
    applicationDateRangeKpiResponse,
    adrkNextToken,
    adrkKpiResult,
    adrkKpiName,
    adrkEndTime,
    adrkStartTime,
    adrkApplicationId,

    -- ** ApplicationResponse
    ApplicationResponse,
    applicationResponse,
    appTags,
    appId,
    appARN,
    appName,

    -- ** ApplicationSettingsResource
    ApplicationSettingsResource,
    applicationSettingsResource,
    asrLastModifiedDate,
    asrLimits,
    asrQuietTime,
    asrCampaignHook,
    asrApplicationId,

    -- ** ApplicationsResponse
    ApplicationsResponse,
    applicationsResponse,
    appNextToken,
    appItem,

    -- ** AttributeDimension
    AttributeDimension,
    attributeDimension,
    adAttributeType,
    adValues,

    -- ** AttributesResource
    AttributesResource,
    attributesResource,
    arAttributes,
    arAttributeType,
    arApplicationId,

    -- ** BaiduChannelRequest
    BaiduChannelRequest,
    baiduChannelRequest,
    bcrEnabled,
    bcrSecretKey,
    bcrAPIKey,

    -- ** BaiduChannelResponse
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

    -- ** BaiduMessage
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

    -- ** BaseKpiResult
    BaseKpiResult,
    baseKpiResult,
    bkrRows,

    -- ** CampaignCustomMessage
    CampaignCustomMessage,
    campaignCustomMessage,
    ccmData,

    -- ** CampaignDateRangeKpiResponse
    CampaignDateRangeKpiResponse,
    campaignDateRangeKpiResponse,
    cdrkNextToken,
    cdrkKpiResult,
    cdrkKpiName,
    cdrkEndTime,
    cdrkCampaignId,
    cdrkStartTime,
    cdrkApplicationId,

    -- ** CampaignEmailMessage
    CampaignEmailMessage,
    campaignEmailMessage,
    cemBody,
    cemFromAddress,
    cemHTMLBody,
    cemTitle,

    -- ** CampaignEventFilter
    CampaignEventFilter,
    campaignEventFilter,
    cefFilterType,
    cefDimensions,

    -- ** CampaignHook
    CampaignHook,
    campaignHook,
    chLambdaFunctionName,
    chMode,
    chWebURL,

    -- ** CampaignLimits
    CampaignLimits,
    campaignLimits,
    clMessagesPerSecond,
    clDaily,
    clTotal,
    clMaximumDuration,

    -- ** CampaignResponse
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

    -- ** CampaignSmsMessage
    CampaignSmsMessage,
    campaignSmsMessage,
    csmBody,
    csmMessageType,
    csmSenderId,

    -- ** CampaignState
    CampaignState,
    campaignState,
    csCampaignStatus,

    -- ** CampaignsResponse
    CampaignsResponse,
    campaignsResponse,
    cNextToken,
    cItem,

    -- ** ChannelResponse
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

    -- ** ChannelsResponse
    ChannelsResponse,
    channelsResponse,
    cChannels,

    -- ** Condition
    Condition,
    condition,
    cOperator,
    cConditions,

    -- ** ConditionalSplitActivity
    ConditionalSplitActivity,
    conditionalSplitActivity,
    csaEvaluationWaitTime,
    csaTrueActivity,
    csaFalseActivity,
    csaCondition,

    -- ** CreateApplicationRequest
    CreateApplicationRequest,
    createApplicationRequest,
    carTags,
    carName,

    -- ** CreateTemplateMessageBody
    CreateTemplateMessageBody,
    createTemplateMessageBody,
    ctmbRequestId,
    ctmbARN,
    ctmbMessage,

    -- ** CustomDeliveryConfiguration
    CustomDeliveryConfiguration,
    customDeliveryConfiguration,
    cdcEndpointTypes,
    cdcDeliveryURI,

    -- ** CustomMessageActivity
    CustomMessageActivity,
    customMessageActivity,
    cmaTemplateName,
    cmaTemplateVersion,
    cmaEndpointTypes,
    cmaNextActivity,
    cmaDeliveryURI,
    cmaMessageConfig,

    -- ** DefaultMessage
    DefaultMessage,
    defaultMessage,
    dmSubstitutions,
    dmBody,

    -- ** DefaultPushNotificationMessage
    DefaultPushNotificationMessage,
    defaultPushNotificationMessage,
    dpnmSubstitutions,
    dpnmSilentPush,
    dpnmData,
    dpnmBody,
    dpnmURL,
    dpnmAction,
    dpnmTitle,

    -- ** DefaultPushNotificationTemplate
    DefaultPushNotificationTemplate,
    defaultPushNotificationTemplate,
    dpntBody,
    dpntURL,
    dpntSound,
    dpntAction,
    dpntTitle,

    -- ** DirectMessageConfiguration
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

    -- ** EmailChannelRequest
    EmailChannelRequest,
    emailChannelRequest,
    ecrEnabled,
    ecrConfigurationSet,
    ecrRoleARN,
    ecrFromAddress,
    ecrIdentity,

    -- ** EmailChannelResponse
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

    -- ** EmailMessage
    EmailMessage,
    emailMessage,
    emSubstitutions,
    emBody,
    emFromAddress,
    emRawEmail,
    emFeedbackForwardingAddress,
    emSimpleEmail,
    emReplyToAddresses,

    -- ** EmailMessageActivity
    EmailMessageActivity,
    emailMessageActivity,
    emaTemplateName,
    emaTemplateVersion,
    emaNextActivity,
    emaMessageConfig,

    -- ** EmailTemplateRequest
    EmailTemplateRequest,
    emailTemplateRequest,
    etrSubject,
    etrTextPart,
    etrTemplateDescription,
    etrDefaultSubstitutions,
    etrHTMLPart,
    etrRecommenderId,
    etrTags,

    -- ** EmailTemplateResponse
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

    -- ** EndpointBatchItem
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

    -- ** EndpointBatchRequest
    EndpointBatchRequest,
    endpointBatchRequest,
    ebrItem,

    -- ** EndpointDemographic
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

    -- ** EndpointItemResponse
    EndpointItemResponse,
    endpointItemResponse,
    eiMessage,
    eiStatusCode,

    -- ** EndpointLocation
    EndpointLocation,
    endpointLocation,
    elPostalCode,
    elLatitude,
    elCountry,
    elCity,
    elRegion,
    elLongitude,

    -- ** EndpointMessageResult
    EndpointMessageResult,
    endpointMessageResult,
    emrAddress,
    emrStatusMessage,
    emrUpdatedToken,
    emrMessageId,
    emrDeliveryStatus,
    emrStatusCode,

    -- ** EndpointRequest
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

    -- ** EndpointResponse
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

    -- ** EndpointSendConfiguration
    EndpointSendConfiguration,
    endpointSendConfiguration,
    escSubstitutions,
    escTitleOverride,
    escContext,
    escRawContent,
    escBodyOverride,

    -- ** EndpointUser
    EndpointUser,
    endpointUser,
    euUserAttributes,
    euUserId,

    -- ** EndpointsResponse
    EndpointsResponse,
    endpointsResponse,
    eItem,

    -- ** Event
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

    -- ** EventCondition
    EventCondition,
    eventCondition,
    ecDimensions,
    ecMessageActivity,

    -- ** EventDimensions
    EventDimensions,
    eventDimensions,
    edMetrics,
    edEventType,
    edAttributes,

    -- ** EventFilter
    EventFilter,
    eventFilter,
    efFilterType,
    efDimensions,

    -- ** EventItemResponse
    EventItemResponse,
    eventItemResponse,
    eMessage,
    eStatusCode,

    -- ** EventStartCondition
    EventStartCondition,
    eventStartCondition,
    escEventFilter,
    escSegmentId,

    -- ** EventStream
    EventStream,
    eventStream,
    esLastUpdatedBy,
    esLastModifiedDate,
    esExternalId,
    esApplicationId,
    esRoleARN,
    esDestinationStreamARN,

    -- ** EventsBatch
    EventsBatch,
    eventsBatch,
    ebEndpoint,
    ebEvents,

    -- ** EventsRequest
    EventsRequest,
    eventsRequest,
    erBatchItem,

    -- ** EventsResponse
    EventsResponse,
    eventsResponse,
    eResults,

    -- ** ExportJobRequest
    ExportJobRequest,
    exportJobRequest,
    eSegmentId,
    eSegmentVersion,
    eS3URLPrefix,
    eRoleARN,

    -- ** ExportJobResource
    ExportJobResource,
    exportJobResource,
    ejrSegmentId,
    ejrSegmentVersion,
    ejrS3URLPrefix,
    ejrRoleARN,

    -- ** ExportJobResponse
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

    -- ** ExportJobsResponse
    ExportJobsResponse,
    exportJobsResponse,
    ejNextToken,
    ejItem,

    -- ** GCMChannelRequest
    GCMChannelRequest,
    gcmChannelRequest,
    gcrEnabled,
    gcrAPIKey,

    -- ** GCMChannelResponse
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

    -- ** GCMMessage
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

    -- ** GPSCoordinates
    GPSCoordinates,
    gPSCoordinates,
    gpscLatitude,
    gpscLongitude,

    -- ** GPSPointDimension
    GPSPointDimension,
    gPSPointDimension,
    gpspdRangeInKilometers,
    gpspdCoordinates,

    -- ** HoldoutActivity
    HoldoutActivity,
    holdoutActivity,
    haNextActivity,
    haPercentage,

    -- ** ImportJobRequest
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

    -- ** ImportJobResource
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

    -- ** ImportJobResponse
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

    -- ** ImportJobsResponse
    ImportJobsResponse,
    importJobsResponse,
    ijNextToken,
    ijItem,

    -- ** ItemResponse
    ItemResponse,
    itemResponse,
    iEndpointItemResponse,
    iEventsItemResponse,

    -- ** JourneyCustomMessage
    JourneyCustomMessage,
    journeyCustomMessage,
    jcmData,

    -- ** JourneyDateRangeKpiResponse
    JourneyDateRangeKpiResponse,
    journeyDateRangeKpiResponse,
    jdrkNextToken,
    jdrkKpiResult,
    jdrkKpiName,
    jdrkJourneyId,
    jdrkEndTime,
    jdrkStartTime,
    jdrkApplicationId,

    -- ** JourneyEmailMessage
    JourneyEmailMessage,
    journeyEmailMessage,
    jemFromAddress,

    -- ** JourneyExecutionActivityMetricsResponse
    JourneyExecutionActivityMetricsResponse,
    journeyExecutionActivityMetricsResponse,
    jeamMetrics,
    jeamJourneyId,
    jeamLastEvaluatedTime,
    jeamJourneyActivityId,
    jeamActivityType,
    jeamApplicationId,

    -- ** JourneyExecutionMetricsResponse
    JourneyExecutionMetricsResponse,
    journeyExecutionMetricsResponse,
    jemMetrics,
    jemJourneyId,
    jemLastEvaluatedTime,
    jemApplicationId,

    -- ** JourneyLimits
    JourneyLimits,
    journeyLimits,
    jlMessagesPerSecond,
    jlEndpointReentryCap,
    jlDailyCap,

    -- ** JourneyPushMessage
    JourneyPushMessage,
    journeyPushMessage,
    jpmTimeToLive,

    -- ** JourneyResponse
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

    -- ** JourneySMSMessage
    JourneySMSMessage,
    journeySMSMessage,
    jsmsmMessageType,
    jsmsmSenderId,

    -- ** JourneySchedule
    JourneySchedule,
    journeySchedule,
    jsStartTime,
    jsEndTime,
    jsTimezone,

    -- ** JourneyStateRequest
    JourneyStateRequest,
    journeyStateRequest,
    jsrState,

    -- ** JourneysResponse
    JourneysResponse,
    journeysResponse,
    jNextToken,
    jItem,

    -- ** ListRecommenderConfigurationsResponse
    ListRecommenderConfigurationsResponse,
    listRecommenderConfigurationsResponse,
    lrcNextToken,
    lrcItem,

    -- ** Message
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

    -- ** MessageBody
    MessageBody,
    messageBody,
    mbRequestId,
    mbMessage,

    -- ** MessageConfiguration
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

    -- ** MessageRequest
    MessageRequest,
    messageRequest,
    mrTraceId,
    mrContext,
    mrAddresses,
    mrTemplateConfiguration,
    mrEndpoints,
    mrMessageConfiguration,

    -- ** MessageResponse
    MessageResponse,
    messageResponse,
    mRequestId,
    mResult,
    mEndpointResult,
    mApplicationId,

    -- ** MessageResult
    MessageResult,
    messageResult,
    mrStatusMessage,
    mrUpdatedToken,
    mrMessageId,
    mrDeliveryStatus,
    mrStatusCode,

    -- ** MetricDimension
    MetricDimension,
    metricDimension,
    mdComparisonOperator,
    mdValue,

    -- ** MultiConditionalBranch
    MultiConditionalBranch,
    multiConditionalBranch,
    mcbNextActivity,
    mcbCondition,

    -- ** MultiConditionalSplitActivity
    MultiConditionalSplitActivity,
    multiConditionalSplitActivity,
    mcsaBranches,
    mcsaEvaluationWaitTime,
    mcsaDefaultActivity,

    -- ** NumberValidateRequest
    NumberValidateRequest,
    numberValidateRequest,
    nvrIsoCountryCode,
    nvrPhoneNumber,

    -- ** NumberValidateResponse
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

    -- ** PublicEndpoint
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

    -- ** PushMessageActivity
    PushMessageActivity,
    pushMessageActivity,
    pmaTemplateName,
    pmaTemplateVersion,
    pmaNextActivity,
    pmaMessageConfig,

    -- ** PushNotificationTemplateRequest
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

    -- ** PushNotificationTemplateResponse
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

    -- ** QuietTime
    QuietTime,
    quietTime,
    qtStart,
    qtEnd,

    -- ** RandomSplitActivity
    RandomSplitActivity,
    randomSplitActivity,
    rsaBranches,

    -- ** RandomSplitEntry
    RandomSplitEntry,
    randomSplitEntry,
    rseNextActivity,
    rsePercentage,

    -- ** RawEmail
    RawEmail,
    rawEmail,
    reData,

    -- ** RecencyDimension
    RecencyDimension,
    recencyDimension,
    rdDuration,
    rdRecencyType,

    -- ** RecommenderConfigurationResponse
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

    -- ** ResultRow
    ResultRow,
    resultRow,
    rrGroupedBys,
    rrValues,

    -- ** ResultRowValue
    ResultRowValue,
    resultRowValue,
    rrvType,
    rrvValue,
    rrvKey,

    -- ** SMSChannelRequest
    SMSChannelRequest,
    sMSChannelRequest,
    smscrShortCode,
    smscrEnabled,
    smscrSenderId,

    -- ** SMSChannelResponse
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

    -- ** SMSMessage
    SMSMessage,
    sMSMessage,
    smsmSubstitutions,
    smsmOriginationNumber,
    smsmBody,
    smsmMessageType,
    smsmSenderId,
    smsmMediaURL,
    smsmKeyword,

    -- ** SMSMessageActivity
    SMSMessageActivity,
    sMSMessageActivity,
    smsmaTemplateName,
    smsmaTemplateVersion,
    smsmaNextActivity,
    smsmaMessageConfig,

    -- ** SMSTemplateRequest
    SMSTemplateRequest,
    sMSTemplateRequest,
    smstrBody,
    smstrTemplateDescription,
    smstrDefaultSubstitutions,
    smstrRecommenderId,
    smstrTags,

    -- ** SMSTemplateResponse
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

    -- ** Schedule
    Schedule,
    schedule,
    sFrequency,
    sQuietTime,
    sEventFilter,
    sIsLocalTime,
    sEndTime,
    sTimezone,
    sStartTime,

    -- ** SegmentBehaviors
    SegmentBehaviors,
    segmentBehaviors,
    sbRecency,

    -- ** SegmentCondition
    SegmentCondition,
    segmentCondition,
    scSegmentId,

    -- ** SegmentDemographics
    SegmentDemographics,
    segmentDemographics,
    sdPlatform,
    sdAppVersion,
    sdChannel,
    sdModel,
    sdMake,
    sdDeviceType,

    -- ** SegmentDimensions
    SegmentDimensions,
    segmentDimensions,
    sdMetrics,
    sdLocation,
    sdDemographic,
    sdUserAttributes,
    sdBehavior,
    sdAttributes,

    -- ** SegmentGroup
    SegmentGroup,
    segmentGroup,
    sgSourceSegments,
    sgSourceType,
    sgType,
    sgDimensions,

    -- ** SegmentGroupList
    SegmentGroupList,
    segmentGroupList,
    sglInclude,
    sglGroups,

    -- ** SegmentImportResource
    SegmentImportResource,
    segmentImportResource,
    sirChannelCounts,
    sirFormat,
    sirS3URL,
    sirSize,
    sirExternalId,
    sirRoleARN,

    -- ** SegmentLocation
    SegmentLocation,
    segmentLocation,
    slCountry,
    slGPSPoint,

    -- ** SegmentReference
    SegmentReference,
    segmentReference,
    srVersion,
    srId,

    -- ** SegmentResponse
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

    -- ** SegmentsResponse
    SegmentsResponse,
    segmentsResponse,
    sNextToken,
    sItem,

    -- ** SendUsersMessageRequest
    SendUsersMessageRequest,
    sendUsersMessageRequest,
    sumrTraceId,
    sumrContext,
    sumrTemplateConfiguration,
    sumrMessageConfiguration,
    sumrUsers,

    -- ** SendUsersMessageResponse
    SendUsersMessageResponse,
    sendUsersMessageResponse,
    sumRequestId,
    sumResult,
    sumApplicationId,

    -- ** Session
    Session,
    session,
    sesStopTimestamp,
    sesDuration,
    sesStartTimestamp,
    sesId,

    -- ** SetDimension
    SetDimension,
    setDimension,
    sdDimensionType,
    sdValues,

    -- ** SimpleCondition
    SimpleCondition,
    simpleCondition,
    scSegmentDimensions,
    scEventCondition,
    scSegmentCondition,

    -- ** SimpleEmail
    SimpleEmail,
    simpleEmail,
    seSubject,
    seTextPart,
    seHTMLPart,

    -- ** SimpleEmailPart
    SimpleEmailPart,
    simpleEmailPart,
    sepData,
    sepCharset,

    -- ** StartCondition
    StartCondition,
    startCondition,
    scSegmentStartCondition,
    scEventStartCondition,
    scDescription,

    -- ** TagsModel
    TagsModel,
    tagsModel,
    tmTags,

    -- ** Template
    Template,
    template,
    tName,
    tVersion,

    -- ** TemplateActiveVersionRequest
    TemplateActiveVersionRequest,
    templateActiveVersionRequest,
    tavrVersion,

    -- ** TemplateConfiguration
    TemplateConfiguration,
    templateConfiguration,
    tcSMSTemplate,
    tcVoiceTemplate,
    tcPushTemplate,
    tcEmailTemplate,

    -- ** TemplateResponse
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

    -- ** TemplateVersionResponse
    TemplateVersionResponse,
    templateVersionResponse,
    tvTemplateDescription,
    tvDefaultSubstitutions,
    tvVersion,
    tvLastModifiedDate,
    tvCreationDate,
    tvTemplateName,
    tvTemplateType,

    -- ** TemplateVersionsResponse
    TemplateVersionsResponse,
    templateVersionsResponse,
    tvRequestId,
    tvNextToken,
    tvMessage,
    tvItem,

    -- ** TemplatesResponse
    TemplatesResponse,
    templatesResponse,
    tNextToken,
    tItem,

    -- ** TreatmentResource
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

    -- ** UpdateAttributesRequest
    UpdateAttributesRequest,
    updateAttributesRequest,
    uarBlacklist,

    -- ** VoiceChannelRequest
    VoiceChannelRequest,
    voiceChannelRequest,
    vcrEnabled,

    -- ** VoiceChannelResponse
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

    -- ** VoiceMessage
    VoiceMessage,
    voiceMessage,
    vmSubstitutions,
    vmLanguageCode,
    vmOriginationNumber,
    vmBody,
    vmVoiceId,

    -- ** VoiceTemplateRequest
    VoiceTemplateRequest,
    voiceTemplateRequest,
    vtrLanguageCode,
    vtrBody,
    vtrTemplateDescription,
    vtrDefaultSubstitutions,
    vtrVoiceId,
    vtrTags,

    -- ** VoiceTemplateResponse
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

    -- ** WaitActivity
    WaitActivity,
    waitActivity,
    waNextActivity,
    waWaitTime,

    -- ** WaitTime
    WaitTime,
    waitTime,
    wtWaitFor,
    wtWaitUntil,

    -- ** WriteApplicationSettingsRequest
    WriteApplicationSettingsRequest,
    writeApplicationSettingsRequest,
    wasrEventTaggingEnabled,
    wasrCloudWatchMetricsEnabled,
    wasrLimits,
    wasrQuietTime,
    wasrCampaignHook,

    -- ** WriteCampaignRequest
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

    -- ** WriteEventStream
    WriteEventStream,
    writeEventStream,
    wesRoleARN,
    wesDestinationStreamARN,

    -- ** WriteJourneyRequest
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

    -- ** WriteSegmentRequest
    WriteSegmentRequest,
    writeSegmentRequest,
    wsrSegmentGroups,
    wsrName,
    wsrDimensions,
    wsrTags,

    -- ** WriteTreatmentResource
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
import Network.AWS.Pinpoint.DeleteADMChannel
import Network.AWS.Pinpoint.DeleteAPNSChannel
import Network.AWS.Pinpoint.DeleteAPNSSandboxChannel
import Network.AWS.Pinpoint.DeleteAPNSVoipChannel
import Network.AWS.Pinpoint.DeleteAPNSVoipSandboxChannel
import Network.AWS.Pinpoint.DeleteApp
import Network.AWS.Pinpoint.DeleteBaiduChannel
import Network.AWS.Pinpoint.DeleteCampaign
import Network.AWS.Pinpoint.DeleteEmailChannel
import Network.AWS.Pinpoint.DeleteEmailTemplate
import Network.AWS.Pinpoint.DeleteEndpoint
import Network.AWS.Pinpoint.DeleteEventStream
import Network.AWS.Pinpoint.DeleteGCMChannel
import Network.AWS.Pinpoint.DeleteJourney
import Network.AWS.Pinpoint.DeletePushTemplate
import Network.AWS.Pinpoint.DeleteRecommenderConfiguration
import Network.AWS.Pinpoint.DeleteSegment
import Network.AWS.Pinpoint.DeleteSmsChannel
import Network.AWS.Pinpoint.DeleteSmsTemplate
import Network.AWS.Pinpoint.DeleteUserEndpoints
import Network.AWS.Pinpoint.DeleteVoiceChannel
import Network.AWS.Pinpoint.DeleteVoiceTemplate
import Network.AWS.Pinpoint.GetADMChannel
import Network.AWS.Pinpoint.GetAPNSChannel
import Network.AWS.Pinpoint.GetAPNSSandboxChannel
import Network.AWS.Pinpoint.GetAPNSVoipChannel
import Network.AWS.Pinpoint.GetAPNSVoipSandboxChannel
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
import Network.AWS.Pinpoint.GetGCMChannel
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
import Network.AWS.Pinpoint.UpdateADMChannel
import Network.AWS.Pinpoint.UpdateAPNSChannel
import Network.AWS.Pinpoint.UpdateAPNSSandboxChannel
import Network.AWS.Pinpoint.UpdateAPNSVoipChannel
import Network.AWS.Pinpoint.UpdateAPNSVoipSandboxChannel
import Network.AWS.Pinpoint.UpdateApplicationSettings
import Network.AWS.Pinpoint.UpdateBaiduChannel
import Network.AWS.Pinpoint.UpdateCampaign
import Network.AWS.Pinpoint.UpdateEmailChannel
import Network.AWS.Pinpoint.UpdateEmailTemplate
import Network.AWS.Pinpoint.UpdateEndpoint
import Network.AWS.Pinpoint.UpdateEndpointsBatch
import Network.AWS.Pinpoint.UpdateGCMChannel
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
