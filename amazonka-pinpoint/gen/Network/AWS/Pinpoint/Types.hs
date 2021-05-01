{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _NotFoundException,
    _BadRequestException,
    _PayloadTooLargeException,
    _InternalServerErrorException,
    _ForbiddenException,
    _ConflictException,
    _MethodNotAllowedException,
    _TooManyRequestsException,

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
    newADMChannelRequest,
    aDMChannelRequest_enabled,
    aDMChannelRequest_clientSecret,
    aDMChannelRequest_clientId,

    -- * ADMChannelResponse
    ADMChannelResponse (..),
    newADMChannelResponse,
    aDMChannelResponse_lastModifiedDate,
    aDMChannelResponse_applicationId,
    aDMChannelResponse_hasCredential,
    aDMChannelResponse_id,
    aDMChannelResponse_creationDate,
    aDMChannelResponse_enabled,
    aDMChannelResponse_version,
    aDMChannelResponse_isArchived,
    aDMChannelResponse_lastModifiedBy,
    aDMChannelResponse_platform,

    -- * ADMMessage
    ADMMessage (..),
    newADMMessage,
    aDMMessage_silentPush,
    aDMMessage_imageIconUrl,
    aDMMessage_expiresAfter,
    aDMMessage_data,
    aDMMessage_title,
    aDMMessage_iconReference,
    aDMMessage_body,
    aDMMessage_consolidationKey,
    aDMMessage_substitutions,
    aDMMessage_imageUrl,
    aDMMessage_md5,
    aDMMessage_action,
    aDMMessage_sound,
    aDMMessage_url,
    aDMMessage_smallImageIconUrl,
    aDMMessage_rawContent,

    -- * APNSChannelRequest
    APNSChannelRequest (..),
    newAPNSChannelRequest,
    aPNSChannelRequest_defaultAuthenticationMethod,
    aPNSChannelRequest_tokenKey,
    aPNSChannelRequest_bundleId,
    aPNSChannelRequest_teamId,
    aPNSChannelRequest_enabled,
    aPNSChannelRequest_privateKey,
    aPNSChannelRequest_tokenKeyId,
    aPNSChannelRequest_certificate,

    -- * APNSChannelResponse
    APNSChannelResponse (..),
    newAPNSChannelResponse,
    aPNSChannelResponse_lastModifiedDate,
    aPNSChannelResponse_applicationId,
    aPNSChannelResponse_defaultAuthenticationMethod,
    aPNSChannelResponse_hasCredential,
    aPNSChannelResponse_hasTokenKey,
    aPNSChannelResponse_id,
    aPNSChannelResponse_creationDate,
    aPNSChannelResponse_enabled,
    aPNSChannelResponse_version,
    aPNSChannelResponse_isArchived,
    aPNSChannelResponse_lastModifiedBy,
    aPNSChannelResponse_platform,

    -- * APNSMessage
    APNSMessage (..),
    newAPNSMessage,
    aPNSMessage_silentPush,
    aPNSMessage_collapseId,
    aPNSMessage_data,
    aPNSMessage_category,
    aPNSMessage_title,
    aPNSMessage_body,
    aPNSMessage_timeToLive,
    aPNSMessage_aPNSPushType,
    aPNSMessage_preferredAuthenticationMethod,
    aPNSMessage_priority,
    aPNSMessage_mediaUrl,
    aPNSMessage_substitutions,
    aPNSMessage_action,
    aPNSMessage_sound,
    aPNSMessage_threadId,
    aPNSMessage_url,
    aPNSMessage_rawContent,
    aPNSMessage_badge,

    -- * APNSPushNotificationTemplate
    APNSPushNotificationTemplate (..),
    newAPNSPushNotificationTemplate,
    aPNSPushNotificationTemplate_title,
    aPNSPushNotificationTemplate_body,
    aPNSPushNotificationTemplate_mediaUrl,
    aPNSPushNotificationTemplate_action,
    aPNSPushNotificationTemplate_sound,
    aPNSPushNotificationTemplate_url,
    aPNSPushNotificationTemplate_rawContent,

    -- * APNSSandboxChannelRequest
    APNSSandboxChannelRequest (..),
    newAPNSSandboxChannelRequest,
    aPNSSandboxChannelRequest_defaultAuthenticationMethod,
    aPNSSandboxChannelRequest_tokenKey,
    aPNSSandboxChannelRequest_bundleId,
    aPNSSandboxChannelRequest_teamId,
    aPNSSandboxChannelRequest_enabled,
    aPNSSandboxChannelRequest_privateKey,
    aPNSSandboxChannelRequest_tokenKeyId,
    aPNSSandboxChannelRequest_certificate,

    -- * APNSSandboxChannelResponse
    APNSSandboxChannelResponse (..),
    newAPNSSandboxChannelResponse,
    aPNSSandboxChannelResponse_lastModifiedDate,
    aPNSSandboxChannelResponse_applicationId,
    aPNSSandboxChannelResponse_defaultAuthenticationMethod,
    aPNSSandboxChannelResponse_hasCredential,
    aPNSSandboxChannelResponse_hasTokenKey,
    aPNSSandboxChannelResponse_id,
    aPNSSandboxChannelResponse_creationDate,
    aPNSSandboxChannelResponse_enabled,
    aPNSSandboxChannelResponse_version,
    aPNSSandboxChannelResponse_isArchived,
    aPNSSandboxChannelResponse_lastModifiedBy,
    aPNSSandboxChannelResponse_platform,

    -- * APNSVoipChannelRequest
    APNSVoipChannelRequest (..),
    newAPNSVoipChannelRequest,
    aPNSVoipChannelRequest_defaultAuthenticationMethod,
    aPNSVoipChannelRequest_tokenKey,
    aPNSVoipChannelRequest_bundleId,
    aPNSVoipChannelRequest_teamId,
    aPNSVoipChannelRequest_enabled,
    aPNSVoipChannelRequest_privateKey,
    aPNSVoipChannelRequest_tokenKeyId,
    aPNSVoipChannelRequest_certificate,

    -- * APNSVoipChannelResponse
    APNSVoipChannelResponse (..),
    newAPNSVoipChannelResponse,
    aPNSVoipChannelResponse_lastModifiedDate,
    aPNSVoipChannelResponse_applicationId,
    aPNSVoipChannelResponse_defaultAuthenticationMethod,
    aPNSVoipChannelResponse_hasCredential,
    aPNSVoipChannelResponse_hasTokenKey,
    aPNSVoipChannelResponse_id,
    aPNSVoipChannelResponse_creationDate,
    aPNSVoipChannelResponse_enabled,
    aPNSVoipChannelResponse_version,
    aPNSVoipChannelResponse_isArchived,
    aPNSVoipChannelResponse_lastModifiedBy,
    aPNSVoipChannelResponse_platform,

    -- * APNSVoipSandboxChannelRequest
    APNSVoipSandboxChannelRequest (..),
    newAPNSVoipSandboxChannelRequest,
    aPNSVoipSandboxChannelRequest_defaultAuthenticationMethod,
    aPNSVoipSandboxChannelRequest_tokenKey,
    aPNSVoipSandboxChannelRequest_bundleId,
    aPNSVoipSandboxChannelRequest_teamId,
    aPNSVoipSandboxChannelRequest_enabled,
    aPNSVoipSandboxChannelRequest_privateKey,
    aPNSVoipSandboxChannelRequest_tokenKeyId,
    aPNSVoipSandboxChannelRequest_certificate,

    -- * APNSVoipSandboxChannelResponse
    APNSVoipSandboxChannelResponse (..),
    newAPNSVoipSandboxChannelResponse,
    aPNSVoipSandboxChannelResponse_lastModifiedDate,
    aPNSVoipSandboxChannelResponse_applicationId,
    aPNSVoipSandboxChannelResponse_defaultAuthenticationMethod,
    aPNSVoipSandboxChannelResponse_hasCredential,
    aPNSVoipSandboxChannelResponse_hasTokenKey,
    aPNSVoipSandboxChannelResponse_id,
    aPNSVoipSandboxChannelResponse_creationDate,
    aPNSVoipSandboxChannelResponse_enabled,
    aPNSVoipSandboxChannelResponse_version,
    aPNSVoipSandboxChannelResponse_isArchived,
    aPNSVoipSandboxChannelResponse_lastModifiedBy,
    aPNSVoipSandboxChannelResponse_platform,

    -- * ActivitiesResponse
    ActivitiesResponse (..),
    newActivitiesResponse,
    activitiesResponse_nextToken,
    activitiesResponse_item,

    -- * Activity
    Activity (..),
    newActivity,
    activity_conditionalSplit,
    activity_push,
    activity_custom,
    activity_wait,
    activity_multiCondition,
    activity_email,
    activity_holdout,
    activity_randomSplit,
    activity_description,
    activity_sms,

    -- * ActivityResponse
    ActivityResponse (..),
    newActivityResponse,
    activityResponse_end,
    activityResponse_successfulEndpointCount,
    activityResponse_result,
    activityResponse_timezonesCompletedCount,
    activityResponse_state,
    activityResponse_timezonesTotalCount,
    activityResponse_treatmentId,
    activityResponse_scheduledStart,
    activityResponse_start,
    activityResponse_totalEndpointCount,
    activityResponse_campaignId,
    activityResponse_id,
    activityResponse_applicationId,

    -- * AddressConfiguration
    AddressConfiguration (..),
    newAddressConfiguration,
    addressConfiguration_channelType,
    addressConfiguration_context,
    addressConfiguration_substitutions,
    addressConfiguration_titleOverride,
    addressConfiguration_bodyOverride,
    addressConfiguration_rawContent,

    -- * AndroidPushNotificationTemplate
    AndroidPushNotificationTemplate (..),
    newAndroidPushNotificationTemplate,
    androidPushNotificationTemplate_imageIconUrl,
    androidPushNotificationTemplate_title,
    androidPushNotificationTemplate_body,
    androidPushNotificationTemplate_imageUrl,
    androidPushNotificationTemplate_action,
    androidPushNotificationTemplate_sound,
    androidPushNotificationTemplate_url,
    androidPushNotificationTemplate_smallImageIconUrl,
    androidPushNotificationTemplate_rawContent,

    -- * ApplicationDateRangeKpiResponse
    ApplicationDateRangeKpiResponse (..),
    newApplicationDateRangeKpiResponse,
    applicationDateRangeKpiResponse_nextToken,
    applicationDateRangeKpiResponse_kpiResult,
    applicationDateRangeKpiResponse_kpiName,
    applicationDateRangeKpiResponse_endTime,
    applicationDateRangeKpiResponse_startTime,
    applicationDateRangeKpiResponse_applicationId,

    -- * ApplicationResponse
    ApplicationResponse (..),
    newApplicationResponse,
    applicationResponse_tags,
    applicationResponse_id,
    applicationResponse_arn,
    applicationResponse_name,

    -- * ApplicationSettingsResource
    ApplicationSettingsResource (..),
    newApplicationSettingsResource,
    applicationSettingsResource_lastModifiedDate,
    applicationSettingsResource_campaignHook,
    applicationSettingsResource_quietTime,
    applicationSettingsResource_limits,
    applicationSettingsResource_applicationId,

    -- * ApplicationsResponse
    ApplicationsResponse (..),
    newApplicationsResponse,
    applicationsResponse_nextToken,
    applicationsResponse_item,

    -- * AttributeDimension
    AttributeDimension (..),
    newAttributeDimension,
    attributeDimension_attributeType,
    attributeDimension_values,

    -- * AttributesResource
    AttributesResource (..),
    newAttributesResource,
    attributesResource_attributes,
    attributesResource_attributeType,
    attributesResource_applicationId,

    -- * BaiduChannelRequest
    BaiduChannelRequest (..),
    newBaiduChannelRequest,
    baiduChannelRequest_enabled,
    baiduChannelRequest_secretKey,
    baiduChannelRequest_apiKey,

    -- * BaiduChannelResponse
    BaiduChannelResponse (..),
    newBaiduChannelResponse,
    baiduChannelResponse_lastModifiedDate,
    baiduChannelResponse_applicationId,
    baiduChannelResponse_hasCredential,
    baiduChannelResponse_id,
    baiduChannelResponse_creationDate,
    baiduChannelResponse_enabled,
    baiduChannelResponse_version,
    baiduChannelResponse_isArchived,
    baiduChannelResponse_lastModifiedBy,
    baiduChannelResponse_credential,
    baiduChannelResponse_platform,

    -- * BaiduMessage
    BaiduMessage (..),
    newBaiduMessage,
    baiduMessage_silentPush,
    baiduMessage_imageIconUrl,
    baiduMessage_data,
    baiduMessage_title,
    baiduMessage_iconReference,
    baiduMessage_body,
    baiduMessage_timeToLive,
    baiduMessage_substitutions,
    baiduMessage_imageUrl,
    baiduMessage_action,
    baiduMessage_sound,
    baiduMessage_url,
    baiduMessage_smallImageIconUrl,
    baiduMessage_rawContent,

    -- * BaseKpiResult
    BaseKpiResult (..),
    newBaseKpiResult,
    baseKpiResult_rows,

    -- * CampaignCustomMessage
    CampaignCustomMessage (..),
    newCampaignCustomMessage,
    campaignCustomMessage_data,

    -- * CampaignDateRangeKpiResponse
    CampaignDateRangeKpiResponse (..),
    newCampaignDateRangeKpiResponse,
    campaignDateRangeKpiResponse_nextToken,
    campaignDateRangeKpiResponse_kpiResult,
    campaignDateRangeKpiResponse_kpiName,
    campaignDateRangeKpiResponse_endTime,
    campaignDateRangeKpiResponse_campaignId,
    campaignDateRangeKpiResponse_startTime,
    campaignDateRangeKpiResponse_applicationId,

    -- * CampaignEmailMessage
    CampaignEmailMessage (..),
    newCampaignEmailMessage,
    campaignEmailMessage_title,
    campaignEmailMessage_body,
    campaignEmailMessage_htmlBody,
    campaignEmailMessage_fromAddress,

    -- * CampaignEventFilter
    CampaignEventFilter (..),
    newCampaignEventFilter,
    campaignEventFilter_filterType,
    campaignEventFilter_dimensions,

    -- * CampaignHook
    CampaignHook (..),
    newCampaignHook,
    campaignHook_mode,
    campaignHook_lambdaFunctionName,
    campaignHook_webUrl,

    -- * CampaignLimits
    CampaignLimits (..),
    newCampaignLimits,
    campaignLimits_total,
    campaignLimits_messagesPerSecond,
    campaignLimits_daily,
    campaignLimits_maximumDuration,

    -- * CampaignResponse
    CampaignResponse (..),
    newCampaignResponse,
    campaignResponse_additionalTreatments,
    campaignResponse_hook,
    campaignResponse_version,
    campaignResponse_customDeliveryConfiguration,
    campaignResponse_state,
    campaignResponse_name,
    campaignResponse_defaultState,
    campaignResponse_isPaused,
    campaignResponse_tags,
    campaignResponse_limits,
    campaignResponse_description,
    campaignResponse_treatmentName,
    campaignResponse_messageConfiguration,
    campaignResponse_templateConfiguration,
    campaignResponse_schedule,
    campaignResponse_holdoutPercent,
    campaignResponse_treatmentDescription,
    campaignResponse_lastModifiedDate,
    campaignResponse_creationDate,
    campaignResponse_segmentId,
    campaignResponse_segmentVersion,
    campaignResponse_id,
    campaignResponse_arn,
    campaignResponse_applicationId,

    -- * CampaignSmsMessage
    CampaignSmsMessage (..),
    newCampaignSmsMessage,
    campaignSmsMessage_entityId,
    campaignSmsMessage_body,
    campaignSmsMessage_senderId,
    campaignSmsMessage_messageType,
    campaignSmsMessage_templateId,
    campaignSmsMessage_originationNumber,

    -- * CampaignState
    CampaignState (..),
    newCampaignState,
    campaignState_campaignStatus,

    -- * CampaignsResponse
    CampaignsResponse (..),
    newCampaignsResponse,
    campaignsResponse_nextToken,
    campaignsResponse_item,

    -- * ChannelResponse
    ChannelResponse (..),
    newChannelResponse,
    channelResponse_lastModifiedDate,
    channelResponse_applicationId,
    channelResponse_hasCredential,
    channelResponse_id,
    channelResponse_creationDate,
    channelResponse_enabled,
    channelResponse_version,
    channelResponse_isArchived,
    channelResponse_lastModifiedBy,

    -- * ChannelsResponse
    ChannelsResponse (..),
    newChannelsResponse,
    channelsResponse_channels,

    -- * Condition
    Condition (..),
    newCondition,
    condition_operator,
    condition_conditions,

    -- * ConditionalSplitActivity
    ConditionalSplitActivity (..),
    newConditionalSplitActivity,
    conditionalSplitActivity_trueActivity,
    conditionalSplitActivity_condition,
    conditionalSplitActivity_evaluationWaitTime,
    conditionalSplitActivity_falseActivity,

    -- * CreateApplicationRequest
    CreateApplicationRequest (..),
    newCreateApplicationRequest,
    createApplicationRequest_tags,
    createApplicationRequest_name,

    -- * CreateRecommenderConfiguration
    CreateRecommenderConfiguration (..),
    newCreateRecommenderConfiguration,
    createRecommenderConfiguration_recommendationsDisplayName,
    createRecommenderConfiguration_recommendationTransformerUri,
    createRecommenderConfiguration_attributes,
    createRecommenderConfiguration_name,
    createRecommenderConfiguration_recommendationsPerMessage,
    createRecommenderConfiguration_description,
    createRecommenderConfiguration_recommendationProviderIdType,
    createRecommenderConfiguration_recommendationProviderUri,
    createRecommenderConfiguration_recommendationProviderRoleArn,

    -- * CreateTemplateMessageBody
    CreateTemplateMessageBody (..),
    newCreateTemplateMessageBody,
    createTemplateMessageBody_message,
    createTemplateMessageBody_arn,
    createTemplateMessageBody_requestID,

    -- * CustomDeliveryConfiguration
    CustomDeliveryConfiguration (..),
    newCustomDeliveryConfiguration,
    customDeliveryConfiguration_endpointTypes,
    customDeliveryConfiguration_deliveryUri,

    -- * CustomMessageActivity
    CustomMessageActivity (..),
    newCustomMessageActivity,
    customMessageActivity_templateName,
    customMessageActivity_messageConfig,
    customMessageActivity_deliveryUri,
    customMessageActivity_endpointTypes,
    customMessageActivity_nextActivity,
    customMessageActivity_templateVersion,

    -- * DefaultMessage
    DefaultMessage (..),
    newDefaultMessage,
    defaultMessage_body,
    defaultMessage_substitutions,

    -- * DefaultPushNotificationMessage
    DefaultPushNotificationMessage (..),
    newDefaultPushNotificationMessage,
    defaultPushNotificationMessage_silentPush,
    defaultPushNotificationMessage_data,
    defaultPushNotificationMessage_title,
    defaultPushNotificationMessage_body,
    defaultPushNotificationMessage_substitutions,
    defaultPushNotificationMessage_action,
    defaultPushNotificationMessage_url,

    -- * DefaultPushNotificationTemplate
    DefaultPushNotificationTemplate (..),
    newDefaultPushNotificationTemplate,
    defaultPushNotificationTemplate_title,
    defaultPushNotificationTemplate_body,
    defaultPushNotificationTemplate_action,
    defaultPushNotificationTemplate_sound,
    defaultPushNotificationTemplate_url,

    -- * DirectMessageConfiguration
    DirectMessageConfiguration (..),
    newDirectMessageConfiguration,
    directMessageConfiguration_aDMMessage,
    directMessageConfiguration_defaultMessage,
    directMessageConfiguration_voiceMessage,
    directMessageConfiguration_gCMMessage,
    directMessageConfiguration_aPNSMessage,
    directMessageConfiguration_emailMessage,
    directMessageConfiguration_sMSMessage,
    directMessageConfiguration_defaultPushNotificationMessage,
    directMessageConfiguration_baiduMessage,

    -- * EmailChannelRequest
    EmailChannelRequest (..),
    newEmailChannelRequest,
    emailChannelRequest_roleArn,
    emailChannelRequest_enabled,
    emailChannelRequest_configurationSet,
    emailChannelRequest_fromAddress,
    emailChannelRequest_identity,

    -- * EmailChannelResponse
    EmailChannelResponse (..),
    newEmailChannelResponse,
    emailChannelResponse_lastModifiedDate,
    emailChannelResponse_applicationId,
    emailChannelResponse_roleArn,
    emailChannelResponse_hasCredential,
    emailChannelResponse_identity,
    emailChannelResponse_id,
    emailChannelResponse_creationDate,
    emailChannelResponse_enabled,
    emailChannelResponse_version,
    emailChannelResponse_messagesPerSecond,
    emailChannelResponse_isArchived,
    emailChannelResponse_fromAddress,
    emailChannelResponse_lastModifiedBy,
    emailChannelResponse_configurationSet,
    emailChannelResponse_platform,

    -- * EmailMessage
    EmailMessage (..),
    newEmailMessage,
    emailMessage_feedbackForwardingAddress,
    emailMessage_rawEmail,
    emailMessage_body,
    emailMessage_simpleEmail,
    emailMessage_substitutions,
    emailMessage_replyToAddresses,
    emailMessage_fromAddress,

    -- * EmailMessageActivity
    EmailMessageActivity (..),
    newEmailMessageActivity,
    emailMessageActivity_templateName,
    emailMessageActivity_messageConfig,
    emailMessageActivity_nextActivity,
    emailMessageActivity_templateVersion,

    -- * EmailTemplateRequest
    EmailTemplateRequest (..),
    newEmailTemplateRequest,
    emailTemplateRequest_templateDescription,
    emailTemplateRequest_textPart,
    emailTemplateRequest_defaultSubstitutions,
    emailTemplateRequest_subject,
    emailTemplateRequest_tags,
    emailTemplateRequest_recommenderId,
    emailTemplateRequest_htmlPart,

    -- * EmailTemplateResponse
    EmailTemplateResponse (..),
    newEmailTemplateResponse,
    emailTemplateResponse_templateDescription,
    emailTemplateResponse_arn,
    emailTemplateResponse_version,
    emailTemplateResponse_textPart,
    emailTemplateResponse_defaultSubstitutions,
    emailTemplateResponse_subject,
    emailTemplateResponse_tags,
    emailTemplateResponse_recommenderId,
    emailTemplateResponse_htmlPart,
    emailTemplateResponse_lastModifiedDate,
    emailTemplateResponse_creationDate,
    emailTemplateResponse_templateName,
    emailTemplateResponse_templateType,

    -- * EndpointBatchItem
    EndpointBatchItem (..),
    newEndpointBatchItem,
    endpointBatchItem_user,
    endpointBatchItem_address,
    endpointBatchItem_channelType,
    endpointBatchItem_id,
    endpointBatchItem_optOut,
    endpointBatchItem_demographic,
    endpointBatchItem_attributes,
    endpointBatchItem_endpointStatus,
    endpointBatchItem_metrics,
    endpointBatchItem_requestId,
    endpointBatchItem_effectiveDate,
    endpointBatchItem_location,

    -- * EndpointBatchRequest
    EndpointBatchRequest (..),
    newEndpointBatchRequest,
    endpointBatchRequest_item,

    -- * EndpointDemographic
    EndpointDemographic (..),
    newEndpointDemographic,
    endpointDemographic_model,
    endpointDemographic_platform,
    endpointDemographic_appVersion,
    endpointDemographic_locale,
    endpointDemographic_platformVersion,
    endpointDemographic_modelVersion,
    endpointDemographic_timezone,
    endpointDemographic_make,

    -- * EndpointItemResponse
    EndpointItemResponse (..),
    newEndpointItemResponse,
    endpointItemResponse_message,
    endpointItemResponse_statusCode,

    -- * EndpointLocation
    EndpointLocation (..),
    newEndpointLocation,
    endpointLocation_longitude,
    endpointLocation_latitude,
    endpointLocation_postalCode,
    endpointLocation_city,
    endpointLocation_country,
    endpointLocation_region,

    -- * EndpointMessageResult
    EndpointMessageResult (..),
    newEndpointMessageResult,
    endpointMessageResult_statusMessage,
    endpointMessageResult_updatedToken,
    endpointMessageResult_address,
    endpointMessageResult_messageId,
    endpointMessageResult_deliveryStatus,
    endpointMessageResult_statusCode,

    -- * EndpointRequest
    EndpointRequest (..),
    newEndpointRequest,
    endpointRequest_user,
    endpointRequest_address,
    endpointRequest_channelType,
    endpointRequest_optOut,
    endpointRequest_demographic,
    endpointRequest_attributes,
    endpointRequest_endpointStatus,
    endpointRequest_metrics,
    endpointRequest_requestId,
    endpointRequest_effectiveDate,
    endpointRequest_location,

    -- * EndpointResponse
    EndpointResponse (..),
    newEndpointResponse,
    endpointResponse_applicationId,
    endpointResponse_user,
    endpointResponse_address,
    endpointResponse_channelType,
    endpointResponse_cohortId,
    endpointResponse_id,
    endpointResponse_creationDate,
    endpointResponse_optOut,
    endpointResponse_demographic,
    endpointResponse_attributes,
    endpointResponse_endpointStatus,
    endpointResponse_metrics,
    endpointResponse_requestId,
    endpointResponse_effectiveDate,
    endpointResponse_location,

    -- * EndpointSendConfiguration
    EndpointSendConfiguration (..),
    newEndpointSendConfiguration,
    endpointSendConfiguration_context,
    endpointSendConfiguration_substitutions,
    endpointSendConfiguration_titleOverride,
    endpointSendConfiguration_bodyOverride,
    endpointSendConfiguration_rawContent,

    -- * EndpointUser
    EndpointUser (..),
    newEndpointUser,
    endpointUser_userId,
    endpointUser_userAttributes,

    -- * EndpointsResponse
    EndpointsResponse (..),
    newEndpointsResponse,
    endpointsResponse_item,

    -- * Event
    Event (..),
    newEvent,
    event_clientSdkVersion,
    event_appTitle,
    event_sdkName,
    event_attributes,
    event_metrics,
    event_appPackageName,
    event_appVersionCode,
    event_session,
    event_eventType,
    event_timestamp,

    -- * EventCondition
    EventCondition (..),
    newEventCondition,
    eventCondition_messageActivity,
    eventCondition_dimensions,

    -- * EventDimensions
    EventDimensions (..),
    newEventDimensions,
    eventDimensions_eventType,
    eventDimensions_attributes,
    eventDimensions_metrics,

    -- * EventFilter
    EventFilter (..),
    newEventFilter,
    eventFilter_filterType,
    eventFilter_dimensions,

    -- * EventItemResponse
    EventItemResponse (..),
    newEventItemResponse,
    eventItemResponse_message,
    eventItemResponse_statusCode,

    -- * EventStartCondition
    EventStartCondition (..),
    newEventStartCondition,
    eventStartCondition_eventFilter,
    eventStartCondition_segmentId,

    -- * EventStream
    EventStream (..),
    newEventStream,
    eventStream_lastModifiedDate,
    eventStream_lastUpdatedBy,
    eventStream_externalId,
    eventStream_applicationId,
    eventStream_roleArn,
    eventStream_destinationStreamArn,

    -- * EventsBatch
    EventsBatch (..),
    newEventsBatch,
    eventsBatch_endpoint,
    eventsBatch_events,

    -- * EventsRequest
    EventsRequest (..),
    newEventsRequest,
    eventsRequest_batchItem,

    -- * EventsResponse
    EventsResponse (..),
    newEventsResponse,
    eventsResponse_results,

    -- * ExportJobRequest
    ExportJobRequest (..),
    newExportJobRequest,
    exportJobRequest_segmentVersion,
    exportJobRequest_segmentId,
    exportJobRequest_s3UrlPrefix,
    exportJobRequest_roleArn,

    -- * ExportJobResource
    ExportJobResource (..),
    newExportJobResource,
    exportJobResource_segmentVersion,
    exportJobResource_segmentId,
    exportJobResource_s3UrlPrefix,
    exportJobResource_roleArn,

    -- * ExportJobResponse
    ExportJobResponse (..),
    newExportJobResponse,
    exportJobResponse_totalFailures,
    exportJobResponse_failures,
    exportJobResponse_totalProcessed,
    exportJobResponse_failedPieces,
    exportJobResponse_completedPieces,
    exportJobResponse_totalPieces,
    exportJobResponse_completionDate,
    exportJobResponse_jobStatus,
    exportJobResponse_creationDate,
    exportJobResponse_type,
    exportJobResponse_definition,
    exportJobResponse_id,
    exportJobResponse_applicationId,

    -- * ExportJobsResponse
    ExportJobsResponse (..),
    newExportJobsResponse,
    exportJobsResponse_nextToken,
    exportJobsResponse_item,

    -- * GCMChannelRequest
    GCMChannelRequest (..),
    newGCMChannelRequest,
    gCMChannelRequest_enabled,
    gCMChannelRequest_apiKey,

    -- * GCMChannelResponse
    GCMChannelResponse (..),
    newGCMChannelResponse,
    gCMChannelResponse_lastModifiedDate,
    gCMChannelResponse_applicationId,
    gCMChannelResponse_hasCredential,
    gCMChannelResponse_id,
    gCMChannelResponse_creationDate,
    gCMChannelResponse_enabled,
    gCMChannelResponse_version,
    gCMChannelResponse_isArchived,
    gCMChannelResponse_lastModifiedBy,
    gCMChannelResponse_credential,
    gCMChannelResponse_platform,

    -- * GCMMessage
    GCMMessage (..),
    newGCMMessage,
    gCMMessage_silentPush,
    gCMMessage_imageIconUrl,
    gCMMessage_collapseKey,
    gCMMessage_data,
    gCMMessage_title,
    gCMMessage_iconReference,
    gCMMessage_body,
    gCMMessage_timeToLive,
    gCMMessage_priority,
    gCMMessage_substitutions,
    gCMMessage_imageUrl,
    gCMMessage_action,
    gCMMessage_sound,
    gCMMessage_url,
    gCMMessage_smallImageIconUrl,
    gCMMessage_restrictedPackageName,
    gCMMessage_rawContent,

    -- * GPSCoordinates
    GPSCoordinates (..),
    newGPSCoordinates,
    gPSCoordinates_latitude,
    gPSCoordinates_longitude,

    -- * GPSPointDimension
    GPSPointDimension (..),
    newGPSPointDimension,
    gPSPointDimension_rangeInKilometers,
    gPSPointDimension_coordinates,

    -- * HoldoutActivity
    HoldoutActivity (..),
    newHoldoutActivity,
    holdoutActivity_nextActivity,
    holdoutActivity_percentage,

    -- * ImportJobRequest
    ImportJobRequest (..),
    newImportJobRequest,
    importJobRequest_defineSegment,
    importJobRequest_segmentName,
    importJobRequest_registerEndpoints,
    importJobRequest_segmentId,
    importJobRequest_externalId,
    importJobRequest_format,
    importJobRequest_s3Url,
    importJobRequest_roleArn,

    -- * ImportJobResource
    ImportJobResource (..),
    newImportJobResource,
    importJobResource_defineSegment,
    importJobResource_segmentName,
    importJobResource_registerEndpoints,
    importJobResource_segmentId,
    importJobResource_externalId,
    importJobResource_format,
    importJobResource_s3Url,
    importJobResource_roleArn,

    -- * ImportJobResponse
    ImportJobResponse (..),
    newImportJobResponse,
    importJobResponse_totalFailures,
    importJobResponse_failures,
    importJobResponse_totalProcessed,
    importJobResponse_failedPieces,
    importJobResponse_completedPieces,
    importJobResponse_totalPieces,
    importJobResponse_completionDate,
    importJobResponse_jobStatus,
    importJobResponse_creationDate,
    importJobResponse_type,
    importJobResponse_definition,
    importJobResponse_id,
    importJobResponse_applicationId,

    -- * ImportJobsResponse
    ImportJobsResponse (..),
    newImportJobsResponse,
    importJobsResponse_nextToken,
    importJobsResponse_item,

    -- * ItemResponse
    ItemResponse (..),
    newItemResponse,
    itemResponse_eventsItemResponse,
    itemResponse_endpointItemResponse,

    -- * JourneyCustomMessage
    JourneyCustomMessage (..),
    newJourneyCustomMessage,
    journeyCustomMessage_data,

    -- * JourneyDateRangeKpiResponse
    JourneyDateRangeKpiResponse (..),
    newJourneyDateRangeKpiResponse,
    journeyDateRangeKpiResponse_nextToken,
    journeyDateRangeKpiResponse_kpiResult,
    journeyDateRangeKpiResponse_kpiName,
    journeyDateRangeKpiResponse_journeyId,
    journeyDateRangeKpiResponse_endTime,
    journeyDateRangeKpiResponse_startTime,
    journeyDateRangeKpiResponse_applicationId,

    -- * JourneyEmailMessage
    JourneyEmailMessage (..),
    newJourneyEmailMessage,
    journeyEmailMessage_fromAddress,

    -- * JourneyExecutionActivityMetricsResponse
    JourneyExecutionActivityMetricsResponse (..),
    newJourneyExecutionActivityMetricsResponse,
    journeyExecutionActivityMetricsResponse_metrics,
    journeyExecutionActivityMetricsResponse_journeyId,
    journeyExecutionActivityMetricsResponse_lastEvaluatedTime,
    journeyExecutionActivityMetricsResponse_journeyActivityId,
    journeyExecutionActivityMetricsResponse_activityType,
    journeyExecutionActivityMetricsResponse_applicationId,

    -- * JourneyExecutionMetricsResponse
    JourneyExecutionMetricsResponse (..),
    newJourneyExecutionMetricsResponse,
    journeyExecutionMetricsResponse_metrics,
    journeyExecutionMetricsResponse_journeyId,
    journeyExecutionMetricsResponse_lastEvaluatedTime,
    journeyExecutionMetricsResponse_applicationId,

    -- * JourneyLimits
    JourneyLimits (..),
    newJourneyLimits,
    journeyLimits_endpointReentryCap,
    journeyLimits_messagesPerSecond,
    journeyLimits_dailyCap,

    -- * JourneyPushMessage
    JourneyPushMessage (..),
    newJourneyPushMessage,
    journeyPushMessage_timeToLive,

    -- * JourneyResponse
    JourneyResponse (..),
    newJourneyResponse,
    journeyResponse_lastModifiedDate,
    journeyResponse_activities,
    journeyResponse_creationDate,
    journeyResponse_state,
    journeyResponse_tags,
    journeyResponse_quietTime,
    journeyResponse_refreshFrequency,
    journeyResponse_limits,
    journeyResponse_startCondition,
    journeyResponse_localTime,
    journeyResponse_startActivity,
    journeyResponse_schedule,
    journeyResponse_name,
    journeyResponse_id,
    journeyResponse_applicationId,

    -- * JourneySMSMessage
    JourneySMSMessage (..),
    newJourneySMSMessage,
    journeySMSMessage_entityId,
    journeySMSMessage_senderId,
    journeySMSMessage_messageType,
    journeySMSMessage_templateId,
    journeySMSMessage_originationNumber,

    -- * JourneySchedule
    JourneySchedule (..),
    newJourneySchedule,
    journeySchedule_startTime,
    journeySchedule_endTime,
    journeySchedule_timezone,

    -- * JourneyStateRequest
    JourneyStateRequest (..),
    newJourneyStateRequest,
    journeyStateRequest_state,

    -- * JourneysResponse
    JourneysResponse (..),
    newJourneysResponse,
    journeysResponse_nextToken,
    journeysResponse_item,

    -- * ListRecommenderConfigurationsResponse
    ListRecommenderConfigurationsResponse (..),
    newListRecommenderConfigurationsResponse,
    listRecommenderConfigurationsResponse_nextToken,
    listRecommenderConfigurationsResponse_item,

    -- * Message
    Message (..),
    newMessage,
    message_silentPush,
    message_imageIconUrl,
    message_title,
    message_jsonBody,
    message_body,
    message_timeToLive,
    message_mediaUrl,
    message_imageUrl,
    message_action,
    message_url,
    message_imageSmallIconUrl,
    message_rawContent,

    -- * MessageBody
    MessageBody (..),
    newMessageBody,
    messageBody_message,
    messageBody_requestID,

    -- * MessageConfiguration
    MessageConfiguration (..),
    newMessageConfiguration,
    messageConfiguration_aDMMessage,
    messageConfiguration_defaultMessage,
    messageConfiguration_gCMMessage,
    messageConfiguration_aPNSMessage,
    messageConfiguration_emailMessage,
    messageConfiguration_sMSMessage,
    messageConfiguration_baiduMessage,
    messageConfiguration_customMessage,

    -- * MessageRequest
    MessageRequest (..),
    newMessageRequest,
    messageRequest_endpoints,
    messageRequest_context,
    messageRequest_traceId,
    messageRequest_addresses,
    messageRequest_templateConfiguration,
    messageRequest_messageConfiguration,

    -- * MessageResponse
    MessageResponse (..),
    newMessageResponse,
    messageResponse_result,
    messageResponse_requestId,
    messageResponse_endpointResult,
    messageResponse_applicationId,

    -- * MessageResult
    MessageResult (..),
    newMessageResult,
    messageResult_statusMessage,
    messageResult_updatedToken,
    messageResult_messageId,
    messageResult_deliveryStatus,
    messageResult_statusCode,

    -- * MetricDimension
    MetricDimension (..),
    newMetricDimension,
    metricDimension_comparisonOperator,
    metricDimension_value,

    -- * MultiConditionalBranch
    MultiConditionalBranch (..),
    newMultiConditionalBranch,
    multiConditionalBranch_condition,
    multiConditionalBranch_nextActivity,

    -- * MultiConditionalSplitActivity
    MultiConditionalSplitActivity (..),
    newMultiConditionalSplitActivity,
    multiConditionalSplitActivity_defaultActivity,
    multiConditionalSplitActivity_evaluationWaitTime,
    multiConditionalSplitActivity_branches,

    -- * NumberValidateRequest
    NumberValidateRequest (..),
    newNumberValidateRequest,
    numberValidateRequest_phoneNumber,
    numberValidateRequest_isoCountryCode,

    -- * NumberValidateResponse
    NumberValidateResponse (..),
    newNumberValidateResponse,
    numberValidateResponse_phoneType,
    numberValidateResponse_originalPhoneNumber,
    numberValidateResponse_zipCode,
    numberValidateResponse_originalCountryCodeIso2,
    numberValidateResponse_countryCodeIso2,
    numberValidateResponse_county,
    numberValidateResponse_city,
    numberValidateResponse_carrier,
    numberValidateResponse_phoneTypeCode,
    numberValidateResponse_cleansedPhoneNumberNational,
    numberValidateResponse_cleansedPhoneNumberE164,
    numberValidateResponse_countryCodeNumeric,
    numberValidateResponse_timezone,
    numberValidateResponse_country,

    -- * PublicEndpoint
    PublicEndpoint (..),
    newPublicEndpoint,
    publicEndpoint_user,
    publicEndpoint_address,
    publicEndpoint_channelType,
    publicEndpoint_optOut,
    publicEndpoint_demographic,
    publicEndpoint_attributes,
    publicEndpoint_endpointStatus,
    publicEndpoint_metrics,
    publicEndpoint_requestId,
    publicEndpoint_effectiveDate,
    publicEndpoint_location,

    -- * PushMessageActivity
    PushMessageActivity (..),
    newPushMessageActivity,
    pushMessageActivity_templateName,
    pushMessageActivity_messageConfig,
    pushMessageActivity_nextActivity,
    pushMessageActivity_templateVersion,

    -- * PushNotificationTemplateRequest
    PushNotificationTemplateRequest (..),
    newPushNotificationTemplateRequest,
    pushNotificationTemplateRequest_templateDescription,
    pushNotificationTemplateRequest_baidu,
    pushNotificationTemplateRequest_adm,
    pushNotificationTemplateRequest_defaultSubstitutions,
    pushNotificationTemplateRequest_apns,
    pushNotificationTemplateRequest_gcm,
    pushNotificationTemplateRequest_tags,
    pushNotificationTemplateRequest_recommenderId,
    pushNotificationTemplateRequest_default,

    -- * PushNotificationTemplateResponse
    PushNotificationTemplateResponse (..),
    newPushNotificationTemplateResponse,
    pushNotificationTemplateResponse_templateDescription,
    pushNotificationTemplateResponse_baidu,
    pushNotificationTemplateResponse_adm,
    pushNotificationTemplateResponse_arn,
    pushNotificationTemplateResponse_version,
    pushNotificationTemplateResponse_defaultSubstitutions,
    pushNotificationTemplateResponse_apns,
    pushNotificationTemplateResponse_gcm,
    pushNotificationTemplateResponse_tags,
    pushNotificationTemplateResponse_recommenderId,
    pushNotificationTemplateResponse_default,
    pushNotificationTemplateResponse_lastModifiedDate,
    pushNotificationTemplateResponse_creationDate,
    pushNotificationTemplateResponse_templateType,
    pushNotificationTemplateResponse_templateName,

    -- * QuietTime
    QuietTime (..),
    newQuietTime,
    quietTime_end,
    quietTime_start,

    -- * RandomSplitActivity
    RandomSplitActivity (..),
    newRandomSplitActivity,
    randomSplitActivity_branches,

    -- * RandomSplitEntry
    RandomSplitEntry (..),
    newRandomSplitEntry,
    randomSplitEntry_percentage,
    randomSplitEntry_nextActivity,

    -- * RawEmail
    RawEmail (..),
    newRawEmail,
    rawEmail_data,

    -- * RecencyDimension
    RecencyDimension (..),
    newRecencyDimension,
    recencyDimension_duration,
    recencyDimension_recencyType,

    -- * RecommenderConfigurationResponse
    RecommenderConfigurationResponse (..),
    newRecommenderConfigurationResponse,
    recommenderConfigurationResponse_recommendationsDisplayName,
    recommenderConfigurationResponse_recommendationTransformerUri,
    recommenderConfigurationResponse_attributes,
    recommenderConfigurationResponse_name,
    recommenderConfigurationResponse_recommendationsPerMessage,
    recommenderConfigurationResponse_description,
    recommenderConfigurationResponse_recommendationProviderIdType,
    recommenderConfigurationResponse_recommendationProviderUri,
    recommenderConfigurationResponse_lastModifiedDate,
    recommenderConfigurationResponse_creationDate,
    recommenderConfigurationResponse_recommendationProviderRoleArn,
    recommenderConfigurationResponse_id,

    -- * ResultRow
    ResultRow (..),
    newResultRow,
    resultRow_groupedBys,
    resultRow_values,

    -- * ResultRowValue
    ResultRowValue (..),
    newResultRowValue,
    resultRowValue_type,
    resultRowValue_value,
    resultRowValue_key,

    -- * SMSChannelRequest
    SMSChannelRequest (..),
    newSMSChannelRequest,
    sMSChannelRequest_enabled,
    sMSChannelRequest_shortCode,
    sMSChannelRequest_senderId,

    -- * SMSChannelResponse
    SMSChannelResponse (..),
    newSMSChannelResponse,
    sMSChannelResponse_lastModifiedDate,
    sMSChannelResponse_applicationId,
    sMSChannelResponse_promotionalMessagesPerSecond,
    sMSChannelResponse_hasCredential,
    sMSChannelResponse_id,
    sMSChannelResponse_creationDate,
    sMSChannelResponse_enabled,
    sMSChannelResponse_version,
    sMSChannelResponse_shortCode,
    sMSChannelResponse_isArchived,
    sMSChannelResponse_senderId,
    sMSChannelResponse_transactionalMessagesPerSecond,
    sMSChannelResponse_lastModifiedBy,
    sMSChannelResponse_platform,

    -- * SMSMessage
    SMSMessage (..),
    newSMSMessage,
    sMSMessage_keyword,
    sMSMessage_entityId,
    sMSMessage_body,
    sMSMessage_mediaUrl,
    sMSMessage_substitutions,
    sMSMessage_senderId,
    sMSMessage_messageType,
    sMSMessage_templateId,
    sMSMessage_originationNumber,

    -- * SMSMessageActivity
    SMSMessageActivity (..),
    newSMSMessageActivity,
    sMSMessageActivity_templateName,
    sMSMessageActivity_messageConfig,
    sMSMessageActivity_nextActivity,
    sMSMessageActivity_templateVersion,

    -- * SMSTemplateRequest
    SMSTemplateRequest (..),
    newSMSTemplateRequest,
    sMSTemplateRequest_templateDescription,
    sMSTemplateRequest_body,
    sMSTemplateRequest_defaultSubstitutions,
    sMSTemplateRequest_tags,
    sMSTemplateRequest_recommenderId,

    -- * SMSTemplateResponse
    SMSTemplateResponse (..),
    newSMSTemplateResponse,
    sMSTemplateResponse_templateDescription,
    sMSTemplateResponse_body,
    sMSTemplateResponse_arn,
    sMSTemplateResponse_version,
    sMSTemplateResponse_defaultSubstitutions,
    sMSTemplateResponse_tags,
    sMSTemplateResponse_recommenderId,
    sMSTemplateResponse_lastModifiedDate,
    sMSTemplateResponse_creationDate,
    sMSTemplateResponse_templateName,
    sMSTemplateResponse_templateType,

    -- * Schedule
    Schedule (..),
    newSchedule,
    schedule_eventFilter,
    schedule_isLocalTime,
    schedule_endTime,
    schedule_frequency,
    schedule_quietTime,
    schedule_timezone,
    schedule_startTime,

    -- * SegmentBehaviors
    SegmentBehaviors (..),
    newSegmentBehaviors,
    segmentBehaviors_recency,

    -- * SegmentCondition
    SegmentCondition (..),
    newSegmentCondition,
    segmentCondition_segmentId,

    -- * SegmentDemographics
    SegmentDemographics (..),
    newSegmentDemographics,
    segmentDemographics_model,
    segmentDemographics_platform,
    segmentDemographics_appVersion,
    segmentDemographics_channel,
    segmentDemographics_deviceType,
    segmentDemographics_make,

    -- * SegmentDimensions
    SegmentDimensions (..),
    newSegmentDimensions,
    segmentDimensions_demographic,
    segmentDimensions_attributes,
    segmentDimensions_metrics,
    segmentDimensions_behavior,
    segmentDimensions_userAttributes,
    segmentDimensions_location,

    -- * SegmentGroup
    SegmentGroup (..),
    newSegmentGroup,
    segmentGroup_dimensions,
    segmentGroup_type,
    segmentGroup_sourceSegments,
    segmentGroup_sourceType,

    -- * SegmentGroupList
    SegmentGroupList (..),
    newSegmentGroupList,
    segmentGroupList_groups,
    segmentGroupList_include,

    -- * SegmentImportResource
    SegmentImportResource (..),
    newSegmentImportResource,
    segmentImportResource_channelCounts,
    segmentImportResource_format,
    segmentImportResource_s3Url,
    segmentImportResource_size,
    segmentImportResource_externalId,
    segmentImportResource_roleArn,

    -- * SegmentLocation
    SegmentLocation (..),
    newSegmentLocation,
    segmentLocation_gPSPoint,
    segmentLocation_country,

    -- * SegmentReference
    SegmentReference (..),
    newSegmentReference,
    segmentReference_version,
    segmentReference_id,

    -- * SegmentResponse
    SegmentResponse (..),
    newSegmentResponse,
    segmentResponse_lastModifiedDate,
    segmentResponse_segmentGroups,
    segmentResponse_version,
    segmentResponse_name,
    segmentResponse_tags,
    segmentResponse_dimensions,
    segmentResponse_importDefinition,
    segmentResponse_segmentType,
    segmentResponse_creationDate,
    segmentResponse_id,
    segmentResponse_arn,
    segmentResponse_applicationId,

    -- * SegmentsResponse
    SegmentsResponse (..),
    newSegmentsResponse,
    segmentsResponse_nextToken,
    segmentsResponse_item,

    -- * SendUsersMessageRequest
    SendUsersMessageRequest (..),
    newSendUsersMessageRequest,
    sendUsersMessageRequest_context,
    sendUsersMessageRequest_traceId,
    sendUsersMessageRequest_templateConfiguration,
    sendUsersMessageRequest_messageConfiguration,
    sendUsersMessageRequest_users,

    -- * SendUsersMessageResponse
    SendUsersMessageResponse (..),
    newSendUsersMessageResponse,
    sendUsersMessageResponse_result,
    sendUsersMessageResponse_requestId,
    sendUsersMessageResponse_applicationId,

    -- * Session
    Session (..),
    newSession,
    session_stopTimestamp,
    session_duration,
    session_startTimestamp,
    session_id,

    -- * SetDimension
    SetDimension (..),
    newSetDimension,
    setDimension_dimensionType,
    setDimension_values,

    -- * SimpleCondition
    SimpleCondition (..),
    newSimpleCondition,
    simpleCondition_eventCondition,
    simpleCondition_segmentDimensions,
    simpleCondition_segmentCondition,

    -- * SimpleEmail
    SimpleEmail (..),
    newSimpleEmail,
    simpleEmail_textPart,
    simpleEmail_subject,
    simpleEmail_htmlPart,

    -- * SimpleEmailPart
    SimpleEmailPart (..),
    newSimpleEmailPart,
    simpleEmailPart_data,
    simpleEmailPart_charset,

    -- * StartCondition
    StartCondition (..),
    newStartCondition,
    startCondition_eventStartCondition,
    startCondition_description,
    startCondition_segmentStartCondition,

    -- * TagsModel
    TagsModel (..),
    newTagsModel,
    tagsModel_tags,

    -- * Template
    Template (..),
    newTemplate,
    template_version,
    template_name,

    -- * TemplateActiveVersionRequest
    TemplateActiveVersionRequest (..),
    newTemplateActiveVersionRequest,
    templateActiveVersionRequest_version,

    -- * TemplateConfiguration
    TemplateConfiguration (..),
    newTemplateConfiguration,
    templateConfiguration_emailTemplate,
    templateConfiguration_voiceTemplate,
    templateConfiguration_sMSTemplate,
    templateConfiguration_pushTemplate,

    -- * TemplateResponse
    TemplateResponse (..),
    newTemplateResponse,
    templateResponse_templateDescription,
    templateResponse_arn,
    templateResponse_version,
    templateResponse_defaultSubstitutions,
    templateResponse_tags,
    templateResponse_lastModifiedDate,
    templateResponse_creationDate,
    templateResponse_templateName,
    templateResponse_templateType,

    -- * TemplateVersionResponse
    TemplateVersionResponse (..),
    newTemplateVersionResponse,
    templateVersionResponse_templateDescription,
    templateVersionResponse_version,
    templateVersionResponse_defaultSubstitutions,
    templateVersionResponse_lastModifiedDate,
    templateVersionResponse_creationDate,
    templateVersionResponse_templateName,
    templateVersionResponse_templateType,

    -- * TemplateVersionsResponse
    TemplateVersionsResponse (..),
    newTemplateVersionsResponse,
    templateVersionsResponse_nextToken,
    templateVersionsResponse_message,
    templateVersionsResponse_requestID,
    templateVersionsResponse_item,

    -- * TemplatesResponse
    TemplatesResponse (..),
    newTemplatesResponse,
    templatesResponse_nextToken,
    templatesResponse_item,

    -- * TreatmentResource
    TreatmentResource (..),
    newTreatmentResource,
    treatmentResource_customDeliveryConfiguration,
    treatmentResource_state,
    treatmentResource_treatmentName,
    treatmentResource_messageConfiguration,
    treatmentResource_templateConfiguration,
    treatmentResource_schedule,
    treatmentResource_treatmentDescription,
    treatmentResource_id,
    treatmentResource_sizePercent,

    -- * UpdateAttributesRequest
    UpdateAttributesRequest (..),
    newUpdateAttributesRequest,
    updateAttributesRequest_blacklist,

    -- * UpdateRecommenderConfiguration
    UpdateRecommenderConfiguration (..),
    newUpdateRecommenderConfiguration,
    updateRecommenderConfiguration_recommendationsDisplayName,
    updateRecommenderConfiguration_recommendationTransformerUri,
    updateRecommenderConfiguration_attributes,
    updateRecommenderConfiguration_name,
    updateRecommenderConfiguration_recommendationsPerMessage,
    updateRecommenderConfiguration_description,
    updateRecommenderConfiguration_recommendationProviderIdType,
    updateRecommenderConfiguration_recommendationProviderUri,
    updateRecommenderConfiguration_recommendationProviderRoleArn,

    -- * VoiceChannelRequest
    VoiceChannelRequest (..),
    newVoiceChannelRequest,
    voiceChannelRequest_enabled,

    -- * VoiceChannelResponse
    VoiceChannelResponse (..),
    newVoiceChannelResponse,
    voiceChannelResponse_lastModifiedDate,
    voiceChannelResponse_applicationId,
    voiceChannelResponse_hasCredential,
    voiceChannelResponse_id,
    voiceChannelResponse_creationDate,
    voiceChannelResponse_enabled,
    voiceChannelResponse_version,
    voiceChannelResponse_isArchived,
    voiceChannelResponse_lastModifiedBy,
    voiceChannelResponse_platform,

    -- * VoiceMessage
    VoiceMessage (..),
    newVoiceMessage,
    voiceMessage_languageCode,
    voiceMessage_voiceId,
    voiceMessage_body,
    voiceMessage_substitutions,
    voiceMessage_originationNumber,

    -- * VoiceTemplateRequest
    VoiceTemplateRequest (..),
    newVoiceTemplateRequest,
    voiceTemplateRequest_languageCode,
    voiceTemplateRequest_templateDescription,
    voiceTemplateRequest_voiceId,
    voiceTemplateRequest_body,
    voiceTemplateRequest_defaultSubstitutions,
    voiceTemplateRequest_tags,

    -- * VoiceTemplateResponse
    VoiceTemplateResponse (..),
    newVoiceTemplateResponse,
    voiceTemplateResponse_languageCode,
    voiceTemplateResponse_templateDescription,
    voiceTemplateResponse_voiceId,
    voiceTemplateResponse_body,
    voiceTemplateResponse_arn,
    voiceTemplateResponse_version,
    voiceTemplateResponse_defaultSubstitutions,
    voiceTemplateResponse_tags,
    voiceTemplateResponse_lastModifiedDate,
    voiceTemplateResponse_creationDate,
    voiceTemplateResponse_templateName,
    voiceTemplateResponse_templateType,

    -- * WaitActivity
    WaitActivity (..),
    newWaitActivity,
    waitActivity_waitTime,
    waitActivity_nextActivity,

    -- * WaitTime
    WaitTime (..),
    newWaitTime,
    waitTime_waitUntil,
    waitTime_waitFor,

    -- * WriteApplicationSettingsRequest
    WriteApplicationSettingsRequest (..),
    newWriteApplicationSettingsRequest,
    writeApplicationSettingsRequest_campaignHook,
    writeApplicationSettingsRequest_cloudWatchMetricsEnabled,
    writeApplicationSettingsRequest_eventTaggingEnabled,
    writeApplicationSettingsRequest_quietTime,
    writeApplicationSettingsRequest_limits,

    -- * WriteCampaignRequest
    WriteCampaignRequest (..),
    newWriteCampaignRequest,
    writeCampaignRequest_additionalTreatments,
    writeCampaignRequest_hook,
    writeCampaignRequest_customDeliveryConfiguration,
    writeCampaignRequest_name,
    writeCampaignRequest_isPaused,
    writeCampaignRequest_tags,
    writeCampaignRequest_segmentVersion,
    writeCampaignRequest_limits,
    writeCampaignRequest_segmentId,
    writeCampaignRequest_description,
    writeCampaignRequest_treatmentName,
    writeCampaignRequest_messageConfiguration,
    writeCampaignRequest_templateConfiguration,
    writeCampaignRequest_schedule,
    writeCampaignRequest_holdoutPercent,
    writeCampaignRequest_treatmentDescription,

    -- * WriteEventStream
    WriteEventStream (..),
    newWriteEventStream,
    writeEventStream_roleArn,
    writeEventStream_destinationStreamArn,

    -- * WriteJourneyRequest
    WriteJourneyRequest (..),
    newWriteJourneyRequest,
    writeJourneyRequest_lastModifiedDate,
    writeJourneyRequest_activities,
    writeJourneyRequest_creationDate,
    writeJourneyRequest_state,
    writeJourneyRequest_quietTime,
    writeJourneyRequest_refreshFrequency,
    writeJourneyRequest_limits,
    writeJourneyRequest_startCondition,
    writeJourneyRequest_localTime,
    writeJourneyRequest_startActivity,
    writeJourneyRequest_schedule,
    writeJourneyRequest_name,

    -- * WriteSegmentRequest
    WriteSegmentRequest (..),
    newWriteSegmentRequest,
    writeSegmentRequest_segmentGroups,
    writeSegmentRequest_name,
    writeSegmentRequest_tags,
    writeSegmentRequest_dimensions,

    -- * WriteTreatmentResource
    WriteTreatmentResource (..),
    newWriteTreatmentResource,
    writeTreatmentResource_customDeliveryConfiguration,
    writeTreatmentResource_treatmentName,
    writeTreatmentResource_messageConfiguration,
    writeTreatmentResource_templateConfiguration,
    writeTreatmentResource_schedule,
    writeTreatmentResource_treatmentDescription,
    writeTreatmentResource_sizePercent,
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
import Network.AWS.Pinpoint.Types.CreateRecommenderConfiguration
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
import Network.AWS.Pinpoint.Types.UpdateRecommenderConfiguration
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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2016-12-01@ of the Amazon Pinpoint SDK configuration.
defaultService :: Prelude.Service
defaultService =
  Prelude.Service
    { Prelude._svcAbbrev = "Pinpoint",
      Prelude._svcSigner = Sign.v4,
      Prelude._svcPrefix = "pinpoint",
      Prelude._svcVersion = "2016-12-01",
      Prelude._svcEndpoint =
        Prelude.defaultEndpoint defaultService,
      Prelude._svcTimeout = Prelude.Just 70,
      Prelude._svcCheck = Prelude.statusSuccess,
      Prelude._svcError =
        Prelude.parseJSONError "Pinpoint",
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

-- | Provides information about an API request or response.
_NotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_NotFoundException =
  Prelude._MatchServiceError
    defaultService
    "NotFoundException"
    Prelude.. Prelude.hasStatus 404

-- | Provides information about an API request or response.
_BadRequestException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_BadRequestException =
  Prelude._MatchServiceError
    defaultService
    "BadRequestException"
    Prelude.. Prelude.hasStatus 400

-- | Provides information about an API request or response.
_PayloadTooLargeException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_PayloadTooLargeException =
  Prelude._MatchServiceError
    defaultService
    "PayloadTooLargeException"
    Prelude.. Prelude.hasStatus 413

-- | Provides information about an API request or response.
_InternalServerErrorException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InternalServerErrorException =
  Prelude._MatchServiceError
    defaultService
    "InternalServerErrorException"
    Prelude.. Prelude.hasStatus 500

-- | Provides information about an API request or response.
_ForbiddenException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ForbiddenException =
  Prelude._MatchServiceError
    defaultService
    "ForbiddenException"
    Prelude.. Prelude.hasStatus 403

-- | Provides information about an API request or response.
_ConflictException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ConflictException =
  Prelude._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Prelude.hasStatus 409

-- | Provides information about an API request or response.
_MethodNotAllowedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_MethodNotAllowedException =
  Prelude._MatchServiceError
    defaultService
    "MethodNotAllowedException"
    Prelude.. Prelude.hasStatus 405

-- | Provides information about an API request or response.
_TooManyRequestsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TooManyRequestsException =
  Prelude._MatchServiceError
    defaultService
    "TooManyRequestsException"
    Prelude.. Prelude.hasStatus 429
