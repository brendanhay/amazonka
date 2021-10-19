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
    _PayloadTooLargeException,
    _ConflictException,
    _ForbiddenException,
    _NotFoundException,
    _TooManyRequestsException,
    _InternalServerErrorException,
    _MethodNotAllowedException,
    _BadRequestException,

    -- * Action
    Action (..),

    -- * Alignment
    Alignment (..),

    -- * AttributeType
    AttributeType (..),

    -- * ButtonAction
    ButtonAction (..),

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

    -- * Layout
    Layout (..),

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
    aDMChannelResponse_enabled,
    aDMChannelResponse_isArchived,
    aDMChannelResponse_applicationId,
    aDMChannelResponse_version,
    aDMChannelResponse_id,
    aDMChannelResponse_creationDate,
    aDMChannelResponse_lastModifiedBy,
    aDMChannelResponse_hasCredential,
    aDMChannelResponse_platform,

    -- * ADMMessage
    ADMMessage (..),
    newADMMessage,
    aDMMessage_substitutions,
    aDMMessage_expiresAfter,
    aDMMessage_md5,
    aDMMessage_silentPush,
    aDMMessage_imageIconUrl,
    aDMMessage_rawContent,
    aDMMessage_data,
    aDMMessage_smallImageIconUrl,
    aDMMessage_body,
    aDMMessage_url,
    aDMMessage_sound,
    aDMMessage_action,
    aDMMessage_imageUrl,
    aDMMessage_consolidationKey,
    aDMMessage_title,
    aDMMessage_iconReference,

    -- * APNSChannelRequest
    APNSChannelRequest (..),
    newAPNSChannelRequest,
    aPNSChannelRequest_tokenKey,
    aPNSChannelRequest_privateKey,
    aPNSChannelRequest_enabled,
    aPNSChannelRequest_teamId,
    aPNSChannelRequest_bundleId,
    aPNSChannelRequest_defaultAuthenticationMethod,
    aPNSChannelRequest_certificate,
    aPNSChannelRequest_tokenKeyId,

    -- * APNSChannelResponse
    APNSChannelResponse (..),
    newAPNSChannelResponse,
    aPNSChannelResponse_lastModifiedDate,
    aPNSChannelResponse_enabled,
    aPNSChannelResponse_hasTokenKey,
    aPNSChannelResponse_defaultAuthenticationMethod,
    aPNSChannelResponse_isArchived,
    aPNSChannelResponse_applicationId,
    aPNSChannelResponse_version,
    aPNSChannelResponse_id,
    aPNSChannelResponse_creationDate,
    aPNSChannelResponse_lastModifiedBy,
    aPNSChannelResponse_hasCredential,
    aPNSChannelResponse_platform,

    -- * APNSMessage
    APNSMessage (..),
    newAPNSMessage,
    aPNSMessage_substitutions,
    aPNSMessage_silentPush,
    aPNSMessage_aPNSPushType,
    aPNSMessage_priority,
    aPNSMessage_rawContent,
    aPNSMessage_data,
    aPNSMessage_body,
    aPNSMessage_category,
    aPNSMessage_timeToLive,
    aPNSMessage_url,
    aPNSMessage_sound,
    aPNSMessage_action,
    aPNSMessage_mediaUrl,
    aPNSMessage_preferredAuthenticationMethod,
    aPNSMessage_badge,
    aPNSMessage_title,
    aPNSMessage_threadId,
    aPNSMessage_collapseId,

    -- * APNSPushNotificationTemplate
    APNSPushNotificationTemplate (..),
    newAPNSPushNotificationTemplate,
    aPNSPushNotificationTemplate_rawContent,
    aPNSPushNotificationTemplate_body,
    aPNSPushNotificationTemplate_url,
    aPNSPushNotificationTemplate_sound,
    aPNSPushNotificationTemplate_action,
    aPNSPushNotificationTemplate_mediaUrl,
    aPNSPushNotificationTemplate_title,

    -- * APNSSandboxChannelRequest
    APNSSandboxChannelRequest (..),
    newAPNSSandboxChannelRequest,
    aPNSSandboxChannelRequest_tokenKey,
    aPNSSandboxChannelRequest_privateKey,
    aPNSSandboxChannelRequest_enabled,
    aPNSSandboxChannelRequest_teamId,
    aPNSSandboxChannelRequest_bundleId,
    aPNSSandboxChannelRequest_defaultAuthenticationMethod,
    aPNSSandboxChannelRequest_certificate,
    aPNSSandboxChannelRequest_tokenKeyId,

    -- * APNSSandboxChannelResponse
    APNSSandboxChannelResponse (..),
    newAPNSSandboxChannelResponse,
    aPNSSandboxChannelResponse_lastModifiedDate,
    aPNSSandboxChannelResponse_enabled,
    aPNSSandboxChannelResponse_hasTokenKey,
    aPNSSandboxChannelResponse_defaultAuthenticationMethod,
    aPNSSandboxChannelResponse_isArchived,
    aPNSSandboxChannelResponse_applicationId,
    aPNSSandboxChannelResponse_version,
    aPNSSandboxChannelResponse_id,
    aPNSSandboxChannelResponse_creationDate,
    aPNSSandboxChannelResponse_lastModifiedBy,
    aPNSSandboxChannelResponse_hasCredential,
    aPNSSandboxChannelResponse_platform,

    -- * APNSVoipChannelRequest
    APNSVoipChannelRequest (..),
    newAPNSVoipChannelRequest,
    aPNSVoipChannelRequest_tokenKey,
    aPNSVoipChannelRequest_privateKey,
    aPNSVoipChannelRequest_enabled,
    aPNSVoipChannelRequest_teamId,
    aPNSVoipChannelRequest_bundleId,
    aPNSVoipChannelRequest_defaultAuthenticationMethod,
    aPNSVoipChannelRequest_certificate,
    aPNSVoipChannelRequest_tokenKeyId,

    -- * APNSVoipChannelResponse
    APNSVoipChannelResponse (..),
    newAPNSVoipChannelResponse,
    aPNSVoipChannelResponse_lastModifiedDate,
    aPNSVoipChannelResponse_enabled,
    aPNSVoipChannelResponse_hasTokenKey,
    aPNSVoipChannelResponse_defaultAuthenticationMethod,
    aPNSVoipChannelResponse_isArchived,
    aPNSVoipChannelResponse_applicationId,
    aPNSVoipChannelResponse_version,
    aPNSVoipChannelResponse_id,
    aPNSVoipChannelResponse_creationDate,
    aPNSVoipChannelResponse_lastModifiedBy,
    aPNSVoipChannelResponse_hasCredential,
    aPNSVoipChannelResponse_platform,

    -- * APNSVoipSandboxChannelRequest
    APNSVoipSandboxChannelRequest (..),
    newAPNSVoipSandboxChannelRequest,
    aPNSVoipSandboxChannelRequest_tokenKey,
    aPNSVoipSandboxChannelRequest_privateKey,
    aPNSVoipSandboxChannelRequest_enabled,
    aPNSVoipSandboxChannelRequest_teamId,
    aPNSVoipSandboxChannelRequest_bundleId,
    aPNSVoipSandboxChannelRequest_defaultAuthenticationMethod,
    aPNSVoipSandboxChannelRequest_certificate,
    aPNSVoipSandboxChannelRequest_tokenKeyId,

    -- * APNSVoipSandboxChannelResponse
    APNSVoipSandboxChannelResponse (..),
    newAPNSVoipSandboxChannelResponse,
    aPNSVoipSandboxChannelResponse_lastModifiedDate,
    aPNSVoipSandboxChannelResponse_enabled,
    aPNSVoipSandboxChannelResponse_hasTokenKey,
    aPNSVoipSandboxChannelResponse_defaultAuthenticationMethod,
    aPNSVoipSandboxChannelResponse_isArchived,
    aPNSVoipSandboxChannelResponse_applicationId,
    aPNSVoipSandboxChannelResponse_version,
    aPNSVoipSandboxChannelResponse_id,
    aPNSVoipSandboxChannelResponse_creationDate,
    aPNSVoipSandboxChannelResponse_lastModifiedBy,
    aPNSVoipSandboxChannelResponse_hasCredential,
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
    activity_email,
    activity_multiCondition,
    activity_contactCenter,
    activity_custom,
    activity_wait,
    activity_randomSplit,
    activity_holdout,
    activity_sms,
    activity_push,
    activity_description,

    -- * ActivityResponse
    ActivityResponse (..),
    newActivityResponse,
    activityResponse_state,
    activityResponse_start,
    activityResponse_timezonesCompletedCount,
    activityResponse_timezonesTotalCount,
    activityResponse_result,
    activityResponse_treatmentId,
    activityResponse_successfulEndpointCount,
    activityResponse_end,
    activityResponse_totalEndpointCount,
    activityResponse_scheduledStart,
    activityResponse_campaignId,
    activityResponse_id,
    activityResponse_applicationId,

    -- * AddressConfiguration
    AddressConfiguration (..),
    newAddressConfiguration,
    addressConfiguration_substitutions,
    addressConfiguration_titleOverride,
    addressConfiguration_context,
    addressConfiguration_rawContent,
    addressConfiguration_bodyOverride,
    addressConfiguration_channelType,

    -- * AndroidPushNotificationTemplate
    AndroidPushNotificationTemplate (..),
    newAndroidPushNotificationTemplate,
    androidPushNotificationTemplate_imageIconUrl,
    androidPushNotificationTemplate_rawContent,
    androidPushNotificationTemplate_smallImageIconUrl,
    androidPushNotificationTemplate_body,
    androidPushNotificationTemplate_url,
    androidPushNotificationTemplate_sound,
    androidPushNotificationTemplate_action,
    androidPushNotificationTemplate_imageUrl,
    androidPushNotificationTemplate_title,

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
    applicationSettingsResource_limits,
    applicationSettingsResource_quietTime,
    applicationSettingsResource_campaignHook,
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
    baiduChannelResponse_enabled,
    baiduChannelResponse_isArchived,
    baiduChannelResponse_applicationId,
    baiduChannelResponse_version,
    baiduChannelResponse_id,
    baiduChannelResponse_creationDate,
    baiduChannelResponse_lastModifiedBy,
    baiduChannelResponse_hasCredential,
    baiduChannelResponse_credential,
    baiduChannelResponse_platform,

    -- * BaiduMessage
    BaiduMessage (..),
    newBaiduMessage,
    baiduMessage_substitutions,
    baiduMessage_silentPush,
    baiduMessage_imageIconUrl,
    baiduMessage_rawContent,
    baiduMessage_data,
    baiduMessage_smallImageIconUrl,
    baiduMessage_body,
    baiduMessage_timeToLive,
    baiduMessage_url,
    baiduMessage_sound,
    baiduMessage_action,
    baiduMessage_imageUrl,
    baiduMessage_title,
    baiduMessage_iconReference,

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
    campaignEmailMessage_body,
    campaignEmailMessage_fromAddress,
    campaignEmailMessage_htmlBody,
    campaignEmailMessage_title,

    -- * CampaignEventFilter
    CampaignEventFilter (..),
    newCampaignEventFilter,
    campaignEventFilter_filterType,
    campaignEventFilter_dimensions,

    -- * CampaignHook
    CampaignHook (..),
    newCampaignHook,
    campaignHook_lambdaFunctionName,
    campaignHook_mode,
    campaignHook_webUrl,

    -- * CampaignInAppMessage
    CampaignInAppMessage (..),
    newCampaignInAppMessage,
    campaignInAppMessage_layout,
    campaignInAppMessage_body,
    campaignInAppMessage_content,
    campaignInAppMessage_customConfig,

    -- * CampaignLimits
    CampaignLimits (..),
    newCampaignLimits,
    campaignLimits_messagesPerSecond,
    campaignLimits_daily,
    campaignLimits_total,
    campaignLimits_session,
    campaignLimits_maximumDuration,

    -- * CampaignResponse
    CampaignResponse (..),
    newCampaignResponse,
    campaignResponse_customDeliveryConfiguration,
    campaignResponse_state,
    campaignResponse_priority,
    campaignResponse_schedule,
    campaignResponse_templateConfiguration,
    campaignResponse_hook,
    campaignResponse_treatmentName,
    campaignResponse_limits,
    campaignResponse_isPaused,
    campaignResponse_defaultState,
    campaignResponse_name,
    campaignResponse_version,
    campaignResponse_holdoutPercent,
    campaignResponse_treatmentDescription,
    campaignResponse_messageConfiguration,
    campaignResponse_description,
    campaignResponse_additionalTreatments,
    campaignResponse_tags,
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
    campaignSmsMessage_originationNumber,
    campaignSmsMessage_templateId,
    campaignSmsMessage_body,
    campaignSmsMessage_messageType,
    campaignSmsMessage_senderId,
    campaignSmsMessage_entityId,

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
    channelResponse_enabled,
    channelResponse_isArchived,
    channelResponse_applicationId,
    channelResponse_version,
    channelResponse_id,
    channelResponse_creationDate,
    channelResponse_lastModifiedBy,
    channelResponse_hasCredential,

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
    conditionalSplitActivity_evaluationWaitTime,
    conditionalSplitActivity_trueActivity,
    conditionalSplitActivity_falseActivity,
    conditionalSplitActivity_condition,

    -- * ContactCenterActivity
    ContactCenterActivity (..),
    newContactCenterActivity,
    contactCenterActivity_nextActivity,

    -- * CreateApplicationRequest
    CreateApplicationRequest (..),
    newCreateApplicationRequest,
    createApplicationRequest_tags,
    createApplicationRequest_name,

    -- * CreateRecommenderConfiguration
    CreateRecommenderConfiguration (..),
    newCreateRecommenderConfiguration,
    createRecommenderConfiguration_recommendationTransformerUri,
    createRecommenderConfiguration_recommendationsDisplayName,
    createRecommenderConfiguration_recommendationProviderIdType,
    createRecommenderConfiguration_attributes,
    createRecommenderConfiguration_name,
    createRecommenderConfiguration_description,
    createRecommenderConfiguration_recommendationsPerMessage,
    createRecommenderConfiguration_recommendationProviderUri,
    createRecommenderConfiguration_recommendationProviderRoleArn,

    -- * CreateTemplateMessageBody
    CreateTemplateMessageBody (..),
    newCreateTemplateMessageBody,
    createTemplateMessageBody_requestID,
    createTemplateMessageBody_arn,
    createTemplateMessageBody_message,

    -- * CustomDeliveryConfiguration
    CustomDeliveryConfiguration (..),
    newCustomDeliveryConfiguration,
    customDeliveryConfiguration_endpointTypes,
    customDeliveryConfiguration_deliveryUri,

    -- * CustomMessageActivity
    CustomMessageActivity (..),
    newCustomMessageActivity,
    customMessageActivity_templateName,
    customMessageActivity_templateVersion,
    customMessageActivity_endpointTypes,
    customMessageActivity_nextActivity,
    customMessageActivity_deliveryUri,
    customMessageActivity_messageConfig,

    -- * DefaultButtonConfiguration
    DefaultButtonConfiguration (..),
    newDefaultButtonConfiguration,
    defaultButtonConfiguration_link,
    defaultButtonConfiguration_backgroundColor,
    defaultButtonConfiguration_borderRadius,
    defaultButtonConfiguration_textColor,
    defaultButtonConfiguration_buttonAction,
    defaultButtonConfiguration_text,

    -- * DefaultMessage
    DefaultMessage (..),
    newDefaultMessage,
    defaultMessage_substitutions,
    defaultMessage_body,

    -- * DefaultPushNotificationMessage
    DefaultPushNotificationMessage (..),
    newDefaultPushNotificationMessage,
    defaultPushNotificationMessage_substitutions,
    defaultPushNotificationMessage_silentPush,
    defaultPushNotificationMessage_data,
    defaultPushNotificationMessage_body,
    defaultPushNotificationMessage_url,
    defaultPushNotificationMessage_action,
    defaultPushNotificationMessage_title,

    -- * DefaultPushNotificationTemplate
    DefaultPushNotificationTemplate (..),
    newDefaultPushNotificationTemplate,
    defaultPushNotificationTemplate_body,
    defaultPushNotificationTemplate_url,
    defaultPushNotificationTemplate_sound,
    defaultPushNotificationTemplate_action,
    defaultPushNotificationTemplate_title,

    -- * DirectMessageConfiguration
    DirectMessageConfiguration (..),
    newDirectMessageConfiguration,
    directMessageConfiguration_aPNSMessage,
    directMessageConfiguration_gCMMessage,
    directMessageConfiguration_defaultMessage,
    directMessageConfiguration_aDMMessage,
    directMessageConfiguration_sMSMessage,
    directMessageConfiguration_emailMessage,
    directMessageConfiguration_voiceMessage,
    directMessageConfiguration_baiduMessage,
    directMessageConfiguration_defaultPushNotificationMessage,

    -- * EmailChannelRequest
    EmailChannelRequest (..),
    newEmailChannelRequest,
    emailChannelRequest_enabled,
    emailChannelRequest_configurationSet,
    emailChannelRequest_roleArn,
    emailChannelRequest_fromAddress,
    emailChannelRequest_identity,

    -- * EmailChannelResponse
    EmailChannelResponse (..),
    newEmailChannelResponse,
    emailChannelResponse_messagesPerSecond,
    emailChannelResponse_lastModifiedDate,
    emailChannelResponse_enabled,
    emailChannelResponse_fromAddress,
    emailChannelResponse_isArchived,
    emailChannelResponse_applicationId,
    emailChannelResponse_version,
    emailChannelResponse_configurationSet,
    emailChannelResponse_id,
    emailChannelResponse_creationDate,
    emailChannelResponse_lastModifiedBy,
    emailChannelResponse_identity,
    emailChannelResponse_hasCredential,
    emailChannelResponse_roleArn,
    emailChannelResponse_platform,

    -- * EmailMessage
    EmailMessage (..),
    newEmailMessage,
    emailMessage_substitutions,
    emailMessage_body,
    emailMessage_fromAddress,
    emailMessage_rawEmail,
    emailMessage_feedbackForwardingAddress,
    emailMessage_simpleEmail,
    emailMessage_replyToAddresses,

    -- * EmailMessageActivity
    EmailMessageActivity (..),
    newEmailMessageActivity,
    emailMessageActivity_templateName,
    emailMessageActivity_templateVersion,
    emailMessageActivity_nextActivity,
    emailMessageActivity_messageConfig,

    -- * EmailTemplateRequest
    EmailTemplateRequest (..),
    newEmailTemplateRequest,
    emailTemplateRequest_subject,
    emailTemplateRequest_textPart,
    emailTemplateRequest_templateDescription,
    emailTemplateRequest_defaultSubstitutions,
    emailTemplateRequest_htmlPart,
    emailTemplateRequest_recommenderId,
    emailTemplateRequest_tags,

    -- * EmailTemplateResponse
    EmailTemplateResponse (..),
    newEmailTemplateResponse,
    emailTemplateResponse_subject,
    emailTemplateResponse_textPart,
    emailTemplateResponse_arn,
    emailTemplateResponse_templateDescription,
    emailTemplateResponse_defaultSubstitutions,
    emailTemplateResponse_version,
    emailTemplateResponse_htmlPart,
    emailTemplateResponse_recommenderId,
    emailTemplateResponse_tags,
    emailTemplateResponse_lastModifiedDate,
    emailTemplateResponse_creationDate,
    emailTemplateResponse_templateName,
    emailTemplateResponse_templateType,

    -- * EndpointBatchItem
    EndpointBatchItem (..),
    newEndpointBatchItem,
    endpointBatchItem_requestId,
    endpointBatchItem_metrics,
    endpointBatchItem_location,
    endpointBatchItem_demographic,
    endpointBatchItem_address,
    endpointBatchItem_effectiveDate,
    endpointBatchItem_user,
    endpointBatchItem_attributes,
    endpointBatchItem_endpointStatus,
    endpointBatchItem_optOut,
    endpointBatchItem_id,
    endpointBatchItem_channelType,

    -- * EndpointBatchRequest
    EndpointBatchRequest (..),
    newEndpointBatchRequest,
    endpointBatchRequest_item,

    -- * EndpointDemographic
    EndpointDemographic (..),
    newEndpointDemographic,
    endpointDemographic_platform,
    endpointDemographic_platformVersion,
    endpointDemographic_locale,
    endpointDemographic_appVersion,
    endpointDemographic_model,
    endpointDemographic_make,
    endpointDemographic_modelVersion,
    endpointDemographic_timezone,

    -- * EndpointItemResponse
    EndpointItemResponse (..),
    newEndpointItemResponse,
    endpointItemResponse_message,
    endpointItemResponse_statusCode,

    -- * EndpointLocation
    EndpointLocation (..),
    newEndpointLocation,
    endpointLocation_postalCode,
    endpointLocation_latitude,
    endpointLocation_country,
    endpointLocation_city,
    endpointLocation_region,
    endpointLocation_longitude,

    -- * EndpointMessageResult
    EndpointMessageResult (..),
    newEndpointMessageResult,
    endpointMessageResult_address,
    endpointMessageResult_statusMessage,
    endpointMessageResult_updatedToken,
    endpointMessageResult_messageId,
    endpointMessageResult_deliveryStatus,
    endpointMessageResult_statusCode,

    -- * EndpointRequest
    EndpointRequest (..),
    newEndpointRequest,
    endpointRequest_requestId,
    endpointRequest_metrics,
    endpointRequest_location,
    endpointRequest_demographic,
    endpointRequest_address,
    endpointRequest_effectiveDate,
    endpointRequest_user,
    endpointRequest_attributes,
    endpointRequest_endpointStatus,
    endpointRequest_optOut,
    endpointRequest_channelType,

    -- * EndpointResponse
    EndpointResponse (..),
    newEndpointResponse,
    endpointResponse_requestId,
    endpointResponse_metrics,
    endpointResponse_location,
    endpointResponse_demographic,
    endpointResponse_cohortId,
    endpointResponse_address,
    endpointResponse_effectiveDate,
    endpointResponse_user,
    endpointResponse_applicationId,
    endpointResponse_attributes,
    endpointResponse_endpointStatus,
    endpointResponse_optOut,
    endpointResponse_id,
    endpointResponse_creationDate,
    endpointResponse_channelType,

    -- * EndpointSendConfiguration
    EndpointSendConfiguration (..),
    newEndpointSendConfiguration,
    endpointSendConfiguration_substitutions,
    endpointSendConfiguration_titleOverride,
    endpointSendConfiguration_context,
    endpointSendConfiguration_rawContent,
    endpointSendConfiguration_bodyOverride,

    -- * EndpointUser
    EndpointUser (..),
    newEndpointUser,
    endpointUser_userAttributes,
    endpointUser_userId,

    -- * EndpointsResponse
    EndpointsResponse (..),
    newEndpointsResponse,
    endpointsResponse_item,

    -- * Event
    Event (..),
    newEvent,
    event_clientSdkVersion,
    event_metrics,
    event_appVersionCode,
    event_appTitle,
    event_appPackageName,
    event_attributes,
    event_sdkName,
    event_session,
    event_eventType,
    event_timestamp,

    -- * EventCondition
    EventCondition (..),
    newEventCondition,
    eventCondition_dimensions,
    eventCondition_messageActivity,

    -- * EventDimensions
    EventDimensions (..),
    newEventDimensions,
    eventDimensions_metrics,
    eventDimensions_eventType,
    eventDimensions_attributes,

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
    eventStream_lastUpdatedBy,
    eventStream_lastModifiedDate,
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
    exportJobRequest_segmentId,
    exportJobRequest_segmentVersion,
    exportJobRequest_s3UrlPrefix,
    exportJobRequest_roleArn,

    -- * ExportJobResource
    ExportJobResource (..),
    newExportJobResource,
    exportJobResource_segmentId,
    exportJobResource_segmentVersion,
    exportJobResource_s3UrlPrefix,
    exportJobResource_roleArn,

    -- * ExportJobResponse
    ExportJobResponse (..),
    newExportJobResponse,
    exportJobResponse_completedPieces,
    exportJobResponse_failedPieces,
    exportJobResponse_totalProcessed,
    exportJobResponse_failures,
    exportJobResponse_totalPieces,
    exportJobResponse_completionDate,
    exportJobResponse_totalFailures,
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
    gCMChannelResponse_enabled,
    gCMChannelResponse_isArchived,
    gCMChannelResponse_applicationId,
    gCMChannelResponse_version,
    gCMChannelResponse_id,
    gCMChannelResponse_creationDate,
    gCMChannelResponse_lastModifiedBy,
    gCMChannelResponse_hasCredential,
    gCMChannelResponse_credential,
    gCMChannelResponse_platform,

    -- * GCMMessage
    GCMMessage (..),
    newGCMMessage,
    gCMMessage_substitutions,
    gCMMessage_silentPush,
    gCMMessage_imageIconUrl,
    gCMMessage_priority,
    gCMMessage_rawContent,
    gCMMessage_data,
    gCMMessage_restrictedPackageName,
    gCMMessage_smallImageIconUrl,
    gCMMessage_body,
    gCMMessage_timeToLive,
    gCMMessage_url,
    gCMMessage_sound,
    gCMMessage_action,
    gCMMessage_collapseKey,
    gCMMessage_imageUrl,
    gCMMessage_title,
    gCMMessage_iconReference,

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
    importJobRequest_segmentName,
    importJobRequest_defineSegment,
    importJobRequest_registerEndpoints,
    importJobRequest_externalId,
    importJobRequest_segmentId,
    importJobRequest_format,
    importJobRequest_s3Url,
    importJobRequest_roleArn,

    -- * ImportJobResource
    ImportJobResource (..),
    newImportJobResource,
    importJobResource_segmentName,
    importJobResource_defineSegment,
    importJobResource_registerEndpoints,
    importJobResource_externalId,
    importJobResource_segmentId,
    importJobResource_format,
    importJobResource_s3Url,
    importJobResource_roleArn,

    -- * ImportJobResponse
    ImportJobResponse (..),
    newImportJobResponse,
    importJobResponse_completedPieces,
    importJobResponse_failedPieces,
    importJobResponse_totalProcessed,
    importJobResponse_failures,
    importJobResponse_totalPieces,
    importJobResponse_completionDate,
    importJobResponse_totalFailures,
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

    -- * InAppCampaignSchedule
    InAppCampaignSchedule (..),
    newInAppCampaignSchedule,
    inAppCampaignSchedule_endDate,
    inAppCampaignSchedule_quietTime,
    inAppCampaignSchedule_eventFilter,

    -- * InAppMessage
    InAppMessage (..),
    newInAppMessage,
    inAppMessage_layout,
    inAppMessage_content,
    inAppMessage_customConfig,

    -- * InAppMessageBodyConfig
    InAppMessageBodyConfig (..),
    newInAppMessageBodyConfig,
    inAppMessageBodyConfig_alignment,
    inAppMessageBodyConfig_textColor,
    inAppMessageBodyConfig_body,

    -- * InAppMessageButton
    InAppMessageButton (..),
    newInAppMessageButton,
    inAppMessageButton_ios,
    inAppMessageButton_defaultConfig,
    inAppMessageButton_web,
    inAppMessageButton_android,

    -- * InAppMessageCampaign
    InAppMessageCampaign (..),
    newInAppMessageCampaign,
    inAppMessageCampaign_sessionCap,
    inAppMessageCampaign_priority,
    inAppMessageCampaign_schedule,
    inAppMessageCampaign_campaignId,
    inAppMessageCampaign_treatmentId,
    inAppMessageCampaign_inAppMessage,
    inAppMessageCampaign_totalCap,
    inAppMessageCampaign_dailyCap,

    -- * InAppMessageContent
    InAppMessageContent (..),
    newInAppMessageContent,
    inAppMessageContent_primaryBtn,
    inAppMessageContent_bodyConfig,
    inAppMessageContent_backgroundColor,
    inAppMessageContent_imageUrl,
    inAppMessageContent_secondaryBtn,
    inAppMessageContent_headerConfig,

    -- * InAppMessageHeaderConfig
    InAppMessageHeaderConfig (..),
    newInAppMessageHeaderConfig,
    inAppMessageHeaderConfig_alignment,
    inAppMessageHeaderConfig_header,
    inAppMessageHeaderConfig_textColor,

    -- * InAppMessagesResponse
    InAppMessagesResponse (..),
    newInAppMessagesResponse,
    inAppMessagesResponse_inAppMessageCampaigns,

    -- * InAppTemplateRequest
    InAppTemplateRequest (..),
    newInAppTemplateRequest,
    inAppTemplateRequest_layout,
    inAppTemplateRequest_templateDescription,
    inAppTemplateRequest_content,
    inAppTemplateRequest_customConfig,
    inAppTemplateRequest_tags,

    -- * InAppTemplateResponse
    InAppTemplateResponse (..),
    newInAppTemplateResponse,
    inAppTemplateResponse_layout,
    inAppTemplateResponse_arn,
    inAppTemplateResponse_templateDescription,
    inAppTemplateResponse_content,
    inAppTemplateResponse_customConfig,
    inAppTemplateResponse_version,
    inAppTemplateResponse_tags,
    inAppTemplateResponse_lastModifiedDate,
    inAppTemplateResponse_creationDate,
    inAppTemplateResponse_templateName,
    inAppTemplateResponse_templateType,

    -- * ItemResponse
    ItemResponse (..),
    newItemResponse,
    itemResponse_endpointItemResponse,
    itemResponse_eventsItemResponse,

    -- * JourneyChannelSettings
    JourneyChannelSettings (..),
    newJourneyChannelSettings,
    journeyChannelSettings_connectCampaignArn,
    journeyChannelSettings_connectCampaignExecutionRoleArn,

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
    journeyLimits_messagesPerSecond,
    journeyLimits_endpointReentryCap,
    journeyLimits_endpointReentryInterval,
    journeyLimits_dailyCap,

    -- * JourneyPushMessage
    JourneyPushMessage (..),
    newJourneyPushMessage,
    journeyPushMessage_timeToLive,

    -- * JourneyResponse
    JourneyResponse (..),
    newJourneyResponse,
    journeyResponse_state,
    journeyResponse_lastModifiedDate,
    journeyResponse_schedule,
    journeyResponse_localTime,
    journeyResponse_activities,
    journeyResponse_refreshOnSegmentUpdate,
    journeyResponse_limits,
    journeyResponse_waitForQuietTime,
    journeyResponse_quietTime,
    journeyResponse_startActivity,
    journeyResponse_creationDate,
    journeyResponse_startCondition,
    journeyResponse_journeyChannelSettings,
    journeyResponse_refreshFrequency,
    journeyResponse_tags,
    journeyResponse_name,
    journeyResponse_id,
    journeyResponse_applicationId,

    -- * JourneySMSMessage
    JourneySMSMessage (..),
    newJourneySMSMessage,
    journeySMSMessage_originationNumber,
    journeySMSMessage_templateId,
    journeySMSMessage_messageType,
    journeySMSMessage_senderId,
    journeySMSMessage_entityId,

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
    message_rawContent,
    message_body,
    message_timeToLive,
    message_imageSmallIconUrl,
    message_jsonBody,
    message_url,
    message_action,
    message_imageUrl,
    message_mediaUrl,
    message_title,

    -- * MessageBody
    MessageBody (..),
    newMessageBody,
    messageBody_requestID,
    messageBody_message,

    -- * MessageConfiguration
    MessageConfiguration (..),
    newMessageConfiguration,
    messageConfiguration_aPNSMessage,
    messageConfiguration_gCMMessage,
    messageConfiguration_defaultMessage,
    messageConfiguration_customMessage,
    messageConfiguration_aDMMessage,
    messageConfiguration_sMSMessage,
    messageConfiguration_emailMessage,
    messageConfiguration_inAppMessage,
    messageConfiguration_baiduMessage,

    -- * MessageRequest
    MessageRequest (..),
    newMessageRequest,
    messageRequest_traceId,
    messageRequest_context,
    messageRequest_addresses,
    messageRequest_templateConfiguration,
    messageRequest_endpoints,
    messageRequest_messageConfiguration,

    -- * MessageResponse
    MessageResponse (..),
    newMessageResponse,
    messageResponse_requestId,
    messageResponse_result,
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
    multiConditionalBranch_nextActivity,
    multiConditionalBranch_condition,

    -- * MultiConditionalSplitActivity
    MultiConditionalSplitActivity (..),
    newMultiConditionalSplitActivity,
    multiConditionalSplitActivity_branches,
    multiConditionalSplitActivity_evaluationWaitTime,
    multiConditionalSplitActivity_defaultActivity,

    -- * NumberValidateRequest
    NumberValidateRequest (..),
    newNumberValidateRequest,
    numberValidateRequest_isoCountryCode,
    numberValidateRequest_phoneNumber,

    -- * NumberValidateResponse
    NumberValidateResponse (..),
    newNumberValidateResponse,
    numberValidateResponse_carrier,
    numberValidateResponse_county,
    numberValidateResponse_country,
    numberValidateResponse_countryCodeNumeric,
    numberValidateResponse_zipCode,
    numberValidateResponse_originalPhoneNumber,
    numberValidateResponse_phoneTypeCode,
    numberValidateResponse_phoneType,
    numberValidateResponse_city,
    numberValidateResponse_countryCodeIso2,
    numberValidateResponse_timezone,
    numberValidateResponse_originalCountryCodeIso2,
    numberValidateResponse_cleansedPhoneNumberNational,
    numberValidateResponse_cleansedPhoneNumberE164,

    -- * OverrideButtonConfiguration
    OverrideButtonConfiguration (..),
    newOverrideButtonConfiguration,
    overrideButtonConfiguration_link,
    overrideButtonConfiguration_buttonAction,

    -- * PublicEndpoint
    PublicEndpoint (..),
    newPublicEndpoint,
    publicEndpoint_requestId,
    publicEndpoint_metrics,
    publicEndpoint_location,
    publicEndpoint_demographic,
    publicEndpoint_address,
    publicEndpoint_effectiveDate,
    publicEndpoint_user,
    publicEndpoint_attributes,
    publicEndpoint_endpointStatus,
    publicEndpoint_optOut,
    publicEndpoint_channelType,

    -- * PushMessageActivity
    PushMessageActivity (..),
    newPushMessageActivity,
    pushMessageActivity_templateName,
    pushMessageActivity_templateVersion,
    pushMessageActivity_nextActivity,
    pushMessageActivity_messageConfig,

    -- * PushNotificationTemplateRequest
    PushNotificationTemplateRequest (..),
    newPushNotificationTemplateRequest,
    pushNotificationTemplateRequest_default,
    pushNotificationTemplateRequest_templateDescription,
    pushNotificationTemplateRequest_gcm,
    pushNotificationTemplateRequest_apns,
    pushNotificationTemplateRequest_defaultSubstitutions,
    pushNotificationTemplateRequest_adm,
    pushNotificationTemplateRequest_baidu,
    pushNotificationTemplateRequest_recommenderId,
    pushNotificationTemplateRequest_tags,

    -- * PushNotificationTemplateResponse
    PushNotificationTemplateResponse (..),
    newPushNotificationTemplateResponse,
    pushNotificationTemplateResponse_arn,
    pushNotificationTemplateResponse_default,
    pushNotificationTemplateResponse_templateDescription,
    pushNotificationTemplateResponse_gcm,
    pushNotificationTemplateResponse_apns,
    pushNotificationTemplateResponse_defaultSubstitutions,
    pushNotificationTemplateResponse_version,
    pushNotificationTemplateResponse_adm,
    pushNotificationTemplateResponse_baidu,
    pushNotificationTemplateResponse_recommenderId,
    pushNotificationTemplateResponse_tags,
    pushNotificationTemplateResponse_lastModifiedDate,
    pushNotificationTemplateResponse_creationDate,
    pushNotificationTemplateResponse_templateType,
    pushNotificationTemplateResponse_templateName,

    -- * QuietTime
    QuietTime (..),
    newQuietTime,
    quietTime_start,
    quietTime_end,

    -- * RandomSplitActivity
    RandomSplitActivity (..),
    newRandomSplitActivity,
    randomSplitActivity_branches,

    -- * RandomSplitEntry
    RandomSplitEntry (..),
    newRandomSplitEntry,
    randomSplitEntry_nextActivity,
    randomSplitEntry_percentage,

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
    recommenderConfigurationResponse_recommendationTransformerUri,
    recommenderConfigurationResponse_recommendationsDisplayName,
    recommenderConfigurationResponse_recommendationProviderIdType,
    recommenderConfigurationResponse_attributes,
    recommenderConfigurationResponse_name,
    recommenderConfigurationResponse_description,
    recommenderConfigurationResponse_recommendationsPerMessage,
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
    sMSChannelRequest_shortCode,
    sMSChannelRequest_enabled,
    sMSChannelRequest_senderId,

    -- * SMSChannelResponse
    SMSChannelResponse (..),
    newSMSChannelResponse,
    sMSChannelResponse_shortCode,
    sMSChannelResponse_lastModifiedDate,
    sMSChannelResponse_enabled,
    sMSChannelResponse_senderId,
    sMSChannelResponse_transactionalMessagesPerSecond,
    sMSChannelResponse_promotionalMessagesPerSecond,
    sMSChannelResponse_isArchived,
    sMSChannelResponse_applicationId,
    sMSChannelResponse_version,
    sMSChannelResponse_id,
    sMSChannelResponse_creationDate,
    sMSChannelResponse_lastModifiedBy,
    sMSChannelResponse_hasCredential,
    sMSChannelResponse_platform,

    -- * SMSMessage
    SMSMessage (..),
    newSMSMessage,
    sMSMessage_substitutions,
    sMSMessage_originationNumber,
    sMSMessage_templateId,
    sMSMessage_body,
    sMSMessage_messageType,
    sMSMessage_senderId,
    sMSMessage_mediaUrl,
    sMSMessage_entityId,
    sMSMessage_keyword,

    -- * SMSMessageActivity
    SMSMessageActivity (..),
    newSMSMessageActivity,
    sMSMessageActivity_templateName,
    sMSMessageActivity_templateVersion,
    sMSMessageActivity_nextActivity,
    sMSMessageActivity_messageConfig,

    -- * SMSTemplateRequest
    SMSTemplateRequest (..),
    newSMSTemplateRequest,
    sMSTemplateRequest_body,
    sMSTemplateRequest_templateDescription,
    sMSTemplateRequest_defaultSubstitutions,
    sMSTemplateRequest_recommenderId,
    sMSTemplateRequest_tags,

    -- * SMSTemplateResponse
    SMSTemplateResponse (..),
    newSMSTemplateResponse,
    sMSTemplateResponse_arn,
    sMSTemplateResponse_body,
    sMSTemplateResponse_templateDescription,
    sMSTemplateResponse_defaultSubstitutions,
    sMSTemplateResponse_version,
    sMSTemplateResponse_recommenderId,
    sMSTemplateResponse_tags,
    sMSTemplateResponse_lastModifiedDate,
    sMSTemplateResponse_creationDate,
    sMSTemplateResponse_templateName,
    sMSTemplateResponse_templateType,

    -- * Schedule
    Schedule (..),
    newSchedule,
    schedule_frequency,
    schedule_quietTime,
    schedule_eventFilter,
    schedule_isLocalTime,
    schedule_endTime,
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
    segmentDemographics_platform,
    segmentDemographics_appVersion,
    segmentDemographics_channel,
    segmentDemographics_model,
    segmentDemographics_make,
    segmentDemographics_deviceType,

    -- * SegmentDimensions
    SegmentDimensions (..),
    newSegmentDimensions,
    segmentDimensions_metrics,
    segmentDimensions_location,
    segmentDimensions_demographic,
    segmentDimensions_userAttributes,
    segmentDimensions_behavior,
    segmentDimensions_attributes,

    -- * SegmentGroup
    SegmentGroup (..),
    newSegmentGroup,
    segmentGroup_sourceSegments,
    segmentGroup_sourceType,
    segmentGroup_type,
    segmentGroup_dimensions,

    -- * SegmentGroupList
    SegmentGroupList (..),
    newSegmentGroupList,
    segmentGroupList_include,
    segmentGroupList_groups,

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
    segmentLocation_country,
    segmentLocation_gPSPoint,

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
    segmentResponse_name,
    segmentResponse_version,
    segmentResponse_importDefinition,
    segmentResponse_dimensions,
    segmentResponse_tags,
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
    sendUsersMessageRequest_traceId,
    sendUsersMessageRequest_context,
    sendUsersMessageRequest_templateConfiguration,
    sendUsersMessageRequest_messageConfiguration,
    sendUsersMessageRequest_users,

    -- * SendUsersMessageResponse
    SendUsersMessageResponse (..),
    newSendUsersMessageResponse,
    sendUsersMessageResponse_requestId,
    sendUsersMessageResponse_result,
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
    simpleCondition_segmentDimensions,
    simpleCondition_eventCondition,
    simpleCondition_segmentCondition,

    -- * SimpleEmail
    SimpleEmail (..),
    newSimpleEmail,
    simpleEmail_subject,
    simpleEmail_textPart,
    simpleEmail_htmlPart,

    -- * SimpleEmailPart
    SimpleEmailPart (..),
    newSimpleEmailPart,
    simpleEmailPart_data,
    simpleEmailPart_charset,

    -- * StartCondition
    StartCondition (..),
    newStartCondition,
    startCondition_segmentStartCondition,
    startCondition_eventStartCondition,
    startCondition_description,

    -- * TagsModel
    TagsModel (..),
    newTagsModel,
    tagsModel_tags,

    -- * Template
    Template (..),
    newTemplate,
    template_name,
    template_version,

    -- * TemplateActiveVersionRequest
    TemplateActiveVersionRequest (..),
    newTemplateActiveVersionRequest,
    templateActiveVersionRequest_version,

    -- * TemplateConfiguration
    TemplateConfiguration (..),
    newTemplateConfiguration,
    templateConfiguration_sMSTemplate,
    templateConfiguration_voiceTemplate,
    templateConfiguration_pushTemplate,
    templateConfiguration_emailTemplate,

    -- * TemplateCreateMessageBody
    TemplateCreateMessageBody (..),
    newTemplateCreateMessageBody,
    templateCreateMessageBody_requestID,
    templateCreateMessageBody_arn,
    templateCreateMessageBody_message,

    -- * TemplateResponse
    TemplateResponse (..),
    newTemplateResponse,
    templateResponse_arn,
    templateResponse_templateDescription,
    templateResponse_defaultSubstitutions,
    templateResponse_version,
    templateResponse_tags,
    templateResponse_lastModifiedDate,
    templateResponse_creationDate,
    templateResponse_templateName,
    templateResponse_templateType,

    -- * TemplateVersionResponse
    TemplateVersionResponse (..),
    newTemplateVersionResponse,
    templateVersionResponse_templateDescription,
    templateVersionResponse_defaultSubstitutions,
    templateVersionResponse_version,
    templateVersionResponse_lastModifiedDate,
    templateVersionResponse_creationDate,
    templateVersionResponse_templateName,
    templateVersionResponse_templateType,

    -- * TemplateVersionsResponse
    TemplateVersionsResponse (..),
    newTemplateVersionsResponse,
    templateVersionsResponse_requestID,
    templateVersionsResponse_nextToken,
    templateVersionsResponse_message,
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
    treatmentResource_schedule,
    treatmentResource_templateConfiguration,
    treatmentResource_treatmentName,
    treatmentResource_treatmentDescription,
    treatmentResource_messageConfiguration,
    treatmentResource_id,
    treatmentResource_sizePercent,

    -- * UpdateAttributesRequest
    UpdateAttributesRequest (..),
    newUpdateAttributesRequest,
    updateAttributesRequest_blacklist,

    -- * UpdateRecommenderConfiguration
    UpdateRecommenderConfiguration (..),
    newUpdateRecommenderConfiguration,
    updateRecommenderConfiguration_recommendationTransformerUri,
    updateRecommenderConfiguration_recommendationsDisplayName,
    updateRecommenderConfiguration_recommendationProviderIdType,
    updateRecommenderConfiguration_attributes,
    updateRecommenderConfiguration_name,
    updateRecommenderConfiguration_description,
    updateRecommenderConfiguration_recommendationsPerMessage,
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
    voiceChannelResponse_enabled,
    voiceChannelResponse_isArchived,
    voiceChannelResponse_applicationId,
    voiceChannelResponse_version,
    voiceChannelResponse_id,
    voiceChannelResponse_creationDate,
    voiceChannelResponse_lastModifiedBy,
    voiceChannelResponse_hasCredential,
    voiceChannelResponse_platform,

    -- * VoiceMessage
    VoiceMessage (..),
    newVoiceMessage,
    voiceMessage_substitutions,
    voiceMessage_languageCode,
    voiceMessage_originationNumber,
    voiceMessage_body,
    voiceMessage_voiceId,

    -- * VoiceTemplateRequest
    VoiceTemplateRequest (..),
    newVoiceTemplateRequest,
    voiceTemplateRequest_languageCode,
    voiceTemplateRequest_body,
    voiceTemplateRequest_templateDescription,
    voiceTemplateRequest_defaultSubstitutions,
    voiceTemplateRequest_voiceId,
    voiceTemplateRequest_tags,

    -- * VoiceTemplateResponse
    VoiceTemplateResponse (..),
    newVoiceTemplateResponse,
    voiceTemplateResponse_languageCode,
    voiceTemplateResponse_arn,
    voiceTemplateResponse_body,
    voiceTemplateResponse_templateDescription,
    voiceTemplateResponse_defaultSubstitutions,
    voiceTemplateResponse_version,
    voiceTemplateResponse_voiceId,
    voiceTemplateResponse_tags,
    voiceTemplateResponse_lastModifiedDate,
    voiceTemplateResponse_creationDate,
    voiceTemplateResponse_templateName,
    voiceTemplateResponse_templateType,

    -- * WaitActivity
    WaitActivity (..),
    newWaitActivity,
    waitActivity_nextActivity,
    waitActivity_waitTime,

    -- * WaitTime
    WaitTime (..),
    newWaitTime,
    waitTime_waitFor,
    waitTime_waitUntil,

    -- * WriteApplicationSettingsRequest
    WriteApplicationSettingsRequest (..),
    newWriteApplicationSettingsRequest,
    writeApplicationSettingsRequest_eventTaggingEnabled,
    writeApplicationSettingsRequest_cloudWatchMetricsEnabled,
    writeApplicationSettingsRequest_limits,
    writeApplicationSettingsRequest_quietTime,
    writeApplicationSettingsRequest_campaignHook,

    -- * WriteCampaignRequest
    WriteCampaignRequest (..),
    newWriteCampaignRequest,
    writeCampaignRequest_customDeliveryConfiguration,
    writeCampaignRequest_priority,
    writeCampaignRequest_schedule,
    writeCampaignRequest_templateConfiguration,
    writeCampaignRequest_hook,
    writeCampaignRequest_treatmentName,
    writeCampaignRequest_limits,
    writeCampaignRequest_isPaused,
    writeCampaignRequest_name,
    writeCampaignRequest_holdoutPercent,
    writeCampaignRequest_treatmentDescription,
    writeCampaignRequest_messageConfiguration,
    writeCampaignRequest_description,
    writeCampaignRequest_segmentId,
    writeCampaignRequest_additionalTreatments,
    writeCampaignRequest_tags,
    writeCampaignRequest_segmentVersion,

    -- * WriteEventStream
    WriteEventStream (..),
    newWriteEventStream,
    writeEventStream_roleArn,
    writeEventStream_destinationStreamArn,

    -- * WriteJourneyRequest
    WriteJourneyRequest (..),
    newWriteJourneyRequest,
    writeJourneyRequest_state,
    writeJourneyRequest_lastModifiedDate,
    writeJourneyRequest_schedule,
    writeJourneyRequest_localTime,
    writeJourneyRequest_activities,
    writeJourneyRequest_refreshOnSegmentUpdate,
    writeJourneyRequest_limits,
    writeJourneyRequest_waitForQuietTime,
    writeJourneyRequest_quietTime,
    writeJourneyRequest_startActivity,
    writeJourneyRequest_creationDate,
    writeJourneyRequest_startCondition,
    writeJourneyRequest_refreshFrequency,
    writeJourneyRequest_name,

    -- * WriteSegmentRequest
    WriteSegmentRequest (..),
    newWriteSegmentRequest,
    writeSegmentRequest_segmentGroups,
    writeSegmentRequest_name,
    writeSegmentRequest_dimensions,
    writeSegmentRequest_tags,

    -- * WriteTreatmentResource
    WriteTreatmentResource (..),
    newWriteTreatmentResource,
    writeTreatmentResource_customDeliveryConfiguration,
    writeTreatmentResource_schedule,
    writeTreatmentResource_templateConfiguration,
    writeTreatmentResource_treatmentName,
    writeTreatmentResource_treatmentDescription,
    writeTreatmentResource_messageConfiguration,
    writeTreatmentResource_sizePercent,
  )
where

import qualified Network.AWS.Core as Core
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
import Network.AWS.Pinpoint.Types.Alignment
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
import Network.AWS.Pinpoint.Types.ButtonAction
import Network.AWS.Pinpoint.Types.CampaignCustomMessage
import Network.AWS.Pinpoint.Types.CampaignDateRangeKpiResponse
import Network.AWS.Pinpoint.Types.CampaignEmailMessage
import Network.AWS.Pinpoint.Types.CampaignEventFilter
import Network.AWS.Pinpoint.Types.CampaignHook
import Network.AWS.Pinpoint.Types.CampaignInAppMessage
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
import Network.AWS.Pinpoint.Types.ContactCenterActivity
import Network.AWS.Pinpoint.Types.CreateApplicationRequest
import Network.AWS.Pinpoint.Types.CreateRecommenderConfiguration
import Network.AWS.Pinpoint.Types.CreateTemplateMessageBody
import Network.AWS.Pinpoint.Types.CustomDeliveryConfiguration
import Network.AWS.Pinpoint.Types.CustomMessageActivity
import Network.AWS.Pinpoint.Types.DefaultButtonConfiguration
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
import Network.AWS.Pinpoint.Types.InAppCampaignSchedule
import Network.AWS.Pinpoint.Types.InAppMessage
import Network.AWS.Pinpoint.Types.InAppMessageBodyConfig
import Network.AWS.Pinpoint.Types.InAppMessageButton
import Network.AWS.Pinpoint.Types.InAppMessageCampaign
import Network.AWS.Pinpoint.Types.InAppMessageContent
import Network.AWS.Pinpoint.Types.InAppMessageHeaderConfig
import Network.AWS.Pinpoint.Types.InAppMessagesResponse
import Network.AWS.Pinpoint.Types.InAppTemplateRequest
import Network.AWS.Pinpoint.Types.InAppTemplateResponse
import Network.AWS.Pinpoint.Types.Include
import Network.AWS.Pinpoint.Types.ItemResponse
import Network.AWS.Pinpoint.Types.JobStatus
import Network.AWS.Pinpoint.Types.JourneyChannelSettings
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
import Network.AWS.Pinpoint.Types.Layout
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
import Network.AWS.Pinpoint.Types.OverrideButtonConfiguration
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
import Network.AWS.Pinpoint.Types.TemplateCreateMessageBody
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
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "Pinpoint",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "pinpoint",
      Core._serviceSigningName = "mobiletargeting",
      Core._serviceVersion = "2016-12-01",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "Pinpoint",
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

-- | Provides information about an API request or response.
_PayloadTooLargeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_PayloadTooLargeException =
  Core._MatchServiceError
    defaultService
    "PayloadTooLargeException"
    Prelude.. Core.hasStatus 413

-- | Provides information about an API request or response.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | Provides information about an API request or response.
_ForbiddenException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ForbiddenException =
  Core._MatchServiceError
    defaultService
    "ForbiddenException"
    Prelude.. Core.hasStatus 403

-- | Provides information about an API request or response.
_NotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError
    defaultService
    "NotFoundException"
    Prelude.. Core.hasStatus 404

-- | Provides information about an API request or response.
_TooManyRequestsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyRequestsException =
  Core._MatchServiceError
    defaultService
    "TooManyRequestsException"
    Prelude.. Core.hasStatus 429

-- | Provides information about an API request or response.
_InternalServerErrorException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerErrorException =
  Core._MatchServiceError
    defaultService
    "InternalServerErrorException"
    Prelude.. Core.hasStatus 500

-- | Provides information about an API request or response.
_MethodNotAllowedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MethodNotAllowedException =
  Core._MatchServiceError
    defaultService
    "MethodNotAllowedException"
    Prelude.. Core.hasStatus 405

-- | Provides information about an API request or response.
_BadRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError
    defaultService
    "BadRequestException"
    Prelude.. Core.hasStatus 400
