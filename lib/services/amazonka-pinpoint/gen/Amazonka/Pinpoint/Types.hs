{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Pinpoint.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _NotFoundException,
    _InternalServerErrorException,
    _ForbiddenException,
    _ConflictException,
    _MethodNotAllowedException,
    _PayloadTooLargeException,
    _BadRequestException,
    _TooManyRequestsException,

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
    aDMChannelResponse_creationDate,
    aDMChannelResponse_hasCredential,
    aDMChannelResponse_id,
    aDMChannelResponse_enabled,
    aDMChannelResponse_lastModifiedBy,
    aDMChannelResponse_isArchived,
    aDMChannelResponse_applicationId,
    aDMChannelResponse_version,
    aDMChannelResponse_platform,

    -- * ADMMessage
    ADMMessage (..),
    newADMMessage,
    aDMMessage_expiresAfter,
    aDMMessage_md5,
    aDMMessage_iconReference,
    aDMMessage_body,
    aDMMessage_imageUrl,
    aDMMessage_url,
    aDMMessage_substitutions,
    aDMMessage_rawContent,
    aDMMessage_sound,
    aDMMessage_silentPush,
    aDMMessage_consolidationKey,
    aDMMessage_imageIconUrl,
    aDMMessage_title,
    aDMMessage_action,
    aDMMessage_data,
    aDMMessage_smallImageIconUrl,

    -- * APNSChannelRequest
    APNSChannelRequest (..),
    newAPNSChannelRequest,
    aPNSChannelRequest_privateKey,
    aPNSChannelRequest_teamId,
    aPNSChannelRequest_tokenKeyId,
    aPNSChannelRequest_certificate,
    aPNSChannelRequest_enabled,
    aPNSChannelRequest_tokenKey,
    aPNSChannelRequest_bundleId,
    aPNSChannelRequest_defaultAuthenticationMethod,

    -- * APNSChannelResponse
    APNSChannelResponse (..),
    newAPNSChannelResponse,
    aPNSChannelResponse_lastModifiedDate,
    aPNSChannelResponse_hasTokenKey,
    aPNSChannelResponse_creationDate,
    aPNSChannelResponse_hasCredential,
    aPNSChannelResponse_id,
    aPNSChannelResponse_enabled,
    aPNSChannelResponse_defaultAuthenticationMethod,
    aPNSChannelResponse_lastModifiedBy,
    aPNSChannelResponse_isArchived,
    aPNSChannelResponse_applicationId,
    aPNSChannelResponse_version,
    aPNSChannelResponse_platform,

    -- * APNSMessage
    APNSMessage (..),
    newAPNSMessage,
    aPNSMessage_collapseId,
    aPNSMessage_timeToLive,
    aPNSMessage_threadId,
    aPNSMessage_body,
    aPNSMessage_url,
    aPNSMessage_substitutions,
    aPNSMessage_rawContent,
    aPNSMessage_sound,
    aPNSMessage_silentPush,
    aPNSMessage_aPNSPushType,
    aPNSMessage_badge,
    aPNSMessage_title,
    aPNSMessage_priority,
    aPNSMessage_mediaUrl,
    aPNSMessage_category,
    aPNSMessage_action,
    aPNSMessage_data,
    aPNSMessage_preferredAuthenticationMethod,

    -- * APNSPushNotificationTemplate
    APNSPushNotificationTemplate (..),
    newAPNSPushNotificationTemplate,
    aPNSPushNotificationTemplate_body,
    aPNSPushNotificationTemplate_url,
    aPNSPushNotificationTemplate_rawContent,
    aPNSPushNotificationTemplate_sound,
    aPNSPushNotificationTemplate_title,
    aPNSPushNotificationTemplate_mediaUrl,
    aPNSPushNotificationTemplate_action,

    -- * APNSSandboxChannelRequest
    APNSSandboxChannelRequest (..),
    newAPNSSandboxChannelRequest,
    aPNSSandboxChannelRequest_privateKey,
    aPNSSandboxChannelRequest_teamId,
    aPNSSandboxChannelRequest_tokenKeyId,
    aPNSSandboxChannelRequest_certificate,
    aPNSSandboxChannelRequest_enabled,
    aPNSSandboxChannelRequest_tokenKey,
    aPNSSandboxChannelRequest_bundleId,
    aPNSSandboxChannelRequest_defaultAuthenticationMethod,

    -- * APNSSandboxChannelResponse
    APNSSandboxChannelResponse (..),
    newAPNSSandboxChannelResponse,
    aPNSSandboxChannelResponse_lastModifiedDate,
    aPNSSandboxChannelResponse_hasTokenKey,
    aPNSSandboxChannelResponse_creationDate,
    aPNSSandboxChannelResponse_hasCredential,
    aPNSSandboxChannelResponse_id,
    aPNSSandboxChannelResponse_enabled,
    aPNSSandboxChannelResponse_defaultAuthenticationMethod,
    aPNSSandboxChannelResponse_lastModifiedBy,
    aPNSSandboxChannelResponse_isArchived,
    aPNSSandboxChannelResponse_applicationId,
    aPNSSandboxChannelResponse_version,
    aPNSSandboxChannelResponse_platform,

    -- * APNSVoipChannelRequest
    APNSVoipChannelRequest (..),
    newAPNSVoipChannelRequest,
    aPNSVoipChannelRequest_privateKey,
    aPNSVoipChannelRequest_teamId,
    aPNSVoipChannelRequest_tokenKeyId,
    aPNSVoipChannelRequest_certificate,
    aPNSVoipChannelRequest_enabled,
    aPNSVoipChannelRequest_tokenKey,
    aPNSVoipChannelRequest_bundleId,
    aPNSVoipChannelRequest_defaultAuthenticationMethod,

    -- * APNSVoipChannelResponse
    APNSVoipChannelResponse (..),
    newAPNSVoipChannelResponse,
    aPNSVoipChannelResponse_lastModifiedDate,
    aPNSVoipChannelResponse_hasTokenKey,
    aPNSVoipChannelResponse_creationDate,
    aPNSVoipChannelResponse_hasCredential,
    aPNSVoipChannelResponse_id,
    aPNSVoipChannelResponse_enabled,
    aPNSVoipChannelResponse_defaultAuthenticationMethod,
    aPNSVoipChannelResponse_lastModifiedBy,
    aPNSVoipChannelResponse_isArchived,
    aPNSVoipChannelResponse_applicationId,
    aPNSVoipChannelResponse_version,
    aPNSVoipChannelResponse_platform,

    -- * APNSVoipSandboxChannelRequest
    APNSVoipSandboxChannelRequest (..),
    newAPNSVoipSandboxChannelRequest,
    aPNSVoipSandboxChannelRequest_privateKey,
    aPNSVoipSandboxChannelRequest_teamId,
    aPNSVoipSandboxChannelRequest_tokenKeyId,
    aPNSVoipSandboxChannelRequest_certificate,
    aPNSVoipSandboxChannelRequest_enabled,
    aPNSVoipSandboxChannelRequest_tokenKey,
    aPNSVoipSandboxChannelRequest_bundleId,
    aPNSVoipSandboxChannelRequest_defaultAuthenticationMethod,

    -- * APNSVoipSandboxChannelResponse
    APNSVoipSandboxChannelResponse (..),
    newAPNSVoipSandboxChannelResponse,
    aPNSVoipSandboxChannelResponse_lastModifiedDate,
    aPNSVoipSandboxChannelResponse_hasTokenKey,
    aPNSVoipSandboxChannelResponse_creationDate,
    aPNSVoipSandboxChannelResponse_hasCredential,
    aPNSVoipSandboxChannelResponse_id,
    aPNSVoipSandboxChannelResponse_enabled,
    aPNSVoipSandboxChannelResponse_defaultAuthenticationMethod,
    aPNSVoipSandboxChannelResponse_lastModifiedBy,
    aPNSVoipSandboxChannelResponse_isArchived,
    aPNSVoipSandboxChannelResponse_applicationId,
    aPNSVoipSandboxChannelResponse_version,
    aPNSVoipSandboxChannelResponse_platform,

    -- * ActivitiesResponse
    ActivitiesResponse (..),
    newActivitiesResponse,
    activitiesResponse_nextToken,
    activitiesResponse_item,

    -- * Activity
    Activity (..),
    newActivity,
    activity_sms,
    activity_multiCondition,
    activity_email,
    activity_push,
    activity_description,
    activity_wait,
    activity_randomSplit,
    activity_contactCenter,
    activity_custom,
    activity_conditionalSplit,
    activity_holdout,

    -- * ActivityResponse
    ActivityResponse (..),
    newActivityResponse,
    activityResponse_timezonesCompletedCount,
    activityResponse_successfulEndpointCount,
    activityResponse_scheduledStart,
    activityResponse_treatmentId,
    activityResponse_start,
    activityResponse_state,
    activityResponse_timezonesTotalCount,
    activityResponse_end,
    activityResponse_result,
    activityResponse_totalEndpointCount,
    activityResponse_campaignId,
    activityResponse_id,
    activityResponse_applicationId,

    -- * AddressConfiguration
    AddressConfiguration (..),
    newAddressConfiguration,
    addressConfiguration_context,
    addressConfiguration_substitutions,
    addressConfiguration_titleOverride,
    addressConfiguration_rawContent,
    addressConfiguration_bodyOverride,
    addressConfiguration_channelType,

    -- * AndroidPushNotificationTemplate
    AndroidPushNotificationTemplate (..),
    newAndroidPushNotificationTemplate,
    androidPushNotificationTemplate_body,
    androidPushNotificationTemplate_imageUrl,
    androidPushNotificationTemplate_url,
    androidPushNotificationTemplate_rawContent,
    androidPushNotificationTemplate_sound,
    androidPushNotificationTemplate_imageIconUrl,
    androidPushNotificationTemplate_title,
    androidPushNotificationTemplate_action,
    androidPushNotificationTemplate_smallImageIconUrl,

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
    baiduChannelResponse_creationDate,
    baiduChannelResponse_hasCredential,
    baiduChannelResponse_id,
    baiduChannelResponse_enabled,
    baiduChannelResponse_lastModifiedBy,
    baiduChannelResponse_isArchived,
    baiduChannelResponse_applicationId,
    baiduChannelResponse_version,
    baiduChannelResponse_credential,
    baiduChannelResponse_platform,

    -- * BaiduMessage
    BaiduMessage (..),
    newBaiduMessage,
    baiduMessage_timeToLive,
    baiduMessage_iconReference,
    baiduMessage_body,
    baiduMessage_imageUrl,
    baiduMessage_url,
    baiduMessage_substitutions,
    baiduMessage_rawContent,
    baiduMessage_sound,
    baiduMessage_silentPush,
    baiduMessage_imageIconUrl,
    baiduMessage_title,
    baiduMessage_action,
    baiduMessage_data,
    baiduMessage_smallImageIconUrl,

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
    campaignEmailMessage_fromAddress,
    campaignEmailMessage_body,
    campaignEmailMessage_title,
    campaignEmailMessage_htmlBody,

    -- * CampaignEventFilter
    CampaignEventFilter (..),
    newCampaignEventFilter,
    campaignEventFilter_filterType,
    campaignEventFilter_dimensions,

    -- * CampaignHook
    CampaignHook (..),
    newCampaignHook,
    campaignHook_lambdaFunctionName,
    campaignHook_webUrl,
    campaignHook_mode,

    -- * CampaignInAppMessage
    CampaignInAppMessage (..),
    newCampaignInAppMessage,
    campaignInAppMessage_customConfig,
    campaignInAppMessage_body,
    campaignInAppMessage_layout,
    campaignInAppMessage_content,

    -- * CampaignLimits
    CampaignLimits (..),
    newCampaignLimits,
    campaignLimits_maximumDuration,
    campaignLimits_total,
    campaignLimits_messagesPerSecond,
    campaignLimits_session,
    campaignLimits_daily,

    -- * CampaignResponse
    CampaignResponse (..),
    newCampaignResponse,
    campaignResponse_tags,
    campaignResponse_schedule,
    campaignResponse_customDeliveryConfiguration,
    campaignResponse_name,
    campaignResponse_hook,
    campaignResponse_limits,
    campaignResponse_treatmentName,
    campaignResponse_holdoutPercent,
    campaignResponse_state,
    campaignResponse_additionalTreatments,
    campaignResponse_messageConfiguration,
    campaignResponse_description,
    campaignResponse_treatmentDescription,
    campaignResponse_priority,
    campaignResponse_templateConfiguration,
    campaignResponse_isPaused,
    campaignResponse_version,
    campaignResponse_defaultState,
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
    campaignSmsMessage_messageType,
    campaignSmsMessage_senderId,
    campaignSmsMessage_body,
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
    channelResponse_creationDate,
    channelResponse_hasCredential,
    channelResponse_id,
    channelResponse_enabled,
    channelResponse_lastModifiedBy,
    channelResponse_isArchived,
    channelResponse_applicationId,
    channelResponse_version,

    -- * ChannelsResponse
    ChannelsResponse (..),
    newChannelsResponse,
    channelsResponse_channels,

    -- * Condition
    Condition (..),
    newCondition,
    condition_conditions,
    condition_operator,

    -- * ConditionalSplitActivity
    ConditionalSplitActivity (..),
    newConditionalSplitActivity,
    conditionalSplitActivity_condition,
    conditionalSplitActivity_falseActivity,
    conditionalSplitActivity_evaluationWaitTime,
    conditionalSplitActivity_trueActivity,

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
    createRecommenderConfiguration_name,
    createRecommenderConfiguration_recommendationTransformerUri,
    createRecommenderConfiguration_description,
    createRecommenderConfiguration_recommendationsDisplayName,
    createRecommenderConfiguration_recommendationProviderIdType,
    createRecommenderConfiguration_attributes,
    createRecommenderConfiguration_recommendationsPerMessage,
    createRecommenderConfiguration_recommendationProviderUri,
    createRecommenderConfiguration_recommendationProviderRoleArn,

    -- * CreateTemplateMessageBody
    CreateTemplateMessageBody (..),
    newCreateTemplateMessageBody,
    createTemplateMessageBody_message,
    createTemplateMessageBody_requestID,
    createTemplateMessageBody_arn,

    -- * CustomDeliveryConfiguration
    CustomDeliveryConfiguration (..),
    newCustomDeliveryConfiguration,
    customDeliveryConfiguration_endpointTypes,
    customDeliveryConfiguration_deliveryUri,

    -- * CustomMessageActivity
    CustomMessageActivity (..),
    newCustomMessageActivity,
    customMessageActivity_endpointTypes,
    customMessageActivity_templateName,
    customMessageActivity_nextActivity,
    customMessageActivity_templateVersion,
    customMessageActivity_deliveryUri,
    customMessageActivity_messageConfig,

    -- * DefaultButtonConfiguration
    DefaultButtonConfiguration (..),
    newDefaultButtonConfiguration,
    defaultButtonConfiguration_link,
    defaultButtonConfiguration_textColor,
    defaultButtonConfiguration_backgroundColor,
    defaultButtonConfiguration_borderRadius,
    defaultButtonConfiguration_buttonAction,
    defaultButtonConfiguration_text,

    -- * DefaultMessage
    DefaultMessage (..),
    newDefaultMessage,
    defaultMessage_body,
    defaultMessage_substitutions,

    -- * DefaultPushNotificationMessage
    DefaultPushNotificationMessage (..),
    newDefaultPushNotificationMessage,
    defaultPushNotificationMessage_body,
    defaultPushNotificationMessage_url,
    defaultPushNotificationMessage_substitutions,
    defaultPushNotificationMessage_silentPush,
    defaultPushNotificationMessage_title,
    defaultPushNotificationMessage_action,
    defaultPushNotificationMessage_data,

    -- * DefaultPushNotificationTemplate
    DefaultPushNotificationTemplate (..),
    newDefaultPushNotificationTemplate,
    defaultPushNotificationTemplate_body,
    defaultPushNotificationTemplate_url,
    defaultPushNotificationTemplate_sound,
    defaultPushNotificationTemplate_title,
    defaultPushNotificationTemplate_action,

    -- * DirectMessageConfiguration
    DirectMessageConfiguration (..),
    newDirectMessageConfiguration,
    directMessageConfiguration_aDMMessage,
    directMessageConfiguration_sMSMessage,
    directMessageConfiguration_voiceMessage,
    directMessageConfiguration_defaultMessage,
    directMessageConfiguration_aPNSMessage,
    directMessageConfiguration_gCMMessage,
    directMessageConfiguration_defaultPushNotificationMessage,
    directMessageConfiguration_emailMessage,
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
    emailChannelResponse_roleArn,
    emailChannelResponse_fromAddress,
    emailChannelResponse_lastModifiedDate,
    emailChannelResponse_messagesPerSecond,
    emailChannelResponse_creationDate,
    emailChannelResponse_hasCredential,
    emailChannelResponse_id,
    emailChannelResponse_enabled,
    emailChannelResponse_identity,
    emailChannelResponse_configurationSet,
    emailChannelResponse_lastModifiedBy,
    emailChannelResponse_isArchived,
    emailChannelResponse_applicationId,
    emailChannelResponse_version,
    emailChannelResponse_platform,

    -- * EmailMessage
    EmailMessage (..),
    newEmailMessage,
    emailMessage_feedbackForwardingAddress,
    emailMessage_replyToAddresses,
    emailMessage_fromAddress,
    emailMessage_simpleEmail,
    emailMessage_body,
    emailMessage_substitutions,
    emailMessage_rawEmail,

    -- * EmailMessageActivity
    EmailMessageActivity (..),
    newEmailMessageActivity,
    emailMessageActivity_templateName,
    emailMessageActivity_nextActivity,
    emailMessageActivity_templateVersion,
    emailMessageActivity_messageConfig,

    -- * EmailTemplateRequest
    EmailTemplateRequest (..),
    newEmailTemplateRequest,
    emailTemplateRequest_tags,
    emailTemplateRequest_recommenderId,
    emailTemplateRequest_defaultSubstitutions,
    emailTemplateRequest_textPart,
    emailTemplateRequest_subject,
    emailTemplateRequest_htmlPart,
    emailTemplateRequest_templateDescription,

    -- * EmailTemplateResponse
    EmailTemplateResponse (..),
    newEmailTemplateResponse,
    emailTemplateResponse_tags,
    emailTemplateResponse_arn,
    emailTemplateResponse_recommenderId,
    emailTemplateResponse_defaultSubstitutions,
    emailTemplateResponse_textPart,
    emailTemplateResponse_subject,
    emailTemplateResponse_htmlPart,
    emailTemplateResponse_templateDescription,
    emailTemplateResponse_version,
    emailTemplateResponse_lastModifiedDate,
    emailTemplateResponse_creationDate,
    emailTemplateResponse_templateName,
    emailTemplateResponse_templateType,

    -- * EndpointBatchItem
    EndpointBatchItem (..),
    newEndpointBatchItem,
    endpointBatchItem_demographic,
    endpointBatchItem_user,
    endpointBatchItem_requestId,
    endpointBatchItem_metrics,
    endpointBatchItem_id,
    endpointBatchItem_location,
    endpointBatchItem_optOut,
    endpointBatchItem_address,
    endpointBatchItem_effectiveDate,
    endpointBatchItem_attributes,
    endpointBatchItem_endpointStatus,
    endpointBatchItem_channelType,

    -- * EndpointBatchRequest
    EndpointBatchRequest (..),
    newEndpointBatchRequest,
    endpointBatchRequest_item,

    -- * EndpointDemographic
    EndpointDemographic (..),
    newEndpointDemographic,
    endpointDemographic_model,
    endpointDemographic_locale,
    endpointDemographic_timezone,
    endpointDemographic_modelVersion,
    endpointDemographic_platform,
    endpointDemographic_make,
    endpointDemographic_appVersion,
    endpointDemographic_platformVersion,

    -- * EndpointItemResponse
    EndpointItemResponse (..),
    newEndpointItemResponse,
    endpointItemResponse_message,
    endpointItemResponse_statusCode,

    -- * EndpointLocation
    EndpointLocation (..),
    newEndpointLocation,
    endpointLocation_longitude,
    endpointLocation_postalCode,
    endpointLocation_country,
    endpointLocation_region,
    endpointLocation_city,
    endpointLocation_latitude,

    -- * EndpointMessageResult
    EndpointMessageResult (..),
    newEndpointMessageResult,
    endpointMessageResult_updatedToken,
    endpointMessageResult_messageId,
    endpointMessageResult_address,
    endpointMessageResult_statusMessage,
    endpointMessageResult_deliveryStatus,
    endpointMessageResult_statusCode,

    -- * EndpointRequest
    EndpointRequest (..),
    newEndpointRequest,
    endpointRequest_demographic,
    endpointRequest_user,
    endpointRequest_requestId,
    endpointRequest_metrics,
    endpointRequest_location,
    endpointRequest_optOut,
    endpointRequest_address,
    endpointRequest_effectiveDate,
    endpointRequest_attributes,
    endpointRequest_endpointStatus,
    endpointRequest_channelType,

    -- * EndpointResponse
    EndpointResponse (..),
    newEndpointResponse,
    endpointResponse_demographic,
    endpointResponse_cohortId,
    endpointResponse_user,
    endpointResponse_requestId,
    endpointResponse_creationDate,
    endpointResponse_metrics,
    endpointResponse_id,
    endpointResponse_location,
    endpointResponse_optOut,
    endpointResponse_address,
    endpointResponse_effectiveDate,
    endpointResponse_attributes,
    endpointResponse_applicationId,
    endpointResponse_endpointStatus,
    endpointResponse_channelType,

    -- * EndpointSendConfiguration
    EndpointSendConfiguration (..),
    newEndpointSendConfiguration,
    endpointSendConfiguration_context,
    endpointSendConfiguration_substitutions,
    endpointSendConfiguration_titleOverride,
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
    event_appTitle,
    event_clientSdkVersion,
    event_session,
    event_appPackageName,
    event_metrics,
    event_appVersionCode,
    event_sdkName,
    event_attributes,
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
    eventDimensions_eventType,
    eventDimensions_metrics,
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
    eventStream_lastModifiedDate,
    eventStream_externalId,
    eventStream_lastUpdatedBy,
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
    exportJobResponse_failedPieces,
    exportJobResponse_completedPieces,
    exportJobResponse_totalPieces,
    exportJobResponse_completionDate,
    exportJobResponse_totalProcessed,
    exportJobResponse_failures,
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
    gCMChannelResponse_creationDate,
    gCMChannelResponse_hasCredential,
    gCMChannelResponse_id,
    gCMChannelResponse_enabled,
    gCMChannelResponse_lastModifiedBy,
    gCMChannelResponse_isArchived,
    gCMChannelResponse_applicationId,
    gCMChannelResponse_version,
    gCMChannelResponse_credential,
    gCMChannelResponse_platform,

    -- * GCMMessage
    GCMMessage (..),
    newGCMMessage,
    gCMMessage_timeToLive,
    gCMMessage_collapseKey,
    gCMMessage_iconReference,
    gCMMessage_body,
    gCMMessage_imageUrl,
    gCMMessage_url,
    gCMMessage_substitutions,
    gCMMessage_rawContent,
    gCMMessage_sound,
    gCMMessage_silentPush,
    gCMMessage_imageIconUrl,
    gCMMessage_title,
    gCMMessage_priority,
    gCMMessage_action,
    gCMMessage_restrictedPackageName,
    gCMMessage_data,
    gCMMessage_smallImageIconUrl,

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
    importJobRequest_segmentId,
    importJobRequest_externalId,
    importJobRequest_segmentName,
    importJobRequest_registerEndpoints,
    importJobRequest_format,
    importJobRequest_s3Url,
    importJobRequest_roleArn,

    -- * ImportJobResource
    ImportJobResource (..),
    newImportJobResource,
    importJobResource_defineSegment,
    importJobResource_segmentId,
    importJobResource_externalId,
    importJobResource_segmentName,
    importJobResource_registerEndpoints,
    importJobResource_format,
    importJobResource_s3Url,
    importJobResource_roleArn,

    -- * ImportJobResponse
    ImportJobResponse (..),
    newImportJobResponse,
    importJobResponse_failedPieces,
    importJobResponse_completedPieces,
    importJobResponse_totalPieces,
    importJobResponse_completionDate,
    importJobResponse_totalProcessed,
    importJobResponse_failures,
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
    inAppCampaignSchedule_eventFilter,
    inAppCampaignSchedule_endDate,
    inAppCampaignSchedule_quietTime,

    -- * InAppMessage
    InAppMessage (..),
    newInAppMessage,
    inAppMessage_customConfig,
    inAppMessage_layout,
    inAppMessage_content,

    -- * InAppMessageBodyConfig
    InAppMessageBodyConfig (..),
    newInAppMessageBodyConfig,
    inAppMessageBodyConfig_alignment,
    inAppMessageBodyConfig_textColor,
    inAppMessageBodyConfig_body,

    -- * InAppMessageButton
    InAppMessageButton (..),
    newInAppMessageButton,
    inAppMessageButton_web,
    inAppMessageButton_ios,
    inAppMessageButton_android,
    inAppMessageButton_defaultConfig,

    -- * InAppMessageCampaign
    InAppMessageCampaign (..),
    newInAppMessageCampaign,
    inAppMessageCampaign_schedule,
    inAppMessageCampaign_sessionCap,
    inAppMessageCampaign_campaignId,
    inAppMessageCampaign_treatmentId,
    inAppMessageCampaign_inAppMessage,
    inAppMessageCampaign_totalCap,
    inAppMessageCampaign_priority,
    inAppMessageCampaign_dailyCap,

    -- * InAppMessageContent
    InAppMessageContent (..),
    newInAppMessageContent,
    inAppMessageContent_bodyConfig,
    inAppMessageContent_secondaryBtn,
    inAppMessageContent_imageUrl,
    inAppMessageContent_headerConfig,
    inAppMessageContent_backgroundColor,
    inAppMessageContent_primaryBtn,

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
    inAppTemplateRequest_customConfig,
    inAppTemplateRequest_tags,
    inAppTemplateRequest_layout,
    inAppTemplateRequest_content,
    inAppTemplateRequest_templateDescription,

    -- * InAppTemplateResponse
    InAppTemplateResponse (..),
    newInAppTemplateResponse,
    inAppTemplateResponse_customConfig,
    inAppTemplateResponse_tags,
    inAppTemplateResponse_arn,
    inAppTemplateResponse_layout,
    inAppTemplateResponse_content,
    inAppTemplateResponse_templateDescription,
    inAppTemplateResponse_version,
    inAppTemplateResponse_lastModifiedDate,
    inAppTemplateResponse_creationDate,
    inAppTemplateResponse_templateName,
    inAppTemplateResponse_templateType,

    -- * ItemResponse
    ItemResponse (..),
    newItemResponse,
    itemResponse_eventsItemResponse,
    itemResponse_endpointItemResponse,

    -- * JourneyChannelSettings
    JourneyChannelSettings (..),
    newJourneyChannelSettings,
    journeyChannelSettings_connectCampaignExecutionRoleArn,
    journeyChannelSettings_connectCampaignArn,

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
    journeyLimits_endpointReentryInterval,
    journeyLimits_messagesPerSecond,
    journeyLimits_dailyCap,
    journeyLimits_endpointReentryCap,

    -- * JourneyPushMessage
    JourneyPushMessage (..),
    newJourneyPushMessage,
    journeyPushMessage_timeToLive,

    -- * JourneyResponse
    JourneyResponse (..),
    newJourneyResponse,
    journeyResponse_tags,
    journeyResponse_schedule,
    journeyResponse_activities,
    journeyResponse_lastModifiedDate,
    journeyResponse_refreshOnSegmentUpdate,
    journeyResponse_limits,
    journeyResponse_startCondition,
    journeyResponse_startActivity,
    journeyResponse_state,
    journeyResponse_creationDate,
    journeyResponse_refreshFrequency,
    journeyResponse_localTime,
    journeyResponse_quietTime,
    journeyResponse_waitForQuietTime,
    journeyResponse_journeyChannelSettings,
    journeyResponse_name,
    journeyResponse_id,
    journeyResponse_applicationId,

    -- * JourneySMSMessage
    JourneySMSMessage (..),
    newJourneySMSMessage,
    journeySMSMessage_entityId,
    journeySMSMessage_messageType,
    journeySMSMessage_senderId,
    journeySMSMessage_templateId,
    journeySMSMessage_originationNumber,

    -- * JourneySchedule
    JourneySchedule (..),
    newJourneySchedule,
    journeySchedule_timezone,
    journeySchedule_endTime,
    journeySchedule_startTime,

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
    message_timeToLive,
    message_jsonBody,
    message_body,
    message_imageUrl,
    message_url,
    message_imageSmallIconUrl,
    message_rawContent,
    message_silentPush,
    message_imageIconUrl,
    message_title,
    message_mediaUrl,
    message_action,

    -- * MessageBody
    MessageBody (..),
    newMessageBody,
    messageBody_message,
    messageBody_requestID,

    -- * MessageConfiguration
    MessageConfiguration (..),
    newMessageConfiguration,
    messageConfiguration_aDMMessage,
    messageConfiguration_sMSMessage,
    messageConfiguration_inAppMessage,
    messageConfiguration_defaultMessage,
    messageConfiguration_aPNSMessage,
    messageConfiguration_customMessage,
    messageConfiguration_gCMMessage,
    messageConfiguration_emailMessage,
    messageConfiguration_baiduMessage,

    -- * MessageRequest
    MessageRequest (..),
    newMessageRequest,
    messageRequest_context,
    messageRequest_endpoints,
    messageRequest_addresses,
    messageRequest_templateConfiguration,
    messageRequest_traceId,
    messageRequest_messageConfiguration,

    -- * MessageResponse
    MessageResponse (..),
    newMessageResponse,
    messageResponse_requestId,
    messageResponse_endpointResult,
    messageResponse_result,
    messageResponse_applicationId,

    -- * MessageResult
    MessageResult (..),
    newMessageResult,
    messageResult_updatedToken,
    messageResult_messageId,
    messageResult_statusMessage,
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
    multiConditionalSplitActivity_defaultActivity,
    multiConditionalSplitActivity_evaluationWaitTime,

    -- * NumberValidateRequest
    NumberValidateRequest (..),
    newNumberValidateRequest,
    numberValidateRequest_isoCountryCode,
    numberValidateRequest_phoneNumber,

    -- * NumberValidateResponse
    NumberValidateResponse (..),
    newNumberValidateResponse,
    numberValidateResponse_zipCode,
    numberValidateResponse_phoneTypeCode,
    numberValidateResponse_country,
    numberValidateResponse_timezone,
    numberValidateResponse_cleansedPhoneNumberE164,
    numberValidateResponse_county,
    numberValidateResponse_carrier,
    numberValidateResponse_cleansedPhoneNumberNational,
    numberValidateResponse_originalPhoneNumber,
    numberValidateResponse_originalCountryCodeIso2,
    numberValidateResponse_phoneType,
    numberValidateResponse_city,
    numberValidateResponse_countryCodeNumeric,
    numberValidateResponse_countryCodeIso2,

    -- * OverrideButtonConfiguration
    OverrideButtonConfiguration (..),
    newOverrideButtonConfiguration,
    overrideButtonConfiguration_link,
    overrideButtonConfiguration_buttonAction,

    -- * PublicEndpoint
    PublicEndpoint (..),
    newPublicEndpoint,
    publicEndpoint_demographic,
    publicEndpoint_user,
    publicEndpoint_requestId,
    publicEndpoint_metrics,
    publicEndpoint_location,
    publicEndpoint_optOut,
    publicEndpoint_address,
    publicEndpoint_effectiveDate,
    publicEndpoint_attributes,
    publicEndpoint_endpointStatus,
    publicEndpoint_channelType,

    -- * PushMessageActivity
    PushMessageActivity (..),
    newPushMessageActivity,
    pushMessageActivity_templateName,
    pushMessageActivity_nextActivity,
    pushMessageActivity_templateVersion,
    pushMessageActivity_messageConfig,

    -- * PushNotificationTemplateRequest
    PushNotificationTemplateRequest (..),
    newPushNotificationTemplateRequest,
    pushNotificationTemplateRequest_tags,
    pushNotificationTemplateRequest_adm,
    pushNotificationTemplateRequest_apns,
    pushNotificationTemplateRequest_default,
    pushNotificationTemplateRequest_recommenderId,
    pushNotificationTemplateRequest_defaultSubstitutions,
    pushNotificationTemplateRequest_baidu,
    pushNotificationTemplateRequest_templateDescription,
    pushNotificationTemplateRequest_gcm,

    -- * PushNotificationTemplateResponse
    PushNotificationTemplateResponse (..),
    newPushNotificationTemplateResponse,
    pushNotificationTemplateResponse_tags,
    pushNotificationTemplateResponse_adm,
    pushNotificationTemplateResponse_apns,
    pushNotificationTemplateResponse_default,
    pushNotificationTemplateResponse_arn,
    pushNotificationTemplateResponse_recommenderId,
    pushNotificationTemplateResponse_defaultSubstitutions,
    pushNotificationTemplateResponse_baidu,
    pushNotificationTemplateResponse_templateDescription,
    pushNotificationTemplateResponse_version,
    pushNotificationTemplateResponse_gcm,
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
    recommenderConfigurationResponse_name,
    recommenderConfigurationResponse_recommendationTransformerUri,
    recommenderConfigurationResponse_description,
    recommenderConfigurationResponse_recommendationsDisplayName,
    recommenderConfigurationResponse_recommendationProviderIdType,
    recommenderConfigurationResponse_attributes,
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
    sMSChannelRequest_senderId,
    sMSChannelRequest_enabled,
    sMSChannelRequest_shortCode,

    -- * SMSChannelResponse
    SMSChannelResponse (..),
    newSMSChannelResponse,
    sMSChannelResponse_lastModifiedDate,
    sMSChannelResponse_senderId,
    sMSChannelResponse_creationDate,
    sMSChannelResponse_hasCredential,
    sMSChannelResponse_id,
    sMSChannelResponse_enabled,
    sMSChannelResponse_shortCode,
    sMSChannelResponse_promotionalMessagesPerSecond,
    sMSChannelResponse_lastModifiedBy,
    sMSChannelResponse_isArchived,
    sMSChannelResponse_applicationId,
    sMSChannelResponse_version,
    sMSChannelResponse_transactionalMessagesPerSecond,
    sMSChannelResponse_platform,

    -- * SMSMessage
    SMSMessage (..),
    newSMSMessage,
    sMSMessage_entityId,
    sMSMessage_messageType,
    sMSMessage_senderId,
    sMSMessage_body,
    sMSMessage_substitutions,
    sMSMessage_templateId,
    sMSMessage_originationNumber,
    sMSMessage_keyword,
    sMSMessage_mediaUrl,

    -- * SMSMessageActivity
    SMSMessageActivity (..),
    newSMSMessageActivity,
    sMSMessageActivity_templateName,
    sMSMessageActivity_nextActivity,
    sMSMessageActivity_templateVersion,
    sMSMessageActivity_messageConfig,

    -- * SMSTemplateRequest
    SMSTemplateRequest (..),
    newSMSTemplateRequest,
    sMSTemplateRequest_tags,
    sMSTemplateRequest_body,
    sMSTemplateRequest_recommenderId,
    sMSTemplateRequest_defaultSubstitutions,
    sMSTemplateRequest_templateDescription,

    -- * SMSTemplateResponse
    SMSTemplateResponse (..),
    newSMSTemplateResponse,
    sMSTemplateResponse_tags,
    sMSTemplateResponse_arn,
    sMSTemplateResponse_body,
    sMSTemplateResponse_recommenderId,
    sMSTemplateResponse_defaultSubstitutions,
    sMSTemplateResponse_templateDescription,
    sMSTemplateResponse_version,
    sMSTemplateResponse_lastModifiedDate,
    sMSTemplateResponse_creationDate,
    sMSTemplateResponse_templateName,
    sMSTemplateResponse_templateType,

    -- * Schedule
    Schedule (..),
    newSchedule,
    schedule_eventFilter,
    schedule_timezone,
    schedule_frequency,
    schedule_endTime,
    schedule_quietTime,
    schedule_isLocalTime,
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
    segmentDemographics_channel,
    segmentDemographics_platform,
    segmentDemographics_make,
    segmentDemographics_appVersion,
    segmentDemographics_deviceType,

    -- * SegmentDimensions
    SegmentDimensions (..),
    newSegmentDimensions,
    segmentDimensions_demographic,
    segmentDimensions_metrics,
    segmentDimensions_userAttributes,
    segmentDimensions_location,
    segmentDimensions_attributes,
    segmentDimensions_behavior,

    -- * SegmentGroup
    SegmentGroup (..),
    newSegmentGroup,
    segmentGroup_type,
    segmentGroup_dimensions,
    segmentGroup_sourceType,
    segmentGroup_sourceSegments,

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
    segmentResponse_tags,
    segmentResponse_name,
    segmentResponse_lastModifiedDate,
    segmentResponse_importDefinition,
    segmentResponse_dimensions,
    segmentResponse_segmentGroups,
    segmentResponse_version,
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
    sendUsersMessageRequest_templateConfiguration,
    sendUsersMessageRequest_traceId,
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
    session_duration,
    session_stopTimestamp,
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
    simpleCondition_segmentCondition,
    simpleCondition_eventCondition,
    simpleCondition_segmentDimensions,

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
    startCondition_segmentStartCondition,
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
    templateConfiguration_emailTemplate,
    templateConfiguration_sMSTemplate,
    templateConfiguration_voiceTemplate,
    templateConfiguration_pushTemplate,

    -- * TemplateCreateMessageBody
    TemplateCreateMessageBody (..),
    newTemplateCreateMessageBody,
    templateCreateMessageBody_message,
    templateCreateMessageBody_requestID,
    templateCreateMessageBody_arn,

    -- * TemplateResponse
    TemplateResponse (..),
    newTemplateResponse,
    templateResponse_tags,
    templateResponse_arn,
    templateResponse_defaultSubstitutions,
    templateResponse_templateDescription,
    templateResponse_version,
    templateResponse_lastModifiedDate,
    templateResponse_creationDate,
    templateResponse_templateName,
    templateResponse_templateType,

    -- * TemplateVersionResponse
    TemplateVersionResponse (..),
    newTemplateVersionResponse,
    templateVersionResponse_defaultSubstitutions,
    templateVersionResponse_templateDescription,
    templateVersionResponse_version,
    templateVersionResponse_lastModifiedDate,
    templateVersionResponse_creationDate,
    templateVersionResponse_templateName,
    templateVersionResponse_templateType,

    -- * TemplateVersionsResponse
    TemplateVersionsResponse (..),
    newTemplateVersionsResponse,
    templateVersionsResponse_message,
    templateVersionsResponse_nextToken,
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
    treatmentResource_schedule,
    treatmentResource_customDeliveryConfiguration,
    treatmentResource_treatmentName,
    treatmentResource_state,
    treatmentResource_messageConfiguration,
    treatmentResource_treatmentDescription,
    treatmentResource_templateConfiguration,
    treatmentResource_id,
    treatmentResource_sizePercent,

    -- * UpdateAttributesRequest
    UpdateAttributesRequest (..),
    newUpdateAttributesRequest,
    updateAttributesRequest_blacklist,

    -- * UpdateRecommenderConfiguration
    UpdateRecommenderConfiguration (..),
    newUpdateRecommenderConfiguration,
    updateRecommenderConfiguration_name,
    updateRecommenderConfiguration_recommendationTransformerUri,
    updateRecommenderConfiguration_description,
    updateRecommenderConfiguration_recommendationsDisplayName,
    updateRecommenderConfiguration_recommendationProviderIdType,
    updateRecommenderConfiguration_attributes,
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
    voiceChannelResponse_creationDate,
    voiceChannelResponse_hasCredential,
    voiceChannelResponse_id,
    voiceChannelResponse_enabled,
    voiceChannelResponse_lastModifiedBy,
    voiceChannelResponse_isArchived,
    voiceChannelResponse_applicationId,
    voiceChannelResponse_version,
    voiceChannelResponse_platform,

    -- * VoiceMessage
    VoiceMessage (..),
    newVoiceMessage,
    voiceMessage_voiceId,
    voiceMessage_body,
    voiceMessage_substitutions,
    voiceMessage_originationNumber,
    voiceMessage_languageCode,

    -- * VoiceTemplateRequest
    VoiceTemplateRequest (..),
    newVoiceTemplateRequest,
    voiceTemplateRequest_tags,
    voiceTemplateRequest_voiceId,
    voiceTemplateRequest_body,
    voiceTemplateRequest_defaultSubstitutions,
    voiceTemplateRequest_languageCode,
    voiceTemplateRequest_templateDescription,

    -- * VoiceTemplateResponse
    VoiceTemplateResponse (..),
    newVoiceTemplateResponse,
    voiceTemplateResponse_tags,
    voiceTemplateResponse_voiceId,
    voiceTemplateResponse_arn,
    voiceTemplateResponse_body,
    voiceTemplateResponse_defaultSubstitutions,
    voiceTemplateResponse_languageCode,
    voiceTemplateResponse_templateDescription,
    voiceTemplateResponse_version,
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
    writeApplicationSettingsRequest_limits,
    writeApplicationSettingsRequest_eventTaggingEnabled,
    writeApplicationSettingsRequest_cloudWatchMetricsEnabled,
    writeApplicationSettingsRequest_quietTime,
    writeApplicationSettingsRequest_campaignHook,

    -- * WriteCampaignRequest
    WriteCampaignRequest (..),
    newWriteCampaignRequest,
    writeCampaignRequest_tags,
    writeCampaignRequest_schedule,
    writeCampaignRequest_customDeliveryConfiguration,
    writeCampaignRequest_name,
    writeCampaignRequest_hook,
    writeCampaignRequest_segmentVersion,
    writeCampaignRequest_limits,
    writeCampaignRequest_treatmentName,
    writeCampaignRequest_holdoutPercent,
    writeCampaignRequest_segmentId,
    writeCampaignRequest_additionalTreatments,
    writeCampaignRequest_messageConfiguration,
    writeCampaignRequest_description,
    writeCampaignRequest_treatmentDescription,
    writeCampaignRequest_priority,
    writeCampaignRequest_templateConfiguration,
    writeCampaignRequest_isPaused,

    -- * WriteEventStream
    WriteEventStream (..),
    newWriteEventStream,
    writeEventStream_roleArn,
    writeEventStream_destinationStreamArn,

    -- * WriteJourneyRequest
    WriteJourneyRequest (..),
    newWriteJourneyRequest,
    writeJourneyRequest_schedule,
    writeJourneyRequest_activities,
    writeJourneyRequest_lastModifiedDate,
    writeJourneyRequest_refreshOnSegmentUpdate,
    writeJourneyRequest_limits,
    writeJourneyRequest_startCondition,
    writeJourneyRequest_startActivity,
    writeJourneyRequest_state,
    writeJourneyRequest_creationDate,
    writeJourneyRequest_refreshFrequency,
    writeJourneyRequest_localTime,
    writeJourneyRequest_quietTime,
    writeJourneyRequest_waitForQuietTime,
    writeJourneyRequest_name,

    -- * WriteSegmentRequest
    WriteSegmentRequest (..),
    newWriteSegmentRequest,
    writeSegmentRequest_tags,
    writeSegmentRequest_name,
    writeSegmentRequest_dimensions,
    writeSegmentRequest_segmentGroups,

    -- * WriteTreatmentResource
    WriteTreatmentResource (..),
    newWriteTreatmentResource,
    writeTreatmentResource_schedule,
    writeTreatmentResource_customDeliveryConfiguration,
    writeTreatmentResource_treatmentName,
    writeTreatmentResource_messageConfiguration,
    writeTreatmentResource_treatmentDescription,
    writeTreatmentResource_templateConfiguration,
    writeTreatmentResource_sizePercent,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Pinpoint.Types.ADMChannelRequest
import Amazonka.Pinpoint.Types.ADMChannelResponse
import Amazonka.Pinpoint.Types.ADMMessage
import Amazonka.Pinpoint.Types.APNSChannelRequest
import Amazonka.Pinpoint.Types.APNSChannelResponse
import Amazonka.Pinpoint.Types.APNSMessage
import Amazonka.Pinpoint.Types.APNSPushNotificationTemplate
import Amazonka.Pinpoint.Types.APNSSandboxChannelRequest
import Amazonka.Pinpoint.Types.APNSSandboxChannelResponse
import Amazonka.Pinpoint.Types.APNSVoipChannelRequest
import Amazonka.Pinpoint.Types.APNSVoipChannelResponse
import Amazonka.Pinpoint.Types.APNSVoipSandboxChannelRequest
import Amazonka.Pinpoint.Types.APNSVoipSandboxChannelResponse
import Amazonka.Pinpoint.Types.Action
import Amazonka.Pinpoint.Types.ActivitiesResponse
import Amazonka.Pinpoint.Types.Activity
import Amazonka.Pinpoint.Types.ActivityResponse
import Amazonka.Pinpoint.Types.AddressConfiguration
import Amazonka.Pinpoint.Types.Alignment
import Amazonka.Pinpoint.Types.AndroidPushNotificationTemplate
import Amazonka.Pinpoint.Types.ApplicationDateRangeKpiResponse
import Amazonka.Pinpoint.Types.ApplicationResponse
import Amazonka.Pinpoint.Types.ApplicationSettingsResource
import Amazonka.Pinpoint.Types.ApplicationsResponse
import Amazonka.Pinpoint.Types.AttributeDimension
import Amazonka.Pinpoint.Types.AttributeType
import Amazonka.Pinpoint.Types.AttributesResource
import Amazonka.Pinpoint.Types.BaiduChannelRequest
import Amazonka.Pinpoint.Types.BaiduChannelResponse
import Amazonka.Pinpoint.Types.BaiduMessage
import Amazonka.Pinpoint.Types.BaseKpiResult
import Amazonka.Pinpoint.Types.ButtonAction
import Amazonka.Pinpoint.Types.CampaignCustomMessage
import Amazonka.Pinpoint.Types.CampaignDateRangeKpiResponse
import Amazonka.Pinpoint.Types.CampaignEmailMessage
import Amazonka.Pinpoint.Types.CampaignEventFilter
import Amazonka.Pinpoint.Types.CampaignHook
import Amazonka.Pinpoint.Types.CampaignInAppMessage
import Amazonka.Pinpoint.Types.CampaignLimits
import Amazonka.Pinpoint.Types.CampaignResponse
import Amazonka.Pinpoint.Types.CampaignSmsMessage
import Amazonka.Pinpoint.Types.CampaignState
import Amazonka.Pinpoint.Types.CampaignStatus
import Amazonka.Pinpoint.Types.CampaignsResponse
import Amazonka.Pinpoint.Types.ChannelResponse
import Amazonka.Pinpoint.Types.ChannelType
import Amazonka.Pinpoint.Types.ChannelsResponse
import Amazonka.Pinpoint.Types.Condition
import Amazonka.Pinpoint.Types.ConditionalSplitActivity
import Amazonka.Pinpoint.Types.ContactCenterActivity
import Amazonka.Pinpoint.Types.CreateApplicationRequest
import Amazonka.Pinpoint.Types.CreateRecommenderConfiguration
import Amazonka.Pinpoint.Types.CreateTemplateMessageBody
import Amazonka.Pinpoint.Types.CustomDeliveryConfiguration
import Amazonka.Pinpoint.Types.CustomMessageActivity
import Amazonka.Pinpoint.Types.DefaultButtonConfiguration
import Amazonka.Pinpoint.Types.DefaultMessage
import Amazonka.Pinpoint.Types.DefaultPushNotificationMessage
import Amazonka.Pinpoint.Types.DefaultPushNotificationTemplate
import Amazonka.Pinpoint.Types.DefinitionFormat
import Amazonka.Pinpoint.Types.DeliveryStatus
import Amazonka.Pinpoint.Types.DimensionType
import Amazonka.Pinpoint.Types.DirectMessageConfiguration
import Amazonka.Pinpoint.Types.Duration
import Amazonka.Pinpoint.Types.EmailChannelRequest
import Amazonka.Pinpoint.Types.EmailChannelResponse
import Amazonka.Pinpoint.Types.EmailMessage
import Amazonka.Pinpoint.Types.EmailMessageActivity
import Amazonka.Pinpoint.Types.EmailTemplateRequest
import Amazonka.Pinpoint.Types.EmailTemplateResponse
import Amazonka.Pinpoint.Types.EndpointBatchItem
import Amazonka.Pinpoint.Types.EndpointBatchRequest
import Amazonka.Pinpoint.Types.EndpointDemographic
import Amazonka.Pinpoint.Types.EndpointItemResponse
import Amazonka.Pinpoint.Types.EndpointLocation
import Amazonka.Pinpoint.Types.EndpointMessageResult
import Amazonka.Pinpoint.Types.EndpointRequest
import Amazonka.Pinpoint.Types.EndpointResponse
import Amazonka.Pinpoint.Types.EndpointSendConfiguration
import Amazonka.Pinpoint.Types.EndpointTypesElement
import Amazonka.Pinpoint.Types.EndpointUser
import Amazonka.Pinpoint.Types.EndpointsResponse
import Amazonka.Pinpoint.Types.Event
import Amazonka.Pinpoint.Types.EventCondition
import Amazonka.Pinpoint.Types.EventDimensions
import Amazonka.Pinpoint.Types.EventFilter
import Amazonka.Pinpoint.Types.EventItemResponse
import Amazonka.Pinpoint.Types.EventStartCondition
import Amazonka.Pinpoint.Types.EventStream
import Amazonka.Pinpoint.Types.EventsBatch
import Amazonka.Pinpoint.Types.EventsRequest
import Amazonka.Pinpoint.Types.EventsResponse
import Amazonka.Pinpoint.Types.ExportJobRequest
import Amazonka.Pinpoint.Types.ExportJobResource
import Amazonka.Pinpoint.Types.ExportJobResponse
import Amazonka.Pinpoint.Types.ExportJobsResponse
import Amazonka.Pinpoint.Types.FilterType
import Amazonka.Pinpoint.Types.Frequency
import Amazonka.Pinpoint.Types.GCMChannelRequest
import Amazonka.Pinpoint.Types.GCMChannelResponse
import Amazonka.Pinpoint.Types.GCMMessage
import Amazonka.Pinpoint.Types.GPSCoordinates
import Amazonka.Pinpoint.Types.GPSPointDimension
import Amazonka.Pinpoint.Types.HoldoutActivity
import Amazonka.Pinpoint.Types.ImportJobRequest
import Amazonka.Pinpoint.Types.ImportJobResource
import Amazonka.Pinpoint.Types.ImportJobResponse
import Amazonka.Pinpoint.Types.ImportJobsResponse
import Amazonka.Pinpoint.Types.InAppCampaignSchedule
import Amazonka.Pinpoint.Types.InAppMessage
import Amazonka.Pinpoint.Types.InAppMessageBodyConfig
import Amazonka.Pinpoint.Types.InAppMessageButton
import Amazonka.Pinpoint.Types.InAppMessageCampaign
import Amazonka.Pinpoint.Types.InAppMessageContent
import Amazonka.Pinpoint.Types.InAppMessageHeaderConfig
import Amazonka.Pinpoint.Types.InAppMessagesResponse
import Amazonka.Pinpoint.Types.InAppTemplateRequest
import Amazonka.Pinpoint.Types.InAppTemplateResponse
import Amazonka.Pinpoint.Types.Include
import Amazonka.Pinpoint.Types.ItemResponse
import Amazonka.Pinpoint.Types.JobStatus
import Amazonka.Pinpoint.Types.JourneyChannelSettings
import Amazonka.Pinpoint.Types.JourneyCustomMessage
import Amazonka.Pinpoint.Types.JourneyDateRangeKpiResponse
import Amazonka.Pinpoint.Types.JourneyEmailMessage
import Amazonka.Pinpoint.Types.JourneyExecutionActivityMetricsResponse
import Amazonka.Pinpoint.Types.JourneyExecutionMetricsResponse
import Amazonka.Pinpoint.Types.JourneyLimits
import Amazonka.Pinpoint.Types.JourneyPushMessage
import Amazonka.Pinpoint.Types.JourneyResponse
import Amazonka.Pinpoint.Types.JourneySMSMessage
import Amazonka.Pinpoint.Types.JourneySchedule
import Amazonka.Pinpoint.Types.JourneyStateRequest
import Amazonka.Pinpoint.Types.JourneysResponse
import Amazonka.Pinpoint.Types.Layout
import Amazonka.Pinpoint.Types.ListRecommenderConfigurationsResponse
import Amazonka.Pinpoint.Types.Message
import Amazonka.Pinpoint.Types.MessageBody
import Amazonka.Pinpoint.Types.MessageConfiguration
import Amazonka.Pinpoint.Types.MessageRequest
import Amazonka.Pinpoint.Types.MessageResponse
import Amazonka.Pinpoint.Types.MessageResult
import Amazonka.Pinpoint.Types.MessageType
import Amazonka.Pinpoint.Types.MetricDimension
import Amazonka.Pinpoint.Types.Mode
import Amazonka.Pinpoint.Types.MultiConditionalBranch
import Amazonka.Pinpoint.Types.MultiConditionalSplitActivity
import Amazonka.Pinpoint.Types.NumberValidateRequest
import Amazonka.Pinpoint.Types.NumberValidateResponse
import Amazonka.Pinpoint.Types.Operator
import Amazonka.Pinpoint.Types.OverrideButtonConfiguration
import Amazonka.Pinpoint.Types.PublicEndpoint
import Amazonka.Pinpoint.Types.PushMessageActivity
import Amazonka.Pinpoint.Types.PushNotificationTemplateRequest
import Amazonka.Pinpoint.Types.PushNotificationTemplateResponse
import Amazonka.Pinpoint.Types.QuietTime
import Amazonka.Pinpoint.Types.RandomSplitActivity
import Amazonka.Pinpoint.Types.RandomSplitEntry
import Amazonka.Pinpoint.Types.RawEmail
import Amazonka.Pinpoint.Types.RecencyDimension
import Amazonka.Pinpoint.Types.RecencyType
import Amazonka.Pinpoint.Types.RecommenderConfigurationResponse
import Amazonka.Pinpoint.Types.ResultRow
import Amazonka.Pinpoint.Types.ResultRowValue
import Amazonka.Pinpoint.Types.SMSChannelRequest
import Amazonka.Pinpoint.Types.SMSChannelResponse
import Amazonka.Pinpoint.Types.SMSMessage
import Amazonka.Pinpoint.Types.SMSMessageActivity
import Amazonka.Pinpoint.Types.SMSTemplateRequest
import Amazonka.Pinpoint.Types.SMSTemplateResponse
import Amazonka.Pinpoint.Types.Schedule
import Amazonka.Pinpoint.Types.SegmentBehaviors
import Amazonka.Pinpoint.Types.SegmentCondition
import Amazonka.Pinpoint.Types.SegmentDemographics
import Amazonka.Pinpoint.Types.SegmentDimensions
import Amazonka.Pinpoint.Types.SegmentGroup
import Amazonka.Pinpoint.Types.SegmentGroupList
import Amazonka.Pinpoint.Types.SegmentImportResource
import Amazonka.Pinpoint.Types.SegmentLocation
import Amazonka.Pinpoint.Types.SegmentReference
import Amazonka.Pinpoint.Types.SegmentResponse
import Amazonka.Pinpoint.Types.SegmentType
import Amazonka.Pinpoint.Types.SegmentsResponse
import Amazonka.Pinpoint.Types.SendUsersMessageRequest
import Amazonka.Pinpoint.Types.SendUsersMessageResponse
import Amazonka.Pinpoint.Types.Session
import Amazonka.Pinpoint.Types.SetDimension
import Amazonka.Pinpoint.Types.SimpleCondition
import Amazonka.Pinpoint.Types.SimpleEmail
import Amazonka.Pinpoint.Types.SimpleEmailPart
import Amazonka.Pinpoint.Types.SourceType
import Amazonka.Pinpoint.Types.StartCondition
import Amazonka.Pinpoint.Types.State
import Amazonka.Pinpoint.Types.TagsModel
import Amazonka.Pinpoint.Types.Template
import Amazonka.Pinpoint.Types.TemplateActiveVersionRequest
import Amazonka.Pinpoint.Types.TemplateConfiguration
import Amazonka.Pinpoint.Types.TemplateCreateMessageBody
import Amazonka.Pinpoint.Types.TemplateResponse
import Amazonka.Pinpoint.Types.TemplateType
import Amazonka.Pinpoint.Types.TemplateVersionResponse
import Amazonka.Pinpoint.Types.TemplateVersionsResponse
import Amazonka.Pinpoint.Types.TemplatesResponse
import Amazonka.Pinpoint.Types.TreatmentResource
import Amazonka.Pinpoint.Types.Type
import Amazonka.Pinpoint.Types.UpdateAttributesRequest
import Amazonka.Pinpoint.Types.UpdateRecommenderConfiguration
import Amazonka.Pinpoint.Types.VoiceChannelRequest
import Amazonka.Pinpoint.Types.VoiceChannelResponse
import Amazonka.Pinpoint.Types.VoiceMessage
import Amazonka.Pinpoint.Types.VoiceTemplateRequest
import Amazonka.Pinpoint.Types.VoiceTemplateResponse
import Amazonka.Pinpoint.Types.WaitActivity
import Amazonka.Pinpoint.Types.WaitTime
import Amazonka.Pinpoint.Types.WriteApplicationSettingsRequest
import Amazonka.Pinpoint.Types.WriteCampaignRequest
import Amazonka.Pinpoint.Types.WriteEventStream
import Amazonka.Pinpoint.Types.WriteJourneyRequest
import Amazonka.Pinpoint.Types.WriteSegmentRequest
import Amazonka.Pinpoint.Types.WriteTreatmentResource
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

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
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | Provides information about an API request or response.
_NotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError
    defaultService
    "NotFoundException"
    Prelude.. Core.hasStatus 404

-- | Provides information about an API request or response.
_InternalServerErrorException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerErrorException =
  Core._MatchServiceError
    defaultService
    "InternalServerErrorException"
    Prelude.. Core.hasStatus 500

-- | Provides information about an API request or response.
_ForbiddenException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ForbiddenException =
  Core._MatchServiceError
    defaultService
    "ForbiddenException"
    Prelude.. Core.hasStatus 403

-- | Provides information about an API request or response.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | Provides information about an API request or response.
_MethodNotAllowedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MethodNotAllowedException =
  Core._MatchServiceError
    defaultService
    "MethodNotAllowedException"
    Prelude.. Core.hasStatus 405

-- | Provides information about an API request or response.
_PayloadTooLargeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_PayloadTooLargeException =
  Core._MatchServiceError
    defaultService
    "PayloadTooLargeException"
    Prelude.. Core.hasStatus 413

-- | Provides information about an API request or response.
_BadRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError
    defaultService
    "BadRequestException"
    Prelude.. Core.hasStatus 400

-- | Provides information about an API request or response.
_TooManyRequestsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyRequestsException =
  Core._MatchServiceError
    defaultService
    "TooManyRequestsException"
    Prelude.. Core.hasStatus 429
