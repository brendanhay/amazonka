{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Pinpoint.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _BadRequestException,
    _ConflictException,
    _ForbiddenException,
    _InternalServerErrorException,
    _MethodNotAllowedException,
    _NotFoundException,
    _PayloadTooLargeException,
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

    -- * DayOfWeek
    DayOfWeek (..),

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

    -- * JourneyRunStatus
    JourneyRunStatus (..),

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

    -- * TimezoneEstimationMethodsElement
    TimezoneEstimationMethodsElement (..),

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
    aDMChannelResponse_applicationId,
    aDMChannelResponse_creationDate,
    aDMChannelResponse_enabled,
    aDMChannelResponse_hasCredential,
    aDMChannelResponse_id,
    aDMChannelResponse_isArchived,
    aDMChannelResponse_lastModifiedBy,
    aDMChannelResponse_lastModifiedDate,
    aDMChannelResponse_version,
    aDMChannelResponse_platform,

    -- * ADMMessage
    ADMMessage (..),
    newADMMessage,
    aDMMessage_action,
    aDMMessage_body,
    aDMMessage_consolidationKey,
    aDMMessage_data,
    aDMMessage_expiresAfter,
    aDMMessage_iconReference,
    aDMMessage_imageIconUrl,
    aDMMessage_imageUrl,
    aDMMessage_md5,
    aDMMessage_rawContent,
    aDMMessage_silentPush,
    aDMMessage_smallImageIconUrl,
    aDMMessage_sound,
    aDMMessage_substitutions,
    aDMMessage_title,
    aDMMessage_url,

    -- * APNSChannelRequest
    APNSChannelRequest (..),
    newAPNSChannelRequest,
    aPNSChannelRequest_bundleId,
    aPNSChannelRequest_certificate,
    aPNSChannelRequest_defaultAuthenticationMethod,
    aPNSChannelRequest_enabled,
    aPNSChannelRequest_privateKey,
    aPNSChannelRequest_teamId,
    aPNSChannelRequest_tokenKey,
    aPNSChannelRequest_tokenKeyId,

    -- * APNSChannelResponse
    APNSChannelResponse (..),
    newAPNSChannelResponse,
    aPNSChannelResponse_applicationId,
    aPNSChannelResponse_creationDate,
    aPNSChannelResponse_defaultAuthenticationMethod,
    aPNSChannelResponse_enabled,
    aPNSChannelResponse_hasCredential,
    aPNSChannelResponse_hasTokenKey,
    aPNSChannelResponse_id,
    aPNSChannelResponse_isArchived,
    aPNSChannelResponse_lastModifiedBy,
    aPNSChannelResponse_lastModifiedDate,
    aPNSChannelResponse_version,
    aPNSChannelResponse_platform,

    -- * APNSMessage
    APNSMessage (..),
    newAPNSMessage,
    aPNSMessage_aPNSPushType,
    aPNSMessage_action,
    aPNSMessage_badge,
    aPNSMessage_body,
    aPNSMessage_category,
    aPNSMessage_collapseId,
    aPNSMessage_data,
    aPNSMessage_mediaUrl,
    aPNSMessage_preferredAuthenticationMethod,
    aPNSMessage_priority,
    aPNSMessage_rawContent,
    aPNSMessage_silentPush,
    aPNSMessage_sound,
    aPNSMessage_substitutions,
    aPNSMessage_threadId,
    aPNSMessage_timeToLive,
    aPNSMessage_title,
    aPNSMessage_url,

    -- * APNSPushNotificationTemplate
    APNSPushNotificationTemplate (..),
    newAPNSPushNotificationTemplate,
    aPNSPushNotificationTemplate_action,
    aPNSPushNotificationTemplate_body,
    aPNSPushNotificationTemplate_mediaUrl,
    aPNSPushNotificationTemplate_rawContent,
    aPNSPushNotificationTemplate_sound,
    aPNSPushNotificationTemplate_title,
    aPNSPushNotificationTemplate_url,

    -- * APNSSandboxChannelRequest
    APNSSandboxChannelRequest (..),
    newAPNSSandboxChannelRequest,
    aPNSSandboxChannelRequest_bundleId,
    aPNSSandboxChannelRequest_certificate,
    aPNSSandboxChannelRequest_defaultAuthenticationMethod,
    aPNSSandboxChannelRequest_enabled,
    aPNSSandboxChannelRequest_privateKey,
    aPNSSandboxChannelRequest_teamId,
    aPNSSandboxChannelRequest_tokenKey,
    aPNSSandboxChannelRequest_tokenKeyId,

    -- * APNSSandboxChannelResponse
    APNSSandboxChannelResponse (..),
    newAPNSSandboxChannelResponse,
    aPNSSandboxChannelResponse_applicationId,
    aPNSSandboxChannelResponse_creationDate,
    aPNSSandboxChannelResponse_defaultAuthenticationMethod,
    aPNSSandboxChannelResponse_enabled,
    aPNSSandboxChannelResponse_hasCredential,
    aPNSSandboxChannelResponse_hasTokenKey,
    aPNSSandboxChannelResponse_id,
    aPNSSandboxChannelResponse_isArchived,
    aPNSSandboxChannelResponse_lastModifiedBy,
    aPNSSandboxChannelResponse_lastModifiedDate,
    aPNSSandboxChannelResponse_version,
    aPNSSandboxChannelResponse_platform,

    -- * APNSVoipChannelRequest
    APNSVoipChannelRequest (..),
    newAPNSVoipChannelRequest,
    aPNSVoipChannelRequest_bundleId,
    aPNSVoipChannelRequest_certificate,
    aPNSVoipChannelRequest_defaultAuthenticationMethod,
    aPNSVoipChannelRequest_enabled,
    aPNSVoipChannelRequest_privateKey,
    aPNSVoipChannelRequest_teamId,
    aPNSVoipChannelRequest_tokenKey,
    aPNSVoipChannelRequest_tokenKeyId,

    -- * APNSVoipChannelResponse
    APNSVoipChannelResponse (..),
    newAPNSVoipChannelResponse,
    aPNSVoipChannelResponse_applicationId,
    aPNSVoipChannelResponse_creationDate,
    aPNSVoipChannelResponse_defaultAuthenticationMethod,
    aPNSVoipChannelResponse_enabled,
    aPNSVoipChannelResponse_hasCredential,
    aPNSVoipChannelResponse_hasTokenKey,
    aPNSVoipChannelResponse_id,
    aPNSVoipChannelResponse_isArchived,
    aPNSVoipChannelResponse_lastModifiedBy,
    aPNSVoipChannelResponse_lastModifiedDate,
    aPNSVoipChannelResponse_version,
    aPNSVoipChannelResponse_platform,

    -- * APNSVoipSandboxChannelRequest
    APNSVoipSandboxChannelRequest (..),
    newAPNSVoipSandboxChannelRequest,
    aPNSVoipSandboxChannelRequest_bundleId,
    aPNSVoipSandboxChannelRequest_certificate,
    aPNSVoipSandboxChannelRequest_defaultAuthenticationMethod,
    aPNSVoipSandboxChannelRequest_enabled,
    aPNSVoipSandboxChannelRequest_privateKey,
    aPNSVoipSandboxChannelRequest_teamId,
    aPNSVoipSandboxChannelRequest_tokenKey,
    aPNSVoipSandboxChannelRequest_tokenKeyId,

    -- * APNSVoipSandboxChannelResponse
    APNSVoipSandboxChannelResponse (..),
    newAPNSVoipSandboxChannelResponse,
    aPNSVoipSandboxChannelResponse_applicationId,
    aPNSVoipSandboxChannelResponse_creationDate,
    aPNSVoipSandboxChannelResponse_defaultAuthenticationMethod,
    aPNSVoipSandboxChannelResponse_enabled,
    aPNSVoipSandboxChannelResponse_hasCredential,
    aPNSVoipSandboxChannelResponse_hasTokenKey,
    aPNSVoipSandboxChannelResponse_id,
    aPNSVoipSandboxChannelResponse_isArchived,
    aPNSVoipSandboxChannelResponse_lastModifiedBy,
    aPNSVoipSandboxChannelResponse_lastModifiedDate,
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
    activity_custom,
    activity_conditionalSplit,
    activity_contactCenter,
    activity_description,
    activity_email,
    activity_holdout,
    activity_multiCondition,
    activity_push,
    activity_randomSplit,
    activity_sms,
    activity_wait,

    -- * ActivityResponse
    ActivityResponse (..),
    newActivityResponse,
    activityResponse_end,
    activityResponse_executionMetrics,
    activityResponse_result,
    activityResponse_scheduledStart,
    activityResponse_start,
    activityResponse_state,
    activityResponse_successfulEndpointCount,
    activityResponse_timezonesCompletedCount,
    activityResponse_timezonesTotalCount,
    activityResponse_totalEndpointCount,
    activityResponse_treatmentId,
    activityResponse_campaignId,
    activityResponse_id,
    activityResponse_applicationId,

    -- * AddressConfiguration
    AddressConfiguration (..),
    newAddressConfiguration,
    addressConfiguration_bodyOverride,
    addressConfiguration_channelType,
    addressConfiguration_context,
    addressConfiguration_rawContent,
    addressConfiguration_substitutions,
    addressConfiguration_titleOverride,

    -- * AndroidPushNotificationTemplate
    AndroidPushNotificationTemplate (..),
    newAndroidPushNotificationTemplate,
    androidPushNotificationTemplate_action,
    androidPushNotificationTemplate_body,
    androidPushNotificationTemplate_imageIconUrl,
    androidPushNotificationTemplate_imageUrl,
    androidPushNotificationTemplate_rawContent,
    androidPushNotificationTemplate_smallImageIconUrl,
    androidPushNotificationTemplate_sound,
    androidPushNotificationTemplate_title,
    androidPushNotificationTemplate_url,

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
    applicationResponse_creationDate,
    applicationResponse_tags,
    applicationResponse_id,
    applicationResponse_arn,
    applicationResponse_name,

    -- * ApplicationSettingsResource
    ApplicationSettingsResource (..),
    newApplicationSettingsResource,
    applicationSettingsResource_campaignHook,
    applicationSettingsResource_lastModifiedDate,
    applicationSettingsResource_limits,
    applicationSettingsResource_quietTime,
    applicationSettingsResource_applicationId,

    -- * ApplicationsResponse
    ApplicationsResponse (..),
    newApplicationsResponse,
    applicationsResponse_item,
    applicationsResponse_nextToken,

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
    baiduChannelResponse_applicationId,
    baiduChannelResponse_creationDate,
    baiduChannelResponse_enabled,
    baiduChannelResponse_hasCredential,
    baiduChannelResponse_id,
    baiduChannelResponse_isArchived,
    baiduChannelResponse_lastModifiedBy,
    baiduChannelResponse_lastModifiedDate,
    baiduChannelResponse_version,
    baiduChannelResponse_credential,
    baiduChannelResponse_platform,

    -- * BaiduMessage
    BaiduMessage (..),
    newBaiduMessage,
    baiduMessage_action,
    baiduMessage_body,
    baiduMessage_data,
    baiduMessage_iconReference,
    baiduMessage_imageIconUrl,
    baiduMessage_imageUrl,
    baiduMessage_rawContent,
    baiduMessage_silentPush,
    baiduMessage_smallImageIconUrl,
    baiduMessage_sound,
    baiduMessage_substitutions,
    baiduMessage_timeToLive,
    baiduMessage_title,
    baiduMessage_url,

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
    campaignInAppMessage_body,
    campaignInAppMessage_content,
    campaignInAppMessage_customConfig,
    campaignInAppMessage_layout,

    -- * CampaignLimits
    CampaignLimits (..),
    newCampaignLimits,
    campaignLimits_daily,
    campaignLimits_maximumDuration,
    campaignLimits_messagesPerSecond,
    campaignLimits_session,
    campaignLimits_total,

    -- * CampaignResponse
    CampaignResponse (..),
    newCampaignResponse,
    campaignResponse_additionalTreatments,
    campaignResponse_customDeliveryConfiguration,
    campaignResponse_defaultState,
    campaignResponse_description,
    campaignResponse_holdoutPercent,
    campaignResponse_hook,
    campaignResponse_isPaused,
    campaignResponse_limits,
    campaignResponse_messageConfiguration,
    campaignResponse_name,
    campaignResponse_priority,
    campaignResponse_schedule,
    campaignResponse_state,
    campaignResponse_templateConfiguration,
    campaignResponse_treatmentDescription,
    campaignResponse_treatmentName,
    campaignResponse_version,
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
    campaignSmsMessage_body,
    campaignSmsMessage_entityId,
    campaignSmsMessage_messageType,
    campaignSmsMessage_originationNumber,
    campaignSmsMessage_senderId,
    campaignSmsMessage_templateId,

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
    channelResponse_applicationId,
    channelResponse_creationDate,
    channelResponse_enabled,
    channelResponse_hasCredential,
    channelResponse_id,
    channelResponse_isArchived,
    channelResponse_lastModifiedBy,
    channelResponse_lastModifiedDate,
    channelResponse_version,

    -- * ChannelsResponse
    ChannelsResponse (..),
    newChannelsResponse,
    channelsResponse_channels,

    -- * ClosedDays
    ClosedDays (..),
    newClosedDays,
    closedDays_custom,
    closedDays_email,
    closedDays_push,
    closedDays_sms,
    closedDays_voice,

    -- * ClosedDaysRule
    ClosedDaysRule (..),
    newClosedDaysRule,
    closedDaysRule_endDateTime,
    closedDaysRule_name,
    closedDaysRule_startDateTime,

    -- * Condition
    Condition (..),
    newCondition,
    condition_conditions,
    condition_operator,

    -- * ConditionalSplitActivity
    ConditionalSplitActivity (..),
    newConditionalSplitActivity,
    conditionalSplitActivity_condition,
    conditionalSplitActivity_evaluationWaitTime,
    conditionalSplitActivity_falseActivity,
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
    createRecommenderConfiguration_attributes,
    createRecommenderConfiguration_description,
    createRecommenderConfiguration_name,
    createRecommenderConfiguration_recommendationProviderIdType,
    createRecommenderConfiguration_recommendationTransformerUri,
    createRecommenderConfiguration_recommendationsDisplayName,
    createRecommenderConfiguration_recommendationsPerMessage,
    createRecommenderConfiguration_recommendationProviderUri,
    createRecommenderConfiguration_recommendationProviderRoleArn,

    -- * CreateTemplateMessageBody
    CreateTemplateMessageBody (..),
    newCreateTemplateMessageBody,
    createTemplateMessageBody_arn,
    createTemplateMessageBody_message,
    createTemplateMessageBody_requestID,

    -- * CustomDeliveryConfiguration
    CustomDeliveryConfiguration (..),
    newCustomDeliveryConfiguration,
    customDeliveryConfiguration_endpointTypes,
    customDeliveryConfiguration_deliveryUri,

    -- * CustomMessageActivity
    CustomMessageActivity (..),
    newCustomMessageActivity,
    customMessageActivity_deliveryUri,
    customMessageActivity_endpointTypes,
    customMessageActivity_messageConfig,
    customMessageActivity_nextActivity,
    customMessageActivity_templateName,
    customMessageActivity_templateVersion,

    -- * DefaultButtonConfiguration
    DefaultButtonConfiguration (..),
    newDefaultButtonConfiguration,
    defaultButtonConfiguration_backgroundColor,
    defaultButtonConfiguration_borderRadius,
    defaultButtonConfiguration_link,
    defaultButtonConfiguration_textColor,
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
    defaultPushNotificationMessage_action,
    defaultPushNotificationMessage_body,
    defaultPushNotificationMessage_data,
    defaultPushNotificationMessage_silentPush,
    defaultPushNotificationMessage_substitutions,
    defaultPushNotificationMessage_title,
    defaultPushNotificationMessage_url,

    -- * DefaultPushNotificationTemplate
    DefaultPushNotificationTemplate (..),
    newDefaultPushNotificationTemplate,
    defaultPushNotificationTemplate_action,
    defaultPushNotificationTemplate_body,
    defaultPushNotificationTemplate_sound,
    defaultPushNotificationTemplate_title,
    defaultPushNotificationTemplate_url,

    -- * DirectMessageConfiguration
    DirectMessageConfiguration (..),
    newDirectMessageConfiguration,
    directMessageConfiguration_aDMMessage,
    directMessageConfiguration_aPNSMessage,
    directMessageConfiguration_baiduMessage,
    directMessageConfiguration_defaultMessage,
    directMessageConfiguration_defaultPushNotificationMessage,
    directMessageConfiguration_emailMessage,
    directMessageConfiguration_gCMMessage,
    directMessageConfiguration_sMSMessage,
    directMessageConfiguration_voiceMessage,

    -- * EmailChannelRequest
    EmailChannelRequest (..),
    newEmailChannelRequest,
    emailChannelRequest_configurationSet,
    emailChannelRequest_enabled,
    emailChannelRequest_roleArn,
    emailChannelRequest_fromAddress,
    emailChannelRequest_identity,

    -- * EmailChannelResponse
    EmailChannelResponse (..),
    newEmailChannelResponse,
    emailChannelResponse_applicationId,
    emailChannelResponse_configurationSet,
    emailChannelResponse_creationDate,
    emailChannelResponse_enabled,
    emailChannelResponse_fromAddress,
    emailChannelResponse_hasCredential,
    emailChannelResponse_id,
    emailChannelResponse_identity,
    emailChannelResponse_isArchived,
    emailChannelResponse_lastModifiedBy,
    emailChannelResponse_lastModifiedDate,
    emailChannelResponse_messagesPerSecond,
    emailChannelResponse_roleArn,
    emailChannelResponse_version,
    emailChannelResponse_platform,

    -- * EmailMessage
    EmailMessage (..),
    newEmailMessage,
    emailMessage_body,
    emailMessage_feedbackForwardingAddress,
    emailMessage_fromAddress,
    emailMessage_rawEmail,
    emailMessage_replyToAddresses,
    emailMessage_simpleEmail,
    emailMessage_substitutions,

    -- * EmailMessageActivity
    EmailMessageActivity (..),
    newEmailMessageActivity,
    emailMessageActivity_messageConfig,
    emailMessageActivity_nextActivity,
    emailMessageActivity_templateName,
    emailMessageActivity_templateVersion,

    -- * EmailTemplateRequest
    EmailTemplateRequest (..),
    newEmailTemplateRequest,
    emailTemplateRequest_defaultSubstitutions,
    emailTemplateRequest_htmlPart,
    emailTemplateRequest_recommenderId,
    emailTemplateRequest_subject,
    emailTemplateRequest_templateDescription,
    emailTemplateRequest_textPart,
    emailTemplateRequest_tags,

    -- * EmailTemplateResponse
    EmailTemplateResponse (..),
    newEmailTemplateResponse,
    emailTemplateResponse_arn,
    emailTemplateResponse_defaultSubstitutions,
    emailTemplateResponse_htmlPart,
    emailTemplateResponse_recommenderId,
    emailTemplateResponse_subject,
    emailTemplateResponse_templateDescription,
    emailTemplateResponse_textPart,
    emailTemplateResponse_version,
    emailTemplateResponse_tags,
    emailTemplateResponse_lastModifiedDate,
    emailTemplateResponse_creationDate,
    emailTemplateResponse_templateName,
    emailTemplateResponse_templateType,

    -- * EndpointBatchItem
    EndpointBatchItem (..),
    newEndpointBatchItem,
    endpointBatchItem_address,
    endpointBatchItem_attributes,
    endpointBatchItem_channelType,
    endpointBatchItem_demographic,
    endpointBatchItem_effectiveDate,
    endpointBatchItem_endpointStatus,
    endpointBatchItem_id,
    endpointBatchItem_location,
    endpointBatchItem_metrics,
    endpointBatchItem_optOut,
    endpointBatchItem_requestId,
    endpointBatchItem_user,

    -- * EndpointBatchRequest
    EndpointBatchRequest (..),
    newEndpointBatchRequest,
    endpointBatchRequest_item,

    -- * EndpointDemographic
    EndpointDemographic (..),
    newEndpointDemographic,
    endpointDemographic_appVersion,
    endpointDemographic_locale,
    endpointDemographic_make,
    endpointDemographic_model,
    endpointDemographic_modelVersion,
    endpointDemographic_platform,
    endpointDemographic_platformVersion,
    endpointDemographic_timezone,

    -- * EndpointItemResponse
    EndpointItemResponse (..),
    newEndpointItemResponse,
    endpointItemResponse_message,
    endpointItemResponse_statusCode,

    -- * EndpointLocation
    EndpointLocation (..),
    newEndpointLocation,
    endpointLocation_city,
    endpointLocation_country,
    endpointLocation_latitude,
    endpointLocation_longitude,
    endpointLocation_postalCode,
    endpointLocation_region,

    -- * EndpointMessageResult
    EndpointMessageResult (..),
    newEndpointMessageResult,
    endpointMessageResult_address,
    endpointMessageResult_messageId,
    endpointMessageResult_statusMessage,
    endpointMessageResult_updatedToken,
    endpointMessageResult_deliveryStatus,
    endpointMessageResult_statusCode,

    -- * EndpointRequest
    EndpointRequest (..),
    newEndpointRequest,
    endpointRequest_address,
    endpointRequest_attributes,
    endpointRequest_channelType,
    endpointRequest_demographic,
    endpointRequest_effectiveDate,
    endpointRequest_endpointStatus,
    endpointRequest_location,
    endpointRequest_metrics,
    endpointRequest_optOut,
    endpointRequest_requestId,
    endpointRequest_user,

    -- * EndpointResponse
    EndpointResponse (..),
    newEndpointResponse,
    endpointResponse_address,
    endpointResponse_applicationId,
    endpointResponse_attributes,
    endpointResponse_channelType,
    endpointResponse_cohortId,
    endpointResponse_creationDate,
    endpointResponse_demographic,
    endpointResponse_effectiveDate,
    endpointResponse_endpointStatus,
    endpointResponse_id,
    endpointResponse_location,
    endpointResponse_metrics,
    endpointResponse_optOut,
    endpointResponse_requestId,
    endpointResponse_user,

    -- * EndpointSendConfiguration
    EndpointSendConfiguration (..),
    newEndpointSendConfiguration,
    endpointSendConfiguration_bodyOverride,
    endpointSendConfiguration_context,
    endpointSendConfiguration_rawContent,
    endpointSendConfiguration_substitutions,
    endpointSendConfiguration_titleOverride,

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
    event_appPackageName,
    event_appTitle,
    event_appVersionCode,
    event_attributes,
    event_clientSdkVersion,
    event_metrics,
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
    eventDimensions_attributes,
    eventDimensions_eventType,
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
    eventStream_externalId,
    eventStream_lastModifiedDate,
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
    exportJobResponse_completionDate,
    exportJobResponse_failedPieces,
    exportJobResponse_failures,
    exportJobResponse_totalFailures,
    exportJobResponse_totalPieces,
    exportJobResponse_totalProcessed,
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
    gCMChannelResponse_applicationId,
    gCMChannelResponse_creationDate,
    gCMChannelResponse_enabled,
    gCMChannelResponse_hasCredential,
    gCMChannelResponse_id,
    gCMChannelResponse_isArchived,
    gCMChannelResponse_lastModifiedBy,
    gCMChannelResponse_lastModifiedDate,
    gCMChannelResponse_version,
    gCMChannelResponse_credential,
    gCMChannelResponse_platform,

    -- * GCMMessage
    GCMMessage (..),
    newGCMMessage,
    gCMMessage_action,
    gCMMessage_body,
    gCMMessage_collapseKey,
    gCMMessage_data,
    gCMMessage_iconReference,
    gCMMessage_imageIconUrl,
    gCMMessage_imageUrl,
    gCMMessage_priority,
    gCMMessage_rawContent,
    gCMMessage_restrictedPackageName,
    gCMMessage_silentPush,
    gCMMessage_smallImageIconUrl,
    gCMMessage_sound,
    gCMMessage_substitutions,
    gCMMessage_timeToLive,
    gCMMessage_title,
    gCMMessage_url,

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
    importJobRequest_externalId,
    importJobRequest_registerEndpoints,
    importJobRequest_segmentId,
    importJobRequest_segmentName,
    importJobRequest_format,
    importJobRequest_s3Url,
    importJobRequest_roleArn,

    -- * ImportJobResource
    ImportJobResource (..),
    newImportJobResource,
    importJobResource_defineSegment,
    importJobResource_externalId,
    importJobResource_registerEndpoints,
    importJobResource_segmentId,
    importJobResource_segmentName,
    importJobResource_format,
    importJobResource_s3Url,
    importJobResource_roleArn,

    -- * ImportJobResponse
    ImportJobResponse (..),
    newImportJobResponse,
    importJobResponse_completedPieces,
    importJobResponse_completionDate,
    importJobResponse_failedPieces,
    importJobResponse_failures,
    importJobResponse_totalFailures,
    importJobResponse_totalPieces,
    importJobResponse_totalProcessed,
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
    inAppCampaignSchedule_eventFilter,
    inAppCampaignSchedule_quietTime,

    -- * InAppMessage
    InAppMessage (..),
    newInAppMessage,
    inAppMessage_content,
    inAppMessage_customConfig,
    inAppMessage_layout,

    -- * InAppMessageBodyConfig
    InAppMessageBodyConfig (..),
    newInAppMessageBodyConfig,
    inAppMessageBodyConfig_alignment,
    inAppMessageBodyConfig_textColor,
    inAppMessageBodyConfig_body,

    -- * InAppMessageButton
    InAppMessageButton (..),
    newInAppMessageButton,
    inAppMessageButton_android,
    inAppMessageButton_defaultConfig,
    inAppMessageButton_ios,
    inAppMessageButton_web,

    -- * InAppMessageCampaign
    InAppMessageCampaign (..),
    newInAppMessageCampaign,
    inAppMessageCampaign_campaignId,
    inAppMessageCampaign_dailyCap,
    inAppMessageCampaign_inAppMessage,
    inAppMessageCampaign_priority,
    inAppMessageCampaign_schedule,
    inAppMessageCampaign_sessionCap,
    inAppMessageCampaign_totalCap,
    inAppMessageCampaign_treatmentId,

    -- * InAppMessageContent
    InAppMessageContent (..),
    newInAppMessageContent,
    inAppMessageContent_backgroundColor,
    inAppMessageContent_bodyConfig,
    inAppMessageContent_headerConfig,
    inAppMessageContent_imageUrl,
    inAppMessageContent_primaryBtn,
    inAppMessageContent_secondaryBtn,

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
    inAppTemplateRequest_content,
    inAppTemplateRequest_customConfig,
    inAppTemplateRequest_layout,
    inAppTemplateRequest_templateDescription,
    inAppTemplateRequest_tags,

    -- * InAppTemplateResponse
    InAppTemplateResponse (..),
    newInAppTemplateResponse,
    inAppTemplateResponse_arn,
    inAppTemplateResponse_content,
    inAppTemplateResponse_customConfig,
    inAppTemplateResponse_layout,
    inAppTemplateResponse_templateDescription,
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
    journeyLimits_dailyCap,
    journeyLimits_endpointReentryCap,
    journeyLimits_endpointReentryInterval,
    journeyLimits_messagesPerSecond,

    -- * JourneyPushMessage
    JourneyPushMessage (..),
    newJourneyPushMessage,
    journeyPushMessage_timeToLive,

    -- * JourneyResponse
    JourneyResponse (..),
    newJourneyResponse,
    journeyResponse_activities,
    journeyResponse_closedDays,
    journeyResponse_creationDate,
    journeyResponse_journeyChannelSettings,
    journeyResponse_lastModifiedDate,
    journeyResponse_limits,
    journeyResponse_localTime,
    journeyResponse_openHours,
    journeyResponse_quietTime,
    journeyResponse_refreshFrequency,
    journeyResponse_refreshOnSegmentUpdate,
    journeyResponse_schedule,
    journeyResponse_sendingSchedule,
    journeyResponse_startActivity,
    journeyResponse_startCondition,
    journeyResponse_state,
    journeyResponse_timezoneEstimationMethods,
    journeyResponse_waitForQuietTime,
    journeyResponse_tags,
    journeyResponse_name,
    journeyResponse_id,
    journeyResponse_applicationId,

    -- * JourneyRunExecutionActivityMetricsResponse
    JourneyRunExecutionActivityMetricsResponse (..),
    newJourneyRunExecutionActivityMetricsResponse,
    journeyRunExecutionActivityMetricsResponse_metrics,
    journeyRunExecutionActivityMetricsResponse_journeyId,
    journeyRunExecutionActivityMetricsResponse_lastEvaluatedTime,
    journeyRunExecutionActivityMetricsResponse_journeyActivityId,
    journeyRunExecutionActivityMetricsResponse_activityType,
    journeyRunExecutionActivityMetricsResponse_runId,
    journeyRunExecutionActivityMetricsResponse_applicationId,

    -- * JourneyRunExecutionMetricsResponse
    JourneyRunExecutionMetricsResponse (..),
    newJourneyRunExecutionMetricsResponse,
    journeyRunExecutionMetricsResponse_metrics,
    journeyRunExecutionMetricsResponse_journeyId,
    journeyRunExecutionMetricsResponse_lastEvaluatedTime,
    journeyRunExecutionMetricsResponse_runId,
    journeyRunExecutionMetricsResponse_applicationId,

    -- * JourneyRunResponse
    JourneyRunResponse (..),
    newJourneyRunResponse,
    journeyRunResponse_status,
    journeyRunResponse_lastUpdateTime,
    journeyRunResponse_creationTime,
    journeyRunResponse_runId,

    -- * JourneyRunsResponse
    JourneyRunsResponse (..),
    newJourneyRunsResponse,
    journeyRunsResponse_nextToken,
    journeyRunsResponse_item,

    -- * JourneySMSMessage
    JourneySMSMessage (..),
    newJourneySMSMessage,
    journeySMSMessage_entityId,
    journeySMSMessage_messageType,
    journeySMSMessage_originationNumber,
    journeySMSMessage_senderId,
    journeySMSMessage_templateId,

    -- * JourneySchedule
    JourneySchedule (..),
    newJourneySchedule,
    journeySchedule_endTime,
    journeySchedule_startTime,
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
    message_action,
    message_body,
    message_imageIconUrl,
    message_imageSmallIconUrl,
    message_imageUrl,
    message_jsonBody,
    message_mediaUrl,
    message_rawContent,
    message_silentPush,
    message_timeToLive,
    message_title,
    message_url,

    -- * MessageBody
    MessageBody (..),
    newMessageBody,
    messageBody_message,
    messageBody_requestID,

    -- * MessageConfiguration
    MessageConfiguration (..),
    newMessageConfiguration,
    messageConfiguration_aDMMessage,
    messageConfiguration_aPNSMessage,
    messageConfiguration_baiduMessage,
    messageConfiguration_customMessage,
    messageConfiguration_defaultMessage,
    messageConfiguration_emailMessage,
    messageConfiguration_gCMMessage,
    messageConfiguration_inAppMessage,
    messageConfiguration_sMSMessage,

    -- * MessageRequest
    MessageRequest (..),
    newMessageRequest,
    messageRequest_addresses,
    messageRequest_context,
    messageRequest_endpoints,
    messageRequest_templateConfiguration,
    messageRequest_traceId,
    messageRequest_messageConfiguration,

    -- * MessageResponse
    MessageResponse (..),
    newMessageResponse,
    messageResponse_endpointResult,
    messageResponse_requestId,
    messageResponse_result,
    messageResponse_applicationId,

    -- * MessageResult
    MessageResult (..),
    newMessageResult,
    messageResult_messageId,
    messageResult_statusMessage,
    messageResult_updatedToken,
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
    numberValidateResponse_carrier,
    numberValidateResponse_city,
    numberValidateResponse_cleansedPhoneNumberE164,
    numberValidateResponse_cleansedPhoneNumberNational,
    numberValidateResponse_country,
    numberValidateResponse_countryCodeIso2,
    numberValidateResponse_countryCodeNumeric,
    numberValidateResponse_county,
    numberValidateResponse_originalCountryCodeIso2,
    numberValidateResponse_originalPhoneNumber,
    numberValidateResponse_phoneType,
    numberValidateResponse_phoneTypeCode,
    numberValidateResponse_timezone,
    numberValidateResponse_zipCode,

    -- * OpenHours
    OpenHours (..),
    newOpenHours,
    openHours_custom,
    openHours_email,
    openHours_push,
    openHours_sms,
    openHours_voice,

    -- * OpenHoursRule
    OpenHoursRule (..),
    newOpenHoursRule,
    openHoursRule_endTime,
    openHoursRule_startTime,

    -- * OverrideButtonConfiguration
    OverrideButtonConfiguration (..),
    newOverrideButtonConfiguration,
    overrideButtonConfiguration_link,
    overrideButtonConfiguration_buttonAction,

    -- * PublicEndpoint
    PublicEndpoint (..),
    newPublicEndpoint,
    publicEndpoint_address,
    publicEndpoint_attributes,
    publicEndpoint_channelType,
    publicEndpoint_demographic,
    publicEndpoint_effectiveDate,
    publicEndpoint_endpointStatus,
    publicEndpoint_location,
    publicEndpoint_metrics,
    publicEndpoint_optOut,
    publicEndpoint_requestId,
    publicEndpoint_user,

    -- * PushMessageActivity
    PushMessageActivity (..),
    newPushMessageActivity,
    pushMessageActivity_messageConfig,
    pushMessageActivity_nextActivity,
    pushMessageActivity_templateName,
    pushMessageActivity_templateVersion,

    -- * PushNotificationTemplateRequest
    PushNotificationTemplateRequest (..),
    newPushNotificationTemplateRequest,
    pushNotificationTemplateRequest_adm,
    pushNotificationTemplateRequest_apns,
    pushNotificationTemplateRequest_baidu,
    pushNotificationTemplateRequest_default,
    pushNotificationTemplateRequest_defaultSubstitutions,
    pushNotificationTemplateRequest_gcm,
    pushNotificationTemplateRequest_recommenderId,
    pushNotificationTemplateRequest_templateDescription,
    pushNotificationTemplateRequest_tags,

    -- * PushNotificationTemplateResponse
    PushNotificationTemplateResponse (..),
    newPushNotificationTemplateResponse,
    pushNotificationTemplateResponse_adm,
    pushNotificationTemplateResponse_apns,
    pushNotificationTemplateResponse_arn,
    pushNotificationTemplateResponse_baidu,
    pushNotificationTemplateResponse_default,
    pushNotificationTemplateResponse_defaultSubstitutions,
    pushNotificationTemplateResponse_gcm,
    pushNotificationTemplateResponse_recommenderId,
    pushNotificationTemplateResponse_templateDescription,
    pushNotificationTemplateResponse_version,
    pushNotificationTemplateResponse_tags,
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
    recommenderConfigurationResponse_attributes,
    recommenderConfigurationResponse_description,
    recommenderConfigurationResponse_name,
    recommenderConfigurationResponse_recommendationProviderIdType,
    recommenderConfigurationResponse_recommendationTransformerUri,
    recommenderConfigurationResponse_recommendationsDisplayName,
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
    sMSChannelRequest_enabled,
    sMSChannelRequest_senderId,
    sMSChannelRequest_shortCode,

    -- * SMSChannelResponse
    SMSChannelResponse (..),
    newSMSChannelResponse,
    sMSChannelResponse_applicationId,
    sMSChannelResponse_creationDate,
    sMSChannelResponse_enabled,
    sMSChannelResponse_hasCredential,
    sMSChannelResponse_id,
    sMSChannelResponse_isArchived,
    sMSChannelResponse_lastModifiedBy,
    sMSChannelResponse_lastModifiedDate,
    sMSChannelResponse_promotionalMessagesPerSecond,
    sMSChannelResponse_senderId,
    sMSChannelResponse_shortCode,
    sMSChannelResponse_transactionalMessagesPerSecond,
    sMSChannelResponse_version,
    sMSChannelResponse_platform,

    -- * SMSMessage
    SMSMessage (..),
    newSMSMessage,
    sMSMessage_body,
    sMSMessage_entityId,
    sMSMessage_keyword,
    sMSMessage_mediaUrl,
    sMSMessage_messageType,
    sMSMessage_originationNumber,
    sMSMessage_senderId,
    sMSMessage_substitutions,
    sMSMessage_templateId,

    -- * SMSMessageActivity
    SMSMessageActivity (..),
    newSMSMessageActivity,
    sMSMessageActivity_messageConfig,
    sMSMessageActivity_nextActivity,
    sMSMessageActivity_templateName,
    sMSMessageActivity_templateVersion,

    -- * SMSTemplateRequest
    SMSTemplateRequest (..),
    newSMSTemplateRequest,
    sMSTemplateRequest_body,
    sMSTemplateRequest_defaultSubstitutions,
    sMSTemplateRequest_recommenderId,
    sMSTemplateRequest_templateDescription,
    sMSTemplateRequest_tags,

    -- * SMSTemplateResponse
    SMSTemplateResponse (..),
    newSMSTemplateResponse,
    sMSTemplateResponse_arn,
    sMSTemplateResponse_body,
    sMSTemplateResponse_defaultSubstitutions,
    sMSTemplateResponse_recommenderId,
    sMSTemplateResponse_templateDescription,
    sMSTemplateResponse_version,
    sMSTemplateResponse_tags,
    sMSTemplateResponse_lastModifiedDate,
    sMSTemplateResponse_creationDate,
    sMSTemplateResponse_templateName,
    sMSTemplateResponse_templateType,

    -- * Schedule
    Schedule (..),
    newSchedule,
    schedule_endTime,
    schedule_eventFilter,
    schedule_frequency,
    schedule_isLocalTime,
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
    segmentDemographics_appVersion,
    segmentDemographics_channel,
    segmentDemographics_deviceType,
    segmentDemographics_make,
    segmentDemographics_model,
    segmentDemographics_platform,

    -- * SegmentDimensions
    SegmentDimensions (..),
    newSegmentDimensions,
    segmentDimensions_attributes,
    segmentDimensions_behavior,
    segmentDimensions_demographic,
    segmentDimensions_location,
    segmentDimensions_metrics,
    segmentDimensions_userAttributes,

    -- * SegmentGroup
    SegmentGroup (..),
    newSegmentGroup,
    segmentGroup_dimensions,
    segmentGroup_sourceSegments,
    segmentGroup_sourceType,
    segmentGroup_type,

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
    segmentResponse_dimensions,
    segmentResponse_importDefinition,
    segmentResponse_lastModifiedDate,
    segmentResponse_name,
    segmentResponse_segmentGroups,
    segmentResponse_version,
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

    -- * SendOTPMessageRequestParameters
    SendOTPMessageRequestParameters (..),
    newSendOTPMessageRequestParameters,
    sendOTPMessageRequestParameters_allowedAttempts,
    sendOTPMessageRequestParameters_codeLength,
    sendOTPMessageRequestParameters_entityId,
    sendOTPMessageRequestParameters_language,
    sendOTPMessageRequestParameters_templateId,
    sendOTPMessageRequestParameters_validityPeriod,
    sendOTPMessageRequestParameters_brandName,
    sendOTPMessageRequestParameters_referenceId,
    sendOTPMessageRequestParameters_channel,
    sendOTPMessageRequestParameters_destinationIdentity,
    sendOTPMessageRequestParameters_originationIdentity,

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
    simpleCondition_eventCondition,
    simpleCondition_segmentCondition,
    simpleCondition_segmentDimensions,

    -- * SimpleEmail
    SimpleEmail (..),
    newSimpleEmail,
    simpleEmail_htmlPart,
    simpleEmail_subject,
    simpleEmail_textPart,

    -- * SimpleEmailPart
    SimpleEmailPart (..),
    newSimpleEmailPart,
    simpleEmailPart_charset,
    simpleEmailPart_data,

    -- * StartCondition
    StartCondition (..),
    newStartCondition,
    startCondition_description,
    startCondition_eventStartCondition,
    startCondition_segmentStartCondition,

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
    templateConfiguration_pushTemplate,
    templateConfiguration_sMSTemplate,
    templateConfiguration_voiceTemplate,

    -- * TemplateCreateMessageBody
    TemplateCreateMessageBody (..),
    newTemplateCreateMessageBody,
    templateCreateMessageBody_arn,
    templateCreateMessageBody_message,
    templateCreateMessageBody_requestID,

    -- * TemplateResponse
    TemplateResponse (..),
    newTemplateResponse,
    templateResponse_arn,
    templateResponse_defaultSubstitutions,
    templateResponse_templateDescription,
    templateResponse_version,
    templateResponse_tags,
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
    treatmentResource_customDeliveryConfiguration,
    treatmentResource_messageConfiguration,
    treatmentResource_schedule,
    treatmentResource_state,
    treatmentResource_templateConfiguration,
    treatmentResource_treatmentDescription,
    treatmentResource_treatmentName,
    treatmentResource_id,
    treatmentResource_sizePercent,

    -- * UpdateAttributesRequest
    UpdateAttributesRequest (..),
    newUpdateAttributesRequest,
    updateAttributesRequest_blacklist,

    -- * UpdateRecommenderConfiguration
    UpdateRecommenderConfiguration (..),
    newUpdateRecommenderConfiguration,
    updateRecommenderConfiguration_attributes,
    updateRecommenderConfiguration_description,
    updateRecommenderConfiguration_name,
    updateRecommenderConfiguration_recommendationProviderIdType,
    updateRecommenderConfiguration_recommendationTransformerUri,
    updateRecommenderConfiguration_recommendationsDisplayName,
    updateRecommenderConfiguration_recommendationsPerMessage,
    updateRecommenderConfiguration_recommendationProviderUri,
    updateRecommenderConfiguration_recommendationProviderRoleArn,

    -- * VerificationResponse
    VerificationResponse (..),
    newVerificationResponse,
    verificationResponse_valid,

    -- * VerifyOTPMessageRequestParameters
    VerifyOTPMessageRequestParameters (..),
    newVerifyOTPMessageRequestParameters,
    verifyOTPMessageRequestParameters_referenceId,
    verifyOTPMessageRequestParameters_otp,
    verifyOTPMessageRequestParameters_destinationIdentity,

    -- * VoiceChannelRequest
    VoiceChannelRequest (..),
    newVoiceChannelRequest,
    voiceChannelRequest_enabled,

    -- * VoiceChannelResponse
    VoiceChannelResponse (..),
    newVoiceChannelResponse,
    voiceChannelResponse_applicationId,
    voiceChannelResponse_creationDate,
    voiceChannelResponse_enabled,
    voiceChannelResponse_hasCredential,
    voiceChannelResponse_id,
    voiceChannelResponse_isArchived,
    voiceChannelResponse_lastModifiedBy,
    voiceChannelResponse_lastModifiedDate,
    voiceChannelResponse_version,
    voiceChannelResponse_platform,

    -- * VoiceMessage
    VoiceMessage (..),
    newVoiceMessage,
    voiceMessage_body,
    voiceMessage_languageCode,
    voiceMessage_originationNumber,
    voiceMessage_substitutions,
    voiceMessage_voiceId,

    -- * VoiceTemplateRequest
    VoiceTemplateRequest (..),
    newVoiceTemplateRequest,
    voiceTemplateRequest_body,
    voiceTemplateRequest_defaultSubstitutions,
    voiceTemplateRequest_languageCode,
    voiceTemplateRequest_templateDescription,
    voiceTemplateRequest_voiceId,
    voiceTemplateRequest_tags,

    -- * VoiceTemplateResponse
    VoiceTemplateResponse (..),
    newVoiceTemplateResponse,
    voiceTemplateResponse_arn,
    voiceTemplateResponse_body,
    voiceTemplateResponse_defaultSubstitutions,
    voiceTemplateResponse_languageCode,
    voiceTemplateResponse_templateDescription,
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
    writeApplicationSettingsRequest_campaignHook,
    writeApplicationSettingsRequest_cloudWatchMetricsEnabled,
    writeApplicationSettingsRequest_eventTaggingEnabled,
    writeApplicationSettingsRequest_limits,
    writeApplicationSettingsRequest_quietTime,

    -- * WriteCampaignRequest
    WriteCampaignRequest (..),
    newWriteCampaignRequest,
    writeCampaignRequest_additionalTreatments,
    writeCampaignRequest_customDeliveryConfiguration,
    writeCampaignRequest_description,
    writeCampaignRequest_holdoutPercent,
    writeCampaignRequest_hook,
    writeCampaignRequest_isPaused,
    writeCampaignRequest_limits,
    writeCampaignRequest_messageConfiguration,
    writeCampaignRequest_name,
    writeCampaignRequest_priority,
    writeCampaignRequest_schedule,
    writeCampaignRequest_segmentId,
    writeCampaignRequest_segmentVersion,
    writeCampaignRequest_templateConfiguration,
    writeCampaignRequest_treatmentDescription,
    writeCampaignRequest_treatmentName,
    writeCampaignRequest_tags,

    -- * WriteEventStream
    WriteEventStream (..),
    newWriteEventStream,
    writeEventStream_roleArn,
    writeEventStream_destinationStreamArn,

    -- * WriteJourneyRequest
    WriteJourneyRequest (..),
    newWriteJourneyRequest,
    writeJourneyRequest_activities,
    writeJourneyRequest_closedDays,
    writeJourneyRequest_creationDate,
    writeJourneyRequest_journeyChannelSettings,
    writeJourneyRequest_lastModifiedDate,
    writeJourneyRequest_limits,
    writeJourneyRequest_localTime,
    writeJourneyRequest_openHours,
    writeJourneyRequest_quietTime,
    writeJourneyRequest_refreshFrequency,
    writeJourneyRequest_refreshOnSegmentUpdate,
    writeJourneyRequest_schedule,
    writeJourneyRequest_sendingSchedule,
    writeJourneyRequest_startActivity,
    writeJourneyRequest_startCondition,
    writeJourneyRequest_state,
    writeJourneyRequest_timezoneEstimationMethods,
    writeJourneyRequest_waitForQuietTime,
    writeJourneyRequest_name,

    -- * WriteSegmentRequest
    WriteSegmentRequest (..),
    newWriteSegmentRequest,
    writeSegmentRequest_dimensions,
    writeSegmentRequest_name,
    writeSegmentRequest_segmentGroups,
    writeSegmentRequest_tags,

    -- * WriteTreatmentResource
    WriteTreatmentResource (..),
    newWriteTreatmentResource,
    writeTreatmentResource_customDeliveryConfiguration,
    writeTreatmentResource_messageConfiguration,
    writeTreatmentResource_schedule,
    writeTreatmentResource_templateConfiguration,
    writeTreatmentResource_treatmentDescription,
    writeTreatmentResource_treatmentName,
    writeTreatmentResource_sizePercent,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
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
import Amazonka.Pinpoint.Types.ClosedDays
import Amazonka.Pinpoint.Types.ClosedDaysRule
import Amazonka.Pinpoint.Types.Condition
import Amazonka.Pinpoint.Types.ConditionalSplitActivity
import Amazonka.Pinpoint.Types.ContactCenterActivity
import Amazonka.Pinpoint.Types.CreateApplicationRequest
import Amazonka.Pinpoint.Types.CreateRecommenderConfiguration
import Amazonka.Pinpoint.Types.CreateTemplateMessageBody
import Amazonka.Pinpoint.Types.CustomDeliveryConfiguration
import Amazonka.Pinpoint.Types.CustomMessageActivity
import Amazonka.Pinpoint.Types.DayOfWeek
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
import Amazonka.Pinpoint.Types.JourneyRunExecutionActivityMetricsResponse
import Amazonka.Pinpoint.Types.JourneyRunExecutionMetricsResponse
import Amazonka.Pinpoint.Types.JourneyRunResponse
import Amazonka.Pinpoint.Types.JourneyRunStatus
import Amazonka.Pinpoint.Types.JourneyRunsResponse
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
import Amazonka.Pinpoint.Types.OpenHours
import Amazonka.Pinpoint.Types.OpenHoursRule
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
import Amazonka.Pinpoint.Types.SendOTPMessageRequestParameters
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
import Amazonka.Pinpoint.Types.TimezoneEstimationMethodsElement
import Amazonka.Pinpoint.Types.TreatmentResource
import Amazonka.Pinpoint.Types.Type
import Amazonka.Pinpoint.Types.UpdateAttributesRequest
import Amazonka.Pinpoint.Types.UpdateRecommenderConfiguration
import Amazonka.Pinpoint.Types.VerificationResponse
import Amazonka.Pinpoint.Types.VerifyOTPMessageRequestParameters
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
    { Core.abbrev = "Pinpoint",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "pinpoint",
      Core.signingName = "mobiletargeting",
      Core.version = "2016-12-01",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "Pinpoint",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 502) e =
          Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
          Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
          Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
          Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
          Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
          Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | Provides information about an API request or response.
_BadRequestException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError
    defaultService
    "BadRequestException"
    Prelude.. Core.hasStatus 400

-- | Provides information about an API request or response.
_ConflictException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | Provides information about an API request or response.
_ForbiddenException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ForbiddenException =
  Core._MatchServiceError
    defaultService
    "ForbiddenException"
    Prelude.. Core.hasStatus 403

-- | Provides information about an API request or response.
_InternalServerErrorException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServerErrorException =
  Core._MatchServiceError
    defaultService
    "InternalServerErrorException"
    Prelude.. Core.hasStatus 500

-- | Provides information about an API request or response.
_MethodNotAllowedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_MethodNotAllowedException =
  Core._MatchServiceError
    defaultService
    "MethodNotAllowedException"
    Prelude.. Core.hasStatus 405

-- | Provides information about an API request or response.
_NotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError
    defaultService
    "NotFoundException"
    Prelude.. Core.hasStatus 404

-- | Provides information about an API request or response.
_PayloadTooLargeException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_PayloadTooLargeException =
  Core._MatchServiceError
    defaultService
    "PayloadTooLargeException"
    Prelude.. Core.hasStatus 413

-- | Provides information about an API request or response.
_TooManyRequestsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyRequestsException =
  Core._MatchServiceError
    defaultService
    "TooManyRequestsException"
    Prelude.. Core.hasStatus 429
