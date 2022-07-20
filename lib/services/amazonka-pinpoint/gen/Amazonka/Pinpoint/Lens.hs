{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Pinpoint.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Lens
  ( -- * Operations

    -- ** CreateApp
    createApp_createApplicationRequest,
    createAppResponse_httpStatus,
    createAppResponse_applicationResponse,

    -- ** CreateCampaign
    createCampaign_applicationId,
    createCampaign_writeCampaignRequest,
    createCampaignResponse_httpStatus,
    createCampaignResponse_campaignResponse,

    -- ** CreateEmailTemplate
    createEmailTemplate_templateName,
    createEmailTemplate_emailTemplateRequest,
    createEmailTemplateResponse_httpStatus,
    createEmailTemplateResponse_createTemplateMessageBody,

    -- ** CreateExportJob
    createExportJob_applicationId,
    createExportJob_exportJobRequest,
    createExportJobResponse_httpStatus,
    createExportJobResponse_exportJobResponse,

    -- ** CreateImportJob
    createImportJob_applicationId,
    createImportJob_importJobRequest,
    createImportJobResponse_httpStatus,
    createImportJobResponse_importJobResponse,

    -- ** CreateInAppTemplate
    createInAppTemplate_templateName,
    createInAppTemplate_inAppTemplateRequest,
    createInAppTemplateResponse_httpStatus,
    createInAppTemplateResponse_templateCreateMessageBody,

    -- ** CreateJourney
    createJourney_applicationId,
    createJourney_writeJourneyRequest,
    createJourneyResponse_httpStatus,
    createJourneyResponse_journeyResponse,

    -- ** CreatePushTemplate
    createPushTemplate_templateName,
    createPushTemplate_pushNotificationTemplateRequest,
    createPushTemplateResponse_httpStatus,
    createPushTemplateResponse_createTemplateMessageBody,

    -- ** CreateRecommenderConfiguration
    createRecommenderConfiguration'_createRecommenderConfiguration,
    createRecommenderConfigurationResponse_httpStatus,
    createRecommenderConfigurationResponse_recommenderConfigurationResponse,

    -- ** CreateSegment
    createSegment_applicationId,
    createSegment_writeSegmentRequest,
    createSegmentResponse_httpStatus,
    createSegmentResponse_segmentResponse,

    -- ** CreateSmsTemplate
    createSmsTemplate_templateName,
    createSmsTemplate_sMSTemplateRequest,
    createSmsTemplateResponse_httpStatus,
    createSmsTemplateResponse_createTemplateMessageBody,

    -- ** CreateVoiceTemplate
    createVoiceTemplate_templateName,
    createVoiceTemplate_voiceTemplateRequest,
    createVoiceTemplateResponse_httpStatus,
    createVoiceTemplateResponse_createTemplateMessageBody,

    -- ** DeleteAdmChannel
    deleteAdmChannel_applicationId,
    deleteAdmChannelResponse_httpStatus,
    deleteAdmChannelResponse_aDMChannelResponse,

    -- ** DeleteApnsChannel
    deleteApnsChannel_applicationId,
    deleteApnsChannelResponse_httpStatus,
    deleteApnsChannelResponse_aPNSChannelResponse,

    -- ** DeleteApnsSandboxChannel
    deleteApnsSandboxChannel_applicationId,
    deleteApnsSandboxChannelResponse_httpStatus,
    deleteApnsSandboxChannelResponse_aPNSSandboxChannelResponse,

    -- ** DeleteApnsVoipChannel
    deleteApnsVoipChannel_applicationId,
    deleteApnsVoipChannelResponse_httpStatus,
    deleteApnsVoipChannelResponse_aPNSVoipChannelResponse,

    -- ** DeleteApnsVoipSandboxChannel
    deleteApnsVoipSandboxChannel_applicationId,
    deleteApnsVoipSandboxChannelResponse_httpStatus,
    deleteApnsVoipSandboxChannelResponse_aPNSVoipSandboxChannelResponse,

    -- ** DeleteApp
    deleteApp_applicationId,
    deleteAppResponse_httpStatus,
    deleteAppResponse_applicationResponse,

    -- ** DeleteBaiduChannel
    deleteBaiduChannel_applicationId,
    deleteBaiduChannelResponse_httpStatus,
    deleteBaiduChannelResponse_baiduChannelResponse,

    -- ** DeleteCampaign
    deleteCampaign_campaignId,
    deleteCampaign_applicationId,
    deleteCampaignResponse_httpStatus,
    deleteCampaignResponse_campaignResponse,

    -- ** DeleteEmailChannel
    deleteEmailChannel_applicationId,
    deleteEmailChannelResponse_httpStatus,
    deleteEmailChannelResponse_emailChannelResponse,

    -- ** DeleteEmailTemplate
    deleteEmailTemplate_version,
    deleteEmailTemplate_templateName,
    deleteEmailTemplateResponse_httpStatus,
    deleteEmailTemplateResponse_messageBody,

    -- ** DeleteEndpoint
    deleteEndpoint_applicationId,
    deleteEndpoint_endpointId,
    deleteEndpointResponse_httpStatus,
    deleteEndpointResponse_endpointResponse,

    -- ** DeleteEventStream
    deleteEventStream_applicationId,
    deleteEventStreamResponse_httpStatus,
    deleteEventStreamResponse_eventStream,

    -- ** DeleteGcmChannel
    deleteGcmChannel_applicationId,
    deleteGcmChannelResponse_httpStatus,
    deleteGcmChannelResponse_gCMChannelResponse,

    -- ** DeleteInAppTemplate
    deleteInAppTemplate_version,
    deleteInAppTemplate_templateName,
    deleteInAppTemplateResponse_httpStatus,
    deleteInAppTemplateResponse_messageBody,

    -- ** DeleteJourney
    deleteJourney_journeyId,
    deleteJourney_applicationId,
    deleteJourneyResponse_httpStatus,
    deleteJourneyResponse_journeyResponse,

    -- ** DeletePushTemplate
    deletePushTemplate_version,
    deletePushTemplate_templateName,
    deletePushTemplateResponse_httpStatus,
    deletePushTemplateResponse_messageBody,

    -- ** DeleteRecommenderConfiguration
    deleteRecommenderConfiguration_recommenderId,
    deleteRecommenderConfigurationResponse_httpStatus,
    deleteRecommenderConfigurationResponse_recommenderConfigurationResponse,

    -- ** DeleteSegment
    deleteSegment_segmentId,
    deleteSegment_applicationId,
    deleteSegmentResponse_httpStatus,
    deleteSegmentResponse_segmentResponse,

    -- ** DeleteSmsChannel
    deleteSmsChannel_applicationId,
    deleteSmsChannelResponse_httpStatus,
    deleteSmsChannelResponse_sMSChannelResponse,

    -- ** DeleteSmsTemplate
    deleteSmsTemplate_version,
    deleteSmsTemplate_templateName,
    deleteSmsTemplateResponse_httpStatus,
    deleteSmsTemplateResponse_messageBody,

    -- ** DeleteUserEndpoints
    deleteUserEndpoints_applicationId,
    deleteUserEndpoints_userId,
    deleteUserEndpointsResponse_httpStatus,
    deleteUserEndpointsResponse_endpointsResponse,

    -- ** DeleteVoiceChannel
    deleteVoiceChannel_applicationId,
    deleteVoiceChannelResponse_httpStatus,
    deleteVoiceChannelResponse_voiceChannelResponse,

    -- ** DeleteVoiceTemplate
    deleteVoiceTemplate_version,
    deleteVoiceTemplate_templateName,
    deleteVoiceTemplateResponse_httpStatus,
    deleteVoiceTemplateResponse_messageBody,

    -- ** GetAdmChannel
    getAdmChannel_applicationId,
    getAdmChannelResponse_httpStatus,
    getAdmChannelResponse_aDMChannelResponse,

    -- ** GetApnsChannel
    getApnsChannel_applicationId,
    getApnsChannelResponse_httpStatus,
    getApnsChannelResponse_aPNSChannelResponse,

    -- ** GetApnsSandboxChannel
    getApnsSandboxChannel_applicationId,
    getApnsSandboxChannelResponse_httpStatus,
    getApnsSandboxChannelResponse_aPNSSandboxChannelResponse,

    -- ** GetApnsVoipChannel
    getApnsVoipChannel_applicationId,
    getApnsVoipChannelResponse_httpStatus,
    getApnsVoipChannelResponse_aPNSVoipChannelResponse,

    -- ** GetApnsVoipSandboxChannel
    getApnsVoipSandboxChannel_applicationId,
    getApnsVoipSandboxChannelResponse_httpStatus,
    getApnsVoipSandboxChannelResponse_aPNSVoipSandboxChannelResponse,

    -- ** GetApp
    getApp_applicationId,
    getAppResponse_httpStatus,
    getAppResponse_applicationResponse,

    -- ** GetApplicationDateRangeKpi
    getApplicationDateRangeKpi_nextToken,
    getApplicationDateRangeKpi_endTime,
    getApplicationDateRangeKpi_pageSize,
    getApplicationDateRangeKpi_startTime,
    getApplicationDateRangeKpi_applicationId,
    getApplicationDateRangeKpi_kpiName,
    getApplicationDateRangeKpiResponse_httpStatus,
    getApplicationDateRangeKpiResponse_applicationDateRangeKpiResponse,

    -- ** GetApplicationSettings
    getApplicationSettings_applicationId,
    getApplicationSettingsResponse_httpStatus,
    getApplicationSettingsResponse_applicationSettingsResource,

    -- ** GetApps
    getApps_pageSize,
    getApps_token,
    getAppsResponse_httpStatus,
    getAppsResponse_applicationsResponse,

    -- ** GetBaiduChannel
    getBaiduChannel_applicationId,
    getBaiduChannelResponse_httpStatus,
    getBaiduChannelResponse_baiduChannelResponse,

    -- ** GetCampaign
    getCampaign_campaignId,
    getCampaign_applicationId,
    getCampaignResponse_httpStatus,
    getCampaignResponse_campaignResponse,

    -- ** GetCampaignActivities
    getCampaignActivities_pageSize,
    getCampaignActivities_token,
    getCampaignActivities_applicationId,
    getCampaignActivities_campaignId,
    getCampaignActivitiesResponse_httpStatus,
    getCampaignActivitiesResponse_activitiesResponse,

    -- ** GetCampaignDateRangeKpi
    getCampaignDateRangeKpi_nextToken,
    getCampaignDateRangeKpi_endTime,
    getCampaignDateRangeKpi_pageSize,
    getCampaignDateRangeKpi_startTime,
    getCampaignDateRangeKpi_applicationId,
    getCampaignDateRangeKpi_kpiName,
    getCampaignDateRangeKpi_campaignId,
    getCampaignDateRangeKpiResponse_httpStatus,
    getCampaignDateRangeKpiResponse_campaignDateRangeKpiResponse,

    -- ** GetCampaignVersion
    getCampaignVersion_version,
    getCampaignVersion_applicationId,
    getCampaignVersion_campaignId,
    getCampaignVersionResponse_httpStatus,
    getCampaignVersionResponse_campaignResponse,

    -- ** GetCampaignVersions
    getCampaignVersions_pageSize,
    getCampaignVersions_token,
    getCampaignVersions_applicationId,
    getCampaignVersions_campaignId,
    getCampaignVersionsResponse_httpStatus,
    getCampaignVersionsResponse_campaignsResponse,

    -- ** GetCampaigns
    getCampaigns_pageSize,
    getCampaigns_token,
    getCampaigns_applicationId,
    getCampaignsResponse_httpStatus,
    getCampaignsResponse_campaignsResponse,

    -- ** GetChannels
    getChannels_applicationId,
    getChannelsResponse_httpStatus,
    getChannelsResponse_channelsResponse,

    -- ** GetEmailChannel
    getEmailChannel_applicationId,
    getEmailChannelResponse_httpStatus,
    getEmailChannelResponse_emailChannelResponse,

    -- ** GetEmailTemplate
    getEmailTemplate_version,
    getEmailTemplate_templateName,
    getEmailTemplateResponse_httpStatus,
    getEmailTemplateResponse_emailTemplateResponse,

    -- ** GetEndpoint
    getEndpoint_applicationId,
    getEndpoint_endpointId,
    getEndpointResponse_httpStatus,
    getEndpointResponse_endpointResponse,

    -- ** GetEventStream
    getEventStream_applicationId,
    getEventStreamResponse_httpStatus,
    getEventStreamResponse_eventStream,

    -- ** GetExportJob
    getExportJob_applicationId,
    getExportJob_jobId,
    getExportJobResponse_httpStatus,
    getExportJobResponse_exportJobResponse,

    -- ** GetExportJobs
    getExportJobs_pageSize,
    getExportJobs_token,
    getExportJobs_applicationId,
    getExportJobsResponse_httpStatus,
    getExportJobsResponse_exportJobsResponse,

    -- ** GetGcmChannel
    getGcmChannel_applicationId,
    getGcmChannelResponse_httpStatus,
    getGcmChannelResponse_gCMChannelResponse,

    -- ** GetImportJob
    getImportJob_applicationId,
    getImportJob_jobId,
    getImportJobResponse_httpStatus,
    getImportJobResponse_importJobResponse,

    -- ** GetImportJobs
    getImportJobs_pageSize,
    getImportJobs_token,
    getImportJobs_applicationId,
    getImportJobsResponse_httpStatus,
    getImportJobsResponse_importJobsResponse,

    -- ** GetInAppMessages
    getInAppMessages_applicationId,
    getInAppMessages_endpointId,
    getInAppMessagesResponse_httpStatus,
    getInAppMessagesResponse_inAppMessagesResponse,

    -- ** GetInAppTemplate
    getInAppTemplate_version,
    getInAppTemplate_templateName,
    getInAppTemplateResponse_httpStatus,
    getInAppTemplateResponse_inAppTemplateResponse,

    -- ** GetJourney
    getJourney_journeyId,
    getJourney_applicationId,
    getJourneyResponse_httpStatus,
    getJourneyResponse_journeyResponse,

    -- ** GetJourneyDateRangeKpi
    getJourneyDateRangeKpi_nextToken,
    getJourneyDateRangeKpi_endTime,
    getJourneyDateRangeKpi_pageSize,
    getJourneyDateRangeKpi_startTime,
    getJourneyDateRangeKpi_journeyId,
    getJourneyDateRangeKpi_applicationId,
    getJourneyDateRangeKpi_kpiName,
    getJourneyDateRangeKpiResponse_httpStatus,
    getJourneyDateRangeKpiResponse_journeyDateRangeKpiResponse,

    -- ** GetJourneyExecutionActivityMetrics
    getJourneyExecutionActivityMetrics_nextToken,
    getJourneyExecutionActivityMetrics_pageSize,
    getJourneyExecutionActivityMetrics_journeyActivityId,
    getJourneyExecutionActivityMetrics_applicationId,
    getJourneyExecutionActivityMetrics_journeyId,
    getJourneyExecutionActivityMetricsResponse_httpStatus,
    getJourneyExecutionActivityMetricsResponse_journeyExecutionActivityMetricsResponse,

    -- ** GetJourneyExecutionMetrics
    getJourneyExecutionMetrics_nextToken,
    getJourneyExecutionMetrics_pageSize,
    getJourneyExecutionMetrics_applicationId,
    getJourneyExecutionMetrics_journeyId,
    getJourneyExecutionMetricsResponse_httpStatus,
    getJourneyExecutionMetricsResponse_journeyExecutionMetricsResponse,

    -- ** GetPushTemplate
    getPushTemplate_version,
    getPushTemplate_templateName,
    getPushTemplateResponse_httpStatus,
    getPushTemplateResponse_pushNotificationTemplateResponse,

    -- ** GetRecommenderConfiguration
    getRecommenderConfiguration_recommenderId,
    getRecommenderConfigurationResponse_httpStatus,
    getRecommenderConfigurationResponse_recommenderConfigurationResponse,

    -- ** GetRecommenderConfigurations
    getRecommenderConfigurations_pageSize,
    getRecommenderConfigurations_token,
    getRecommenderConfigurationsResponse_httpStatus,
    getRecommenderConfigurationsResponse_listRecommenderConfigurationsResponse,

    -- ** GetSegment
    getSegment_segmentId,
    getSegment_applicationId,
    getSegmentResponse_httpStatus,
    getSegmentResponse_segmentResponse,

    -- ** GetSegmentExportJobs
    getSegmentExportJobs_pageSize,
    getSegmentExportJobs_token,
    getSegmentExportJobs_segmentId,
    getSegmentExportJobs_applicationId,
    getSegmentExportJobsResponse_httpStatus,
    getSegmentExportJobsResponse_exportJobsResponse,

    -- ** GetSegmentImportJobs
    getSegmentImportJobs_pageSize,
    getSegmentImportJobs_token,
    getSegmentImportJobs_segmentId,
    getSegmentImportJobs_applicationId,
    getSegmentImportJobsResponse_httpStatus,
    getSegmentImportJobsResponse_importJobsResponse,

    -- ** GetSegmentVersion
    getSegmentVersion_segmentId,
    getSegmentVersion_version,
    getSegmentVersion_applicationId,
    getSegmentVersionResponse_httpStatus,
    getSegmentVersionResponse_segmentResponse,

    -- ** GetSegmentVersions
    getSegmentVersions_pageSize,
    getSegmentVersions_token,
    getSegmentVersions_segmentId,
    getSegmentVersions_applicationId,
    getSegmentVersionsResponse_httpStatus,
    getSegmentVersionsResponse_segmentsResponse,

    -- ** GetSegments
    getSegments_pageSize,
    getSegments_token,
    getSegments_applicationId,
    getSegmentsResponse_httpStatus,
    getSegmentsResponse_segmentsResponse,

    -- ** GetSmsChannel
    getSmsChannel_applicationId,
    getSmsChannelResponse_httpStatus,
    getSmsChannelResponse_sMSChannelResponse,

    -- ** GetSmsTemplate
    getSmsTemplate_version,
    getSmsTemplate_templateName,
    getSmsTemplateResponse_httpStatus,
    getSmsTemplateResponse_sMSTemplateResponse,

    -- ** GetUserEndpoints
    getUserEndpoints_applicationId,
    getUserEndpoints_userId,
    getUserEndpointsResponse_httpStatus,
    getUserEndpointsResponse_endpointsResponse,

    -- ** GetVoiceChannel
    getVoiceChannel_applicationId,
    getVoiceChannelResponse_httpStatus,
    getVoiceChannelResponse_voiceChannelResponse,

    -- ** GetVoiceTemplate
    getVoiceTemplate_version,
    getVoiceTemplate_templateName,
    getVoiceTemplateResponse_httpStatus,
    getVoiceTemplateResponse_voiceTemplateResponse,

    -- ** ListJourneys
    listJourneys_pageSize,
    listJourneys_token,
    listJourneys_applicationId,
    listJourneysResponse_httpStatus,
    listJourneysResponse_journeysResponse,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_httpStatus,
    listTagsForResourceResponse_tagsModel,

    -- ** ListTemplateVersions
    listTemplateVersions_nextToken,
    listTemplateVersions_pageSize,
    listTemplateVersions_templateName,
    listTemplateVersions_templateType,
    listTemplateVersionsResponse_httpStatus,
    listTemplateVersionsResponse_templateVersionsResponse,

    -- ** ListTemplates
    listTemplates_nextToken,
    listTemplates_templateType,
    listTemplates_pageSize,
    listTemplates_prefix,
    listTemplatesResponse_httpStatus,
    listTemplatesResponse_templatesResponse,

    -- ** PhoneNumberValidate
    phoneNumberValidate_numberValidateRequest,
    phoneNumberValidateResponse_httpStatus,
    phoneNumberValidateResponse_numberValidateResponse,

    -- ** PutEventStream
    putEventStream_applicationId,
    putEventStream_writeEventStream,
    putEventStreamResponse_httpStatus,
    putEventStreamResponse_eventStream,

    -- ** PutEvents
    putEvents_applicationId,
    putEvents_eventsRequest,
    putEventsResponse_httpStatus,
    putEventsResponse_eventsResponse,

    -- ** RemoveAttributes
    removeAttributes_attributeType,
    removeAttributes_applicationId,
    removeAttributes_updateAttributesRequest,
    removeAttributesResponse_httpStatus,
    removeAttributesResponse_attributesResource,

    -- ** SendMessages
    sendMessages_applicationId,
    sendMessages_messageRequest,
    sendMessagesResponse_httpStatus,
    sendMessagesResponse_messageResponse,

    -- ** SendUsersMessages
    sendUsersMessages_applicationId,
    sendUsersMessages_sendUsersMessageRequest,
    sendUsersMessagesResponse_httpStatus,
    sendUsersMessagesResponse_sendUsersMessageResponse,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tagsModel,

    -- ** UntagResource
    untagResource_tagKeys,
    untagResource_resourceArn,

    -- ** UpdateAdmChannel
    updateAdmChannel_applicationId,
    updateAdmChannel_aDMChannelRequest,
    updateAdmChannelResponse_httpStatus,
    updateAdmChannelResponse_aDMChannelResponse,

    -- ** UpdateApnsChannel
    updateApnsChannel_applicationId,
    updateApnsChannel_aPNSChannelRequest,
    updateApnsChannelResponse_httpStatus,
    updateApnsChannelResponse_aPNSChannelResponse,

    -- ** UpdateApnsSandboxChannel
    updateApnsSandboxChannel_applicationId,
    updateApnsSandboxChannel_aPNSSandboxChannelRequest,
    updateApnsSandboxChannelResponse_httpStatus,
    updateApnsSandboxChannelResponse_aPNSSandboxChannelResponse,

    -- ** UpdateApnsVoipChannel
    updateApnsVoipChannel_applicationId,
    updateApnsVoipChannel_aPNSVoipChannelRequest,
    updateApnsVoipChannelResponse_httpStatus,
    updateApnsVoipChannelResponse_aPNSVoipChannelResponse,

    -- ** UpdateApnsVoipSandboxChannel
    updateApnsVoipSandboxChannel_applicationId,
    updateApnsVoipSandboxChannel_aPNSVoipSandboxChannelRequest,
    updateApnsVoipSandboxChannelResponse_httpStatus,
    updateApnsVoipSandboxChannelResponse_aPNSVoipSandboxChannelResponse,

    -- ** UpdateApplicationSettings
    updateApplicationSettings_applicationId,
    updateApplicationSettings_writeApplicationSettingsRequest,
    updateApplicationSettingsResponse_httpStatus,
    updateApplicationSettingsResponse_applicationSettingsResource,

    -- ** UpdateBaiduChannel
    updateBaiduChannel_applicationId,
    updateBaiduChannel_baiduChannelRequest,
    updateBaiduChannelResponse_httpStatus,
    updateBaiduChannelResponse_baiduChannelResponse,

    -- ** UpdateCampaign
    updateCampaign_campaignId,
    updateCampaign_applicationId,
    updateCampaign_writeCampaignRequest,
    updateCampaignResponse_httpStatus,
    updateCampaignResponse_campaignResponse,

    -- ** UpdateEmailChannel
    updateEmailChannel_applicationId,
    updateEmailChannel_emailChannelRequest,
    updateEmailChannelResponse_httpStatus,
    updateEmailChannelResponse_emailChannelResponse,

    -- ** UpdateEmailTemplate
    updateEmailTemplate_createNewVersion,
    updateEmailTemplate_version,
    updateEmailTemplate_templateName,
    updateEmailTemplate_emailTemplateRequest,
    updateEmailTemplateResponse_httpStatus,
    updateEmailTemplateResponse_messageBody,

    -- ** UpdateEndpoint
    updateEndpoint_applicationId,
    updateEndpoint_endpointId,
    updateEndpoint_endpointRequest,
    updateEndpointResponse_httpStatus,
    updateEndpointResponse_messageBody,

    -- ** UpdateEndpointsBatch
    updateEndpointsBatch_applicationId,
    updateEndpointsBatch_endpointBatchRequest,
    updateEndpointsBatchResponse_httpStatus,
    updateEndpointsBatchResponse_messageBody,

    -- ** UpdateGcmChannel
    updateGcmChannel_applicationId,
    updateGcmChannel_gCMChannelRequest,
    updateGcmChannelResponse_httpStatus,
    updateGcmChannelResponse_gCMChannelResponse,

    -- ** UpdateInAppTemplate
    updateInAppTemplate_createNewVersion,
    updateInAppTemplate_version,
    updateInAppTemplate_templateName,
    updateInAppTemplate_inAppTemplateRequest,
    updateInAppTemplateResponse_httpStatus,
    updateInAppTemplateResponse_messageBody,

    -- ** UpdateJourney
    updateJourney_journeyId,
    updateJourney_applicationId,
    updateJourney_writeJourneyRequest,
    updateJourneyResponse_httpStatus,
    updateJourneyResponse_journeyResponse,

    -- ** UpdateJourneyState
    updateJourneyState_journeyId,
    updateJourneyState_applicationId,
    updateJourneyState_journeyStateRequest,
    updateJourneyStateResponse_httpStatus,
    updateJourneyStateResponse_journeyResponse,

    -- ** UpdatePushTemplate
    updatePushTemplate_createNewVersion,
    updatePushTemplate_version,
    updatePushTemplate_templateName,
    updatePushTemplate_pushNotificationTemplateRequest,
    updatePushTemplateResponse_httpStatus,
    updatePushTemplateResponse_messageBody,

    -- ** UpdateRecommenderConfiguration
    updateRecommenderConfiguration'_recommenderId,
    updateRecommenderConfiguration'_updateRecommenderConfiguration,
    updateRecommenderConfigurationResponse_httpStatus,
    updateRecommenderConfigurationResponse_recommenderConfigurationResponse,

    -- ** UpdateSegment
    updateSegment_segmentId,
    updateSegment_applicationId,
    updateSegment_writeSegmentRequest,
    updateSegmentResponse_httpStatus,
    updateSegmentResponse_segmentResponse,

    -- ** UpdateSmsChannel
    updateSmsChannel_applicationId,
    updateSmsChannel_sMSChannelRequest,
    updateSmsChannelResponse_httpStatus,
    updateSmsChannelResponse_sMSChannelResponse,

    -- ** UpdateSmsTemplate
    updateSmsTemplate_createNewVersion,
    updateSmsTemplate_version,
    updateSmsTemplate_templateName,
    updateSmsTemplate_sMSTemplateRequest,
    updateSmsTemplateResponse_httpStatus,
    updateSmsTemplateResponse_messageBody,

    -- ** UpdateTemplateActiveVersion
    updateTemplateActiveVersion_templateName,
    updateTemplateActiveVersion_templateType,
    updateTemplateActiveVersion_templateActiveVersionRequest,
    updateTemplateActiveVersionResponse_httpStatus,
    updateTemplateActiveVersionResponse_messageBody,

    -- ** UpdateVoiceChannel
    updateVoiceChannel_applicationId,
    updateVoiceChannel_voiceChannelRequest,
    updateVoiceChannelResponse_httpStatus,
    updateVoiceChannelResponse_voiceChannelResponse,

    -- ** UpdateVoiceTemplate
    updateVoiceTemplate_createNewVersion,
    updateVoiceTemplate_version,
    updateVoiceTemplate_templateName,
    updateVoiceTemplate_voiceTemplateRequest,
    updateVoiceTemplateResponse_httpStatus,
    updateVoiceTemplateResponse_messageBody,

    -- * Types

    -- ** ADMChannelRequest
    aDMChannelRequest_enabled,
    aDMChannelRequest_clientSecret,
    aDMChannelRequest_clientId,

    -- ** ADMChannelResponse
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

    -- ** ADMMessage
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

    -- ** APNSChannelRequest
    aPNSChannelRequest_privateKey,
    aPNSChannelRequest_teamId,
    aPNSChannelRequest_tokenKeyId,
    aPNSChannelRequest_certificate,
    aPNSChannelRequest_enabled,
    aPNSChannelRequest_tokenKey,
    aPNSChannelRequest_bundleId,
    aPNSChannelRequest_defaultAuthenticationMethod,

    -- ** APNSChannelResponse
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

    -- ** APNSMessage
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

    -- ** APNSPushNotificationTemplate
    aPNSPushNotificationTemplate_body,
    aPNSPushNotificationTemplate_url,
    aPNSPushNotificationTemplate_rawContent,
    aPNSPushNotificationTemplate_sound,
    aPNSPushNotificationTemplate_title,
    aPNSPushNotificationTemplate_mediaUrl,
    aPNSPushNotificationTemplate_action,

    -- ** APNSSandboxChannelRequest
    aPNSSandboxChannelRequest_privateKey,
    aPNSSandboxChannelRequest_teamId,
    aPNSSandboxChannelRequest_tokenKeyId,
    aPNSSandboxChannelRequest_certificate,
    aPNSSandboxChannelRequest_enabled,
    aPNSSandboxChannelRequest_tokenKey,
    aPNSSandboxChannelRequest_bundleId,
    aPNSSandboxChannelRequest_defaultAuthenticationMethod,

    -- ** APNSSandboxChannelResponse
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

    -- ** APNSVoipChannelRequest
    aPNSVoipChannelRequest_privateKey,
    aPNSVoipChannelRequest_teamId,
    aPNSVoipChannelRequest_tokenKeyId,
    aPNSVoipChannelRequest_certificate,
    aPNSVoipChannelRequest_enabled,
    aPNSVoipChannelRequest_tokenKey,
    aPNSVoipChannelRequest_bundleId,
    aPNSVoipChannelRequest_defaultAuthenticationMethod,

    -- ** APNSVoipChannelResponse
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

    -- ** APNSVoipSandboxChannelRequest
    aPNSVoipSandboxChannelRequest_privateKey,
    aPNSVoipSandboxChannelRequest_teamId,
    aPNSVoipSandboxChannelRequest_tokenKeyId,
    aPNSVoipSandboxChannelRequest_certificate,
    aPNSVoipSandboxChannelRequest_enabled,
    aPNSVoipSandboxChannelRequest_tokenKey,
    aPNSVoipSandboxChannelRequest_bundleId,
    aPNSVoipSandboxChannelRequest_defaultAuthenticationMethod,

    -- ** APNSVoipSandboxChannelResponse
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

    -- ** ActivitiesResponse
    activitiesResponse_nextToken,
    activitiesResponse_item,

    -- ** Activity
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

    -- ** ActivityResponse
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

    -- ** AddressConfiguration
    addressConfiguration_context,
    addressConfiguration_substitutions,
    addressConfiguration_titleOverride,
    addressConfiguration_rawContent,
    addressConfiguration_bodyOverride,
    addressConfiguration_channelType,

    -- ** AndroidPushNotificationTemplate
    androidPushNotificationTemplate_body,
    androidPushNotificationTemplate_imageUrl,
    androidPushNotificationTemplate_url,
    androidPushNotificationTemplate_rawContent,
    androidPushNotificationTemplate_sound,
    androidPushNotificationTemplate_imageIconUrl,
    androidPushNotificationTemplate_title,
    androidPushNotificationTemplate_action,
    androidPushNotificationTemplate_smallImageIconUrl,

    -- ** ApplicationDateRangeKpiResponse
    applicationDateRangeKpiResponse_nextToken,
    applicationDateRangeKpiResponse_kpiResult,
    applicationDateRangeKpiResponse_kpiName,
    applicationDateRangeKpiResponse_endTime,
    applicationDateRangeKpiResponse_startTime,
    applicationDateRangeKpiResponse_applicationId,

    -- ** ApplicationResponse
    applicationResponse_tags,
    applicationResponse_id,
    applicationResponse_arn,
    applicationResponse_name,

    -- ** ApplicationSettingsResource
    applicationSettingsResource_lastModifiedDate,
    applicationSettingsResource_limits,
    applicationSettingsResource_quietTime,
    applicationSettingsResource_campaignHook,
    applicationSettingsResource_applicationId,

    -- ** ApplicationsResponse
    applicationsResponse_nextToken,
    applicationsResponse_item,

    -- ** AttributeDimension
    attributeDimension_attributeType,
    attributeDimension_values,

    -- ** AttributesResource
    attributesResource_attributes,
    attributesResource_attributeType,
    attributesResource_applicationId,

    -- ** BaiduChannelRequest
    baiduChannelRequest_enabled,
    baiduChannelRequest_secretKey,
    baiduChannelRequest_apiKey,

    -- ** BaiduChannelResponse
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

    -- ** BaiduMessage
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

    -- ** BaseKpiResult
    baseKpiResult_rows,

    -- ** CampaignCustomMessage
    campaignCustomMessage_data,

    -- ** CampaignDateRangeKpiResponse
    campaignDateRangeKpiResponse_nextToken,
    campaignDateRangeKpiResponse_kpiResult,
    campaignDateRangeKpiResponse_kpiName,
    campaignDateRangeKpiResponse_endTime,
    campaignDateRangeKpiResponse_campaignId,
    campaignDateRangeKpiResponse_startTime,
    campaignDateRangeKpiResponse_applicationId,

    -- ** CampaignEmailMessage
    campaignEmailMessage_fromAddress,
    campaignEmailMessage_body,
    campaignEmailMessage_title,
    campaignEmailMessage_htmlBody,

    -- ** CampaignEventFilter
    campaignEventFilter_filterType,
    campaignEventFilter_dimensions,

    -- ** CampaignHook
    campaignHook_lambdaFunctionName,
    campaignHook_webUrl,
    campaignHook_mode,

    -- ** CampaignInAppMessage
    campaignInAppMessage_customConfig,
    campaignInAppMessage_body,
    campaignInAppMessage_layout,
    campaignInAppMessage_content,

    -- ** CampaignLimits
    campaignLimits_maximumDuration,
    campaignLimits_total,
    campaignLimits_messagesPerSecond,
    campaignLimits_session,
    campaignLimits_daily,

    -- ** CampaignResponse
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

    -- ** CampaignSmsMessage
    campaignSmsMessage_entityId,
    campaignSmsMessage_messageType,
    campaignSmsMessage_senderId,
    campaignSmsMessage_body,
    campaignSmsMessage_templateId,
    campaignSmsMessage_originationNumber,

    -- ** CampaignState
    campaignState_campaignStatus,

    -- ** CampaignsResponse
    campaignsResponse_nextToken,
    campaignsResponse_item,

    -- ** ChannelResponse
    channelResponse_lastModifiedDate,
    channelResponse_creationDate,
    channelResponse_hasCredential,
    channelResponse_id,
    channelResponse_enabled,
    channelResponse_lastModifiedBy,
    channelResponse_isArchived,
    channelResponse_applicationId,
    channelResponse_version,

    -- ** ChannelsResponse
    channelsResponse_channels,

    -- ** Condition
    condition_conditions,
    condition_operator,

    -- ** ConditionalSplitActivity
    conditionalSplitActivity_condition,
    conditionalSplitActivity_falseActivity,
    conditionalSplitActivity_evaluationWaitTime,
    conditionalSplitActivity_trueActivity,

    -- ** ContactCenterActivity
    contactCenterActivity_nextActivity,

    -- ** CreateApplicationRequest
    createApplicationRequest_tags,
    createApplicationRequest_name,

    -- ** CreateRecommenderConfiguration
    createRecommenderConfiguration_name,
    createRecommenderConfiguration_recommendationTransformerUri,
    createRecommenderConfiguration_description,
    createRecommenderConfiguration_recommendationsDisplayName,
    createRecommenderConfiguration_recommendationProviderIdType,
    createRecommenderConfiguration_attributes,
    createRecommenderConfiguration_recommendationsPerMessage,
    createRecommenderConfiguration_recommendationProviderUri,
    createRecommenderConfiguration_recommendationProviderRoleArn,

    -- ** CreateTemplateMessageBody
    createTemplateMessageBody_message,
    createTemplateMessageBody_requestID,
    createTemplateMessageBody_arn,

    -- ** CustomDeliveryConfiguration
    customDeliveryConfiguration_endpointTypes,
    customDeliveryConfiguration_deliveryUri,

    -- ** CustomMessageActivity
    customMessageActivity_endpointTypes,
    customMessageActivity_templateName,
    customMessageActivity_nextActivity,
    customMessageActivity_templateVersion,
    customMessageActivity_deliveryUri,
    customMessageActivity_messageConfig,

    -- ** DefaultButtonConfiguration
    defaultButtonConfiguration_link,
    defaultButtonConfiguration_textColor,
    defaultButtonConfiguration_backgroundColor,
    defaultButtonConfiguration_borderRadius,
    defaultButtonConfiguration_buttonAction,
    defaultButtonConfiguration_text,

    -- ** DefaultMessage
    defaultMessage_body,
    defaultMessage_substitutions,

    -- ** DefaultPushNotificationMessage
    defaultPushNotificationMessage_body,
    defaultPushNotificationMessage_url,
    defaultPushNotificationMessage_substitutions,
    defaultPushNotificationMessage_silentPush,
    defaultPushNotificationMessage_title,
    defaultPushNotificationMessage_action,
    defaultPushNotificationMessage_data,

    -- ** DefaultPushNotificationTemplate
    defaultPushNotificationTemplate_body,
    defaultPushNotificationTemplate_url,
    defaultPushNotificationTemplate_sound,
    defaultPushNotificationTemplate_title,
    defaultPushNotificationTemplate_action,

    -- ** DirectMessageConfiguration
    directMessageConfiguration_aDMMessage,
    directMessageConfiguration_sMSMessage,
    directMessageConfiguration_voiceMessage,
    directMessageConfiguration_defaultMessage,
    directMessageConfiguration_aPNSMessage,
    directMessageConfiguration_gCMMessage,
    directMessageConfiguration_defaultPushNotificationMessage,
    directMessageConfiguration_emailMessage,
    directMessageConfiguration_baiduMessage,

    -- ** EmailChannelRequest
    emailChannelRequest_roleArn,
    emailChannelRequest_enabled,
    emailChannelRequest_configurationSet,
    emailChannelRequest_fromAddress,
    emailChannelRequest_identity,

    -- ** EmailChannelResponse
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

    -- ** EmailMessage
    emailMessage_feedbackForwardingAddress,
    emailMessage_replyToAddresses,
    emailMessage_fromAddress,
    emailMessage_simpleEmail,
    emailMessage_body,
    emailMessage_substitutions,
    emailMessage_rawEmail,

    -- ** EmailMessageActivity
    emailMessageActivity_templateName,
    emailMessageActivity_nextActivity,
    emailMessageActivity_templateVersion,
    emailMessageActivity_messageConfig,

    -- ** EmailTemplateRequest
    emailTemplateRequest_tags,
    emailTemplateRequest_recommenderId,
    emailTemplateRequest_defaultSubstitutions,
    emailTemplateRequest_textPart,
    emailTemplateRequest_subject,
    emailTemplateRequest_htmlPart,
    emailTemplateRequest_templateDescription,

    -- ** EmailTemplateResponse
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

    -- ** EndpointBatchItem
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

    -- ** EndpointBatchRequest
    endpointBatchRequest_item,

    -- ** EndpointDemographic
    endpointDemographic_model,
    endpointDemographic_locale,
    endpointDemographic_timezone,
    endpointDemographic_modelVersion,
    endpointDemographic_platform,
    endpointDemographic_make,
    endpointDemographic_appVersion,
    endpointDemographic_platformVersion,

    -- ** EndpointItemResponse
    endpointItemResponse_message,
    endpointItemResponse_statusCode,

    -- ** EndpointLocation
    endpointLocation_longitude,
    endpointLocation_postalCode,
    endpointLocation_country,
    endpointLocation_region,
    endpointLocation_city,
    endpointLocation_latitude,

    -- ** EndpointMessageResult
    endpointMessageResult_updatedToken,
    endpointMessageResult_messageId,
    endpointMessageResult_address,
    endpointMessageResult_statusMessage,
    endpointMessageResult_deliveryStatus,
    endpointMessageResult_statusCode,

    -- ** EndpointRequest
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

    -- ** EndpointResponse
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

    -- ** EndpointSendConfiguration
    endpointSendConfiguration_context,
    endpointSendConfiguration_substitutions,
    endpointSendConfiguration_titleOverride,
    endpointSendConfiguration_rawContent,
    endpointSendConfiguration_bodyOverride,

    -- ** EndpointUser
    endpointUser_userAttributes,
    endpointUser_userId,

    -- ** EndpointsResponse
    endpointsResponse_item,

    -- ** Event
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

    -- ** EventCondition
    eventCondition_dimensions,
    eventCondition_messageActivity,

    -- ** EventDimensions
    eventDimensions_eventType,
    eventDimensions_metrics,
    eventDimensions_attributes,

    -- ** EventFilter
    eventFilter_filterType,
    eventFilter_dimensions,

    -- ** EventItemResponse
    eventItemResponse_message,
    eventItemResponse_statusCode,

    -- ** EventStartCondition
    eventStartCondition_eventFilter,
    eventStartCondition_segmentId,

    -- ** EventStream
    eventStream_lastModifiedDate,
    eventStream_externalId,
    eventStream_lastUpdatedBy,
    eventStream_applicationId,
    eventStream_roleArn,
    eventStream_destinationStreamArn,

    -- ** EventsBatch
    eventsBatch_endpoint,
    eventsBatch_events,

    -- ** EventsRequest
    eventsRequest_batchItem,

    -- ** EventsResponse
    eventsResponse_results,

    -- ** ExportJobRequest
    exportJobRequest_segmentVersion,
    exportJobRequest_segmentId,
    exportJobRequest_s3UrlPrefix,
    exportJobRequest_roleArn,

    -- ** ExportJobResource
    exportJobResource_segmentVersion,
    exportJobResource_segmentId,
    exportJobResource_s3UrlPrefix,
    exportJobResource_roleArn,

    -- ** ExportJobResponse
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

    -- ** ExportJobsResponse
    exportJobsResponse_nextToken,
    exportJobsResponse_item,

    -- ** GCMChannelRequest
    gCMChannelRequest_enabled,
    gCMChannelRequest_apiKey,

    -- ** GCMChannelResponse
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

    -- ** GCMMessage
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

    -- ** GPSCoordinates
    gPSCoordinates_latitude,
    gPSCoordinates_longitude,

    -- ** GPSPointDimension
    gPSPointDimension_rangeInKilometers,
    gPSPointDimension_coordinates,

    -- ** HoldoutActivity
    holdoutActivity_nextActivity,
    holdoutActivity_percentage,

    -- ** ImportJobRequest
    importJobRequest_defineSegment,
    importJobRequest_segmentId,
    importJobRequest_externalId,
    importJobRequest_segmentName,
    importJobRequest_registerEndpoints,
    importJobRequest_format,
    importJobRequest_s3Url,
    importJobRequest_roleArn,

    -- ** ImportJobResource
    importJobResource_defineSegment,
    importJobResource_segmentId,
    importJobResource_externalId,
    importJobResource_segmentName,
    importJobResource_registerEndpoints,
    importJobResource_format,
    importJobResource_s3Url,
    importJobResource_roleArn,

    -- ** ImportJobResponse
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

    -- ** ImportJobsResponse
    importJobsResponse_nextToken,
    importJobsResponse_item,

    -- ** InAppCampaignSchedule
    inAppCampaignSchedule_eventFilter,
    inAppCampaignSchedule_endDate,
    inAppCampaignSchedule_quietTime,

    -- ** InAppMessage
    inAppMessage_customConfig,
    inAppMessage_layout,
    inAppMessage_content,

    -- ** InAppMessageBodyConfig
    inAppMessageBodyConfig_alignment,
    inAppMessageBodyConfig_textColor,
    inAppMessageBodyConfig_body,

    -- ** InAppMessageButton
    inAppMessageButton_web,
    inAppMessageButton_ios,
    inAppMessageButton_android,
    inAppMessageButton_defaultConfig,

    -- ** InAppMessageCampaign
    inAppMessageCampaign_schedule,
    inAppMessageCampaign_sessionCap,
    inAppMessageCampaign_campaignId,
    inAppMessageCampaign_treatmentId,
    inAppMessageCampaign_inAppMessage,
    inAppMessageCampaign_totalCap,
    inAppMessageCampaign_priority,
    inAppMessageCampaign_dailyCap,

    -- ** InAppMessageContent
    inAppMessageContent_bodyConfig,
    inAppMessageContent_secondaryBtn,
    inAppMessageContent_imageUrl,
    inAppMessageContent_headerConfig,
    inAppMessageContent_backgroundColor,
    inAppMessageContent_primaryBtn,

    -- ** InAppMessageHeaderConfig
    inAppMessageHeaderConfig_alignment,
    inAppMessageHeaderConfig_header,
    inAppMessageHeaderConfig_textColor,

    -- ** InAppMessagesResponse
    inAppMessagesResponse_inAppMessageCampaigns,

    -- ** InAppTemplateRequest
    inAppTemplateRequest_customConfig,
    inAppTemplateRequest_tags,
    inAppTemplateRequest_layout,
    inAppTemplateRequest_content,
    inAppTemplateRequest_templateDescription,

    -- ** InAppTemplateResponse
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

    -- ** ItemResponse
    itemResponse_eventsItemResponse,
    itemResponse_endpointItemResponse,

    -- ** JourneyChannelSettings
    journeyChannelSettings_connectCampaignExecutionRoleArn,
    journeyChannelSettings_connectCampaignArn,

    -- ** JourneyCustomMessage
    journeyCustomMessage_data,

    -- ** JourneyDateRangeKpiResponse
    journeyDateRangeKpiResponse_nextToken,
    journeyDateRangeKpiResponse_kpiResult,
    journeyDateRangeKpiResponse_kpiName,
    journeyDateRangeKpiResponse_journeyId,
    journeyDateRangeKpiResponse_endTime,
    journeyDateRangeKpiResponse_startTime,
    journeyDateRangeKpiResponse_applicationId,

    -- ** JourneyEmailMessage
    journeyEmailMessage_fromAddress,

    -- ** JourneyExecutionActivityMetricsResponse
    journeyExecutionActivityMetricsResponse_metrics,
    journeyExecutionActivityMetricsResponse_journeyId,
    journeyExecutionActivityMetricsResponse_lastEvaluatedTime,
    journeyExecutionActivityMetricsResponse_journeyActivityId,
    journeyExecutionActivityMetricsResponse_activityType,
    journeyExecutionActivityMetricsResponse_applicationId,

    -- ** JourneyExecutionMetricsResponse
    journeyExecutionMetricsResponse_metrics,
    journeyExecutionMetricsResponse_journeyId,
    journeyExecutionMetricsResponse_lastEvaluatedTime,
    journeyExecutionMetricsResponse_applicationId,

    -- ** JourneyLimits
    journeyLimits_endpointReentryInterval,
    journeyLimits_messagesPerSecond,
    journeyLimits_dailyCap,
    journeyLimits_endpointReentryCap,

    -- ** JourneyPushMessage
    journeyPushMessage_timeToLive,

    -- ** JourneyResponse
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

    -- ** JourneySMSMessage
    journeySMSMessage_entityId,
    journeySMSMessage_messageType,
    journeySMSMessage_senderId,
    journeySMSMessage_templateId,
    journeySMSMessage_originationNumber,

    -- ** JourneySchedule
    journeySchedule_timezone,
    journeySchedule_endTime,
    journeySchedule_startTime,

    -- ** JourneyStateRequest
    journeyStateRequest_state,

    -- ** JourneysResponse
    journeysResponse_nextToken,
    journeysResponse_item,

    -- ** ListRecommenderConfigurationsResponse
    listRecommenderConfigurationsResponse_nextToken,
    listRecommenderConfigurationsResponse_item,

    -- ** Message
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

    -- ** MessageBody
    messageBody_message,
    messageBody_requestID,

    -- ** MessageConfiguration
    messageConfiguration_aDMMessage,
    messageConfiguration_sMSMessage,
    messageConfiguration_inAppMessage,
    messageConfiguration_defaultMessage,
    messageConfiguration_aPNSMessage,
    messageConfiguration_customMessage,
    messageConfiguration_gCMMessage,
    messageConfiguration_emailMessage,
    messageConfiguration_baiduMessage,

    -- ** MessageRequest
    messageRequest_context,
    messageRequest_endpoints,
    messageRequest_addresses,
    messageRequest_templateConfiguration,
    messageRequest_traceId,
    messageRequest_messageConfiguration,

    -- ** MessageResponse
    messageResponse_requestId,
    messageResponse_endpointResult,
    messageResponse_result,
    messageResponse_applicationId,

    -- ** MessageResult
    messageResult_updatedToken,
    messageResult_messageId,
    messageResult_statusMessage,
    messageResult_deliveryStatus,
    messageResult_statusCode,

    -- ** MetricDimension
    metricDimension_comparisonOperator,
    metricDimension_value,

    -- ** MultiConditionalBranch
    multiConditionalBranch_nextActivity,
    multiConditionalBranch_condition,

    -- ** MultiConditionalSplitActivity
    multiConditionalSplitActivity_branches,
    multiConditionalSplitActivity_defaultActivity,
    multiConditionalSplitActivity_evaluationWaitTime,

    -- ** NumberValidateRequest
    numberValidateRequest_isoCountryCode,
    numberValidateRequest_phoneNumber,

    -- ** NumberValidateResponse
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

    -- ** OverrideButtonConfiguration
    overrideButtonConfiguration_link,
    overrideButtonConfiguration_buttonAction,

    -- ** PublicEndpoint
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

    -- ** PushMessageActivity
    pushMessageActivity_templateName,
    pushMessageActivity_nextActivity,
    pushMessageActivity_templateVersion,
    pushMessageActivity_messageConfig,

    -- ** PushNotificationTemplateRequest
    pushNotificationTemplateRequest_tags,
    pushNotificationTemplateRequest_adm,
    pushNotificationTemplateRequest_apns,
    pushNotificationTemplateRequest_default,
    pushNotificationTemplateRequest_recommenderId,
    pushNotificationTemplateRequest_defaultSubstitutions,
    pushNotificationTemplateRequest_baidu,
    pushNotificationTemplateRequest_templateDescription,
    pushNotificationTemplateRequest_gcm,

    -- ** PushNotificationTemplateResponse
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

    -- ** QuietTime
    quietTime_start,
    quietTime_end,

    -- ** RandomSplitActivity
    randomSplitActivity_branches,

    -- ** RandomSplitEntry
    randomSplitEntry_nextActivity,
    randomSplitEntry_percentage,

    -- ** RawEmail
    rawEmail_data,

    -- ** RecencyDimension
    recencyDimension_duration,
    recencyDimension_recencyType,

    -- ** RecommenderConfigurationResponse
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

    -- ** ResultRow
    resultRow_groupedBys,
    resultRow_values,

    -- ** ResultRowValue
    resultRowValue_type,
    resultRowValue_value,
    resultRowValue_key,

    -- ** SMSChannelRequest
    sMSChannelRequest_senderId,
    sMSChannelRequest_enabled,
    sMSChannelRequest_shortCode,

    -- ** SMSChannelResponse
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

    -- ** SMSMessage
    sMSMessage_entityId,
    sMSMessage_messageType,
    sMSMessage_senderId,
    sMSMessage_body,
    sMSMessage_substitutions,
    sMSMessage_templateId,
    sMSMessage_originationNumber,
    sMSMessage_keyword,
    sMSMessage_mediaUrl,

    -- ** SMSMessageActivity
    sMSMessageActivity_templateName,
    sMSMessageActivity_nextActivity,
    sMSMessageActivity_templateVersion,
    sMSMessageActivity_messageConfig,

    -- ** SMSTemplateRequest
    sMSTemplateRequest_tags,
    sMSTemplateRequest_body,
    sMSTemplateRequest_recommenderId,
    sMSTemplateRequest_defaultSubstitutions,
    sMSTemplateRequest_templateDescription,

    -- ** SMSTemplateResponse
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

    -- ** Schedule
    schedule_eventFilter,
    schedule_timezone,
    schedule_frequency,
    schedule_endTime,
    schedule_quietTime,
    schedule_isLocalTime,
    schedule_startTime,

    -- ** SegmentBehaviors
    segmentBehaviors_recency,

    -- ** SegmentCondition
    segmentCondition_segmentId,

    -- ** SegmentDemographics
    segmentDemographics_model,
    segmentDemographics_channel,
    segmentDemographics_platform,
    segmentDemographics_make,
    segmentDemographics_appVersion,
    segmentDemographics_deviceType,

    -- ** SegmentDimensions
    segmentDimensions_demographic,
    segmentDimensions_metrics,
    segmentDimensions_userAttributes,
    segmentDimensions_location,
    segmentDimensions_attributes,
    segmentDimensions_behavior,

    -- ** SegmentGroup
    segmentGroup_type,
    segmentGroup_dimensions,
    segmentGroup_sourceType,
    segmentGroup_sourceSegments,

    -- ** SegmentGroupList
    segmentGroupList_include,
    segmentGroupList_groups,

    -- ** SegmentImportResource
    segmentImportResource_channelCounts,
    segmentImportResource_format,
    segmentImportResource_s3Url,
    segmentImportResource_size,
    segmentImportResource_externalId,
    segmentImportResource_roleArn,

    -- ** SegmentLocation
    segmentLocation_country,
    segmentLocation_gPSPoint,

    -- ** SegmentReference
    segmentReference_version,
    segmentReference_id,

    -- ** SegmentResponse
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

    -- ** SegmentsResponse
    segmentsResponse_nextToken,
    segmentsResponse_item,

    -- ** SendUsersMessageRequest
    sendUsersMessageRequest_context,
    sendUsersMessageRequest_templateConfiguration,
    sendUsersMessageRequest_traceId,
    sendUsersMessageRequest_messageConfiguration,
    sendUsersMessageRequest_users,

    -- ** SendUsersMessageResponse
    sendUsersMessageResponse_requestId,
    sendUsersMessageResponse_result,
    sendUsersMessageResponse_applicationId,

    -- ** Session
    session_duration,
    session_stopTimestamp,
    session_startTimestamp,
    session_id,

    -- ** SetDimension
    setDimension_dimensionType,
    setDimension_values,

    -- ** SimpleCondition
    simpleCondition_segmentCondition,
    simpleCondition_eventCondition,
    simpleCondition_segmentDimensions,

    -- ** SimpleEmail
    simpleEmail_textPart,
    simpleEmail_subject,
    simpleEmail_htmlPart,

    -- ** SimpleEmailPart
    simpleEmailPart_data,
    simpleEmailPart_charset,

    -- ** StartCondition
    startCondition_eventStartCondition,
    startCondition_segmentStartCondition,
    startCondition_description,

    -- ** TagsModel
    tagsModel_tags,

    -- ** Template
    template_name,
    template_version,

    -- ** TemplateActiveVersionRequest
    templateActiveVersionRequest_version,

    -- ** TemplateConfiguration
    templateConfiguration_emailTemplate,
    templateConfiguration_sMSTemplate,
    templateConfiguration_voiceTemplate,
    templateConfiguration_pushTemplate,

    -- ** TemplateCreateMessageBody
    templateCreateMessageBody_message,
    templateCreateMessageBody_requestID,
    templateCreateMessageBody_arn,

    -- ** TemplateResponse
    templateResponse_tags,
    templateResponse_arn,
    templateResponse_defaultSubstitutions,
    templateResponse_templateDescription,
    templateResponse_version,
    templateResponse_lastModifiedDate,
    templateResponse_creationDate,
    templateResponse_templateName,
    templateResponse_templateType,

    -- ** TemplateVersionResponse
    templateVersionResponse_defaultSubstitutions,
    templateVersionResponse_templateDescription,
    templateVersionResponse_version,
    templateVersionResponse_lastModifiedDate,
    templateVersionResponse_creationDate,
    templateVersionResponse_templateName,
    templateVersionResponse_templateType,

    -- ** TemplateVersionsResponse
    templateVersionsResponse_message,
    templateVersionsResponse_nextToken,
    templateVersionsResponse_requestID,
    templateVersionsResponse_item,

    -- ** TemplatesResponse
    templatesResponse_nextToken,
    templatesResponse_item,

    -- ** TreatmentResource
    treatmentResource_schedule,
    treatmentResource_customDeliveryConfiguration,
    treatmentResource_treatmentName,
    treatmentResource_state,
    treatmentResource_messageConfiguration,
    treatmentResource_treatmentDescription,
    treatmentResource_templateConfiguration,
    treatmentResource_id,
    treatmentResource_sizePercent,

    -- ** UpdateAttributesRequest
    updateAttributesRequest_blacklist,

    -- ** UpdateRecommenderConfiguration
    updateRecommenderConfiguration_name,
    updateRecommenderConfiguration_recommendationTransformerUri,
    updateRecommenderConfiguration_description,
    updateRecommenderConfiguration_recommendationsDisplayName,
    updateRecommenderConfiguration_recommendationProviderIdType,
    updateRecommenderConfiguration_attributes,
    updateRecommenderConfiguration_recommendationsPerMessage,
    updateRecommenderConfiguration_recommendationProviderUri,
    updateRecommenderConfiguration_recommendationProviderRoleArn,

    -- ** VoiceChannelRequest
    voiceChannelRequest_enabled,

    -- ** VoiceChannelResponse
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

    -- ** VoiceMessage
    voiceMessage_voiceId,
    voiceMessage_body,
    voiceMessage_substitutions,
    voiceMessage_originationNumber,
    voiceMessage_languageCode,

    -- ** VoiceTemplateRequest
    voiceTemplateRequest_tags,
    voiceTemplateRequest_voiceId,
    voiceTemplateRequest_body,
    voiceTemplateRequest_defaultSubstitutions,
    voiceTemplateRequest_languageCode,
    voiceTemplateRequest_templateDescription,

    -- ** VoiceTemplateResponse
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

    -- ** WaitActivity
    waitActivity_nextActivity,
    waitActivity_waitTime,

    -- ** WaitTime
    waitTime_waitFor,
    waitTime_waitUntil,

    -- ** WriteApplicationSettingsRequest
    writeApplicationSettingsRequest_limits,
    writeApplicationSettingsRequest_eventTaggingEnabled,
    writeApplicationSettingsRequest_cloudWatchMetricsEnabled,
    writeApplicationSettingsRequest_quietTime,
    writeApplicationSettingsRequest_campaignHook,

    -- ** WriteCampaignRequest
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

    -- ** WriteEventStream
    writeEventStream_roleArn,
    writeEventStream_destinationStreamArn,

    -- ** WriteJourneyRequest
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

    -- ** WriteSegmentRequest
    writeSegmentRequest_tags,
    writeSegmentRequest_name,
    writeSegmentRequest_dimensions,
    writeSegmentRequest_segmentGroups,

    -- ** WriteTreatmentResource
    writeTreatmentResource_schedule,
    writeTreatmentResource_customDeliveryConfiguration,
    writeTreatmentResource_treatmentName,
    writeTreatmentResource_messageConfiguration,
    writeTreatmentResource_treatmentDescription,
    writeTreatmentResource_templateConfiguration,
    writeTreatmentResource_sizePercent,
  )
where

import Amazonka.Pinpoint.CreateApp
import Amazonka.Pinpoint.CreateCampaign
import Amazonka.Pinpoint.CreateEmailTemplate
import Amazonka.Pinpoint.CreateExportJob
import Amazonka.Pinpoint.CreateImportJob
import Amazonka.Pinpoint.CreateInAppTemplate
import Amazonka.Pinpoint.CreateJourney
import Amazonka.Pinpoint.CreatePushTemplate
import Amazonka.Pinpoint.CreateRecommenderConfiguration
import Amazonka.Pinpoint.CreateSegment
import Amazonka.Pinpoint.CreateSmsTemplate
import Amazonka.Pinpoint.CreateVoiceTemplate
import Amazonka.Pinpoint.DeleteAdmChannel
import Amazonka.Pinpoint.DeleteApnsChannel
import Amazonka.Pinpoint.DeleteApnsSandboxChannel
import Amazonka.Pinpoint.DeleteApnsVoipChannel
import Amazonka.Pinpoint.DeleteApnsVoipSandboxChannel
import Amazonka.Pinpoint.DeleteApp
import Amazonka.Pinpoint.DeleteBaiduChannel
import Amazonka.Pinpoint.DeleteCampaign
import Amazonka.Pinpoint.DeleteEmailChannel
import Amazonka.Pinpoint.DeleteEmailTemplate
import Amazonka.Pinpoint.DeleteEndpoint
import Amazonka.Pinpoint.DeleteEventStream
import Amazonka.Pinpoint.DeleteGcmChannel
import Amazonka.Pinpoint.DeleteInAppTemplate
import Amazonka.Pinpoint.DeleteJourney
import Amazonka.Pinpoint.DeletePushTemplate
import Amazonka.Pinpoint.DeleteRecommenderConfiguration
import Amazonka.Pinpoint.DeleteSegment
import Amazonka.Pinpoint.DeleteSmsChannel
import Amazonka.Pinpoint.DeleteSmsTemplate
import Amazonka.Pinpoint.DeleteUserEndpoints
import Amazonka.Pinpoint.DeleteVoiceChannel
import Amazonka.Pinpoint.DeleteVoiceTemplate
import Amazonka.Pinpoint.GetAdmChannel
import Amazonka.Pinpoint.GetApnsChannel
import Amazonka.Pinpoint.GetApnsSandboxChannel
import Amazonka.Pinpoint.GetApnsVoipChannel
import Amazonka.Pinpoint.GetApnsVoipSandboxChannel
import Amazonka.Pinpoint.GetApp
import Amazonka.Pinpoint.GetApplicationDateRangeKpi
import Amazonka.Pinpoint.GetApplicationSettings
import Amazonka.Pinpoint.GetApps
import Amazonka.Pinpoint.GetBaiduChannel
import Amazonka.Pinpoint.GetCampaign
import Amazonka.Pinpoint.GetCampaignActivities
import Amazonka.Pinpoint.GetCampaignDateRangeKpi
import Amazonka.Pinpoint.GetCampaignVersion
import Amazonka.Pinpoint.GetCampaignVersions
import Amazonka.Pinpoint.GetCampaigns
import Amazonka.Pinpoint.GetChannels
import Amazonka.Pinpoint.GetEmailChannel
import Amazonka.Pinpoint.GetEmailTemplate
import Amazonka.Pinpoint.GetEndpoint
import Amazonka.Pinpoint.GetEventStream
import Amazonka.Pinpoint.GetExportJob
import Amazonka.Pinpoint.GetExportJobs
import Amazonka.Pinpoint.GetGcmChannel
import Amazonka.Pinpoint.GetImportJob
import Amazonka.Pinpoint.GetImportJobs
import Amazonka.Pinpoint.GetInAppMessages
import Amazonka.Pinpoint.GetInAppTemplate
import Amazonka.Pinpoint.GetJourney
import Amazonka.Pinpoint.GetJourneyDateRangeKpi
import Amazonka.Pinpoint.GetJourneyExecutionActivityMetrics
import Amazonka.Pinpoint.GetJourneyExecutionMetrics
import Amazonka.Pinpoint.GetPushTemplate
import Amazonka.Pinpoint.GetRecommenderConfiguration
import Amazonka.Pinpoint.GetRecommenderConfigurations
import Amazonka.Pinpoint.GetSegment
import Amazonka.Pinpoint.GetSegmentExportJobs
import Amazonka.Pinpoint.GetSegmentImportJobs
import Amazonka.Pinpoint.GetSegmentVersion
import Amazonka.Pinpoint.GetSegmentVersions
import Amazonka.Pinpoint.GetSegments
import Amazonka.Pinpoint.GetSmsChannel
import Amazonka.Pinpoint.GetSmsTemplate
import Amazonka.Pinpoint.GetUserEndpoints
import Amazonka.Pinpoint.GetVoiceChannel
import Amazonka.Pinpoint.GetVoiceTemplate
import Amazonka.Pinpoint.ListJourneys
import Amazonka.Pinpoint.ListTagsForResource
import Amazonka.Pinpoint.ListTemplateVersions
import Amazonka.Pinpoint.ListTemplates
import Amazonka.Pinpoint.PhoneNumberValidate
import Amazonka.Pinpoint.PutEventStream
import Amazonka.Pinpoint.PutEvents
import Amazonka.Pinpoint.RemoveAttributes
import Amazonka.Pinpoint.SendMessages
import Amazonka.Pinpoint.SendUsersMessages
import Amazonka.Pinpoint.TagResource
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
import Amazonka.Pinpoint.Types.ActivitiesResponse
import Amazonka.Pinpoint.Types.Activity
import Amazonka.Pinpoint.Types.ActivityResponse
import Amazonka.Pinpoint.Types.AddressConfiguration
import Amazonka.Pinpoint.Types.AndroidPushNotificationTemplate
import Amazonka.Pinpoint.Types.ApplicationDateRangeKpiResponse
import Amazonka.Pinpoint.Types.ApplicationResponse
import Amazonka.Pinpoint.Types.ApplicationSettingsResource
import Amazonka.Pinpoint.Types.ApplicationsResponse
import Amazonka.Pinpoint.Types.AttributeDimension
import Amazonka.Pinpoint.Types.AttributesResource
import Amazonka.Pinpoint.Types.BaiduChannelRequest
import Amazonka.Pinpoint.Types.BaiduChannelResponse
import Amazonka.Pinpoint.Types.BaiduMessage
import Amazonka.Pinpoint.Types.BaseKpiResult
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
import Amazonka.Pinpoint.Types.CampaignsResponse
import Amazonka.Pinpoint.Types.ChannelResponse
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
import Amazonka.Pinpoint.Types.DirectMessageConfiguration
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
import Amazonka.Pinpoint.Types.ItemResponse
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
import Amazonka.Pinpoint.Types.ListRecommenderConfigurationsResponse
import Amazonka.Pinpoint.Types.Message
import Amazonka.Pinpoint.Types.MessageBody
import Amazonka.Pinpoint.Types.MessageConfiguration
import Amazonka.Pinpoint.Types.MessageRequest
import Amazonka.Pinpoint.Types.MessageResponse
import Amazonka.Pinpoint.Types.MessageResult
import Amazonka.Pinpoint.Types.MetricDimension
import Amazonka.Pinpoint.Types.MultiConditionalBranch
import Amazonka.Pinpoint.Types.MultiConditionalSplitActivity
import Amazonka.Pinpoint.Types.NumberValidateRequest
import Amazonka.Pinpoint.Types.NumberValidateResponse
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
import Amazonka.Pinpoint.Types.SegmentsResponse
import Amazonka.Pinpoint.Types.SendUsersMessageRequest
import Amazonka.Pinpoint.Types.SendUsersMessageResponse
import Amazonka.Pinpoint.Types.Session
import Amazonka.Pinpoint.Types.SetDimension
import Amazonka.Pinpoint.Types.SimpleCondition
import Amazonka.Pinpoint.Types.SimpleEmail
import Amazonka.Pinpoint.Types.SimpleEmailPart
import Amazonka.Pinpoint.Types.StartCondition
import Amazonka.Pinpoint.Types.TagsModel
import Amazonka.Pinpoint.Types.Template
import Amazonka.Pinpoint.Types.TemplateActiveVersionRequest
import Amazonka.Pinpoint.Types.TemplateConfiguration
import Amazonka.Pinpoint.Types.TemplateCreateMessageBody
import Amazonka.Pinpoint.Types.TemplateResponse
import Amazonka.Pinpoint.Types.TemplateVersionResponse
import Amazonka.Pinpoint.Types.TemplateVersionsResponse
import Amazonka.Pinpoint.Types.TemplatesResponse
import Amazonka.Pinpoint.Types.TreatmentResource
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
import Amazonka.Pinpoint.UntagResource
import Amazonka.Pinpoint.UpdateAdmChannel
import Amazonka.Pinpoint.UpdateApnsChannel
import Amazonka.Pinpoint.UpdateApnsSandboxChannel
import Amazonka.Pinpoint.UpdateApnsVoipChannel
import Amazonka.Pinpoint.UpdateApnsVoipSandboxChannel
import Amazonka.Pinpoint.UpdateApplicationSettings
import Amazonka.Pinpoint.UpdateBaiduChannel
import Amazonka.Pinpoint.UpdateCampaign
import Amazonka.Pinpoint.UpdateEmailChannel
import Amazonka.Pinpoint.UpdateEmailTemplate
import Amazonka.Pinpoint.UpdateEndpoint
import Amazonka.Pinpoint.UpdateEndpointsBatch
import Amazonka.Pinpoint.UpdateGcmChannel
import Amazonka.Pinpoint.UpdateInAppTemplate
import Amazonka.Pinpoint.UpdateJourney
import Amazonka.Pinpoint.UpdateJourneyState
import Amazonka.Pinpoint.UpdatePushTemplate
import Amazonka.Pinpoint.UpdateRecommenderConfiguration
import Amazonka.Pinpoint.UpdateSegment
import Amazonka.Pinpoint.UpdateSmsChannel
import Amazonka.Pinpoint.UpdateSmsTemplate
import Amazonka.Pinpoint.UpdateTemplateActiveVersion
import Amazonka.Pinpoint.UpdateVoiceChannel
import Amazonka.Pinpoint.UpdateVoiceTemplate
