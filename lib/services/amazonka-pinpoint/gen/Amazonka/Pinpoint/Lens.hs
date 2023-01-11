{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Pinpoint.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
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
    getApplicationDateRangeKpi_endTime,
    getApplicationDateRangeKpi_nextToken,
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
    getCampaignDateRangeKpi_endTime,
    getCampaignDateRangeKpi_nextToken,
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
    getJourneyDateRangeKpi_endTime,
    getJourneyDateRangeKpi_nextToken,
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
    listTemplates_pageSize,
    listTemplates_prefix,
    listTemplates_templateType,
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

    -- ** SendOTPMessage
    sendOTPMessage_applicationId,
    sendOTPMessage_sendOTPMessageRequestParameters,
    sendOTPMessageResponse_httpStatus,
    sendOTPMessageResponse_messageResponse,

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

    -- ** VerifyOTPMessage
    verifyOTPMessage_applicationId,
    verifyOTPMessage_verifyOTPMessageRequestParameters,
    verifyOTPMessageResponse_httpStatus,
    verifyOTPMessageResponse_verificationResponse,

    -- * Types

    -- ** ADMChannelRequest
    aDMChannelRequest_enabled,
    aDMChannelRequest_clientSecret,
    aDMChannelRequest_clientId,

    -- ** ADMChannelResponse
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

    -- ** ADMMessage
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

    -- ** APNSChannelRequest
    aPNSChannelRequest_bundleId,
    aPNSChannelRequest_certificate,
    aPNSChannelRequest_defaultAuthenticationMethod,
    aPNSChannelRequest_enabled,
    aPNSChannelRequest_privateKey,
    aPNSChannelRequest_teamId,
    aPNSChannelRequest_tokenKey,
    aPNSChannelRequest_tokenKeyId,

    -- ** APNSChannelResponse
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

    -- ** APNSMessage
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

    -- ** APNSPushNotificationTemplate
    aPNSPushNotificationTemplate_action,
    aPNSPushNotificationTemplate_body,
    aPNSPushNotificationTemplate_mediaUrl,
    aPNSPushNotificationTemplate_rawContent,
    aPNSPushNotificationTemplate_sound,
    aPNSPushNotificationTemplate_title,
    aPNSPushNotificationTemplate_url,

    -- ** APNSSandboxChannelRequest
    aPNSSandboxChannelRequest_bundleId,
    aPNSSandboxChannelRequest_certificate,
    aPNSSandboxChannelRequest_defaultAuthenticationMethod,
    aPNSSandboxChannelRequest_enabled,
    aPNSSandboxChannelRequest_privateKey,
    aPNSSandboxChannelRequest_teamId,
    aPNSSandboxChannelRequest_tokenKey,
    aPNSSandboxChannelRequest_tokenKeyId,

    -- ** APNSSandboxChannelResponse
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

    -- ** APNSVoipChannelRequest
    aPNSVoipChannelRequest_bundleId,
    aPNSVoipChannelRequest_certificate,
    aPNSVoipChannelRequest_defaultAuthenticationMethod,
    aPNSVoipChannelRequest_enabled,
    aPNSVoipChannelRequest_privateKey,
    aPNSVoipChannelRequest_teamId,
    aPNSVoipChannelRequest_tokenKey,
    aPNSVoipChannelRequest_tokenKeyId,

    -- ** APNSVoipChannelResponse
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

    -- ** APNSVoipSandboxChannelRequest
    aPNSVoipSandboxChannelRequest_bundleId,
    aPNSVoipSandboxChannelRequest_certificate,
    aPNSVoipSandboxChannelRequest_defaultAuthenticationMethod,
    aPNSVoipSandboxChannelRequest_enabled,
    aPNSVoipSandboxChannelRequest_privateKey,
    aPNSVoipSandboxChannelRequest_teamId,
    aPNSVoipSandboxChannelRequest_tokenKey,
    aPNSVoipSandboxChannelRequest_tokenKeyId,

    -- ** APNSVoipSandboxChannelResponse
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

    -- ** ActivitiesResponse
    activitiesResponse_nextToken,
    activitiesResponse_item,

    -- ** Activity
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

    -- ** ActivityResponse
    activityResponse_end,
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

    -- ** AddressConfiguration
    addressConfiguration_bodyOverride,
    addressConfiguration_channelType,
    addressConfiguration_context,
    addressConfiguration_rawContent,
    addressConfiguration_substitutions,
    addressConfiguration_titleOverride,

    -- ** AndroidPushNotificationTemplate
    androidPushNotificationTemplate_action,
    androidPushNotificationTemplate_body,
    androidPushNotificationTemplate_imageIconUrl,
    androidPushNotificationTemplate_imageUrl,
    androidPushNotificationTemplate_rawContent,
    androidPushNotificationTemplate_smallImageIconUrl,
    androidPushNotificationTemplate_sound,
    androidPushNotificationTemplate_title,
    androidPushNotificationTemplate_url,

    -- ** ApplicationDateRangeKpiResponse
    applicationDateRangeKpiResponse_nextToken,
    applicationDateRangeKpiResponse_kpiResult,
    applicationDateRangeKpiResponse_kpiName,
    applicationDateRangeKpiResponse_endTime,
    applicationDateRangeKpiResponse_startTime,
    applicationDateRangeKpiResponse_applicationId,

    -- ** ApplicationResponse
    applicationResponse_creationDate,
    applicationResponse_tags,
    applicationResponse_id,
    applicationResponse_arn,
    applicationResponse_name,

    -- ** ApplicationSettingsResource
    applicationSettingsResource_campaignHook,
    applicationSettingsResource_lastModifiedDate,
    applicationSettingsResource_limits,
    applicationSettingsResource_quietTime,
    applicationSettingsResource_applicationId,

    -- ** ApplicationsResponse
    applicationsResponse_item,
    applicationsResponse_nextToken,

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

    -- ** BaiduMessage
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
    campaignEmailMessage_body,
    campaignEmailMessage_fromAddress,
    campaignEmailMessage_htmlBody,
    campaignEmailMessage_title,

    -- ** CampaignEventFilter
    campaignEventFilter_filterType,
    campaignEventFilter_dimensions,

    -- ** CampaignHook
    campaignHook_lambdaFunctionName,
    campaignHook_mode,
    campaignHook_webUrl,

    -- ** CampaignInAppMessage
    campaignInAppMessage_body,
    campaignInAppMessage_content,
    campaignInAppMessage_customConfig,
    campaignInAppMessage_layout,

    -- ** CampaignLimits
    campaignLimits_daily,
    campaignLimits_maximumDuration,
    campaignLimits_messagesPerSecond,
    campaignLimits_session,
    campaignLimits_total,

    -- ** CampaignResponse
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

    -- ** CampaignSmsMessage
    campaignSmsMessage_body,
    campaignSmsMessage_entityId,
    campaignSmsMessage_messageType,
    campaignSmsMessage_originationNumber,
    campaignSmsMessage_senderId,
    campaignSmsMessage_templateId,

    -- ** CampaignState
    campaignState_campaignStatus,

    -- ** CampaignsResponse
    campaignsResponse_nextToken,
    campaignsResponse_item,

    -- ** ChannelResponse
    channelResponse_applicationId,
    channelResponse_creationDate,
    channelResponse_enabled,
    channelResponse_hasCredential,
    channelResponse_id,
    channelResponse_isArchived,
    channelResponse_lastModifiedBy,
    channelResponse_lastModifiedDate,
    channelResponse_version,

    -- ** ChannelsResponse
    channelsResponse_channels,

    -- ** ClosedDays
    closedDays_custom,
    closedDays_email,
    closedDays_push,
    closedDays_sms,
    closedDays_voice,

    -- ** ClosedDaysRule
    closedDaysRule_endDateTime,
    closedDaysRule_name,
    closedDaysRule_startDateTime,

    -- ** Condition
    condition_conditions,
    condition_operator,

    -- ** ConditionalSplitActivity
    conditionalSplitActivity_condition,
    conditionalSplitActivity_evaluationWaitTime,
    conditionalSplitActivity_falseActivity,
    conditionalSplitActivity_trueActivity,

    -- ** ContactCenterActivity
    contactCenterActivity_nextActivity,

    -- ** CreateApplicationRequest
    createApplicationRequest_tags,
    createApplicationRequest_name,

    -- ** CreateRecommenderConfiguration
    createRecommenderConfiguration_attributes,
    createRecommenderConfiguration_description,
    createRecommenderConfiguration_name,
    createRecommenderConfiguration_recommendationProviderIdType,
    createRecommenderConfiguration_recommendationTransformerUri,
    createRecommenderConfiguration_recommendationsDisplayName,
    createRecommenderConfiguration_recommendationsPerMessage,
    createRecommenderConfiguration_recommendationProviderUri,
    createRecommenderConfiguration_recommendationProviderRoleArn,

    -- ** CreateTemplateMessageBody
    createTemplateMessageBody_arn,
    createTemplateMessageBody_message,
    createTemplateMessageBody_requestID,

    -- ** CustomDeliveryConfiguration
    customDeliveryConfiguration_endpointTypes,
    customDeliveryConfiguration_deliveryUri,

    -- ** CustomMessageActivity
    customMessageActivity_deliveryUri,
    customMessageActivity_endpointTypes,
    customMessageActivity_messageConfig,
    customMessageActivity_nextActivity,
    customMessageActivity_templateName,
    customMessageActivity_templateVersion,

    -- ** DefaultButtonConfiguration
    defaultButtonConfiguration_backgroundColor,
    defaultButtonConfiguration_borderRadius,
    defaultButtonConfiguration_link,
    defaultButtonConfiguration_textColor,
    defaultButtonConfiguration_buttonAction,
    defaultButtonConfiguration_text,

    -- ** DefaultMessage
    defaultMessage_body,
    defaultMessage_substitutions,

    -- ** DefaultPushNotificationMessage
    defaultPushNotificationMessage_action,
    defaultPushNotificationMessage_body,
    defaultPushNotificationMessage_data,
    defaultPushNotificationMessage_silentPush,
    defaultPushNotificationMessage_substitutions,
    defaultPushNotificationMessage_title,
    defaultPushNotificationMessage_url,

    -- ** DefaultPushNotificationTemplate
    defaultPushNotificationTemplate_action,
    defaultPushNotificationTemplate_body,
    defaultPushNotificationTemplate_sound,
    defaultPushNotificationTemplate_title,
    defaultPushNotificationTemplate_url,

    -- ** DirectMessageConfiguration
    directMessageConfiguration_aDMMessage,
    directMessageConfiguration_aPNSMessage,
    directMessageConfiguration_baiduMessage,
    directMessageConfiguration_defaultMessage,
    directMessageConfiguration_defaultPushNotificationMessage,
    directMessageConfiguration_emailMessage,
    directMessageConfiguration_gCMMessage,
    directMessageConfiguration_sMSMessage,
    directMessageConfiguration_voiceMessage,

    -- ** EmailChannelRequest
    emailChannelRequest_configurationSet,
    emailChannelRequest_enabled,
    emailChannelRequest_roleArn,
    emailChannelRequest_fromAddress,
    emailChannelRequest_identity,

    -- ** EmailChannelResponse
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

    -- ** EmailMessage
    emailMessage_body,
    emailMessage_feedbackForwardingAddress,
    emailMessage_fromAddress,
    emailMessage_rawEmail,
    emailMessage_replyToAddresses,
    emailMessage_simpleEmail,
    emailMessage_substitutions,

    -- ** EmailMessageActivity
    emailMessageActivity_messageConfig,
    emailMessageActivity_nextActivity,
    emailMessageActivity_templateName,
    emailMessageActivity_templateVersion,

    -- ** EmailTemplateRequest
    emailTemplateRequest_defaultSubstitutions,
    emailTemplateRequest_htmlPart,
    emailTemplateRequest_recommenderId,
    emailTemplateRequest_subject,
    emailTemplateRequest_templateDescription,
    emailTemplateRequest_textPart,
    emailTemplateRequest_tags,

    -- ** EmailTemplateResponse
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

    -- ** EndpointBatchItem
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

    -- ** EndpointBatchRequest
    endpointBatchRequest_item,

    -- ** EndpointDemographic
    endpointDemographic_appVersion,
    endpointDemographic_locale,
    endpointDemographic_make,
    endpointDemographic_model,
    endpointDemographic_modelVersion,
    endpointDemographic_platform,
    endpointDemographic_platformVersion,
    endpointDemographic_timezone,

    -- ** EndpointItemResponse
    endpointItemResponse_message,
    endpointItemResponse_statusCode,

    -- ** EndpointLocation
    endpointLocation_city,
    endpointLocation_country,
    endpointLocation_latitude,
    endpointLocation_longitude,
    endpointLocation_postalCode,
    endpointLocation_region,

    -- ** EndpointMessageResult
    endpointMessageResult_address,
    endpointMessageResult_messageId,
    endpointMessageResult_statusMessage,
    endpointMessageResult_updatedToken,
    endpointMessageResult_deliveryStatus,
    endpointMessageResult_statusCode,

    -- ** EndpointRequest
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

    -- ** EndpointResponse
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

    -- ** EndpointSendConfiguration
    endpointSendConfiguration_bodyOverride,
    endpointSendConfiguration_context,
    endpointSendConfiguration_rawContent,
    endpointSendConfiguration_substitutions,
    endpointSendConfiguration_titleOverride,

    -- ** EndpointUser
    endpointUser_userAttributes,
    endpointUser_userId,

    -- ** EndpointsResponse
    endpointsResponse_item,

    -- ** Event
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

    -- ** EventCondition
    eventCondition_dimensions,
    eventCondition_messageActivity,

    -- ** EventDimensions
    eventDimensions_attributes,
    eventDimensions_eventType,
    eventDimensions_metrics,

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
    eventStream_externalId,
    eventStream_lastModifiedDate,
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
    exportJobRequest_segmentId,
    exportJobRequest_segmentVersion,
    exportJobRequest_s3UrlPrefix,
    exportJobRequest_roleArn,

    -- ** ExportJobResource
    exportJobResource_segmentId,
    exportJobResource_segmentVersion,
    exportJobResource_s3UrlPrefix,
    exportJobResource_roleArn,

    -- ** ExportJobResponse
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

    -- ** ExportJobsResponse
    exportJobsResponse_nextToken,
    exportJobsResponse_item,

    -- ** GCMChannelRequest
    gCMChannelRequest_enabled,
    gCMChannelRequest_apiKey,

    -- ** GCMChannelResponse
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

    -- ** GCMMessage
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
    importJobRequest_externalId,
    importJobRequest_registerEndpoints,
    importJobRequest_segmentId,
    importJobRequest_segmentName,
    importJobRequest_format,
    importJobRequest_s3Url,
    importJobRequest_roleArn,

    -- ** ImportJobResource
    importJobResource_defineSegment,
    importJobResource_externalId,
    importJobResource_registerEndpoints,
    importJobResource_segmentId,
    importJobResource_segmentName,
    importJobResource_format,
    importJobResource_s3Url,
    importJobResource_roleArn,

    -- ** ImportJobResponse
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

    -- ** ImportJobsResponse
    importJobsResponse_nextToken,
    importJobsResponse_item,

    -- ** InAppCampaignSchedule
    inAppCampaignSchedule_endDate,
    inAppCampaignSchedule_eventFilter,
    inAppCampaignSchedule_quietTime,

    -- ** InAppMessage
    inAppMessage_content,
    inAppMessage_customConfig,
    inAppMessage_layout,

    -- ** InAppMessageBodyConfig
    inAppMessageBodyConfig_alignment,
    inAppMessageBodyConfig_textColor,
    inAppMessageBodyConfig_body,

    -- ** InAppMessageButton
    inAppMessageButton_android,
    inAppMessageButton_defaultConfig,
    inAppMessageButton_ios,
    inAppMessageButton_web,

    -- ** InAppMessageCampaign
    inAppMessageCampaign_campaignId,
    inAppMessageCampaign_dailyCap,
    inAppMessageCampaign_inAppMessage,
    inAppMessageCampaign_priority,
    inAppMessageCampaign_schedule,
    inAppMessageCampaign_sessionCap,
    inAppMessageCampaign_totalCap,
    inAppMessageCampaign_treatmentId,

    -- ** InAppMessageContent
    inAppMessageContent_backgroundColor,
    inAppMessageContent_bodyConfig,
    inAppMessageContent_headerConfig,
    inAppMessageContent_imageUrl,
    inAppMessageContent_primaryBtn,
    inAppMessageContent_secondaryBtn,

    -- ** InAppMessageHeaderConfig
    inAppMessageHeaderConfig_alignment,
    inAppMessageHeaderConfig_header,
    inAppMessageHeaderConfig_textColor,

    -- ** InAppMessagesResponse
    inAppMessagesResponse_inAppMessageCampaigns,

    -- ** InAppTemplateRequest
    inAppTemplateRequest_content,
    inAppTemplateRequest_customConfig,
    inAppTemplateRequest_layout,
    inAppTemplateRequest_templateDescription,
    inAppTemplateRequest_tags,

    -- ** InAppTemplateResponse
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

    -- ** ItemResponse
    itemResponse_endpointItemResponse,
    itemResponse_eventsItemResponse,

    -- ** JourneyChannelSettings
    journeyChannelSettings_connectCampaignArn,
    journeyChannelSettings_connectCampaignExecutionRoleArn,

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
    journeyLimits_dailyCap,
    journeyLimits_endpointReentryCap,
    journeyLimits_endpointReentryInterval,
    journeyLimits_messagesPerSecond,

    -- ** JourneyPushMessage
    journeyPushMessage_timeToLive,

    -- ** JourneyResponse
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
    journeyResponse_waitForQuietTime,
    journeyResponse_tags,
    journeyResponse_name,
    journeyResponse_id,
    journeyResponse_applicationId,

    -- ** JourneySMSMessage
    journeySMSMessage_entityId,
    journeySMSMessage_messageType,
    journeySMSMessage_originationNumber,
    journeySMSMessage_senderId,
    journeySMSMessage_templateId,

    -- ** JourneySchedule
    journeySchedule_endTime,
    journeySchedule_startTime,
    journeySchedule_timezone,

    -- ** JourneyStateRequest
    journeyStateRequest_state,

    -- ** JourneysResponse
    journeysResponse_nextToken,
    journeysResponse_item,

    -- ** ListRecommenderConfigurationsResponse
    listRecommenderConfigurationsResponse_nextToken,
    listRecommenderConfigurationsResponse_item,

    -- ** Message
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

    -- ** MessageBody
    messageBody_message,
    messageBody_requestID,

    -- ** MessageConfiguration
    messageConfiguration_aDMMessage,
    messageConfiguration_aPNSMessage,
    messageConfiguration_baiduMessage,
    messageConfiguration_customMessage,
    messageConfiguration_defaultMessage,
    messageConfiguration_emailMessage,
    messageConfiguration_gCMMessage,
    messageConfiguration_inAppMessage,
    messageConfiguration_sMSMessage,

    -- ** MessageRequest
    messageRequest_addresses,
    messageRequest_context,
    messageRequest_endpoints,
    messageRequest_templateConfiguration,
    messageRequest_traceId,
    messageRequest_messageConfiguration,

    -- ** MessageResponse
    messageResponse_endpointResult,
    messageResponse_requestId,
    messageResponse_result,
    messageResponse_applicationId,

    -- ** MessageResult
    messageResult_messageId,
    messageResult_statusMessage,
    messageResult_updatedToken,
    messageResult_deliveryStatus,
    messageResult_statusCode,

    -- ** MetricDimension
    metricDimension_comparisonOperator,
    metricDimension_value,

    -- ** MultiConditionalBranch
    multiConditionalBranch_condition,
    multiConditionalBranch_nextActivity,

    -- ** MultiConditionalSplitActivity
    multiConditionalSplitActivity_branches,
    multiConditionalSplitActivity_defaultActivity,
    multiConditionalSplitActivity_evaluationWaitTime,

    -- ** NumberValidateRequest
    numberValidateRequest_isoCountryCode,
    numberValidateRequest_phoneNumber,

    -- ** NumberValidateResponse
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

    -- ** OpenHours
    openHours_custom,
    openHours_email,
    openHours_push,
    openHours_sms,
    openHours_voice,

    -- ** OpenHoursRule
    openHoursRule_endTime,
    openHoursRule_startTime,

    -- ** OverrideButtonConfiguration
    overrideButtonConfiguration_link,
    overrideButtonConfiguration_buttonAction,

    -- ** PublicEndpoint
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

    -- ** PushMessageActivity
    pushMessageActivity_messageConfig,
    pushMessageActivity_nextActivity,
    pushMessageActivity_templateName,
    pushMessageActivity_templateVersion,

    -- ** PushNotificationTemplateRequest
    pushNotificationTemplateRequest_adm,
    pushNotificationTemplateRequest_apns,
    pushNotificationTemplateRequest_baidu,
    pushNotificationTemplateRequest_default,
    pushNotificationTemplateRequest_defaultSubstitutions,
    pushNotificationTemplateRequest_gcm,
    pushNotificationTemplateRequest_recommenderId,
    pushNotificationTemplateRequest_templateDescription,
    pushNotificationTemplateRequest_tags,

    -- ** PushNotificationTemplateResponse
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

    -- ** QuietTime
    quietTime_end,
    quietTime_start,

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

    -- ** ResultRow
    resultRow_groupedBys,
    resultRow_values,

    -- ** ResultRowValue
    resultRowValue_type,
    resultRowValue_value,
    resultRowValue_key,

    -- ** SMSChannelRequest
    sMSChannelRequest_enabled,
    sMSChannelRequest_senderId,
    sMSChannelRequest_shortCode,

    -- ** SMSChannelResponse
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

    -- ** SMSMessage
    sMSMessage_body,
    sMSMessage_entityId,
    sMSMessage_keyword,
    sMSMessage_mediaUrl,
    sMSMessage_messageType,
    sMSMessage_originationNumber,
    sMSMessage_senderId,
    sMSMessage_substitutions,
    sMSMessage_templateId,

    -- ** SMSMessageActivity
    sMSMessageActivity_messageConfig,
    sMSMessageActivity_nextActivity,
    sMSMessageActivity_templateName,
    sMSMessageActivity_templateVersion,

    -- ** SMSTemplateRequest
    sMSTemplateRequest_body,
    sMSTemplateRequest_defaultSubstitutions,
    sMSTemplateRequest_recommenderId,
    sMSTemplateRequest_templateDescription,
    sMSTemplateRequest_tags,

    -- ** SMSTemplateResponse
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

    -- ** Schedule
    schedule_endTime,
    schedule_eventFilter,
    schedule_frequency,
    schedule_isLocalTime,
    schedule_quietTime,
    schedule_timezone,
    schedule_startTime,

    -- ** SegmentBehaviors
    segmentBehaviors_recency,

    -- ** SegmentCondition
    segmentCondition_segmentId,

    -- ** SegmentDemographics
    segmentDemographics_appVersion,
    segmentDemographics_channel,
    segmentDemographics_deviceType,
    segmentDemographics_make,
    segmentDemographics_model,
    segmentDemographics_platform,

    -- ** SegmentDimensions
    segmentDimensions_attributes,
    segmentDimensions_behavior,
    segmentDimensions_demographic,
    segmentDimensions_location,
    segmentDimensions_metrics,
    segmentDimensions_userAttributes,

    -- ** SegmentGroup
    segmentGroup_dimensions,
    segmentGroup_sourceSegments,
    segmentGroup_sourceType,
    segmentGroup_type,

    -- ** SegmentGroupList
    segmentGroupList_groups,
    segmentGroupList_include,

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

    -- ** SegmentsResponse
    segmentsResponse_nextToken,
    segmentsResponse_item,

    -- ** SendOTPMessageRequestParameters
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
    simpleCondition_eventCondition,
    simpleCondition_segmentCondition,
    simpleCondition_segmentDimensions,

    -- ** SimpleEmail
    simpleEmail_htmlPart,
    simpleEmail_subject,
    simpleEmail_textPart,

    -- ** SimpleEmailPart
    simpleEmailPart_charset,
    simpleEmailPart_data,

    -- ** StartCondition
    startCondition_description,
    startCondition_eventStartCondition,
    startCondition_segmentStartCondition,

    -- ** TagsModel
    tagsModel_tags,

    -- ** Template
    template_name,
    template_version,

    -- ** TemplateActiveVersionRequest
    templateActiveVersionRequest_version,

    -- ** TemplateConfiguration
    templateConfiguration_emailTemplate,
    templateConfiguration_pushTemplate,
    templateConfiguration_sMSTemplate,
    templateConfiguration_voiceTemplate,

    -- ** TemplateCreateMessageBody
    templateCreateMessageBody_arn,
    templateCreateMessageBody_message,
    templateCreateMessageBody_requestID,

    -- ** TemplateResponse
    templateResponse_arn,
    templateResponse_defaultSubstitutions,
    templateResponse_templateDescription,
    templateResponse_version,
    templateResponse_tags,
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
    treatmentResource_customDeliveryConfiguration,
    treatmentResource_messageConfiguration,
    treatmentResource_schedule,
    treatmentResource_state,
    treatmentResource_templateConfiguration,
    treatmentResource_treatmentDescription,
    treatmentResource_treatmentName,
    treatmentResource_id,
    treatmentResource_sizePercent,

    -- ** UpdateAttributesRequest
    updateAttributesRequest_blacklist,

    -- ** UpdateRecommenderConfiguration
    updateRecommenderConfiguration_attributes,
    updateRecommenderConfiguration_description,
    updateRecommenderConfiguration_name,
    updateRecommenderConfiguration_recommendationProviderIdType,
    updateRecommenderConfiguration_recommendationTransformerUri,
    updateRecommenderConfiguration_recommendationsDisplayName,
    updateRecommenderConfiguration_recommendationsPerMessage,
    updateRecommenderConfiguration_recommendationProviderUri,
    updateRecommenderConfiguration_recommendationProviderRoleArn,

    -- ** VerificationResponse
    verificationResponse_valid,

    -- ** VerifyOTPMessageRequestParameters
    verifyOTPMessageRequestParameters_referenceId,
    verifyOTPMessageRequestParameters_otp,
    verifyOTPMessageRequestParameters_destinationIdentity,

    -- ** VoiceChannelRequest
    voiceChannelRequest_enabled,

    -- ** VoiceChannelResponse
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

    -- ** VoiceMessage
    voiceMessage_body,
    voiceMessage_languageCode,
    voiceMessage_originationNumber,
    voiceMessage_substitutions,
    voiceMessage_voiceId,

    -- ** VoiceTemplateRequest
    voiceTemplateRequest_body,
    voiceTemplateRequest_defaultSubstitutions,
    voiceTemplateRequest_languageCode,
    voiceTemplateRequest_templateDescription,
    voiceTemplateRequest_voiceId,
    voiceTemplateRequest_tags,

    -- ** VoiceTemplateResponse
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

    -- ** WaitActivity
    waitActivity_nextActivity,
    waitActivity_waitTime,

    -- ** WaitTime
    waitTime_waitFor,
    waitTime_waitUntil,

    -- ** WriteApplicationSettingsRequest
    writeApplicationSettingsRequest_campaignHook,
    writeApplicationSettingsRequest_cloudWatchMetricsEnabled,
    writeApplicationSettingsRequest_eventTaggingEnabled,
    writeApplicationSettingsRequest_limits,
    writeApplicationSettingsRequest_quietTime,

    -- ** WriteCampaignRequest
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

    -- ** WriteEventStream
    writeEventStream_roleArn,
    writeEventStream_destinationStreamArn,

    -- ** WriteJourneyRequest
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
    writeJourneyRequest_waitForQuietTime,
    writeJourneyRequest_name,

    -- ** WriteSegmentRequest
    writeSegmentRequest_dimensions,
    writeSegmentRequest_name,
    writeSegmentRequest_segmentGroups,
    writeSegmentRequest_tags,

    -- ** WriteTreatmentResource
    writeTreatmentResource_customDeliveryConfiguration,
    writeTreatmentResource_messageConfiguration,
    writeTreatmentResource_schedule,
    writeTreatmentResource_templateConfiguration,
    writeTreatmentResource_treatmentDescription,
    writeTreatmentResource_treatmentName,
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
import Amazonka.Pinpoint.SendOTPMessage
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
import Amazonka.Pinpoint.Types.OpenHours
import Amazonka.Pinpoint.Types.OpenHoursRule
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
import Amazonka.Pinpoint.Types.SendOTPMessageRequestParameters
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
import Amazonka.Pinpoint.VerifyOTPMessage
