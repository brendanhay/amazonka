{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.Pinpoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2016-12-01@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon Pinpoint makes it easy to run targeted campaigns to drive user
-- engagement in mobile apps. Amazon Pinpoint helps you understand user
-- behavior, define which users to target, determine which messages to
-- send, schedule the best time to deliver the messages, and then track the
-- results of your campaign.
--
-- Targeted push notifications based on app usage trends and user behavior
-- have become a popular approach for mobile app user engagement because
-- response rates are often several times higher than tradition email
-- marketing campaigns. By using targeted push notifications, you can
-- increase message relevance and effectiveness, measure engagement, and
-- continually improve your campaigns.
--
-- Getting started with Amazon Pinpoint is easy. First, AWS Mobile Hub
-- guides you through the process to integrate the AWS Mobile SDK with your
-- app. Next, you define your target segments, campaign message, and
-- specify the delivery schedule. Once your campaign is running, Pinpoint
-- provides metrics so you can run analytics and track the impact of your
-- campaign.
--
-- With Amazon Pinpoint, there are no upfront setup costs, and no fixed
-- monthly cost. You only pay for the number of users your campaign
-- targets, the messages you send, and the events you collect, so you can
-- start small and scale as your application grows.
module Amazonka.Pinpoint
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** NotFoundException
    _NotFoundException,

    -- ** InternalServerErrorException
    _InternalServerErrorException,

    -- ** ForbiddenException
    _ForbiddenException,

    -- ** ConflictException
    _ConflictException,

    -- ** MethodNotAllowedException
    _MethodNotAllowedException,

    -- ** PayloadTooLargeException
    _PayloadTooLargeException,

    -- ** BadRequestException
    _BadRequestException,

    -- ** TooManyRequestsException
    _TooManyRequestsException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateApp
    CreateApp (CreateApp'),
    newCreateApp,
    CreateAppResponse (CreateAppResponse'),
    newCreateAppResponse,

    -- ** CreateCampaign
    CreateCampaign (CreateCampaign'),
    newCreateCampaign,
    CreateCampaignResponse (CreateCampaignResponse'),
    newCreateCampaignResponse,

    -- ** CreateEmailTemplate
    CreateEmailTemplate (CreateEmailTemplate'),
    newCreateEmailTemplate,
    CreateEmailTemplateResponse (CreateEmailTemplateResponse'),
    newCreateEmailTemplateResponse,

    -- ** CreateExportJob
    CreateExportJob (CreateExportJob'),
    newCreateExportJob,
    CreateExportJobResponse (CreateExportJobResponse'),
    newCreateExportJobResponse,

    -- ** CreateImportJob
    CreateImportJob (CreateImportJob'),
    newCreateImportJob,
    CreateImportJobResponse (CreateImportJobResponse'),
    newCreateImportJobResponse,

    -- ** CreateInAppTemplate
    CreateInAppTemplate (CreateInAppTemplate'),
    newCreateInAppTemplate,
    CreateInAppTemplateResponse (CreateInAppTemplateResponse'),
    newCreateInAppTemplateResponse,

    -- ** CreateJourney
    CreateJourney (CreateJourney'),
    newCreateJourney,
    CreateJourneyResponse (CreateJourneyResponse'),
    newCreateJourneyResponse,

    -- ** CreatePushTemplate
    CreatePushTemplate (CreatePushTemplate'),
    newCreatePushTemplate,
    CreatePushTemplateResponse (CreatePushTemplateResponse'),
    newCreatePushTemplateResponse,

    -- ** CreateRecommenderConfiguration
    CreateRecommenderConfiguration' (CreateRecommenderConfiguration''),
    newCreateRecommenderConfiguration',
    CreateRecommenderConfigurationResponse (CreateRecommenderConfigurationResponse'),
    newCreateRecommenderConfigurationResponse,

    -- ** CreateSegment
    CreateSegment (CreateSegment'),
    newCreateSegment,
    CreateSegmentResponse (CreateSegmentResponse'),
    newCreateSegmentResponse,

    -- ** CreateSmsTemplate
    CreateSmsTemplate (CreateSmsTemplate'),
    newCreateSmsTemplate,
    CreateSmsTemplateResponse (CreateSmsTemplateResponse'),
    newCreateSmsTemplateResponse,

    -- ** CreateVoiceTemplate
    CreateVoiceTemplate (CreateVoiceTemplate'),
    newCreateVoiceTemplate,
    CreateVoiceTemplateResponse (CreateVoiceTemplateResponse'),
    newCreateVoiceTemplateResponse,

    -- ** DeleteAdmChannel
    DeleteAdmChannel (DeleteAdmChannel'),
    newDeleteAdmChannel,
    DeleteAdmChannelResponse (DeleteAdmChannelResponse'),
    newDeleteAdmChannelResponse,

    -- ** DeleteApnsChannel
    DeleteApnsChannel (DeleteApnsChannel'),
    newDeleteApnsChannel,
    DeleteApnsChannelResponse (DeleteApnsChannelResponse'),
    newDeleteApnsChannelResponse,

    -- ** DeleteApnsSandboxChannel
    DeleteApnsSandboxChannel (DeleteApnsSandboxChannel'),
    newDeleteApnsSandboxChannel,
    DeleteApnsSandboxChannelResponse (DeleteApnsSandboxChannelResponse'),
    newDeleteApnsSandboxChannelResponse,

    -- ** DeleteApnsVoipChannel
    DeleteApnsVoipChannel (DeleteApnsVoipChannel'),
    newDeleteApnsVoipChannel,
    DeleteApnsVoipChannelResponse (DeleteApnsVoipChannelResponse'),
    newDeleteApnsVoipChannelResponse,

    -- ** DeleteApnsVoipSandboxChannel
    DeleteApnsVoipSandboxChannel (DeleteApnsVoipSandboxChannel'),
    newDeleteApnsVoipSandboxChannel,
    DeleteApnsVoipSandboxChannelResponse (DeleteApnsVoipSandboxChannelResponse'),
    newDeleteApnsVoipSandboxChannelResponse,

    -- ** DeleteApp
    DeleteApp (DeleteApp'),
    newDeleteApp,
    DeleteAppResponse (DeleteAppResponse'),
    newDeleteAppResponse,

    -- ** DeleteBaiduChannel
    DeleteBaiduChannel (DeleteBaiduChannel'),
    newDeleteBaiduChannel,
    DeleteBaiduChannelResponse (DeleteBaiduChannelResponse'),
    newDeleteBaiduChannelResponse,

    -- ** DeleteCampaign
    DeleteCampaign (DeleteCampaign'),
    newDeleteCampaign,
    DeleteCampaignResponse (DeleteCampaignResponse'),
    newDeleteCampaignResponse,

    -- ** DeleteEmailChannel
    DeleteEmailChannel (DeleteEmailChannel'),
    newDeleteEmailChannel,
    DeleteEmailChannelResponse (DeleteEmailChannelResponse'),
    newDeleteEmailChannelResponse,

    -- ** DeleteEmailTemplate
    DeleteEmailTemplate (DeleteEmailTemplate'),
    newDeleteEmailTemplate,
    DeleteEmailTemplateResponse (DeleteEmailTemplateResponse'),
    newDeleteEmailTemplateResponse,

    -- ** DeleteEndpoint
    DeleteEndpoint (DeleteEndpoint'),
    newDeleteEndpoint,
    DeleteEndpointResponse (DeleteEndpointResponse'),
    newDeleteEndpointResponse,

    -- ** DeleteEventStream
    DeleteEventStream (DeleteEventStream'),
    newDeleteEventStream,
    DeleteEventStreamResponse (DeleteEventStreamResponse'),
    newDeleteEventStreamResponse,

    -- ** DeleteGcmChannel
    DeleteGcmChannel (DeleteGcmChannel'),
    newDeleteGcmChannel,
    DeleteGcmChannelResponse (DeleteGcmChannelResponse'),
    newDeleteGcmChannelResponse,

    -- ** DeleteInAppTemplate
    DeleteInAppTemplate (DeleteInAppTemplate'),
    newDeleteInAppTemplate,
    DeleteInAppTemplateResponse (DeleteInAppTemplateResponse'),
    newDeleteInAppTemplateResponse,

    -- ** DeleteJourney
    DeleteJourney (DeleteJourney'),
    newDeleteJourney,
    DeleteJourneyResponse (DeleteJourneyResponse'),
    newDeleteJourneyResponse,

    -- ** DeletePushTemplate
    DeletePushTemplate (DeletePushTemplate'),
    newDeletePushTemplate,
    DeletePushTemplateResponse (DeletePushTemplateResponse'),
    newDeletePushTemplateResponse,

    -- ** DeleteRecommenderConfiguration
    DeleteRecommenderConfiguration (DeleteRecommenderConfiguration'),
    newDeleteRecommenderConfiguration,
    DeleteRecommenderConfigurationResponse (DeleteRecommenderConfigurationResponse'),
    newDeleteRecommenderConfigurationResponse,

    -- ** DeleteSegment
    DeleteSegment (DeleteSegment'),
    newDeleteSegment,
    DeleteSegmentResponse (DeleteSegmentResponse'),
    newDeleteSegmentResponse,

    -- ** DeleteSmsChannel
    DeleteSmsChannel (DeleteSmsChannel'),
    newDeleteSmsChannel,
    DeleteSmsChannelResponse (DeleteSmsChannelResponse'),
    newDeleteSmsChannelResponse,

    -- ** DeleteSmsTemplate
    DeleteSmsTemplate (DeleteSmsTemplate'),
    newDeleteSmsTemplate,
    DeleteSmsTemplateResponse (DeleteSmsTemplateResponse'),
    newDeleteSmsTemplateResponse,

    -- ** DeleteUserEndpoints
    DeleteUserEndpoints (DeleteUserEndpoints'),
    newDeleteUserEndpoints,
    DeleteUserEndpointsResponse (DeleteUserEndpointsResponse'),
    newDeleteUserEndpointsResponse,

    -- ** DeleteVoiceChannel
    DeleteVoiceChannel (DeleteVoiceChannel'),
    newDeleteVoiceChannel,
    DeleteVoiceChannelResponse (DeleteVoiceChannelResponse'),
    newDeleteVoiceChannelResponse,

    -- ** DeleteVoiceTemplate
    DeleteVoiceTemplate (DeleteVoiceTemplate'),
    newDeleteVoiceTemplate,
    DeleteVoiceTemplateResponse (DeleteVoiceTemplateResponse'),
    newDeleteVoiceTemplateResponse,

    -- ** GetAdmChannel
    GetAdmChannel (GetAdmChannel'),
    newGetAdmChannel,
    GetAdmChannelResponse (GetAdmChannelResponse'),
    newGetAdmChannelResponse,

    -- ** GetApnsChannel
    GetApnsChannel (GetApnsChannel'),
    newGetApnsChannel,
    GetApnsChannelResponse (GetApnsChannelResponse'),
    newGetApnsChannelResponse,

    -- ** GetApnsSandboxChannel
    GetApnsSandboxChannel (GetApnsSandboxChannel'),
    newGetApnsSandboxChannel,
    GetApnsSandboxChannelResponse (GetApnsSandboxChannelResponse'),
    newGetApnsSandboxChannelResponse,

    -- ** GetApnsVoipChannel
    GetApnsVoipChannel (GetApnsVoipChannel'),
    newGetApnsVoipChannel,
    GetApnsVoipChannelResponse (GetApnsVoipChannelResponse'),
    newGetApnsVoipChannelResponse,

    -- ** GetApnsVoipSandboxChannel
    GetApnsVoipSandboxChannel (GetApnsVoipSandboxChannel'),
    newGetApnsVoipSandboxChannel,
    GetApnsVoipSandboxChannelResponse (GetApnsVoipSandboxChannelResponse'),
    newGetApnsVoipSandboxChannelResponse,

    -- ** GetApp
    GetApp (GetApp'),
    newGetApp,
    GetAppResponse (GetAppResponse'),
    newGetAppResponse,

    -- ** GetApplicationDateRangeKpi
    GetApplicationDateRangeKpi (GetApplicationDateRangeKpi'),
    newGetApplicationDateRangeKpi,
    GetApplicationDateRangeKpiResponse (GetApplicationDateRangeKpiResponse'),
    newGetApplicationDateRangeKpiResponse,

    -- ** GetApplicationSettings
    GetApplicationSettings (GetApplicationSettings'),
    newGetApplicationSettings,
    GetApplicationSettingsResponse (GetApplicationSettingsResponse'),
    newGetApplicationSettingsResponse,

    -- ** GetApps
    GetApps (GetApps'),
    newGetApps,
    GetAppsResponse (GetAppsResponse'),
    newGetAppsResponse,

    -- ** GetBaiduChannel
    GetBaiduChannel (GetBaiduChannel'),
    newGetBaiduChannel,
    GetBaiduChannelResponse (GetBaiduChannelResponse'),
    newGetBaiduChannelResponse,

    -- ** GetCampaign
    GetCampaign (GetCampaign'),
    newGetCampaign,
    GetCampaignResponse (GetCampaignResponse'),
    newGetCampaignResponse,

    -- ** GetCampaignActivities
    GetCampaignActivities (GetCampaignActivities'),
    newGetCampaignActivities,
    GetCampaignActivitiesResponse (GetCampaignActivitiesResponse'),
    newGetCampaignActivitiesResponse,

    -- ** GetCampaignDateRangeKpi
    GetCampaignDateRangeKpi (GetCampaignDateRangeKpi'),
    newGetCampaignDateRangeKpi,
    GetCampaignDateRangeKpiResponse (GetCampaignDateRangeKpiResponse'),
    newGetCampaignDateRangeKpiResponse,

    -- ** GetCampaignVersion
    GetCampaignVersion (GetCampaignVersion'),
    newGetCampaignVersion,
    GetCampaignVersionResponse (GetCampaignVersionResponse'),
    newGetCampaignVersionResponse,

    -- ** GetCampaignVersions
    GetCampaignVersions (GetCampaignVersions'),
    newGetCampaignVersions,
    GetCampaignVersionsResponse (GetCampaignVersionsResponse'),
    newGetCampaignVersionsResponse,

    -- ** GetCampaigns
    GetCampaigns (GetCampaigns'),
    newGetCampaigns,
    GetCampaignsResponse (GetCampaignsResponse'),
    newGetCampaignsResponse,

    -- ** GetChannels
    GetChannels (GetChannels'),
    newGetChannels,
    GetChannelsResponse (GetChannelsResponse'),
    newGetChannelsResponse,

    -- ** GetEmailChannel
    GetEmailChannel (GetEmailChannel'),
    newGetEmailChannel,
    GetEmailChannelResponse (GetEmailChannelResponse'),
    newGetEmailChannelResponse,

    -- ** GetEmailTemplate
    GetEmailTemplate (GetEmailTemplate'),
    newGetEmailTemplate,
    GetEmailTemplateResponse (GetEmailTemplateResponse'),
    newGetEmailTemplateResponse,

    -- ** GetEndpoint
    GetEndpoint (GetEndpoint'),
    newGetEndpoint,
    GetEndpointResponse (GetEndpointResponse'),
    newGetEndpointResponse,

    -- ** GetEventStream
    GetEventStream (GetEventStream'),
    newGetEventStream,
    GetEventStreamResponse (GetEventStreamResponse'),
    newGetEventStreamResponse,

    -- ** GetExportJob
    GetExportJob (GetExportJob'),
    newGetExportJob,
    GetExportJobResponse (GetExportJobResponse'),
    newGetExportJobResponse,

    -- ** GetExportJobs
    GetExportJobs (GetExportJobs'),
    newGetExportJobs,
    GetExportJobsResponse (GetExportJobsResponse'),
    newGetExportJobsResponse,

    -- ** GetGcmChannel
    GetGcmChannel (GetGcmChannel'),
    newGetGcmChannel,
    GetGcmChannelResponse (GetGcmChannelResponse'),
    newGetGcmChannelResponse,

    -- ** GetImportJob
    GetImportJob (GetImportJob'),
    newGetImportJob,
    GetImportJobResponse (GetImportJobResponse'),
    newGetImportJobResponse,

    -- ** GetImportJobs
    GetImportJobs (GetImportJobs'),
    newGetImportJobs,
    GetImportJobsResponse (GetImportJobsResponse'),
    newGetImportJobsResponse,

    -- ** GetInAppMessages
    GetInAppMessages (GetInAppMessages'),
    newGetInAppMessages,
    GetInAppMessagesResponse (GetInAppMessagesResponse'),
    newGetInAppMessagesResponse,

    -- ** GetInAppTemplate
    GetInAppTemplate (GetInAppTemplate'),
    newGetInAppTemplate,
    GetInAppTemplateResponse (GetInAppTemplateResponse'),
    newGetInAppTemplateResponse,

    -- ** GetJourney
    GetJourney (GetJourney'),
    newGetJourney,
    GetJourneyResponse (GetJourneyResponse'),
    newGetJourneyResponse,

    -- ** GetJourneyDateRangeKpi
    GetJourneyDateRangeKpi (GetJourneyDateRangeKpi'),
    newGetJourneyDateRangeKpi,
    GetJourneyDateRangeKpiResponse (GetJourneyDateRangeKpiResponse'),
    newGetJourneyDateRangeKpiResponse,

    -- ** GetJourneyExecutionActivityMetrics
    GetJourneyExecutionActivityMetrics (GetJourneyExecutionActivityMetrics'),
    newGetJourneyExecutionActivityMetrics,
    GetJourneyExecutionActivityMetricsResponse (GetJourneyExecutionActivityMetricsResponse'),
    newGetJourneyExecutionActivityMetricsResponse,

    -- ** GetJourneyExecutionMetrics
    GetJourneyExecutionMetrics (GetJourneyExecutionMetrics'),
    newGetJourneyExecutionMetrics,
    GetJourneyExecutionMetricsResponse (GetJourneyExecutionMetricsResponse'),
    newGetJourneyExecutionMetricsResponse,

    -- ** GetPushTemplate
    GetPushTemplate (GetPushTemplate'),
    newGetPushTemplate,
    GetPushTemplateResponse (GetPushTemplateResponse'),
    newGetPushTemplateResponse,

    -- ** GetRecommenderConfiguration
    GetRecommenderConfiguration (GetRecommenderConfiguration'),
    newGetRecommenderConfiguration,
    GetRecommenderConfigurationResponse (GetRecommenderConfigurationResponse'),
    newGetRecommenderConfigurationResponse,

    -- ** GetRecommenderConfigurations
    GetRecommenderConfigurations (GetRecommenderConfigurations'),
    newGetRecommenderConfigurations,
    GetRecommenderConfigurationsResponse (GetRecommenderConfigurationsResponse'),
    newGetRecommenderConfigurationsResponse,

    -- ** GetSegment
    GetSegment (GetSegment'),
    newGetSegment,
    GetSegmentResponse (GetSegmentResponse'),
    newGetSegmentResponse,

    -- ** GetSegmentExportJobs
    GetSegmentExportJobs (GetSegmentExportJobs'),
    newGetSegmentExportJobs,
    GetSegmentExportJobsResponse (GetSegmentExportJobsResponse'),
    newGetSegmentExportJobsResponse,

    -- ** GetSegmentImportJobs
    GetSegmentImportJobs (GetSegmentImportJobs'),
    newGetSegmentImportJobs,
    GetSegmentImportJobsResponse (GetSegmentImportJobsResponse'),
    newGetSegmentImportJobsResponse,

    -- ** GetSegmentVersion
    GetSegmentVersion (GetSegmentVersion'),
    newGetSegmentVersion,
    GetSegmentVersionResponse (GetSegmentVersionResponse'),
    newGetSegmentVersionResponse,

    -- ** GetSegmentVersions
    GetSegmentVersions (GetSegmentVersions'),
    newGetSegmentVersions,
    GetSegmentVersionsResponse (GetSegmentVersionsResponse'),
    newGetSegmentVersionsResponse,

    -- ** GetSegments
    GetSegments (GetSegments'),
    newGetSegments,
    GetSegmentsResponse (GetSegmentsResponse'),
    newGetSegmentsResponse,

    -- ** GetSmsChannel
    GetSmsChannel (GetSmsChannel'),
    newGetSmsChannel,
    GetSmsChannelResponse (GetSmsChannelResponse'),
    newGetSmsChannelResponse,

    -- ** GetSmsTemplate
    GetSmsTemplate (GetSmsTemplate'),
    newGetSmsTemplate,
    GetSmsTemplateResponse (GetSmsTemplateResponse'),
    newGetSmsTemplateResponse,

    -- ** GetUserEndpoints
    GetUserEndpoints (GetUserEndpoints'),
    newGetUserEndpoints,
    GetUserEndpointsResponse (GetUserEndpointsResponse'),
    newGetUserEndpointsResponse,

    -- ** GetVoiceChannel
    GetVoiceChannel (GetVoiceChannel'),
    newGetVoiceChannel,
    GetVoiceChannelResponse (GetVoiceChannelResponse'),
    newGetVoiceChannelResponse,

    -- ** GetVoiceTemplate
    GetVoiceTemplate (GetVoiceTemplate'),
    newGetVoiceTemplate,
    GetVoiceTemplateResponse (GetVoiceTemplateResponse'),
    newGetVoiceTemplateResponse,

    -- ** ListJourneys
    ListJourneys (ListJourneys'),
    newListJourneys,
    ListJourneysResponse (ListJourneysResponse'),
    newListJourneysResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ListTemplateVersions
    ListTemplateVersions (ListTemplateVersions'),
    newListTemplateVersions,
    ListTemplateVersionsResponse (ListTemplateVersionsResponse'),
    newListTemplateVersionsResponse,

    -- ** ListTemplates
    ListTemplates (ListTemplates'),
    newListTemplates,
    ListTemplatesResponse (ListTemplatesResponse'),
    newListTemplatesResponse,

    -- ** PhoneNumberValidate
    PhoneNumberValidate (PhoneNumberValidate'),
    newPhoneNumberValidate,
    PhoneNumberValidateResponse (PhoneNumberValidateResponse'),
    newPhoneNumberValidateResponse,

    -- ** PutEventStream
    PutEventStream (PutEventStream'),
    newPutEventStream,
    PutEventStreamResponse (PutEventStreamResponse'),
    newPutEventStreamResponse,

    -- ** PutEvents
    PutEvents (PutEvents'),
    newPutEvents,
    PutEventsResponse (PutEventsResponse'),
    newPutEventsResponse,

    -- ** RemoveAttributes
    RemoveAttributes (RemoveAttributes'),
    newRemoveAttributes,
    RemoveAttributesResponse (RemoveAttributesResponse'),
    newRemoveAttributesResponse,

    -- ** SendMessages
    SendMessages (SendMessages'),
    newSendMessages,
    SendMessagesResponse (SendMessagesResponse'),
    newSendMessagesResponse,

    -- ** SendUsersMessages
    SendUsersMessages (SendUsersMessages'),
    newSendUsersMessages,
    SendUsersMessagesResponse (SendUsersMessagesResponse'),
    newSendUsersMessagesResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateAdmChannel
    UpdateAdmChannel (UpdateAdmChannel'),
    newUpdateAdmChannel,
    UpdateAdmChannelResponse (UpdateAdmChannelResponse'),
    newUpdateAdmChannelResponse,

    -- ** UpdateApnsChannel
    UpdateApnsChannel (UpdateApnsChannel'),
    newUpdateApnsChannel,
    UpdateApnsChannelResponse (UpdateApnsChannelResponse'),
    newUpdateApnsChannelResponse,

    -- ** UpdateApnsSandboxChannel
    UpdateApnsSandboxChannel (UpdateApnsSandboxChannel'),
    newUpdateApnsSandboxChannel,
    UpdateApnsSandboxChannelResponse (UpdateApnsSandboxChannelResponse'),
    newUpdateApnsSandboxChannelResponse,

    -- ** UpdateApnsVoipChannel
    UpdateApnsVoipChannel (UpdateApnsVoipChannel'),
    newUpdateApnsVoipChannel,
    UpdateApnsVoipChannelResponse (UpdateApnsVoipChannelResponse'),
    newUpdateApnsVoipChannelResponse,

    -- ** UpdateApnsVoipSandboxChannel
    UpdateApnsVoipSandboxChannel (UpdateApnsVoipSandboxChannel'),
    newUpdateApnsVoipSandboxChannel,
    UpdateApnsVoipSandboxChannelResponse (UpdateApnsVoipSandboxChannelResponse'),
    newUpdateApnsVoipSandboxChannelResponse,

    -- ** UpdateApplicationSettings
    UpdateApplicationSettings (UpdateApplicationSettings'),
    newUpdateApplicationSettings,
    UpdateApplicationSettingsResponse (UpdateApplicationSettingsResponse'),
    newUpdateApplicationSettingsResponse,

    -- ** UpdateBaiduChannel
    UpdateBaiduChannel (UpdateBaiduChannel'),
    newUpdateBaiduChannel,
    UpdateBaiduChannelResponse (UpdateBaiduChannelResponse'),
    newUpdateBaiduChannelResponse,

    -- ** UpdateCampaign
    UpdateCampaign (UpdateCampaign'),
    newUpdateCampaign,
    UpdateCampaignResponse (UpdateCampaignResponse'),
    newUpdateCampaignResponse,

    -- ** UpdateEmailChannel
    UpdateEmailChannel (UpdateEmailChannel'),
    newUpdateEmailChannel,
    UpdateEmailChannelResponse (UpdateEmailChannelResponse'),
    newUpdateEmailChannelResponse,

    -- ** UpdateEmailTemplate
    UpdateEmailTemplate (UpdateEmailTemplate'),
    newUpdateEmailTemplate,
    UpdateEmailTemplateResponse (UpdateEmailTemplateResponse'),
    newUpdateEmailTemplateResponse,

    -- ** UpdateEndpoint
    UpdateEndpoint (UpdateEndpoint'),
    newUpdateEndpoint,
    UpdateEndpointResponse (UpdateEndpointResponse'),
    newUpdateEndpointResponse,

    -- ** UpdateEndpointsBatch
    UpdateEndpointsBatch (UpdateEndpointsBatch'),
    newUpdateEndpointsBatch,
    UpdateEndpointsBatchResponse (UpdateEndpointsBatchResponse'),
    newUpdateEndpointsBatchResponse,

    -- ** UpdateGcmChannel
    UpdateGcmChannel (UpdateGcmChannel'),
    newUpdateGcmChannel,
    UpdateGcmChannelResponse (UpdateGcmChannelResponse'),
    newUpdateGcmChannelResponse,

    -- ** UpdateInAppTemplate
    UpdateInAppTemplate (UpdateInAppTemplate'),
    newUpdateInAppTemplate,
    UpdateInAppTemplateResponse (UpdateInAppTemplateResponse'),
    newUpdateInAppTemplateResponse,

    -- ** UpdateJourney
    UpdateJourney (UpdateJourney'),
    newUpdateJourney,
    UpdateJourneyResponse (UpdateJourneyResponse'),
    newUpdateJourneyResponse,

    -- ** UpdateJourneyState
    UpdateJourneyState (UpdateJourneyState'),
    newUpdateJourneyState,
    UpdateJourneyStateResponse (UpdateJourneyStateResponse'),
    newUpdateJourneyStateResponse,

    -- ** UpdatePushTemplate
    UpdatePushTemplate (UpdatePushTemplate'),
    newUpdatePushTemplate,
    UpdatePushTemplateResponse (UpdatePushTemplateResponse'),
    newUpdatePushTemplateResponse,

    -- ** UpdateRecommenderConfiguration
    UpdateRecommenderConfiguration' (UpdateRecommenderConfiguration''),
    newUpdateRecommenderConfiguration',
    UpdateRecommenderConfigurationResponse (UpdateRecommenderConfigurationResponse'),
    newUpdateRecommenderConfigurationResponse,

    -- ** UpdateSegment
    UpdateSegment (UpdateSegment'),
    newUpdateSegment,
    UpdateSegmentResponse (UpdateSegmentResponse'),
    newUpdateSegmentResponse,

    -- ** UpdateSmsChannel
    UpdateSmsChannel (UpdateSmsChannel'),
    newUpdateSmsChannel,
    UpdateSmsChannelResponse (UpdateSmsChannelResponse'),
    newUpdateSmsChannelResponse,

    -- ** UpdateSmsTemplate
    UpdateSmsTemplate (UpdateSmsTemplate'),
    newUpdateSmsTemplate,
    UpdateSmsTemplateResponse (UpdateSmsTemplateResponse'),
    newUpdateSmsTemplateResponse,

    -- ** UpdateTemplateActiveVersion
    UpdateTemplateActiveVersion (UpdateTemplateActiveVersion'),
    newUpdateTemplateActiveVersion,
    UpdateTemplateActiveVersionResponse (UpdateTemplateActiveVersionResponse'),
    newUpdateTemplateActiveVersionResponse,

    -- ** UpdateVoiceChannel
    UpdateVoiceChannel (UpdateVoiceChannel'),
    newUpdateVoiceChannel,
    UpdateVoiceChannelResponse (UpdateVoiceChannelResponse'),
    newUpdateVoiceChannelResponse,

    -- ** UpdateVoiceTemplate
    UpdateVoiceTemplate (UpdateVoiceTemplate'),
    newUpdateVoiceTemplate,
    UpdateVoiceTemplateResponse (UpdateVoiceTemplateResponse'),
    newUpdateVoiceTemplateResponse,

    -- * Types

    -- ** Action
    Action (..),

    -- ** Alignment
    Alignment (..),

    -- ** AttributeType
    AttributeType (..),

    -- ** ButtonAction
    ButtonAction (..),

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

    -- ** Layout
    Layout (..),

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
    ADMChannelRequest (ADMChannelRequest'),
    newADMChannelRequest,

    -- ** ADMChannelResponse
    ADMChannelResponse (ADMChannelResponse'),
    newADMChannelResponse,

    -- ** ADMMessage
    ADMMessage (ADMMessage'),
    newADMMessage,

    -- ** APNSChannelRequest
    APNSChannelRequest (APNSChannelRequest'),
    newAPNSChannelRequest,

    -- ** APNSChannelResponse
    APNSChannelResponse (APNSChannelResponse'),
    newAPNSChannelResponse,

    -- ** APNSMessage
    APNSMessage (APNSMessage'),
    newAPNSMessage,

    -- ** APNSPushNotificationTemplate
    APNSPushNotificationTemplate (APNSPushNotificationTemplate'),
    newAPNSPushNotificationTemplate,

    -- ** APNSSandboxChannelRequest
    APNSSandboxChannelRequest (APNSSandboxChannelRequest'),
    newAPNSSandboxChannelRequest,

    -- ** APNSSandboxChannelResponse
    APNSSandboxChannelResponse (APNSSandboxChannelResponse'),
    newAPNSSandboxChannelResponse,

    -- ** APNSVoipChannelRequest
    APNSVoipChannelRequest (APNSVoipChannelRequest'),
    newAPNSVoipChannelRequest,

    -- ** APNSVoipChannelResponse
    APNSVoipChannelResponse (APNSVoipChannelResponse'),
    newAPNSVoipChannelResponse,

    -- ** APNSVoipSandboxChannelRequest
    APNSVoipSandboxChannelRequest (APNSVoipSandboxChannelRequest'),
    newAPNSVoipSandboxChannelRequest,

    -- ** APNSVoipSandboxChannelResponse
    APNSVoipSandboxChannelResponse (APNSVoipSandboxChannelResponse'),
    newAPNSVoipSandboxChannelResponse,

    -- ** ActivitiesResponse
    ActivitiesResponse (ActivitiesResponse'),
    newActivitiesResponse,

    -- ** Activity
    Activity (Activity'),
    newActivity,

    -- ** ActivityResponse
    ActivityResponse (ActivityResponse'),
    newActivityResponse,

    -- ** AddressConfiguration
    AddressConfiguration (AddressConfiguration'),
    newAddressConfiguration,

    -- ** AndroidPushNotificationTemplate
    AndroidPushNotificationTemplate (AndroidPushNotificationTemplate'),
    newAndroidPushNotificationTemplate,

    -- ** ApplicationDateRangeKpiResponse
    ApplicationDateRangeKpiResponse (ApplicationDateRangeKpiResponse'),
    newApplicationDateRangeKpiResponse,

    -- ** ApplicationResponse
    ApplicationResponse (ApplicationResponse'),
    newApplicationResponse,

    -- ** ApplicationSettingsResource
    ApplicationSettingsResource (ApplicationSettingsResource'),
    newApplicationSettingsResource,

    -- ** ApplicationsResponse
    ApplicationsResponse (ApplicationsResponse'),
    newApplicationsResponse,

    -- ** AttributeDimension
    AttributeDimension (AttributeDimension'),
    newAttributeDimension,

    -- ** AttributesResource
    AttributesResource (AttributesResource'),
    newAttributesResource,

    -- ** BaiduChannelRequest
    BaiduChannelRequest (BaiduChannelRequest'),
    newBaiduChannelRequest,

    -- ** BaiduChannelResponse
    BaiduChannelResponse (BaiduChannelResponse'),
    newBaiduChannelResponse,

    -- ** BaiduMessage
    BaiduMessage (BaiduMessage'),
    newBaiduMessage,

    -- ** BaseKpiResult
    BaseKpiResult (BaseKpiResult'),
    newBaseKpiResult,

    -- ** CampaignCustomMessage
    CampaignCustomMessage (CampaignCustomMessage'),
    newCampaignCustomMessage,

    -- ** CampaignDateRangeKpiResponse
    CampaignDateRangeKpiResponse (CampaignDateRangeKpiResponse'),
    newCampaignDateRangeKpiResponse,

    -- ** CampaignEmailMessage
    CampaignEmailMessage (CampaignEmailMessage'),
    newCampaignEmailMessage,

    -- ** CampaignEventFilter
    CampaignEventFilter (CampaignEventFilter'),
    newCampaignEventFilter,

    -- ** CampaignHook
    CampaignHook (CampaignHook'),
    newCampaignHook,

    -- ** CampaignInAppMessage
    CampaignInAppMessage (CampaignInAppMessage'),
    newCampaignInAppMessage,

    -- ** CampaignLimits
    CampaignLimits (CampaignLimits'),
    newCampaignLimits,

    -- ** CampaignResponse
    CampaignResponse (CampaignResponse'),
    newCampaignResponse,

    -- ** CampaignSmsMessage
    CampaignSmsMessage (CampaignSmsMessage'),
    newCampaignSmsMessage,

    -- ** CampaignState
    CampaignState (CampaignState'),
    newCampaignState,

    -- ** CampaignsResponse
    CampaignsResponse (CampaignsResponse'),
    newCampaignsResponse,

    -- ** ChannelResponse
    ChannelResponse (ChannelResponse'),
    newChannelResponse,

    -- ** ChannelsResponse
    ChannelsResponse (ChannelsResponse'),
    newChannelsResponse,

    -- ** Condition
    Condition (Condition'),
    newCondition,

    -- ** ConditionalSplitActivity
    ConditionalSplitActivity (ConditionalSplitActivity'),
    newConditionalSplitActivity,

    -- ** ContactCenterActivity
    ContactCenterActivity (ContactCenterActivity'),
    newContactCenterActivity,

    -- ** CreateApplicationRequest
    CreateApplicationRequest (CreateApplicationRequest'),
    newCreateApplicationRequest,

    -- ** CreateRecommenderConfiguration
    CreateRecommenderConfiguration (CreateRecommenderConfiguration'),
    newCreateRecommenderConfiguration,

    -- ** CreateTemplateMessageBody
    CreateTemplateMessageBody (CreateTemplateMessageBody'),
    newCreateTemplateMessageBody,

    -- ** CustomDeliveryConfiguration
    CustomDeliveryConfiguration (CustomDeliveryConfiguration'),
    newCustomDeliveryConfiguration,

    -- ** CustomMessageActivity
    CustomMessageActivity (CustomMessageActivity'),
    newCustomMessageActivity,

    -- ** DefaultButtonConfiguration
    DefaultButtonConfiguration (DefaultButtonConfiguration'),
    newDefaultButtonConfiguration,

    -- ** DefaultMessage
    DefaultMessage (DefaultMessage'),
    newDefaultMessage,

    -- ** DefaultPushNotificationMessage
    DefaultPushNotificationMessage (DefaultPushNotificationMessage'),
    newDefaultPushNotificationMessage,

    -- ** DefaultPushNotificationTemplate
    DefaultPushNotificationTemplate (DefaultPushNotificationTemplate'),
    newDefaultPushNotificationTemplate,

    -- ** DirectMessageConfiguration
    DirectMessageConfiguration (DirectMessageConfiguration'),
    newDirectMessageConfiguration,

    -- ** EmailChannelRequest
    EmailChannelRequest (EmailChannelRequest'),
    newEmailChannelRequest,

    -- ** EmailChannelResponse
    EmailChannelResponse (EmailChannelResponse'),
    newEmailChannelResponse,

    -- ** EmailMessage
    EmailMessage (EmailMessage'),
    newEmailMessage,

    -- ** EmailMessageActivity
    EmailMessageActivity (EmailMessageActivity'),
    newEmailMessageActivity,

    -- ** EmailTemplateRequest
    EmailTemplateRequest (EmailTemplateRequest'),
    newEmailTemplateRequest,

    -- ** EmailTemplateResponse
    EmailTemplateResponse (EmailTemplateResponse'),
    newEmailTemplateResponse,

    -- ** EndpointBatchItem
    EndpointBatchItem (EndpointBatchItem'),
    newEndpointBatchItem,

    -- ** EndpointBatchRequest
    EndpointBatchRequest (EndpointBatchRequest'),
    newEndpointBatchRequest,

    -- ** EndpointDemographic
    EndpointDemographic (EndpointDemographic'),
    newEndpointDemographic,

    -- ** EndpointItemResponse
    EndpointItemResponse (EndpointItemResponse'),
    newEndpointItemResponse,

    -- ** EndpointLocation
    EndpointLocation (EndpointLocation'),
    newEndpointLocation,

    -- ** EndpointMessageResult
    EndpointMessageResult (EndpointMessageResult'),
    newEndpointMessageResult,

    -- ** EndpointRequest
    EndpointRequest (EndpointRequest'),
    newEndpointRequest,

    -- ** EndpointResponse
    EndpointResponse (EndpointResponse'),
    newEndpointResponse,

    -- ** EndpointSendConfiguration
    EndpointSendConfiguration (EndpointSendConfiguration'),
    newEndpointSendConfiguration,

    -- ** EndpointUser
    EndpointUser (EndpointUser'),
    newEndpointUser,

    -- ** EndpointsResponse
    EndpointsResponse (EndpointsResponse'),
    newEndpointsResponse,

    -- ** Event
    Event (Event'),
    newEvent,

    -- ** EventCondition
    EventCondition (EventCondition'),
    newEventCondition,

    -- ** EventDimensions
    EventDimensions (EventDimensions'),
    newEventDimensions,

    -- ** EventFilter
    EventFilter (EventFilter'),
    newEventFilter,

    -- ** EventItemResponse
    EventItemResponse (EventItemResponse'),
    newEventItemResponse,

    -- ** EventStartCondition
    EventStartCondition (EventStartCondition'),
    newEventStartCondition,

    -- ** EventStream
    EventStream (EventStream'),
    newEventStream,

    -- ** EventsBatch
    EventsBatch (EventsBatch'),
    newEventsBatch,

    -- ** EventsRequest
    EventsRequest (EventsRequest'),
    newEventsRequest,

    -- ** EventsResponse
    EventsResponse (EventsResponse'),
    newEventsResponse,

    -- ** ExportJobRequest
    ExportJobRequest (ExportJobRequest'),
    newExportJobRequest,

    -- ** ExportJobResource
    ExportJobResource (ExportJobResource'),
    newExportJobResource,

    -- ** ExportJobResponse
    ExportJobResponse (ExportJobResponse'),
    newExportJobResponse,

    -- ** ExportJobsResponse
    ExportJobsResponse (ExportJobsResponse'),
    newExportJobsResponse,

    -- ** GCMChannelRequest
    GCMChannelRequest (GCMChannelRequest'),
    newGCMChannelRequest,

    -- ** GCMChannelResponse
    GCMChannelResponse (GCMChannelResponse'),
    newGCMChannelResponse,

    -- ** GCMMessage
    GCMMessage (GCMMessage'),
    newGCMMessage,

    -- ** GPSCoordinates
    GPSCoordinates (GPSCoordinates'),
    newGPSCoordinates,

    -- ** GPSPointDimension
    GPSPointDimension (GPSPointDimension'),
    newGPSPointDimension,

    -- ** HoldoutActivity
    HoldoutActivity (HoldoutActivity'),
    newHoldoutActivity,

    -- ** ImportJobRequest
    ImportJobRequest (ImportJobRequest'),
    newImportJobRequest,

    -- ** ImportJobResource
    ImportJobResource (ImportJobResource'),
    newImportJobResource,

    -- ** ImportJobResponse
    ImportJobResponse (ImportJobResponse'),
    newImportJobResponse,

    -- ** ImportJobsResponse
    ImportJobsResponse (ImportJobsResponse'),
    newImportJobsResponse,

    -- ** InAppCampaignSchedule
    InAppCampaignSchedule (InAppCampaignSchedule'),
    newInAppCampaignSchedule,

    -- ** InAppMessage
    InAppMessage (InAppMessage'),
    newInAppMessage,

    -- ** InAppMessageBodyConfig
    InAppMessageBodyConfig (InAppMessageBodyConfig'),
    newInAppMessageBodyConfig,

    -- ** InAppMessageButton
    InAppMessageButton (InAppMessageButton'),
    newInAppMessageButton,

    -- ** InAppMessageCampaign
    InAppMessageCampaign (InAppMessageCampaign'),
    newInAppMessageCampaign,

    -- ** InAppMessageContent
    InAppMessageContent (InAppMessageContent'),
    newInAppMessageContent,

    -- ** InAppMessageHeaderConfig
    InAppMessageHeaderConfig (InAppMessageHeaderConfig'),
    newInAppMessageHeaderConfig,

    -- ** InAppMessagesResponse
    InAppMessagesResponse (InAppMessagesResponse'),
    newInAppMessagesResponse,

    -- ** InAppTemplateRequest
    InAppTemplateRequest (InAppTemplateRequest'),
    newInAppTemplateRequest,

    -- ** InAppTemplateResponse
    InAppTemplateResponse (InAppTemplateResponse'),
    newInAppTemplateResponse,

    -- ** ItemResponse
    ItemResponse (ItemResponse'),
    newItemResponse,

    -- ** JourneyChannelSettings
    JourneyChannelSettings (JourneyChannelSettings'),
    newJourneyChannelSettings,

    -- ** JourneyCustomMessage
    JourneyCustomMessage (JourneyCustomMessage'),
    newJourneyCustomMessage,

    -- ** JourneyDateRangeKpiResponse
    JourneyDateRangeKpiResponse (JourneyDateRangeKpiResponse'),
    newJourneyDateRangeKpiResponse,

    -- ** JourneyEmailMessage
    JourneyEmailMessage (JourneyEmailMessage'),
    newJourneyEmailMessage,

    -- ** JourneyExecutionActivityMetricsResponse
    JourneyExecutionActivityMetricsResponse (JourneyExecutionActivityMetricsResponse'),
    newJourneyExecutionActivityMetricsResponse,

    -- ** JourneyExecutionMetricsResponse
    JourneyExecutionMetricsResponse (JourneyExecutionMetricsResponse'),
    newJourneyExecutionMetricsResponse,

    -- ** JourneyLimits
    JourneyLimits (JourneyLimits'),
    newJourneyLimits,

    -- ** JourneyPushMessage
    JourneyPushMessage (JourneyPushMessage'),
    newJourneyPushMessage,

    -- ** JourneyResponse
    JourneyResponse (JourneyResponse'),
    newJourneyResponse,

    -- ** JourneySMSMessage
    JourneySMSMessage (JourneySMSMessage'),
    newJourneySMSMessage,

    -- ** JourneySchedule
    JourneySchedule (JourneySchedule'),
    newJourneySchedule,

    -- ** JourneyStateRequest
    JourneyStateRequest (JourneyStateRequest'),
    newJourneyStateRequest,

    -- ** JourneysResponse
    JourneysResponse (JourneysResponse'),
    newJourneysResponse,

    -- ** ListRecommenderConfigurationsResponse
    ListRecommenderConfigurationsResponse (ListRecommenderConfigurationsResponse'),
    newListRecommenderConfigurationsResponse,

    -- ** Message
    Message (Message'),
    newMessage,

    -- ** MessageBody
    MessageBody (MessageBody'),
    newMessageBody,

    -- ** MessageConfiguration
    MessageConfiguration (MessageConfiguration'),
    newMessageConfiguration,

    -- ** MessageRequest
    MessageRequest (MessageRequest'),
    newMessageRequest,

    -- ** MessageResponse
    MessageResponse (MessageResponse'),
    newMessageResponse,

    -- ** MessageResult
    MessageResult (MessageResult'),
    newMessageResult,

    -- ** MetricDimension
    MetricDimension (MetricDimension'),
    newMetricDimension,

    -- ** MultiConditionalBranch
    MultiConditionalBranch (MultiConditionalBranch'),
    newMultiConditionalBranch,

    -- ** MultiConditionalSplitActivity
    MultiConditionalSplitActivity (MultiConditionalSplitActivity'),
    newMultiConditionalSplitActivity,

    -- ** NumberValidateRequest
    NumberValidateRequest (NumberValidateRequest'),
    newNumberValidateRequest,

    -- ** NumberValidateResponse
    NumberValidateResponse (NumberValidateResponse'),
    newNumberValidateResponse,

    -- ** OverrideButtonConfiguration
    OverrideButtonConfiguration (OverrideButtonConfiguration'),
    newOverrideButtonConfiguration,

    -- ** PublicEndpoint
    PublicEndpoint (PublicEndpoint'),
    newPublicEndpoint,

    -- ** PushMessageActivity
    PushMessageActivity (PushMessageActivity'),
    newPushMessageActivity,

    -- ** PushNotificationTemplateRequest
    PushNotificationTemplateRequest (PushNotificationTemplateRequest'),
    newPushNotificationTemplateRequest,

    -- ** PushNotificationTemplateResponse
    PushNotificationTemplateResponse (PushNotificationTemplateResponse'),
    newPushNotificationTemplateResponse,

    -- ** QuietTime
    QuietTime (QuietTime'),
    newQuietTime,

    -- ** RandomSplitActivity
    RandomSplitActivity (RandomSplitActivity'),
    newRandomSplitActivity,

    -- ** RandomSplitEntry
    RandomSplitEntry (RandomSplitEntry'),
    newRandomSplitEntry,

    -- ** RawEmail
    RawEmail (RawEmail'),
    newRawEmail,

    -- ** RecencyDimension
    RecencyDimension (RecencyDimension'),
    newRecencyDimension,

    -- ** RecommenderConfigurationResponse
    RecommenderConfigurationResponse (RecommenderConfigurationResponse'),
    newRecommenderConfigurationResponse,

    -- ** ResultRow
    ResultRow (ResultRow'),
    newResultRow,

    -- ** ResultRowValue
    ResultRowValue (ResultRowValue'),
    newResultRowValue,

    -- ** SMSChannelRequest
    SMSChannelRequest (SMSChannelRequest'),
    newSMSChannelRequest,

    -- ** SMSChannelResponse
    SMSChannelResponse (SMSChannelResponse'),
    newSMSChannelResponse,

    -- ** SMSMessage
    SMSMessage (SMSMessage'),
    newSMSMessage,

    -- ** SMSMessageActivity
    SMSMessageActivity (SMSMessageActivity'),
    newSMSMessageActivity,

    -- ** SMSTemplateRequest
    SMSTemplateRequest (SMSTemplateRequest'),
    newSMSTemplateRequest,

    -- ** SMSTemplateResponse
    SMSTemplateResponse (SMSTemplateResponse'),
    newSMSTemplateResponse,

    -- ** Schedule
    Schedule (Schedule'),
    newSchedule,

    -- ** SegmentBehaviors
    SegmentBehaviors (SegmentBehaviors'),
    newSegmentBehaviors,

    -- ** SegmentCondition
    SegmentCondition (SegmentCondition'),
    newSegmentCondition,

    -- ** SegmentDemographics
    SegmentDemographics (SegmentDemographics'),
    newSegmentDemographics,

    -- ** SegmentDimensions
    SegmentDimensions (SegmentDimensions'),
    newSegmentDimensions,

    -- ** SegmentGroup
    SegmentGroup (SegmentGroup'),
    newSegmentGroup,

    -- ** SegmentGroupList
    SegmentGroupList (SegmentGroupList'),
    newSegmentGroupList,

    -- ** SegmentImportResource
    SegmentImportResource (SegmentImportResource'),
    newSegmentImportResource,

    -- ** SegmentLocation
    SegmentLocation (SegmentLocation'),
    newSegmentLocation,

    -- ** SegmentReference
    SegmentReference (SegmentReference'),
    newSegmentReference,

    -- ** SegmentResponse
    SegmentResponse (SegmentResponse'),
    newSegmentResponse,

    -- ** SegmentsResponse
    SegmentsResponse (SegmentsResponse'),
    newSegmentsResponse,

    -- ** SendUsersMessageRequest
    SendUsersMessageRequest (SendUsersMessageRequest'),
    newSendUsersMessageRequest,

    -- ** SendUsersMessageResponse
    SendUsersMessageResponse (SendUsersMessageResponse'),
    newSendUsersMessageResponse,

    -- ** Session
    Session (Session'),
    newSession,

    -- ** SetDimension
    SetDimension (SetDimension'),
    newSetDimension,

    -- ** SimpleCondition
    SimpleCondition (SimpleCondition'),
    newSimpleCondition,

    -- ** SimpleEmail
    SimpleEmail (SimpleEmail'),
    newSimpleEmail,

    -- ** SimpleEmailPart
    SimpleEmailPart (SimpleEmailPart'),
    newSimpleEmailPart,

    -- ** StartCondition
    StartCondition (StartCondition'),
    newStartCondition,

    -- ** TagsModel
    TagsModel (TagsModel'),
    newTagsModel,

    -- ** Template
    Template (Template'),
    newTemplate,

    -- ** TemplateActiveVersionRequest
    TemplateActiveVersionRequest (TemplateActiveVersionRequest'),
    newTemplateActiveVersionRequest,

    -- ** TemplateConfiguration
    TemplateConfiguration (TemplateConfiguration'),
    newTemplateConfiguration,

    -- ** TemplateCreateMessageBody
    TemplateCreateMessageBody (TemplateCreateMessageBody'),
    newTemplateCreateMessageBody,

    -- ** TemplateResponse
    TemplateResponse (TemplateResponse'),
    newTemplateResponse,

    -- ** TemplateVersionResponse
    TemplateVersionResponse (TemplateVersionResponse'),
    newTemplateVersionResponse,

    -- ** TemplateVersionsResponse
    TemplateVersionsResponse (TemplateVersionsResponse'),
    newTemplateVersionsResponse,

    -- ** TemplatesResponse
    TemplatesResponse (TemplatesResponse'),
    newTemplatesResponse,

    -- ** TreatmentResource
    TreatmentResource (TreatmentResource'),
    newTreatmentResource,

    -- ** UpdateAttributesRequest
    UpdateAttributesRequest (UpdateAttributesRequest'),
    newUpdateAttributesRequest,

    -- ** UpdateRecommenderConfiguration
    UpdateRecommenderConfiguration (UpdateRecommenderConfiguration'),
    newUpdateRecommenderConfiguration,

    -- ** VoiceChannelRequest
    VoiceChannelRequest (VoiceChannelRequest'),
    newVoiceChannelRequest,

    -- ** VoiceChannelResponse
    VoiceChannelResponse (VoiceChannelResponse'),
    newVoiceChannelResponse,

    -- ** VoiceMessage
    VoiceMessage (VoiceMessage'),
    newVoiceMessage,

    -- ** VoiceTemplateRequest
    VoiceTemplateRequest (VoiceTemplateRequest'),
    newVoiceTemplateRequest,

    -- ** VoiceTemplateResponse
    VoiceTemplateResponse (VoiceTemplateResponse'),
    newVoiceTemplateResponse,

    -- ** WaitActivity
    WaitActivity (WaitActivity'),
    newWaitActivity,

    -- ** WaitTime
    WaitTime (WaitTime'),
    newWaitTime,

    -- ** WriteApplicationSettingsRequest
    WriteApplicationSettingsRequest (WriteApplicationSettingsRequest'),
    newWriteApplicationSettingsRequest,

    -- ** WriteCampaignRequest
    WriteCampaignRequest (WriteCampaignRequest'),
    newWriteCampaignRequest,

    -- ** WriteEventStream
    WriteEventStream (WriteEventStream'),
    newWriteEventStream,

    -- ** WriteJourneyRequest
    WriteJourneyRequest (WriteJourneyRequest'),
    newWriteJourneyRequest,

    -- ** WriteSegmentRequest
    WriteSegmentRequest (WriteSegmentRequest'),
    newWriteSegmentRequest,

    -- ** WriteTreatmentResource
    WriteTreatmentResource (WriteTreatmentResource'),
    newWriteTreatmentResource,
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
import Amazonka.Pinpoint.Lens
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
import Amazonka.Pinpoint.Types
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
import Amazonka.Pinpoint.Waiters

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
