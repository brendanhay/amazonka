{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
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
module Network.AWS.Pinpoint
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** NotFoundException
    _NotFoundException,

    -- ** BadRequestException
    _BadRequestException,

    -- ** PayloadTooLargeException
    _PayloadTooLargeException,

    -- ** InternalServerErrorException
    _InternalServerErrorException,

    -- ** ForbiddenException
    _ForbiddenException,

    -- ** ConflictException
    _ConflictException,

    -- ** MethodNotAllowedException
    _MethodNotAllowedException,

    -- ** TooManyRequestsException
    _TooManyRequestsException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DeleteVoiceTemplate
    DeleteVoiceTemplate (DeleteVoiceTemplate'),
    newDeleteVoiceTemplate,
    DeleteVoiceTemplateResponse (DeleteVoiceTemplateResponse'),
    newDeleteVoiceTemplateResponse,

    -- ** GetImportJobs
    GetImportJobs (GetImportJobs'),
    newGetImportJobs,
    GetImportJobsResponse (GetImportJobsResponse'),
    newGetImportJobsResponse,

    -- ** UpdatePushTemplate
    UpdatePushTemplate (UpdatePushTemplate'),
    newUpdatePushTemplate,
    UpdatePushTemplateResponse (UpdatePushTemplateResponse'),
    newUpdatePushTemplateResponse,

    -- ** DeleteCampaign
    DeleteCampaign (DeleteCampaign'),
    newDeleteCampaign,
    DeleteCampaignResponse (DeleteCampaignResponse'),
    newDeleteCampaignResponse,

    -- ** UpdateVoiceTemplate
    UpdateVoiceTemplate (UpdateVoiceTemplate'),
    newUpdateVoiceTemplate,
    UpdateVoiceTemplateResponse (UpdateVoiceTemplateResponse'),
    newUpdateVoiceTemplateResponse,

    -- ** UpdateCampaign
    UpdateCampaign (UpdateCampaign'),
    newUpdateCampaign,
    UpdateCampaignResponse (UpdateCampaignResponse'),
    newUpdateCampaignResponse,

    -- ** CreateRecommenderConfiguration
    CreateRecommenderConfiguration' (CreateRecommenderConfiguration''),
    newCreateRecommenderConfiguration',
    CreateRecommenderConfigurationResponse (CreateRecommenderConfigurationResponse'),
    newCreateRecommenderConfigurationResponse,

    -- ** UpdateTemplateActiveVersion
    UpdateTemplateActiveVersion (UpdateTemplateActiveVersion'),
    newUpdateTemplateActiveVersion,
    UpdateTemplateActiveVersionResponse (UpdateTemplateActiveVersionResponse'),
    newUpdateTemplateActiveVersionResponse,

    -- ** DeletePushTemplate
    DeletePushTemplate (DeletePushTemplate'),
    newDeletePushTemplate,
    DeletePushTemplateResponse (DeletePushTemplateResponse'),
    newDeletePushTemplateResponse,

    -- ** CreateJourney
    CreateJourney (CreateJourney'),
    newCreateJourney,
    CreateJourneyResponse (CreateJourneyResponse'),
    newCreateJourneyResponse,

    -- ** GetImportJob
    GetImportJob (GetImportJob'),
    newGetImportJob,
    GetImportJobResponse (GetImportJobResponse'),
    newGetImportJobResponse,

    -- ** GetSegmentVersions
    GetSegmentVersions (GetSegmentVersions'),
    newGetSegmentVersions,
    GetSegmentVersionsResponse (GetSegmentVersionsResponse'),
    newGetSegmentVersionsResponse,

    -- ** GetApps
    GetApps (GetApps'),
    newGetApps,
    GetAppsResponse (GetAppsResponse'),
    newGetAppsResponse,

    -- ** GetSegmentImportJobs
    GetSegmentImportJobs (GetSegmentImportJobs'),
    newGetSegmentImportJobs,
    GetSegmentImportJobsResponse (GetSegmentImportJobsResponse'),
    newGetSegmentImportJobsResponse,

    -- ** GetApnsSandboxChannel
    GetApnsSandboxChannel (GetApnsSandboxChannel'),
    newGetApnsSandboxChannel,
    GetApnsSandboxChannelResponse (GetApnsSandboxChannelResponse'),
    newGetApnsSandboxChannelResponse,

    -- ** SendMessages
    SendMessages (SendMessages'),
    newSendMessages,
    SendMessagesResponse (SendMessagesResponse'),
    newSendMessagesResponse,

    -- ** CreateSmsTemplate
    CreateSmsTemplate (CreateSmsTemplate'),
    newCreateSmsTemplate,
    CreateSmsTemplateResponse (CreateSmsTemplateResponse'),
    newCreateSmsTemplateResponse,

    -- ** RemoveAttributes
    RemoveAttributes (RemoveAttributes'),
    newRemoveAttributes,
    RemoveAttributesResponse (RemoveAttributesResponse'),
    newRemoveAttributesResponse,

    -- ** GetApnsChannel
    GetApnsChannel (GetApnsChannel'),
    newGetApnsChannel,
    GetApnsChannelResponse (GetApnsChannelResponse'),
    newGetApnsChannelResponse,

    -- ** PhoneNumberValidate
    PhoneNumberValidate (PhoneNumberValidate'),
    newPhoneNumberValidate,
    PhoneNumberValidateResponse (PhoneNumberValidateResponse'),
    newPhoneNumberValidateResponse,

    -- ** GetEmailChannel
    GetEmailChannel (GetEmailChannel'),
    newGetEmailChannel,
    GetEmailChannelResponse (GetEmailChannelResponse'),
    newGetEmailChannelResponse,

    -- ** PutEventStream
    PutEventStream (PutEventStream'),
    newPutEventStream,
    PutEventStreamResponse (PutEventStreamResponse'),
    newPutEventStreamResponse,

    -- ** GetJourneyExecutionActivityMetrics
    GetJourneyExecutionActivityMetrics (GetJourneyExecutionActivityMetrics'),
    newGetJourneyExecutionActivityMetrics,
    GetJourneyExecutionActivityMetricsResponse (GetJourneyExecutionActivityMetricsResponse'),
    newGetJourneyExecutionActivityMetricsResponse,

    -- ** UpdateApnsChannel
    UpdateApnsChannel (UpdateApnsChannel'),
    newUpdateApnsChannel,
    UpdateApnsChannelResponse (UpdateApnsChannelResponse'),
    newUpdateApnsChannelResponse,

    -- ** DeleteApnsChannel
    DeleteApnsChannel (DeleteApnsChannel'),
    newDeleteApnsChannel,
    DeleteApnsChannelResponse (DeleteApnsChannelResponse'),
    newDeleteApnsChannelResponse,

    -- ** GetBaiduChannel
    GetBaiduChannel (GetBaiduChannel'),
    newGetBaiduChannel,
    GetBaiduChannelResponse (GetBaiduChannelResponse'),
    newGetBaiduChannelResponse,

    -- ** GetChannels
    GetChannels (GetChannels'),
    newGetChannels,
    GetChannelsResponse (GetChannelsResponse'),
    newGetChannelsResponse,

    -- ** GetRecommenderConfigurations
    GetRecommenderConfigurations (GetRecommenderConfigurations'),
    newGetRecommenderConfigurations,
    GetRecommenderConfigurationsResponse (GetRecommenderConfigurationsResponse'),
    newGetRecommenderConfigurationsResponse,

    -- ** UpdateGcmChannel
    UpdateGcmChannel (UpdateGcmChannel'),
    newUpdateGcmChannel,
    UpdateGcmChannelResponse (UpdateGcmChannelResponse'),
    newUpdateGcmChannelResponse,

    -- ** DeleteGcmChannel
    DeleteGcmChannel (DeleteGcmChannel'),
    newDeleteGcmChannel,
    DeleteGcmChannelResponse (DeleteGcmChannelResponse'),
    newDeleteGcmChannelResponse,

    -- ** GetJourneyExecutionMetrics
    GetJourneyExecutionMetrics (GetJourneyExecutionMetrics'),
    newGetJourneyExecutionMetrics,
    GetJourneyExecutionMetricsResponse (GetJourneyExecutionMetricsResponse'),
    newGetJourneyExecutionMetricsResponse,

    -- ** GetVoiceChannel
    GetVoiceChannel (GetVoiceChannel'),
    newGetVoiceChannel,
    GetVoiceChannelResponse (GetVoiceChannelResponse'),
    newGetVoiceChannelResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateApnsVoipSandboxChannel
    UpdateApnsVoipSandboxChannel (UpdateApnsVoipSandboxChannel'),
    newUpdateApnsVoipSandboxChannel,
    UpdateApnsVoipSandboxChannelResponse (UpdateApnsVoipSandboxChannelResponse'),
    newUpdateApnsVoipSandboxChannelResponse,

    -- ** DeleteApnsVoipSandboxChannel
    DeleteApnsVoipSandboxChannel (DeleteApnsVoipSandboxChannel'),
    newDeleteApnsVoipSandboxChannel,
    DeleteApnsVoipSandboxChannelResponse (DeleteApnsVoipSandboxChannelResponse'),
    newDeleteApnsVoipSandboxChannelResponse,

    -- ** GetVoiceTemplate
    GetVoiceTemplate (GetVoiceTemplate'),
    newGetVoiceTemplate,
    GetVoiceTemplateResponse (GetVoiceTemplateResponse'),
    newGetVoiceTemplateResponse,

    -- ** GetSmsChannel
    GetSmsChannel (GetSmsChannel'),
    newGetSmsChannel,
    GetSmsChannelResponse (GetSmsChannelResponse'),
    newGetSmsChannelResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** GetEndpoint
    GetEndpoint (GetEndpoint'),
    newGetEndpoint,
    GetEndpointResponse (GetEndpointResponse'),
    newGetEndpointResponse,

    -- ** GetApplicationDateRangeKpi
    GetApplicationDateRangeKpi (GetApplicationDateRangeKpi'),
    newGetApplicationDateRangeKpi,
    GetApplicationDateRangeKpiResponse (GetApplicationDateRangeKpiResponse'),
    newGetApplicationDateRangeKpiResponse,

    -- ** GetAdmChannel
    GetAdmChannel (GetAdmChannel'),
    newGetAdmChannel,
    GetAdmChannelResponse (GetAdmChannelResponse'),
    newGetAdmChannelResponse,

    -- ** GetRecommenderConfiguration
    GetRecommenderConfiguration (GetRecommenderConfiguration'),
    newGetRecommenderConfiguration,
    GetRecommenderConfigurationResponse (GetRecommenderConfigurationResponse'),
    newGetRecommenderConfigurationResponse,

    -- ** GetSegmentExportJobs
    GetSegmentExportJobs (GetSegmentExportJobs'),
    newGetSegmentExportJobs,
    GetSegmentExportJobsResponse (GetSegmentExportJobsResponse'),
    newGetSegmentExportJobsResponse,

    -- ** UpdateSegment
    UpdateSegment (UpdateSegment'),
    newUpdateSegment,
    UpdateSegmentResponse (UpdateSegmentResponse'),
    newUpdateSegmentResponse,

    -- ** DeleteSegment
    DeleteSegment (DeleteSegment'),
    newDeleteSegment,
    DeleteSegmentResponse (DeleteSegmentResponse'),
    newDeleteSegmentResponse,

    -- ** CreatePushTemplate
    CreatePushTemplate (CreatePushTemplate'),
    newCreatePushTemplate,
    CreatePushTemplateResponse (CreatePushTemplateResponse'),
    newCreatePushTemplateResponse,

    -- ** DeleteAdmChannel
    DeleteAdmChannel (DeleteAdmChannel'),
    newDeleteAdmChannel,
    DeleteAdmChannelResponse (DeleteAdmChannelResponse'),
    newDeleteAdmChannelResponse,

    -- ** UpdateRecommenderConfiguration
    UpdateRecommenderConfiguration' (UpdateRecommenderConfiguration''),
    newUpdateRecommenderConfiguration',
    UpdateRecommenderConfigurationResponse (UpdateRecommenderConfigurationResponse'),
    newUpdateRecommenderConfigurationResponse,

    -- ** DeleteEndpoint
    DeleteEndpoint (DeleteEndpoint'),
    newDeleteEndpoint,
    DeleteEndpointResponse (DeleteEndpointResponse'),
    newDeleteEndpointResponse,

    -- ** CreateCampaign
    CreateCampaign (CreateCampaign'),
    newCreateCampaign,
    CreateCampaignResponse (CreateCampaignResponse'),
    newCreateCampaignResponse,

    -- ** UpdateEndpoint
    UpdateEndpoint (UpdateEndpoint'),
    newUpdateEndpoint,
    UpdateEndpointResponse (UpdateEndpointResponse'),
    newUpdateEndpointResponse,

    -- ** GetEmailTemplate
    GetEmailTemplate (GetEmailTemplate'),
    newGetEmailTemplate,
    GetEmailTemplateResponse (GetEmailTemplateResponse'),
    newGetEmailTemplateResponse,

    -- ** DeleteRecommenderConfiguration
    DeleteRecommenderConfiguration (DeleteRecommenderConfiguration'),
    newDeleteRecommenderConfiguration,
    DeleteRecommenderConfigurationResponse (DeleteRecommenderConfigurationResponse'),
    newDeleteRecommenderConfigurationResponse,

    -- ** UpdateAdmChannel
    UpdateAdmChannel (UpdateAdmChannel'),
    newUpdateAdmChannel,
    UpdateAdmChannelResponse (UpdateAdmChannelResponse'),
    newUpdateAdmChannelResponse,

    -- ** DeleteSmsChannel
    DeleteSmsChannel (DeleteSmsChannel'),
    newDeleteSmsChannel,
    DeleteSmsChannelResponse (DeleteSmsChannelResponse'),
    newDeleteSmsChannelResponse,

    -- ** GetJourneyDateRangeKpi
    GetJourneyDateRangeKpi (GetJourneyDateRangeKpi'),
    newGetJourneyDateRangeKpi,
    GetJourneyDateRangeKpiResponse (GetJourneyDateRangeKpiResponse'),
    newGetJourneyDateRangeKpiResponse,

    -- ** GetApp
    GetApp (GetApp'),
    newGetApp,
    GetAppResponse (GetAppResponse'),
    newGetAppResponse,

    -- ** CreateExportJob
    CreateExportJob (CreateExportJob'),
    newCreateExportJob,
    CreateExportJobResponse (CreateExportJobResponse'),
    newCreateExportJobResponse,

    -- ** GetUserEndpoints
    GetUserEndpoints (GetUserEndpoints'),
    newGetUserEndpoints,
    GetUserEndpointsResponse (GetUserEndpointsResponse'),
    newGetUserEndpointsResponse,

    -- ** GetSegmentVersion
    GetSegmentVersion (GetSegmentVersion'),
    newGetSegmentVersion,
    GetSegmentVersionResponse (GetSegmentVersionResponse'),
    newGetSegmentVersionResponse,

    -- ** UpdateSmsChannel
    UpdateSmsChannel (UpdateSmsChannel'),
    newUpdateSmsChannel,
    UpdateSmsChannelResponse (UpdateSmsChannelResponse'),
    newUpdateSmsChannelResponse,

    -- ** CreateSegment
    CreateSegment (CreateSegment'),
    newCreateSegment,
    CreateSegmentResponse (CreateSegmentResponse'),
    newCreateSegmentResponse,

    -- ** DeleteSmsTemplate
    DeleteSmsTemplate (DeleteSmsTemplate'),
    newDeleteSmsTemplate,
    DeleteSmsTemplateResponse (DeleteSmsTemplateResponse'),
    newDeleteSmsTemplateResponse,

    -- ** UpdateSmsTemplate
    UpdateSmsTemplate (UpdateSmsTemplate'),
    newUpdateSmsTemplate,
    UpdateSmsTemplateResponse (UpdateSmsTemplateResponse'),
    newUpdateSmsTemplateResponse,

    -- ** GetGcmChannel
    GetGcmChannel (GetGcmChannel'),
    newGetGcmChannel,
    GetGcmChannelResponse (GetGcmChannelResponse'),
    newGetGcmChannelResponse,

    -- ** DeleteVoiceChannel
    DeleteVoiceChannel (DeleteVoiceChannel'),
    newDeleteVoiceChannel,
    DeleteVoiceChannelResponse (DeleteVoiceChannelResponse'),
    newDeleteVoiceChannelResponse,

    -- ** UpdateVoiceChannel
    UpdateVoiceChannel (UpdateVoiceChannel'),
    newUpdateVoiceChannel,
    UpdateVoiceChannelResponse (UpdateVoiceChannelResponse'),
    newUpdateVoiceChannelResponse,

    -- ** GetApnsVoipSandboxChannel
    GetApnsVoipSandboxChannel (GetApnsVoipSandboxChannel'),
    newGetApnsVoipSandboxChannel,
    GetApnsVoipSandboxChannelResponse (GetApnsVoipSandboxChannelResponse'),
    newGetApnsVoipSandboxChannelResponse,

    -- ** DeleteJourney
    DeleteJourney (DeleteJourney'),
    newDeleteJourney,
    DeleteJourneyResponse (DeleteJourneyResponse'),
    newDeleteJourneyResponse,

    -- ** GetCampaignDateRangeKpi
    GetCampaignDateRangeKpi (GetCampaignDateRangeKpi'),
    newGetCampaignDateRangeKpi,
    GetCampaignDateRangeKpiResponse (GetCampaignDateRangeKpiResponse'),
    newGetCampaignDateRangeKpiResponse,

    -- ** UpdateJourney
    UpdateJourney (UpdateJourney'),
    newUpdateJourney,
    UpdateJourneyResponse (UpdateJourneyResponse'),
    newUpdateJourneyResponse,

    -- ** ListTemplates
    ListTemplates (ListTemplates'),
    newListTemplates,
    ListTemplatesResponse (ListTemplatesResponse'),
    newListTemplatesResponse,

    -- ** DeleteBaiduChannel
    DeleteBaiduChannel (DeleteBaiduChannel'),
    newDeleteBaiduChannel,
    DeleteBaiduChannelResponse (DeleteBaiduChannelResponse'),
    newDeleteBaiduChannelResponse,

    -- ** GetCampaignVersions
    GetCampaignVersions (GetCampaignVersions'),
    newGetCampaignVersions,
    GetCampaignVersionsResponse (GetCampaignVersionsResponse'),
    newGetCampaignVersionsResponse,

    -- ** GetApplicationSettings
    GetApplicationSettings (GetApplicationSettings'),
    newGetApplicationSettings,
    GetApplicationSettingsResponse (GetApplicationSettingsResponse'),
    newGetApplicationSettingsResponse,

    -- ** GetApnsVoipChannel
    GetApnsVoipChannel (GetApnsVoipChannel'),
    newGetApnsVoipChannel,
    GetApnsVoipChannelResponse (GetApnsVoipChannelResponse'),
    newGetApnsVoipChannelResponse,

    -- ** ListJourneys
    ListJourneys (ListJourneys'),
    newListJourneys,
    ListJourneysResponse (ListJourneysResponse'),
    newListJourneysResponse,

    -- ** DeleteEventStream
    DeleteEventStream (DeleteEventStream'),
    newDeleteEventStream,
    DeleteEventStreamResponse (DeleteEventStreamResponse'),
    newDeleteEventStreamResponse,

    -- ** UpdateBaiduChannel
    UpdateBaiduChannel (UpdateBaiduChannel'),
    newUpdateBaiduChannel,
    UpdateBaiduChannelResponse (UpdateBaiduChannelResponse'),
    newUpdateBaiduChannelResponse,

    -- ** GetExportJobs
    GetExportJobs (GetExportJobs'),
    newGetExportJobs,
    GetExportJobsResponse (GetExportJobsResponse'),
    newGetExportJobsResponse,

    -- ** GetSegments
    GetSegments (GetSegments'),
    newGetSegments,
    GetSegmentsResponse (GetSegmentsResponse'),
    newGetSegmentsResponse,

    -- ** GetJourney
    GetJourney (GetJourney'),
    newGetJourney,
    GetJourneyResponse (GetJourneyResponse'),
    newGetJourneyResponse,

    -- ** PutEvents
    PutEvents (PutEvents'),
    newPutEvents,
    PutEventsResponse (PutEventsResponse'),
    newPutEventsResponse,

    -- ** DeleteApnsVoipChannel
    DeleteApnsVoipChannel (DeleteApnsVoipChannel'),
    newDeleteApnsVoipChannel,
    DeleteApnsVoipChannelResponse (DeleteApnsVoipChannelResponse'),
    newDeleteApnsVoipChannelResponse,

    -- ** UpdateApnsVoipChannel
    UpdateApnsVoipChannel (UpdateApnsVoipChannel'),
    newUpdateApnsVoipChannel,
    UpdateApnsVoipChannelResponse (UpdateApnsVoipChannelResponse'),
    newUpdateApnsVoipChannelResponse,

    -- ** CreateImportJob
    CreateImportJob (CreateImportJob'),
    newCreateImportJob,
    CreateImportJobResponse (CreateImportJobResponse'),
    newCreateImportJobResponse,

    -- ** UpdateEmailChannel
    UpdateEmailChannel (UpdateEmailChannel'),
    newUpdateEmailChannel,
    UpdateEmailChannelResponse (UpdateEmailChannelResponse'),
    newUpdateEmailChannelResponse,

    -- ** GetEventStream
    GetEventStream (GetEventStream'),
    newGetEventStream,
    GetEventStreamResponse (GetEventStreamResponse'),
    newGetEventStreamResponse,

    -- ** SendUsersMessages
    SendUsersMessages (SendUsersMessages'),
    newSendUsersMessages,
    SendUsersMessagesResponse (SendUsersMessagesResponse'),
    newSendUsersMessagesResponse,

    -- ** DeleteEmailChannel
    DeleteEmailChannel (DeleteEmailChannel'),
    newDeleteEmailChannel,
    DeleteEmailChannelResponse (DeleteEmailChannelResponse'),
    newDeleteEmailChannelResponse,

    -- ** UpdateApplicationSettings
    UpdateApplicationSettings (UpdateApplicationSettings'),
    newUpdateApplicationSettings,
    UpdateApplicationSettingsResponse (UpdateApplicationSettingsResponse'),
    newUpdateApplicationSettingsResponse,

    -- ** UpdateJourneyState
    UpdateJourneyState (UpdateJourneyState'),
    newUpdateJourneyState,
    UpdateJourneyStateResponse (UpdateJourneyStateResponse'),
    newUpdateJourneyStateResponse,

    -- ** ListTemplateVersions
    ListTemplateVersions (ListTemplateVersions'),
    newListTemplateVersions,
    ListTemplateVersionsResponse (ListTemplateVersionsResponse'),
    newListTemplateVersionsResponse,

    -- ** DeleteApnsSandboxChannel
    DeleteApnsSandboxChannel (DeleteApnsSandboxChannel'),
    newDeleteApnsSandboxChannel,
    DeleteApnsSandboxChannelResponse (DeleteApnsSandboxChannelResponse'),
    newDeleteApnsSandboxChannelResponse,

    -- ** GetCampaignActivities
    GetCampaignActivities (GetCampaignActivities'),
    newGetCampaignActivities,
    GetCampaignActivitiesResponse (GetCampaignActivitiesResponse'),
    newGetCampaignActivitiesResponse,

    -- ** UpdateApnsSandboxChannel
    UpdateApnsSandboxChannel (UpdateApnsSandboxChannel'),
    newUpdateApnsSandboxChannel,
    UpdateApnsSandboxChannelResponse (UpdateApnsSandboxChannelResponse'),
    newUpdateApnsSandboxChannelResponse,

    -- ** GetCampaigns
    GetCampaigns (GetCampaigns'),
    newGetCampaigns,
    GetCampaignsResponse (GetCampaignsResponse'),
    newGetCampaignsResponse,

    -- ** GetSmsTemplate
    GetSmsTemplate (GetSmsTemplate'),
    newGetSmsTemplate,
    GetSmsTemplateResponse (GetSmsTemplateResponse'),
    newGetSmsTemplateResponse,

    -- ** GetPushTemplate
    GetPushTemplate (GetPushTemplate'),
    newGetPushTemplate,
    GetPushTemplateResponse (GetPushTemplateResponse'),
    newGetPushTemplateResponse,

    -- ** GetCampaign
    GetCampaign (GetCampaign'),
    newGetCampaign,
    GetCampaignResponse (GetCampaignResponse'),
    newGetCampaignResponse,

    -- ** DeleteApp
    DeleteApp (DeleteApp'),
    newDeleteApp,
    DeleteAppResponse (DeleteAppResponse'),
    newDeleteAppResponse,

    -- ** DeleteUserEndpoints
    DeleteUserEndpoints (DeleteUserEndpoints'),
    newDeleteUserEndpoints,
    DeleteUserEndpointsResponse (DeleteUserEndpointsResponse'),
    newDeleteUserEndpointsResponse,

    -- ** CreateEmailTemplate
    CreateEmailTemplate (CreateEmailTemplate'),
    newCreateEmailTemplate,
    CreateEmailTemplateResponse (CreateEmailTemplateResponse'),
    newCreateEmailTemplateResponse,

    -- ** UpdateEmailTemplate
    UpdateEmailTemplate (UpdateEmailTemplate'),
    newUpdateEmailTemplate,
    UpdateEmailTemplateResponse (UpdateEmailTemplateResponse'),
    newUpdateEmailTemplateResponse,

    -- ** DeleteEmailTemplate
    DeleteEmailTemplate (DeleteEmailTemplate'),
    newDeleteEmailTemplate,
    DeleteEmailTemplateResponse (DeleteEmailTemplateResponse'),
    newDeleteEmailTemplateResponse,

    -- ** CreateApp
    CreateApp (CreateApp'),
    newCreateApp,
    CreateAppResponse (CreateAppResponse'),
    newCreateAppResponse,

    -- ** UpdateEndpointsBatch
    UpdateEndpointsBatch (UpdateEndpointsBatch'),
    newUpdateEndpointsBatch,
    UpdateEndpointsBatchResponse (UpdateEndpointsBatchResponse'),
    newUpdateEndpointsBatchResponse,

    -- ** GetExportJob
    GetExportJob (GetExportJob'),
    newGetExportJob,
    GetExportJobResponse (GetExportJobResponse'),
    newGetExportJobResponse,

    -- ** GetSegment
    GetSegment (GetSegment'),
    newGetSegment,
    GetSegmentResponse (GetSegmentResponse'),
    newGetSegmentResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** GetCampaignVersion
    GetCampaignVersion (GetCampaignVersion'),
    newGetCampaignVersion,
    GetCampaignVersionResponse (GetCampaignVersionResponse'),
    newGetCampaignVersionResponse,

    -- ** CreateVoiceTemplate
    CreateVoiceTemplate (CreateVoiceTemplate'),
    newCreateVoiceTemplate,
    CreateVoiceTemplateResponse (CreateVoiceTemplateResponse'),
    newCreateVoiceTemplateResponse,

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

    -- ** ItemResponse
    ItemResponse (ItemResponse'),
    newItemResponse,

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
import Network.AWS.Pinpoint.DeleteAdmChannel
import Network.AWS.Pinpoint.DeleteApnsChannel
import Network.AWS.Pinpoint.DeleteApnsSandboxChannel
import Network.AWS.Pinpoint.DeleteApnsVoipChannel
import Network.AWS.Pinpoint.DeleteApnsVoipSandboxChannel
import Network.AWS.Pinpoint.DeleteApp
import Network.AWS.Pinpoint.DeleteBaiduChannel
import Network.AWS.Pinpoint.DeleteCampaign
import Network.AWS.Pinpoint.DeleteEmailChannel
import Network.AWS.Pinpoint.DeleteEmailTemplate
import Network.AWS.Pinpoint.DeleteEndpoint
import Network.AWS.Pinpoint.DeleteEventStream
import Network.AWS.Pinpoint.DeleteGcmChannel
import Network.AWS.Pinpoint.DeleteJourney
import Network.AWS.Pinpoint.DeletePushTemplate
import Network.AWS.Pinpoint.DeleteRecommenderConfiguration
import Network.AWS.Pinpoint.DeleteSegment
import Network.AWS.Pinpoint.DeleteSmsChannel
import Network.AWS.Pinpoint.DeleteSmsTemplate
import Network.AWS.Pinpoint.DeleteUserEndpoints
import Network.AWS.Pinpoint.DeleteVoiceChannel
import Network.AWS.Pinpoint.DeleteVoiceTemplate
import Network.AWS.Pinpoint.GetAdmChannel
import Network.AWS.Pinpoint.GetApnsChannel
import Network.AWS.Pinpoint.GetApnsSandboxChannel
import Network.AWS.Pinpoint.GetApnsVoipChannel
import Network.AWS.Pinpoint.GetApnsVoipSandboxChannel
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
import Network.AWS.Pinpoint.GetGcmChannel
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
import Network.AWS.Pinpoint.Lens
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
import Network.AWS.Pinpoint.UpdateAdmChannel
import Network.AWS.Pinpoint.UpdateApnsChannel
import Network.AWS.Pinpoint.UpdateApnsSandboxChannel
import Network.AWS.Pinpoint.UpdateApnsVoipChannel
import Network.AWS.Pinpoint.UpdateApnsVoipSandboxChannel
import Network.AWS.Pinpoint.UpdateApplicationSettings
import Network.AWS.Pinpoint.UpdateBaiduChannel
import Network.AWS.Pinpoint.UpdateCampaign
import Network.AWS.Pinpoint.UpdateEmailChannel
import Network.AWS.Pinpoint.UpdateEmailTemplate
import Network.AWS.Pinpoint.UpdateEndpoint
import Network.AWS.Pinpoint.UpdateEndpointsBatch
import Network.AWS.Pinpoint.UpdateGcmChannel
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
