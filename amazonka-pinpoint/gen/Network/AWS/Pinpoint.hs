{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint
-- Copyright   : (c) 2013-2018 Brendan Hay
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
--
module Network.AWS.Pinpoint
    (
    -- * Service Configuration
      pinpoint

    -- * Errors
    -- $errors

    -- ** ForbiddenException
    , _ForbiddenException

    -- ** NotFoundException
    , _NotFoundException

    -- ** TooManyRequestsException
    , _TooManyRequestsException

    -- ** InternalServerErrorException
    , _InternalServerErrorException

    -- ** MethodNotAllowedException
    , _MethodNotAllowedException

    -- ** BadRequestException
    , _BadRequestException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** GetGCMChannel
    , module Network.AWS.Pinpoint.GetGCMChannel

    -- ** GetSegmentImportJobs
    , module Network.AWS.Pinpoint.GetSegmentImportJobs

    -- ** SendMessages
    , module Network.AWS.Pinpoint.SendMessages

    -- ** GetImportJob
    , module Network.AWS.Pinpoint.GetImportJob

    -- ** GetAPNSVoipSandboxChannel
    , module Network.AWS.Pinpoint.GetAPNSVoipSandboxChannel

    -- ** GetSegmentVersions
    , module Network.AWS.Pinpoint.GetSegmentVersions

    -- ** DeleteCampaign
    , module Network.AWS.Pinpoint.DeleteCampaign

    -- ** UpdateCampaign
    , module Network.AWS.Pinpoint.UpdateCampaign

    -- ** GetSegmentVersion
    , module Network.AWS.Pinpoint.GetSegmentVersion

    -- ** CreateExportJob
    , module Network.AWS.Pinpoint.CreateExportJob

    -- ** CreateSegment
    , module Network.AWS.Pinpoint.CreateSegment

    -- ** UpdateADMChannel
    , module Network.AWS.Pinpoint.UpdateADMChannel

    -- ** DeleteADMChannel
    , module Network.AWS.Pinpoint.DeleteADMChannel

    -- ** DeleteEndpoint
    , module Network.AWS.Pinpoint.DeleteEndpoint

    -- ** UpdateEndpoint
    , module Network.AWS.Pinpoint.UpdateEndpoint

    -- ** ListTagsForResource
    , module Network.AWS.Pinpoint.ListTagsForResource

    -- ** CreateCampaign
    , module Network.AWS.Pinpoint.CreateCampaign

    -- ** GetExportJob
    , module Network.AWS.Pinpoint.GetExportJob

    -- ** GetEndpoint
    , module Network.AWS.Pinpoint.GetEndpoint

    -- ** GetSegment
    , module Network.AWS.Pinpoint.GetSegment

    -- ** UpdateEndpointsBatch
    , module Network.AWS.Pinpoint.UpdateEndpointsBatch

    -- ** GetADMChannel
    , module Network.AWS.Pinpoint.GetADMChannel

    -- ** GetCampaign
    , module Network.AWS.Pinpoint.GetCampaign

    -- ** DeleteUserEndpoints
    , module Network.AWS.Pinpoint.DeleteUserEndpoints

    -- ** DeleteApp
    , module Network.AWS.Pinpoint.DeleteApp

    -- ** UpdateAPNSVoipSandboxChannel
    , module Network.AWS.Pinpoint.UpdateAPNSVoipSandboxChannel

    -- ** DeleteAPNSVoipSandboxChannel
    , module Network.AWS.Pinpoint.DeleteAPNSVoipSandboxChannel

    -- ** UpdateGCMChannel
    , module Network.AWS.Pinpoint.UpdateGCMChannel

    -- ** DeleteGCMChannel
    , module Network.AWS.Pinpoint.DeleteGCMChannel

    -- ** GetCampaignActivities
    , module Network.AWS.Pinpoint.GetCampaignActivities

    -- ** GetEventStream
    , module Network.AWS.Pinpoint.GetEventStream

    -- ** GetChannels
    , module Network.AWS.Pinpoint.GetChannels

    -- ** DeleteEmailChannel
    , module Network.AWS.Pinpoint.DeleteEmailChannel

    -- ** UpdateEmailChannel
    , module Network.AWS.Pinpoint.UpdateEmailChannel

    -- ** GetBaiduChannel
    , module Network.AWS.Pinpoint.GetBaiduChannel

    -- ** DeleteAPNSChannel
    , module Network.AWS.Pinpoint.DeleteAPNSChannel

    -- ** UpdateAPNSChannel
    , module Network.AWS.Pinpoint.UpdateAPNSChannel

    -- ** RemoveAttributes
    , module Network.AWS.Pinpoint.RemoveAttributes

    -- ** PutEventStream
    , module Network.AWS.Pinpoint.PutEventStream

    -- ** DeleteEventStream
    , module Network.AWS.Pinpoint.DeleteEventStream

    -- ** GetCampaignVersions
    , module Network.AWS.Pinpoint.GetCampaignVersions

    -- ** GetAPNSChannel
    , module Network.AWS.Pinpoint.GetAPNSChannel

    -- ** UpdateVoiceChannel
    , module Network.AWS.Pinpoint.UpdateVoiceChannel

    -- ** DeleteVoiceChannel
    , module Network.AWS.Pinpoint.DeleteVoiceChannel

    -- ** GetApps
    , module Network.AWS.Pinpoint.GetApps

    -- ** GetAPNSSandboxChannel
    , module Network.AWS.Pinpoint.GetAPNSSandboxChannel

    -- ** GetUserEndpoints
    , module Network.AWS.Pinpoint.GetUserEndpoints

    -- ** GetImportJobs
    , module Network.AWS.Pinpoint.GetImportJobs

    -- ** DeleteSmsChannel
    , module Network.AWS.Pinpoint.DeleteSmsChannel

    -- ** UpdateSmsChannel
    , module Network.AWS.Pinpoint.UpdateSmsChannel

    -- ** GetApp
    , module Network.AWS.Pinpoint.GetApp

    -- ** GetCampaignVersion
    , module Network.AWS.Pinpoint.GetCampaignVersion

    -- ** DeleteSegment
    , module Network.AWS.Pinpoint.DeleteSegment

    -- ** UpdateSegment
    , module Network.AWS.Pinpoint.UpdateSegment

    -- ** CreateApp
    , module Network.AWS.Pinpoint.CreateApp

    -- ** GetSegmentExportJobs
    , module Network.AWS.Pinpoint.GetSegmentExportJobs

    -- ** GetSmsChannel
    , module Network.AWS.Pinpoint.GetSmsChannel

    -- ** TagResource
    , module Network.AWS.Pinpoint.TagResource

    -- ** DeleteAPNSSandboxChannel
    , module Network.AWS.Pinpoint.DeleteAPNSSandboxChannel

    -- ** UpdateAPNSSandboxChannel
    , module Network.AWS.Pinpoint.UpdateAPNSSandboxChannel

    -- ** GetCampaigns
    , module Network.AWS.Pinpoint.GetCampaigns

    -- ** GetVoiceChannel
    , module Network.AWS.Pinpoint.GetVoiceChannel

    -- ** UntagResource
    , module Network.AWS.Pinpoint.UntagResource

    -- ** PutEvents
    , module Network.AWS.Pinpoint.PutEvents

    -- ** UpdateApplicationSettings
    , module Network.AWS.Pinpoint.UpdateApplicationSettings

    -- ** GetSegments
    , module Network.AWS.Pinpoint.GetSegments

    -- ** GetExportJobs
    , module Network.AWS.Pinpoint.GetExportJobs

    -- ** CreateImportJob
    , module Network.AWS.Pinpoint.CreateImportJob

    -- ** DeleteAPNSVoipChannel
    , module Network.AWS.Pinpoint.DeleteAPNSVoipChannel

    -- ** UpdateAPNSVoipChannel
    , module Network.AWS.Pinpoint.UpdateAPNSVoipChannel

    -- ** SendUsersMessages
    , module Network.AWS.Pinpoint.SendUsersMessages

    -- ** GetApplicationSettings
    , module Network.AWS.Pinpoint.GetApplicationSettings

    -- ** DeleteBaiduChannel
    , module Network.AWS.Pinpoint.DeleteBaiduChannel

    -- ** UpdateBaiduChannel
    , module Network.AWS.Pinpoint.UpdateBaiduChannel

    -- ** PhoneNumberValidate
    , module Network.AWS.Pinpoint.PhoneNumberValidate

    -- ** GetAPNSVoipChannel
    , module Network.AWS.Pinpoint.GetAPNSVoipChannel

    -- ** GetEmailChannel
    , module Network.AWS.Pinpoint.GetEmailChannel

    -- * Types

    -- ** Action
    , Action (..)

    -- ** AttributeType
    , AttributeType (..)

    -- ** CampaignStatus
    , CampaignStatus (..)

    -- ** ChannelType
    , ChannelType (..)

    -- ** DefinitionFormat
    , DefinitionFormat (..)

    -- ** DeliveryStatus
    , DeliveryStatus (..)

    -- ** DimensionType
    , DimensionType (..)

    -- ** Duration
    , Duration (..)

    -- ** FilterType
    , FilterType (..)

    -- ** Frequency
    , Frequency (..)

    -- ** Include
    , Include (..)

    -- ** JobStatus
    , JobStatus (..)

    -- ** MessageType
    , MessageType (..)

    -- ** Mode
    , Mode (..)

    -- ** RecencyType
    , RecencyType (..)

    -- ** SegmentType
    , SegmentType (..)

    -- ** SourceType
    , SourceType (..)

    -- ** Type
    , Type (..)

    -- ** ADMChannelRequest
    , ADMChannelRequest
    , aDMChannelRequest
    , admcrClientId
    , admcrClientSecret
    , admcrEnabled

    -- ** ADMChannelResponse
    , ADMChannelResponse
    , aDMChannelResponse
    , admcPlatform
    , admcLastModifiedDate
    , admcEnabled
    , admcIsArchived
    , admcApplicationId
    , admcVersion
    , admcId
    , admcCreationDate
    , admcLastModifiedBy
    , admcHasCredential

    -- ** ADMMessage
    , ADMMessage
    , aDMMessage
    , admmSubstitutions
    , admmExpiresAfter
    , admmMD5
    , admmSilentPush
    , admmImageIconURL
    , admmRawContent
    , admmData
    , admmSmallImageIconURL
    , admmBody
    , admmURL
    , admmSound
    , admmAction
    , admmImageURL
    , admmConsolidationKey
    , admmTitle
    , admmIconReference

    -- ** APNSChannelRequest
    , APNSChannelRequest
    , apnsChannelRequest
    , acrTokenKey
    , acrPrivateKey
    , acrEnabled
    , acrTeamId
    , acrBundleId
    , acrDefaultAuthenticationMethod
    , acrCertificate
    , acrTokenKeyId

    -- ** APNSChannelResponse
    , APNSChannelResponse
    , apnsChannelResponse
    , acPlatform
    , acLastModifiedDate
    , acEnabled
    , acHasTokenKey
    , acDefaultAuthenticationMethod
    , acIsArchived
    , acApplicationId
    , acVersion
    , acId
    , acCreationDate
    , acLastModifiedBy
    , acHasCredential

    -- ** APNSMessage
    , APNSMessage
    , apnsMessage
    , amSubstitutions
    , amSilentPush
    , amPriority
    , amRawContent
    , amData
    , amBody
    , amCategory
    , amTimeToLive
    , amURL
    , amSound
    , amAction
    , amMediaURL
    , amPreferredAuthenticationMethod
    , amBadge
    , amTitle
    , amThreadId
    , amCollapseId

    -- ** APNSSandboxChannelRequest
    , APNSSandboxChannelRequest
    , apnsSandboxChannelRequest
    , ascrTokenKey
    , ascrPrivateKey
    , ascrEnabled
    , ascrTeamId
    , ascrBundleId
    , ascrDefaultAuthenticationMethod
    , ascrCertificate
    , ascrTokenKeyId

    -- ** APNSSandboxChannelResponse
    , APNSSandboxChannelResponse
    , apnsSandboxChannelResponse
    , ascPlatform
    , ascLastModifiedDate
    , ascEnabled
    , ascHasTokenKey
    , ascDefaultAuthenticationMethod
    , ascIsArchived
    , ascApplicationId
    , ascVersion
    , ascId
    , ascCreationDate
    , ascLastModifiedBy
    , ascHasCredential

    -- ** APNSVoipChannelRequest
    , APNSVoipChannelRequest
    , apnsVoipChannelRequest
    , avcrTokenKey
    , avcrPrivateKey
    , avcrEnabled
    , avcrTeamId
    , avcrBundleId
    , avcrDefaultAuthenticationMethod
    , avcrCertificate
    , avcrTokenKeyId

    -- ** APNSVoipChannelResponse
    , APNSVoipChannelResponse
    , apnsVoipChannelResponse
    , avcPlatform
    , avcLastModifiedDate
    , avcEnabled
    , avcHasTokenKey
    , avcDefaultAuthenticationMethod
    , avcIsArchived
    , avcApplicationId
    , avcVersion
    , avcId
    , avcCreationDate
    , avcLastModifiedBy
    , avcHasCredential

    -- ** APNSVoipSandboxChannelRequest
    , APNSVoipSandboxChannelRequest
    , apnsVoipSandboxChannelRequest
    , avscrTokenKey
    , avscrPrivateKey
    , avscrEnabled
    , avscrTeamId
    , avscrBundleId
    , avscrDefaultAuthenticationMethod
    , avscrCertificate
    , avscrTokenKeyId

    -- ** APNSVoipSandboxChannelResponse
    , APNSVoipSandboxChannelResponse
    , apnsVoipSandboxChannelResponse
    , avscPlatform
    , avscLastModifiedDate
    , avscEnabled
    , avscHasTokenKey
    , avscDefaultAuthenticationMethod
    , avscIsArchived
    , avscApplicationId
    , avscVersion
    , avscId
    , avscCreationDate
    , avscLastModifiedBy
    , avscHasCredential

    -- ** ActivitiesResponse
    , ActivitiesResponse
    , activitiesResponse
    , aNextToken
    , aItem

    -- ** ActivityResponse
    , ActivityResponse
    , activityResponse
    , aState
    , aStart
    , aCampaignId
    , aTimezonesCompletedCount
    , aTimezonesTotalCount
    , aResult
    , aTreatmentId
    , aSuccessfulEndpointCount
    , aEnd
    , aApplicationId
    , aTotalEndpointCount
    , aId
    , aScheduledStart

    -- ** AddressConfiguration
    , AddressConfiguration
    , addressConfiguration
    , acSubstitutions
    , acTitleOverride
    , acContext
    , acRawContent
    , acBodyOverride
    , acChannelType

    -- ** ApplicationResponse
    , ApplicationResponse
    , applicationResponse
    , appARN
    , appName
    , appId
    , appTags

    -- ** ApplicationSettingsResource
    , ApplicationSettingsResource
    , applicationSettingsResource
    , asrLastModifiedDate
    , asrLimits
    , asrQuietTime
    , asrApplicationId
    , asrCampaignHook

    -- ** ApplicationsResponse
    , ApplicationsResponse
    , applicationsResponse
    , appNextToken
    , appItem

    -- ** AttributeDimension
    , AttributeDimension
    , attributeDimension
    , adValues
    , adAttributeType

    -- ** AttributesResource
    , AttributesResource
    , attributesResource
    , arAttributeType
    , arApplicationId
    , arAttributes

    -- ** BaiduChannelRequest
    , BaiduChannelRequest
    , baiduChannelRequest
    , bcrAPIKey
    , bcrEnabled
    , bcrSecretKey

    -- ** BaiduChannelResponse
    , BaiduChannelResponse
    , baiduChannelResponse
    , bcPlatform
    , bcLastModifiedDate
    , bcEnabled
    , bcCredential
    , bcIsArchived
    , bcApplicationId
    , bcVersion
    , bcId
    , bcCreationDate
    , bcLastModifiedBy
    , bcHasCredential

    -- ** BaiduMessage
    , BaiduMessage
    , baiduMessage
    , bmSubstitutions
    , bmSilentPush
    , bmImageIconURL
    , bmRawContent
    , bmData
    , bmSmallImageIconURL
    , bmBody
    , bmTimeToLive
    , bmURL
    , bmSound
    , bmAction
    , bmImageURL
    , bmTitle
    , bmIconReference

    -- ** CampaignEmailMessage
    , CampaignEmailMessage
    , campaignEmailMessage
    , cemBody
    , cemFromAddress
    , cemHTMLBody
    , cemTitle

    -- ** CampaignEventFilter
    , CampaignEventFilter
    , campaignEventFilter
    , cefFilterType
    , cefDimensions

    -- ** CampaignHook
    , CampaignHook
    , campaignHook
    , chLambdaFunctionName
    , chMode
    , chWebURL

    -- ** CampaignLimits
    , CampaignLimits
    , campaignLimits
    , clMessagesPerSecond
    , clDaily
    , clTotal
    , clMaximumDuration

    -- ** CampaignResponse
    , CampaignResponse
    , campaignResponse
    , cState
    , cLastModifiedDate
    , cARN
    , cSchedule
    , cHook
    , cTreatmentName
    , cLimits
    , cIsPaused
    , cDefaultState
    , cApplicationId
    , cName
    , cVersion
    , cHoldoutPercent
    , cTreatmentDescription
    , cId
    , cCreationDate
    , cMessageConfiguration
    , cDescription
    , cSegmentId
    , cAdditionalTreatments
    , cTags
    , cSegmentVersion

    -- ** CampaignSmsMessage
    , CampaignSmsMessage
    , campaignSmsMessage
    , csmBody
    , csmMessageType
    , csmSenderId

    -- ** CampaignState
    , CampaignState
    , campaignState
    , csCampaignStatus

    -- ** CampaignsResponse
    , CampaignsResponse
    , campaignsResponse
    , cNextToken
    , cItem

    -- ** ChannelResponse
    , ChannelResponse
    , channelResponse
    , chaLastModifiedDate
    , chaEnabled
    , chaIsArchived
    , chaApplicationId
    , chaVersion
    , chaId
    , chaCreationDate
    , chaLastModifiedBy
    , chaHasCredential

    -- ** ChannelsResponse
    , ChannelsResponse
    , channelsResponse
    , cChannels

    -- ** CreateApplicationRequest
    , CreateApplicationRequest
    , createApplicationRequest
    , carName
    , carTags

    -- ** DefaultMessage
    , DefaultMessage
    , defaultMessage
    , dmSubstitutions
    , dmBody

    -- ** DefaultPushNotificationMessage
    , DefaultPushNotificationMessage
    , defaultPushNotificationMessage
    , dpnmSubstitutions
    , dpnmSilentPush
    , dpnmData
    , dpnmBody
    , dpnmURL
    , dpnmAction
    , dpnmTitle

    -- ** DirectMessageConfiguration
    , DirectMessageConfiguration
    , directMessageConfiguration
    , dmcAPNSMessage
    , dmcGCMMessage
    , dmcDefaultMessage
    , dmcADMMessage
    , dmcSMSMessage
    , dmcEmailMessage
    , dmcVoiceMessage
    , dmcBaiduMessage
    , dmcDefaultPushNotificationMessage

    -- ** EmailChannelRequest
    , EmailChannelRequest
    , emailChannelRequest
    , ecrEnabled
    , ecrFromAddress
    , ecrConfigurationSet
    , ecrIdentity
    , ecrRoleARN

    -- ** EmailChannelResponse
    , EmailChannelResponse
    , emailChannelResponse
    , ecPlatform
    , ecMessagesPerSecond
    , ecLastModifiedDate
    , ecEnabled
    , ecFromAddress
    , ecIsArchived
    , ecApplicationId
    , ecVersion
    , ecConfigurationSet
    , ecId
    , ecCreationDate
    , ecLastModifiedBy
    , ecIdentity
    , ecHasCredential
    , ecRoleARN

    -- ** EmailMessage
    , EmailMessage
    , emailMessage
    , emSubstitutions
    , emBody
    , emFromAddress
    , emRawEmail
    , emFeedbackForwardingAddress
    , emSimpleEmail
    , emReplyToAddresses

    -- ** EndpointBatchItem
    , EndpointBatchItem
    , endpointBatchItem
    , ebiRequestId
    , ebiMetrics
    , ebiLocation
    , ebiDemographic
    , ebiAddress
    , ebiEffectiveDate
    , ebiUser
    , ebiAttributes
    , ebiEndpointStatus
    , ebiOptOut
    , ebiId
    , ebiChannelType

    -- ** EndpointBatchRequest
    , EndpointBatchRequest
    , endpointBatchRequest
    , ebrItem

    -- ** EndpointDemographic
    , EndpointDemographic
    , endpointDemographic
    , edPlatform
    , edPlatformVersion
    , edLocale
    , edAppVersion
    , edModel
    , edMake
    , edModelVersion
    , edTimezone

    -- ** EndpointItemResponse
    , EndpointItemResponse
    , endpointItemResponse
    , eiMessage
    , eiStatusCode

    -- ** EndpointLocation
    , EndpointLocation
    , endpointLocation
    , elPostalCode
    , elLatitude
    , elCountry
    , elCity
    , elRegion
    , elLongitude

    -- ** EndpointMessageResult
    , EndpointMessageResult
    , endpointMessageResult
    , emrDeliveryStatus
    , emrAddress
    , emrStatusMessage
    , emrUpdatedToken
    , emrMessageId
    , emrStatusCode

    -- ** EndpointRequest
    , EndpointRequest
    , endpointRequest
    , erRequestId
    , erMetrics
    , erLocation
    , erDemographic
    , erAddress
    , erEffectiveDate
    , erUser
    , erAttributes
    , erEndpointStatus
    , erOptOut
    , erChannelType

    -- ** EndpointResponse
    , EndpointResponse
    , endpointResponse
    , endRequestId
    , endMetrics
    , endLocation
    , endDemographic
    , endCohortId
    , endAddress
    , endEffectiveDate
    , endUser
    , endApplicationId
    , endAttributes
    , endEndpointStatus
    , endOptOut
    , endId
    , endCreationDate
    , endChannelType

    -- ** EndpointSendConfiguration
    , EndpointSendConfiguration
    , endpointSendConfiguration
    , escSubstitutions
    , escTitleOverride
    , escContext
    , escRawContent
    , escBodyOverride

    -- ** EndpointUser
    , EndpointUser
    , endpointUser
    , euUserAttributes
    , euUserId

    -- ** EndpointsResponse
    , EndpointsResponse
    , endpointsResponse
    , eItem

    -- ** Event
    , Event
    , event
    , eClientSDKVersion
    , eMetrics
    , eAppVersionCode
    , eAppTitle
    , eEventType
    , eAppPackageName
    , eAttributes
    , eSDKName
    , eTimestamp
    , eSession

    -- ** EventDimensions
    , EventDimensions
    , eventDimensions
    , edMetrics
    , edEventType
    , edAttributes

    -- ** EventItemResponse
    , EventItemResponse
    , eventItemResponse
    , eMessage
    , eStatusCode

    -- ** EventStream
    , EventStream
    , eventStream
    , esLastUpdatedBy
    , esLastModifiedDate
    , esDestinationStreamARN
    , esApplicationId
    , esExternalId
    , esRoleARN

    -- ** EventsBatch
    , EventsBatch
    , eventsBatch
    , ebEvents
    , ebEndpoint

    -- ** EventsRequest
    , EventsRequest
    , eventsRequest
    , erBatchItem

    -- ** EventsResponse
    , EventsResponse
    , eventsResponse
    , eResults

    -- ** ExportJobRequest
    , ExportJobRequest
    , exportJobRequest
    , eS3URLPrefix
    , eSegmentId
    , eRoleARN
    , eSegmentVersion

    -- ** ExportJobResource
    , ExportJobResource
    , exportJobResource
    , ejrS3URLPrefix
    , ejrSegmentId
    , ejrRoleARN
    , ejrSegmentVersion

    -- ** ExportJobResponse
    , ExportJobResponse
    , exportJobResponse
    , ejCompletedPieces
    , ejFailedPieces
    , ejDefinition
    , ejTotalProcessed
    , ejFailures
    , ejTotalPieces
    , ejApplicationId
    , ejId
    , ejCreationDate
    , ejType
    , ejCompletionDate
    , ejJobStatus
    , ejTotalFailures

    -- ** ExportJobsResponse
    , ExportJobsResponse
    , exportJobsResponse
    , ejNextToken
    , ejItem

    -- ** GCMChannelRequest
    , GCMChannelRequest
    , gcmChannelRequest
    , gcrAPIKey
    , gcrEnabled

    -- ** GCMChannelResponse
    , GCMChannelResponse
    , gcmChannelResponse
    , gcPlatform
    , gcLastModifiedDate
    , gcEnabled
    , gcCredential
    , gcIsArchived
    , gcApplicationId
    , gcVersion
    , gcId
    , gcCreationDate
    , gcLastModifiedBy
    , gcHasCredential

    -- ** GCMMessage
    , GCMMessage
    , gcmMessage
    , gmSubstitutions
    , gmSilentPush
    , gmImageIconURL
    , gmPriority
    , gmRawContent
    , gmData
    , gmRestrictedPackageName
    , gmSmallImageIconURL
    , gmBody
    , gmTimeToLive
    , gmURL
    , gmSound
    , gmAction
    , gmCollapseKey
    , gmImageURL
    , gmTitle
    , gmIconReference

    -- ** GPSCoordinates
    , GPSCoordinates
    , gPSCoordinates
    , gpscLatitude
    , gpscLongitude

    -- ** GPSPointDimension
    , GPSPointDimension
    , gPSPointDimension
    , gpspdCoordinates
    , gpspdRangeInKilometers

    -- ** ImportJobRequest
    , ImportJobRequest
    , importJobRequest
    , iSegmentName
    , iFormat
    , iDefineSegment
    , iRegisterEndpoints
    , iExternalId
    , iS3URL
    , iSegmentId
    , iRoleARN

    -- ** ImportJobResource
    , ImportJobResource
    , importJobResource
    , ijrSegmentName
    , ijrFormat
    , ijrDefineSegment
    , ijrRegisterEndpoints
    , ijrExternalId
    , ijrS3URL
    , ijrSegmentId
    , ijrRoleARN

    -- ** ImportJobResponse
    , ImportJobResponse
    , importJobResponse
    , ijCompletedPieces
    , ijFailedPieces
    , ijDefinition
    , ijTotalProcessed
    , ijFailures
    , ijTotalPieces
    , ijApplicationId
    , ijId
    , ijCreationDate
    , ijType
    , ijCompletionDate
    , ijJobStatus
    , ijTotalFailures

    -- ** ImportJobsResponse
    , ImportJobsResponse
    , importJobsResponse
    , ijNextToken
    , ijItem

    -- ** ItemResponse
    , ItemResponse
    , itemResponse
    , iEndpointItemResponse
    , iEventsItemResponse

    -- ** Message
    , Message
    , message
    , mSilentPush
    , mImageIconURL
    , mRawContent
    , mBody
    , mTimeToLive
    , mImageSmallIconURL
    , mJSONBody
    , mURL
    , mAction
    , mImageURL
    , mMediaURL
    , mTitle

    -- ** MessageBody
    , MessageBody
    , messageBody
    , mbRequestId
    , mbMessage

    -- ** MessageConfiguration
    , MessageConfiguration
    , messageConfiguration
    , mcAPNSMessage
    , mcGCMMessage
    , mcDefaultMessage
    , mcADMMessage
    , mcSMSMessage
    , mcEmailMessage
    , mcBaiduMessage

    -- ** MessageRequest
    , MessageRequest
    , messageRequest
    , mrTraceId
    , mrContext
    , mrAddresses
    , mrEndpoints
    , mrMessageConfiguration

    -- ** MessageResponse
    , MessageResponse
    , messageResponse
    , mRequestId
    , mResult
    , mApplicationId
    , mEndpointResult

    -- ** MessageResult
    , MessageResult
    , messageResult
    , mrDeliveryStatus
    , mrStatusMessage
    , mrUpdatedToken
    , mrMessageId
    , mrStatusCode

    -- ** MetricDimension
    , MetricDimension
    , metricDimension
    , mdValue
    , mdComparisonOperator

    -- ** NumberValidateRequest
    , NumberValidateRequest
    , numberValidateRequest
    , nvrIsoCountryCode
    , nvrPhoneNumber

    -- ** NumberValidateResponse
    , NumberValidateResponse
    , numberValidateResponse
    , nvCarrier
    , nvCounty
    , nvCountry
    , nvCountryCodeNumeric
    , nvZipCode
    , nvOriginalPhoneNumber
    , nvPhoneTypeCode
    , nvPhoneType
    , nvCity
    , nvCountryCodeIso2
    , nvTimezone
    , nvOriginalCountryCodeIso2
    , nvCleansedPhoneNumberNational
    , nvCleansedPhoneNumberE164

    -- ** PublicEndpoint
    , PublicEndpoint
    , publicEndpoint
    , peRequestId
    , peMetrics
    , peLocation
    , peDemographic
    , peAddress
    , peEffectiveDate
    , peUser
    , peAttributes
    , peEndpointStatus
    , peOptOut
    , peChannelType

    -- ** QuietTime
    , QuietTime
    , quietTime
    , qtStart
    , qtEnd

    -- ** RawEmail
    , RawEmail
    , rawEmail
    , reData

    -- ** RecencyDimension
    , RecencyDimension
    , recencyDimension
    , rdRecencyType
    , rdDuration

    -- ** SMSChannelRequest
    , SMSChannelRequest
    , sMSChannelRequest
    , smscrShortCode
    , smscrEnabled
    , smscrSenderId

    -- ** SMSChannelResponse
    , SMSChannelResponse
    , sMSChannelResponse
    , smscPlatform
    , smscShortCode
    , smscLastModifiedDate
    , smscEnabled
    , smscSenderId
    , smscTransactionalMessagesPerSecond
    , smscPromotionalMessagesPerSecond
    , smscIsArchived
    , smscApplicationId
    , smscVersion
    , smscId
    , smscCreationDate
    , smscLastModifiedBy
    , smscHasCredential

    -- ** SMSMessage
    , SMSMessage
    , sMSMessage
    , smsmSubstitutions
    , smsmOriginationNumber
    , smsmBody
    , smsmMessageType
    , smsmSenderId
    , smsmKeyword

    -- ** Schedule
    , Schedule
    , schedule
    , sFrequency
    , sStartTime
    , sQuietTime
    , sEventFilter
    , sIsLocalTime
    , sEndTime
    , sTimezone

    -- ** SegmentBehaviors
    , SegmentBehaviors
    , segmentBehaviors
    , sbRecency

    -- ** SegmentDemographics
    , SegmentDemographics
    , segmentDemographics
    , sdPlatform
    , sdAppVersion
    , sdChannel
    , sdModel
    , sdMake
    , sdDeviceType

    -- ** SegmentDimensions
    , SegmentDimensions
    , segmentDimensions
    , sdMetrics
    , sdLocation
    , sdDemographic
    , sdUserAttributes
    , sdBehavior
    , sdAttributes

    -- ** SegmentGroup
    , SegmentGroup
    , segmentGroup
    , sgSourceSegments
    , sgSourceType
    , sgType
    , sgDimensions

    -- ** SegmentGroupList
    , SegmentGroupList
    , segmentGroupList
    , sglInclude
    , sglGroups

    -- ** SegmentImportResource
    , SegmentImportResource
    , segmentImportResource
    , sirSize
    , sirFormat
    , sirChannelCounts
    , sirExternalId
    , sirS3URL
    , sirRoleARN

    -- ** SegmentLocation
    , SegmentLocation
    , segmentLocation
    , slCountry
    , slGPSPoint

    -- ** SegmentReference
    , SegmentReference
    , segmentReference
    , srVersion
    , srId

    -- ** SegmentResponse
    , SegmentResponse
    , segmentResponse
    , sLastModifiedDate
    , sARN
    , sSegmentType
    , sSegmentGroups
    , sApplicationId
    , sName
    , sVersion
    , sId
    , sCreationDate
    , sImportDefinition
    , sDimensions
    , sTags

    -- ** SegmentsResponse
    , SegmentsResponse
    , segmentsResponse
    , sNextToken
    , sItem

    -- ** SendUsersMessageRequest
    , SendUsersMessageRequest
    , sendUsersMessageRequest
    , sumrTraceId
    , sumrContext
    , sumrUsers
    , sumrMessageConfiguration

    -- ** SendUsersMessageResponse
    , SendUsersMessageResponse
    , sendUsersMessageResponse
    , sumRequestId
    , sumResult
    , sumApplicationId

    -- ** Session
    , Session
    , session
    , sesStopTimestamp
    , sesId
    , sesStartTimestamp
    , sesDuration

    -- ** SetDimension
    , SetDimension
    , setDimension
    , sdValues
    , sdDimensionType

    -- ** SimpleEmail
    , SimpleEmail
    , simpleEmail
    , seSubject
    , seTextPart
    , seHTMLPart

    -- ** SimpleEmailPart
    , SimpleEmailPart
    , simpleEmailPart
    , sepData
    , sepCharset

    -- ** TagsModel
    , TagsModel
    , tagsModel
    , tmTags

    -- ** TreatmentResource
    , TreatmentResource
    , treatmentResource
    , trState
    , trSchedule
    , trTreatmentName
    , trSizePercent
    , trTreatmentDescription
    , trId
    , trMessageConfiguration

    -- ** UpdateAttributesRequest
    , UpdateAttributesRequest
    , updateAttributesRequest
    , uarBlacklist

    -- ** VoiceChannelRequest
    , VoiceChannelRequest
    , voiceChannelRequest
    , vcrEnabled

    -- ** VoiceChannelResponse
    , VoiceChannelResponse
    , voiceChannelResponse
    , vcPlatform
    , vcLastModifiedDate
    , vcEnabled
    , vcIsArchived
    , vcApplicationId
    , vcVersion
    , vcId
    , vcCreationDate
    , vcLastModifiedBy
    , vcHasCredential

    -- ** VoiceMessage
    , VoiceMessage
    , voiceMessage
    , vmSubstitutions
    , vmLanguageCode
    , vmOriginationNumber
    , vmBody
    , vmVoiceId

    -- ** WriteApplicationSettingsRequest
    , WriteApplicationSettingsRequest
    , writeApplicationSettingsRequest
    , wasrCloudWatchMetricsEnabled
    , wasrLimits
    , wasrQuietTime
    , wasrCampaignHook

    -- ** WriteCampaignRequest
    , WriteCampaignRequest
    , writeCampaignRequest
    , wcrSchedule
    , wcrHook
    , wcrTreatmentName
    , wcrLimits
    , wcrIsPaused
    , wcrName
    , wcrHoldoutPercent
    , wcrTreatmentDescription
    , wcrMessageConfiguration
    , wcrDescription
    , wcrSegmentId
    , wcrAdditionalTreatments
    , wcrTags
    , wcrSegmentVersion

    -- ** WriteEventStream
    , WriteEventStream
    , writeEventStream
    , wesDestinationStreamARN
    , wesRoleARN

    -- ** WriteSegmentRequest
    , WriteSegmentRequest
    , writeSegmentRequest
    , wsrSegmentGroups
    , wsrName
    , wsrDimensions
    , wsrTags

    -- ** WriteTreatmentResource
    , WriteTreatmentResource
    , writeTreatmentResource
    , wtrSchedule
    , wtrTreatmentName
    , wtrSizePercent
    , wtrTreatmentDescription
    , wtrMessageConfiguration
    ) where

import Network.AWS.Pinpoint.CreateApp
import Network.AWS.Pinpoint.CreateCampaign
import Network.AWS.Pinpoint.CreateExportJob
import Network.AWS.Pinpoint.CreateImportJob
import Network.AWS.Pinpoint.CreateSegment
import Network.AWS.Pinpoint.DeleteADMChannel
import Network.AWS.Pinpoint.DeleteAPNSChannel
import Network.AWS.Pinpoint.DeleteAPNSSandboxChannel
import Network.AWS.Pinpoint.DeleteAPNSVoipChannel
import Network.AWS.Pinpoint.DeleteAPNSVoipSandboxChannel
import Network.AWS.Pinpoint.DeleteApp
import Network.AWS.Pinpoint.DeleteBaiduChannel
import Network.AWS.Pinpoint.DeleteCampaign
import Network.AWS.Pinpoint.DeleteEmailChannel
import Network.AWS.Pinpoint.DeleteEndpoint
import Network.AWS.Pinpoint.DeleteEventStream
import Network.AWS.Pinpoint.DeleteGCMChannel
import Network.AWS.Pinpoint.DeleteSegment
import Network.AWS.Pinpoint.DeleteSmsChannel
import Network.AWS.Pinpoint.DeleteUserEndpoints
import Network.AWS.Pinpoint.DeleteVoiceChannel
import Network.AWS.Pinpoint.GetADMChannel
import Network.AWS.Pinpoint.GetAPNSChannel
import Network.AWS.Pinpoint.GetAPNSSandboxChannel
import Network.AWS.Pinpoint.GetAPNSVoipChannel
import Network.AWS.Pinpoint.GetAPNSVoipSandboxChannel
import Network.AWS.Pinpoint.GetApp
import Network.AWS.Pinpoint.GetApplicationSettings
import Network.AWS.Pinpoint.GetApps
import Network.AWS.Pinpoint.GetBaiduChannel
import Network.AWS.Pinpoint.GetCampaign
import Network.AWS.Pinpoint.GetCampaignActivities
import Network.AWS.Pinpoint.GetCampaigns
import Network.AWS.Pinpoint.GetCampaignVersion
import Network.AWS.Pinpoint.GetCampaignVersions
import Network.AWS.Pinpoint.GetChannels
import Network.AWS.Pinpoint.GetEmailChannel
import Network.AWS.Pinpoint.GetEndpoint
import Network.AWS.Pinpoint.GetEventStream
import Network.AWS.Pinpoint.GetExportJob
import Network.AWS.Pinpoint.GetExportJobs
import Network.AWS.Pinpoint.GetGCMChannel
import Network.AWS.Pinpoint.GetImportJob
import Network.AWS.Pinpoint.GetImportJobs
import Network.AWS.Pinpoint.GetSegment
import Network.AWS.Pinpoint.GetSegmentExportJobs
import Network.AWS.Pinpoint.GetSegmentImportJobs
import Network.AWS.Pinpoint.GetSegments
import Network.AWS.Pinpoint.GetSegmentVersion
import Network.AWS.Pinpoint.GetSegmentVersions
import Network.AWS.Pinpoint.GetSmsChannel
import Network.AWS.Pinpoint.GetUserEndpoints
import Network.AWS.Pinpoint.GetVoiceChannel
import Network.AWS.Pinpoint.ListTagsForResource
import Network.AWS.Pinpoint.PhoneNumberValidate
import Network.AWS.Pinpoint.PutEvents
import Network.AWS.Pinpoint.PutEventStream
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
import Network.AWS.Pinpoint.UpdateEndpoint
import Network.AWS.Pinpoint.UpdateEndpointsBatch
import Network.AWS.Pinpoint.UpdateGCMChannel
import Network.AWS.Pinpoint.UpdateSegment
import Network.AWS.Pinpoint.UpdateSmsChannel
import Network.AWS.Pinpoint.UpdateVoiceChannel
import Network.AWS.Pinpoint.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'Pinpoint'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}
