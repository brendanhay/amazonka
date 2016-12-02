{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
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

    -- ** GetImportJob
    , module Network.AWS.Pinpoint.GetImportJob

    -- ** GetSegmentVersions
    , module Network.AWS.Pinpoint.GetSegmentVersions

    -- ** DeleteCampaign
    , module Network.AWS.Pinpoint.DeleteCampaign

    -- ** UpdateCampaign
    , module Network.AWS.Pinpoint.UpdateCampaign

    -- ** GetSegmentVersion
    , module Network.AWS.Pinpoint.GetSegmentVersion

    -- ** CreateSegment
    , module Network.AWS.Pinpoint.CreateSegment

    -- ** UpdateEndpoint
    , module Network.AWS.Pinpoint.UpdateEndpoint

    -- ** CreateCampaign
    , module Network.AWS.Pinpoint.CreateCampaign

    -- ** GetEndpoint
    , module Network.AWS.Pinpoint.GetEndpoint

    -- ** GetSegment
    , module Network.AWS.Pinpoint.GetSegment

    -- ** UpdateEndpointsBatch
    , module Network.AWS.Pinpoint.UpdateEndpointsBatch

    -- ** GetCampaign
    , module Network.AWS.Pinpoint.GetCampaign

    -- ** UpdateGCMChannel
    , module Network.AWS.Pinpoint.UpdateGCMChannel

    -- ** DeleteGCMChannel
    , module Network.AWS.Pinpoint.DeleteGCMChannel

    -- ** GetCampaignActivities
    , module Network.AWS.Pinpoint.GetCampaignActivities

    -- ** DeleteAPNSChannel
    , module Network.AWS.Pinpoint.DeleteAPNSChannel

    -- ** UpdateAPNSChannel
    , module Network.AWS.Pinpoint.UpdateAPNSChannel

    -- ** GetCampaignVersions
    , module Network.AWS.Pinpoint.GetCampaignVersions

    -- ** GetAPNSChannel
    , module Network.AWS.Pinpoint.GetAPNSChannel

    -- ** GetImportJobs
    , module Network.AWS.Pinpoint.GetImportJobs

    -- ** GetCampaignVersion
    , module Network.AWS.Pinpoint.GetCampaignVersion

    -- ** DeleteSegment
    , module Network.AWS.Pinpoint.DeleteSegment

    -- ** UpdateSegment
    , module Network.AWS.Pinpoint.UpdateSegment

    -- ** GetCampaigns
    , module Network.AWS.Pinpoint.GetCampaigns

    -- ** UpdateApplicationSettings
    , module Network.AWS.Pinpoint.UpdateApplicationSettings

    -- ** GetSegments
    , module Network.AWS.Pinpoint.GetSegments

    -- ** CreateImportJob
    , module Network.AWS.Pinpoint.CreateImportJob

    -- ** GetApplicationSettings
    , module Network.AWS.Pinpoint.GetApplicationSettings

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

    -- ** DimensionType
    , DimensionType (..)

    -- ** Duration
    , Duration (..)

    -- ** Frequency
    , Frequency (..)

    -- ** JobStatus
    , JobStatus (..)

    -- ** RecencyType
    , RecencyType (..)

    -- ** SegmentType
    , SegmentType (..)

    -- ** APNSChannelRequest
    , APNSChannelRequest
    , apnsChannelRequest
    , acrPrivateKey
    , acrCertificate

    -- ** APNSChannelResponse
    , APNSChannelResponse
    , apnsChannelResponse
    , acPlatform
    , acLastModifiedDate
    , acIsArchived
    , acApplicationId
    , acVersion
    , acId
    , acCreationDate
    , acLastModifiedBy

    -- ** ActivitiesResponse
    , ActivitiesResponse
    , activitiesResponse
    , aItem

    -- ** ActivityResponse
    , ActivityResponse
    , activityResponse
    , aState
    , aStart
    , aCampaignId
    , aResult
    , aTreatmentId
    , aSuccessfulEndpointCount
    , aEnd
    , aApplicationId
    , aTotalEndpointCount
    , aId
    , aScheduledStart

    -- ** ApplicationSettingsResource
    , ApplicationSettingsResource
    , applicationSettingsResource
    , asrLastModifiedDate
    , asrLimits
    , asrQuietTime
    , asrApplicationId

    -- ** AttributeDimension
    , AttributeDimension
    , attributeDimension
    , adValues
    , adAttributeType

    -- ** CampaignLimits
    , CampaignLimits
    , campaignLimits
    , clDaily
    , clTotal

    -- ** CampaignResponse
    , CampaignResponse
    , campaignResponse
    , cState
    , cLastModifiedDate
    , cSchedule
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
    , cSegmentVersion

    -- ** CampaignState
    , CampaignState
    , campaignState
    , csCampaignStatus

    -- ** CampaignsResponse
    , CampaignsResponse
    , campaignsResponse
    , cNextToken
    , cItem

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

    -- ** EndpointLocation
    , EndpointLocation
    , endpointLocation
    , elPostalCode
    , elLatitude
    , elCountry
    , elCity
    , elRegion
    , elLongitude

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
    , eRequestId
    , eMetrics
    , eLocation
    , eDemographic
    , eCohortId
    , eAddress
    , eEffectiveDate
    , eUser
    , eApplicationId
    , eAttributes
    , eEndpointStatus
    , eOptOut
    , eId
    , eCreationDate
    , eChannelType
    , eShardId

    -- ** EndpointUser
    , EndpointUser
    , endpointUser
    , euUserAttributes
    , euUserId

    -- ** GCMChannelRequest
    , GCMChannelRequest
    , gcmChannelRequest
    , gcrAPIKey

    -- ** GCMChannelResponse
    , GCMChannelResponse
    , gcmChannelResponse
    , gcPlatform
    , gcLastModifiedDate
    , gcCredential
    , gcIsArchived
    , gcApplicationId
    , gcVersion
    , gcId
    , gcCreationDate
    , gcLastModifiedBy

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

    -- ** Message
    , Message
    , message
    , mSilentPush
    , mImageIconURL
    , mBody
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

    -- ** QuietTime
    , QuietTime
    , quietTime
    , qtStart
    , qtEnd

    -- ** RecencyDimension
    , RecencyDimension
    , recencyDimension
    , rdRecencyType
    , rdDuration

    -- ** Schedule
    , Schedule
    , schedule
    , sFrequency
    , sStartTime
    , sQuietTime
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
    , sdModel
    , sdMake
    , sdDeviceType

    -- ** SegmentDimensions
    , SegmentDimensions
    , segmentDimensions
    , sdLocation
    , sdDemographic
    , sdBehavior
    , sdAttributes

    -- ** SegmentImportResource
    , SegmentImportResource
    , segmentImportResource
    , sirSize
    , sirFormat
    , sirExternalId
    , sirS3URL
    , sirRoleARN

    -- ** SegmentLocation
    , SegmentLocation
    , segmentLocation
    , slCountry

    -- ** SegmentResponse
    , SegmentResponse
    , segmentResponse
    , sLastModifiedDate
    , sSegmentType
    , sApplicationId
    , sName
    , sVersion
    , sId
    , sCreationDate
    , sImportDefinition
    , sDimensions

    -- ** SegmentsResponse
    , SegmentsResponse
    , segmentsResponse
    , sNextToken
    , sItem

    -- ** SetDimension
    , SetDimension
    , setDimension
    , sdValues
    , sdDimensionType

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

    -- ** WriteApplicationSettingsRequest
    , WriteApplicationSettingsRequest
    , writeApplicationSettingsRequest
    , wasrLimits
    , wasrQuietTime

    -- ** WriteCampaignRequest
    , WriteCampaignRequest
    , writeCampaignRequest
    , wcrSchedule
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
    , wcrSegmentVersion

    -- ** WriteSegmentRequest
    , WriteSegmentRequest
    , writeSegmentRequest
    , wsrName
    , wsrDimensions

    -- ** WriteTreatmentResource
    , WriteTreatmentResource
    , writeTreatmentResource
    , wtrSchedule
    , wtrTreatmentName
    , wtrSizePercent
    , wtrTreatmentDescription
    , wtrMessageConfiguration
    ) where

import           Network.AWS.Pinpoint.CreateCampaign
import           Network.AWS.Pinpoint.CreateImportJob
import           Network.AWS.Pinpoint.CreateSegment
import           Network.AWS.Pinpoint.DeleteAPNSChannel
import           Network.AWS.Pinpoint.DeleteCampaign
import           Network.AWS.Pinpoint.DeleteGCMChannel
import           Network.AWS.Pinpoint.DeleteSegment
import           Network.AWS.Pinpoint.GetAPNSChannel
import           Network.AWS.Pinpoint.GetApplicationSettings
import           Network.AWS.Pinpoint.GetCampaign
import           Network.AWS.Pinpoint.GetCampaignActivities
import           Network.AWS.Pinpoint.GetCampaigns
import           Network.AWS.Pinpoint.GetCampaignVersion
import           Network.AWS.Pinpoint.GetCampaignVersions
import           Network.AWS.Pinpoint.GetEndpoint
import           Network.AWS.Pinpoint.GetGCMChannel
import           Network.AWS.Pinpoint.GetImportJob
import           Network.AWS.Pinpoint.GetImportJobs
import           Network.AWS.Pinpoint.GetSegment
import           Network.AWS.Pinpoint.GetSegmentImportJobs
import           Network.AWS.Pinpoint.GetSegments
import           Network.AWS.Pinpoint.GetSegmentVersion
import           Network.AWS.Pinpoint.GetSegmentVersions
import           Network.AWS.Pinpoint.Types
import           Network.AWS.Pinpoint.UpdateAPNSChannel
import           Network.AWS.Pinpoint.UpdateApplicationSettings
import           Network.AWS.Pinpoint.UpdateCampaign
import           Network.AWS.Pinpoint.UpdateEndpoint
import           Network.AWS.Pinpoint.UpdateEndpointsBatch
import           Network.AWS.Pinpoint.UpdateGCMChannel
import           Network.AWS.Pinpoint.UpdateSegment
import           Network.AWS.Pinpoint.Waiters

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
