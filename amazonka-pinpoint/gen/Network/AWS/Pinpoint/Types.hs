{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types
    (
    -- * Service Configuration
      pinpoint

    -- * Errors
    , _ForbiddenException
    , _NotFoundException
    , _TooManyRequestsException
    , _InternalServerErrorException
    , _MethodNotAllowedException
    , _BadRequestException

    -- * Action
    , Action (..)

    -- * AttributeType
    , AttributeType (..)

    -- * CampaignStatus
    , CampaignStatus (..)

    -- * ChannelType
    , ChannelType (..)

    -- * DefinitionFormat
    , DefinitionFormat (..)

    -- * DimensionType
    , DimensionType (..)

    -- * Duration
    , Duration (..)

    -- * Frequency
    , Frequency (..)

    -- * JobStatus
    , JobStatus (..)

    -- * RecencyType
    , RecencyType (..)

    -- * SegmentType
    , SegmentType (..)

    -- * APNSChannelRequest
    , APNSChannelRequest
    , apnsChannelRequest
    , acrPrivateKey
    , acrCertificate

    -- * APNSChannelResponse
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

    -- * ActivitiesResponse
    , ActivitiesResponse
    , activitiesResponse
    , aItem

    -- * ActivityResponse
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

    -- * ApplicationSettingsResource
    , ApplicationSettingsResource
    , applicationSettingsResource
    , asrLastModifiedDate
    , asrLimits
    , asrQuietTime
    , asrApplicationId

    -- * AttributeDimension
    , AttributeDimension
    , attributeDimension
    , adValues
    , adAttributeType

    -- * CampaignLimits
    , CampaignLimits
    , campaignLimits
    , clDaily
    , clTotal

    -- * CampaignResponse
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

    -- * CampaignState
    , CampaignState
    , campaignState
    , csCampaignStatus

    -- * CampaignsResponse
    , CampaignsResponse
    , campaignsResponse
    , cNextToken
    , cItem

    -- * EndpointBatchItem
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

    -- * EndpointBatchRequest
    , EndpointBatchRequest
    , endpointBatchRequest
    , ebrItem

    -- * EndpointDemographic
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

    -- * EndpointLocation
    , EndpointLocation
    , endpointLocation
    , elPostalCode
    , elLatitude
    , elCountry
    , elCity
    , elRegion
    , elLongitude

    -- * EndpointRequest
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

    -- * EndpointResponse
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

    -- * EndpointUser
    , EndpointUser
    , endpointUser
    , euUserAttributes
    , euUserId

    -- * GCMChannelRequest
    , GCMChannelRequest
    , gcmChannelRequest
    , gcrAPIKey

    -- * GCMChannelResponse
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

    -- * ImportJobRequest
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

    -- * ImportJobResource
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

    -- * ImportJobResponse
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

    -- * ImportJobsResponse
    , ImportJobsResponse
    , importJobsResponse
    , ijNextToken
    , ijItem

    -- * Message
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

    -- * MessageBody
    , MessageBody
    , messageBody
    , mbRequestId
    , mbMessage

    -- * MessageConfiguration
    , MessageConfiguration
    , messageConfiguration
    , mcAPNSMessage
    , mcGCMMessage
    , mcDefaultMessage

    -- * QuietTime
    , QuietTime
    , quietTime
    , qtStart
    , qtEnd

    -- * RecencyDimension
    , RecencyDimension
    , recencyDimension
    , rdRecencyType
    , rdDuration

    -- * Schedule
    , Schedule
    , schedule
    , sFrequency
    , sStartTime
    , sQuietTime
    , sIsLocalTime
    , sEndTime
    , sTimezone

    -- * SegmentBehaviors
    , SegmentBehaviors
    , segmentBehaviors
    , sbRecency

    -- * SegmentDemographics
    , SegmentDemographics
    , segmentDemographics
    , sdPlatform
    , sdAppVersion
    , sdModel
    , sdMake
    , sdDeviceType

    -- * SegmentDimensions
    , SegmentDimensions
    , segmentDimensions
    , sdLocation
    , sdDemographic
    , sdBehavior
    , sdAttributes

    -- * SegmentImportResource
    , SegmentImportResource
    , segmentImportResource
    , sirSize
    , sirFormat
    , sirExternalId
    , sirS3URL
    , sirRoleARN

    -- * SegmentLocation
    , SegmentLocation
    , segmentLocation
    , slCountry

    -- * SegmentResponse
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

    -- * SegmentsResponse
    , SegmentsResponse
    , segmentsResponse
    , sNextToken
    , sItem

    -- * SetDimension
    , SetDimension
    , setDimension
    , sdValues
    , sdDimensionType

    -- * TreatmentResource
    , TreatmentResource
    , treatmentResource
    , trState
    , trSchedule
    , trTreatmentName
    , trSizePercent
    , trTreatmentDescription
    , trId
    , trMessageConfiguration

    -- * WriteApplicationSettingsRequest
    , WriteApplicationSettingsRequest
    , writeApplicationSettingsRequest
    , wasrLimits
    , wasrQuietTime

    -- * WriteCampaignRequest
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

    -- * WriteSegmentRequest
    , WriteSegmentRequest
    , writeSegmentRequest
    , wsrName
    , wsrDimensions

    -- * WriteTreatmentResource
    , WriteTreatmentResource
    , writeTreatmentResource
    , wtrSchedule
    , wtrTreatmentName
    , wtrSizePercent
    , wtrTreatmentDescription
    , wtrMessageConfiguration
    ) where

import           Network.AWS.Lens
import           Network.AWS.Pinpoint.Types.Product
import           Network.AWS.Pinpoint.Types.Sum
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | API version @2016-12-01@ of the Amazon Pinpoint SDK configuration.
pinpoint :: Service
pinpoint =
    Service
    { _svcAbbrev = "Pinpoint"
    , _svcSigner = v4
    , _svcPrefix = "pinpoint"
    , _svcVersion = "2016-12-01"
    , _svcEndpoint = defaultEndpoint pinpoint
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "Pinpoint"
    , _svcRetry = retry
    }
  where
    retry =
        Exponential
        { _retryBase = 5.0e-2
        , _retryGrowth = 2
        , _retryAttempts = 5
        , _retryCheck = check
        }
    check e
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
          Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing

-- | Prism for ForbiddenException' errors.
_ForbiddenException :: AsError a => Getting (First ServiceError) a ServiceError
_ForbiddenException =
    _ServiceError . hasStatus 403 . hasCode "ForbiddenException"

-- | Prism for NotFoundException' errors.
_NotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_NotFoundException =
    _ServiceError . hasStatus 404 . hasCode "NotFoundException"

-- | Prism for TooManyRequestsException' errors.
_TooManyRequestsException :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyRequestsException =
    _ServiceError . hasStatus 429 . hasCode "TooManyRequestsException"

-- | Prism for InternalServerErrorException' errors.
_InternalServerErrorException :: AsError a => Getting (First ServiceError) a ServiceError
_InternalServerErrorException =
    _ServiceError . hasStatus 500 . hasCode "InternalServerErrorException"

-- | Prism for MethodNotAllowedException' errors.
_MethodNotAllowedException :: AsError a => Getting (First ServiceError) a ServiceError
_MethodNotAllowedException =
    _ServiceError . hasStatus 405 . hasCode "MethodNotAllowedException"

-- | Prism for BadRequestException' errors.
_BadRequestException :: AsError a => Getting (First ServiceError) a ServiceError
_BadRequestException =
    _ServiceError . hasStatus 400 . hasCode "BadRequestException"
