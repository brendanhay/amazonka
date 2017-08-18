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

    -- * DeliveryStatus
    , DeliveryStatus (..)

    -- * DimensionType
    , DimensionType (..)

    -- * Duration
    , Duration (..)

    -- * Frequency
    , Frequency (..)

    -- * JobStatus
    , JobStatus (..)

    -- * MessageType
    , MessageType (..)

    -- * RecencyType
    , RecencyType (..)

    -- * SegmentType
    , SegmentType (..)

    -- * APNSChannelRequest
    , APNSChannelRequest
    , apnsChannelRequest
    , acrPrivateKey
    , acrEnabled
    , acrCertificate

    -- * APNSChannelResponse
    , APNSChannelResponse
    , apnsChannelResponse
    , acPlatform
    , acLastModifiedDate
    , acEnabled
    , acIsArchived
    , acApplicationId
    , acVersion
    , acId
    , acCreationDate
    , acLastModifiedBy

    -- * APNSMessage
    , APNSMessage
    , apnsMessage
    , amSubstitutions
    , amSilentPush
    , amRawContent
    , amData
    , amBody
    , amCategory
    , amURL
    , amSound
    , amAction
    , amMediaURL
    , amBadge
    , amTitle
    , amThreadId

    -- * APNSSandboxChannelRequest
    , APNSSandboxChannelRequest
    , apnsSandboxChannelRequest
    , ascrPrivateKey
    , ascrEnabled
    , ascrCertificate

    -- * APNSSandboxChannelResponse
    , APNSSandboxChannelResponse
    , apnsSandboxChannelResponse
    , ascPlatform
    , ascLastModifiedDate
    , ascEnabled
    , ascIsArchived
    , ascApplicationId
    , ascVersion
    , ascId
    , ascCreationDate
    , ascLastModifiedBy

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

    -- * AddressConfiguration
    , AddressConfiguration
    , addressConfiguration
    , acSubstitutions
    , acTitleOverride
    , acContext
    , acRawContent
    , acBodyOverride
    , acChannelType

    -- * ApplicationResponse
    , ApplicationResponse
    , applicationResponse
    , appName
    , appId

    -- * ApplicationSettingsResource
    , ApplicationSettingsResource
    , applicationSettingsResource
    , asrLastModifiedDate
    , asrLimits
    , asrQuietTime
    , asrApplicationId

    -- * ApplicationsResponse
    , ApplicationsResponse
    , applicationsResponse
    , appNextToken
    , appItem

    -- * AttributeDimension
    , AttributeDimension
    , attributeDimension
    , adValues
    , adAttributeType

    -- * CampaignEmailMessage
    , CampaignEmailMessage
    , campaignEmailMessage
    , cemBody
    , cemFromAddress
    , cemHTMLBody
    , cemTitle

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

    -- * CampaignSmsMessage
    , CampaignSmsMessage
    , campaignSmsMessage
    , csmBody
    , csmMessageType
    , csmSenderId

    -- * CampaignState
    , CampaignState
    , campaignState
    , csCampaignStatus

    -- * CampaignsResponse
    , CampaignsResponse
    , campaignsResponse
    , cNextToken
    , cItem

    -- * CreateApplicationRequest
    , CreateApplicationRequest
    , createApplicationRequest
    , carName

    -- * DefaultMessage
    , DefaultMessage
    , defaultMessage
    , dmSubstitutions
    , dmBody

    -- * DefaultPushNotificationMessage
    , DefaultPushNotificationMessage
    , defaultPushNotificationMessage
    , dpnmSubstitutions
    , dpnmSilentPush
    , dpnmData
    , dpnmBody
    , dpnmURL
    , dpnmAction
    , dpnmTitle

    -- * DirectMessageConfiguration
    , DirectMessageConfiguration
    , directMessageConfiguration
    , dmcAPNSMessage
    , dmcGCMMessage
    , dmcDefaultMessage
    , dmcSMSMessage
    , dmcDefaultPushNotificationMessage

    -- * EmailChannelRequest
    , EmailChannelRequest
    , emailChannelRequest
    , ecrEnabled
    , ecrFromAddress
    , ecrIdentity
    , ecrRoleARN

    -- * EmailChannelResponse
    , EmailChannelResponse
    , emailChannelResponse
    , ecPlatform
    , ecLastModifiedDate
    , ecEnabled
    , ecFromAddress
    , ecIsArchived
    , ecApplicationId
    , ecVersion
    , ecId
    , ecCreationDate
    , ecLastModifiedBy
    , ecIdentity
    , ecRoleARN

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

    -- * EventStream
    , EventStream
    , eventStream
    , esLastUpdatedBy
    , esLastModifiedDate
    , esDestinationStreamARN
    , esApplicationId
    , esExternalId
    , esRoleARN

    -- * GCMChannelRequest
    , GCMChannelRequest
    , gcmChannelRequest
    , gcrAPIKey
    , gcrEnabled

    -- * GCMChannelResponse
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

    -- * GCMMessage
    , GCMMessage
    , gcmMessage
    , gmSubstitutions
    , gmSilentPush
    , gmImageIconURL
    , gmRawContent
    , gmData
    , gmRestrictedPackageName
    , gmSmallImageIconURL
    , gmBody
    , gmURL
    , gmSound
    , gmAction
    , gmCollapseKey
    , gmImageURL
    , gmTitle
    , gmIconReference

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
    , mRawContent
    , mBody
    , mImageSmallIconURL
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
    , mcSMSMessage
    , mcEmailMessage

    -- * MessageRequest
    , MessageRequest
    , messageRequest
    , mrContext
    , mrAddresses
    , mrMessageConfiguration

    -- * MessageResponse
    , MessageResponse
    , messageResponse
    , mRequestId
    , mResult
    , mApplicationId

    -- * MessageResult
    , MessageResult
    , messageResult
    , mrDeliveryStatus
    , mrStatusMessage
    , mrUpdatedToken
    , mrStatusCode

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

    -- * SMSChannelRequest
    , SMSChannelRequest
    , sMSChannelRequest
    , smscrEnabled
    , smscrSenderId

    -- * SMSChannelResponse
    , SMSChannelResponse
    , sMSChannelResponse
    , smscPlatform
    , smscShortCode
    , smscLastModifiedDate
    , smscEnabled
    , smscSenderId
    , smscIsArchived
    , smscApplicationId
    , smscVersion
    , smscId
    , smscCreationDate
    , smscLastModifiedBy

    -- * SMSMessage
    , SMSMessage
    , sMSMessage
    , smsmSubstitutions
    , smsmBody
    , smsmMessageType
    , smsmSenderId

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
    , sdChannel
    , sdModel
    , sdMake
    , sdDeviceType

    -- * SegmentDimensions
    , SegmentDimensions
    , segmentDimensions
    , sdLocation
    , sdDemographic
    , sdUserAttributes
    , sdBehavior
    , sdAttributes

    -- * SegmentImportResource
    , SegmentImportResource
    , segmentImportResource
    , sirSize
    , sirFormat
    , sirChannelCounts
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

    -- * WriteEventStream
    , WriteEventStream
    , writeEventStream
    , wesDestinationStreamARN
    , wesExternalId
    , wesRoleARN

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
      | has (hasCode "ThrottledException" . hasStatus 400) e =
          Just "throttled_exception"
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

-- | Simple message object.
_ForbiddenException :: AsError a => Getting (First ServiceError) a ServiceError
_ForbiddenException =
    _MatchServiceError pinpoint "ForbiddenException" . hasStatus 403

-- | Simple message object.
_NotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_NotFoundException =
    _MatchServiceError pinpoint "NotFoundException" . hasStatus 404

-- | Simple message object.
_TooManyRequestsException :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyRequestsException =
    _MatchServiceError pinpoint "TooManyRequestsException" . hasStatus 429

-- | Simple message object.
_InternalServerErrorException :: AsError a => Getting (First ServiceError) a ServiceError
_InternalServerErrorException =
    _MatchServiceError pinpoint "InternalServerErrorException" . hasStatus 500

-- | Simple message object.
_MethodNotAllowedException :: AsError a => Getting (First ServiceError) a ServiceError
_MethodNotAllowedException =
    _MatchServiceError pinpoint "MethodNotAllowedException" . hasStatus 405

-- | Simple message object.
_BadRequestException :: AsError a => Getting (First ServiceError) a ServiceError
_BadRequestException =
    _MatchServiceError pinpoint "BadRequestException" . hasStatus 400
