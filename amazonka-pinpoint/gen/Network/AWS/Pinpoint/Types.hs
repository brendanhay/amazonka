{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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

    -- * Mode
    , Mode (..)

    -- * RecencyType
    , RecencyType (..)

    -- * SegmentType
    , SegmentType (..)

    -- * ADMChannelRequest
    , ADMChannelRequest
    , aDMChannelRequest
    , admcrClientId
    , admcrClientSecret
    , admcrEnabled

    -- * ADMChannelResponse
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

    -- * ADMMessage
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

    -- * APNSChannelRequest
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

    -- * APNSChannelResponse
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

    -- * APNSMessage
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

    -- * APNSSandboxChannelRequest
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

    -- * APNSSandboxChannelResponse
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

    -- * APNSVoipChannelRequest
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

    -- * APNSVoipChannelResponse
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

    -- * APNSVoipSandboxChannelRequest
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

    -- * APNSVoipSandboxChannelResponse
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
    , asrCampaignHook

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

    -- * BaiduChannelRequest
    , BaiduChannelRequest
    , baiduChannelRequest
    , bcrAPIKey
    , bcrEnabled
    , bcrSecretKey

    -- * BaiduChannelResponse
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

    -- * BaiduMessage
    , BaiduMessage
    , baiduMessage
    , bmSubstitutions
    , bmSilentPush
    , bmImageIconURL
    , bmRawContent
    , bmData
    , bmSmallImageIconURL
    , bmBody
    , bmURL
    , bmSound
    , bmAction
    , bmImageURL
    , bmTitle
    , bmIconReference

    -- * CampaignEmailMessage
    , CampaignEmailMessage
    , campaignEmailMessage
    , cemBody
    , cemFromAddress
    , cemHTMLBody
    , cemTitle

    -- * CampaignHook
    , CampaignHook
    , campaignHook
    , chLambdaFunctionName
    , chMode
    , chWebURL

    -- * CampaignLimits
    , CampaignLimits
    , campaignLimits
    , clMessagesPerSecond
    , clDaily
    , clTotal
    , clMaximumDuration

    -- * CampaignResponse
    , CampaignResponse
    , campaignResponse
    , cState
    , cLastModifiedDate
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
    , dmcADMMessage
    , dmcSMSMessage
    , dmcBaiduMessage
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
    , ecHasCredential
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

    -- * EndpointMessageResult
    , EndpointMessageResult
    , endpointMessageResult
    , emrDeliveryStatus
    , emrAddress
    , emrStatusMessage
    , emrUpdatedToken
    , emrStatusCode

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

    -- * EndpointSendConfiguration
    , EndpointSendConfiguration
    , endpointSendConfiguration
    , escSubstitutions
    , escTitleOverride
    , escContext
    , escRawContent
    , escBodyOverride

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

    -- * ExportJobRequest
    , ExportJobRequest
    , exportJobRequest
    , eS3URLPrefix
    , eSegmentId
    , eRoleARN

    -- * ExportJobResource
    , ExportJobResource
    , exportJobResource
    , ejrS3URLPrefix
    , ejrSegmentId
    , ejrRoleARN

    -- * ExportJobResponse
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

    -- * ExportJobsResponse
    , ExportJobsResponse
    , exportJobsResponse
    , ejNextToken
    , ejItem

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
    , gcHasCredential

    -- * GCMMessage
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
    , mcADMMessage
    , mcSMSMessage
    , mcEmailMessage
    , mcBaiduMessage

    -- * MessageRequest
    , MessageRequest
    , messageRequest
    , mrContext
    , mrAddresses
    , mrEndpoints
    , mrMessageConfiguration

    -- * MessageResponse
    , MessageResponse
    , messageResponse
    , mRequestId
    , mResult
    , mApplicationId
    , mEndpointResult

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
    , smscrShortCode
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
    , smscHasCredential

    -- * SMSMessage
    , SMSMessage
    , sMSMessage
    , smsmSubstitutions
    , smsmOriginationNumber
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

    -- * SendUsersMessageRequest
    , SendUsersMessageRequest
    , sendUsersMessageRequest
    , sumrContext
    , sumrUsers
    , sumrMessageConfiguration

    -- * SendUsersMessageResponse
    , SendUsersMessageResponse
    , sendUsersMessageResponse
    , sumRequestId
    , sumResult
    , sumApplicationId

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
    , wasrCampaignHook

    -- * WriteCampaignRequest
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
    , wcrSegmentVersion

    -- * WriteEventStream
    , WriteEventStream
    , writeEventStream
    , wesDestinationStreamARN
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

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.Product
import Network.AWS.Pinpoint.Types.Sum
import Network.AWS.Prelude
import Network.AWS.Sign.V4

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
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
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

