{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GuardDuty.Types
    (
    -- * Service Configuration
      guardDuty

    -- * Errors
    , _InternalServerErrorException
    , _BadRequestException

    -- * DetectorStatus
    , DetectorStatus (..)

    -- * Feedback
    , Feedback (..)

    -- * FilterAction
    , FilterAction (..)

    -- * FindingStatisticType
    , FindingStatisticType (..)

    -- * IPSetFormat
    , IPSetFormat (..)

    -- * IPSetStatus
    , IPSetStatus (..)

    -- * OrderBy
    , OrderBy (..)

    -- * ThreatIntelSetFormat
    , ThreatIntelSetFormat (..)

    -- * ThreatIntelSetStatus
    , ThreatIntelSetStatus (..)

    -- * AWSAPICallAction
    , AWSAPICallAction
    , awsAPICallAction
    , aacaRemoteIPDetails
    , aacaCallerType
    , aacaDomainDetails
    , aacaServiceName
    , aacaAPI

    -- * AccessKeyDetails
    , AccessKeyDetails
    , accessKeyDetails
    , akdPrincipalId
    , akdUserName
    , akdAccessKeyId
    , akdUserType

    -- * AccountDetail
    , AccountDetail
    , accountDetail
    , adEmail
    , adAccountId

    -- * Action
    , Action
    , action
    , aNetworkConnectionAction
    , aPortProbeAction
    , aActionType
    , aDNSRequestAction
    , aAWSAPICallAction

    -- * City
    , City
    , city
    , cCityName

    -- * Condition
    , Condition
    , condition
    , cEQ
    , cLte
    , cGT
    , cNeq
    , cLT
    , cGte

    -- * Country
    , Country
    , country
    , cCountryName
    , cCountryCode

    -- * DNSRequestAction
    , DNSRequestAction
    , dnsRequestAction
    , draDomain

    -- * DomainDetails
    , DomainDetails
    , domainDetails

    -- * Finding
    , Finding
    , finding
    , fService
    , fConfidence
    , fPartition
    , fTitle
    , fDescription
    , fAccountId
    , fSchemaVersion
    , fCreatedAt
    , fResource
    , fSeverity
    , fUpdatedAt
    , fType
    , fRegion
    , fId
    , fARN

    -- * FindingCriteria
    , FindingCriteria
    , findingCriteria
    , fcCriterion

    -- * FindingStatistics
    , FindingStatistics
    , findingStatistics
    , fsCountBySeverity

    -- * GeoLocation
    , GeoLocation
    , geoLocation
    , glLat
    , glLon

    -- * IAMInstanceProfile
    , IAMInstanceProfile
    , iamInstanceProfile
    , iapARN
    , iapId

    -- * InstanceDetails
    , InstanceDetails
    , instanceDetails
    , idInstanceId
    , idPlatform
    , idLaunchTime
    , idNetworkInterfaces
    , idInstanceType
    , idAvailabilityZone
    , idIAMInstanceProfile
    , idImageId
    , idProductCodes
    , idInstanceState
    , idTags
    , idImageDescription

    -- * Invitation
    , Invitation
    , invitation
    , iInvitedAt
    , iRelationshipStatus
    , iInvitationId
    , iAccountId

    -- * LocalPortDetails
    , LocalPortDetails
    , localPortDetails
    , lpdPortName
    , lpdPort

    -- * Master
    , Master
    , master
    , masInvitedAt
    , masRelationshipStatus
    , masInvitationId
    , masAccountId

    -- * Member
    , Member
    , member
    , mInvitedAt
    , mDetectorId
    , mEmail
    , mAccountId
    , mMasterId
    , mUpdatedAt
    , mRelationshipStatus

    -- * NetworkConnectionAction
    , NetworkConnectionAction
    , networkConnectionAction
    , ncaRemoteIPDetails
    , ncaProtocol
    , ncaRemotePortDetails
    , ncaBlocked
    , ncaConnectionDirection
    , ncaLocalPortDetails

    -- * NetworkInterface
    , NetworkInterface
    , networkInterface
    , niPrivateIPAddresses
    , niPublicDNSName
    , niSecurityGroups
    , niVPCId
    , niNetworkInterfaceId
    , niSubnetId
    , niPrivateIPAddress
    , niPublicIP
    , niPrivateDNSName
    , niIPv6Addresses

    -- * Organization
    , Organization
    , organization
    , oOrg
    , oASNOrg
    , oASN
    , oIsp

    -- * PortProbeAction
    , PortProbeAction
    , portProbeAction
    , ppaPortProbeDetails
    , ppaBlocked

    -- * PortProbeDetail
    , PortProbeDetail
    , portProbeDetail
    , ppdRemoteIPDetails
    , ppdLocalPortDetails

    -- * PrivateIPAddressDetails
    , PrivateIPAddressDetails
    , privateIPAddressDetails
    , piadPrivateIPAddress
    , piadPrivateDNSName

    -- * ProductCode
    , ProductCode
    , productCode
    , pcProductType
    , pcCode

    -- * RemoteIPDetails
    , RemoteIPDetails
    , remoteIPDetails
    , ridCountry
    , ridCity
    , ridIPAddressV4
    , ridGeoLocation
    , ridOrganization

    -- * RemotePortDetails
    , RemotePortDetails
    , remotePortDetails
    , rpdPortName
    , rpdPort

    -- * Resource
    , Resource
    , resource
    , rResourceType
    , rInstanceDetails
    , rAccessKeyDetails

    -- * SecurityGroup
    , SecurityGroup
    , securityGroup
    , sgGroupId
    , sgGroupName

    -- * ServiceInfo
    , ServiceInfo
    , serviceInfo
    , siCount
    , siEventFirstSeen
    , siAction
    , siDetectorId
    , siServiceName
    , siUserFeedback
    , siEventLastSeen
    , siResourceRole
    , siArchived

    -- * SortCriteria
    , SortCriteria
    , sortCriteria
    , scOrderBy
    , scAttributeName

    -- * Tag
    , Tag
    , tag
    , tagValue
    , tagKey

    -- * UnprocessedAccount
    , UnprocessedAccount
    , unprocessedAccount
    , uaAccountId
    , uaResult
    ) where

import Network.AWS.GuardDuty.Types.Product
import Network.AWS.GuardDuty.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2017-11-28@ of the Amazon GuardDuty SDK configuration.
guardDuty :: Service
guardDuty =
  Service
    { _svcAbbrev = "GuardDuty"
    , _svcSigner = v4
    , _svcPrefix = "guardduty"
    , _svcVersion = "2017-11-28"
    , _svcEndpoint = defaultEndpoint guardDuty
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "GuardDuty"
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


-- | Error response object.
_InternalServerErrorException :: AsError a => Getting (First ServiceError) a ServiceError
_InternalServerErrorException =
  _MatchServiceError guardDuty "InternalServerErrorException" . hasStatus 500


-- | Error response object.
_BadRequestException :: AsError a => Getting (First ServiceError) a ServiceError
_BadRequestException =
  _MatchServiceError guardDuty "BadRequestException" . hasStatus 400

