{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types
    (
    -- * Service Configuration
      lightsail

    -- * Errors
    , _AccessDeniedException
    , _AccountSetupInProgressException
    , _NotFoundException
    , _OperationFailureException
    , _ServiceException
    , _UnauthenticatedException
    , _InvalidInputException

    -- * AccessDirection
    , AccessDirection (..)

    -- * BlueprintType
    , BlueprintType (..)

    -- * DiskSnapshotState
    , DiskSnapshotState (..)

    -- * DiskState
    , DiskState (..)

    -- * InstanceAccessProtocol
    , InstanceAccessProtocol (..)

    -- * InstanceHealthReason
    , InstanceHealthReason (..)

    -- * InstanceHealthState
    , InstanceHealthState (..)

    -- * InstanceMetricName
    , InstanceMetricName (..)

    -- * InstancePlatform
    , InstancePlatform (..)

    -- * InstanceSnapshotState
    , InstanceSnapshotState (..)

    -- * LoadBalancerAttributeName
    , LoadBalancerAttributeName (..)

    -- * LoadBalancerMetricName
    , LoadBalancerMetricName (..)

    -- * LoadBalancerProtocol
    , LoadBalancerProtocol (..)

    -- * LoadBalancerState
    , LoadBalancerState (..)

    -- * LoadBalancerTLSCertificateDomainStatus
    , LoadBalancerTLSCertificateDomainStatus (..)

    -- * LoadBalancerTLSCertificateFailureReason
    , LoadBalancerTLSCertificateFailureReason (..)

    -- * LoadBalancerTLSCertificateRenewalStatus
    , LoadBalancerTLSCertificateRenewalStatus (..)

    -- * LoadBalancerTLSCertificateRevocationReason
    , LoadBalancerTLSCertificateRevocationReason (..)

    -- * LoadBalancerTLSCertificateStatus
    , LoadBalancerTLSCertificateStatus (..)

    -- * MetricStatistic
    , MetricStatistic (..)

    -- * MetricUnit
    , MetricUnit (..)

    -- * NetworkProtocol
    , NetworkProtocol (..)

    -- * OperationStatus
    , OperationStatus (..)

    -- * OperationType
    , OperationType (..)

    -- * PortAccessType
    , PortAccessType (..)

    -- * PortState
    , PortState (..)

    -- * RegionName
    , RegionName (..)

    -- * ResourceType
    , ResourceType (..)

    -- * AvailabilityZone
    , AvailabilityZone
    , availabilityZone
    , azState
    , azZoneName

    -- * Blueprint
    , Blueprint
    , blueprint
    , bVersionCode
    , bPlatform
    , bGroup
    , bMinPower
    , bProductURL
    , bLicenseURL
    , bName
    , bVersion
    , bBlueprintId
    , bType
    , bIsActive
    , bDescription

    -- * Bundle
    , Bundle
    , bundle
    , bunCpuCount
    , bunTransferPerMonthInGb
    , bunBundleId
    , bunInstanceType
    , bunName
    , bunPower
    , bunDiskSizeInGb
    , bunSupportedPlatforms
    , bunPrice
    , bunIsActive
    , bunRamSizeInGb

    -- * Disk
    , Disk
    , disk
    , dState
    , dResourceType
    , dArn
    , dPath
    , dCreatedAt
    , dLocation
    , dIops
    , dIsAttached
    , dAttachmentState
    , dName
    , dSizeInGb
    , dSupportCode
    , dIsSystemDisk
    , dAttachedTo
    , dGbInUse

    -- * DiskMap
    , DiskMap
    , diskMap
    , dmNewDiskName
    , dmOriginalDiskPath

    -- * DiskSnapshot
    , DiskSnapshot
    , diskSnapshot
    , dsFromDiskName
    , dsState
    , dsResourceType
    , dsArn
    , dsCreatedAt
    , dsLocation
    , dsProgress
    , dsName
    , dsSizeInGb
    , dsSupportCode
    , dsFromDiskARN

    -- * Domain
    , Domain
    , domain
    , domResourceType
    , domDomainEntries
    , domArn
    , domCreatedAt
    , domLocation
    , domName
    , domSupportCode

    -- * DomainEntry
    , DomainEntry
    , domainEntry
    , deIsAlias
    , deName
    , deId
    , deOptions
    , deType
    , deTarget

    -- * Instance
    , Instance
    , instance'
    , iState
    , iIpv6Address
    , iResourceType
    , iArn
    , iCreatedAt
    , iLocation
    , iSshKeyName
    , iUsername
    , iNetworking
    , iBundleId
    , iName
    , iSupportCode
    , iBlueprintId
    , iPrivateIPAddress
    , iBlueprintName
    , iIsStaticIP
    , iPublicIPAddress
    , iHardware

    -- * InstanceAccessDetails
    , InstanceAccessDetails
    , instanceAccessDetails
    , iadCertKey
    , iadIpAddress
    , iadPrivateKey
    , iadExpiresAt
    , iadUsername
    , iadProtocol
    , iadPasswordData
    , iadPassword
    , iadInstanceName

    -- * InstanceHardware
    , InstanceHardware
    , instanceHardware
    , ihCpuCount
    , ihDisks
    , ihRamSizeInGb

    -- * InstanceHealthSummary
    , InstanceHealthSummary
    , instanceHealthSummary
    , ihsInstanceHealth
    , ihsInstanceName
    , ihsInstanceHealthReason

    -- * InstanceNetworking
    , InstanceNetworking
    , instanceNetworking
    , inMonthlyTransfer
    , inPorts

    -- * InstancePortInfo
    , InstancePortInfo
    , instancePortInfo
    , ipiFromPort
    , ipiCommonName
    , ipiProtocol
    , ipiAccessDirection
    , ipiAccessType
    , ipiToPort
    , ipiAccessFrom

    -- * InstancePortState
    , InstancePortState
    , instancePortState
    , ipsFromPort
    , ipsState
    , ipsProtocol
    , ipsToPort

    -- * InstanceSnapshot
    , InstanceSnapshot
    , instanceSnapshot
    , insFromBlueprintId
    , insState
    , insResourceType
    , insFromAttachedDisks
    , insArn
    , insCreatedAt
    , insLocation
    , insProgress
    , insName
    , insFromBundleId
    , insSizeInGb
    , insSupportCode
    , insFromInstanceARN
    , insFromInstanceName

    -- * InstanceState
    , InstanceState
    , instanceState
    , isName
    , isCode

    -- * KeyPair
    , KeyPair
    , keyPair
    , kpResourceType
    , kpArn
    , kpCreatedAt
    , kpLocation
    , kpFingerprint
    , kpName
    , kpSupportCode

    -- * LoadBalancer
    , LoadBalancer
    , loadBalancer
    , lbHealthCheckPath
    , lbState
    , lbResourceType
    , lbArn
    , lbCreatedAt
    , lbLocation
    , lbInstancePort
    , lbConfigurationOptions
    , lbProtocol
    , lbTlsCertificateSummaries
    , lbName
    , lbSupportCode
    , lbPublicPorts
    , lbDnsName
    , lbInstanceHealthSummary

    -- * LoadBalancerTLSCertificate
    , LoadBalancerTLSCertificate
    , loadBalancerTLSCertificate
    , lbtcFailureReason
    , lbtcSubject
    , lbtcStatus
    , lbtcSubjectAlternativeNames
    , lbtcResourceType
    , lbtcArn
    , lbtcCreatedAt
    , lbtcLocation
    , lbtcLoadBalancerName
    , lbtcSerial
    , lbtcIsAttached
    , lbtcRevokedAt
    , lbtcNotBefore
    , lbtcRevocationReason
    , lbtcDomainName
    , lbtcName
    , lbtcRenewalSummary
    , lbtcSupportCode
    , lbtcDomainValidationRecords
    , lbtcIssuedAt
    , lbtcKeyAlgorithm
    , lbtcSignatureAlgorithm
    , lbtcIssuer
    , lbtcNotAfter

    -- * LoadBalancerTLSCertificateDomainValidationOption
    , LoadBalancerTLSCertificateDomainValidationOption
    , loadBalancerTLSCertificateDomainValidationOption
    , lbtcdvoDomainName
    , lbtcdvoValidationStatus

    -- * LoadBalancerTLSCertificateDomainValidationRecord
    , LoadBalancerTLSCertificateDomainValidationRecord
    , loadBalancerTLSCertificateDomainValidationRecord
    , lbtcdvrValue
    , lbtcdvrDomainName
    , lbtcdvrName
    , lbtcdvrValidationStatus
    , lbtcdvrType

    -- * LoadBalancerTLSCertificateRenewalSummary
    , LoadBalancerTLSCertificateRenewalSummary
    , loadBalancerTLSCertificateRenewalSummary
    , lbtcrsRenewalStatus
    , lbtcrsDomainValidationOptions

    -- * LoadBalancerTLSCertificateSummary
    , LoadBalancerTLSCertificateSummary
    , loadBalancerTLSCertificateSummary
    , lbtcsIsAttached
    , lbtcsName

    -- * MetricDatapoint
    , MetricDatapoint
    , metricDatapoint
    , mdSampleCount
    , mdMaximum
    , mdAverage
    , mdMinimum
    , mdSum
    , mdTimestamp
    , mdUnit

    -- * MonthlyTransfer
    , MonthlyTransfer
    , monthlyTransfer
    , mtGbPerMonthAllocated

    -- * Operation
    , Operation
    , operation
    , oStatus
    , oOperationDetails
    , oResourceType
    , oCreatedAt
    , oResourceName
    , oLocation
    , oStatusChangedAt
    , oErrorDetails
    , oErrorCode
    , oId
    , oOperationType
    , oIsTerminal

    -- * PasswordData
    , PasswordData
    , passwordData
    , pdKeyPairName
    , pdCiphertext

    -- * PortInfo
    , PortInfo
    , portInfo
    , piFromPort
    , piProtocol
    , piToPort

    -- * RegionInfo
    , RegionInfo
    , regionInfo
    , riAvailabilityZones
    , riName
    , riDisplayName
    , riContinentCode
    , riDescription

    -- * ResourceLocation
    , ResourceLocation
    , resourceLocation
    , rlRegionName
    , rlAvailabilityZone

    -- * StaticIP
    , StaticIP
    , staticIP
    , siIpAddress
    , siResourceType
    , siArn
    , siCreatedAt
    , siLocation
    , siIsAttached
    , siName
    , siSupportCode
    , siAttachedTo
    ) where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types.Product
import Network.AWS.Lightsail.Types.Sum
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2016-11-28@ of the Amazon Lightsail SDK configuration.
lightsail :: Service
lightsail =
  Service
    { _svcAbbrev = "Lightsail"
    , _svcSigner = v4
    , _svcPrefix = "lightsail"
    , _svcVersion = "2016-11-28"
    , _svcEndpoint = defaultEndpoint lightsail
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "Lightsail"
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


-- | Lightsail throws this exception when the user cannot be authenticated or uses invalid credentials to access a resource.
--
--
_AccessDeniedException :: AsError a => Getting (First ServiceError) a ServiceError
_AccessDeniedException = _MatchServiceError lightsail "AccessDeniedException"


-- | Lightsail throws this exception when an account is still in the setup in progress state.
--
--
_AccountSetupInProgressException :: AsError a => Getting (First ServiceError) a ServiceError
_AccountSetupInProgressException =
  _MatchServiceError lightsail "AccountSetupInProgressException"


-- | Lightsail throws this exception when it cannot find a resource.
--
--
_NotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_NotFoundException = _MatchServiceError lightsail "NotFoundException"


-- | Lightsail throws this exception when an operation fails to execute.
--
--
_OperationFailureException :: AsError a => Getting (First ServiceError) a ServiceError
_OperationFailureException =
  _MatchServiceError lightsail "OperationFailureException"


-- | A general service exception.
--
--
_ServiceException :: AsError a => Getting (First ServiceError) a ServiceError
_ServiceException = _MatchServiceError lightsail "ServiceException"


-- | Lightsail throws this exception when the user has not been authenticated.
--
--
_UnauthenticatedException :: AsError a => Getting (First ServiceError) a ServiceError
_UnauthenticatedException =
  _MatchServiceError lightsail "UnauthenticatedException"


-- | Lightsail throws this exception when user input does not conform to the validation rules of an input field.
--
--
_InvalidInputException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidInputException = _MatchServiceError lightsail "InvalidInputException"

