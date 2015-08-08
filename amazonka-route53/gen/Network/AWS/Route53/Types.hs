{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53.Types
    (
    -- * Service Decription
      Route53

    -- * Error Matchers
    , _HealthCheckVersionMismatch
    , _InvalidInput
    , _HostedZoneNotEmpty
    , _InvalidArgument
    , _DelegationSetAlreadyReusable
    , _PriorRequestNotComplete
    , _InvalidChangeBatch
    , _InvalidDomainName
    , _DelegationSetNotReusable
    , _HealthCheckAlreadyExists
    , _HostedZoneNotFound
    , _DelegationSetInUse
    , _NoSuchDelegationSet
    , _NoSuchGeoLocation
    , _DelegationSetNotAvailable
    , _VPCAssociationNotFound
    , _ThrottlingException
    , _NoSuchChange
    , _LimitsExceeded
    , _IncompatibleVersion
    , _NoSuchHostedZone
    , _TooManyHostedZones
    , _PublicZoneVPCAssociation
    , _ConflictingDomainExists
    , _LastVPCAssociation
    , _HealthCheckInUse
    , _DelegationSetAlreadyCreated
    , _TooManyHealthChecks
    , _NoSuchHealthCheck
    , _HostedZoneAlreadyExists
    , _InvalidVPCId

    -- * Re-exported Types
    , module Network.AWS.Route53.Internal

    -- * ChangeAction
    , ChangeAction (..)

    -- * ChangeStatus
    , ChangeStatus (..)

    -- * Failover
    , Failover (..)

    -- * HealthCheckType
    , HealthCheckType (..)

    -- * RecordType
    , RecordType (..)

    -- * TagResourceType
    , TagResourceType (..)

    -- * VPCRegion
    , VPCRegion (..)

    -- * AliasTarget
    , AliasTarget
    , aliasTarget
    , atHostedZoneId
    , atDNSName
    , atEvaluateTargetHealth

    -- * Change
    , Change
    , change
    , cAction
    , cResourceRecordSet

    -- * ChangeBatch
    , ChangeBatch
    , changeBatch
    , cbComment
    , cbChanges

    -- * ChangeInfo
    , ChangeInfo
    , changeInfo
    , ciComment
    , ciId
    , ciStatus
    , ciSubmittedAt

    -- * DelegationSet
    , DelegationSet
    , delegationSet
    , dsId
    , dsCallerReference
    , dsNameServers

    -- * GeoLocation
    , GeoLocation
    , geoLocation
    , glSubdivisionCode
    , glCountryCode
    , glContinentCode

    -- * GeoLocationDetails
    , GeoLocationDetails
    , geoLocationDetails
    , gldSubdivisionName
    , gldSubdivisionCode
    , gldCountryName
    , gldCountryCode
    , gldContinentCode
    , gldContinentName

    -- * HealthCheck
    , HealthCheck
    , healthCheck
    , hcId
    , hcCallerReference
    , hcHealthCheckConfig
    , hcHealthCheckVersion

    -- * HealthCheckConfig
    , HealthCheckConfig
    , healthCheckConfig
    , hccIPAddress
    , hccFailureThreshold
    , hccSearchString
    , hccResourcePath
    , hccFullyQualifiedDomainName
    , hccRequestInterval
    , hccPort
    , hccType

    -- * HealthCheckObservation
    , HealthCheckObservation
    , healthCheckObservation
    , hcoIPAddress
    , hcoStatusReport

    -- * HostedZone
    , HostedZone
    , hostedZone
    , hzConfig
    , hzResourceRecordSetCount
    , hzId
    , hzName
    , hzCallerReference

    -- * HostedZoneConfig
    , HostedZoneConfig
    , hostedZoneConfig
    , hzcPrivateZone
    , hzcComment

    -- * ResourceRecord
    , ResourceRecord
    , resourceRecord
    , rrValue

    -- * ResourceRecordSet
    , ResourceRecordSet
    , resourceRecordSet
    , rrsResourceRecords
    , rrsTTL
    , rrsAliasTarget
    , rrsWeight
    , rrsSetIdentifier
    , rrsFailover
    , rrsHealthCheckId
    , rrsRegion
    , rrsGeoLocation
    , rrsName
    , rrsType

    -- * ResourceTagSet
    , ResourceTagSet
    , resourceTagSet
    , rtsResourceId
    , rtsResourceType
    , rtsTags

    -- * StatusReport
    , StatusReport
    , statusReport
    , srStatus
    , srCheckedTime

    -- * Tag
    , Tag
    , tag
    , tagValue
    , tagKey

    -- * VPC
    , VPC
    , vpc
    , vpcVPCRegion
    , vpcVPCId
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Route53.Internal
import           Network.AWS.Route53.Types.Product
import           Network.AWS.Route53.Types.Sum
import           Network.AWS.Sign.V4

-- | Version @2013-04-01@ of the Amazon Route 53 SDK.
data Route53

instance AWSService Route53 where
    type Sg Route53 = V4
    service = const svc
      where
        svc =
            Service
            { _svcAbbrev = "Route53"
            , _svcPrefix = "route53"
            , _svcVersion = "2013-04-01"
            , _svcEndpoint = defaultEndpoint svc
            , _svcTimeout = Just 70
            , _svcStatus = statusSuccess
            , _svcError = parseXMLError
            , _svcRetry = retry
            }
        retry =
            Exponential
            { _retryBase = 5.0e-2
            , _retryGrowth = 2
            , _retryAttempts = 5
            , _retryCheck = check
            }
        check e
          | has (hasCode "ThrottlingException" . hasStatus 400) e =
              Just "throttling_exception"
          | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
          | has (hasStatus 503) e = Just "service_unavailable"
          | has (hasStatus 500) e = Just "general_server_error"
          | has (hasStatus 509) e = Just "limit_exceeded"
          | otherwise = Nothing

-- | Prism for HealthCheckVersionMismatch' errors.
_HealthCheckVersionMismatch :: AsError a => Getting (First ServiceError) a ServiceError
_HealthCheckVersionMismatch =
    _ServiceError . hasStatus 409 . hasCode "HealthCheckVersionMismatch"

-- | Some value specified in the request is invalid or the XML document is
-- malformed.
_InvalidInput :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidInput = _ServiceError . hasStatus 400 . hasCode "InvalidInput"

-- | The hosted zone contains resource record sets in addition to the default
-- NS and SOA resource record sets. Before you can delete the hosted zone,
-- you must delete the additional resource record sets.
_HostedZoneNotEmpty :: AsError a => Getting (First ServiceError) a ServiceError
_HostedZoneNotEmpty =
    _ServiceError . hasStatus 400 . hasCode "HostedZoneNotEmpty"

-- | At least one of the specified arguments is invalid.
_InvalidArgument :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidArgument = _ServiceError . hasCode "InvalidArgument"

-- | The specified delegation set has already been marked as reusable.
_DelegationSetAlreadyReusable :: AsError a => Getting (First ServiceError) a ServiceError
_DelegationSetAlreadyReusable =
    _ServiceError . hasCode "DelegationSetAlreadyReusable"

-- | The request was rejected because Route 53 was still processing a prior
-- request.
_PriorRequestNotComplete :: AsError a => Getting (First ServiceError) a ServiceError
_PriorRequestNotComplete =
    _ServiceError . hasStatus 400 . hasCode "PriorRequestNotComplete"

-- | This error contains a list of one or more error messages. Each error
-- message indicates one error in the change batch. For more information,
-- see
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/API_ChangeResourceRecordSets.html#example_Errors Example InvalidChangeBatch Errors>.
_InvalidChangeBatch :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidChangeBatch = _ServiceError . hasCode "InvalidChangeBatch"

-- | This error indicates that the specified domain name is not valid.
_InvalidDomainName :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDomainName =
    _ServiceError . hasStatus 400 . hasCode "InvalidDomainName"

-- | The specified delegation set has not been marked as reusable.
_DelegationSetNotReusable :: AsError a => Getting (First ServiceError) a ServiceError
_DelegationSetNotReusable = _ServiceError . hasCode "DelegationSetNotReusable"

-- | The health check you are trying to create already exists. Route 53
-- returns this error when a health check has already been created with the
-- specified @CallerReference@.
_HealthCheckAlreadyExists :: AsError a => Getting (First ServiceError) a ServiceError
_HealthCheckAlreadyExists =
    _ServiceError . hasStatus 409 . hasCode "HealthCheckAlreadyExists"

-- | The specified HostedZone cannot be found.
_HostedZoneNotFound :: AsError a => Getting (First ServiceError) a ServiceError
_HostedZoneNotFound = _ServiceError . hasCode "HostedZoneNotFound"

-- | The specified delegation contains associated hosted zones which must be
-- deleted before the reusable delegation set can be deleted.
_DelegationSetInUse :: AsError a => Getting (First ServiceError) a ServiceError
_DelegationSetInUse = _ServiceError . hasCode "DelegationSetInUse"

-- | The specified delegation set does not exist.
_NoSuchDelegationSet :: AsError a => Getting (First ServiceError) a ServiceError
_NoSuchDelegationSet = _ServiceError . hasCode "NoSuchDelegationSet"

-- | The geo location you are trying to get does not exist.
_NoSuchGeoLocation :: AsError a => Getting (First ServiceError) a ServiceError
_NoSuchGeoLocation =
    _ServiceError . hasStatus 404 . hasCode "NoSuchGeoLocation"

-- | Route 53 allows some duplicate domain names, but there is a maximum
-- number of duplicate names. This error indicates that you have reached
-- that maximum. If you want to create another hosted zone with the same
-- name and Route 53 generates this error, you can request an increase to
-- the limit on the <http://aws.amazon.com/route53-request/ Contact Us>
-- page.
_DelegationSetNotAvailable :: AsError a => Getting (First ServiceError) a ServiceError
_DelegationSetNotAvailable =
    _ServiceError . hasCode "DelegationSetNotAvailable"

-- | The VPC you specified is not currently associated with the hosted zone.
_VPCAssociationNotFound :: AsError a => Getting (First ServiceError) a ServiceError
_VPCAssociationNotFound =
    _ServiceError . hasStatus 404 . hasCode "VPCAssociationNotFound"

-- | Prism for ThrottlingException' errors.
_ThrottlingException :: AsError a => Getting (First ServiceError) a ServiceError
_ThrottlingException =
    _ServiceError . hasStatus 400 . hasCode "ThrottlingException"

-- | Prism for NoSuchChange' errors.
_NoSuchChange :: AsError a => Getting (First ServiceError) a ServiceError
_NoSuchChange = _ServiceError . hasStatus 404 . hasCode "NoSuchChange"

-- | The limits specified for a resource have been exceeded.
_LimitsExceeded :: AsError a => Getting (First ServiceError) a ServiceError
_LimitsExceeded = _ServiceError . hasCode "LimitsExceeded"

-- | The resource you are trying to access is unsupported on this Route 53
-- endpoint. Please consider using a newer endpoint or a tool that does so.
_IncompatibleVersion :: AsError a => Getting (First ServiceError) a ServiceError
_IncompatibleVersion =
    _ServiceError . hasStatus 400 . hasCode "IncompatibleVersion"

-- | Prism for NoSuchHostedZone' errors.
_NoSuchHostedZone :: AsError a => Getting (First ServiceError) a ServiceError
_NoSuchHostedZone = _ServiceError . hasStatus 404 . hasCode "NoSuchHostedZone"

-- | This error indicates that you\'ve reached the maximum number of hosted
-- zones that can be created for the current AWS account. You can request
-- an increase to the limit on the
-- <http://aws.amazon.com/route53-request/ Contact Us> page.
_TooManyHostedZones :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyHostedZones =
    _ServiceError . hasStatus 400 . hasCode "TooManyHostedZones"

-- | The hosted zone you are trying to associate VPC with doesn\'t have any
-- VPC association. Route 53 currently doesn\'t support associate a VPC
-- with a public hosted zone.
_PublicZoneVPCAssociation :: AsError a => Getting (First ServiceError) a ServiceError
_PublicZoneVPCAssociation =
    _ServiceError . hasStatus 400 . hasCode "PublicZoneVPCAssociation"

-- | Prism for ConflictingDomainExists' errors.
_ConflictingDomainExists :: AsError a => Getting (First ServiceError) a ServiceError
_ConflictingDomainExists = _ServiceError . hasCode "ConflictingDomainExists"

-- | The VPC you are trying to disassociate from the hosted zone is the last
-- the VPC that is associated with the hosted zone. Route 53 currently
-- doesn\'t support disassociate the last VPC from the hosted zone.
_LastVPCAssociation :: AsError a => Getting (First ServiceError) a ServiceError
_LastVPCAssociation =
    _ServiceError . hasStatus 400 . hasCode "LastVPCAssociation"

-- | There are resource records associated with this health check. Before you
-- can delete the health check, you must disassociate it from the resource
-- record sets.
_HealthCheckInUse :: AsError a => Getting (First ServiceError) a ServiceError
_HealthCheckInUse = _ServiceError . hasStatus 400 . hasCode "HealthCheckInUse"

-- | A delegation set with the same owner and caller reference combination
-- has already been created.
_DelegationSetAlreadyCreated :: AsError a => Getting (First ServiceError) a ServiceError
_DelegationSetAlreadyCreated =
    _ServiceError . hasCode "DelegationSetAlreadyCreated"

-- | Prism for TooManyHealthChecks' errors.
_TooManyHealthChecks :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyHealthChecks = _ServiceError . hasCode "TooManyHealthChecks"

-- | The health check you are trying to get or delete does not exist.
_NoSuchHealthCheck :: AsError a => Getting (First ServiceError) a ServiceError
_NoSuchHealthCheck =
    _ServiceError . hasStatus 404 . hasCode "NoSuchHealthCheck"

-- | The hosted zone you are trying to create already exists. Route 53
-- returns this error when a hosted zone has already been created with the
-- specified @CallerReference@.
_HostedZoneAlreadyExists :: AsError a => Getting (First ServiceError) a ServiceError
_HostedZoneAlreadyExists =
    _ServiceError . hasStatus 409 . hasCode "HostedZoneAlreadyExists"

-- | The hosted zone you are trying to create for your VPC_ID does not belong
-- to you. Route 53 returns this error when the VPC specified by @VPCId@
-- does not belong to you.
_InvalidVPCId :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidVPCId = _ServiceError . hasStatus 400 . hasCode "InvalidVPCId"
