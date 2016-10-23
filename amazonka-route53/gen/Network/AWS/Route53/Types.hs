{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53.Types
    (
    -- * Service Configuration
      route53

    -- * Errors
    , _HealthCheckVersionMismatch
    , _InvalidInput
    , _HostedZoneNotEmpty
    , _InvalidArgument
    , _TrafficPolicyInstanceAlreadyExists
    , _ConflictingTypes
    , _ConcurrentModification
    , _DelegationSetAlreadyReusable
    , _PriorRequestNotComplete
    , _InvalidChangeBatch
    , _TrafficPolicyAlreadyExists
    , _InvalidTrafficPolicyDocument
    , _DelegationSetNotReusable
    , _InvalidDomainName
    , _NoSuchTrafficPolicy
    , _HostedZoneNotFound
    , _DelegationSetInUse
    , _NoSuchDelegationSet
    , _HealthCheckAlreadyExists
    , _TooManyTrafficPolicies
    , _NoSuchGeoLocation
    , _DelegationSetNotAvailable
    , _VPCAssociationNotFound
    , _ThrottlingException
    , _NoSuchChange
    , _LimitsExceeded
    , _TooManyTrafficPolicyInstances
    , _NoSuchTrafficPolicyInstance
    , _IncompatibleVersion
    , _PublicZoneVPCAssociation
    , _NoSuchHostedZone
    , _TooManyHostedZones
    , _HealthCheckInUse
    , _DelegationSetAlreadyCreated
    , _ConflictingDomainExists
    , _LastVPCAssociation
    , _TooManyHealthChecks
    , _NoSuchHealthCheck
    , _TrafficPolicyInUse
    , _InvalidVPCId
    , _HostedZoneAlreadyExists

    -- * Re-exported Types
    , module Network.AWS.Route53.Internal

    -- * ChangeAction
    , ChangeAction (..)

    -- * ChangeStatus
    , ChangeStatus (..)

    -- * CloudWatchRegion
    , CloudWatchRegion (..)

    -- * ComparisonOperator
    , ComparisonOperator (..)

    -- * Failover
    , Failover (..)

    -- * HealthCheckRegion
    , HealthCheckRegion (..)

    -- * HealthCheckType
    , HealthCheckType (..)

    -- * InsufficientDataHealthStatus
    , InsufficientDataHealthStatus (..)

    -- * RecordType
    , RecordType (..)

    -- * Statistic
    , Statistic (..)

    -- * TagResourceType
    , TagResourceType (..)

    -- * VPCRegion
    , VPCRegion (..)

    -- * AlarmIdentifier
    , AlarmIdentifier
    , alarmIdentifier
    , aiRegion
    , aiName

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

    -- * CloudWatchAlarmConfiguration
    , CloudWatchAlarmConfiguration
    , cloudWatchAlarmConfiguration
    , cwacDimensions
    , cwacEvaluationPeriods
    , cwacThreshold
    , cwacComparisonOperator
    , cwacPeriod
    , cwacMetricName
    , cwacNamespace
    , cwacStatistic

    -- * DelegationSet
    , DelegationSet
    , delegationSet
    , dsId
    , dsCallerReference
    , dsNameServers

    -- * Dimension
    , Dimension
    , dimension
    , dName
    , dValue

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
    , hcCloudWatchAlarmConfiguration
    , hcId
    , hcCallerReference
    , hcHealthCheckConfig
    , hcHealthCheckVersion

    -- * HealthCheckConfig
    , HealthCheckConfig
    , healthCheckConfig
    , hccFailureThreshold
    , hccIPAddress
    , hccEnableSNI
    , hccSearchString
    , hccHealthThreshold
    , hccRegions
    , hccResourcePath
    , hccInsufficientDataHealthStatus
    , hccAlarmIdentifier
    , hccMeasureLatency
    , hccInverted
    , hccFullyQualifiedDomainName
    , hccChildHealthChecks
    , hccRequestInterval
    , hccPort
    , hccType

    -- * HealthCheckObservation
    , HealthCheckObservation
    , healthCheckObservation
    , hcoIPAddress
    , hcoStatusReport
    , hcoRegion

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
    , rrsTTL
    , rrsResourceRecords
    , rrsAliasTarget
    , rrsWeight
    , rrsTrafficPolicyInstanceId
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

    -- * TrafficPolicy
    , TrafficPolicy
    , trafficPolicy
    , tpComment
    , tpId
    , tpVersion
    , tpName
    , tpType
    , tpDocument

    -- * TrafficPolicyInstance
    , TrafficPolicyInstance
    , trafficPolicyInstance
    , tpiId
    , tpiHostedZoneId
    , tpiName
    , tpiTTL
    , tpiState
    , tpiMessage
    , tpiTrafficPolicyId
    , tpiTrafficPolicyVersion
    , tpiTrafficPolicyType

    -- * TrafficPolicySummary
    , TrafficPolicySummary
    , trafficPolicySummary
    , tpsId
    , tpsName
    , tpsType
    , tpsLatestVersion
    , tpsTrafficPolicyCount

    -- * VPC
    , VPC
    , vpc
    , vpcVPCRegion
    , vpcVPCId
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Route53.Internal
import           Network.AWS.Route53.Types.Product
import           Network.AWS.Route53.Types.Sum
import           Network.AWS.Sign.V4

-- | API version '2013-04-01' of the Amazon Route 53 SDK configuration.
route53 :: Service
route53 =
    Service
    { _svcAbbrev = "Route53"
    , _svcSigner = v4
    , _svcPrefix = "route53"
    , _svcVersion = "2013-04-01"
    , _svcEndpoint = defaultEndpoint route53
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseXMLError "Route53"
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
      | has (hasCode "Throttling" . hasStatus 400) e =
          Just "request_limit_exceeded"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
          Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "PriorRequestNotComplete" . hasStatus 400) e =
          Just "still_processing"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing

-- | Prism for HealthCheckVersionMismatch' errors.
_HealthCheckVersionMismatch :: AsError a => Getting (First ServiceError) a ServiceError
_HealthCheckVersionMismatch =
    _ServiceError . hasStatus 409 . hasCode "HealthCheckVersionMismatch"

-- | The input is not valid.
_InvalidInput :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidInput = _ServiceError . hasStatus 400 . hasCode "InvalidInput"

-- | The hosted zone contains resource records that are not SOA or NS records.
_HostedZoneNotEmpty :: AsError a => Getting (First ServiceError) a ServiceError
_HostedZoneNotEmpty =
    _ServiceError . hasStatus 400 . hasCode "HostedZoneNotEmpty"

-- | Parameter name and problem.
_InvalidArgument :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidArgument = _ServiceError . hasCode "InvalidArgument"

-- | Traffic policy instance with given Id already exists.
_TrafficPolicyInstanceAlreadyExists :: AsError a => Getting (First ServiceError) a ServiceError
_TrafficPolicyInstanceAlreadyExists =
    _ServiceError .
    hasStatus 409 . hasCode "TrafficPolicyInstanceAlreadyExists"

-- | You tried to update a traffic policy instance by using a traffic policy version that has a different DNS type than the current type for the instance. You specified the type in the JSON document in the 'CreateTrafficPolicy' or 'CreateTrafficPolicyVersion'request.
_ConflictingTypes :: AsError a => Getting (First ServiceError) a ServiceError
_ConflictingTypes = _ServiceError . hasStatus 400 . hasCode "ConflictingTypes"

-- | Another user submitted a request to update the object at the same time that you did. Retry the request.
_ConcurrentModification :: AsError a => Getting (First ServiceError) a ServiceError
_ConcurrentModification =
    _ServiceError . hasStatus 400 . hasCode "ConcurrentModification"

-- | The specified delegation set has already been marked as reusable.
_DelegationSetAlreadyReusable :: AsError a => Getting (First ServiceError) a ServiceError
_DelegationSetAlreadyReusable =
    _ServiceError . hasCode "DelegationSetAlreadyReusable"

-- | If Amazon Route 53 can\'t process a request before the next request arrives, it will reject subsequent requests for the same hosted zone and return an 'HTTP 400 error' ('Bad request'). If Amazon Route 53 returns this error repeatedly for the same request, we recommend that you wait, in intervals of increasing duration, before you try the request again.
_PriorRequestNotComplete :: AsError a => Getting (First ServiceError) a ServiceError
_PriorRequestNotComplete =
    _ServiceError . hasStatus 400 . hasCode "PriorRequestNotComplete"

-- | This exception contains a list of messages that might contain one or more error messages. Each error message indicates one error in the change batch.
_InvalidChangeBatch :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidChangeBatch = _ServiceError . hasCode "InvalidChangeBatch"

-- | A traffic policy that has the same value for 'Name' already exists.
_TrafficPolicyAlreadyExists :: AsError a => Getting (First ServiceError) a ServiceError
_TrafficPolicyAlreadyExists =
    _ServiceError . hasStatus 409 . hasCode "TrafficPolicyAlreadyExists"

-- | The format of the traffic policy document that you specified in the 'Document' element is invalid.
_InvalidTrafficPolicyDocument :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidTrafficPolicyDocument =
    _ServiceError . hasStatus 400 . hasCode "InvalidTrafficPolicyDocument"

-- | A reusable delegation set with the specified ID does not exist.
_DelegationSetNotReusable :: AsError a => Getting (First ServiceError) a ServiceError
_DelegationSetNotReusable = _ServiceError . hasCode "DelegationSetNotReusable"

-- | The specified domain name is not valid.
_InvalidDomainName :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDomainName =
    _ServiceError . hasStatus 400 . hasCode "InvalidDomainName"

-- | No traffic policy exists with the specified ID.
_NoSuchTrafficPolicy :: AsError a => Getting (First ServiceError) a ServiceError
_NoSuchTrafficPolicy =
    _ServiceError . hasStatus 404 . hasCode "NoSuchTrafficPolicy"

-- | The specified HostedZone cannot be found.
_HostedZoneNotFound :: AsError a => Getting (First ServiceError) a ServiceError
_HostedZoneNotFound = _ServiceError . hasCode "HostedZoneNotFound"

-- | The specified delegation contains associated hosted zones which must be deleted before the reusable delegation set can be deleted.
_DelegationSetInUse :: AsError a => Getting (First ServiceError) a ServiceError
_DelegationSetInUse = _ServiceError . hasCode "DelegationSetInUse"

-- | A reusable delegation set with the specified ID does not exist.
_NoSuchDelegationSet :: AsError a => Getting (First ServiceError) a ServiceError
_NoSuchDelegationSet = _ServiceError . hasCode "NoSuchDelegationSet"

-- | The health check you\'re attempting to create already exists.
--
-- Amazon Route 53 returns this error when a health check has already been created with the specified value for 'CallerReference'.
_HealthCheckAlreadyExists :: AsError a => Getting (First ServiceError) a ServiceError
_HealthCheckAlreadyExists =
    _ServiceError . hasStatus 409 . hasCode "HealthCheckAlreadyExists"

-- | You\'ve created the maximum number of traffic policies that can be created for the current AWS account. You can request an increase to the limit on the <http://aws.amazon.com/route53-request/ Contact Us> page.
_TooManyTrafficPolicies :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyTrafficPolicies =
    _ServiceError . hasStatus 400 . hasCode "TooManyTrafficPolicies"

-- | Amazon Route 53 doesn\'t support the specified geolocation.
_NoSuchGeoLocation :: AsError a => Getting (First ServiceError) a ServiceError
_NoSuchGeoLocation =
    _ServiceError . hasStatus 404 . hasCode "NoSuchGeoLocation"

-- | You can create a hosted zone that has the same name as an existing hosted zone (example.com is common), but there is a limit to the number of hosted zones that have the same name. If you get this error, Amazon Route 53 has reached that limit. If you own the domain name and Amazon Route 53 generates this error, contact Customer Support.
_DelegationSetNotAvailable :: AsError a => Getting (First ServiceError) a ServiceError
_DelegationSetNotAvailable =
    _ServiceError . hasCode "DelegationSetNotAvailable"

-- | The specified VPC and hosted zone are not currently associated.
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

-- | You\'ve created the maximum number of traffic policy instances that can be created for the current AWS account. You can request an increase to the limit on the <http://aws.amazon.com/route53-request/ Contact Us> page.
_TooManyTrafficPolicyInstances :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyTrafficPolicyInstances =
    _ServiceError . hasStatus 400 . hasCode "TooManyTrafficPolicyInstances"

-- | No traffic policy instance exists with the specified ID.
_NoSuchTrafficPolicyInstance :: AsError a => Getting (First ServiceError) a ServiceError
_NoSuchTrafficPolicyInstance =
    _ServiceError . hasStatus 404 . hasCode "NoSuchTrafficPolicyInstance"

-- | The resource you are trying to access is unsupported on this Amazon Route 53 endpoint. Please consider using a newer endpoint or a tool that does so.
_IncompatibleVersion :: AsError a => Getting (First ServiceError) a ServiceError
_IncompatibleVersion =
    _ServiceError . hasStatus 400 . hasCode "IncompatibleVersion"

-- | The hosted zone specified in 'HostedZoneId' is a public hosted zone.
_PublicZoneVPCAssociation :: AsError a => Getting (First ServiceError) a ServiceError
_PublicZoneVPCAssociation =
    _ServiceError . hasStatus 400 . hasCode "PublicZoneVPCAssociation"

-- | No hosted zone exists with the ID that you specified.
_NoSuchHostedZone :: AsError a => Getting (First ServiceError) a ServiceError
_NoSuchHostedZone = _ServiceError . hasStatus 404 . hasCode "NoSuchHostedZone"

-- | This hosted zone cannot be created because the hosted zone limit is exceeded. To request a limit increase, go to the Amazon Route 53 <http://aws.amazon.com/route53-request/ Contact Us> page.
_TooManyHostedZones :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyHostedZones =
    _ServiceError . hasStatus 400 . hasCode "TooManyHostedZones"

-- | The health check ID for this health check is referenced in the 'HealthCheckId' element in one of the resource record sets in one of the hosted zones that are owned by the current AWS account.
_HealthCheckInUse :: AsError a => Getting (First ServiceError) a ServiceError
_HealthCheckInUse = _ServiceError . hasStatus 400 . hasCode "HealthCheckInUse"

-- | A delegation set with the same owner and caller reference combination has already been created.
_DelegationSetAlreadyCreated :: AsError a => Getting (First ServiceError) a ServiceError
_DelegationSetAlreadyCreated =
    _ServiceError . hasCode "DelegationSetAlreadyCreated"

-- | Prism for ConflictingDomainExists' errors.
_ConflictingDomainExists :: AsError a => Getting (First ServiceError) a ServiceError
_ConflictingDomainExists = _ServiceError . hasCode "ConflictingDomainExists"

-- | Only one VPC is currently associated with the hosted zone. You cannot convert a private hosted zone into a public hosted zone by disassociating the last VPC from a hosted zone.
_LastVPCAssociation :: AsError a => Getting (First ServiceError) a ServiceError
_LastVPCAssociation =
    _ServiceError . hasStatus 400 . hasCode "LastVPCAssociation"

-- | Prism for TooManyHealthChecks' errors.
_TooManyHealthChecks :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyHealthChecks = _ServiceError . hasCode "TooManyHealthChecks"

-- | No health check exists with the ID that you specified in the 'DeleteHealthCheck' request.
_NoSuchHealthCheck :: AsError a => Getting (First ServiceError) a ServiceError
_NoSuchHealthCheck =
    _ServiceError . hasStatus 404 . hasCode "NoSuchHealthCheck"

-- | One or more traffic policy instances were created by using the specified traffic policy.
_TrafficPolicyInUse :: AsError a => Getting (First ServiceError) a ServiceError
_TrafficPolicyInUse =
    _ServiceError . hasStatus 400 . hasCode "TrafficPolicyInUse"

-- | The hosted zone you are trying to create for your VPC_ID does not belong to you. Amazon Route 53 returns this error when the VPC specified by 'VPCId' does not belong to you.
_InvalidVPCId :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidVPCId = _ServiceError . hasStatus 400 . hasCode "InvalidVPCId"

-- | The hosted zone you are trying to create already exists. Amazon Route 53 returns this error when a hosted zone has already been created with the specified 'CallerReference'.
_HostedZoneAlreadyExists :: AsError a => Getting (First ServiceError) a ServiceError
_HostedZoneAlreadyExists =
    _ServiceError . hasStatus 409 . hasCode "HostedZoneAlreadyExists"
