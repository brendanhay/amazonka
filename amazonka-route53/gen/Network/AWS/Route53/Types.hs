{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53.Types
    (
    -- * Service Configuration
      route53

    -- * Errors
    , _HealthCheckVersionMismatch
    , _NoSuchQueryLoggingConfig
    , _HostedZoneNotPrivate
    , _InvalidInput
    , _HostedZoneNotEmpty
    , _InvalidArgument
    , _TrafficPolicyInstanceAlreadyExists
    , _ConflictingTypes
    , _QueryLoggingConfigAlreadyExists
    , _ConcurrentModification
    , _DelegationSetAlreadyReusable
    , _NotAuthorizedException
    , _InsufficientCloudWatchLogsResourcePolicy
    , _NoSuchCloudWatchLogsLogGroup
    , _PriorRequestNotComplete
    , _InvalidChangeBatch
    , _TooManyVPCAssociationAuthorizations
    , _TrafficPolicyAlreadyExists
    , _InvalidTrafficPolicyDocument
    , _InvalidPaginationToken
    , _DelegationSetNotReusable
    , _InvalidDomainName
    , _NoSuchTrafficPolicy
    , _HostedZoneNotFound
    , _DelegationSetInUse
    , _NoSuchDelegationSet
    , _HealthCheckAlreadyExists
    , _TooManyTrafficPolicies
    , _VPCAssociationAuthorizationNotFound
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
    , _TooManyTrafficPolicyVersionsForCurrentPolicy

    -- * Re-exported Types
    , module Network.AWS.Route53.Internal

    -- * AccountLimitType
    , AccountLimitType (..)

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

    -- * HostedZoneLimitType
    , HostedZoneLimitType (..)

    -- * InsufficientDataHealthStatus
    , InsufficientDataHealthStatus (..)

    -- * RecordType
    , RecordType (..)

    -- * ResettableElementName
    , ResettableElementName (..)

    -- * ReusableDelegationSetLimitType
    , ReusableDelegationSetLimitType (..)

    -- * Statistic
    , Statistic (..)

    -- * TagResourceType
    , TagResourceType (..)

    -- * VPCRegion
    , VPCRegion (..)

    -- * AccountLimit
    , AccountLimit
    , accountLimit
    , alType
    , alValue

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
    , hcLinkedService
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
    , hzLinkedService
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

    -- * HostedZoneLimit
    , HostedZoneLimit
    , hostedZoneLimit
    , hzlType
    , hzlValue

    -- * LinkedService
    , LinkedService
    , linkedService
    , lsServicePrincipal
    , lsDescription

    -- * QueryLoggingConfig
    , QueryLoggingConfig
    , queryLoggingConfig
    , qlcId
    , qlcHostedZoneId
    , qlcCloudWatchLogsLogGroupARN

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
    , rrsMultiValueAnswer
    , rrsName
    , rrsType

    -- * ResourceTagSet
    , ResourceTagSet
    , resourceTagSet
    , rtsResourceId
    , rtsResourceType
    , rtsTags

    -- * ReusableDelegationSetLimit
    , ReusableDelegationSetLimit
    , reusableDelegationSetLimit
    , rdslType
    , rdslValue

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

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Route53.Internal
import Network.AWS.Route53.Types.Product
import Network.AWS.Route53.Types.Sum
import Network.AWS.Sign.V4

-- | API version @2013-04-01@ of the Amazon Route 53 SDK configuration.
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
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasCode "Throttling" . hasStatus 400) e =
        Just "request_limit_exceeded"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "PriorRequestNotComplete" . hasStatus 400) e =
        Just "still_processing"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing


-- | The value of @HealthCheckVersion@ in the request doesn't match the value of @HealthCheckVersion@ in the health check.
--
--
_HealthCheckVersionMismatch :: AsError a => Getting (First ServiceError) a ServiceError
_HealthCheckVersionMismatch =
  _MatchServiceError route53 "HealthCheckVersionMismatch" . hasStatus 409


-- | There is no DNS query logging configuration with the specified ID.
--
--
_NoSuchQueryLoggingConfig :: AsError a => Getting (First ServiceError) a ServiceError
_NoSuchQueryLoggingConfig =
  _MatchServiceError route53 "NoSuchQueryLoggingConfig" . hasStatus 404


-- | The specified hosted zone is a public hosted zone, not a private hosted zone.
--
--
_HostedZoneNotPrivate :: AsError a => Getting (First ServiceError) a ServiceError
_HostedZoneNotPrivate = _MatchServiceError route53 "HostedZoneNotPrivate"


-- | The input is not valid.
--
--
_InvalidInput :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidInput = _MatchServiceError route53 "InvalidInput" . hasStatus 400


-- | The hosted zone contains resource records that are not SOA or NS records.
--
--
_HostedZoneNotEmpty :: AsError a => Getting (First ServiceError) a ServiceError
_HostedZoneNotEmpty =
  _MatchServiceError route53 "HostedZoneNotEmpty" . hasStatus 400


-- | Parameter name is invalid.
--
--
_InvalidArgument :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidArgument = _MatchServiceError route53 "InvalidArgument"


-- | There is already a traffic policy instance with the specified ID.
--
--
_TrafficPolicyInstanceAlreadyExists :: AsError a => Getting (First ServiceError) a ServiceError
_TrafficPolicyInstanceAlreadyExists =
  _MatchServiceError route53 "TrafficPolicyInstanceAlreadyExists" .
  hasStatus 409


-- | You tried to update a traffic policy instance by using a traffic policy version that has a different DNS type than the current type for the instance. You specified the type in the JSON document in the @CreateTrafficPolicy@ or @CreateTrafficPolicyVersion@ request.
--
--
_ConflictingTypes :: AsError a => Getting (First ServiceError) a ServiceError
_ConflictingTypes =
  _MatchServiceError route53 "ConflictingTypes" . hasStatus 400


-- | You can create only one query logging configuration for a hosted zone, and a query logging configuration already exists for this hosted zone.
--
--
_QueryLoggingConfigAlreadyExists :: AsError a => Getting (First ServiceError) a ServiceError
_QueryLoggingConfigAlreadyExists =
  _MatchServiceError route53 "QueryLoggingConfigAlreadyExists" . hasStatus 409


-- | Another user submitted a request to create, update, or delete the object at the same time that you did. Retry the request.
--
--
_ConcurrentModification :: AsError a => Getting (First ServiceError) a ServiceError
_ConcurrentModification =
  _MatchServiceError route53 "ConcurrentModification" . hasStatus 400


-- | The specified delegation set has already been marked as reusable.
--
--
_DelegationSetAlreadyReusable :: AsError a => Getting (First ServiceError) a ServiceError
_DelegationSetAlreadyReusable =
  _MatchServiceError route53 "DelegationSetAlreadyReusable"


-- | Associating the specified VPC with the specified hosted zone has not been authorized.
--
--
_NotAuthorizedException :: AsError a => Getting (First ServiceError) a ServiceError
_NotAuthorizedException =
  _MatchServiceError route53 "NotAuthorizedException" . hasStatus 401


-- | Amazon Route 53 doesn't have the permissions required to create log streams and send query logs to log streams. Possible causes include the following:
--
--
--     * There is no resource policy that specifies the log group ARN in the value for @Resource@ .
--
--     * The resource policy that includes the log group ARN in the value for @Resource@ doesn't have the necessary permissions.
--
--     * The resource policy hasn't finished propagating yet.
--
--
--
_InsufficientCloudWatchLogsResourcePolicy :: AsError a => Getting (First ServiceError) a ServiceError
_InsufficientCloudWatchLogsResourcePolicy =
  _MatchServiceError route53 "InsufficientCloudWatchLogsResourcePolicy" .
  hasStatus 400


-- | There is no CloudWatch Logs log group with the specified ARN.
--
--
_NoSuchCloudWatchLogsLogGroup :: AsError a => Getting (First ServiceError) a ServiceError
_NoSuchCloudWatchLogsLogGroup =
  _MatchServiceError route53 "NoSuchCloudWatchLogsLogGroup" . hasStatus 404


-- | If Amazon Route 53 can't process a request before the next request arrives, it will reject subsequent requests for the same hosted zone and return an @HTTP 400 error@ (@Bad request@ ). If Amazon Route 53 returns this error repeatedly for the same request, we recommend that you wait, in intervals of increasing duration, before you try the request again.
--
--
_PriorRequestNotComplete :: AsError a => Getting (First ServiceError) a ServiceError
_PriorRequestNotComplete =
  _MatchServiceError route53 "PriorRequestNotComplete" . hasStatus 400


-- | This exception contains a list of messages that might contain one or more error messages. Each error message indicates one error in the change batch.
--
--
_InvalidChangeBatch :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidChangeBatch = _MatchServiceError route53 "InvalidChangeBatch"


-- | You've created the maximum number of authorizations that can be created for the specified hosted zone. To authorize another VPC to be associated with the hosted zone, submit a @DeleteVPCAssociationAuthorization@ request to remove an existing authorization. To get a list of existing authorizations, submit a @ListVPCAssociationAuthorizations@ request.
--
--
_TooManyVPCAssociationAuthorizations :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyVPCAssociationAuthorizations =
  _MatchServiceError route53 "TooManyVPCAssociationAuthorizations" .
  hasStatus 400


-- | A traffic policy that has the same value for @Name@ already exists.
--
--
_TrafficPolicyAlreadyExists :: AsError a => Getting (First ServiceError) a ServiceError
_TrafficPolicyAlreadyExists =
  _MatchServiceError route53 "TrafficPolicyAlreadyExists" . hasStatus 409


-- | The format of the traffic policy document that you specified in the @Document@ element is invalid.
--
--
_InvalidTrafficPolicyDocument :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidTrafficPolicyDocument =
  _MatchServiceError route53 "InvalidTrafficPolicyDocument" . hasStatus 400


-- | The value that you specified to get the second or subsequent page of results is invalid.
--
--
_InvalidPaginationToken :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidPaginationToken =
  _MatchServiceError route53 "InvalidPaginationToken" . hasStatus 400


-- | A reusable delegation set with the specified ID does not exist.
--
--
_DelegationSetNotReusable :: AsError a => Getting (First ServiceError) a ServiceError
_DelegationSetNotReusable =
  _MatchServiceError route53 "DelegationSetNotReusable"


-- | The specified domain name is not valid.
--
--
_InvalidDomainName :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDomainName =
  _MatchServiceError route53 "InvalidDomainName" . hasStatus 400


-- | No traffic policy exists with the specified ID.
--
--
_NoSuchTrafficPolicy :: AsError a => Getting (First ServiceError) a ServiceError
_NoSuchTrafficPolicy =
  _MatchServiceError route53 "NoSuchTrafficPolicy" . hasStatus 404


-- | The specified HostedZone can't be found.
--
--
_HostedZoneNotFound :: AsError a => Getting (First ServiceError) a ServiceError
_HostedZoneNotFound = _MatchServiceError route53 "HostedZoneNotFound"


-- | The specified delegation contains associated hosted zones which must be deleted before the reusable delegation set can be deleted.
--
--
_DelegationSetInUse :: AsError a => Getting (First ServiceError) a ServiceError
_DelegationSetInUse = _MatchServiceError route53 "DelegationSetInUse"


-- | A reusable delegation set with the specified ID does not exist.
--
--
_NoSuchDelegationSet :: AsError a => Getting (First ServiceError) a ServiceError
_NoSuchDelegationSet = _MatchServiceError route53 "NoSuchDelegationSet"


-- | The health check you're attempting to create already exists. Amazon Route 53 returns this error when you submit a request that has the following values:
--
--
--     * The same value for @CallerReference@ as an existing health check, and one or more values that differ from the existing health check that has the same caller reference.
--
--     * The same value for @CallerReference@ as a health check that you created and later deleted, regardless of the other settings in the request.
--
--
--
_HealthCheckAlreadyExists :: AsError a => Getting (First ServiceError) a ServiceError
_HealthCheckAlreadyExists =
  _MatchServiceError route53 "HealthCheckAlreadyExists" . hasStatus 409


-- | This traffic policy can't be created because the current account has reached the limit on the number of traffic policies.
--
--
-- For information about default limits, see <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DNSLimitations.html Limits> in the /Amazon Route 53 Developer Guide/ .
--
-- To get the current limit for an account, see 'GetAccountLimit' .
--
-- To request a higher limit, <http://aws.amazon.com/route53-request create a case> with the AWS Support Center.
--
_TooManyTrafficPolicies :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyTrafficPolicies =
  _MatchServiceError route53 "TooManyTrafficPolicies" . hasStatus 400


-- | The VPC that you specified is not authorized to be associated with the hosted zone.
--
--
_VPCAssociationAuthorizationNotFound :: AsError a => Getting (First ServiceError) a ServiceError
_VPCAssociationAuthorizationNotFound =
  _MatchServiceError route53 "VPCAssociationAuthorizationNotFound" .
  hasStatus 404


-- | Amazon Route 53 doesn't support the specified geolocation.
--
--
_NoSuchGeoLocation :: AsError a => Getting (First ServiceError) a ServiceError
_NoSuchGeoLocation =
  _MatchServiceError route53 "NoSuchGeoLocation" . hasStatus 404


-- | You can create a hosted zone that has the same name as an existing hosted zone (example.com is common), but there is a limit to the number of hosted zones that have the same name. If you get this error, Amazon Route 53 has reached that limit. If you own the domain name and Amazon Route 53 generates this error, contact Customer Support.
--
--
_DelegationSetNotAvailable :: AsError a => Getting (First ServiceError) a ServiceError
_DelegationSetNotAvailable =
  _MatchServiceError route53 "DelegationSetNotAvailable"


-- | The specified VPC and hosted zone are not currently associated.
--
--
_VPCAssociationNotFound :: AsError a => Getting (First ServiceError) a ServiceError
_VPCAssociationNotFound =
  _MatchServiceError route53 "VPCAssociationNotFound" . hasStatus 404


-- | The limit on the number of requests per second was exceeded.
--
--
_ThrottlingException :: AsError a => Getting (First ServiceError) a ServiceError
_ThrottlingException =
  _MatchServiceError route53 "ThrottlingException" . hasStatus 400


-- | A change with the specified change ID does not exist.
--
--
_NoSuchChange :: AsError a => Getting (First ServiceError) a ServiceError
_NoSuchChange = _MatchServiceError route53 "NoSuchChange" . hasStatus 404


-- | This operation can't be completed either because the current account has reached the limit on reusable delegation sets that it can create or because you've reached the limit on the number of Amazon VPCs that you can associate with a private hosted zone. To get the current limit on the number of reusable delegation sets, see 'GetAccountLimit' . To get the current limit on the number of Amazon VPCs that you can associate with a private hosted zone, see 'GetHostedZoneLimit' . To request a higher limit, <http://aws.amazon.com/route53-request create a case> with the AWS Support Center.
--
--
_LimitsExceeded :: AsError a => Getting (First ServiceError) a ServiceError
_LimitsExceeded = _MatchServiceError route53 "LimitsExceeded"


-- | This traffic policy instance can't be created because the current account has reached the limit on the number of traffic policy instances.
--
--
-- For information about default limits, see <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DNSLimitations.html Limits> in the /Amazon Route 53 Developer Guide/ .
--
-- For information about how to get the current limit for an account, see 'GetAccountLimit' .
--
-- To request a higher limit, <http://aws.amazon.com/route53-request create a case> with the AWS Support Center.
--
_TooManyTrafficPolicyInstances :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyTrafficPolicyInstances =
  _MatchServiceError route53 "TooManyTrafficPolicyInstances" . hasStatus 400


-- | No traffic policy instance exists with the specified ID.
--
--
_NoSuchTrafficPolicyInstance :: AsError a => Getting (First ServiceError) a ServiceError
_NoSuchTrafficPolicyInstance =
  _MatchServiceError route53 "NoSuchTrafficPolicyInstance" . hasStatus 404


-- | The resource you're trying to access is unsupported on this Amazon Route 53 endpoint.
--
--
_IncompatibleVersion :: AsError a => Getting (First ServiceError) a ServiceError
_IncompatibleVersion =
  _MatchServiceError route53 "IncompatibleVersion" . hasStatus 400


-- | You're trying to associate a VPC with a public hosted zone. Amazon Route 53 doesn't support associating a VPC with a public hosted zone.
--
--
_PublicZoneVPCAssociation :: AsError a => Getting (First ServiceError) a ServiceError
_PublicZoneVPCAssociation =
  _MatchServiceError route53 "PublicZoneVPCAssociation" . hasStatus 400


-- | No hosted zone exists with the ID that you specified.
--
--
_NoSuchHostedZone :: AsError a => Getting (First ServiceError) a ServiceError
_NoSuchHostedZone =
  _MatchServiceError route53 "NoSuchHostedZone" . hasStatus 404


-- | This operation can't be completed either because the current account has reached the limit on the number of hosted zones or because you've reached the limit on the number of hosted zones that can be associated with a reusable delegation set.
--
--
-- For information about default limits, see <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DNSLimitations.html Limits> in the /Amazon Route 53 Developer Guide/ .
--
-- To get the current limit on hosted zones that can be created by an account, see 'GetAccountLimit' .
--
-- To get the current limit on hosted zones that can be associated with a reusable delegation set, see 'GetReusableDelegationSetLimit' .
--
-- To request a higher limit, <http://aws.amazon.com/route53-request create a case> with the AWS Support Center.
--
_TooManyHostedZones :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyHostedZones =
  _MatchServiceError route53 "TooManyHostedZones" . hasStatus 400


-- | This error code is not in use.
--
--
_HealthCheckInUse :: AsError a => Getting (First ServiceError) a ServiceError
_HealthCheckInUse =
  _MatchServiceError route53 "HealthCheckInUse" . hasStatus 400


-- | A delegation set with the same owner and caller reference combination has already been created.
--
--
_DelegationSetAlreadyCreated :: AsError a => Getting (First ServiceError) a ServiceError
_DelegationSetAlreadyCreated =
  _MatchServiceError route53 "DelegationSetAlreadyCreated"


-- | The cause of this error depends on whether you're trying to create a public or a private hosted zone:
--
--
--     * __Public hosted zone:__ Two hosted zones that have the same name or that have a parent/child relationship (example.com and test.example.com) can't have any common name servers. You tried to create a hosted zone that has the same name as an existing hosted zone or that's the parent or child of an existing hosted zone, and you specified a delegation set that shares one or more name servers with the existing hosted zone. For more information, see 'CreateReusableDelegationSet' .
--
--     * __Private hosted zone:__ You specified an Amazon VPC that you're already using for another hosted zone, and the domain that you specified for one of the hosted zones is a subdomain of the domain that you specified for the other hosted zone. For example, you can't use the same Amazon VPC for the hosted zones for example.com and test.example.com.
--
--
--
_ConflictingDomainExists :: AsError a => Getting (First ServiceError) a ServiceError
_ConflictingDomainExists = _MatchServiceError route53 "ConflictingDomainExists"


-- | The VPC that you're trying to disassociate from the private hosted zone is the last VPC that is associated with the hosted zone. Amazon Route 53 doesn't support disassociating the last VPC from a hosted zone.
--
--
_LastVPCAssociation :: AsError a => Getting (First ServiceError) a ServiceError
_LastVPCAssociation =
  _MatchServiceError route53 "LastVPCAssociation" . hasStatus 400


-- | This health check can't be created because the current account has reached the limit on the number of active health checks.
--
--
-- For information about default limits, see <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DNSLimitations.html Limits> in the /Amazon Route 53 Developer Guide/ .
--
-- For information about how to get the current limit for an account, see 'GetAccountLimit' . To request a higher limit, <http://aws.amazon.com/route53-request create a case> with the AWS Support Center.
--
-- You have reached the maximum number of active health checks for an AWS account. To request a higher limit, <http://aws.amazon.com/route53-request create a case> with the AWS Support Center.
--
_TooManyHealthChecks :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyHealthChecks = _MatchServiceError route53 "TooManyHealthChecks"


-- | No health check exists with the ID that you specified in the @DeleteHealthCheck@ request.
--
--
_NoSuchHealthCheck :: AsError a => Getting (First ServiceError) a ServiceError
_NoSuchHealthCheck =
  _MatchServiceError route53 "NoSuchHealthCheck" . hasStatus 404


-- | One or more traffic policy instances were created by using the specified traffic policy.
--
--
_TrafficPolicyInUse :: AsError a => Getting (First ServiceError) a ServiceError
_TrafficPolicyInUse =
  _MatchServiceError route53 "TrafficPolicyInUse" . hasStatus 400


-- | The VPC ID that you specified either isn't a valid ID or the current account is not authorized to access this VPC.
--
--
_InvalidVPCId :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidVPCId = _MatchServiceError route53 "InvalidVPCId" . hasStatus 400


-- | The hosted zone you're trying to create already exists. Amazon Route 53 returns this error when a hosted zone has already been created with the specified @CallerReference@ .
--
--
_HostedZoneAlreadyExists :: AsError a => Getting (First ServiceError) a ServiceError
_HostedZoneAlreadyExists =
  _MatchServiceError route53 "HostedZoneAlreadyExists" . hasStatus 409


-- | This traffic policy version can't be created because you've reached the limit of 1000 on the number of versions that you can create for the current traffic policy.
--
--
-- To create more traffic policy versions, you can use 'GetTrafficPolicy' to get the traffic policy document for a specified traffic policy version, and then use 'CreateTrafficPolicy' to create a new traffic policy using the traffic policy document.
--
_TooManyTrafficPolicyVersionsForCurrentPolicy :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyTrafficPolicyVersionsForCurrentPolicy =
  _MatchServiceError route53 "TooManyTrafficPolicyVersionsForCurrentPolicy" .
  hasStatus 400

