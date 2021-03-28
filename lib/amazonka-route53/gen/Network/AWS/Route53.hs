{-# OPTIONS_GHC -fno-warn-unused-imports    #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon Route 53 is a scalable Domain Name System (DNS) web service. It provides secure and reliable routing to your infrastructure that uses Amazon Web Services (AWS) products, such as Amazon Elastic Compute Cloud (Amazon EC2), Elastic Load Balancing, or Amazon Simple Storage Service (Amazon S3). You can also use Amazon Route 53 to route users to your infrastructure outside of AWS.
module Network.AWS.Route53
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    -- $errors

    -- ** HealthCheckVersionMismatch
    , _HealthCheckVersionMismatch

    -- ** NoSuchQueryLoggingConfig
    , _NoSuchQueryLoggingConfig

    -- ** HostedZoneNotPrivate
    , _HostedZoneNotPrivate

    -- ** InvalidInput
    , _InvalidInput

    -- ** HostedZoneNotEmpty
    , _HostedZoneNotEmpty

    -- ** InvalidArgument
    , _InvalidArgument

    -- ** TrafficPolicyInstanceAlreadyExists
    , _TrafficPolicyInstanceAlreadyExists

    -- ** ConflictingTypes
    , _ConflictingTypes

    -- ** QueryLoggingConfigAlreadyExists
    , _QueryLoggingConfigAlreadyExists

    -- ** ConcurrentModification
    , _ConcurrentModification

    -- ** DelegationSetAlreadyReusable
    , _DelegationSetAlreadyReusable

    -- ** NotAuthorizedException
    , _NotAuthorizedException

    -- ** InsufficientCloudWatchLogsResourcePolicy
    , _InsufficientCloudWatchLogsResourcePolicy

    -- ** NoSuchCloudWatchLogsLogGroup
    , _NoSuchCloudWatchLogsLogGroup

    -- ** PriorRequestNotComplete
    , _PriorRequestNotComplete

    -- ** InvalidChangeBatch
    , _InvalidChangeBatch

    -- ** TooManyVPCAssociationAuthorizations
    , _TooManyVPCAssociationAuthorizations

    -- ** TrafficPolicyAlreadyExists
    , _TrafficPolicyAlreadyExists

    -- ** InvalidTrafficPolicyDocument
    , _InvalidTrafficPolicyDocument

    -- ** InvalidPaginationToken
    , _InvalidPaginationToken

    -- ** DelegationSetNotReusable
    , _DelegationSetNotReusable

    -- ** InvalidDomainName
    , _InvalidDomainName

    -- ** NoSuchTrafficPolicy
    , _NoSuchTrafficPolicy

    -- ** HostedZoneNotFound
    , _HostedZoneNotFound

    -- ** DelegationSetInUse
    , _DelegationSetInUse

    -- ** NoSuchDelegationSet
    , _NoSuchDelegationSet

    -- ** HealthCheckAlreadyExists
    , _HealthCheckAlreadyExists

    -- ** TooManyTrafficPolicies
    , _TooManyTrafficPolicies

    -- ** VPCAssociationAuthorizationNotFound
    , _VPCAssociationAuthorizationNotFound

    -- ** NoSuchGeoLocation
    , _NoSuchGeoLocation

    -- ** DelegationSetNotAvailable
    , _DelegationSetNotAvailable

    -- ** VPCAssociationNotFound
    , _VPCAssociationNotFound

    -- ** ThrottlingException
    , _ThrottlingException

    -- ** NoSuchChange
    , _NoSuchChange

    -- ** LimitsExceeded
    , _LimitsExceeded

    -- ** TooManyTrafficPolicyInstances
    , _TooManyTrafficPolicyInstances

    -- ** NoSuchTrafficPolicyInstance
    , _NoSuchTrafficPolicyInstance

    -- ** IncompatibleVersion
    , _IncompatibleVersion

    -- ** PublicZoneVPCAssociation
    , _PublicZoneVPCAssociation

    -- ** NoSuchHostedZone
    , _NoSuchHostedZone

    -- ** TooManyHostedZones
    , _TooManyHostedZones

    -- ** HealthCheckInUse
    , _HealthCheckInUse

    -- ** DelegationSetAlreadyCreated
    , _DelegationSetAlreadyCreated

    -- ** ConflictingDomainExists
    , _ConflictingDomainExists

    -- ** LastVPCAssociation
    , _LastVPCAssociation

    -- ** TooManyHealthChecks
    , _TooManyHealthChecks

    -- ** NoSuchHealthCheck
    , _NoSuchHealthCheck

    -- ** TrafficPolicyInUse
    , _TrafficPolicyInUse

    -- ** InvalidVPCId
    , _InvalidVPCId

    -- ** HostedZoneAlreadyExists
    , _HostedZoneAlreadyExists

    -- ** TooManyTrafficPolicyVersionsForCurrentPolicy
    , _TooManyTrafficPolicyVersionsForCurrentPolicy

    -- * Waiters
    -- $waiters

    -- ** ResourceRecordSetsChanged
    , mkResourceRecordSetsChanged

    -- * Operations
    -- $operations

    -- ** GetHostedZoneLimit 
    , module Network.AWS.Route53.GetHostedZoneLimit

    -- ** AssociateVPCWithHostedZone 
    , module Network.AWS.Route53.AssociateVPCWithHostedZone

    -- ** DeleteTrafficPolicy 
    , module Network.AWS.Route53.DeleteTrafficPolicy

    -- ** GetCheckerIpRanges 
    , module Network.AWS.Route53.GetCheckerIpRanges

    -- ** GetTrafficPolicyInstance 
    , module Network.AWS.Route53.GetTrafficPolicyInstance

    -- ** GetHealthCheckLastFailureReason 
    , module Network.AWS.Route53.GetHealthCheckLastFailureReason

    -- ** DeleteReusableDelegationSet 
    , module Network.AWS.Route53.DeleteReusableDelegationSet

    -- ** ListHostedZonesByName 
    , module Network.AWS.Route53.ListHostedZonesByName

    -- ** ListReusableDelegationSets 
    , module Network.AWS.Route53.ListReusableDelegationSets

    -- ** ListTagsForResource 
    , module Network.AWS.Route53.ListTagsForResource

    -- ** ListQueryLoggingConfigs (Paginated)
    , module Network.AWS.Route53.ListQueryLoggingConfigs

    -- ** ListTrafficPolicyInstances 
    , module Network.AWS.Route53.ListTrafficPolicyInstances

    -- ** CreateTrafficPolicyInstance 
    , module Network.AWS.Route53.CreateTrafficPolicyInstance

    -- ** GetChange 
    , module Network.AWS.Route53.GetChange

    -- ** ChangeResourceRecordSets 
    , module Network.AWS.Route53.ChangeResourceRecordSets

    -- ** DeleteHealthCheck 
    , module Network.AWS.Route53.DeleteHealthCheck

    -- ** UpdateHealthCheck 
    , module Network.AWS.Route53.UpdateHealthCheck

    -- ** CreateHostedZone 
    , module Network.AWS.Route53.CreateHostedZone

    -- ** CreateVPCAssociationAuthorization 
    , module Network.AWS.Route53.CreateVPCAssociationAuthorization

    -- ** ListVPCAssociationAuthorizations (Paginated)
    , module Network.AWS.Route53.ListVPCAssociationAuthorizations

    -- ** ListTrafficPolicyInstancesByPolicy 
    , module Network.AWS.Route53.ListTrafficPolicyInstancesByPolicy

    -- ** DisassociateVPCFromHostedZone 
    , module Network.AWS.Route53.DisassociateVPCFromHostedZone

    -- ** CreateHealthCheck 
    , module Network.AWS.Route53.CreateHealthCheck

    -- ** DeleteVPCAssociationAuthorization 
    , module Network.AWS.Route53.DeleteVPCAssociationAuthorization

    -- ** ChangeTagsForResource 
    , module Network.AWS.Route53.ChangeTagsForResource

    -- ** ListHostedZones (Paginated)
    , module Network.AWS.Route53.ListHostedZones

    -- ** GetTrafficPolicyInstanceCount 
    , module Network.AWS.Route53.GetTrafficPolicyInstanceCount

    -- ** ListGeoLocations 
    , module Network.AWS.Route53.ListGeoLocations

    -- ** GetHostedZone 
    , module Network.AWS.Route53.GetHostedZone

    -- ** GetHealthCheck 
    , module Network.AWS.Route53.GetHealthCheck

    -- ** ListResourceRecordSets (Paginated)
    , module Network.AWS.Route53.ListResourceRecordSets

    -- ** CreateReusableDelegationSet 
    , module Network.AWS.Route53.CreateReusableDelegationSet

    -- ** CreateQueryLoggingConfig 
    , module Network.AWS.Route53.CreateQueryLoggingConfig

    -- ** GetHealthCheckCount 
    , module Network.AWS.Route53.GetHealthCheckCount

    -- ** UpdateTrafficPolicyComment 
    , module Network.AWS.Route53.UpdateTrafficPolicyComment

    -- ** GetHostedZoneCount 
    , module Network.AWS.Route53.GetHostedZoneCount

    -- ** GetAccountLimit 
    , module Network.AWS.Route53.GetAccountLimit

    -- ** DeleteQueryLoggingConfig 
    , module Network.AWS.Route53.DeleteQueryLoggingConfig

    -- ** GetQueryLoggingConfig 
    , module Network.AWS.Route53.GetQueryLoggingConfig

    -- ** GetReusableDelegationSet 
    , module Network.AWS.Route53.GetReusableDelegationSet

    -- ** DeleteTrafficPolicyInstance 
    , module Network.AWS.Route53.DeleteTrafficPolicyInstance

    -- ** UpdateTrafficPolicyInstance 
    , module Network.AWS.Route53.UpdateTrafficPolicyInstance

    -- ** UpdateHostedZoneComment 
    , module Network.AWS.Route53.UpdateHostedZoneComment

    -- ** GetHealthCheckStatus 
    , module Network.AWS.Route53.GetHealthCheckStatus

    -- ** ListHostedZonesByVPC 
    , module Network.AWS.Route53.ListHostedZonesByVPC

    -- ** GetReusableDelegationSetLimit 
    , module Network.AWS.Route53.GetReusableDelegationSetLimit

    -- ** CreateTrafficPolicyVersion 
    , module Network.AWS.Route53.CreateTrafficPolicyVersion

    -- ** TestDNSAnswer 
    , module Network.AWS.Route53.TestDNSAnswer

    -- ** ListHealthChecks (Paginated)
    , module Network.AWS.Route53.ListHealthChecks

    -- ** GetTrafficPolicy 
    , module Network.AWS.Route53.GetTrafficPolicy

    -- ** ListTrafficPolicyVersions 
    , module Network.AWS.Route53.ListTrafficPolicyVersions

    -- ** DeleteHostedZone 
    , module Network.AWS.Route53.DeleteHostedZone

    -- ** GetGeoLocation 
    , module Network.AWS.Route53.GetGeoLocation

    -- ** ListTagsForResources 
    , module Network.AWS.Route53.ListTagsForResources

    -- ** CreateTrafficPolicy 
    , module Network.AWS.Route53.CreateTrafficPolicy

    -- ** ListTrafficPolicyInstancesByHostedZone 
    , module Network.AWS.Route53.ListTrafficPolicyInstancesByHostedZone

    -- ** ListTrafficPolicies 
    , module Network.AWS.Route53.ListTrafficPolicies

    -- * Types

    -- ** Common
    , module Network.AWS.Route53.Internal

    -- ** AlarmName
    , AlarmName (..)

    -- ** AccountLimit
    , AccountLimit (..)
    , mkAccountLimit
    , alType
    , alValue

    -- ** ResourceURI
    , ResourceURI (..)

    -- ** Status
    , Status (..)

    -- ** LinkedService
    , LinkedService (..)
    , mkLinkedService
    , lsDescription
    , lsServicePrincipal

    -- ** PaginationToken
    , PaginationToken (..)

    -- ** ServicePrincipal
    , ServicePrincipal (..)

    -- ** AliasTarget
    , AliasTarget (..)
    , mkAliasTarget
    , atHostedZoneId
    , atDNSName
    , atEvaluateTargetHealth

    -- ** IPAddress
    , IPAddress (..)

    -- ** GeoLocationCountryName
    , GeoLocationCountryName (..)

    -- ** ResourceRecord
    , ResourceRecord (..)
    , mkResourceRecord
    , rrValue

    -- ** Tag
    , Tag (..)
    , mkTag
    , tKey
    , tValue

    -- ** GeoLocationDetails
    , GeoLocationDetails (..)
    , mkGeoLocationDetails
    , gldContinentCode
    , gldContinentName
    , gldCountryCode
    , gldCountryName
    , gldSubdivisionCode
    , gldSubdivisionName

    -- ** ResettableElementName
    , ResettableElementName (..)

    -- ** DimensionField
    , DimensionField (..)

    -- ** HostedZoneLimitType
    , HostedZoneLimitType (..)

    -- ** HealthCheck
    , HealthCheck (..)
    , mkHealthCheck
    , hcId
    , hcCallerReference
    , hcHealthCheckConfig
    , hcHealthCheckVersion
    , hcCloudWatchAlarmConfiguration
    , hcLinkedService

    -- ** Nameserver
    , Nameserver (..)

    -- ** TagResourceId
    , TagResourceId (..)

    -- ** TrafficPolicy
    , TrafficPolicy (..)
    , mkTrafficPolicy
    , tpId
    , tpVersion
    , tpName
    , tpType
    , tpDocument
    , tpComment

    -- ** VPCRegion
    , VPCRegion (..)

    -- ** TransportProtocol
    , TransportProtocol (..)

    -- ** GeoLocationCountryCode
    , GeoLocationCountryCode (..)

    -- ** ChangeAction
    , ChangeAction (..)

    -- ** TagResourceType
    , TagResourceType (..)

    -- ** HealthCheckConfig
    , HealthCheckConfig (..)
    , mkHealthCheckConfig
    , hccType
    , hccAlarmIdentifier
    , hccChildHealthChecks
    , hccDisabled
    , hccEnableSNI
    , hccFailureThreshold
    , hccFullyQualifiedDomainName
    , hccHealthThreshold
    , hccIPAddress
    , hccInsufficientDataHealthStatus
    , hccInverted
    , hccMeasureLatency
    , hccPort
    , hccRegions
    , hccRequestInterval
    , hccResourcePath
    , hccSearchString

    -- ** CloudWatchRegion
    , CloudWatchRegion (..)

    -- ** Dimension
    , Dimension (..)
    , mkDimension
    , dName
    , dValue

    -- ** VPCId
    , VPCId (..)

    -- ** MetricName
    , MetricName (..)

    -- ** Namespace
    , Namespace (..)

    -- ** SearchString
    , SearchString (..)

    -- ** CloudWatchAlarmConfiguration
    , CloudWatchAlarmConfiguration (..)
    , mkCloudWatchAlarmConfiguration
    , cwacEvaluationPeriods
    , cwacThreshold
    , cwacComparisonOperator
    , cwacPeriod
    , cwacMetricName
    , cwacNamespace
    , cwacStatistic
    , cwacDimensions

    -- ** HostedZoneSummary
    , HostedZoneSummary (..)
    , mkHostedZoneSummary
    , hzsHostedZoneId
    , hzsName
    , hzsOwner

    -- ** QueryLoggingConfig
    , QueryLoggingConfig (..)
    , mkQueryLoggingConfig
    , qlcId
    , qlcHostedZoneId
    , qlcCloudWatchLogsLogGroupArn

    -- ** GeoLocationContinentCode
    , GeoLocationContinentCode (..)

    -- ** ResourceRecordSetIdentifier
    , ResourceRecordSetIdentifier (..)

    -- ** QueryLoggingConfigId
    , QueryLoggingConfigId (..)

    -- ** ReusableDelegationSetLimitType
    , ReusableDelegationSetLimitType (..)

    -- ** Change
    , Change (..)
    , mkChange
    , cAction
    , cResourceRecordSet

    -- ** CloudWatchLogsLogGroupArn
    , CloudWatchLogsLogGroupArn (..)

    -- ** ComparisonOperator
    , ComparisonOperator (..)

    -- ** TrafficPolicyInstanceId
    , TrafficPolicyInstanceId (..)

    -- ** Failover
    , Failover (..)

    -- ** AWSAccountID
    , AWSAccountID (..)

    -- ** AccountLimitType
    , AccountLimitType (..)

    -- ** TrafficPolicyInstance
    , TrafficPolicyInstance (..)
    , mkTrafficPolicyInstance
    , tpiId
    , tpiHostedZoneId
    , tpiName
    , tpiTTL
    , tpiState
    , tpiMessage
    , tpiTrafficPolicyId
    , tpiTrafficPolicyVersion
    , tpiTrafficPolicyType

    -- ** ResourceDescription
    , ResourceDescription (..)

    -- ** HostedZoneOwningService
    , HostedZoneOwningService (..)

    -- ** SubnetMask
    , SubnetMask (..)

    -- ** HostedZone
    , HostedZone (..)
    , mkHostedZone
    , hzId
    , hzName
    , hzCallerReference
    , hzConfig
    , hzLinkedService
    , hzResourceRecordSetCount

    -- ** ResourcePath
    , ResourcePath (..)

    -- ** HealthCheckId
    , HealthCheckId (..)

    -- ** ResourceTagSet
    , ResourceTagSet (..)
    , mkResourceTagSet
    , rtsResourceId
    , rtsResourceType
    , rtsTags

    -- ** ChangeStatus
    , ChangeStatus (..)

    -- ** HealthCheckRegion
    , HealthCheckRegion (..)

    -- ** TrafficPolicyDocument
    , TrafficPolicyDocument (..)

    -- ** AssociateVPCComment
    , AssociateVPCComment (..)

    -- ** ChangeBatch
    , ChangeBatch (..)
    , mkChangeBatch
    , cbChanges
    , cbComment

    -- ** HostedZoneLimit
    , HostedZoneLimit (..)
    , mkHostedZoneLimit
    , hzlType
    , hzlValue

    -- ** TrafficPolicyName
    , TrafficPolicyName (..)

    -- ** StatusReport
    , StatusReport (..)
    , mkStatusReport
    , srCheckedTime
    , srStatus

    -- ** HealthCheckType
    , HealthCheckType (..)

    -- ** VPC
    , VPC (..)
    , mkVPC
    , vpcVPCId
    , vpcVPCRegion

    -- ** HostedZoneConfig
    , HostedZoneConfig (..)
    , mkHostedZoneConfig
    , hzcComment
    , hzcPrivateZone

    -- ** InsufficientDataHealthStatus
    , InsufficientDataHealthStatus (..)

    -- ** TrafficPolicyVersionMarker
    , TrafficPolicyVersionMarker (..)

    -- ** ResourceRecordSet
    , ResourceRecordSet (..)
    , mkResourceRecordSet
    , rrsName
    , rrsType
    , rrsAliasTarget
    , rrsFailover
    , rrsGeoLocation
    , rrsHealthCheckId
    , rrsMultiValueAnswer
    , rrsRegion
    , rrsResourceRecords
    , rrsSetIdentifier
    , rrsTTL
    , rrsTrafficPolicyInstanceId
    , rrsWeight

    -- ** TagKey
    , TagKey (..)

    -- ** DelegationSet
    , DelegationSet (..)
    , mkDelegationSet
    , dsNameServers
    , dsCallerReference
    , dsId

    -- ** ChangeInfo
    , ChangeInfo (..)
    , mkChangeInfo
    , ciId
    , ciStatus
    , ciSubmittedAt
    , ciComment

    -- ** GeoLocationSubdivisionCode
    , GeoLocationSubdivisionCode (..)

    -- ** RecordType
    , RecordType (..)

    -- ** AlarmIdentifier
    , AlarmIdentifier (..)
    , mkAlarmIdentifier
    , aiRegion
    , aiName

    -- ** GeoLocation
    , GeoLocation (..)
    , mkGeoLocation
    , glContinentCode
    , glCountryCode
    , glSubdivisionCode

    -- ** TrafficPolicyId
    , TrafficPolicyId (..)

    -- ** HealthCheckObservation
    , HealthCheckObservation (..)
    , mkHealthCheckObservation
    , hcoIPAddress
    , hcoRegion
    , hcoStatusReport

    -- ** Message
    , Message (..)

    -- ** FullyQualifiedDomainName
    , FullyQualifiedDomainName (..)

    -- ** IPAddressCidr
    , IPAddressCidr (..)

    -- ** ReusableDelegationSetLimit
    , ReusableDelegationSetLimit (..)
    , mkReusableDelegationSetLimit
    , rdslType
    , rdslValue

    -- ** DNSName
    , DNSName (..)

    -- ** HostedZoneOwner
    , HostedZoneOwner (..)
    , mkHostedZoneOwner
    , hzoOwningAccount
    , hzoOwningService

    -- ** MaxResults
    , MaxResults (..)

    -- ** Statistic
    , Statistic (..)

    -- ** RecordDataEntry
    , RecordDataEntry (..)

    -- ** TrafficPolicySummary
    , TrafficPolicySummary (..)
    , mkTrafficPolicySummary
    , tpsId
    , tpsName
    , tpsType
    , tpsLatestVersion
    , tpsTrafficPolicyCount

    -- ** HostedZoneIdMarker
    , HostedZoneIdMarker (..)

    -- ** MaxItems
    , MaxItems (..)

    -- ** TrafficPolicyInstanceNameMarker
    , TrafficPolicyInstanceNameMarker (..)

    -- ** Description
    , Description (..)

    -- ** DelegationSetId
    , DelegationSetId (..)

    -- ** Marker
    , Marker (..)

    -- ** CallerReference
    , CallerReference (..)

    -- ** HostedZoneId
    , HostedZoneId (..)

    -- ** Comment
    , Comment (..)

    -- ** Value
    , Value (..)

    -- ** Key
    , Key (..)

    -- ** NextContinentCode
    , NextContinentCode (..)

    -- ** NextCountryCode
    , NextCountryCode (..)

    -- ** NextSubdivisionCode
    , NextSubdivisionCode (..)

    -- ** ContinentCode
    , ContinentCode (..)

    -- ** ContinentName
    , ContinentName (..)

    -- ** CountryCode
    , CountryCode (..)

    -- ** SubdivisionCode
    , SubdivisionCode (..)

    -- ** SubdivisionName
    , SubdivisionName (..)

    -- ** Name
    , Name (..)

    -- ** Id
    , Id (..)

    -- ** Document
    , Document (..)

    -- ** StartRecordIdentifier
    , StartRecordIdentifier (..)

    -- ** StartRecordName
    , StartRecordName (..)

    -- ** StartContinentCode
    , StartContinentCode (..)

    -- ** StartSubdivisionCode
    , StartSubdivisionCode (..)

    -- ** NextMarker
    , NextMarker (..)

    -- ** NextDNSName
    , NextDNSName (..)

    -- ** State
    , State (..)

    -- ** NextRecordName
    , NextRecordName (..)

    -- ** RecordName
    , RecordName (..)

    -- ** TrafficPolicyIdMarker
    , TrafficPolicyIdMarker (..)

    -- ** ResponseCode
    , ResponseCode (..)

    -- * Serialization types
    , Lude.Base64 (..)
    , Lude._Base64
    , Lude.Sensitive (..)
    , Lude._Sensitive
    , Lude.UTCTime
    , Lude.NominalDiffTime
    ) where

import Network.AWS.Route53.Types
import Network.AWS.Route53.Waiters
import Network.AWS.Route53.GetHostedZoneLimit
import Network.AWS.Route53.AssociateVPCWithHostedZone
import Network.AWS.Route53.DeleteTrafficPolicy
import Network.AWS.Route53.GetCheckerIpRanges
import Network.AWS.Route53.GetTrafficPolicyInstance
import Network.AWS.Route53.GetHealthCheckLastFailureReason
import Network.AWS.Route53.DeleteReusableDelegationSet
import Network.AWS.Route53.ListHostedZonesByName
import Network.AWS.Route53.ListReusableDelegationSets
import Network.AWS.Route53.ListTagsForResource
import Network.AWS.Route53.ListQueryLoggingConfigs
import Network.AWS.Route53.ListTrafficPolicyInstances
import Network.AWS.Route53.CreateTrafficPolicyInstance
import Network.AWS.Route53.GetChange
import Network.AWS.Route53.ChangeResourceRecordSets
import Network.AWS.Route53.DeleteHealthCheck
import Network.AWS.Route53.UpdateHealthCheck
import Network.AWS.Route53.CreateHostedZone
import Network.AWS.Route53.CreateVPCAssociationAuthorization
import Network.AWS.Route53.ListVPCAssociationAuthorizations
import Network.AWS.Route53.ListTrafficPolicyInstancesByPolicy
import Network.AWS.Route53.DisassociateVPCFromHostedZone
import Network.AWS.Route53.CreateHealthCheck
import Network.AWS.Route53.DeleteVPCAssociationAuthorization
import Network.AWS.Route53.ChangeTagsForResource
import Network.AWS.Route53.ListHostedZones
import Network.AWS.Route53.GetTrafficPolicyInstanceCount
import Network.AWS.Route53.ListGeoLocations
import Network.AWS.Route53.GetHostedZone
import Network.AWS.Route53.GetHealthCheck
import Network.AWS.Route53.ListResourceRecordSets
import Network.AWS.Route53.CreateReusableDelegationSet
import Network.AWS.Route53.CreateQueryLoggingConfig
import Network.AWS.Route53.GetHealthCheckCount
import Network.AWS.Route53.UpdateTrafficPolicyComment
import Network.AWS.Route53.GetHostedZoneCount
import Network.AWS.Route53.GetAccountLimit
import Network.AWS.Route53.DeleteQueryLoggingConfig
import Network.AWS.Route53.GetQueryLoggingConfig
import Network.AWS.Route53.GetReusableDelegationSet
import Network.AWS.Route53.DeleteTrafficPolicyInstance
import Network.AWS.Route53.UpdateTrafficPolicyInstance
import Network.AWS.Route53.UpdateHostedZoneComment
import Network.AWS.Route53.GetHealthCheckStatus
import Network.AWS.Route53.ListHostedZonesByVPC
import Network.AWS.Route53.GetReusableDelegationSetLimit
import Network.AWS.Route53.CreateTrafficPolicyVersion
import Network.AWS.Route53.TestDNSAnswer
import Network.AWS.Route53.ListHealthChecks
import Network.AWS.Route53.GetTrafficPolicy
import Network.AWS.Route53.ListTrafficPolicyVersions
import Network.AWS.Route53.DeleteHostedZone
import Network.AWS.Route53.GetGeoLocation
import Network.AWS.Route53.ListTagsForResources
import Network.AWS.Route53.CreateTrafficPolicy
import Network.AWS.Route53.ListTrafficPolicyInstancesByHostedZone
import Network.AWS.Route53.ListTrafficPolicies
import Network.AWS.Route53.Internal
import qualified Network.AWS.Prelude as Lude

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'Route53'.
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
