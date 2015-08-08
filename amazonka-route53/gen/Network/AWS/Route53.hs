{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon Route 53 is a scalable Domain Name System (DNS) web service. It
-- provides secure and reliable routing to your infrastructure that uses
-- Amazon Web Services (AWS) products, such as Amazon Elastic Compute Cloud
-- (Amazon EC2), Elastic Load Balancing, or Amazon Simple Storage Service
-- (Amazon S3). You can also use Amazon Route 53 to route users to your
-- infrastructure outside of AWS.
--
-- /See:/ <http://docs.aws.amazon.com/Route53/latest/APIReference/Welcome.html AWS API Reference>
module Network.AWS.Route53
    (
    -- * Service Description
      Route53

    -- * Error Matchers
    -- $errors

    -- ** HealthCheckVersionMismatch
    , _HealthCheckVersionMismatch

    -- ** InvalidInput
    , _InvalidInput

    -- ** HostedZoneNotEmpty
    , _HostedZoneNotEmpty

    -- ** InvalidArgument
    , _InvalidArgument

    -- ** DelegationSetAlreadyReusable
    , _DelegationSetAlreadyReusable

    -- ** PriorRequestNotComplete
    , _PriorRequestNotComplete

    -- ** InvalidChangeBatch
    , _InvalidChangeBatch

    -- ** InvalidDomainName
    , _InvalidDomainName

    -- ** DelegationSetNotReusable
    , _DelegationSetNotReusable

    -- ** HealthCheckAlreadyExists
    , _HealthCheckAlreadyExists

    -- ** HostedZoneNotFound
    , _HostedZoneNotFound

    -- ** DelegationSetInUse
    , _DelegationSetInUse

    -- ** NoSuchDelegationSet
    , _NoSuchDelegationSet

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

    -- ** IncompatibleVersion
    , _IncompatibleVersion

    -- ** NoSuchHostedZone
    , _NoSuchHostedZone

    -- ** TooManyHostedZones
    , _TooManyHostedZones

    -- ** PublicZoneVPCAssociation
    , _PublicZoneVPCAssociation

    -- ** ConflictingDomainExists
    , _ConflictingDomainExists

    -- ** LastVPCAssociation
    , _LastVPCAssociation

    -- ** HealthCheckInUse
    , _HealthCheckInUse

    -- ** DelegationSetAlreadyCreated
    , _DelegationSetAlreadyCreated

    -- ** TooManyHealthChecks
    , _TooManyHealthChecks

    -- ** NoSuchHealthCheck
    , _NoSuchHealthCheck

    -- ** HostedZoneAlreadyExists
    , _HostedZoneAlreadyExists

    -- ** InvalidVPCId
    , _InvalidVPCId

    -- * Waiters
    -- $waiters

    -- ** ResourceRecordSetsChanged
    , resourceRecordSetsChanged

    -- * Operations
    -- $operations

    -- ** AssociateVPCWithHostedZone
    , module Network.AWS.Route53.AssociateVPCWithHostedZone

    -- ** GetHealthCheckLastFailureReason
    , module Network.AWS.Route53.GetHealthCheckLastFailureReason

    -- ** ListHostedZonesByName
    , module Network.AWS.Route53.ListHostedZonesByName

    -- ** DeleteReusableDelegationSet
    , module Network.AWS.Route53.DeleteReusableDelegationSet

    -- ** ListReusableDelegationSets
    , module Network.AWS.Route53.ListReusableDelegationSets

    -- ** GetCheckerIPRanges
    , module Network.AWS.Route53.GetCheckerIPRanges

    -- ** ListTagsForResource
    , module Network.AWS.Route53.ListTagsForResource

    -- ** ChangeResourceRecordSets
    , module Network.AWS.Route53.ChangeResourceRecordSets

    -- ** GetChange
    , module Network.AWS.Route53.GetChange

    -- ** CreateHostedZone
    , module Network.AWS.Route53.CreateHostedZone

    -- ** DeleteHealthCheck
    , module Network.AWS.Route53.DeleteHealthCheck

    -- ** UpdateHealthCheck
    , module Network.AWS.Route53.UpdateHealthCheck

    -- ** ChangeTagsForResource
    , module Network.AWS.Route53.ChangeTagsForResource

    -- ** CreateHealthCheck
    , module Network.AWS.Route53.CreateHealthCheck

    -- ** ListHostedZones (Paginated)
    , module Network.AWS.Route53.ListHostedZones
    -- $pager

    -- ** DisassociateVPCFromHostedZone
    , module Network.AWS.Route53.DisassociateVPCFromHostedZone

    -- ** GetHostedZone
    , module Network.AWS.Route53.GetHostedZone

    -- ** ListGeoLocations
    , module Network.AWS.Route53.ListGeoLocations

    -- ** GetHealthCheck
    , module Network.AWS.Route53.GetHealthCheck

    -- ** ListResourceRecordSets (Paginated)
    , module Network.AWS.Route53.ListResourceRecordSets
    -- $pager

    -- ** GetHealthCheckCount
    , module Network.AWS.Route53.GetHealthCheckCount

    -- ** CreateReusableDelegationSet
    , module Network.AWS.Route53.CreateReusableDelegationSet

    -- ** GetHostedZoneCount
    , module Network.AWS.Route53.GetHostedZoneCount

    -- ** GetReusableDelegationSet
    , module Network.AWS.Route53.GetReusableDelegationSet

    -- ** UpdateHostedZoneComment
    , module Network.AWS.Route53.UpdateHostedZoneComment

    -- ** GetHealthCheckStatus
    , module Network.AWS.Route53.GetHealthCheckStatus

    -- ** ListHealthChecks (Paginated)
    , module Network.AWS.Route53.ListHealthChecks
    -- $pager

    -- ** DeleteHostedZone
    , module Network.AWS.Route53.DeleteHostedZone

    -- ** GetGeoLocation
    , module Network.AWS.Route53.GetGeoLocation

    -- ** ListTagsForResources
    , module Network.AWS.Route53.ListTagsForResources

    -- * Types

    -- ** Re-exported Types
    , module Network.AWS.Route53.Internal

    -- ** ChangeAction
    , ChangeAction (..)

    -- ** ChangeStatus
    , ChangeStatus (..)

    -- ** Failover
    , Failover (..)

    -- ** HealthCheckType
    , HealthCheckType (..)

    -- ** RecordType
    , RecordType (..)

    -- ** TagResourceType
    , TagResourceType (..)

    -- ** VPCRegion
    , VPCRegion (..)

    -- ** AliasTarget
    , AliasTarget
    , aliasTarget
    , atHostedZoneId
    , atDNSName
    , atEvaluateTargetHealth

    -- ** Change
    , Change
    , change
    , cAction
    , cResourceRecordSet

    -- ** ChangeBatch
    , ChangeBatch
    , changeBatch
    , cbComment
    , cbChanges

    -- ** ChangeInfo
    , ChangeInfo
    , changeInfo
    , ciComment
    , ciId
    , ciStatus
    , ciSubmittedAt

    -- ** DelegationSet
    , DelegationSet
    , delegationSet
    , dsId
    , dsCallerReference
    , dsNameServers

    -- ** GeoLocation
    , GeoLocation
    , geoLocation
    , glSubdivisionCode
    , glCountryCode
    , glContinentCode

    -- ** GeoLocationDetails
    , GeoLocationDetails
    , geoLocationDetails
    , gldSubdivisionName
    , gldSubdivisionCode
    , gldCountryName
    , gldCountryCode
    , gldContinentCode
    , gldContinentName

    -- ** HealthCheck
    , HealthCheck
    , healthCheck
    , hcId
    , hcCallerReference
    , hcHealthCheckConfig
    , hcHealthCheckVersion

    -- ** HealthCheckConfig
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

    -- ** HealthCheckObservation
    , HealthCheckObservation
    , healthCheckObservation
    , hcoIPAddress
    , hcoStatusReport

    -- ** HostedZone
    , HostedZone
    , hostedZone
    , hzConfig
    , hzResourceRecordSetCount
    , hzId
    , hzName
    , hzCallerReference

    -- ** HostedZoneConfig
    , HostedZoneConfig
    , hostedZoneConfig
    , hzcPrivateZone
    , hzcComment

    -- ** ResourceRecord
    , ResourceRecord
    , resourceRecord
    , rrValue

    -- ** ResourceRecordSet
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

    -- ** ResourceTagSet
    , ResourceTagSet
    , resourceTagSet
    , rtsResourceId
    , rtsResourceType
    , rtsTags

    -- ** StatusReport
    , StatusReport
    , statusReport
    , srStatus
    , srCheckedTime

    -- ** Tag
    , Tag
    , tag
    , tagValue
    , tagKey

    -- ** VPC
    , VPC
    , vpc
    , vpcVPCRegion
    , vpcVPCId
    ) where

import           Network.AWS.Route53.AssociateVPCWithHostedZone
import           Network.AWS.Route53.ChangeResourceRecordSets
import           Network.AWS.Route53.ChangeTagsForResource
import           Network.AWS.Route53.CreateHealthCheck
import           Network.AWS.Route53.CreateHostedZone
import           Network.AWS.Route53.CreateReusableDelegationSet
import           Network.AWS.Route53.DeleteHealthCheck
import           Network.AWS.Route53.DeleteHostedZone
import           Network.AWS.Route53.DeleteReusableDelegationSet
import           Network.AWS.Route53.DisassociateVPCFromHostedZone
import           Network.AWS.Route53.GetChange
import           Network.AWS.Route53.GetCheckerIPRanges
import           Network.AWS.Route53.GetGeoLocation
import           Network.AWS.Route53.GetHealthCheck
import           Network.AWS.Route53.GetHealthCheckCount
import           Network.AWS.Route53.GetHealthCheckLastFailureReason
import           Network.AWS.Route53.GetHealthCheckStatus
import           Network.AWS.Route53.GetHostedZone
import           Network.AWS.Route53.GetHostedZoneCount
import           Network.AWS.Route53.GetReusableDelegationSet
import           Network.AWS.Route53.Internal
import           Network.AWS.Route53.ListGeoLocations
import           Network.AWS.Route53.ListHealthChecks
import           Network.AWS.Route53.ListHostedZones
import           Network.AWS.Route53.ListHostedZonesByName
import           Network.AWS.Route53.ListResourceRecordSets
import           Network.AWS.Route53.ListReusableDelegationSets
import           Network.AWS.Route53.ListTagsForResource
import           Network.AWS.Route53.ListTagsForResources
import           Network.AWS.Route53.Types
import           Network.AWS.Route53.UpdateHealthCheck
import           Network.AWS.Route53.UpdateHostedZoneComment
import           Network.AWS.Route53.Waiters

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

{- $pager
This operation can return paginated results.
-}
