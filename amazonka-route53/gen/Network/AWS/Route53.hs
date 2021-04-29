{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon Route 53 is a scalable Domain Name System (DNS) web service. It
-- provides secure and reliable routing to your infrastructure that uses
-- Amazon Web Services (AWS) products, such as Amazon Elastic Compute Cloud
-- (Amazon EC2), Elastic Load Balancing, or Amazon Simple Storage Service
-- (Amazon S3). You can also use Amazon Route 53 to route users to your
-- infrastructure outside of AWS.
module Network.AWS.Route53
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** KeySigningKeyInUse
    _KeySigningKeyInUse,

    -- ** NoSuchKeySigningKey
    _NoSuchKeySigningKey,

    -- ** HostedZoneNotEmpty
    _HostedZoneNotEmpty,

    -- ** NoSuchQueryLoggingConfig
    _NoSuchQueryLoggingConfig,

    -- ** InvalidInput
    _InvalidInput,

    -- ** HostedZoneNotPrivate
    _HostedZoneNotPrivate,

    -- ** DNSSECNotFound
    _DNSSECNotFound,

    -- ** NoSuchDelegationSet
    _NoSuchDelegationSet,

    -- ** VPCAssociationAuthorizationNotFound
    _VPCAssociationAuthorizationNotFound,

    -- ** NoSuchGeoLocation
    _NoSuchGeoLocation,

    -- ** DelegationSetNotAvailable
    _DelegationSetNotAvailable,

    -- ** HealthCheckVersionMismatch
    _HealthCheckVersionMismatch,

    -- ** HostedZoneAlreadyExists
    _HostedZoneAlreadyExists,

    -- ** InvalidKMSArn
    _InvalidKMSArn,

    -- ** InvalidDomainName
    _InvalidDomainName,

    -- ** InvalidKeySigningKeyStatus
    _InvalidKeySigningKeyStatus,

    -- ** LastVPCAssociation
    _LastVPCAssociation,

    -- ** HealthCheckInUse
    _HealthCheckInUse,

    -- ** TrafficPolicyAlreadyExists
    _TrafficPolicyAlreadyExists,

    -- ** InvalidChangeBatch
    _InvalidChangeBatch,

    -- ** IncompatibleVersion
    _IncompatibleVersion,

    -- ** InvalidTrafficPolicyDocument
    _InvalidTrafficPolicyDocument,

    -- ** NoSuchCloudWatchLogsLogGroup
    _NoSuchCloudWatchLogsLogGroup,

    -- ** InsufficientCloudWatchLogsResourcePolicy
    _InsufficientCloudWatchLogsResourcePolicy,

    -- ** NoSuchTrafficPolicyInstance
    _NoSuchTrafficPolicyInstance,

    -- ** TooManyTrafficPolicyInstances
    _TooManyTrafficPolicyInstances,

    -- ** InvalidKeySigningKeyName
    _InvalidKeySigningKeyName,

    -- ** QueryLoggingConfigAlreadyExists
    _QueryLoggingConfigAlreadyExists,

    -- ** LimitsExceeded
    _LimitsExceeded,

    -- ** KeySigningKeyAlreadyExists
    _KeySigningKeyAlreadyExists,

    -- ** TrafficPolicyInstanceAlreadyExists
    _TrafficPolicyInstanceAlreadyExists,

    -- ** KeySigningKeyInParentDSRecord
    _KeySigningKeyInParentDSRecord,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** VPCAssociationNotFound
    _VPCAssociationNotFound,

    -- ** TooManyKeySigningKeys
    _TooManyKeySigningKeys,

    -- ** DelegationSetInUse
    _DelegationSetInUse,

    -- ** HealthCheckAlreadyExists
    _HealthCheckAlreadyExists,

    -- ** HostedZoneNotFound
    _HostedZoneNotFound,

    -- ** TooManyTrafficPolicies
    _TooManyTrafficPolicies,

    -- ** NoSuchTrafficPolicy
    _NoSuchTrafficPolicy,

    -- ** TooManyTrafficPolicyVersionsForCurrentPolicy
    _TooManyTrafficPolicyVersionsForCurrentPolicy,

    -- ** InvalidVPCId
    _InvalidVPCId,

    -- ** TrafficPolicyInUse
    _TrafficPolicyInUse,

    -- ** InvalidSigningStatus
    _InvalidSigningStatus,

    -- ** DelegationSetNotReusable
    _DelegationSetNotReusable,

    -- ** NoSuchHealthCheck
    _NoSuchHealthCheck,

    -- ** InvalidPaginationToken
    _InvalidPaginationToken,

    -- ** ConflictingDomainExists
    _ConflictingDomainExists,

    -- ** TooManyHealthChecks
    _TooManyHealthChecks,

    -- ** DelegationSetAlreadyCreated
    _DelegationSetAlreadyCreated,

    -- ** TooManyVPCAssociationAuthorizations
    _TooManyVPCAssociationAuthorizations,

    -- ** TooManyHostedZones
    _TooManyHostedZones,

    -- ** PriorRequestNotComplete
    _PriorRequestNotComplete,

    -- ** NoSuchHostedZone
    _NoSuchHostedZone,

    -- ** PublicZoneVPCAssociation
    _PublicZoneVPCAssociation,

    -- ** ConflictingTypes
    _ConflictingTypes,

    -- ** NotAuthorizedException
    _NotAuthorizedException,

    -- ** DelegationSetAlreadyReusable
    _DelegationSetAlreadyReusable,

    -- ** ConcurrentModification
    _ConcurrentModification,

    -- ** KeySigningKeyWithActiveStatusNotFound
    _KeySigningKeyWithActiveStatusNotFound,

    -- ** HostedZonePartiallyDelegated
    _HostedZonePartiallyDelegated,

    -- ** NoSuchChange
    _NoSuchChange,

    -- ** InvalidArgument
    _InvalidArgument,

    -- * Waiters
    -- $waiters

    -- ** ResourceRecordSetsChanged
    newResourceRecordSetsChanged,

    -- * Operations
    -- $operations

    -- ** CreateReusableDelegationSet
    CreateReusableDelegationSet (CreateReusableDelegationSet'),
    newCreateReusableDelegationSet,
    CreateReusableDelegationSetResponse (CreateReusableDelegationSetResponse'),
    newCreateReusableDelegationSetResponse,

    -- ** GetHealthCheckCount
    GetHealthCheckCount (GetHealthCheckCount'),
    newGetHealthCheckCount,
    GetHealthCheckCountResponse (GetHealthCheckCountResponse'),
    newGetHealthCheckCountResponse,

    -- ** GetHostedZoneLimit
    GetHostedZoneLimit (GetHostedZoneLimit'),
    newGetHostedZoneLimit,
    GetHostedZoneLimitResponse (GetHostedZoneLimitResponse'),
    newGetHostedZoneLimitResponse,

    -- ** AssociateVPCWithHostedZone
    AssociateVPCWithHostedZone (AssociateVPCWithHostedZone'),
    newAssociateVPCWithHostedZone,
    AssociateVPCWithHostedZoneResponse (AssociateVPCWithHostedZoneResponse'),
    newAssociateVPCWithHostedZoneResponse,

    -- ** ListGeoLocations
    ListGeoLocations (ListGeoLocations'),
    newListGeoLocations,
    ListGeoLocationsResponse (ListGeoLocationsResponse'),
    newListGeoLocationsResponse,

    -- ** ListTrafficPolicies
    ListTrafficPolicies (ListTrafficPolicies'),
    newListTrafficPolicies,
    ListTrafficPoliciesResponse (ListTrafficPoliciesResponse'),
    newListTrafficPoliciesResponse,

    -- ** CreateTrafficPolicy
    CreateTrafficPolicy (CreateTrafficPolicy'),
    newCreateTrafficPolicy,
    CreateTrafficPolicyResponse (CreateTrafficPolicyResponse'),
    newCreateTrafficPolicyResponse,

    -- ** DeleteHostedZone
    DeleteHostedZone (DeleteHostedZone'),
    newDeleteHostedZone,
    DeleteHostedZoneResponse (DeleteHostedZoneResponse'),
    newDeleteHostedZoneResponse,

    -- ** CreateHealthCheck
    CreateHealthCheck (CreateHealthCheck'),
    newCreateHealthCheck,
    CreateHealthCheckResponse (CreateHealthCheckResponse'),
    newCreateHealthCheckResponse,

    -- ** DisassociateVPCFromHostedZone
    DisassociateVPCFromHostedZone (DisassociateVPCFromHostedZone'),
    newDisassociateVPCFromHostedZone,
    DisassociateVPCFromHostedZoneResponse (DisassociateVPCFromHostedZoneResponse'),
    newDisassociateVPCFromHostedZoneResponse,

    -- ** ChangeTagsForResource
    ChangeTagsForResource (ChangeTagsForResource'),
    newChangeTagsForResource,
    ChangeTagsForResourceResponse (ChangeTagsForResourceResponse'),
    newChangeTagsForResourceResponse,

    -- ** GetGeoLocation
    GetGeoLocation (GetGeoLocation'),
    newGetGeoLocation,
    GetGeoLocationResponse (GetGeoLocationResponse'),
    newGetGeoLocationResponse,

    -- ** DeleteVPCAssociationAuthorization
    DeleteVPCAssociationAuthorization (DeleteVPCAssociationAuthorization'),
    newDeleteVPCAssociationAuthorization,
    DeleteVPCAssociationAuthorizationResponse (DeleteVPCAssociationAuthorizationResponse'),
    newDeleteVPCAssociationAuthorizationResponse,

    -- ** ListHostedZones (Paginated)
    ListHostedZones (ListHostedZones'),
    newListHostedZones,
    ListHostedZonesResponse (ListHostedZonesResponse'),
    newListHostedZonesResponse,

    -- ** DeactivateKeySigningKey
    DeactivateKeySigningKey (DeactivateKeySigningKey'),
    newDeactivateKeySigningKey,
    DeactivateKeySigningKeyResponse (DeactivateKeySigningKeyResponse'),
    newDeactivateKeySigningKeyResponse,

    -- ** TestDNSAnswer
    TestDNSAnswer (TestDNSAnswer'),
    newTestDNSAnswer,
    TestDNSAnswerResponse (TestDNSAnswerResponse'),
    newTestDNSAnswerResponse,

    -- ** CreateHostedZone
    CreateHostedZone (CreateHostedZone'),
    newCreateHostedZone,
    CreateHostedZoneResponse (CreateHostedZoneResponse'),
    newCreateHostedZoneResponse,

    -- ** GetReusableDelegationSetLimit
    GetReusableDelegationSetLimit (GetReusableDelegationSetLimit'),
    newGetReusableDelegationSetLimit,
    GetReusableDelegationSetLimitResponse (GetReusableDelegationSetLimitResponse'),
    newGetReusableDelegationSetLimitResponse,

    -- ** ChangeResourceRecordSets
    ChangeResourceRecordSets (ChangeResourceRecordSets'),
    newChangeResourceRecordSets,
    ChangeResourceRecordSetsResponse (ChangeResourceRecordSetsResponse'),
    newChangeResourceRecordSetsResponse,

    -- ** GetReusableDelegationSet
    GetReusableDelegationSet (GetReusableDelegationSet'),
    newGetReusableDelegationSet,
    GetReusableDelegationSetResponse (GetReusableDelegationSetResponse'),
    newGetReusableDelegationSetResponse,

    -- ** GetCheckerIpRanges
    GetCheckerIpRanges (GetCheckerIpRanges'),
    newGetCheckerIpRanges,
    GetCheckerIpRangesResponse (GetCheckerIpRangesResponse'),
    newGetCheckerIpRangesResponse,

    -- ** GetDNSSEC
    GetDNSSEC (GetDNSSEC'),
    newGetDNSSEC,
    GetDNSSECResponse (GetDNSSECResponse'),
    newGetDNSSECResponse,

    -- ** DeleteKeySigningKey
    DeleteKeySigningKey (DeleteKeySigningKey'),
    newDeleteKeySigningKey,
    DeleteKeySigningKeyResponse (DeleteKeySigningKeyResponse'),
    newDeleteKeySigningKeyResponse,

    -- ** GetTrafficPolicyInstance
    GetTrafficPolicyInstance (GetTrafficPolicyInstance'),
    newGetTrafficPolicyInstance,
    GetTrafficPolicyInstanceResponse (GetTrafficPolicyInstanceResponse'),
    newGetTrafficPolicyInstanceResponse,

    -- ** ListReusableDelegationSets
    ListReusableDelegationSets (ListReusableDelegationSets'),
    newListReusableDelegationSets,
    ListReusableDelegationSetsResponse (ListReusableDelegationSetsResponse'),
    newListReusableDelegationSetsResponse,

    -- ** GetAccountLimit
    GetAccountLimit (GetAccountLimit'),
    newGetAccountLimit,
    GetAccountLimitResponse (GetAccountLimitResponse'),
    newGetAccountLimitResponse,

    -- ** CreateQueryLoggingConfig
    CreateQueryLoggingConfig (CreateQueryLoggingConfig'),
    newCreateQueryLoggingConfig,
    CreateQueryLoggingConfigResponse (CreateQueryLoggingConfigResponse'),
    newCreateQueryLoggingConfigResponse,

    -- ** UpdateTrafficPolicyComment
    UpdateTrafficPolicyComment (UpdateTrafficPolicyComment'),
    newUpdateTrafficPolicyComment,
    UpdateTrafficPolicyCommentResponse (UpdateTrafficPolicyCommentResponse'),
    newUpdateTrafficPolicyCommentResponse,

    -- ** CreateKeySigningKey
    CreateKeySigningKey (CreateKeySigningKey'),
    newCreateKeySigningKey,
    CreateKeySigningKeyResponse (CreateKeySigningKeyResponse'),
    newCreateKeySigningKeyResponse,

    -- ** DisableHostedZoneDNSSEC
    DisableHostedZoneDNSSEC (DisableHostedZoneDNSSEC'),
    newDisableHostedZoneDNSSEC,
    DisableHostedZoneDNSSECResponse (DisableHostedZoneDNSSECResponse'),
    newDisableHostedZoneDNSSECResponse,

    -- ** ListResourceRecordSets (Paginated)
    ListResourceRecordSets (ListResourceRecordSets'),
    newListResourceRecordSets,
    ListResourceRecordSetsResponse (ListResourceRecordSetsResponse'),
    newListResourceRecordSetsResponse,

    -- ** DeleteTrafficPolicy
    DeleteTrafficPolicy (DeleteTrafficPolicy'),
    newDeleteTrafficPolicy,
    DeleteTrafficPolicyResponse (DeleteTrafficPolicyResponse'),
    newDeleteTrafficPolicyResponse,

    -- ** GetHealthCheck
    GetHealthCheck (GetHealthCheck'),
    newGetHealthCheck,
    GetHealthCheckResponse (GetHealthCheckResponse'),
    newGetHealthCheckResponse,

    -- ** ListTrafficPolicyInstancesByHostedZone
    ListTrafficPolicyInstancesByHostedZone (ListTrafficPolicyInstancesByHostedZone'),
    newListTrafficPolicyInstancesByHostedZone,
    ListTrafficPolicyInstancesByHostedZoneResponse (ListTrafficPolicyInstancesByHostedZoneResponse'),
    newListTrafficPolicyInstancesByHostedZoneResponse,

    -- ** ListTagsForResources
    ListTagsForResources (ListTagsForResources'),
    newListTagsForResources,
    ListTagsForResourcesResponse (ListTagsForResourcesResponse'),
    newListTagsForResourcesResponse,

    -- ** GetTrafficPolicyInstanceCount
    GetTrafficPolicyInstanceCount (GetTrafficPolicyInstanceCount'),
    newGetTrafficPolicyInstanceCount,
    GetTrafficPolicyInstanceCountResponse (GetTrafficPolicyInstanceCountResponse'),
    newGetTrafficPolicyInstanceCountResponse,

    -- ** GetHostedZone
    GetHostedZone (GetHostedZone'),
    newGetHostedZone,
    GetHostedZoneResponse (GetHostedZoneResponse'),
    newGetHostedZoneResponse,

    -- ** ListVPCAssociationAuthorizations (Paginated)
    ListVPCAssociationAuthorizations (ListVPCAssociationAuthorizations'),
    newListVPCAssociationAuthorizations,
    ListVPCAssociationAuthorizationsResponse (ListVPCAssociationAuthorizationsResponse'),
    newListVPCAssociationAuthorizationsResponse,

    -- ** ListTrafficPolicyVersions
    ListTrafficPolicyVersions (ListTrafficPolicyVersions'),
    newListTrafficPolicyVersions,
    ListTrafficPolicyVersionsResponse (ListTrafficPolicyVersionsResponse'),
    newListTrafficPolicyVersionsResponse,

    -- ** ListTrafficPolicyInstancesByPolicy
    ListTrafficPolicyInstancesByPolicy (ListTrafficPolicyInstancesByPolicy'),
    newListTrafficPolicyInstancesByPolicy,
    ListTrafficPolicyInstancesByPolicyResponse (ListTrafficPolicyInstancesByPolicyResponse'),
    newListTrafficPolicyInstancesByPolicyResponse,

    -- ** ListHealthChecks (Paginated)
    ListHealthChecks (ListHealthChecks'),
    newListHealthChecks,
    ListHealthChecksResponse (ListHealthChecksResponse'),
    newListHealthChecksResponse,

    -- ** DeleteHealthCheck
    DeleteHealthCheck (DeleteHealthCheck'),
    newDeleteHealthCheck,
    DeleteHealthCheckResponse (DeleteHealthCheckResponse'),
    newDeleteHealthCheckResponse,

    -- ** CreateTrafficPolicyVersion
    CreateTrafficPolicyVersion (CreateTrafficPolicyVersion'),
    newCreateTrafficPolicyVersion,
    CreateTrafficPolicyVersionResponse (CreateTrafficPolicyVersionResponse'),
    newCreateTrafficPolicyVersionResponse,

    -- ** GetTrafficPolicy
    GetTrafficPolicy (GetTrafficPolicy'),
    newGetTrafficPolicy,
    GetTrafficPolicyResponse (GetTrafficPolicyResponse'),
    newGetTrafficPolicyResponse,

    -- ** CreateVPCAssociationAuthorization
    CreateVPCAssociationAuthorization (CreateVPCAssociationAuthorization'),
    newCreateVPCAssociationAuthorization,
    CreateVPCAssociationAuthorizationResponse (CreateVPCAssociationAuthorizationResponse'),
    newCreateVPCAssociationAuthorizationResponse,

    -- ** UpdateHealthCheck
    UpdateHealthCheck (UpdateHealthCheck'),
    newUpdateHealthCheck,
    UpdateHealthCheckResponse (UpdateHealthCheckResponse'),
    newUpdateHealthCheckResponse,

    -- ** CreateTrafficPolicyInstance
    CreateTrafficPolicyInstance (CreateTrafficPolicyInstance'),
    newCreateTrafficPolicyInstance,
    CreateTrafficPolicyInstanceResponse (CreateTrafficPolicyInstanceResponse'),
    newCreateTrafficPolicyInstanceResponse,

    -- ** ListHostedZonesByVPC
    ListHostedZonesByVPC (ListHostedZonesByVPC'),
    newListHostedZonesByVPC,
    ListHostedZonesByVPCResponse (ListHostedZonesByVPCResponse'),
    newListHostedZonesByVPCResponse,

    -- ** GetHealthCheckStatus
    GetHealthCheckStatus (GetHealthCheckStatus'),
    newGetHealthCheckStatus,
    GetHealthCheckStatusResponse (GetHealthCheckStatusResponse'),
    newGetHealthCheckStatusResponse,

    -- ** GetChange
    GetChange (GetChange'),
    newGetChange,
    GetChangeResponse (GetChangeResponse'),
    newGetChangeResponse,

    -- ** UpdateHostedZoneComment
    UpdateHostedZoneComment (UpdateHostedZoneComment'),
    newUpdateHostedZoneComment,
    UpdateHostedZoneCommentResponse (UpdateHostedZoneCommentResponse'),
    newUpdateHostedZoneCommentResponse,

    -- ** ListTrafficPolicyInstances
    ListTrafficPolicyInstances (ListTrafficPolicyInstances'),
    newListTrafficPolicyInstances,
    ListTrafficPolicyInstancesResponse (ListTrafficPolicyInstancesResponse'),
    newListTrafficPolicyInstancesResponse,

    -- ** DeleteTrafficPolicyInstance
    DeleteTrafficPolicyInstance (DeleteTrafficPolicyInstance'),
    newDeleteTrafficPolicyInstance,
    DeleteTrafficPolicyInstanceResponse (DeleteTrafficPolicyInstanceResponse'),
    newDeleteTrafficPolicyInstanceResponse,

    -- ** UpdateTrafficPolicyInstance
    UpdateTrafficPolicyInstance (UpdateTrafficPolicyInstance'),
    newUpdateTrafficPolicyInstance,
    UpdateTrafficPolicyInstanceResponse (UpdateTrafficPolicyInstanceResponse'),
    newUpdateTrafficPolicyInstanceResponse,

    -- ** GetQueryLoggingConfig
    GetQueryLoggingConfig (GetQueryLoggingConfig'),
    newGetQueryLoggingConfig,
    GetQueryLoggingConfigResponse (GetQueryLoggingConfigResponse'),
    newGetQueryLoggingConfigResponse,

    -- ** DeleteReusableDelegationSet
    DeleteReusableDelegationSet (DeleteReusableDelegationSet'),
    newDeleteReusableDelegationSet,
    DeleteReusableDelegationSetResponse (DeleteReusableDelegationSetResponse'),
    newDeleteReusableDelegationSetResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** DeleteQueryLoggingConfig
    DeleteQueryLoggingConfig (DeleteQueryLoggingConfig'),
    newDeleteQueryLoggingConfig,
    DeleteQueryLoggingConfigResponse (DeleteQueryLoggingConfigResponse'),
    newDeleteQueryLoggingConfigResponse,

    -- ** GetHealthCheckLastFailureReason
    GetHealthCheckLastFailureReason (GetHealthCheckLastFailureReason'),
    newGetHealthCheckLastFailureReason,
    GetHealthCheckLastFailureReasonResponse (GetHealthCheckLastFailureReasonResponse'),
    newGetHealthCheckLastFailureReasonResponse,

    -- ** EnableHostedZoneDNSSEC
    EnableHostedZoneDNSSEC (EnableHostedZoneDNSSEC'),
    newEnableHostedZoneDNSSEC,
    EnableHostedZoneDNSSECResponse (EnableHostedZoneDNSSECResponse'),
    newEnableHostedZoneDNSSECResponse,

    -- ** ListQueryLoggingConfigs (Paginated)
    ListQueryLoggingConfigs (ListQueryLoggingConfigs'),
    newListQueryLoggingConfigs,
    ListQueryLoggingConfigsResponse (ListQueryLoggingConfigsResponse'),
    newListQueryLoggingConfigsResponse,

    -- ** ListHostedZonesByName
    ListHostedZonesByName (ListHostedZonesByName'),
    newListHostedZonesByName,
    ListHostedZonesByNameResponse (ListHostedZonesByNameResponse'),
    newListHostedZonesByNameResponse,

    -- ** GetHostedZoneCount
    GetHostedZoneCount (GetHostedZoneCount'),
    newGetHostedZoneCount,
    GetHostedZoneCountResponse (GetHostedZoneCountResponse'),
    newGetHostedZoneCountResponse,

    -- ** ActivateKeySigningKey
    ActivateKeySigningKey (ActivateKeySigningKey'),
    newActivateKeySigningKey,
    ActivateKeySigningKeyResponse (ActivateKeySigningKeyResponse'),
    newActivateKeySigningKeyResponse,

    -- * Types

    -- ** Common
    module Network.AWS.Route53.Internal,

    -- ** AccountLimitType
    AccountLimitType (..),

    -- ** ChangeAction
    ChangeAction (..),

    -- ** ChangeStatus
    ChangeStatus (..),

    -- ** CloudWatchRegion
    CloudWatchRegion (..),

    -- ** ComparisonOperator
    ComparisonOperator (..),

    -- ** HealthCheckRegion
    HealthCheckRegion (..),

    -- ** HealthCheckType
    HealthCheckType (..),

    -- ** HostedZoneLimitType
    HostedZoneLimitType (..),

    -- ** InsufficientDataHealthStatus
    InsufficientDataHealthStatus (..),

    -- ** RRType
    RRType (..),

    -- ** ResettableElementName
    ResettableElementName (..),

    -- ** ResourceRecordSetFailover
    ResourceRecordSetFailover (..),

    -- ** ReusableDelegationSetLimitType
    ReusableDelegationSetLimitType (..),

    -- ** Statistic
    Statistic (..),

    -- ** TagResourceType
    TagResourceType (..),

    -- ** VPCRegion
    VPCRegion (..),

    -- ** AccountLimit
    AccountLimit (AccountLimit'),
    newAccountLimit,

    -- ** AlarmIdentifier
    AlarmIdentifier (AlarmIdentifier'),
    newAlarmIdentifier,

    -- ** AliasTarget
    AliasTarget (AliasTarget'),
    newAliasTarget,

    -- ** Change
    Change (Change'),
    newChange,

    -- ** ChangeBatch
    ChangeBatch (ChangeBatch'),
    newChangeBatch,

    -- ** ChangeInfo
    ChangeInfo (ChangeInfo'),
    newChangeInfo,

    -- ** CloudWatchAlarmConfiguration
    CloudWatchAlarmConfiguration (CloudWatchAlarmConfiguration'),
    newCloudWatchAlarmConfiguration,

    -- ** DNSSECStatus
    DNSSECStatus (DNSSECStatus'),
    newDNSSECStatus,

    -- ** DelegationSet
    DelegationSet (DelegationSet'),
    newDelegationSet,

    -- ** Dimension
    Dimension (Dimension'),
    newDimension,

    -- ** GeoLocation
    GeoLocation (GeoLocation'),
    newGeoLocation,

    -- ** GeoLocationDetails
    GeoLocationDetails (GeoLocationDetails'),
    newGeoLocationDetails,

    -- ** HealthCheck
    HealthCheck (HealthCheck'),
    newHealthCheck,

    -- ** HealthCheckConfig
    HealthCheckConfig (HealthCheckConfig'),
    newHealthCheckConfig,

    -- ** HealthCheckObservation
    HealthCheckObservation (HealthCheckObservation'),
    newHealthCheckObservation,

    -- ** HostedZone
    HostedZone (HostedZone'),
    newHostedZone,

    -- ** HostedZoneConfig
    HostedZoneConfig (HostedZoneConfig'),
    newHostedZoneConfig,

    -- ** HostedZoneLimit
    HostedZoneLimit (HostedZoneLimit'),
    newHostedZoneLimit,

    -- ** HostedZoneOwner
    HostedZoneOwner (HostedZoneOwner'),
    newHostedZoneOwner,

    -- ** HostedZoneSummary
    HostedZoneSummary (HostedZoneSummary'),
    newHostedZoneSummary,

    -- ** KeySigningKey
    KeySigningKey (KeySigningKey'),
    newKeySigningKey,

    -- ** LinkedService
    LinkedService (LinkedService'),
    newLinkedService,

    -- ** QueryLoggingConfig
    QueryLoggingConfig (QueryLoggingConfig'),
    newQueryLoggingConfig,

    -- ** ResourceRecord
    ResourceRecord (ResourceRecord'),
    newResourceRecord,

    -- ** ResourceRecordSet
    ResourceRecordSet (ResourceRecordSet'),
    newResourceRecordSet,

    -- ** ResourceTagSet
    ResourceTagSet (ResourceTagSet'),
    newResourceTagSet,

    -- ** ReusableDelegationSetLimit
    ReusableDelegationSetLimit (ReusableDelegationSetLimit'),
    newReusableDelegationSetLimit,

    -- ** StatusReport
    StatusReport (StatusReport'),
    newStatusReport,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** TrafficPolicy
    TrafficPolicy (TrafficPolicy'),
    newTrafficPolicy,

    -- ** TrafficPolicyInstance
    TrafficPolicyInstance (TrafficPolicyInstance'),
    newTrafficPolicyInstance,

    -- ** TrafficPolicySummary
    TrafficPolicySummary (TrafficPolicySummary'),
    newTrafficPolicySummary,

    -- ** VPC
    VPC (VPC'),
    newVPC,
  )
where

import Network.AWS.Route53.ActivateKeySigningKey
import Network.AWS.Route53.AssociateVPCWithHostedZone
import Network.AWS.Route53.ChangeResourceRecordSets
import Network.AWS.Route53.ChangeTagsForResource
import Network.AWS.Route53.CreateHealthCheck
import Network.AWS.Route53.CreateHostedZone
import Network.AWS.Route53.CreateKeySigningKey
import Network.AWS.Route53.CreateQueryLoggingConfig
import Network.AWS.Route53.CreateReusableDelegationSet
import Network.AWS.Route53.CreateTrafficPolicy
import Network.AWS.Route53.CreateTrafficPolicyInstance
import Network.AWS.Route53.CreateTrafficPolicyVersion
import Network.AWS.Route53.CreateVPCAssociationAuthorization
import Network.AWS.Route53.DeactivateKeySigningKey
import Network.AWS.Route53.DeleteHealthCheck
import Network.AWS.Route53.DeleteHostedZone
import Network.AWS.Route53.DeleteKeySigningKey
import Network.AWS.Route53.DeleteQueryLoggingConfig
import Network.AWS.Route53.DeleteReusableDelegationSet
import Network.AWS.Route53.DeleteTrafficPolicy
import Network.AWS.Route53.DeleteTrafficPolicyInstance
import Network.AWS.Route53.DeleteVPCAssociationAuthorization
import Network.AWS.Route53.DisableHostedZoneDNSSEC
import Network.AWS.Route53.DisassociateVPCFromHostedZone
import Network.AWS.Route53.EnableHostedZoneDNSSEC
import Network.AWS.Route53.GetAccountLimit
import Network.AWS.Route53.GetChange
import Network.AWS.Route53.GetCheckerIpRanges
import Network.AWS.Route53.GetDNSSEC
import Network.AWS.Route53.GetGeoLocation
import Network.AWS.Route53.GetHealthCheck
import Network.AWS.Route53.GetHealthCheckCount
import Network.AWS.Route53.GetHealthCheckLastFailureReason
import Network.AWS.Route53.GetHealthCheckStatus
import Network.AWS.Route53.GetHostedZone
import Network.AWS.Route53.GetHostedZoneCount
import Network.AWS.Route53.GetHostedZoneLimit
import Network.AWS.Route53.GetQueryLoggingConfig
import Network.AWS.Route53.GetReusableDelegationSet
import Network.AWS.Route53.GetReusableDelegationSetLimit
import Network.AWS.Route53.GetTrafficPolicy
import Network.AWS.Route53.GetTrafficPolicyInstance
import Network.AWS.Route53.GetTrafficPolicyInstanceCount
import Network.AWS.Route53.Internal
import Network.AWS.Route53.Lens
import Network.AWS.Route53.ListGeoLocations
import Network.AWS.Route53.ListHealthChecks
import Network.AWS.Route53.ListHostedZones
import Network.AWS.Route53.ListHostedZonesByName
import Network.AWS.Route53.ListHostedZonesByVPC
import Network.AWS.Route53.ListQueryLoggingConfigs
import Network.AWS.Route53.ListResourceRecordSets
import Network.AWS.Route53.ListReusableDelegationSets
import Network.AWS.Route53.ListTagsForResource
import Network.AWS.Route53.ListTagsForResources
import Network.AWS.Route53.ListTrafficPolicies
import Network.AWS.Route53.ListTrafficPolicyInstances
import Network.AWS.Route53.ListTrafficPolicyInstancesByHostedZone
import Network.AWS.Route53.ListTrafficPolicyInstancesByPolicy
import Network.AWS.Route53.ListTrafficPolicyVersions
import Network.AWS.Route53.ListVPCAssociationAuthorizations
import Network.AWS.Route53.TestDNSAnswer
import Network.AWS.Route53.Types
import Network.AWS.Route53.UpdateHealthCheck
import Network.AWS.Route53.UpdateHostedZoneComment
import Network.AWS.Route53.UpdateTrafficPolicyComment
import Network.AWS.Route53.UpdateTrafficPolicyInstance
import Network.AWS.Route53.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'Route53'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
