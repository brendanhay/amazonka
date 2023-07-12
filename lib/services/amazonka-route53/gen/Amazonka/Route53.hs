{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.Route53
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2013-04-01@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon Route 53 is a scalable Domain Name System (DNS) web service. It
-- provides secure and reliable routing to your infrastructure that uses
-- Amazon Web Services (AWS) products, such as Amazon Elastic Compute Cloud
-- (Amazon EC2), Elastic Load Balancing, or Amazon Simple Storage Service
-- (Amazon S3). You can also use Amazon Route 53 to route users to your
-- infrastructure outside of AWS.
module Amazonka.Route53
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** CidrBlockInUseException
    _CidrBlockInUseException,

    -- ** CidrCollectionAlreadyExistsException
    _CidrCollectionAlreadyExistsException,

    -- ** CidrCollectionInUseException
    _CidrCollectionInUseException,

    -- ** CidrCollectionVersionMismatchException
    _CidrCollectionVersionMismatchException,

    -- ** ConcurrentModification
    _ConcurrentModification,

    -- ** ConflictingDomainExists
    _ConflictingDomainExists,

    -- ** ConflictingTypes
    _ConflictingTypes,

    -- ** DNSSECNotFound
    _DNSSECNotFound,

    -- ** DelegationSetAlreadyCreated
    _DelegationSetAlreadyCreated,

    -- ** DelegationSetAlreadyReusable
    _DelegationSetAlreadyReusable,

    -- ** DelegationSetInUse
    _DelegationSetInUse,

    -- ** DelegationSetNotAvailable
    _DelegationSetNotAvailable,

    -- ** DelegationSetNotReusable
    _DelegationSetNotReusable,

    -- ** HealthCheckAlreadyExists
    _HealthCheckAlreadyExists,

    -- ** HealthCheckInUse
    _HealthCheckInUse,

    -- ** HealthCheckVersionMismatch
    _HealthCheckVersionMismatch,

    -- ** HostedZoneAlreadyExists
    _HostedZoneAlreadyExists,

    -- ** HostedZoneNotEmpty
    _HostedZoneNotEmpty,

    -- ** HostedZoneNotFound
    _HostedZoneNotFound,

    -- ** HostedZoneNotPrivate
    _HostedZoneNotPrivate,

    -- ** HostedZonePartiallyDelegated
    _HostedZonePartiallyDelegated,

    -- ** IncompatibleVersion
    _IncompatibleVersion,

    -- ** InsufficientCloudWatchLogsResourcePolicy
    _InsufficientCloudWatchLogsResourcePolicy,

    -- ** InvalidArgument
    _InvalidArgument,

    -- ** InvalidChangeBatch
    _InvalidChangeBatch,

    -- ** InvalidDomainName
    _InvalidDomainName,

    -- ** InvalidInput
    _InvalidInput,

    -- ** InvalidKMSArn
    _InvalidKMSArn,

    -- ** InvalidKeySigningKeyName
    _InvalidKeySigningKeyName,

    -- ** InvalidKeySigningKeyStatus
    _InvalidKeySigningKeyStatus,

    -- ** InvalidPaginationToken
    _InvalidPaginationToken,

    -- ** InvalidSigningStatus
    _InvalidSigningStatus,

    -- ** InvalidTrafficPolicyDocument
    _InvalidTrafficPolicyDocument,

    -- ** InvalidVPCId
    _InvalidVPCId,

    -- ** KeySigningKeyAlreadyExists
    _KeySigningKeyAlreadyExists,

    -- ** KeySigningKeyInParentDSRecord
    _KeySigningKeyInParentDSRecord,

    -- ** KeySigningKeyInUse
    _KeySigningKeyInUse,

    -- ** KeySigningKeyWithActiveStatusNotFound
    _KeySigningKeyWithActiveStatusNotFound,

    -- ** LastVPCAssociation
    _LastVPCAssociation,

    -- ** LimitsExceeded
    _LimitsExceeded,

    -- ** NoSuchChange
    _NoSuchChange,

    -- ** NoSuchCidrCollectionException
    _NoSuchCidrCollectionException,

    -- ** NoSuchCidrLocationException
    _NoSuchCidrLocationException,

    -- ** NoSuchCloudWatchLogsLogGroup
    _NoSuchCloudWatchLogsLogGroup,

    -- ** NoSuchDelegationSet
    _NoSuchDelegationSet,

    -- ** NoSuchGeoLocation
    _NoSuchGeoLocation,

    -- ** NoSuchHealthCheck
    _NoSuchHealthCheck,

    -- ** NoSuchHostedZone
    _NoSuchHostedZone,

    -- ** NoSuchKeySigningKey
    _NoSuchKeySigningKey,

    -- ** NoSuchQueryLoggingConfig
    _NoSuchQueryLoggingConfig,

    -- ** NoSuchTrafficPolicy
    _NoSuchTrafficPolicy,

    -- ** NoSuchTrafficPolicyInstance
    _NoSuchTrafficPolicyInstance,

    -- ** NotAuthorizedException
    _NotAuthorizedException,

    -- ** PriorRequestNotComplete
    _PriorRequestNotComplete,

    -- ** PublicZoneVPCAssociation
    _PublicZoneVPCAssociation,

    -- ** QueryLoggingConfigAlreadyExists
    _QueryLoggingConfigAlreadyExists,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** TooManyHealthChecks
    _TooManyHealthChecks,

    -- ** TooManyHostedZones
    _TooManyHostedZones,

    -- ** TooManyKeySigningKeys
    _TooManyKeySigningKeys,

    -- ** TooManyTrafficPolicies
    _TooManyTrafficPolicies,

    -- ** TooManyTrafficPolicyInstances
    _TooManyTrafficPolicyInstances,

    -- ** TooManyTrafficPolicyVersionsForCurrentPolicy
    _TooManyTrafficPolicyVersionsForCurrentPolicy,

    -- ** TooManyVPCAssociationAuthorizations
    _TooManyVPCAssociationAuthorizations,

    -- ** TrafficPolicyAlreadyExists
    _TrafficPolicyAlreadyExists,

    -- ** TrafficPolicyInUse
    _TrafficPolicyInUse,

    -- ** TrafficPolicyInstanceAlreadyExists
    _TrafficPolicyInstanceAlreadyExists,

    -- ** VPCAssociationAuthorizationNotFound
    _VPCAssociationAuthorizationNotFound,

    -- ** VPCAssociationNotFound
    _VPCAssociationNotFound,

    -- * Waiters
    -- $waiters

    -- ** ResourceRecordSetsChanged
    newResourceRecordSetsChanged,

    -- * Operations
    -- $operations

    -- ** ActivateKeySigningKey
    ActivateKeySigningKey (ActivateKeySigningKey'),
    newActivateKeySigningKey,
    ActivateKeySigningKeyResponse (ActivateKeySigningKeyResponse'),
    newActivateKeySigningKeyResponse,

    -- ** AssociateVPCWithHostedZone
    AssociateVPCWithHostedZone (AssociateVPCWithHostedZone'),
    newAssociateVPCWithHostedZone,
    AssociateVPCWithHostedZoneResponse (AssociateVPCWithHostedZoneResponse'),
    newAssociateVPCWithHostedZoneResponse,

    -- ** ChangeCidrCollection
    ChangeCidrCollection (ChangeCidrCollection'),
    newChangeCidrCollection,
    ChangeCidrCollectionResponse (ChangeCidrCollectionResponse'),
    newChangeCidrCollectionResponse,

    -- ** ChangeResourceRecordSets
    ChangeResourceRecordSets (ChangeResourceRecordSets'),
    newChangeResourceRecordSets,
    ChangeResourceRecordSetsResponse (ChangeResourceRecordSetsResponse'),
    newChangeResourceRecordSetsResponse,

    -- ** ChangeTagsForResource
    ChangeTagsForResource (ChangeTagsForResource'),
    newChangeTagsForResource,
    ChangeTagsForResourceResponse (ChangeTagsForResourceResponse'),
    newChangeTagsForResourceResponse,

    -- ** CreateCidrCollection
    CreateCidrCollection (CreateCidrCollection'),
    newCreateCidrCollection,
    CreateCidrCollectionResponse (CreateCidrCollectionResponse'),
    newCreateCidrCollectionResponse,

    -- ** CreateHealthCheck
    CreateHealthCheck (CreateHealthCheck'),
    newCreateHealthCheck,
    CreateHealthCheckResponse (CreateHealthCheckResponse'),
    newCreateHealthCheckResponse,

    -- ** CreateHostedZone
    CreateHostedZone (CreateHostedZone'),
    newCreateHostedZone,
    CreateHostedZoneResponse (CreateHostedZoneResponse'),
    newCreateHostedZoneResponse,

    -- ** CreateKeySigningKey
    CreateKeySigningKey (CreateKeySigningKey'),
    newCreateKeySigningKey,
    CreateKeySigningKeyResponse (CreateKeySigningKeyResponse'),
    newCreateKeySigningKeyResponse,

    -- ** CreateQueryLoggingConfig
    CreateQueryLoggingConfig (CreateQueryLoggingConfig'),
    newCreateQueryLoggingConfig,
    CreateQueryLoggingConfigResponse (CreateQueryLoggingConfigResponse'),
    newCreateQueryLoggingConfigResponse,

    -- ** CreateReusableDelegationSet
    CreateReusableDelegationSet (CreateReusableDelegationSet'),
    newCreateReusableDelegationSet,
    CreateReusableDelegationSetResponse (CreateReusableDelegationSetResponse'),
    newCreateReusableDelegationSetResponse,

    -- ** CreateTrafficPolicy
    CreateTrafficPolicy (CreateTrafficPolicy'),
    newCreateTrafficPolicy,
    CreateTrafficPolicyResponse (CreateTrafficPolicyResponse'),
    newCreateTrafficPolicyResponse,

    -- ** CreateTrafficPolicyInstance
    CreateTrafficPolicyInstance (CreateTrafficPolicyInstance'),
    newCreateTrafficPolicyInstance,
    CreateTrafficPolicyInstanceResponse (CreateTrafficPolicyInstanceResponse'),
    newCreateTrafficPolicyInstanceResponse,

    -- ** CreateTrafficPolicyVersion
    CreateTrafficPolicyVersion (CreateTrafficPolicyVersion'),
    newCreateTrafficPolicyVersion,
    CreateTrafficPolicyVersionResponse (CreateTrafficPolicyVersionResponse'),
    newCreateTrafficPolicyVersionResponse,

    -- ** CreateVPCAssociationAuthorization
    CreateVPCAssociationAuthorization (CreateVPCAssociationAuthorization'),
    newCreateVPCAssociationAuthorization,
    CreateVPCAssociationAuthorizationResponse (CreateVPCAssociationAuthorizationResponse'),
    newCreateVPCAssociationAuthorizationResponse,

    -- ** DeactivateKeySigningKey
    DeactivateKeySigningKey (DeactivateKeySigningKey'),
    newDeactivateKeySigningKey,
    DeactivateKeySigningKeyResponse (DeactivateKeySigningKeyResponse'),
    newDeactivateKeySigningKeyResponse,

    -- ** DeleteCidrCollection
    DeleteCidrCollection (DeleteCidrCollection'),
    newDeleteCidrCollection,
    DeleteCidrCollectionResponse (DeleteCidrCollectionResponse'),
    newDeleteCidrCollectionResponse,

    -- ** DeleteHealthCheck
    DeleteHealthCheck (DeleteHealthCheck'),
    newDeleteHealthCheck,
    DeleteHealthCheckResponse (DeleteHealthCheckResponse'),
    newDeleteHealthCheckResponse,

    -- ** DeleteHostedZone
    DeleteHostedZone (DeleteHostedZone'),
    newDeleteHostedZone,
    DeleteHostedZoneResponse (DeleteHostedZoneResponse'),
    newDeleteHostedZoneResponse,

    -- ** DeleteKeySigningKey
    DeleteKeySigningKey (DeleteKeySigningKey'),
    newDeleteKeySigningKey,
    DeleteKeySigningKeyResponse (DeleteKeySigningKeyResponse'),
    newDeleteKeySigningKeyResponse,

    -- ** DeleteQueryLoggingConfig
    DeleteQueryLoggingConfig (DeleteQueryLoggingConfig'),
    newDeleteQueryLoggingConfig,
    DeleteQueryLoggingConfigResponse (DeleteQueryLoggingConfigResponse'),
    newDeleteQueryLoggingConfigResponse,

    -- ** DeleteReusableDelegationSet
    DeleteReusableDelegationSet (DeleteReusableDelegationSet'),
    newDeleteReusableDelegationSet,
    DeleteReusableDelegationSetResponse (DeleteReusableDelegationSetResponse'),
    newDeleteReusableDelegationSetResponse,

    -- ** DeleteTrafficPolicy
    DeleteTrafficPolicy (DeleteTrafficPolicy'),
    newDeleteTrafficPolicy,
    DeleteTrafficPolicyResponse (DeleteTrafficPolicyResponse'),
    newDeleteTrafficPolicyResponse,

    -- ** DeleteTrafficPolicyInstance
    DeleteTrafficPolicyInstance (DeleteTrafficPolicyInstance'),
    newDeleteTrafficPolicyInstance,
    DeleteTrafficPolicyInstanceResponse (DeleteTrafficPolicyInstanceResponse'),
    newDeleteTrafficPolicyInstanceResponse,

    -- ** DeleteVPCAssociationAuthorization
    DeleteVPCAssociationAuthorization (DeleteVPCAssociationAuthorization'),
    newDeleteVPCAssociationAuthorization,
    DeleteVPCAssociationAuthorizationResponse (DeleteVPCAssociationAuthorizationResponse'),
    newDeleteVPCAssociationAuthorizationResponse,

    -- ** DisableHostedZoneDNSSEC
    DisableHostedZoneDNSSEC (DisableHostedZoneDNSSEC'),
    newDisableHostedZoneDNSSEC,
    DisableHostedZoneDNSSECResponse (DisableHostedZoneDNSSECResponse'),
    newDisableHostedZoneDNSSECResponse,

    -- ** DisassociateVPCFromHostedZone
    DisassociateVPCFromHostedZone (DisassociateVPCFromHostedZone'),
    newDisassociateVPCFromHostedZone,
    DisassociateVPCFromHostedZoneResponse (DisassociateVPCFromHostedZoneResponse'),
    newDisassociateVPCFromHostedZoneResponse,

    -- ** EnableHostedZoneDNSSEC
    EnableHostedZoneDNSSEC (EnableHostedZoneDNSSEC'),
    newEnableHostedZoneDNSSEC,
    EnableHostedZoneDNSSECResponse (EnableHostedZoneDNSSECResponse'),
    newEnableHostedZoneDNSSECResponse,

    -- ** GetAccountLimit
    GetAccountLimit (GetAccountLimit'),
    newGetAccountLimit,
    GetAccountLimitResponse (GetAccountLimitResponse'),
    newGetAccountLimitResponse,

    -- ** GetChange
    GetChange (GetChange'),
    newGetChange,
    GetChangeResponse (GetChangeResponse'),
    newGetChangeResponse,

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

    -- ** GetGeoLocation
    GetGeoLocation (GetGeoLocation'),
    newGetGeoLocation,
    GetGeoLocationResponse (GetGeoLocationResponse'),
    newGetGeoLocationResponse,

    -- ** GetHealthCheck
    GetHealthCheck (GetHealthCheck'),
    newGetHealthCheck,
    GetHealthCheckResponse (GetHealthCheckResponse'),
    newGetHealthCheckResponse,

    -- ** GetHealthCheckCount
    GetHealthCheckCount (GetHealthCheckCount'),
    newGetHealthCheckCount,
    GetHealthCheckCountResponse (GetHealthCheckCountResponse'),
    newGetHealthCheckCountResponse,

    -- ** GetHealthCheckLastFailureReason
    GetHealthCheckLastFailureReason (GetHealthCheckLastFailureReason'),
    newGetHealthCheckLastFailureReason,
    GetHealthCheckLastFailureReasonResponse (GetHealthCheckLastFailureReasonResponse'),
    newGetHealthCheckLastFailureReasonResponse,

    -- ** GetHealthCheckStatus
    GetHealthCheckStatus (GetHealthCheckStatus'),
    newGetHealthCheckStatus,
    GetHealthCheckStatusResponse (GetHealthCheckStatusResponse'),
    newGetHealthCheckStatusResponse,

    -- ** GetHostedZone
    GetHostedZone (GetHostedZone'),
    newGetHostedZone,
    GetHostedZoneResponse (GetHostedZoneResponse'),
    newGetHostedZoneResponse,

    -- ** GetHostedZoneCount
    GetHostedZoneCount (GetHostedZoneCount'),
    newGetHostedZoneCount,
    GetHostedZoneCountResponse (GetHostedZoneCountResponse'),
    newGetHostedZoneCountResponse,

    -- ** GetHostedZoneLimit
    GetHostedZoneLimit (GetHostedZoneLimit'),
    newGetHostedZoneLimit,
    GetHostedZoneLimitResponse (GetHostedZoneLimitResponse'),
    newGetHostedZoneLimitResponse,

    -- ** GetQueryLoggingConfig
    GetQueryLoggingConfig (GetQueryLoggingConfig'),
    newGetQueryLoggingConfig,
    GetQueryLoggingConfigResponse (GetQueryLoggingConfigResponse'),
    newGetQueryLoggingConfigResponse,

    -- ** GetReusableDelegationSet
    GetReusableDelegationSet (GetReusableDelegationSet'),
    newGetReusableDelegationSet,
    GetReusableDelegationSetResponse (GetReusableDelegationSetResponse'),
    newGetReusableDelegationSetResponse,

    -- ** GetReusableDelegationSetLimit
    GetReusableDelegationSetLimit (GetReusableDelegationSetLimit'),
    newGetReusableDelegationSetLimit,
    GetReusableDelegationSetLimitResponse (GetReusableDelegationSetLimitResponse'),
    newGetReusableDelegationSetLimitResponse,

    -- ** GetTrafficPolicy
    GetTrafficPolicy (GetTrafficPolicy'),
    newGetTrafficPolicy,
    GetTrafficPolicyResponse (GetTrafficPolicyResponse'),
    newGetTrafficPolicyResponse,

    -- ** GetTrafficPolicyInstance
    GetTrafficPolicyInstance (GetTrafficPolicyInstance'),
    newGetTrafficPolicyInstance,
    GetTrafficPolicyInstanceResponse (GetTrafficPolicyInstanceResponse'),
    newGetTrafficPolicyInstanceResponse,

    -- ** GetTrafficPolicyInstanceCount
    GetTrafficPolicyInstanceCount (GetTrafficPolicyInstanceCount'),
    newGetTrafficPolicyInstanceCount,
    GetTrafficPolicyInstanceCountResponse (GetTrafficPolicyInstanceCountResponse'),
    newGetTrafficPolicyInstanceCountResponse,

    -- ** ListCidrBlocks (Paginated)
    ListCidrBlocks (ListCidrBlocks'),
    newListCidrBlocks,
    ListCidrBlocksResponse (ListCidrBlocksResponse'),
    newListCidrBlocksResponse,

    -- ** ListCidrCollections (Paginated)
    ListCidrCollections (ListCidrCollections'),
    newListCidrCollections,
    ListCidrCollectionsResponse (ListCidrCollectionsResponse'),
    newListCidrCollectionsResponse,

    -- ** ListCidrLocations (Paginated)
    ListCidrLocations (ListCidrLocations'),
    newListCidrLocations,
    ListCidrLocationsResponse (ListCidrLocationsResponse'),
    newListCidrLocationsResponse,

    -- ** ListGeoLocations
    ListGeoLocations (ListGeoLocations'),
    newListGeoLocations,
    ListGeoLocationsResponse (ListGeoLocationsResponse'),
    newListGeoLocationsResponse,

    -- ** ListHealthChecks (Paginated)
    ListHealthChecks (ListHealthChecks'),
    newListHealthChecks,
    ListHealthChecksResponse (ListHealthChecksResponse'),
    newListHealthChecksResponse,

    -- ** ListHostedZones (Paginated)
    ListHostedZones (ListHostedZones'),
    newListHostedZones,
    ListHostedZonesResponse (ListHostedZonesResponse'),
    newListHostedZonesResponse,

    -- ** ListHostedZonesByName
    ListHostedZonesByName (ListHostedZonesByName'),
    newListHostedZonesByName,
    ListHostedZonesByNameResponse (ListHostedZonesByNameResponse'),
    newListHostedZonesByNameResponse,

    -- ** ListHostedZonesByVPC
    ListHostedZonesByVPC (ListHostedZonesByVPC'),
    newListHostedZonesByVPC,
    ListHostedZonesByVPCResponse (ListHostedZonesByVPCResponse'),
    newListHostedZonesByVPCResponse,

    -- ** ListQueryLoggingConfigs (Paginated)
    ListQueryLoggingConfigs (ListQueryLoggingConfigs'),
    newListQueryLoggingConfigs,
    ListQueryLoggingConfigsResponse (ListQueryLoggingConfigsResponse'),
    newListQueryLoggingConfigsResponse,

    -- ** ListResourceRecordSets (Paginated)
    ListResourceRecordSets (ListResourceRecordSets'),
    newListResourceRecordSets,
    ListResourceRecordSetsResponse (ListResourceRecordSetsResponse'),
    newListResourceRecordSetsResponse,

    -- ** ListReusableDelegationSets
    ListReusableDelegationSets (ListReusableDelegationSets'),
    newListReusableDelegationSets,
    ListReusableDelegationSetsResponse (ListReusableDelegationSetsResponse'),
    newListReusableDelegationSetsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ListTagsForResources
    ListTagsForResources (ListTagsForResources'),
    newListTagsForResources,
    ListTagsForResourcesResponse (ListTagsForResourcesResponse'),
    newListTagsForResourcesResponse,

    -- ** ListTrafficPolicies
    ListTrafficPolicies (ListTrafficPolicies'),
    newListTrafficPolicies,
    ListTrafficPoliciesResponse (ListTrafficPoliciesResponse'),
    newListTrafficPoliciesResponse,

    -- ** ListTrafficPolicyInstances
    ListTrafficPolicyInstances (ListTrafficPolicyInstances'),
    newListTrafficPolicyInstances,
    ListTrafficPolicyInstancesResponse (ListTrafficPolicyInstancesResponse'),
    newListTrafficPolicyInstancesResponse,

    -- ** ListTrafficPolicyInstancesByHostedZone
    ListTrafficPolicyInstancesByHostedZone (ListTrafficPolicyInstancesByHostedZone'),
    newListTrafficPolicyInstancesByHostedZone,
    ListTrafficPolicyInstancesByHostedZoneResponse (ListTrafficPolicyInstancesByHostedZoneResponse'),
    newListTrafficPolicyInstancesByHostedZoneResponse,

    -- ** ListTrafficPolicyInstancesByPolicy
    ListTrafficPolicyInstancesByPolicy (ListTrafficPolicyInstancesByPolicy'),
    newListTrafficPolicyInstancesByPolicy,
    ListTrafficPolicyInstancesByPolicyResponse (ListTrafficPolicyInstancesByPolicyResponse'),
    newListTrafficPolicyInstancesByPolicyResponse,

    -- ** ListTrafficPolicyVersions
    ListTrafficPolicyVersions (ListTrafficPolicyVersions'),
    newListTrafficPolicyVersions,
    ListTrafficPolicyVersionsResponse (ListTrafficPolicyVersionsResponse'),
    newListTrafficPolicyVersionsResponse,

    -- ** ListVPCAssociationAuthorizations (Paginated)
    ListVPCAssociationAuthorizations (ListVPCAssociationAuthorizations'),
    newListVPCAssociationAuthorizations,
    ListVPCAssociationAuthorizationsResponse (ListVPCAssociationAuthorizationsResponse'),
    newListVPCAssociationAuthorizationsResponse,

    -- ** TestDNSAnswer
    TestDNSAnswer (TestDNSAnswer'),
    newTestDNSAnswer,
    TestDNSAnswerResponse (TestDNSAnswerResponse'),
    newTestDNSAnswerResponse,

    -- ** UpdateHealthCheck
    UpdateHealthCheck (UpdateHealthCheck'),
    newUpdateHealthCheck,
    UpdateHealthCheckResponse (UpdateHealthCheckResponse'),
    newUpdateHealthCheckResponse,

    -- ** UpdateHostedZoneComment
    UpdateHostedZoneComment (UpdateHostedZoneComment'),
    newUpdateHostedZoneComment,
    UpdateHostedZoneCommentResponse (UpdateHostedZoneCommentResponse'),
    newUpdateHostedZoneCommentResponse,

    -- ** UpdateTrafficPolicyComment
    UpdateTrafficPolicyComment (UpdateTrafficPolicyComment'),
    newUpdateTrafficPolicyComment,
    UpdateTrafficPolicyCommentResponse (UpdateTrafficPolicyCommentResponse'),
    newUpdateTrafficPolicyCommentResponse,

    -- ** UpdateTrafficPolicyInstance
    UpdateTrafficPolicyInstance (UpdateTrafficPolicyInstance'),
    newUpdateTrafficPolicyInstance,
    UpdateTrafficPolicyInstanceResponse (UpdateTrafficPolicyInstanceResponse'),
    newUpdateTrafficPolicyInstanceResponse,

    -- * Types

    -- ** Common
    module Amazonka.Route53.Internal,

    -- ** AccountLimitType
    AccountLimitType (..),

    -- ** ChangeAction
    ChangeAction (..),

    -- ** ChangeStatus
    ChangeStatus (..),

    -- ** CidrCollectionChangeAction
    CidrCollectionChangeAction (..),

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

    -- ** CidrBlockSummary
    CidrBlockSummary (CidrBlockSummary'),
    newCidrBlockSummary,

    -- ** CidrCollection
    CidrCollection (CidrCollection'),
    newCidrCollection,

    -- ** CidrCollectionChange
    CidrCollectionChange (CidrCollectionChange'),
    newCidrCollectionChange,

    -- ** CidrRoutingConfig
    CidrRoutingConfig (CidrRoutingConfig'),
    newCidrRoutingConfig,

    -- ** CloudWatchAlarmConfiguration
    CloudWatchAlarmConfiguration (CloudWatchAlarmConfiguration'),
    newCloudWatchAlarmConfiguration,

    -- ** CollectionSummary
    CollectionSummary (CollectionSummary'),
    newCollectionSummary,

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

    -- ** LocationSummary
    LocationSummary (LocationSummary'),
    newLocationSummary,

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

import Amazonka.Route53.ActivateKeySigningKey
import Amazonka.Route53.AssociateVPCWithHostedZone
import Amazonka.Route53.ChangeCidrCollection
import Amazonka.Route53.ChangeResourceRecordSets
import Amazonka.Route53.ChangeTagsForResource
import Amazonka.Route53.CreateCidrCollection
import Amazonka.Route53.CreateHealthCheck
import Amazonka.Route53.CreateHostedZone
import Amazonka.Route53.CreateKeySigningKey
import Amazonka.Route53.CreateQueryLoggingConfig
import Amazonka.Route53.CreateReusableDelegationSet
import Amazonka.Route53.CreateTrafficPolicy
import Amazonka.Route53.CreateTrafficPolicyInstance
import Amazonka.Route53.CreateTrafficPolicyVersion
import Amazonka.Route53.CreateVPCAssociationAuthorization
import Amazonka.Route53.DeactivateKeySigningKey
import Amazonka.Route53.DeleteCidrCollection
import Amazonka.Route53.DeleteHealthCheck
import Amazonka.Route53.DeleteHostedZone
import Amazonka.Route53.DeleteKeySigningKey
import Amazonka.Route53.DeleteQueryLoggingConfig
import Amazonka.Route53.DeleteReusableDelegationSet
import Amazonka.Route53.DeleteTrafficPolicy
import Amazonka.Route53.DeleteTrafficPolicyInstance
import Amazonka.Route53.DeleteVPCAssociationAuthorization
import Amazonka.Route53.DisableHostedZoneDNSSEC
import Amazonka.Route53.DisassociateVPCFromHostedZone
import Amazonka.Route53.EnableHostedZoneDNSSEC
import Amazonka.Route53.GetAccountLimit
import Amazonka.Route53.GetChange
import Amazonka.Route53.GetCheckerIpRanges
import Amazonka.Route53.GetDNSSEC
import Amazonka.Route53.GetGeoLocation
import Amazonka.Route53.GetHealthCheck
import Amazonka.Route53.GetHealthCheckCount
import Amazonka.Route53.GetHealthCheckLastFailureReason
import Amazonka.Route53.GetHealthCheckStatus
import Amazonka.Route53.GetHostedZone
import Amazonka.Route53.GetHostedZoneCount
import Amazonka.Route53.GetHostedZoneLimit
import Amazonka.Route53.GetQueryLoggingConfig
import Amazonka.Route53.GetReusableDelegationSet
import Amazonka.Route53.GetReusableDelegationSetLimit
import Amazonka.Route53.GetTrafficPolicy
import Amazonka.Route53.GetTrafficPolicyInstance
import Amazonka.Route53.GetTrafficPolicyInstanceCount
import Amazonka.Route53.Internal
import Amazonka.Route53.Lens
import Amazonka.Route53.ListCidrBlocks
import Amazonka.Route53.ListCidrCollections
import Amazonka.Route53.ListCidrLocations
import Amazonka.Route53.ListGeoLocations
import Amazonka.Route53.ListHealthChecks
import Amazonka.Route53.ListHostedZones
import Amazonka.Route53.ListHostedZonesByName
import Amazonka.Route53.ListHostedZonesByVPC
import Amazonka.Route53.ListQueryLoggingConfigs
import Amazonka.Route53.ListResourceRecordSets
import Amazonka.Route53.ListReusableDelegationSets
import Amazonka.Route53.ListTagsForResource
import Amazonka.Route53.ListTagsForResources
import Amazonka.Route53.ListTrafficPolicies
import Amazonka.Route53.ListTrafficPolicyInstances
import Amazonka.Route53.ListTrafficPolicyInstancesByHostedZone
import Amazonka.Route53.ListTrafficPolicyInstancesByPolicy
import Amazonka.Route53.ListTrafficPolicyVersions
import Amazonka.Route53.ListVPCAssociationAuthorizations
import Amazonka.Route53.TestDNSAnswer
import Amazonka.Route53.Types
import Amazonka.Route53.UpdateHealthCheck
import Amazonka.Route53.UpdateHostedZoneComment
import Amazonka.Route53.UpdateTrafficPolicyComment
import Amazonka.Route53.UpdateTrafficPolicyInstance
import Amazonka.Route53.Waiters

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
