{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon Elasticsearch Configuration Service
--
-- Use the Amazon Elasticsearch Configuration API to create, configure, and
-- manage Elasticsearch domains.
--
-- For sample code that uses the Configuration API, see the
-- <https://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-configuration-samples.html Amazon Elasticsearch Service Developer Guide>.
-- The guide also contains
-- <https://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-request-signing.html sample code for sending signed HTTP requests to the Elasticsearch APIs>.
--
-- The endpoint for configuration service requests is region-specific:
-- es./region/.amazonaws.com. For example, es.us-east-1.amazonaws.com. For
-- a current list of supported regions and endpoints, see
-- <http://docs.aws.amazon.com/general/latest/gr/rande.html#elasticsearch-service-regions Regions and Endpoints>.
module Network.AWS.ElasticSearch
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** InvalidPaginationTokenException
    _InvalidPaginationTokenException,

    -- ** InvalidTypeException
    _InvalidTypeException,

    -- ** ResourceAlreadyExistsException
    _ResourceAlreadyExistsException,

    -- ** InternalException
    _InternalException,

    -- ** BaseException
    _BaseException,

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ValidationException
    _ValidationException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** ConflictException
    _ConflictException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** DisabledOperationException
    _DisabledOperationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DescribeInboundCrossClusterSearchConnections
    DescribeInboundCrossClusterSearchConnections (DescribeInboundCrossClusterSearchConnections'),
    newDescribeInboundCrossClusterSearchConnections,
    DescribeInboundCrossClusterSearchConnectionsResponse (DescribeInboundCrossClusterSearchConnectionsResponse'),
    newDescribeInboundCrossClusterSearchConnectionsResponse,

    -- ** RemoveTags
    RemoveTags (RemoveTags'),
    newRemoveTags,
    RemoveTagsResponse (RemoveTagsResponse'),
    newRemoveTagsResponse,

    -- ** CreateOutboundCrossClusterSearchConnection
    CreateOutboundCrossClusterSearchConnection (CreateOutboundCrossClusterSearchConnection'),
    newCreateOutboundCrossClusterSearchConnection,
    CreateOutboundCrossClusterSearchConnectionResponse (CreateOutboundCrossClusterSearchConnectionResponse'),
    newCreateOutboundCrossClusterSearchConnectionResponse,

    -- ** GetUpgradeHistory (Paginated)
    GetUpgradeHistory (GetUpgradeHistory'),
    newGetUpgradeHistory,
    GetUpgradeHistoryResponse (GetUpgradeHistoryResponse'),
    newGetUpgradeHistoryResponse,

    -- ** DescribeElasticsearchDomainConfig
    DescribeElasticsearchDomainConfig (DescribeElasticsearchDomainConfig'),
    newDescribeElasticsearchDomainConfig,
    DescribeElasticsearchDomainConfigResponse (DescribeElasticsearchDomainConfigResponse'),
    newDescribeElasticsearchDomainConfigResponse,

    -- ** AcceptInboundCrossClusterSearchConnection
    AcceptInboundCrossClusterSearchConnection (AcceptInboundCrossClusterSearchConnection'),
    newAcceptInboundCrossClusterSearchConnection,
    AcceptInboundCrossClusterSearchConnectionResponse (AcceptInboundCrossClusterSearchConnectionResponse'),
    newAcceptInboundCrossClusterSearchConnectionResponse,

    -- ** DeleteOutboundCrossClusterSearchConnection
    DeleteOutboundCrossClusterSearchConnection (DeleteOutboundCrossClusterSearchConnection'),
    newDeleteOutboundCrossClusterSearchConnection,
    DeleteOutboundCrossClusterSearchConnectionResponse (DeleteOutboundCrossClusterSearchConnectionResponse'),
    newDeleteOutboundCrossClusterSearchConnectionResponse,

    -- ** ListDomainNames
    ListDomainNames (ListDomainNames'),
    newListDomainNames,
    ListDomainNamesResponse (ListDomainNamesResponse'),
    newListDomainNamesResponse,

    -- ** CancelElasticsearchServiceSoftwareUpdate
    CancelElasticsearchServiceSoftwareUpdate (CancelElasticsearchServiceSoftwareUpdate'),
    newCancelElasticsearchServiceSoftwareUpdate,
    CancelElasticsearchServiceSoftwareUpdateResponse (CancelElasticsearchServiceSoftwareUpdateResponse'),
    newCancelElasticsearchServiceSoftwareUpdateResponse,

    -- ** DescribeElasticsearchDomain
    DescribeElasticsearchDomain (DescribeElasticsearchDomain'),
    newDescribeElasticsearchDomain,
    DescribeElasticsearchDomainResponse (DescribeElasticsearchDomainResponse'),
    newDescribeElasticsearchDomainResponse,

    -- ** DeleteElasticsearchServiceRole
    DeleteElasticsearchServiceRole (DeleteElasticsearchServiceRole'),
    newDeleteElasticsearchServiceRole,
    DeleteElasticsearchServiceRoleResponse (DeleteElasticsearchServiceRoleResponse'),
    newDeleteElasticsearchServiceRoleResponse,

    -- ** ListElasticsearchInstanceTypes (Paginated)
    ListElasticsearchInstanceTypes (ListElasticsearchInstanceTypes'),
    newListElasticsearchInstanceTypes,
    ListElasticsearchInstanceTypesResponse (ListElasticsearchInstanceTypesResponse'),
    newListElasticsearchInstanceTypesResponse,

    -- ** UpdatePackage
    UpdatePackage (UpdatePackage'),
    newUpdatePackage,
    UpdatePackageResponse (UpdatePackageResponse'),
    newUpdatePackageResponse,

    -- ** DeletePackage
    DeletePackage (DeletePackage'),
    newDeletePackage,
    DeletePackageResponse (DeletePackageResponse'),
    newDeletePackageResponse,

    -- ** AddTags
    AddTags (AddTags'),
    newAddTags,
    AddTagsResponse (AddTagsResponse'),
    newAddTagsResponse,

    -- ** DeleteInboundCrossClusterSearchConnection
    DeleteInboundCrossClusterSearchConnection (DeleteInboundCrossClusterSearchConnection'),
    newDeleteInboundCrossClusterSearchConnection,
    DeleteInboundCrossClusterSearchConnectionResponse (DeleteInboundCrossClusterSearchConnectionResponse'),
    newDeleteInboundCrossClusterSearchConnectionResponse,

    -- ** UpdateElasticsearchDomainConfig
    UpdateElasticsearchDomainConfig (UpdateElasticsearchDomainConfig'),
    newUpdateElasticsearchDomainConfig,
    UpdateElasticsearchDomainConfigResponse (UpdateElasticsearchDomainConfigResponse'),
    newUpdateElasticsearchDomainConfigResponse,

    -- ** ListElasticsearchVersions (Paginated)
    ListElasticsearchVersions (ListElasticsearchVersions'),
    newListElasticsearchVersions,
    ListElasticsearchVersionsResponse (ListElasticsearchVersionsResponse'),
    newListElasticsearchVersionsResponse,

    -- ** DeleteElasticsearchDomain
    DeleteElasticsearchDomain (DeleteElasticsearchDomain'),
    newDeleteElasticsearchDomain,
    DeleteElasticsearchDomainResponse (DeleteElasticsearchDomainResponse'),
    newDeleteElasticsearchDomainResponse,

    -- ** GetCompatibleElasticsearchVersions
    GetCompatibleElasticsearchVersions (GetCompatibleElasticsearchVersions'),
    newGetCompatibleElasticsearchVersions,
    GetCompatibleElasticsearchVersionsResponse (GetCompatibleElasticsearchVersionsResponse'),
    newGetCompatibleElasticsearchVersionsResponse,

    -- ** DissociatePackage
    DissociatePackage (DissociatePackage'),
    newDissociatePackage,
    DissociatePackageResponse (DissociatePackageResponse'),
    newDissociatePackageResponse,

    -- ** CreateElasticsearchDomain
    CreateElasticsearchDomain (CreateElasticsearchDomain'),
    newCreateElasticsearchDomain,
    CreateElasticsearchDomainResponse (CreateElasticsearchDomainResponse'),
    newCreateElasticsearchDomainResponse,

    -- ** DescribePackages
    DescribePackages (DescribePackages'),
    newDescribePackages,
    DescribePackagesResponse (DescribePackagesResponse'),
    newDescribePackagesResponse,

    -- ** GetPackageVersionHistory
    GetPackageVersionHistory (GetPackageVersionHistory'),
    newGetPackageVersionHistory,
    GetPackageVersionHistoryResponse (GetPackageVersionHistoryResponse'),
    newGetPackageVersionHistoryResponse,

    -- ** DescribeElasticsearchInstanceTypeLimits
    DescribeElasticsearchInstanceTypeLimits (DescribeElasticsearchInstanceTypeLimits'),
    newDescribeElasticsearchInstanceTypeLimits,
    DescribeElasticsearchInstanceTypeLimitsResponse (DescribeElasticsearchInstanceTypeLimitsResponse'),
    newDescribeElasticsearchInstanceTypeLimitsResponse,

    -- ** DescribeOutboundCrossClusterSearchConnections
    DescribeOutboundCrossClusterSearchConnections (DescribeOutboundCrossClusterSearchConnections'),
    newDescribeOutboundCrossClusterSearchConnections,
    DescribeOutboundCrossClusterSearchConnectionsResponse (DescribeOutboundCrossClusterSearchConnectionsResponse'),
    newDescribeOutboundCrossClusterSearchConnectionsResponse,

    -- ** AssociatePackage
    AssociatePackage (AssociatePackage'),
    newAssociatePackage,
    AssociatePackageResponse (AssociatePackageResponse'),
    newAssociatePackageResponse,

    -- ** CreatePackage
    CreatePackage (CreatePackage'),
    newCreatePackage,
    CreatePackageResponse (CreatePackageResponse'),
    newCreatePackageResponse,

    -- ** RejectInboundCrossClusterSearchConnection
    RejectInboundCrossClusterSearchConnection (RejectInboundCrossClusterSearchConnection'),
    newRejectInboundCrossClusterSearchConnection,
    RejectInboundCrossClusterSearchConnectionResponse (RejectInboundCrossClusterSearchConnectionResponse'),
    newRejectInboundCrossClusterSearchConnectionResponse,

    -- ** DescribeDomainAutoTunes
    DescribeDomainAutoTunes (DescribeDomainAutoTunes'),
    newDescribeDomainAutoTunes,
    DescribeDomainAutoTunesResponse (DescribeDomainAutoTunesResponse'),
    newDescribeDomainAutoTunesResponse,

    -- ** ListTags
    ListTags (ListTags'),
    newListTags,
    ListTagsResponse (ListTagsResponse'),
    newListTagsResponse,

    -- ** UpgradeElasticsearchDomain
    UpgradeElasticsearchDomain (UpgradeElasticsearchDomain'),
    newUpgradeElasticsearchDomain,
    UpgradeElasticsearchDomainResponse (UpgradeElasticsearchDomainResponse'),
    newUpgradeElasticsearchDomainResponse,

    -- ** ListPackagesForDomain
    ListPackagesForDomain (ListPackagesForDomain'),
    newListPackagesForDomain,
    ListPackagesForDomainResponse (ListPackagesForDomainResponse'),
    newListPackagesForDomainResponse,

    -- ** DescribeReservedElasticsearchInstances (Paginated)
    DescribeReservedElasticsearchInstances (DescribeReservedElasticsearchInstances'),
    newDescribeReservedElasticsearchInstances,
    DescribeReservedElasticsearchInstancesResponse (DescribeReservedElasticsearchInstancesResponse'),
    newDescribeReservedElasticsearchInstancesResponse,

    -- ** DescribeReservedElasticsearchInstanceOfferings (Paginated)
    DescribeReservedElasticsearchInstanceOfferings (DescribeReservedElasticsearchInstanceOfferings'),
    newDescribeReservedElasticsearchInstanceOfferings,
    DescribeReservedElasticsearchInstanceOfferingsResponse (DescribeReservedElasticsearchInstanceOfferingsResponse'),
    newDescribeReservedElasticsearchInstanceOfferingsResponse,

    -- ** StartElasticsearchServiceSoftwareUpdate
    StartElasticsearchServiceSoftwareUpdate (StartElasticsearchServiceSoftwareUpdate'),
    newStartElasticsearchServiceSoftwareUpdate,
    StartElasticsearchServiceSoftwareUpdateResponse (StartElasticsearchServiceSoftwareUpdateResponse'),
    newStartElasticsearchServiceSoftwareUpdateResponse,

    -- ** ListDomainsForPackage
    ListDomainsForPackage (ListDomainsForPackage'),
    newListDomainsForPackage,
    ListDomainsForPackageResponse (ListDomainsForPackageResponse'),
    newListDomainsForPackageResponse,

    -- ** DescribeElasticsearchDomains
    DescribeElasticsearchDomains (DescribeElasticsearchDomains'),
    newDescribeElasticsearchDomains,
    DescribeElasticsearchDomainsResponse (DescribeElasticsearchDomainsResponse'),
    newDescribeElasticsearchDomainsResponse,

    -- ** PurchaseReservedElasticsearchInstanceOffering
    PurchaseReservedElasticsearchInstanceOffering (PurchaseReservedElasticsearchInstanceOffering'),
    newPurchaseReservedElasticsearchInstanceOffering,
    PurchaseReservedElasticsearchInstanceOfferingResponse (PurchaseReservedElasticsearchInstanceOfferingResponse'),
    newPurchaseReservedElasticsearchInstanceOfferingResponse,

    -- ** GetUpgradeStatus
    GetUpgradeStatus (GetUpgradeStatus'),
    newGetUpgradeStatus,
    GetUpgradeStatusResponse (GetUpgradeStatusResponse'),
    newGetUpgradeStatusResponse,

    -- * Types

    -- ** AutoTuneDesiredState
    AutoTuneDesiredState (..),

    -- ** AutoTuneState
    AutoTuneState (..),

    -- ** AutoTuneType
    AutoTuneType (..),

    -- ** DeploymentStatus
    DeploymentStatus (..),

    -- ** DescribePackagesFilterName
    DescribePackagesFilterName (..),

    -- ** DomainPackageStatus
    DomainPackageStatus (..),

    -- ** ESPartitionInstanceType
    ESPartitionInstanceType (..),

    -- ** ESWarmPartitionInstanceType
    ESWarmPartitionInstanceType (..),

    -- ** InboundCrossClusterSearchConnectionStatusCode
    InboundCrossClusterSearchConnectionStatusCode (..),

    -- ** LogType
    LogType (..),

    -- ** OptionState
    OptionState (..),

    -- ** OutboundCrossClusterSearchConnectionStatusCode
    OutboundCrossClusterSearchConnectionStatusCode (..),

    -- ** PackageStatus
    PackageStatus (..),

    -- ** PackageType
    PackageType (..),

    -- ** ReservedElasticsearchInstancePaymentOption
    ReservedElasticsearchInstancePaymentOption (..),

    -- ** RollbackOnDisable
    RollbackOnDisable (..),

    -- ** ScheduledAutoTuneActionType
    ScheduledAutoTuneActionType (..),

    -- ** ScheduledAutoTuneSeverityType
    ScheduledAutoTuneSeverityType (..),

    -- ** TLSSecurityPolicy
    TLSSecurityPolicy (..),

    -- ** TimeUnit
    TimeUnit (..),

    -- ** UpgradeStatus
    UpgradeStatus (..),

    -- ** UpgradeStep
    UpgradeStep (..),

    -- ** VolumeType
    VolumeType (..),

    -- ** AccessPoliciesStatus
    AccessPoliciesStatus (AccessPoliciesStatus'),
    newAccessPoliciesStatus,

    -- ** AdditionalLimit
    AdditionalLimit (AdditionalLimit'),
    newAdditionalLimit,

    -- ** AdvancedOptionsStatus
    AdvancedOptionsStatus (AdvancedOptionsStatus'),
    newAdvancedOptionsStatus,

    -- ** AdvancedSecurityOptions
    AdvancedSecurityOptions (AdvancedSecurityOptions'),
    newAdvancedSecurityOptions,

    -- ** AdvancedSecurityOptionsInput
    AdvancedSecurityOptionsInput (AdvancedSecurityOptionsInput'),
    newAdvancedSecurityOptionsInput,

    -- ** AdvancedSecurityOptionsStatus
    AdvancedSecurityOptionsStatus (AdvancedSecurityOptionsStatus'),
    newAdvancedSecurityOptionsStatus,

    -- ** AutoTune
    AutoTune (AutoTune'),
    newAutoTune,

    -- ** AutoTuneDetails
    AutoTuneDetails (AutoTuneDetails'),
    newAutoTuneDetails,

    -- ** AutoTuneMaintenanceSchedule
    AutoTuneMaintenanceSchedule (AutoTuneMaintenanceSchedule'),
    newAutoTuneMaintenanceSchedule,

    -- ** AutoTuneOptions
    AutoTuneOptions (AutoTuneOptions'),
    newAutoTuneOptions,

    -- ** AutoTuneOptionsInput
    AutoTuneOptionsInput (AutoTuneOptionsInput'),
    newAutoTuneOptionsInput,

    -- ** AutoTuneOptionsOutput
    AutoTuneOptionsOutput (AutoTuneOptionsOutput'),
    newAutoTuneOptionsOutput,

    -- ** AutoTuneOptionsStatus
    AutoTuneOptionsStatus (AutoTuneOptionsStatus'),
    newAutoTuneOptionsStatus,

    -- ** AutoTuneStatus
    AutoTuneStatus (AutoTuneStatus'),
    newAutoTuneStatus,

    -- ** CognitoOptions
    CognitoOptions (CognitoOptions'),
    newCognitoOptions,

    -- ** CognitoOptionsStatus
    CognitoOptionsStatus (CognitoOptionsStatus'),
    newCognitoOptionsStatus,

    -- ** CompatibleVersionsMap
    CompatibleVersionsMap (CompatibleVersionsMap'),
    newCompatibleVersionsMap,

    -- ** DescribePackagesFilter
    DescribePackagesFilter (DescribePackagesFilter'),
    newDescribePackagesFilter,

    -- ** DomainEndpointOptions
    DomainEndpointOptions (DomainEndpointOptions'),
    newDomainEndpointOptions,

    -- ** DomainEndpointOptionsStatus
    DomainEndpointOptionsStatus (DomainEndpointOptionsStatus'),
    newDomainEndpointOptionsStatus,

    -- ** DomainInfo
    DomainInfo (DomainInfo'),
    newDomainInfo,

    -- ** DomainInformation
    DomainInformation (DomainInformation'),
    newDomainInformation,

    -- ** DomainPackageDetails
    DomainPackageDetails (DomainPackageDetails'),
    newDomainPackageDetails,

    -- ** Duration
    Duration (Duration'),
    newDuration,

    -- ** EBSOptions
    EBSOptions (EBSOptions'),
    newEBSOptions,

    -- ** EBSOptionsStatus
    EBSOptionsStatus (EBSOptionsStatus'),
    newEBSOptionsStatus,

    -- ** ElasticsearchClusterConfig
    ElasticsearchClusterConfig (ElasticsearchClusterConfig'),
    newElasticsearchClusterConfig,

    -- ** ElasticsearchClusterConfigStatus
    ElasticsearchClusterConfigStatus (ElasticsearchClusterConfigStatus'),
    newElasticsearchClusterConfigStatus,

    -- ** ElasticsearchDomainConfig
    ElasticsearchDomainConfig (ElasticsearchDomainConfig'),
    newElasticsearchDomainConfig,

    -- ** ElasticsearchDomainStatus
    ElasticsearchDomainStatus (ElasticsearchDomainStatus'),
    newElasticsearchDomainStatus,

    -- ** ElasticsearchVersionStatus
    ElasticsearchVersionStatus (ElasticsearchVersionStatus'),
    newElasticsearchVersionStatus,

    -- ** EncryptionAtRestOptions
    EncryptionAtRestOptions (EncryptionAtRestOptions'),
    newEncryptionAtRestOptions,

    -- ** EncryptionAtRestOptionsStatus
    EncryptionAtRestOptionsStatus (EncryptionAtRestOptionsStatus'),
    newEncryptionAtRestOptionsStatus,

    -- ** ErrorDetails
    ErrorDetails (ErrorDetails'),
    newErrorDetails,

    -- ** Filter
    Filter (Filter'),
    newFilter,

    -- ** InboundCrossClusterSearchConnection
    InboundCrossClusterSearchConnection (InboundCrossClusterSearchConnection'),
    newInboundCrossClusterSearchConnection,

    -- ** InboundCrossClusterSearchConnectionStatus
    InboundCrossClusterSearchConnectionStatus (InboundCrossClusterSearchConnectionStatus'),
    newInboundCrossClusterSearchConnectionStatus,

    -- ** InstanceCountLimits
    InstanceCountLimits (InstanceCountLimits'),
    newInstanceCountLimits,

    -- ** InstanceLimits
    InstanceLimits (InstanceLimits'),
    newInstanceLimits,

    -- ** Limits
    Limits (Limits'),
    newLimits,

    -- ** LogPublishingOption
    LogPublishingOption (LogPublishingOption'),
    newLogPublishingOption,

    -- ** LogPublishingOptionsStatus
    LogPublishingOptionsStatus (LogPublishingOptionsStatus'),
    newLogPublishingOptionsStatus,

    -- ** MasterUserOptions
    MasterUserOptions (MasterUserOptions'),
    newMasterUserOptions,

    -- ** NodeToNodeEncryptionOptions
    NodeToNodeEncryptionOptions (NodeToNodeEncryptionOptions'),
    newNodeToNodeEncryptionOptions,

    -- ** NodeToNodeEncryptionOptionsStatus
    NodeToNodeEncryptionOptionsStatus (NodeToNodeEncryptionOptionsStatus'),
    newNodeToNodeEncryptionOptionsStatus,

    -- ** OptionStatus
    OptionStatus (OptionStatus'),
    newOptionStatus,

    -- ** OutboundCrossClusterSearchConnection
    OutboundCrossClusterSearchConnection (OutboundCrossClusterSearchConnection'),
    newOutboundCrossClusterSearchConnection,

    -- ** OutboundCrossClusterSearchConnectionStatus
    OutboundCrossClusterSearchConnectionStatus (OutboundCrossClusterSearchConnectionStatus'),
    newOutboundCrossClusterSearchConnectionStatus,

    -- ** PackageDetails
    PackageDetails (PackageDetails'),
    newPackageDetails,

    -- ** PackageSource
    PackageSource (PackageSource'),
    newPackageSource,

    -- ** PackageVersionHistory
    PackageVersionHistory (PackageVersionHistory'),
    newPackageVersionHistory,

    -- ** RecurringCharge
    RecurringCharge (RecurringCharge'),
    newRecurringCharge,

    -- ** ReservedElasticsearchInstance
    ReservedElasticsearchInstance (ReservedElasticsearchInstance'),
    newReservedElasticsearchInstance,

    -- ** ReservedElasticsearchInstanceOffering
    ReservedElasticsearchInstanceOffering (ReservedElasticsearchInstanceOffering'),
    newReservedElasticsearchInstanceOffering,

    -- ** SAMLIdp
    SAMLIdp (SAMLIdp'),
    newSAMLIdp,

    -- ** SAMLOptionsInput
    SAMLOptionsInput (SAMLOptionsInput'),
    newSAMLOptionsInput,

    -- ** SAMLOptionsOutput
    SAMLOptionsOutput (SAMLOptionsOutput'),
    newSAMLOptionsOutput,

    -- ** ScheduledAutoTuneDetails
    ScheduledAutoTuneDetails (ScheduledAutoTuneDetails'),
    newScheduledAutoTuneDetails,

    -- ** ServiceSoftwareOptions
    ServiceSoftwareOptions (ServiceSoftwareOptions'),
    newServiceSoftwareOptions,

    -- ** SnapshotOptions
    SnapshotOptions (SnapshotOptions'),
    newSnapshotOptions,

    -- ** SnapshotOptionsStatus
    SnapshotOptionsStatus (SnapshotOptionsStatus'),
    newSnapshotOptionsStatus,

    -- ** StorageType
    StorageType (StorageType'),
    newStorageType,

    -- ** StorageTypeLimit
    StorageTypeLimit (StorageTypeLimit'),
    newStorageTypeLimit,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** UpgradeHistory
    UpgradeHistory (UpgradeHistory'),
    newUpgradeHistory,

    -- ** UpgradeStepItem
    UpgradeStepItem (UpgradeStepItem'),
    newUpgradeStepItem,

    -- ** VPCDerivedInfo
    VPCDerivedInfo (VPCDerivedInfo'),
    newVPCDerivedInfo,

    -- ** VPCDerivedInfoStatus
    VPCDerivedInfoStatus (VPCDerivedInfoStatus'),
    newVPCDerivedInfoStatus,

    -- ** VPCOptions
    VPCOptions (VPCOptions'),
    newVPCOptions,

    -- ** ZoneAwarenessConfig
    ZoneAwarenessConfig (ZoneAwarenessConfig'),
    newZoneAwarenessConfig,
  )
where

import Network.AWS.ElasticSearch.AcceptInboundCrossClusterSearchConnection
import Network.AWS.ElasticSearch.AddTags
import Network.AWS.ElasticSearch.AssociatePackage
import Network.AWS.ElasticSearch.CancelElasticsearchServiceSoftwareUpdate
import Network.AWS.ElasticSearch.CreateElasticsearchDomain
import Network.AWS.ElasticSearch.CreateOutboundCrossClusterSearchConnection
import Network.AWS.ElasticSearch.CreatePackage
import Network.AWS.ElasticSearch.DeleteElasticsearchDomain
import Network.AWS.ElasticSearch.DeleteElasticsearchServiceRole
import Network.AWS.ElasticSearch.DeleteInboundCrossClusterSearchConnection
import Network.AWS.ElasticSearch.DeleteOutboundCrossClusterSearchConnection
import Network.AWS.ElasticSearch.DeletePackage
import Network.AWS.ElasticSearch.DescribeDomainAutoTunes
import Network.AWS.ElasticSearch.DescribeElasticsearchDomain
import Network.AWS.ElasticSearch.DescribeElasticsearchDomainConfig
import Network.AWS.ElasticSearch.DescribeElasticsearchDomains
import Network.AWS.ElasticSearch.DescribeElasticsearchInstanceTypeLimits
import Network.AWS.ElasticSearch.DescribeInboundCrossClusterSearchConnections
import Network.AWS.ElasticSearch.DescribeOutboundCrossClusterSearchConnections
import Network.AWS.ElasticSearch.DescribePackages
import Network.AWS.ElasticSearch.DescribeReservedElasticsearchInstanceOfferings
import Network.AWS.ElasticSearch.DescribeReservedElasticsearchInstances
import Network.AWS.ElasticSearch.DissociatePackage
import Network.AWS.ElasticSearch.GetCompatibleElasticsearchVersions
import Network.AWS.ElasticSearch.GetPackageVersionHistory
import Network.AWS.ElasticSearch.GetUpgradeHistory
import Network.AWS.ElasticSearch.GetUpgradeStatus
import Network.AWS.ElasticSearch.Lens
import Network.AWS.ElasticSearch.ListDomainNames
import Network.AWS.ElasticSearch.ListDomainsForPackage
import Network.AWS.ElasticSearch.ListElasticsearchInstanceTypes
import Network.AWS.ElasticSearch.ListElasticsearchVersions
import Network.AWS.ElasticSearch.ListPackagesForDomain
import Network.AWS.ElasticSearch.ListTags
import Network.AWS.ElasticSearch.PurchaseReservedElasticsearchInstanceOffering
import Network.AWS.ElasticSearch.RejectInboundCrossClusterSearchConnection
import Network.AWS.ElasticSearch.RemoveTags
import Network.AWS.ElasticSearch.StartElasticsearchServiceSoftwareUpdate
import Network.AWS.ElasticSearch.Types
import Network.AWS.ElasticSearch.UpdateElasticsearchDomainConfig
import Network.AWS.ElasticSearch.UpdatePackage
import Network.AWS.ElasticSearch.UpgradeElasticsearchDomain
import Network.AWS.ElasticSearch.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'ElasticSearch'.

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
