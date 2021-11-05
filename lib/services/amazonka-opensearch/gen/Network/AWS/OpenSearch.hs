{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.OpenSearch
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2021-01-01@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon OpenSearch Configuration Service
--
-- Use the Amazon OpenSearch configuration API to create, configure, and
-- manage Amazon OpenSearch Service domains.
--
-- For sample code that uses the configuration API, see the
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/opensearch-configuration-samples.html Amazon OpenSearch Service Developer Guide>.
-- The guide also contains
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/request-signing.html sample code for sending signed HTTP requests to the OpenSearch APIs>.
--
-- The endpoint for configuration service requests is region-specific:
-- es./region/.amazonaws.com. For example, es.us-east-1.amazonaws.com. For
-- a current list of supported regions and endpoints, see
-- <http://docs.aws.amazon.com/general/latest/gr/rande.html#service-regions Regions and Endpoints>.
module Network.AWS.OpenSearch
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ValidationException
    _ValidationException,

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ResourceAlreadyExistsException
    _ResourceAlreadyExistsException,

    -- ** ConflictException
    _ConflictException,

    -- ** BaseException
    _BaseException,

    -- ** DisabledOperationException
    _DisabledOperationException,

    -- ** InternalException
    _InternalException,

    -- ** InvalidTypeException
    _InvalidTypeException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** InvalidPaginationTokenException
    _InvalidPaginationTokenException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** RejectInboundConnection
    RejectInboundConnection (RejectInboundConnection'),
    newRejectInboundConnection,
    RejectInboundConnectionResponse (RejectInboundConnectionResponse'),
    newRejectInboundConnectionResponse,

    -- ** DescribeOutboundConnections
    DescribeOutboundConnections (DescribeOutboundConnections'),
    newDescribeOutboundConnections,
    DescribeOutboundConnectionsResponse (DescribeOutboundConnectionsResponse'),
    newDescribeOutboundConnectionsResponse,

    -- ** RemoveTags
    RemoveTags (RemoveTags'),
    newRemoveTags,
    RemoveTagsResponse (RemoveTagsResponse'),
    newRemoveTagsResponse,

    -- ** DescribeInstanceTypeLimits
    DescribeInstanceTypeLimits (DescribeInstanceTypeLimits'),
    newDescribeInstanceTypeLimits,
    DescribeInstanceTypeLimitsResponse (DescribeInstanceTypeLimitsResponse'),
    newDescribeInstanceTypeLimitsResponse,

    -- ** DescribeInboundConnections
    DescribeInboundConnections (DescribeInboundConnections'),
    newDescribeInboundConnections,
    DescribeInboundConnectionsResponse (DescribeInboundConnectionsResponse'),
    newDescribeInboundConnectionsResponse,

    -- ** CancelServiceSoftwareUpdate
    CancelServiceSoftwareUpdate (CancelServiceSoftwareUpdate'),
    newCancelServiceSoftwareUpdate,
    CancelServiceSoftwareUpdateResponse (CancelServiceSoftwareUpdateResponse'),
    newCancelServiceSoftwareUpdateResponse,

    -- ** ListDomainsForPackage
    ListDomainsForPackage (ListDomainsForPackage'),
    newListDomainsForPackage,
    ListDomainsForPackageResponse (ListDomainsForPackageResponse'),
    newListDomainsForPackageResponse,

    -- ** ListPackagesForDomain
    ListPackagesForDomain (ListPackagesForDomain'),
    newListPackagesForDomain,
    ListPackagesForDomainResponse (ListPackagesForDomainResponse'),
    newListPackagesForDomainResponse,

    -- ** UpgradeDomain
    UpgradeDomain (UpgradeDomain'),
    newUpgradeDomain,
    UpgradeDomainResponse (UpgradeDomainResponse'),
    newUpgradeDomainResponse,

    -- ** DescribeDomainAutoTunes
    DescribeDomainAutoTunes (DescribeDomainAutoTunes'),
    newDescribeDomainAutoTunes,
    DescribeDomainAutoTunesResponse (DescribeDomainAutoTunesResponse'),
    newDescribeDomainAutoTunesResponse,

    -- ** DescribeReservedInstances
    DescribeReservedInstances (DescribeReservedInstances'),
    newDescribeReservedInstances,
    DescribeReservedInstancesResponse (DescribeReservedInstancesResponse'),
    newDescribeReservedInstancesResponse,

    -- ** StartServiceSoftwareUpdate
    StartServiceSoftwareUpdate (StartServiceSoftwareUpdate'),
    newStartServiceSoftwareUpdate,
    StartServiceSoftwareUpdateResponse (StartServiceSoftwareUpdateResponse'),
    newStartServiceSoftwareUpdateResponse,

    -- ** DeleteOutboundConnection
    DeleteOutboundConnection (DeleteOutboundConnection'),
    newDeleteOutboundConnection,
    DeleteOutboundConnectionResponse (DeleteOutboundConnectionResponse'),
    newDeleteOutboundConnectionResponse,

    -- ** ListVersions
    ListVersions (ListVersions'),
    newListVersions,
    ListVersionsResponse (ListVersionsResponse'),
    newListVersionsResponse,

    -- ** DescribeReservedInstanceOfferings
    DescribeReservedInstanceOfferings (DescribeReservedInstanceOfferings'),
    newDescribeReservedInstanceOfferings,
    DescribeReservedInstanceOfferingsResponse (DescribeReservedInstanceOfferingsResponse'),
    newDescribeReservedInstanceOfferingsResponse,

    -- ** ListDomainNames
    ListDomainNames (ListDomainNames'),
    newListDomainNames,
    ListDomainNamesResponse (ListDomainNamesResponse'),
    newListDomainNamesResponse,

    -- ** PurchaseReservedInstanceOffering
    PurchaseReservedInstanceOffering (PurchaseReservedInstanceOffering'),
    newPurchaseReservedInstanceOffering,
    PurchaseReservedInstanceOfferingResponse (PurchaseReservedInstanceOfferingResponse'),
    newPurchaseReservedInstanceOfferingResponse,

    -- ** DescribeDomains
    DescribeDomains (DescribeDomains'),
    newDescribeDomains,
    DescribeDomainsResponse (DescribeDomainsResponse'),
    newDescribeDomainsResponse,

    -- ** AssociatePackage
    AssociatePackage (AssociatePackage'),
    newAssociatePackage,
    AssociatePackageResponse (AssociatePackageResponse'),
    newAssociatePackageResponse,

    -- ** ListInstanceTypeDetails
    ListInstanceTypeDetails (ListInstanceTypeDetails'),
    newListInstanceTypeDetails,
    ListInstanceTypeDetailsResponse (ListInstanceTypeDetailsResponse'),
    newListInstanceTypeDetailsResponse,

    -- ** GetPackageVersionHistory
    GetPackageVersionHistory (GetPackageVersionHistory'),
    newGetPackageVersionHistory,
    GetPackageVersionHistoryResponse (GetPackageVersionHistoryResponse'),
    newGetPackageVersionHistoryResponse,

    -- ** GetUpgradeHistory
    GetUpgradeHistory (GetUpgradeHistory'),
    newGetUpgradeHistory,
    GetUpgradeHistoryResponse (GetUpgradeHistoryResponse'),
    newGetUpgradeHistoryResponse,

    -- ** DescribePackages
    DescribePackages (DescribePackages'),
    newDescribePackages,
    DescribePackagesResponse (DescribePackagesResponse'),
    newDescribePackagesResponse,

    -- ** CreateDomain
    CreateDomain (CreateDomain'),
    newCreateDomain,
    CreateDomainResponse (CreateDomainResponse'),
    newCreateDomainResponse,

    -- ** DescribeDomainConfig
    DescribeDomainConfig (DescribeDomainConfig'),
    newDescribeDomainConfig,
    DescribeDomainConfigResponse (DescribeDomainConfigResponse'),
    newDescribeDomainConfigResponse,

    -- ** GetUpgradeStatus
    GetUpgradeStatus (GetUpgradeStatus'),
    newGetUpgradeStatus,
    GetUpgradeStatusResponse (GetUpgradeStatusResponse'),
    newGetUpgradeStatusResponse,

    -- ** DeleteInboundConnection
    DeleteInboundConnection (DeleteInboundConnection'),
    newDeleteInboundConnection,
    DeleteInboundConnectionResponse (DeleteInboundConnectionResponse'),
    newDeleteInboundConnectionResponse,

    -- ** DissociatePackage
    DissociatePackage (DissociatePackage'),
    newDissociatePackage,
    DissociatePackageResponse (DissociatePackageResponse'),
    newDissociatePackageResponse,

    -- ** DescribeDomain
    DescribeDomain (DescribeDomain'),
    newDescribeDomain,
    DescribeDomainResponse (DescribeDomainResponse'),
    newDescribeDomainResponse,

    -- ** AddTags
    AddTags (AddTags'),
    newAddTags,
    AddTagsResponse (AddTagsResponse'),
    newAddTagsResponse,

    -- ** AcceptInboundConnection
    AcceptInboundConnection (AcceptInboundConnection'),
    newAcceptInboundConnection,
    AcceptInboundConnectionResponse (AcceptInboundConnectionResponse'),
    newAcceptInboundConnectionResponse,

    -- ** UpdateDomainConfig
    UpdateDomainConfig (UpdateDomainConfig'),
    newUpdateDomainConfig,
    UpdateDomainConfigResponse (UpdateDomainConfigResponse'),
    newUpdateDomainConfigResponse,

    -- ** ListTags
    ListTags (ListTags'),
    newListTags,
    ListTagsResponse (ListTagsResponse'),
    newListTagsResponse,

    -- ** DeletePackage
    DeletePackage (DeletePackage'),
    newDeletePackage,
    DeletePackageResponse (DeletePackageResponse'),
    newDeletePackageResponse,

    -- ** UpdatePackage
    UpdatePackage (UpdatePackage'),
    newUpdatePackage,
    UpdatePackageResponse (UpdatePackageResponse'),
    newUpdatePackageResponse,

    -- ** CreateOutboundConnection
    CreateOutboundConnection (CreateOutboundConnection'),
    newCreateOutboundConnection,
    CreateOutboundConnectionResponse (CreateOutboundConnectionResponse'),
    newCreateOutboundConnectionResponse,

    -- ** CreatePackage
    CreatePackage (CreatePackage'),
    newCreatePackage,
    CreatePackageResponse (CreatePackageResponse'),
    newCreatePackageResponse,

    -- ** DeleteDomain
    DeleteDomain (DeleteDomain'),
    newDeleteDomain,
    DeleteDomainResponse (DeleteDomainResponse'),
    newDeleteDomainResponse,

    -- ** GetCompatibleVersions
    GetCompatibleVersions (GetCompatibleVersions'),
    newGetCompatibleVersions,
    GetCompatibleVersionsResponse (GetCompatibleVersionsResponse'),
    newGetCompatibleVersionsResponse,

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

    -- ** EngineType
    EngineType (..),

    -- ** InboundConnectionStatusCode
    InboundConnectionStatusCode (..),

    -- ** LogType
    LogType (..),

    -- ** OpenSearchPartitionInstanceType
    OpenSearchPartitionInstanceType (..),

    -- ** OpenSearchWarmPartitionInstanceType
    OpenSearchWarmPartitionInstanceType (..),

    -- ** OptionState
    OptionState (..),

    -- ** OutboundConnectionStatusCode
    OutboundConnectionStatusCode (..),

    -- ** PackageStatus
    PackageStatus (..),

    -- ** PackageType
    PackageType (..),

    -- ** ReservedInstancePaymentOption
    ReservedInstancePaymentOption (..),

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

    -- ** AWSDomainInformation
    AWSDomainInformation (AWSDomainInformation'),
    newAWSDomainInformation,

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

    -- ** ClusterConfig
    ClusterConfig (ClusterConfig'),
    newClusterConfig,

    -- ** ClusterConfigStatus
    ClusterConfigStatus (ClusterConfigStatus'),
    newClusterConfigStatus,

    -- ** CognitoOptions
    CognitoOptions (CognitoOptions'),
    newCognitoOptions,

    -- ** CognitoOptionsStatus
    CognitoOptionsStatus (CognitoOptionsStatus'),
    newCognitoOptionsStatus,

    -- ** ColdStorageOptions
    ColdStorageOptions (ColdStorageOptions'),
    newColdStorageOptions,

    -- ** CompatibleVersionsMap
    CompatibleVersionsMap (CompatibleVersionsMap'),
    newCompatibleVersionsMap,

    -- ** DescribePackagesFilter
    DescribePackagesFilter (DescribePackagesFilter'),
    newDescribePackagesFilter,

    -- ** DomainConfig
    DomainConfig (DomainConfig'),
    newDomainConfig,

    -- ** DomainEndpointOptions
    DomainEndpointOptions (DomainEndpointOptions'),
    newDomainEndpointOptions,

    -- ** DomainEndpointOptionsStatus
    DomainEndpointOptionsStatus (DomainEndpointOptionsStatus'),
    newDomainEndpointOptionsStatus,

    -- ** DomainInfo
    DomainInfo (DomainInfo'),
    newDomainInfo,

    -- ** DomainInformationContainer
    DomainInformationContainer (DomainInformationContainer'),
    newDomainInformationContainer,

    -- ** DomainPackageDetails
    DomainPackageDetails (DomainPackageDetails'),
    newDomainPackageDetails,

    -- ** DomainStatus
    DomainStatus (DomainStatus'),
    newDomainStatus,

    -- ** Duration
    Duration (Duration'),
    newDuration,

    -- ** EBSOptions
    EBSOptions (EBSOptions'),
    newEBSOptions,

    -- ** EBSOptionsStatus
    EBSOptionsStatus (EBSOptionsStatus'),
    newEBSOptionsStatus,

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

    -- ** InboundConnection
    InboundConnection (InboundConnection'),
    newInboundConnection,

    -- ** InboundConnectionStatus
    InboundConnectionStatus (InboundConnectionStatus'),
    newInboundConnectionStatus,

    -- ** InstanceCountLimits
    InstanceCountLimits (InstanceCountLimits'),
    newInstanceCountLimits,

    -- ** InstanceLimits
    InstanceLimits (InstanceLimits'),
    newInstanceLimits,

    -- ** InstanceTypeDetails
    InstanceTypeDetails (InstanceTypeDetails'),
    newInstanceTypeDetails,

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

    -- ** OutboundConnection
    OutboundConnection (OutboundConnection'),
    newOutboundConnection,

    -- ** OutboundConnectionStatus
    OutboundConnectionStatus (OutboundConnectionStatus'),
    newOutboundConnectionStatus,

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

    -- ** ReservedInstance
    ReservedInstance (ReservedInstance'),
    newReservedInstance,

    -- ** ReservedInstanceOffering
    ReservedInstanceOffering (ReservedInstanceOffering'),
    newReservedInstanceOffering,

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

    -- ** VersionStatus
    VersionStatus (VersionStatus'),
    newVersionStatus,

    -- ** ZoneAwarenessConfig
    ZoneAwarenessConfig (ZoneAwarenessConfig'),
    newZoneAwarenessConfig,
  )
where

import Network.AWS.OpenSearch.AcceptInboundConnection
import Network.AWS.OpenSearch.AddTags
import Network.AWS.OpenSearch.AssociatePackage
import Network.AWS.OpenSearch.CancelServiceSoftwareUpdate
import Network.AWS.OpenSearch.CreateDomain
import Network.AWS.OpenSearch.CreateOutboundConnection
import Network.AWS.OpenSearch.CreatePackage
import Network.AWS.OpenSearch.DeleteDomain
import Network.AWS.OpenSearch.DeleteInboundConnection
import Network.AWS.OpenSearch.DeleteOutboundConnection
import Network.AWS.OpenSearch.DeletePackage
import Network.AWS.OpenSearch.DescribeDomain
import Network.AWS.OpenSearch.DescribeDomainAutoTunes
import Network.AWS.OpenSearch.DescribeDomainConfig
import Network.AWS.OpenSearch.DescribeDomains
import Network.AWS.OpenSearch.DescribeInboundConnections
import Network.AWS.OpenSearch.DescribeInstanceTypeLimits
import Network.AWS.OpenSearch.DescribeOutboundConnections
import Network.AWS.OpenSearch.DescribePackages
import Network.AWS.OpenSearch.DescribeReservedInstanceOfferings
import Network.AWS.OpenSearch.DescribeReservedInstances
import Network.AWS.OpenSearch.DissociatePackage
import Network.AWS.OpenSearch.GetCompatibleVersions
import Network.AWS.OpenSearch.GetPackageVersionHistory
import Network.AWS.OpenSearch.GetUpgradeHistory
import Network.AWS.OpenSearch.GetUpgradeStatus
import Network.AWS.OpenSearch.Lens
import Network.AWS.OpenSearch.ListDomainNames
import Network.AWS.OpenSearch.ListDomainsForPackage
import Network.AWS.OpenSearch.ListInstanceTypeDetails
import Network.AWS.OpenSearch.ListPackagesForDomain
import Network.AWS.OpenSearch.ListTags
import Network.AWS.OpenSearch.ListVersions
import Network.AWS.OpenSearch.PurchaseReservedInstanceOffering
import Network.AWS.OpenSearch.RejectInboundConnection
import Network.AWS.OpenSearch.RemoveTags
import Network.AWS.OpenSearch.StartServiceSoftwareUpdate
import Network.AWS.OpenSearch.Types
import Network.AWS.OpenSearch.UpdateDomainConfig
import Network.AWS.OpenSearch.UpdatePackage
import Network.AWS.OpenSearch.UpgradeDomain
import Network.AWS.OpenSearch.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'OpenSearch'.

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
