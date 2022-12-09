{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.ElasticSearch
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2015-01-01@ of the AWS service descriptions, licensed under Apache 2.0.
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
module Amazonka.ElasticSearch
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** BaseException
    _BaseException,

    -- ** ConflictException
    _ConflictException,

    -- ** DisabledOperationException
    _DisabledOperationException,

    -- ** InternalException
    _InternalException,

    -- ** InvalidPaginationTokenException
    _InvalidPaginationTokenException,

    -- ** InvalidTypeException
    _InvalidTypeException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** ResourceAlreadyExistsException
    _ResourceAlreadyExistsException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AcceptInboundCrossClusterSearchConnection
    AcceptInboundCrossClusterSearchConnection (AcceptInboundCrossClusterSearchConnection'),
    newAcceptInboundCrossClusterSearchConnection,
    AcceptInboundCrossClusterSearchConnectionResponse (AcceptInboundCrossClusterSearchConnectionResponse'),
    newAcceptInboundCrossClusterSearchConnectionResponse,

    -- ** AddTags
    AddTags (AddTags'),
    newAddTags,
    AddTagsResponse (AddTagsResponse'),
    newAddTagsResponse,

    -- ** AssociatePackage
    AssociatePackage (AssociatePackage'),
    newAssociatePackage,
    AssociatePackageResponse (AssociatePackageResponse'),
    newAssociatePackageResponse,

    -- ** AuthorizeVpcEndpointAccess
    AuthorizeVpcEndpointAccess (AuthorizeVpcEndpointAccess'),
    newAuthorizeVpcEndpointAccess,
    AuthorizeVpcEndpointAccessResponse (AuthorizeVpcEndpointAccessResponse'),
    newAuthorizeVpcEndpointAccessResponse,

    -- ** CancelElasticsearchServiceSoftwareUpdate
    CancelElasticsearchServiceSoftwareUpdate (CancelElasticsearchServiceSoftwareUpdate'),
    newCancelElasticsearchServiceSoftwareUpdate,
    CancelElasticsearchServiceSoftwareUpdateResponse (CancelElasticsearchServiceSoftwareUpdateResponse'),
    newCancelElasticsearchServiceSoftwareUpdateResponse,

    -- ** CreateElasticsearchDomain
    CreateElasticsearchDomain (CreateElasticsearchDomain'),
    newCreateElasticsearchDomain,
    CreateElasticsearchDomainResponse (CreateElasticsearchDomainResponse'),
    newCreateElasticsearchDomainResponse,

    -- ** CreateOutboundCrossClusterSearchConnection
    CreateOutboundCrossClusterSearchConnection (CreateOutboundCrossClusterSearchConnection'),
    newCreateOutboundCrossClusterSearchConnection,
    CreateOutboundCrossClusterSearchConnectionResponse (CreateOutboundCrossClusterSearchConnectionResponse'),
    newCreateOutboundCrossClusterSearchConnectionResponse,

    -- ** CreatePackage
    CreatePackage (CreatePackage'),
    newCreatePackage,
    CreatePackageResponse (CreatePackageResponse'),
    newCreatePackageResponse,

    -- ** CreateVpcEndpoint
    CreateVpcEndpoint (CreateVpcEndpoint'),
    newCreateVpcEndpoint,
    CreateVpcEndpointResponse (CreateVpcEndpointResponse'),
    newCreateVpcEndpointResponse,

    -- ** DeleteElasticsearchDomain
    DeleteElasticsearchDomain (DeleteElasticsearchDomain'),
    newDeleteElasticsearchDomain,
    DeleteElasticsearchDomainResponse (DeleteElasticsearchDomainResponse'),
    newDeleteElasticsearchDomainResponse,

    -- ** DeleteElasticsearchServiceRole
    DeleteElasticsearchServiceRole (DeleteElasticsearchServiceRole'),
    newDeleteElasticsearchServiceRole,
    DeleteElasticsearchServiceRoleResponse (DeleteElasticsearchServiceRoleResponse'),
    newDeleteElasticsearchServiceRoleResponse,

    -- ** DeleteInboundCrossClusterSearchConnection
    DeleteInboundCrossClusterSearchConnection (DeleteInboundCrossClusterSearchConnection'),
    newDeleteInboundCrossClusterSearchConnection,
    DeleteInboundCrossClusterSearchConnectionResponse (DeleteInboundCrossClusterSearchConnectionResponse'),
    newDeleteInboundCrossClusterSearchConnectionResponse,

    -- ** DeleteOutboundCrossClusterSearchConnection
    DeleteOutboundCrossClusterSearchConnection (DeleteOutboundCrossClusterSearchConnection'),
    newDeleteOutboundCrossClusterSearchConnection,
    DeleteOutboundCrossClusterSearchConnectionResponse (DeleteOutboundCrossClusterSearchConnectionResponse'),
    newDeleteOutboundCrossClusterSearchConnectionResponse,

    -- ** DeletePackage
    DeletePackage (DeletePackage'),
    newDeletePackage,
    DeletePackageResponse (DeletePackageResponse'),
    newDeletePackageResponse,

    -- ** DeleteVpcEndpoint
    DeleteVpcEndpoint (DeleteVpcEndpoint'),
    newDeleteVpcEndpoint,
    DeleteVpcEndpointResponse (DeleteVpcEndpointResponse'),
    newDeleteVpcEndpointResponse,

    -- ** DescribeDomainAutoTunes
    DescribeDomainAutoTunes (DescribeDomainAutoTunes'),
    newDescribeDomainAutoTunes,
    DescribeDomainAutoTunesResponse (DescribeDomainAutoTunesResponse'),
    newDescribeDomainAutoTunesResponse,

    -- ** DescribeDomainChangeProgress
    DescribeDomainChangeProgress (DescribeDomainChangeProgress'),
    newDescribeDomainChangeProgress,
    DescribeDomainChangeProgressResponse (DescribeDomainChangeProgressResponse'),
    newDescribeDomainChangeProgressResponse,

    -- ** DescribeElasticsearchDomain
    DescribeElasticsearchDomain (DescribeElasticsearchDomain'),
    newDescribeElasticsearchDomain,
    DescribeElasticsearchDomainResponse (DescribeElasticsearchDomainResponse'),
    newDescribeElasticsearchDomainResponse,

    -- ** DescribeElasticsearchDomainConfig
    DescribeElasticsearchDomainConfig (DescribeElasticsearchDomainConfig'),
    newDescribeElasticsearchDomainConfig,
    DescribeElasticsearchDomainConfigResponse (DescribeElasticsearchDomainConfigResponse'),
    newDescribeElasticsearchDomainConfigResponse,

    -- ** DescribeElasticsearchDomains
    DescribeElasticsearchDomains (DescribeElasticsearchDomains'),
    newDescribeElasticsearchDomains,
    DescribeElasticsearchDomainsResponse (DescribeElasticsearchDomainsResponse'),
    newDescribeElasticsearchDomainsResponse,

    -- ** DescribeElasticsearchInstanceTypeLimits
    DescribeElasticsearchInstanceTypeLimits (DescribeElasticsearchInstanceTypeLimits'),
    newDescribeElasticsearchInstanceTypeLimits,
    DescribeElasticsearchInstanceTypeLimitsResponse (DescribeElasticsearchInstanceTypeLimitsResponse'),
    newDescribeElasticsearchInstanceTypeLimitsResponse,

    -- ** DescribeInboundCrossClusterSearchConnections
    DescribeInboundCrossClusterSearchConnections (DescribeInboundCrossClusterSearchConnections'),
    newDescribeInboundCrossClusterSearchConnections,
    DescribeInboundCrossClusterSearchConnectionsResponse (DescribeInboundCrossClusterSearchConnectionsResponse'),
    newDescribeInboundCrossClusterSearchConnectionsResponse,

    -- ** DescribeOutboundCrossClusterSearchConnections
    DescribeOutboundCrossClusterSearchConnections (DescribeOutboundCrossClusterSearchConnections'),
    newDescribeOutboundCrossClusterSearchConnections,
    DescribeOutboundCrossClusterSearchConnectionsResponse (DescribeOutboundCrossClusterSearchConnectionsResponse'),
    newDescribeOutboundCrossClusterSearchConnectionsResponse,

    -- ** DescribePackages
    DescribePackages (DescribePackages'),
    newDescribePackages,
    DescribePackagesResponse (DescribePackagesResponse'),
    newDescribePackagesResponse,

    -- ** DescribeReservedElasticsearchInstanceOfferings (Paginated)
    DescribeReservedElasticsearchInstanceOfferings (DescribeReservedElasticsearchInstanceOfferings'),
    newDescribeReservedElasticsearchInstanceOfferings,
    DescribeReservedElasticsearchInstanceOfferingsResponse (DescribeReservedElasticsearchInstanceOfferingsResponse'),
    newDescribeReservedElasticsearchInstanceOfferingsResponse,

    -- ** DescribeReservedElasticsearchInstances (Paginated)
    DescribeReservedElasticsearchInstances (DescribeReservedElasticsearchInstances'),
    newDescribeReservedElasticsearchInstances,
    DescribeReservedElasticsearchInstancesResponse (DescribeReservedElasticsearchInstancesResponse'),
    newDescribeReservedElasticsearchInstancesResponse,

    -- ** DescribeVpcEndpoints
    DescribeVpcEndpoints (DescribeVpcEndpoints'),
    newDescribeVpcEndpoints,
    DescribeVpcEndpointsResponse (DescribeVpcEndpointsResponse'),
    newDescribeVpcEndpointsResponse,

    -- ** DissociatePackage
    DissociatePackage (DissociatePackage'),
    newDissociatePackage,
    DissociatePackageResponse (DissociatePackageResponse'),
    newDissociatePackageResponse,

    -- ** GetCompatibleElasticsearchVersions
    GetCompatibleElasticsearchVersions (GetCompatibleElasticsearchVersions'),
    newGetCompatibleElasticsearchVersions,
    GetCompatibleElasticsearchVersionsResponse (GetCompatibleElasticsearchVersionsResponse'),
    newGetCompatibleElasticsearchVersionsResponse,

    -- ** GetPackageVersionHistory
    GetPackageVersionHistory (GetPackageVersionHistory'),
    newGetPackageVersionHistory,
    GetPackageVersionHistoryResponse (GetPackageVersionHistoryResponse'),
    newGetPackageVersionHistoryResponse,

    -- ** GetUpgradeHistory (Paginated)
    GetUpgradeHistory (GetUpgradeHistory'),
    newGetUpgradeHistory,
    GetUpgradeHistoryResponse (GetUpgradeHistoryResponse'),
    newGetUpgradeHistoryResponse,

    -- ** GetUpgradeStatus
    GetUpgradeStatus (GetUpgradeStatus'),
    newGetUpgradeStatus,
    GetUpgradeStatusResponse (GetUpgradeStatusResponse'),
    newGetUpgradeStatusResponse,

    -- ** ListDomainNames
    ListDomainNames (ListDomainNames'),
    newListDomainNames,
    ListDomainNamesResponse (ListDomainNamesResponse'),
    newListDomainNamesResponse,

    -- ** ListDomainsForPackage
    ListDomainsForPackage (ListDomainsForPackage'),
    newListDomainsForPackage,
    ListDomainsForPackageResponse (ListDomainsForPackageResponse'),
    newListDomainsForPackageResponse,

    -- ** ListElasticsearchInstanceTypes (Paginated)
    ListElasticsearchInstanceTypes (ListElasticsearchInstanceTypes'),
    newListElasticsearchInstanceTypes,
    ListElasticsearchInstanceTypesResponse (ListElasticsearchInstanceTypesResponse'),
    newListElasticsearchInstanceTypesResponse,

    -- ** ListElasticsearchVersions (Paginated)
    ListElasticsearchVersions (ListElasticsearchVersions'),
    newListElasticsearchVersions,
    ListElasticsearchVersionsResponse (ListElasticsearchVersionsResponse'),
    newListElasticsearchVersionsResponse,

    -- ** ListPackagesForDomain
    ListPackagesForDomain (ListPackagesForDomain'),
    newListPackagesForDomain,
    ListPackagesForDomainResponse (ListPackagesForDomainResponse'),
    newListPackagesForDomainResponse,

    -- ** ListTags
    ListTags (ListTags'),
    newListTags,
    ListTagsResponse (ListTagsResponse'),
    newListTagsResponse,

    -- ** ListVpcEndpointAccess
    ListVpcEndpointAccess (ListVpcEndpointAccess'),
    newListVpcEndpointAccess,
    ListVpcEndpointAccessResponse (ListVpcEndpointAccessResponse'),
    newListVpcEndpointAccessResponse,

    -- ** ListVpcEndpoints
    ListVpcEndpoints (ListVpcEndpoints'),
    newListVpcEndpoints,
    ListVpcEndpointsResponse (ListVpcEndpointsResponse'),
    newListVpcEndpointsResponse,

    -- ** ListVpcEndpointsForDomain
    ListVpcEndpointsForDomain (ListVpcEndpointsForDomain'),
    newListVpcEndpointsForDomain,
    ListVpcEndpointsForDomainResponse (ListVpcEndpointsForDomainResponse'),
    newListVpcEndpointsForDomainResponse,

    -- ** PurchaseReservedElasticsearchInstanceOffering
    PurchaseReservedElasticsearchInstanceOffering (PurchaseReservedElasticsearchInstanceOffering'),
    newPurchaseReservedElasticsearchInstanceOffering,
    PurchaseReservedElasticsearchInstanceOfferingResponse (PurchaseReservedElasticsearchInstanceOfferingResponse'),
    newPurchaseReservedElasticsearchInstanceOfferingResponse,

    -- ** RejectInboundCrossClusterSearchConnection
    RejectInboundCrossClusterSearchConnection (RejectInboundCrossClusterSearchConnection'),
    newRejectInboundCrossClusterSearchConnection,
    RejectInboundCrossClusterSearchConnectionResponse (RejectInboundCrossClusterSearchConnectionResponse'),
    newRejectInboundCrossClusterSearchConnectionResponse,

    -- ** RemoveTags
    RemoveTags (RemoveTags'),
    newRemoveTags,
    RemoveTagsResponse (RemoveTagsResponse'),
    newRemoveTagsResponse,

    -- ** RevokeVpcEndpointAccess
    RevokeVpcEndpointAccess (RevokeVpcEndpointAccess'),
    newRevokeVpcEndpointAccess,
    RevokeVpcEndpointAccessResponse (RevokeVpcEndpointAccessResponse'),
    newRevokeVpcEndpointAccessResponse,

    -- ** StartElasticsearchServiceSoftwareUpdate
    StartElasticsearchServiceSoftwareUpdate (StartElasticsearchServiceSoftwareUpdate'),
    newStartElasticsearchServiceSoftwareUpdate,
    StartElasticsearchServiceSoftwareUpdateResponse (StartElasticsearchServiceSoftwareUpdateResponse'),
    newStartElasticsearchServiceSoftwareUpdateResponse,

    -- ** UpdateElasticsearchDomainConfig
    UpdateElasticsearchDomainConfig (UpdateElasticsearchDomainConfig'),
    newUpdateElasticsearchDomainConfig,
    UpdateElasticsearchDomainConfigResponse (UpdateElasticsearchDomainConfigResponse'),
    newUpdateElasticsearchDomainConfigResponse,

    -- ** UpdatePackage
    UpdatePackage (UpdatePackage'),
    newUpdatePackage,
    UpdatePackageResponse (UpdatePackageResponse'),
    newUpdatePackageResponse,

    -- ** UpdateVpcEndpoint
    UpdateVpcEndpoint (UpdateVpcEndpoint'),
    newUpdateVpcEndpoint,
    UpdateVpcEndpointResponse (UpdateVpcEndpointResponse'),
    newUpdateVpcEndpointResponse,

    -- ** UpgradeElasticsearchDomain
    UpgradeElasticsearchDomain (UpgradeElasticsearchDomain'),
    newUpgradeElasticsearchDomain,
    UpgradeElasticsearchDomainResponse (UpgradeElasticsearchDomainResponse'),
    newUpgradeElasticsearchDomainResponse,

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

    -- ** EngineType
    EngineType (..),

    -- ** InboundCrossClusterSearchConnectionStatusCode
    InboundCrossClusterSearchConnectionStatusCode (..),

    -- ** LogType
    LogType (..),

    -- ** OptionState
    OptionState (..),

    -- ** OutboundCrossClusterSearchConnectionStatusCode
    OutboundCrossClusterSearchConnectionStatusCode (..),

    -- ** OverallChangeStatus
    OverallChangeStatus (..),

    -- ** PackageStatus
    PackageStatus (..),

    -- ** PackageType
    PackageType (..),

    -- ** PrincipalType
    PrincipalType (..),

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

    -- ** VpcEndpointErrorCode
    VpcEndpointErrorCode (..),

    -- ** VpcEndpointStatus
    VpcEndpointStatus (..),

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

    -- ** AuthorizedPrincipal
    AuthorizedPrincipal (AuthorizedPrincipal'),
    newAuthorizedPrincipal,

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

    -- ** ChangeProgressDetails
    ChangeProgressDetails (ChangeProgressDetails'),
    newChangeProgressDetails,

    -- ** ChangeProgressStage
    ChangeProgressStage (ChangeProgressStage'),
    newChangeProgressStage,

    -- ** ChangeProgressStatusDetails
    ChangeProgressStatusDetails (ChangeProgressStatusDetails'),
    newChangeProgressStatusDetails,

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

    -- ** DryRunResults
    DryRunResults (DryRunResults'),
    newDryRunResults,

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

    -- ** VpcEndpoint
    VpcEndpoint (VpcEndpoint'),
    newVpcEndpoint,

    -- ** VpcEndpointError
    VpcEndpointError (VpcEndpointError'),
    newVpcEndpointError,

    -- ** VpcEndpointSummary
    VpcEndpointSummary (VpcEndpointSummary'),
    newVpcEndpointSummary,

    -- ** ZoneAwarenessConfig
    ZoneAwarenessConfig (ZoneAwarenessConfig'),
    newZoneAwarenessConfig,
  )
where

import Amazonka.ElasticSearch.AcceptInboundCrossClusterSearchConnection
import Amazonka.ElasticSearch.AddTags
import Amazonka.ElasticSearch.AssociatePackage
import Amazonka.ElasticSearch.AuthorizeVpcEndpointAccess
import Amazonka.ElasticSearch.CancelElasticsearchServiceSoftwareUpdate
import Amazonka.ElasticSearch.CreateElasticsearchDomain
import Amazonka.ElasticSearch.CreateOutboundCrossClusterSearchConnection
import Amazonka.ElasticSearch.CreatePackage
import Amazonka.ElasticSearch.CreateVpcEndpoint
import Amazonka.ElasticSearch.DeleteElasticsearchDomain
import Amazonka.ElasticSearch.DeleteElasticsearchServiceRole
import Amazonka.ElasticSearch.DeleteInboundCrossClusterSearchConnection
import Amazonka.ElasticSearch.DeleteOutboundCrossClusterSearchConnection
import Amazonka.ElasticSearch.DeletePackage
import Amazonka.ElasticSearch.DeleteVpcEndpoint
import Amazonka.ElasticSearch.DescribeDomainAutoTunes
import Amazonka.ElasticSearch.DescribeDomainChangeProgress
import Amazonka.ElasticSearch.DescribeElasticsearchDomain
import Amazonka.ElasticSearch.DescribeElasticsearchDomainConfig
import Amazonka.ElasticSearch.DescribeElasticsearchDomains
import Amazonka.ElasticSearch.DescribeElasticsearchInstanceTypeLimits
import Amazonka.ElasticSearch.DescribeInboundCrossClusterSearchConnections
import Amazonka.ElasticSearch.DescribeOutboundCrossClusterSearchConnections
import Amazonka.ElasticSearch.DescribePackages
import Amazonka.ElasticSearch.DescribeReservedElasticsearchInstanceOfferings
import Amazonka.ElasticSearch.DescribeReservedElasticsearchInstances
import Amazonka.ElasticSearch.DescribeVpcEndpoints
import Amazonka.ElasticSearch.DissociatePackage
import Amazonka.ElasticSearch.GetCompatibleElasticsearchVersions
import Amazonka.ElasticSearch.GetPackageVersionHistory
import Amazonka.ElasticSearch.GetUpgradeHistory
import Amazonka.ElasticSearch.GetUpgradeStatus
import Amazonka.ElasticSearch.Lens
import Amazonka.ElasticSearch.ListDomainNames
import Amazonka.ElasticSearch.ListDomainsForPackage
import Amazonka.ElasticSearch.ListElasticsearchInstanceTypes
import Amazonka.ElasticSearch.ListElasticsearchVersions
import Amazonka.ElasticSearch.ListPackagesForDomain
import Amazonka.ElasticSearch.ListTags
import Amazonka.ElasticSearch.ListVpcEndpointAccess
import Amazonka.ElasticSearch.ListVpcEndpoints
import Amazonka.ElasticSearch.ListVpcEndpointsForDomain
import Amazonka.ElasticSearch.PurchaseReservedElasticsearchInstanceOffering
import Amazonka.ElasticSearch.RejectInboundCrossClusterSearchConnection
import Amazonka.ElasticSearch.RemoveTags
import Amazonka.ElasticSearch.RevokeVpcEndpointAccess
import Amazonka.ElasticSearch.StartElasticsearchServiceSoftwareUpdate
import Amazonka.ElasticSearch.Types
import Amazonka.ElasticSearch.UpdateElasticsearchDomainConfig
import Amazonka.ElasticSearch.UpdatePackage
import Amazonka.ElasticSearch.UpdateVpcEndpoint
import Amazonka.ElasticSearch.UpgradeElasticsearchDomain
import Amazonka.ElasticSearch.Waiters

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
