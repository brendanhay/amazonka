{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.OpenSearch
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2021-01-01@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Use the Amazon OpenSearch Service configuration API to create,
-- configure, and manage OpenSearch Service domains.
--
-- For sample code that uses the configuration API, see the
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/opensearch-configuration-samples.html Amazon OpenSearch Service Developer Guide>
-- . The guide also contains
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/request-signing.html sample code>
-- for sending signed HTTP requests to the OpenSearch APIs. The endpoint
-- for configuration service requests is Region specific:
-- es./region/.amazonaws.com. For example, es.us-east-1.amazonaws.com. For
-- a current list of supported Regions and endpoints, see
-- <https://docs.aws.amazon.com/general/latest/gr/rande.html#service-regions Amazon Web Services service endpoints>.
module Amazonka.OpenSearch
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

    -- ** DependencyFailureException
    _DependencyFailureException,

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

    -- ** SlotNotAvailableException
    _SlotNotAvailableException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AcceptInboundConnection
    AcceptInboundConnection (AcceptInboundConnection'),
    newAcceptInboundConnection,
    AcceptInboundConnectionResponse (AcceptInboundConnectionResponse'),
    newAcceptInboundConnectionResponse,

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

    -- ** CancelServiceSoftwareUpdate
    CancelServiceSoftwareUpdate (CancelServiceSoftwareUpdate'),
    newCancelServiceSoftwareUpdate,
    CancelServiceSoftwareUpdateResponse (CancelServiceSoftwareUpdateResponse'),
    newCancelServiceSoftwareUpdateResponse,

    -- ** CreateDomain
    CreateDomain (CreateDomain'),
    newCreateDomain,
    CreateDomainResponse (CreateDomainResponse'),
    newCreateDomainResponse,

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

    -- ** CreateVpcEndpoint
    CreateVpcEndpoint (CreateVpcEndpoint'),
    newCreateVpcEndpoint,
    CreateVpcEndpointResponse (CreateVpcEndpointResponse'),
    newCreateVpcEndpointResponse,

    -- ** DeleteDomain
    DeleteDomain (DeleteDomain'),
    newDeleteDomain,
    DeleteDomainResponse (DeleteDomainResponse'),
    newDeleteDomainResponse,

    -- ** DeleteInboundConnection
    DeleteInboundConnection (DeleteInboundConnection'),
    newDeleteInboundConnection,
    DeleteInboundConnectionResponse (DeleteInboundConnectionResponse'),
    newDeleteInboundConnectionResponse,

    -- ** DeleteOutboundConnection
    DeleteOutboundConnection (DeleteOutboundConnection'),
    newDeleteOutboundConnection,
    DeleteOutboundConnectionResponse (DeleteOutboundConnectionResponse'),
    newDeleteOutboundConnectionResponse,

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

    -- ** DescribeDomain
    DescribeDomain (DescribeDomain'),
    newDescribeDomain,
    DescribeDomainResponse (DescribeDomainResponse'),
    newDescribeDomainResponse,

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

    -- ** DescribeDomainConfig
    DescribeDomainConfig (DescribeDomainConfig'),
    newDescribeDomainConfig,
    DescribeDomainConfigResponse (DescribeDomainConfigResponse'),
    newDescribeDomainConfigResponse,

    -- ** DescribeDomainHealth
    DescribeDomainHealth (DescribeDomainHealth'),
    newDescribeDomainHealth,
    DescribeDomainHealthResponse (DescribeDomainHealthResponse'),
    newDescribeDomainHealthResponse,

    -- ** DescribeDomainNodes
    DescribeDomainNodes (DescribeDomainNodes'),
    newDescribeDomainNodes,
    DescribeDomainNodesResponse (DescribeDomainNodesResponse'),
    newDescribeDomainNodesResponse,

    -- ** DescribeDomains
    DescribeDomains (DescribeDomains'),
    newDescribeDomains,
    DescribeDomainsResponse (DescribeDomainsResponse'),
    newDescribeDomainsResponse,

    -- ** DescribeDryRunProgress
    DescribeDryRunProgress (DescribeDryRunProgress'),
    newDescribeDryRunProgress,
    DescribeDryRunProgressResponse (DescribeDryRunProgressResponse'),
    newDescribeDryRunProgressResponse,

    -- ** DescribeInboundConnections
    DescribeInboundConnections (DescribeInboundConnections'),
    newDescribeInboundConnections,
    DescribeInboundConnectionsResponse (DescribeInboundConnectionsResponse'),
    newDescribeInboundConnectionsResponse,

    -- ** DescribeInstanceTypeLimits
    DescribeInstanceTypeLimits (DescribeInstanceTypeLimits'),
    newDescribeInstanceTypeLimits,
    DescribeInstanceTypeLimitsResponse (DescribeInstanceTypeLimitsResponse'),
    newDescribeInstanceTypeLimitsResponse,

    -- ** DescribeOutboundConnections
    DescribeOutboundConnections (DescribeOutboundConnections'),
    newDescribeOutboundConnections,
    DescribeOutboundConnectionsResponse (DescribeOutboundConnectionsResponse'),
    newDescribeOutboundConnectionsResponse,

    -- ** DescribePackages
    DescribePackages (DescribePackages'),
    newDescribePackages,
    DescribePackagesResponse (DescribePackagesResponse'),
    newDescribePackagesResponse,

    -- ** DescribeReservedInstanceOfferings
    DescribeReservedInstanceOfferings (DescribeReservedInstanceOfferings'),
    newDescribeReservedInstanceOfferings,
    DescribeReservedInstanceOfferingsResponse (DescribeReservedInstanceOfferingsResponse'),
    newDescribeReservedInstanceOfferingsResponse,

    -- ** DescribeReservedInstances
    DescribeReservedInstances (DescribeReservedInstances'),
    newDescribeReservedInstances,
    DescribeReservedInstancesResponse (DescribeReservedInstancesResponse'),
    newDescribeReservedInstancesResponse,

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

    -- ** GetCompatibleVersions
    GetCompatibleVersions (GetCompatibleVersions'),
    newGetCompatibleVersions,
    GetCompatibleVersionsResponse (GetCompatibleVersionsResponse'),
    newGetCompatibleVersionsResponse,

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

    -- ** ListInstanceTypeDetails
    ListInstanceTypeDetails (ListInstanceTypeDetails'),
    newListInstanceTypeDetails,
    ListInstanceTypeDetailsResponse (ListInstanceTypeDetailsResponse'),
    newListInstanceTypeDetailsResponse,

    -- ** ListPackagesForDomain
    ListPackagesForDomain (ListPackagesForDomain'),
    newListPackagesForDomain,
    ListPackagesForDomainResponse (ListPackagesForDomainResponse'),
    newListPackagesForDomainResponse,

    -- ** ListScheduledActions
    ListScheduledActions (ListScheduledActions'),
    newListScheduledActions,
    ListScheduledActionsResponse (ListScheduledActionsResponse'),
    newListScheduledActionsResponse,

    -- ** ListTags
    ListTags (ListTags'),
    newListTags,
    ListTagsResponse (ListTagsResponse'),
    newListTagsResponse,

    -- ** ListVersions
    ListVersions (ListVersions'),
    newListVersions,
    ListVersionsResponse (ListVersionsResponse'),
    newListVersionsResponse,

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

    -- ** PurchaseReservedInstanceOffering
    PurchaseReservedInstanceOffering (PurchaseReservedInstanceOffering'),
    newPurchaseReservedInstanceOffering,
    PurchaseReservedInstanceOfferingResponse (PurchaseReservedInstanceOfferingResponse'),
    newPurchaseReservedInstanceOfferingResponse,

    -- ** RejectInboundConnection
    RejectInboundConnection (RejectInboundConnection'),
    newRejectInboundConnection,
    RejectInboundConnectionResponse (RejectInboundConnectionResponse'),
    newRejectInboundConnectionResponse,

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

    -- ** StartServiceSoftwareUpdate
    StartServiceSoftwareUpdate (StartServiceSoftwareUpdate'),
    newStartServiceSoftwareUpdate,
    StartServiceSoftwareUpdateResponse (StartServiceSoftwareUpdateResponse'),
    newStartServiceSoftwareUpdateResponse,

    -- ** UpdateDomainConfig
    UpdateDomainConfig (UpdateDomainConfig'),
    newUpdateDomainConfig,
    UpdateDomainConfigResponse (UpdateDomainConfigResponse'),
    newUpdateDomainConfigResponse,

    -- ** UpdatePackage
    UpdatePackage (UpdatePackage'),
    newUpdatePackage,
    UpdatePackageResponse (UpdatePackageResponse'),
    newUpdatePackageResponse,

    -- ** UpdateScheduledAction
    UpdateScheduledAction (UpdateScheduledAction'),
    newUpdateScheduledAction,
    UpdateScheduledActionResponse (UpdateScheduledActionResponse'),
    newUpdateScheduledActionResponse,

    -- ** UpdateVpcEndpoint
    UpdateVpcEndpoint (UpdateVpcEndpoint'),
    newUpdateVpcEndpoint,
    UpdateVpcEndpointResponse (UpdateVpcEndpointResponse'),
    newUpdateVpcEndpointResponse,

    -- ** UpgradeDomain
    UpgradeDomain (UpgradeDomain'),
    newUpgradeDomain,
    UpgradeDomainResponse (UpgradeDomainResponse'),
    newUpgradeDomainResponse,

    -- * Types

    -- ** ActionSeverity
    ActionSeverity (..),

    -- ** ActionStatus
    ActionStatus (..),

    -- ** ActionType
    ActionType (..),

    -- ** AutoTuneDesiredState
    AutoTuneDesiredState (..),

    -- ** AutoTuneState
    AutoTuneState (..),

    -- ** AutoTuneType
    AutoTuneType (..),

    -- ** ConnectionMode
    ConnectionMode (..),

    -- ** DeploymentStatus
    DeploymentStatus (..),

    -- ** DescribePackagesFilterName
    DescribePackagesFilterName (..),

    -- ** DomainHealth
    DomainHealth (..),

    -- ** DomainPackageStatus
    DomainPackageStatus (..),

    -- ** DomainState
    DomainState (..),

    -- ** DryRunMode
    DryRunMode (..),

    -- ** EngineType
    EngineType (..),

    -- ** InboundConnectionStatusCode
    InboundConnectionStatusCode (..),

    -- ** LogType
    LogType (..),

    -- ** MasterNodeStatus
    MasterNodeStatus (..),

    -- ** NodeStatus
    NodeStatus (..),

    -- ** NodeType
    NodeType (..),

    -- ** OpenSearchPartitionInstanceType
    OpenSearchPartitionInstanceType (..),

    -- ** OpenSearchWarmPartitionInstanceType
    OpenSearchWarmPartitionInstanceType (..),

    -- ** OptionState
    OptionState (..),

    -- ** OutboundConnectionStatusCode
    OutboundConnectionStatusCode (..),

    -- ** OverallChangeStatus
    OverallChangeStatus (..),

    -- ** PackageStatus
    PackageStatus (..),

    -- ** PackageType
    PackageType (..),

    -- ** PrincipalType
    PrincipalType (..),

    -- ** ReservedInstancePaymentOption
    ReservedInstancePaymentOption (..),

    -- ** RollbackOnDisable
    RollbackOnDisable (..),

    -- ** ScheduleAt
    ScheduleAt (..),

    -- ** ScheduledAutoTuneActionType
    ScheduledAutoTuneActionType (..),

    -- ** ScheduledAutoTuneSeverityType
    ScheduledAutoTuneSeverityType (..),

    -- ** ScheduledBy
    ScheduledBy (..),

    -- ** SkipUnavailableStatus
    SkipUnavailableStatus (..),

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

    -- ** ZoneStatus
    ZoneStatus (..),

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

    -- ** AvailabilityZoneInfo
    AvailabilityZoneInfo (AvailabilityZoneInfo'),
    newAvailabilityZoneInfo,

    -- ** ChangeProgressDetails
    ChangeProgressDetails (ChangeProgressDetails'),
    newChangeProgressDetails,

    -- ** ChangeProgressStage
    ChangeProgressStage (ChangeProgressStage'),
    newChangeProgressStage,

    -- ** ChangeProgressStatusDetails
    ChangeProgressStatusDetails (ChangeProgressStatusDetails'),
    newChangeProgressStatusDetails,

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

    -- ** ConnectionProperties
    ConnectionProperties (ConnectionProperties'),
    newConnectionProperties,

    -- ** CrossClusterSearchConnectionProperties
    CrossClusterSearchConnectionProperties (CrossClusterSearchConnectionProperties'),
    newCrossClusterSearchConnectionProperties,

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

    -- ** DomainNodesStatus
    DomainNodesStatus (DomainNodesStatus'),
    newDomainNodesStatus,

    -- ** DomainPackageDetails
    DomainPackageDetails (DomainPackageDetails'),
    newDomainPackageDetails,

    -- ** DomainStatus
    DomainStatus (DomainStatus'),
    newDomainStatus,

    -- ** DryRunProgressStatus
    DryRunProgressStatus (DryRunProgressStatus'),
    newDryRunProgressStatus,

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

    -- ** EncryptionAtRestOptions
    EncryptionAtRestOptions (EncryptionAtRestOptions'),
    newEncryptionAtRestOptions,

    -- ** EncryptionAtRestOptionsStatus
    EncryptionAtRestOptionsStatus (EncryptionAtRestOptionsStatus'),
    newEncryptionAtRestOptionsStatus,

    -- ** EnvironmentInfo
    EnvironmentInfo (EnvironmentInfo'),
    newEnvironmentInfo,

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

    -- ** OffPeakWindow
    OffPeakWindow (OffPeakWindow'),
    newOffPeakWindow,

    -- ** OffPeakWindowOptions
    OffPeakWindowOptions (OffPeakWindowOptions'),
    newOffPeakWindowOptions,

    -- ** OffPeakWindowOptionsStatus
    OffPeakWindowOptionsStatus (OffPeakWindowOptionsStatus'),
    newOffPeakWindowOptionsStatus,

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

    -- ** ScheduledAction
    ScheduledAction (ScheduledAction'),
    newScheduledAction,

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

    -- ** SoftwareUpdateOptions
    SoftwareUpdateOptions (SoftwareUpdateOptions'),
    newSoftwareUpdateOptions,

    -- ** SoftwareUpdateOptionsStatus
    SoftwareUpdateOptionsStatus (SoftwareUpdateOptionsStatus'),
    newSoftwareUpdateOptionsStatus,

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

    -- ** ValidationFailure
    ValidationFailure (ValidationFailure'),
    newValidationFailure,

    -- ** VersionStatus
    VersionStatus (VersionStatus'),
    newVersionStatus,

    -- ** VpcEndpoint
    VpcEndpoint (VpcEndpoint'),
    newVpcEndpoint,

    -- ** VpcEndpointError
    VpcEndpointError (VpcEndpointError'),
    newVpcEndpointError,

    -- ** VpcEndpointSummary
    VpcEndpointSummary (VpcEndpointSummary'),
    newVpcEndpointSummary,

    -- ** WindowStartTime
    WindowStartTime (WindowStartTime'),
    newWindowStartTime,

    -- ** ZoneAwarenessConfig
    ZoneAwarenessConfig (ZoneAwarenessConfig'),
    newZoneAwarenessConfig,
  )
where

import Amazonka.OpenSearch.AcceptInboundConnection
import Amazonka.OpenSearch.AddTags
import Amazonka.OpenSearch.AssociatePackage
import Amazonka.OpenSearch.AuthorizeVpcEndpointAccess
import Amazonka.OpenSearch.CancelServiceSoftwareUpdate
import Amazonka.OpenSearch.CreateDomain
import Amazonka.OpenSearch.CreateOutboundConnection
import Amazonka.OpenSearch.CreatePackage
import Amazonka.OpenSearch.CreateVpcEndpoint
import Amazonka.OpenSearch.DeleteDomain
import Amazonka.OpenSearch.DeleteInboundConnection
import Amazonka.OpenSearch.DeleteOutboundConnection
import Amazonka.OpenSearch.DeletePackage
import Amazonka.OpenSearch.DeleteVpcEndpoint
import Amazonka.OpenSearch.DescribeDomain
import Amazonka.OpenSearch.DescribeDomainAutoTunes
import Amazonka.OpenSearch.DescribeDomainChangeProgress
import Amazonka.OpenSearch.DescribeDomainConfig
import Amazonka.OpenSearch.DescribeDomainHealth
import Amazonka.OpenSearch.DescribeDomainNodes
import Amazonka.OpenSearch.DescribeDomains
import Amazonka.OpenSearch.DescribeDryRunProgress
import Amazonka.OpenSearch.DescribeInboundConnections
import Amazonka.OpenSearch.DescribeInstanceTypeLimits
import Amazonka.OpenSearch.DescribeOutboundConnections
import Amazonka.OpenSearch.DescribePackages
import Amazonka.OpenSearch.DescribeReservedInstanceOfferings
import Amazonka.OpenSearch.DescribeReservedInstances
import Amazonka.OpenSearch.DescribeVpcEndpoints
import Amazonka.OpenSearch.DissociatePackage
import Amazonka.OpenSearch.GetCompatibleVersions
import Amazonka.OpenSearch.GetPackageVersionHistory
import Amazonka.OpenSearch.GetUpgradeHistory
import Amazonka.OpenSearch.GetUpgradeStatus
import Amazonka.OpenSearch.Lens
import Amazonka.OpenSearch.ListDomainNames
import Amazonka.OpenSearch.ListDomainsForPackage
import Amazonka.OpenSearch.ListInstanceTypeDetails
import Amazonka.OpenSearch.ListPackagesForDomain
import Amazonka.OpenSearch.ListScheduledActions
import Amazonka.OpenSearch.ListTags
import Amazonka.OpenSearch.ListVersions
import Amazonka.OpenSearch.ListVpcEndpointAccess
import Amazonka.OpenSearch.ListVpcEndpoints
import Amazonka.OpenSearch.ListVpcEndpointsForDomain
import Amazonka.OpenSearch.PurchaseReservedInstanceOffering
import Amazonka.OpenSearch.RejectInboundConnection
import Amazonka.OpenSearch.RemoveTags
import Amazonka.OpenSearch.RevokeVpcEndpointAccess
import Amazonka.OpenSearch.StartServiceSoftwareUpdate
import Amazonka.OpenSearch.Types
import Amazonka.OpenSearch.UpdateDomainConfig
import Amazonka.OpenSearch.UpdatePackage
import Amazonka.OpenSearch.UpdateScheduledAction
import Amazonka.OpenSearch.UpdateVpcEndpoint
import Amazonka.OpenSearch.UpgradeDomain
import Amazonka.OpenSearch.Waiters

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
