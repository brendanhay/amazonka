{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.Panorama
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2019-07-24@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- AWS Panorama
--
-- __Overview__
--
-- This is the /AWS Panorama API Reference/. For an introduction to the
-- service, see
-- <https://docs.aws.amazon.com/panorama/latest/dev/panorama-welcome.html What is AWS Panorama?>
-- in the /AWS Panorama Developer Guide/.
module Amazonka.Panorama
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ConflictException
    _ConflictException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateApplicationInstance
    CreateApplicationInstance (CreateApplicationInstance'),
    newCreateApplicationInstance,
    CreateApplicationInstanceResponse (CreateApplicationInstanceResponse'),
    newCreateApplicationInstanceResponse,

    -- ** CreateJobForDevices
    CreateJobForDevices (CreateJobForDevices'),
    newCreateJobForDevices,
    CreateJobForDevicesResponse (CreateJobForDevicesResponse'),
    newCreateJobForDevicesResponse,

    -- ** CreateNodeFromTemplateJob
    CreateNodeFromTemplateJob (CreateNodeFromTemplateJob'),
    newCreateNodeFromTemplateJob,
    CreateNodeFromTemplateJobResponse (CreateNodeFromTemplateJobResponse'),
    newCreateNodeFromTemplateJobResponse,

    -- ** CreatePackage
    CreatePackage (CreatePackage'),
    newCreatePackage,
    CreatePackageResponse (CreatePackageResponse'),
    newCreatePackageResponse,

    -- ** CreatePackageImportJob
    CreatePackageImportJob (CreatePackageImportJob'),
    newCreatePackageImportJob,
    CreatePackageImportJobResponse (CreatePackageImportJobResponse'),
    newCreatePackageImportJobResponse,

    -- ** DeleteDevice
    DeleteDevice (DeleteDevice'),
    newDeleteDevice,
    DeleteDeviceResponse (DeleteDeviceResponse'),
    newDeleteDeviceResponse,

    -- ** DeletePackage
    DeletePackage (DeletePackage'),
    newDeletePackage,
    DeletePackageResponse (DeletePackageResponse'),
    newDeletePackageResponse,

    -- ** DeregisterPackageVersion
    DeregisterPackageVersion (DeregisterPackageVersion'),
    newDeregisterPackageVersion,
    DeregisterPackageVersionResponse (DeregisterPackageVersionResponse'),
    newDeregisterPackageVersionResponse,

    -- ** DescribeApplicationInstance
    DescribeApplicationInstance (DescribeApplicationInstance'),
    newDescribeApplicationInstance,
    DescribeApplicationInstanceResponse (DescribeApplicationInstanceResponse'),
    newDescribeApplicationInstanceResponse,

    -- ** DescribeApplicationInstanceDetails
    DescribeApplicationInstanceDetails (DescribeApplicationInstanceDetails'),
    newDescribeApplicationInstanceDetails,
    DescribeApplicationInstanceDetailsResponse (DescribeApplicationInstanceDetailsResponse'),
    newDescribeApplicationInstanceDetailsResponse,

    -- ** DescribeDevice
    DescribeDevice (DescribeDevice'),
    newDescribeDevice,
    DescribeDeviceResponse (DescribeDeviceResponse'),
    newDescribeDeviceResponse,

    -- ** DescribeDeviceJob
    DescribeDeviceJob (DescribeDeviceJob'),
    newDescribeDeviceJob,
    DescribeDeviceJobResponse (DescribeDeviceJobResponse'),
    newDescribeDeviceJobResponse,

    -- ** DescribeNode
    DescribeNode (DescribeNode'),
    newDescribeNode,
    DescribeNodeResponse (DescribeNodeResponse'),
    newDescribeNodeResponse,

    -- ** DescribeNodeFromTemplateJob
    DescribeNodeFromTemplateJob (DescribeNodeFromTemplateJob'),
    newDescribeNodeFromTemplateJob,
    DescribeNodeFromTemplateJobResponse (DescribeNodeFromTemplateJobResponse'),
    newDescribeNodeFromTemplateJobResponse,

    -- ** DescribePackage
    DescribePackage (DescribePackage'),
    newDescribePackage,
    DescribePackageResponse (DescribePackageResponse'),
    newDescribePackageResponse,

    -- ** DescribePackageImportJob
    DescribePackageImportJob (DescribePackageImportJob'),
    newDescribePackageImportJob,
    DescribePackageImportJobResponse (DescribePackageImportJobResponse'),
    newDescribePackageImportJobResponse,

    -- ** DescribePackageVersion
    DescribePackageVersion (DescribePackageVersion'),
    newDescribePackageVersion,
    DescribePackageVersionResponse (DescribePackageVersionResponse'),
    newDescribePackageVersionResponse,

    -- ** ListApplicationInstanceDependencies
    ListApplicationInstanceDependencies (ListApplicationInstanceDependencies'),
    newListApplicationInstanceDependencies,
    ListApplicationInstanceDependenciesResponse (ListApplicationInstanceDependenciesResponse'),
    newListApplicationInstanceDependenciesResponse,

    -- ** ListApplicationInstanceNodeInstances
    ListApplicationInstanceNodeInstances (ListApplicationInstanceNodeInstances'),
    newListApplicationInstanceNodeInstances,
    ListApplicationInstanceNodeInstancesResponse (ListApplicationInstanceNodeInstancesResponse'),
    newListApplicationInstanceNodeInstancesResponse,

    -- ** ListApplicationInstances
    ListApplicationInstances (ListApplicationInstances'),
    newListApplicationInstances,
    ListApplicationInstancesResponse (ListApplicationInstancesResponse'),
    newListApplicationInstancesResponse,

    -- ** ListDevices
    ListDevices (ListDevices'),
    newListDevices,
    ListDevicesResponse (ListDevicesResponse'),
    newListDevicesResponse,

    -- ** ListDevicesJobs
    ListDevicesJobs (ListDevicesJobs'),
    newListDevicesJobs,
    ListDevicesJobsResponse (ListDevicesJobsResponse'),
    newListDevicesJobsResponse,

    -- ** ListNodeFromTemplateJobs
    ListNodeFromTemplateJobs (ListNodeFromTemplateJobs'),
    newListNodeFromTemplateJobs,
    ListNodeFromTemplateJobsResponse (ListNodeFromTemplateJobsResponse'),
    newListNodeFromTemplateJobsResponse,

    -- ** ListNodes
    ListNodes (ListNodes'),
    newListNodes,
    ListNodesResponse (ListNodesResponse'),
    newListNodesResponse,

    -- ** ListPackageImportJobs
    ListPackageImportJobs (ListPackageImportJobs'),
    newListPackageImportJobs,
    ListPackageImportJobsResponse (ListPackageImportJobsResponse'),
    newListPackageImportJobsResponse,

    -- ** ListPackages
    ListPackages (ListPackages'),
    newListPackages,
    ListPackagesResponse (ListPackagesResponse'),
    newListPackagesResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ProvisionDevice
    ProvisionDevice (ProvisionDevice'),
    newProvisionDevice,
    ProvisionDeviceResponse (ProvisionDeviceResponse'),
    newProvisionDeviceResponse,

    -- ** RegisterPackageVersion
    RegisterPackageVersion (RegisterPackageVersion'),
    newRegisterPackageVersion,
    RegisterPackageVersionResponse (RegisterPackageVersionResponse'),
    newRegisterPackageVersionResponse,

    -- ** RemoveApplicationInstance
    RemoveApplicationInstance (RemoveApplicationInstance'),
    newRemoveApplicationInstance,
    RemoveApplicationInstanceResponse (RemoveApplicationInstanceResponse'),
    newRemoveApplicationInstanceResponse,

    -- ** SignalApplicationInstanceNodeInstances
    SignalApplicationInstanceNodeInstances (SignalApplicationInstanceNodeInstances'),
    newSignalApplicationInstanceNodeInstances,
    SignalApplicationInstanceNodeInstancesResponse (SignalApplicationInstanceNodeInstancesResponse'),
    newSignalApplicationInstanceNodeInstancesResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateDeviceMetadata
    UpdateDeviceMetadata (UpdateDeviceMetadata'),
    newUpdateDeviceMetadata,
    UpdateDeviceMetadataResponse (UpdateDeviceMetadataResponse'),
    newUpdateDeviceMetadataResponse,

    -- * Types

    -- ** ApplicationInstanceHealthStatus
    ApplicationInstanceHealthStatus (..),

    -- ** ApplicationInstanceStatus
    ApplicationInstanceStatus (..),

    -- ** ConnectionType
    ConnectionType (..),

    -- ** DesiredState
    DesiredState (..),

    -- ** DeviceAggregatedStatus
    DeviceAggregatedStatus (..),

    -- ** DeviceBrand
    DeviceBrand (..),

    -- ** DeviceConnectionStatus
    DeviceConnectionStatus (..),

    -- ** DeviceReportedStatus
    DeviceReportedStatus (..),

    -- ** DeviceStatus
    DeviceStatus (..),

    -- ** DeviceType
    DeviceType (..),

    -- ** JobResourceType
    JobResourceType (..),

    -- ** JobType
    JobType (..),

    -- ** ListDevicesSortBy
    ListDevicesSortBy (..),

    -- ** NetworkConnectionStatus
    NetworkConnectionStatus (..),

    -- ** NodeCategory
    NodeCategory (..),

    -- ** NodeFromTemplateJobStatus
    NodeFromTemplateJobStatus (..),

    -- ** NodeInstanceStatus
    NodeInstanceStatus (..),

    -- ** NodeSignalValue
    NodeSignalValue (..),

    -- ** PackageImportJobStatus
    PackageImportJobStatus (..),

    -- ** PackageImportJobType
    PackageImportJobType (..),

    -- ** PackageVersionStatus
    PackageVersionStatus (..),

    -- ** PortType
    PortType (..),

    -- ** SortOrder
    SortOrder (..),

    -- ** StatusFilter
    StatusFilter (..),

    -- ** TemplateType
    TemplateType (..),

    -- ** UpdateProgress
    UpdateProgress (..),

    -- ** AlternateSoftwareMetadata
    AlternateSoftwareMetadata (AlternateSoftwareMetadata'),
    newAlternateSoftwareMetadata,

    -- ** ApplicationInstance
    ApplicationInstance (ApplicationInstance'),
    newApplicationInstance,

    -- ** Device
    Device (Device'),
    newDevice,

    -- ** DeviceJob
    DeviceJob (DeviceJob'),
    newDeviceJob,

    -- ** DeviceJobConfig
    DeviceJobConfig (DeviceJobConfig'),
    newDeviceJobConfig,

    -- ** EthernetPayload
    EthernetPayload (EthernetPayload'),
    newEthernetPayload,

    -- ** EthernetStatus
    EthernetStatus (EthernetStatus'),
    newEthernetStatus,

    -- ** Job
    Job (Job'),
    newJob,

    -- ** JobResourceTags
    JobResourceTags (JobResourceTags'),
    newJobResourceTags,

    -- ** LatestDeviceJob
    LatestDeviceJob (LatestDeviceJob'),
    newLatestDeviceJob,

    -- ** ManifestOverridesPayload
    ManifestOverridesPayload (ManifestOverridesPayload'),
    newManifestOverridesPayload,

    -- ** ManifestPayload
    ManifestPayload (ManifestPayload'),
    newManifestPayload,

    -- ** NetworkPayload
    NetworkPayload (NetworkPayload'),
    newNetworkPayload,

    -- ** NetworkStatus
    NetworkStatus (NetworkStatus'),
    newNetworkStatus,

    -- ** Node
    Node (Node'),
    newNode,

    -- ** NodeFromTemplateJob
    NodeFromTemplateJob (NodeFromTemplateJob'),
    newNodeFromTemplateJob,

    -- ** NodeInputPort
    NodeInputPort (NodeInputPort'),
    newNodeInputPort,

    -- ** NodeInstance
    NodeInstance (NodeInstance'),
    newNodeInstance,

    -- ** NodeInterface
    NodeInterface (NodeInterface'),
    newNodeInterface,

    -- ** NodeOutputPort
    NodeOutputPort (NodeOutputPort'),
    newNodeOutputPort,

    -- ** NodeSignal
    NodeSignal (NodeSignal'),
    newNodeSignal,

    -- ** NtpPayload
    NtpPayload (NtpPayload'),
    newNtpPayload,

    -- ** NtpStatus
    NtpStatus (NtpStatus'),
    newNtpStatus,

    -- ** OTAJobConfig
    OTAJobConfig (OTAJobConfig'),
    newOTAJobConfig,

    -- ** OutPutS3Location
    OutPutS3Location (OutPutS3Location'),
    newOutPutS3Location,

    -- ** PackageImportJob
    PackageImportJob (PackageImportJob'),
    newPackageImportJob,

    -- ** PackageImportJobInputConfig
    PackageImportJobInputConfig (PackageImportJobInputConfig'),
    newPackageImportJobInputConfig,

    -- ** PackageImportJobOutput
    PackageImportJobOutput (PackageImportJobOutput'),
    newPackageImportJobOutput,

    -- ** PackageImportJobOutputConfig
    PackageImportJobOutputConfig (PackageImportJobOutputConfig'),
    newPackageImportJobOutputConfig,

    -- ** PackageListItem
    PackageListItem (PackageListItem'),
    newPackageListItem,

    -- ** PackageObject
    PackageObject (PackageObject'),
    newPackageObject,

    -- ** PackageVersionInputConfig
    PackageVersionInputConfig (PackageVersionInputConfig'),
    newPackageVersionInputConfig,

    -- ** PackageVersionOutputConfig
    PackageVersionOutputConfig (PackageVersionOutputConfig'),
    newPackageVersionOutputConfig,

    -- ** ReportedRuntimeContextState
    ReportedRuntimeContextState (ReportedRuntimeContextState'),
    newReportedRuntimeContextState,

    -- ** S3Location
    S3Location (S3Location'),
    newS3Location,

    -- ** StaticIpConnectionInfo
    StaticIpConnectionInfo (StaticIpConnectionInfo'),
    newStaticIpConnectionInfo,

    -- ** StorageLocation
    StorageLocation (StorageLocation'),
    newStorageLocation,
  )
where

import Amazonka.Panorama.CreateApplicationInstance
import Amazonka.Panorama.CreateJobForDevices
import Amazonka.Panorama.CreateNodeFromTemplateJob
import Amazonka.Panorama.CreatePackage
import Amazonka.Panorama.CreatePackageImportJob
import Amazonka.Panorama.DeleteDevice
import Amazonka.Panorama.DeletePackage
import Amazonka.Panorama.DeregisterPackageVersion
import Amazonka.Panorama.DescribeApplicationInstance
import Amazonka.Panorama.DescribeApplicationInstanceDetails
import Amazonka.Panorama.DescribeDevice
import Amazonka.Panorama.DescribeDeviceJob
import Amazonka.Panorama.DescribeNode
import Amazonka.Panorama.DescribeNodeFromTemplateJob
import Amazonka.Panorama.DescribePackage
import Amazonka.Panorama.DescribePackageImportJob
import Amazonka.Panorama.DescribePackageVersion
import Amazonka.Panorama.Lens
import Amazonka.Panorama.ListApplicationInstanceDependencies
import Amazonka.Panorama.ListApplicationInstanceNodeInstances
import Amazonka.Panorama.ListApplicationInstances
import Amazonka.Panorama.ListDevices
import Amazonka.Panorama.ListDevicesJobs
import Amazonka.Panorama.ListNodeFromTemplateJobs
import Amazonka.Panorama.ListNodes
import Amazonka.Panorama.ListPackageImportJobs
import Amazonka.Panorama.ListPackages
import Amazonka.Panorama.ListTagsForResource
import Amazonka.Panorama.ProvisionDevice
import Amazonka.Panorama.RegisterPackageVersion
import Amazonka.Panorama.RemoveApplicationInstance
import Amazonka.Panorama.SignalApplicationInstanceNodeInstances
import Amazonka.Panorama.TagResource
import Amazonka.Panorama.Types
import Amazonka.Panorama.UntagResource
import Amazonka.Panorama.UpdateDeviceMetadata
import Amazonka.Panorama.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'Panorama'.

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
