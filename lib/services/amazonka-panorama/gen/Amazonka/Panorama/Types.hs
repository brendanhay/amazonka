{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Panorama.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Panorama.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _ConflictException,
    _InternalServerException,
    _ResourceNotFoundException,
    _ServiceQuotaExceededException,
    _ValidationException,

    -- * ApplicationInstanceHealthStatus
    ApplicationInstanceHealthStatus (..),

    -- * ApplicationInstanceStatus
    ApplicationInstanceStatus (..),

    -- * ConnectionType
    ConnectionType (..),

    -- * DesiredState
    DesiredState (..),

    -- * DeviceAggregatedStatus
    DeviceAggregatedStatus (..),

    -- * DeviceBrand
    DeviceBrand (..),

    -- * DeviceConnectionStatus
    DeviceConnectionStatus (..),

    -- * DeviceReportedStatus
    DeviceReportedStatus (..),

    -- * DeviceStatus
    DeviceStatus (..),

    -- * DeviceType
    DeviceType (..),

    -- * JobResourceType
    JobResourceType (..),

    -- * JobType
    JobType (..),

    -- * ListDevicesSortBy
    ListDevicesSortBy (..),

    -- * NetworkConnectionStatus
    NetworkConnectionStatus (..),

    -- * NodeCategory
    NodeCategory (..),

    -- * NodeFromTemplateJobStatus
    NodeFromTemplateJobStatus (..),

    -- * NodeInstanceStatus
    NodeInstanceStatus (..),

    -- * NodeSignalValue
    NodeSignalValue (..),

    -- * PackageImportJobStatus
    PackageImportJobStatus (..),

    -- * PackageImportJobType
    PackageImportJobType (..),

    -- * PackageVersionStatus
    PackageVersionStatus (..),

    -- * PortType
    PortType (..),

    -- * SortOrder
    SortOrder (..),

    -- * StatusFilter
    StatusFilter (..),

    -- * TemplateType
    TemplateType (..),

    -- * UpdateProgress
    UpdateProgress (..),

    -- * AlternateSoftwareMetadata
    AlternateSoftwareMetadata (..),
    newAlternateSoftwareMetadata,
    alternateSoftwareMetadata_version,

    -- * ApplicationInstance
    ApplicationInstance (..),
    newApplicationInstance,
    applicationInstance_applicationInstanceId,
    applicationInstance_arn,
    applicationInstance_createdTime,
    applicationInstance_defaultRuntimeContextDevice,
    applicationInstance_defaultRuntimeContextDeviceName,
    applicationInstance_description,
    applicationInstance_healthStatus,
    applicationInstance_name,
    applicationInstance_runtimeContextStates,
    applicationInstance_status,
    applicationInstance_statusDescription,
    applicationInstance_tags,

    -- * Device
    Device (..),
    newDevice,
    device_brand,
    device_createdTime,
    device_currentSoftware,
    device_description,
    device_deviceAggregatedStatus,
    device_deviceId,
    device_lastUpdatedTime,
    device_latestDeviceJob,
    device_leaseExpirationTime,
    device_name,
    device_provisioningStatus,
    device_tags,
    device_type,

    -- * DeviceJob
    DeviceJob (..),
    newDeviceJob,
    deviceJob_createdTime,
    deviceJob_deviceId,
    deviceJob_deviceName,
    deviceJob_jobId,
    deviceJob_jobType,

    -- * DeviceJobConfig
    DeviceJobConfig (..),
    newDeviceJobConfig,
    deviceJobConfig_oTAJobConfig,

    -- * EthernetPayload
    EthernetPayload (..),
    newEthernetPayload,
    ethernetPayload_staticIpConnectionInfo,
    ethernetPayload_connectionType,

    -- * EthernetStatus
    EthernetStatus (..),
    newEthernetStatus,
    ethernetStatus_connectionStatus,
    ethernetStatus_hwAddress,
    ethernetStatus_ipAddress,

    -- * Job
    Job (..),
    newJob,
    job_deviceId,
    job_jobId,

    -- * JobResourceTags
    JobResourceTags (..),
    newJobResourceTags,
    jobResourceTags_resourceType,
    jobResourceTags_tags,

    -- * LatestDeviceJob
    LatestDeviceJob (..),
    newLatestDeviceJob,
    latestDeviceJob_imageVersion,
    latestDeviceJob_jobType,
    latestDeviceJob_status,

    -- * ManifestOverridesPayload
    ManifestOverridesPayload (..),
    newManifestOverridesPayload,
    manifestOverridesPayload_payloadData,

    -- * ManifestPayload
    ManifestPayload (..),
    newManifestPayload,
    manifestPayload_payloadData,

    -- * NetworkPayload
    NetworkPayload (..),
    newNetworkPayload,
    networkPayload_ethernet0,
    networkPayload_ethernet1,
    networkPayload_ntp,

    -- * NetworkStatus
    NetworkStatus (..),
    newNetworkStatus,
    networkStatus_ethernet0Status,
    networkStatus_ethernet1Status,
    networkStatus_lastUpdatedTime,
    networkStatus_ntpStatus,

    -- * Node
    Node (..),
    newNode,
    node_description,
    node_ownerAccount,
    node_packageArn,
    node_category,
    node_createdTime,
    node_name,
    node_nodeId,
    node_packageId,
    node_packageName,
    node_packageVersion,
    node_patchVersion,

    -- * NodeFromTemplateJob
    NodeFromTemplateJob (..),
    newNodeFromTemplateJob,
    nodeFromTemplateJob_createdTime,
    nodeFromTemplateJob_jobId,
    nodeFromTemplateJob_nodeName,
    nodeFromTemplateJob_status,
    nodeFromTemplateJob_statusMessage,
    nodeFromTemplateJob_templateType,

    -- * NodeInputPort
    NodeInputPort (..),
    newNodeInputPort,
    nodeInputPort_defaultValue,
    nodeInputPort_description,
    nodeInputPort_maxConnections,
    nodeInputPort_name,
    nodeInputPort_type,

    -- * NodeInstance
    NodeInstance (..),
    newNodeInstance,
    nodeInstance_nodeId,
    nodeInstance_nodeName,
    nodeInstance_packageName,
    nodeInstance_packagePatchVersion,
    nodeInstance_packageVersion,
    nodeInstance_currentStatus,
    nodeInstance_nodeInstanceId,

    -- * NodeInterface
    NodeInterface (..),
    newNodeInterface,
    nodeInterface_inputs,
    nodeInterface_outputs,

    -- * NodeOutputPort
    NodeOutputPort (..),
    newNodeOutputPort,
    nodeOutputPort_description,
    nodeOutputPort_name,
    nodeOutputPort_type,

    -- * NodeSignal
    NodeSignal (..),
    newNodeSignal,
    nodeSignal_nodeInstanceId,
    nodeSignal_signal,

    -- * NtpPayload
    NtpPayload (..),
    newNtpPayload,
    ntpPayload_ntpServers,

    -- * NtpStatus
    NtpStatus (..),
    newNtpStatus,
    ntpStatus_connectionStatus,
    ntpStatus_ipAddress,
    ntpStatus_ntpServerName,

    -- * OTAJobConfig
    OTAJobConfig (..),
    newOTAJobConfig,
    oTAJobConfig_imageVersion,

    -- * OutPutS3Location
    OutPutS3Location (..),
    newOutPutS3Location,
    outPutS3Location_bucketName,
    outPutS3Location_objectKey,

    -- * PackageImportJob
    PackageImportJob (..),
    newPackageImportJob,
    packageImportJob_createdTime,
    packageImportJob_jobId,
    packageImportJob_jobType,
    packageImportJob_lastUpdatedTime,
    packageImportJob_status,
    packageImportJob_statusMessage,

    -- * PackageImportJobInputConfig
    PackageImportJobInputConfig (..),
    newPackageImportJobInputConfig,
    packageImportJobInputConfig_packageVersionInputConfig,

    -- * PackageImportJobOutput
    PackageImportJobOutput (..),
    newPackageImportJobOutput,
    packageImportJobOutput_outputS3Location,
    packageImportJobOutput_packageId,
    packageImportJobOutput_packageVersion,
    packageImportJobOutput_patchVersion,

    -- * PackageImportJobOutputConfig
    PackageImportJobOutputConfig (..),
    newPackageImportJobOutputConfig,
    packageImportJobOutputConfig_packageVersionOutputConfig,

    -- * PackageListItem
    PackageListItem (..),
    newPackageListItem,
    packageListItem_arn,
    packageListItem_createdTime,
    packageListItem_packageId,
    packageListItem_packageName,
    packageListItem_tags,

    -- * PackageObject
    PackageObject (..),
    newPackageObject,
    packageObject_name,
    packageObject_packageVersion,
    packageObject_patchVersion,

    -- * PackageVersionInputConfig
    PackageVersionInputConfig (..),
    newPackageVersionInputConfig,
    packageVersionInputConfig_s3Location,

    -- * PackageVersionOutputConfig
    PackageVersionOutputConfig (..),
    newPackageVersionOutputConfig,
    packageVersionOutputConfig_markLatest,
    packageVersionOutputConfig_packageName,
    packageVersionOutputConfig_packageVersion,

    -- * ReportedRuntimeContextState
    ReportedRuntimeContextState (..),
    newReportedRuntimeContextState,
    reportedRuntimeContextState_desiredState,
    reportedRuntimeContextState_deviceReportedStatus,
    reportedRuntimeContextState_deviceReportedTime,
    reportedRuntimeContextState_runtimeContextName,

    -- * S3Location
    S3Location (..),
    newS3Location,
    s3Location_region,
    s3Location_bucketName,
    s3Location_objectKey,

    -- * StaticIpConnectionInfo
    StaticIpConnectionInfo (..),
    newStaticIpConnectionInfo,
    staticIpConnectionInfo_defaultGateway,
    staticIpConnectionInfo_dns,
    staticIpConnectionInfo_ipAddress,
    staticIpConnectionInfo_mask,

    -- * StorageLocation
    StorageLocation (..),
    newStorageLocation,
    storageLocation_binaryPrefixLocation,
    storageLocation_bucket,
    storageLocation_generatedPrefixLocation,
    storageLocation_manifestPrefixLocation,
    storageLocation_repoPrefixLocation,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Panorama.Types.AlternateSoftwareMetadata
import Amazonka.Panorama.Types.ApplicationInstance
import Amazonka.Panorama.Types.ApplicationInstanceHealthStatus
import Amazonka.Panorama.Types.ApplicationInstanceStatus
import Amazonka.Panorama.Types.ConnectionType
import Amazonka.Panorama.Types.DesiredState
import Amazonka.Panorama.Types.Device
import Amazonka.Panorama.Types.DeviceAggregatedStatus
import Amazonka.Panorama.Types.DeviceBrand
import Amazonka.Panorama.Types.DeviceConnectionStatus
import Amazonka.Panorama.Types.DeviceJob
import Amazonka.Panorama.Types.DeviceJobConfig
import Amazonka.Panorama.Types.DeviceReportedStatus
import Amazonka.Panorama.Types.DeviceStatus
import Amazonka.Panorama.Types.DeviceType
import Amazonka.Panorama.Types.EthernetPayload
import Amazonka.Panorama.Types.EthernetStatus
import Amazonka.Panorama.Types.Job
import Amazonka.Panorama.Types.JobResourceTags
import Amazonka.Panorama.Types.JobResourceType
import Amazonka.Panorama.Types.JobType
import Amazonka.Panorama.Types.LatestDeviceJob
import Amazonka.Panorama.Types.ListDevicesSortBy
import Amazonka.Panorama.Types.ManifestOverridesPayload
import Amazonka.Panorama.Types.ManifestPayload
import Amazonka.Panorama.Types.NetworkConnectionStatus
import Amazonka.Panorama.Types.NetworkPayload
import Amazonka.Panorama.Types.NetworkStatus
import Amazonka.Panorama.Types.Node
import Amazonka.Panorama.Types.NodeCategory
import Amazonka.Panorama.Types.NodeFromTemplateJob
import Amazonka.Panorama.Types.NodeFromTemplateJobStatus
import Amazonka.Panorama.Types.NodeInputPort
import Amazonka.Panorama.Types.NodeInstance
import Amazonka.Panorama.Types.NodeInstanceStatus
import Amazonka.Panorama.Types.NodeInterface
import Amazonka.Panorama.Types.NodeOutputPort
import Amazonka.Panorama.Types.NodeSignal
import Amazonka.Panorama.Types.NodeSignalValue
import Amazonka.Panorama.Types.NtpPayload
import Amazonka.Panorama.Types.NtpStatus
import Amazonka.Panorama.Types.OTAJobConfig
import Amazonka.Panorama.Types.OutPutS3Location
import Amazonka.Panorama.Types.PackageImportJob
import Amazonka.Panorama.Types.PackageImportJobInputConfig
import Amazonka.Panorama.Types.PackageImportJobOutput
import Amazonka.Panorama.Types.PackageImportJobOutputConfig
import Amazonka.Panorama.Types.PackageImportJobStatus
import Amazonka.Panorama.Types.PackageImportJobType
import Amazonka.Panorama.Types.PackageListItem
import Amazonka.Panorama.Types.PackageObject
import Amazonka.Panorama.Types.PackageVersionInputConfig
import Amazonka.Panorama.Types.PackageVersionOutputConfig
import Amazonka.Panorama.Types.PackageVersionStatus
import Amazonka.Panorama.Types.PortType
import Amazonka.Panorama.Types.ReportedRuntimeContextState
import Amazonka.Panorama.Types.S3Location
import Amazonka.Panorama.Types.SortOrder
import Amazonka.Panorama.Types.StaticIpConnectionInfo
import Amazonka.Panorama.Types.StatusFilter
import Amazonka.Panorama.Types.StorageLocation
import Amazonka.Panorama.Types.TemplateType
import Amazonka.Panorama.Types.UpdateProgress
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2019-07-24@ of the Amazon Panorama SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "Panorama",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "panorama",
      Core.signingName = "panorama",
      Core.version = "2019-07-24",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "Panorama",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 502) e =
          Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
          Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
          Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
          Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
          Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
          Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | The requestor does not have permission to access the target action or
-- resource.
_AccessDeniedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | The target resource is in use.
_ConflictException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | An internal error occurred.
_InternalServerException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | The target resource was not found.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | The request would cause a limit to be exceeded.
_ServiceQuotaExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | The request contains an invalid parameter value.
_ValidationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400
