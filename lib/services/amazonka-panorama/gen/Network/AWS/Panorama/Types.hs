{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Panorama.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Panorama.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ValidationException,
    _AccessDeniedException,
    _ConflictException,
    _ServiceQuotaExceededException,
    _InternalServerException,
    _ResourceNotFoundException,

    -- * ApplicationInstanceHealthStatus
    ApplicationInstanceHealthStatus (..),

    -- * ApplicationInstanceStatus
    ApplicationInstanceStatus (..),

    -- * ConnectionType
    ConnectionType (..),

    -- * DeviceConnectionStatus
    DeviceConnectionStatus (..),

    -- * DeviceStatus
    DeviceStatus (..),

    -- * DeviceType
    DeviceType (..),

    -- * JobResourceType
    JobResourceType (..),

    -- * JobType
    JobType (..),

    -- * NetworkConnectionStatus
    NetworkConnectionStatus (..),

    -- * NodeCategory
    NodeCategory (..),

    -- * NodeFromTemplateJobStatus
    NodeFromTemplateJobStatus (..),

    -- * NodeInstanceStatus
    NodeInstanceStatus (..),

    -- * PackageImportJobStatus
    PackageImportJobStatus (..),

    -- * PackageImportJobType
    PackageImportJobType (..),

    -- * PackageVersionStatus
    PackageVersionStatus (..),

    -- * PortType
    PortType (..),

    -- * StatusFilter
    StatusFilter (..),

    -- * TemplateType
    TemplateType (..),

    -- * UpdateProgress
    UpdateProgress (..),

    -- * ApplicationInstance
    ApplicationInstance (..),
    newApplicationInstance,
    applicationInstance_status,
    applicationInstance_statusDescription,
    applicationInstance_arn,
    applicationInstance_createdTime,
    applicationInstance_defaultRuntimeContextDevice,
    applicationInstance_defaultRuntimeContextDeviceName,
    applicationInstance_name,
    applicationInstance_healthStatus,
    applicationInstance_applicationInstanceId,
    applicationInstance_description,
    applicationInstance_tags,

    -- * Device
    Device (..),
    newDevice,
    device_lastUpdatedTime,
    device_provisioningStatus,
    device_createdTime,
    device_name,
    device_deviceId,
    device_leaseExpirationTime,

    -- * DeviceJob
    DeviceJob (..),
    newDeviceJob,
    deviceJob_jobId,
    deviceJob_createdTime,
    deviceJob_deviceName,
    deviceJob_deviceId,

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
    ethernetStatus_ipAddress,
    ethernetStatus_connectionStatus,
    ethernetStatus_hwAddress,

    -- * Job
    Job (..),
    newJob,
    job_jobId,
    job_deviceId,

    -- * JobResourceTags
    JobResourceTags (..),
    newJobResourceTags,
    jobResourceTags_resourceType,
    jobResourceTags_tags,

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
    networkPayload_ethernet1,
    networkPayload_ethernet0,

    -- * NetworkStatus
    NetworkStatus (..),
    newNetworkStatus,
    networkStatus_ethernet1Status,
    networkStatus_ethernet0Status,

    -- * Node
    Node (..),
    newNode,
    node_packageArn,
    node_ownerAccount,
    node_description,
    node_nodeId,
    node_name,
    node_category,
    node_packageName,
    node_packageId,
    node_packageVersion,
    node_patchVersion,
    node_createdTime,

    -- * NodeFromTemplateJob
    NodeFromTemplateJob (..),
    newNodeFromTemplateJob,
    nodeFromTemplateJob_status,
    nodeFromTemplateJob_jobId,
    nodeFromTemplateJob_createdTime,
    nodeFromTemplateJob_templateType,
    nodeFromTemplateJob_nodeName,
    nodeFromTemplateJob_statusMessage,

    -- * NodeInputPort
    NodeInputPort (..),
    newNodeInputPort,
    nodeInputPort_maxConnections,
    nodeInputPort_name,
    nodeInputPort_defaultValue,
    nodeInputPort_type,
    nodeInputPort_description,

    -- * NodeInstance
    NodeInstance (..),
    newNodeInstance,
    nodeInstance_packageName,
    nodeInstance_packageVersion,
    nodeInstance_packagePatchVersion,
    nodeInstance_nodeName,
    nodeInstance_nodeId,
    nodeInstance_nodeInstanceId,
    nodeInstance_currentStatus,

    -- * NodeInterface
    NodeInterface (..),
    newNodeInterface,
    nodeInterface_inputs,
    nodeInterface_outputs,

    -- * NodeOutputPort
    NodeOutputPort (..),
    newNodeOutputPort,
    nodeOutputPort_name,
    nodeOutputPort_type,
    nodeOutputPort_description,

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
    packageImportJob_status,
    packageImportJob_jobType,
    packageImportJob_lastUpdatedTime,
    packageImportJob_jobId,
    packageImportJob_createdTime,
    packageImportJob_statusMessage,

    -- * PackageImportJobInputConfig
    PackageImportJobInputConfig (..),
    newPackageImportJobInputConfig,
    packageImportJobInputConfig_packageVersionInputConfig,

    -- * PackageImportJobOutput
    PackageImportJobOutput (..),
    newPackageImportJobOutput,
    packageImportJobOutput_packageId,
    packageImportJobOutput_packageVersion,
    packageImportJobOutput_patchVersion,
    packageImportJobOutput_outputS3Location,

    -- * PackageImportJobOutputConfig
    PackageImportJobOutputConfig (..),
    newPackageImportJobOutputConfig,
    packageImportJobOutputConfig_packageVersionOutputConfig,

    -- * PackageListItem
    PackageListItem (..),
    newPackageListItem,
    packageListItem_packageId,
    packageListItem_arn,
    packageListItem_createdTime,
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

    -- * S3Location
    S3Location (..),
    newS3Location,
    s3Location_region,
    s3Location_bucketName,
    s3Location_objectKey,

    -- * StaticIpConnectionInfo
    StaticIpConnectionInfo (..),
    newStaticIpConnectionInfo,
    staticIpConnectionInfo_ipAddress,
    staticIpConnectionInfo_mask,
    staticIpConnectionInfo_dns,
    staticIpConnectionInfo_defaultGateway,

    -- * StorageLocation
    StorageLocation (..),
    newStorageLocation,
    storageLocation_bucket,
    storageLocation_repoPrefixLocation,
    storageLocation_generatedPrefixLocation,
    storageLocation_binaryPrefixLocation,
    storageLocation_manifestPrefixLocation,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Panorama.Types.ApplicationInstance
import Network.AWS.Panorama.Types.ApplicationInstanceHealthStatus
import Network.AWS.Panorama.Types.ApplicationInstanceStatus
import Network.AWS.Panorama.Types.ConnectionType
import Network.AWS.Panorama.Types.Device
import Network.AWS.Panorama.Types.DeviceConnectionStatus
import Network.AWS.Panorama.Types.DeviceJob
import Network.AWS.Panorama.Types.DeviceJobConfig
import Network.AWS.Panorama.Types.DeviceStatus
import Network.AWS.Panorama.Types.DeviceType
import Network.AWS.Panorama.Types.EthernetPayload
import Network.AWS.Panorama.Types.EthernetStatus
import Network.AWS.Panorama.Types.Job
import Network.AWS.Panorama.Types.JobResourceTags
import Network.AWS.Panorama.Types.JobResourceType
import Network.AWS.Panorama.Types.JobType
import Network.AWS.Panorama.Types.ManifestOverridesPayload
import Network.AWS.Panorama.Types.ManifestPayload
import Network.AWS.Panorama.Types.NetworkConnectionStatus
import Network.AWS.Panorama.Types.NetworkPayload
import Network.AWS.Panorama.Types.NetworkStatus
import Network.AWS.Panorama.Types.Node
import Network.AWS.Panorama.Types.NodeCategory
import Network.AWS.Panorama.Types.NodeFromTemplateJob
import Network.AWS.Panorama.Types.NodeFromTemplateJobStatus
import Network.AWS.Panorama.Types.NodeInputPort
import Network.AWS.Panorama.Types.NodeInstance
import Network.AWS.Panorama.Types.NodeInstanceStatus
import Network.AWS.Panorama.Types.NodeInterface
import Network.AWS.Panorama.Types.NodeOutputPort
import Network.AWS.Panorama.Types.OTAJobConfig
import Network.AWS.Panorama.Types.OutPutS3Location
import Network.AWS.Panorama.Types.PackageImportJob
import Network.AWS.Panorama.Types.PackageImportJobInputConfig
import Network.AWS.Panorama.Types.PackageImportJobOutput
import Network.AWS.Panorama.Types.PackageImportJobOutputConfig
import Network.AWS.Panorama.Types.PackageImportJobStatus
import Network.AWS.Panorama.Types.PackageImportJobType
import Network.AWS.Panorama.Types.PackageListItem
import Network.AWS.Panorama.Types.PackageObject
import Network.AWS.Panorama.Types.PackageVersionInputConfig
import Network.AWS.Panorama.Types.PackageVersionOutputConfig
import Network.AWS.Panorama.Types.PackageVersionStatus
import Network.AWS.Panorama.Types.PortType
import Network.AWS.Panorama.Types.S3Location
import Network.AWS.Panorama.Types.StaticIpConnectionInfo
import Network.AWS.Panorama.Types.StatusFilter
import Network.AWS.Panorama.Types.StorageLocation
import Network.AWS.Panorama.Types.TemplateType
import Network.AWS.Panorama.Types.UpdateProgress
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2019-07-24@ of the Amazon Panorama SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "Panorama",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "panorama",
      Core._serviceSigningName = "panorama",
      Core._serviceVersion = "2019-07-24",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "Panorama",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | The request contains an invalid parameter value.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400

-- | The requestor does not have permission to access the target action or
-- resource.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | The target resource is in use.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | The request would cause a limit to be exceeded.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | An internal error occurred.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | The target resource was not found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404
