{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataSync.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DataSync.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InvalidRequestException,
    _InternalException,

    -- * AgentStatus
    AgentStatus (..),

    -- * Atime
    Atime (..),

    -- * EndpointType
    EndpointType (..),

    -- * FilterType
    FilterType (..),

    -- * Gid
    Gid (..),

    -- * LocationFilterName
    LocationFilterName (..),

    -- * LogLevel
    LogLevel (..),

    -- * Mtime
    Mtime (..),

    -- * NfsVersion
    NfsVersion (..),

    -- * ObjectStorageServerProtocol
    ObjectStorageServerProtocol (..),

    -- * Operator
    Operator (..),

    -- * OverwriteMode
    OverwriteMode (..),

    -- * PhaseStatus
    PhaseStatus (..),

    -- * PosixPermissions
    PosixPermissions (..),

    -- * PreserveDeletedFiles
    PreserveDeletedFiles (..),

    -- * PreserveDevices
    PreserveDevices (..),

    -- * S3StorageClass
    S3StorageClass (..),

    -- * SmbSecurityDescriptorCopyFlags
    SmbSecurityDescriptorCopyFlags (..),

    -- * SmbVersion
    SmbVersion (..),

    -- * TaskExecutionStatus
    TaskExecutionStatus (..),

    -- * TaskFilterName
    TaskFilterName (..),

    -- * TaskQueueing
    TaskQueueing (..),

    -- * TaskStatus
    TaskStatus (..),

    -- * TransferMode
    TransferMode (..),

    -- * Uid
    Uid (..),

    -- * VerifyMode
    VerifyMode (..),

    -- * AgentListEntry
    AgentListEntry (..),
    newAgentListEntry,
    agentListEntry_status,
    agentListEntry_agentArn,
    agentListEntry_name,

    -- * Ec2Config
    Ec2Config (..),
    newEc2Config,
    ec2Config_subnetArn,
    ec2Config_securityGroupArns,

    -- * FilterRule
    FilterRule (..),
    newFilterRule,
    filterRule_filterType,
    filterRule_value,

    -- * LocationFilter
    LocationFilter (..),
    newLocationFilter,
    locationFilter_name,
    locationFilter_values,
    locationFilter_operator,

    -- * LocationListEntry
    LocationListEntry (..),
    newLocationListEntry,
    locationListEntry_locationUri,
    locationListEntry_locationArn,

    -- * NfsMountOptions
    NfsMountOptions (..),
    newNfsMountOptions,
    nfsMountOptions_version,

    -- * OnPremConfig
    OnPremConfig (..),
    newOnPremConfig,
    onPremConfig_agentArns,

    -- * Options
    Options (..),
    newOptions,
    options_atime,
    options_verifyMode,
    options_taskQueueing,
    options_logLevel,
    options_posixPermissions,
    options_mtime,
    options_uid,
    options_bytesPerSecond,
    options_securityDescriptorCopyFlags,
    options_gid,
    options_overwriteMode,
    options_transferMode,
    options_preserveDeletedFiles,
    options_preserveDevices,

    -- * PrivateLinkConfig
    PrivateLinkConfig (..),
    newPrivateLinkConfig,
    privateLinkConfig_securityGroupArns,
    privateLinkConfig_subnetArns,
    privateLinkConfig_privateLinkEndpoint,
    privateLinkConfig_vpcEndpointId,

    -- * S3Config
    S3Config (..),
    newS3Config,
    s3Config_bucketAccessRoleArn,

    -- * SmbMountOptions
    SmbMountOptions (..),
    newSmbMountOptions,
    smbMountOptions_version,

    -- * TagListEntry
    TagListEntry (..),
    newTagListEntry,
    tagListEntry_value,
    tagListEntry_key,

    -- * TaskExecutionListEntry
    TaskExecutionListEntry (..),
    newTaskExecutionListEntry,
    taskExecutionListEntry_status,
    taskExecutionListEntry_taskExecutionArn,

    -- * TaskExecutionResultDetail
    TaskExecutionResultDetail (..),
    newTaskExecutionResultDetail,
    taskExecutionResultDetail_prepareDuration,
    taskExecutionResultDetail_prepareStatus,
    taskExecutionResultDetail_verifyStatus,
    taskExecutionResultDetail_verifyDuration,
    taskExecutionResultDetail_totalDuration,
    taskExecutionResultDetail_transferStatus,
    taskExecutionResultDetail_errorCode,
    taskExecutionResultDetail_transferDuration,
    taskExecutionResultDetail_errorDetail,

    -- * TaskFilter
    TaskFilter (..),
    newTaskFilter,
    taskFilter_name,
    taskFilter_values,
    taskFilter_operator,

    -- * TaskListEntry
    TaskListEntry (..),
    newTaskListEntry,
    taskListEntry_status,
    taskListEntry_taskArn,
    taskListEntry_name,

    -- * TaskSchedule
    TaskSchedule (..),
    newTaskSchedule,
    taskSchedule_scheduleExpression,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DataSync.Types.AgentListEntry
import Network.AWS.DataSync.Types.AgentStatus
import Network.AWS.DataSync.Types.Atime
import Network.AWS.DataSync.Types.Ec2Config
import Network.AWS.DataSync.Types.EndpointType
import Network.AWS.DataSync.Types.FilterRule
import Network.AWS.DataSync.Types.FilterType
import Network.AWS.DataSync.Types.Gid
import Network.AWS.DataSync.Types.LocationFilter
import Network.AWS.DataSync.Types.LocationFilterName
import Network.AWS.DataSync.Types.LocationListEntry
import Network.AWS.DataSync.Types.LogLevel
import Network.AWS.DataSync.Types.Mtime
import Network.AWS.DataSync.Types.NfsMountOptions
import Network.AWS.DataSync.Types.NfsVersion
import Network.AWS.DataSync.Types.ObjectStorageServerProtocol
import Network.AWS.DataSync.Types.OnPremConfig
import Network.AWS.DataSync.Types.Operator
import Network.AWS.DataSync.Types.Options
import Network.AWS.DataSync.Types.OverwriteMode
import Network.AWS.DataSync.Types.PhaseStatus
import Network.AWS.DataSync.Types.PosixPermissions
import Network.AWS.DataSync.Types.PreserveDeletedFiles
import Network.AWS.DataSync.Types.PreserveDevices
import Network.AWS.DataSync.Types.PrivateLinkConfig
import Network.AWS.DataSync.Types.S3Config
import Network.AWS.DataSync.Types.S3StorageClass
import Network.AWS.DataSync.Types.SmbMountOptions
import Network.AWS.DataSync.Types.SmbSecurityDescriptorCopyFlags
import Network.AWS.DataSync.Types.SmbVersion
import Network.AWS.DataSync.Types.TagListEntry
import Network.AWS.DataSync.Types.TaskExecutionListEntry
import Network.AWS.DataSync.Types.TaskExecutionResultDetail
import Network.AWS.DataSync.Types.TaskExecutionStatus
import Network.AWS.DataSync.Types.TaskFilter
import Network.AWS.DataSync.Types.TaskFilterName
import Network.AWS.DataSync.Types.TaskListEntry
import Network.AWS.DataSync.Types.TaskQueueing
import Network.AWS.DataSync.Types.TaskSchedule
import Network.AWS.DataSync.Types.TaskStatus
import Network.AWS.DataSync.Types.TransferMode
import Network.AWS.DataSync.Types.Uid
import Network.AWS.DataSync.Types.VerifyMode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2018-11-09@ of the Amazon DataSync SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "DataSync",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "datasync",
      Core._serviceSigningName = "datasync",
      Core._serviceVersion = "2018-11-09",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "DataSync",
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

-- | This exception is thrown when the client submits a malformed request.
_InvalidRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"

-- | This exception is thrown when an error occurs in the DataSync service.
_InternalException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalException =
  Core._MatchServiceError
    defaultService
    "InternalException"
