{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Braket.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Braket.Lens
  ( -- * Operations

    -- ** CancelJob
    cancelJob_jobArn,
    cancelJobResponse_httpStatus,
    cancelJobResponse_cancellationStatus,
    cancelJobResponse_jobArn,

    -- ** CancelQuantumTask
    cancelQuantumTask_clientToken,
    cancelQuantumTask_quantumTaskArn,
    cancelQuantumTaskResponse_httpStatus,
    cancelQuantumTaskResponse_cancellationStatus,
    cancelQuantumTaskResponse_quantumTaskArn,

    -- ** CreateJob
    createJob_tags,
    createJob_checkpointConfig,
    createJob_stoppingCondition,
    createJob_inputDataConfig,
    createJob_hyperParameters,
    createJob_algorithmSpecification,
    createJob_clientToken,
    createJob_deviceConfig,
    createJob_instanceConfig,
    createJob_jobName,
    createJob_outputDataConfig,
    createJob_roleArn,
    createJobResponse_httpStatus,
    createJobResponse_jobArn,

    -- ** CreateQuantumTask
    createQuantumTask_tags,
    createQuantumTask_jobToken,
    createQuantumTask_deviceParameters,
    createQuantumTask_action,
    createQuantumTask_clientToken,
    createQuantumTask_deviceArn,
    createQuantumTask_outputS3Bucket,
    createQuantumTask_outputS3KeyPrefix,
    createQuantumTask_shots,
    createQuantumTaskResponse_httpStatus,
    createQuantumTaskResponse_quantumTaskArn,

    -- ** GetDevice
    getDevice_deviceArn,
    getDeviceResponse_httpStatus,
    getDeviceResponse_deviceArn,
    getDeviceResponse_deviceCapabilities,
    getDeviceResponse_deviceName,
    getDeviceResponse_deviceStatus,
    getDeviceResponse_deviceType,
    getDeviceResponse_providerName,

    -- ** GetJob
    getJob_jobArn,
    getJobResponse_tags,
    getJobResponse_endedAt,
    getJobResponse_checkpointConfig,
    getJobResponse_deviceConfig,
    getJobResponse_startedAt,
    getJobResponse_stoppingCondition,
    getJobResponse_events,
    getJobResponse_inputDataConfig,
    getJobResponse_hyperParameters,
    getJobResponse_billableDuration,
    getJobResponse_failureReason,
    getJobResponse_httpStatus,
    getJobResponse_algorithmSpecification,
    getJobResponse_createdAt,
    getJobResponse_instanceConfig,
    getJobResponse_jobArn,
    getJobResponse_jobName,
    getJobResponse_outputDataConfig,
    getJobResponse_roleArn,
    getJobResponse_status,

    -- ** GetQuantumTask
    getQuantumTask_quantumTaskArn,
    getQuantumTaskResponse_tags,
    getQuantumTaskResponse_endedAt,
    getQuantumTaskResponse_jobArn,
    getQuantumTaskResponse_failureReason,
    getQuantumTaskResponse_httpStatus,
    getQuantumTaskResponse_createdAt,
    getQuantumTaskResponse_deviceArn,
    getQuantumTaskResponse_deviceParameters,
    getQuantumTaskResponse_outputS3Bucket,
    getQuantumTaskResponse_outputS3Directory,
    getQuantumTaskResponse_quantumTaskArn,
    getQuantumTaskResponse_shots,
    getQuantumTaskResponse_status,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** SearchDevices
    searchDevices_nextToken,
    searchDevices_maxResults,
    searchDevices_filters,
    searchDevicesResponse_nextToken,
    searchDevicesResponse_httpStatus,
    searchDevicesResponse_devices,

    -- ** SearchJobs
    searchJobs_nextToken,
    searchJobs_maxResults,
    searchJobs_filters,
    searchJobsResponse_nextToken,
    searchJobsResponse_httpStatus,
    searchJobsResponse_jobs,

    -- ** SearchQuantumTasks
    searchQuantumTasks_nextToken,
    searchQuantumTasks_maxResults,
    searchQuantumTasks_filters,
    searchQuantumTasksResponse_nextToken,
    searchQuantumTasksResponse_httpStatus,
    searchQuantumTasksResponse_quantumTasks,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- * Types

    -- ** AlgorithmSpecification
    algorithmSpecification_containerImage,
    algorithmSpecification_scriptModeConfig,

    -- ** ContainerImage
    containerImage_uri,

    -- ** DataSource
    dataSource_s3DataSource,

    -- ** DeviceConfig
    deviceConfig_device,

    -- ** DeviceSummary
    deviceSummary_deviceArn,
    deviceSummary_deviceName,
    deviceSummary_deviceStatus,
    deviceSummary_deviceType,
    deviceSummary_providerName,

    -- ** InputFileConfig
    inputFileConfig_contentType,
    inputFileConfig_channelName,
    inputFileConfig_dataSource,

    -- ** InstanceConfig
    instanceConfig_instanceCount,
    instanceConfig_instanceType,
    instanceConfig_volumeSizeInGb,

    -- ** JobCheckpointConfig
    jobCheckpointConfig_localPath,
    jobCheckpointConfig_s3Uri,

    -- ** JobEventDetails
    jobEventDetails_message,
    jobEventDetails_eventType,
    jobEventDetails_timeOfEvent,

    -- ** JobOutputDataConfig
    jobOutputDataConfig_kmsKeyId,
    jobOutputDataConfig_s3Path,

    -- ** JobStoppingCondition
    jobStoppingCondition_maxRuntimeInSeconds,

    -- ** JobSummary
    jobSummary_tags,
    jobSummary_endedAt,
    jobSummary_startedAt,
    jobSummary_createdAt,
    jobSummary_device,
    jobSummary_jobArn,
    jobSummary_jobName,
    jobSummary_status,

    -- ** QuantumTaskSummary
    quantumTaskSummary_tags,
    quantumTaskSummary_endedAt,
    quantumTaskSummary_createdAt,
    quantumTaskSummary_deviceArn,
    quantumTaskSummary_outputS3Bucket,
    quantumTaskSummary_outputS3Directory,
    quantumTaskSummary_quantumTaskArn,
    quantumTaskSummary_shots,
    quantumTaskSummary_status,

    -- ** S3DataSource
    s3DataSource_s3Uri,

    -- ** ScriptModeConfig
    scriptModeConfig_compressionType,
    scriptModeConfig_entryPoint,
    scriptModeConfig_s3Uri,

    -- ** SearchDevicesFilter
    searchDevicesFilter_name,
    searchDevicesFilter_values,

    -- ** SearchJobsFilter
    searchJobsFilter_name,
    searchJobsFilter_operator,
    searchJobsFilter_values,

    -- ** SearchQuantumTasksFilter
    searchQuantumTasksFilter_name,
    searchQuantumTasksFilter_operator,
    searchQuantumTasksFilter_values,
  )
where

import Amazonka.Braket.CancelJob
import Amazonka.Braket.CancelQuantumTask
import Amazonka.Braket.CreateJob
import Amazonka.Braket.CreateQuantumTask
import Amazonka.Braket.GetDevice
import Amazonka.Braket.GetJob
import Amazonka.Braket.GetQuantumTask
import Amazonka.Braket.ListTagsForResource
import Amazonka.Braket.SearchDevices
import Amazonka.Braket.SearchJobs
import Amazonka.Braket.SearchQuantumTasks
import Amazonka.Braket.TagResource
import Amazonka.Braket.Types.AlgorithmSpecification
import Amazonka.Braket.Types.ContainerImage
import Amazonka.Braket.Types.DataSource
import Amazonka.Braket.Types.DeviceConfig
import Amazonka.Braket.Types.DeviceSummary
import Amazonka.Braket.Types.InputFileConfig
import Amazonka.Braket.Types.InstanceConfig
import Amazonka.Braket.Types.JobCheckpointConfig
import Amazonka.Braket.Types.JobEventDetails
import Amazonka.Braket.Types.JobOutputDataConfig
import Amazonka.Braket.Types.JobStoppingCondition
import Amazonka.Braket.Types.JobSummary
import Amazonka.Braket.Types.QuantumTaskSummary
import Amazonka.Braket.Types.S3DataSource
import Amazonka.Braket.Types.ScriptModeConfig
import Amazonka.Braket.Types.SearchDevicesFilter
import Amazonka.Braket.Types.SearchJobsFilter
import Amazonka.Braket.Types.SearchQuantumTasksFilter
import Amazonka.Braket.UntagResource
