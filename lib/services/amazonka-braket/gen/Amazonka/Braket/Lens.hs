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

    -- ** CancelQuantumTask
    cancelQuantumTask_clientToken,
    cancelQuantumTask_quantumTaskArn,
    cancelQuantumTaskResponse_httpStatus,
    cancelQuantumTaskResponse_cancellationStatus,
    cancelQuantumTaskResponse_quantumTaskArn,

    -- ** CreateQuantumTask
    createQuantumTask_tags,
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

    -- ** GetQuantumTask
    getQuantumTask_quantumTaskArn,
    getQuantumTaskResponse_tags,
    getQuantumTaskResponse_endedAt,
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

    -- ** DeviceSummary
    deviceSummary_deviceArn,
    deviceSummary_deviceName,
    deviceSummary_deviceStatus,
    deviceSummary_deviceType,
    deviceSummary_providerName,

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

    -- ** SearchDevicesFilter
    searchDevicesFilter_name,
    searchDevicesFilter_values,

    -- ** SearchQuantumTasksFilter
    searchQuantumTasksFilter_name,
    searchQuantumTasksFilter_operator,
    searchQuantumTasksFilter_values,
  )
where

import Amazonka.Braket.CancelQuantumTask
import Amazonka.Braket.CreateQuantumTask
import Amazonka.Braket.GetDevice
import Amazonka.Braket.GetQuantumTask
import Amazonka.Braket.ListTagsForResource
import Amazonka.Braket.SearchDevices
import Amazonka.Braket.SearchQuantumTasks
import Amazonka.Braket.TagResource
import Amazonka.Braket.Types.DeviceSummary
import Amazonka.Braket.Types.QuantumTaskSummary
import Amazonka.Braket.Types.SearchDevicesFilter
import Amazonka.Braket.Types.SearchQuantumTasksFilter
import Amazonka.Braket.UntagResource
