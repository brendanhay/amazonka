{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.IoT1ClickDevices.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT1ClickDevices.Lens
  ( -- * Operations

    -- ** GetDeviceMethods
    getDeviceMethods_deviceId,
    getDeviceMethodsResponse_deviceMethods,
    getDeviceMethodsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ClaimDevicesByClaimCode
    claimDevicesByClaimCode_claimCode,
    claimDevicesByClaimCodeResponse_claimCode,
    claimDevicesByClaimCodeResponse_total,
    claimDevicesByClaimCodeResponse_httpStatus,

    -- ** InitiateDeviceClaim
    initiateDeviceClaim_deviceId,
    initiateDeviceClaimResponse_state,
    initiateDeviceClaimResponse_httpStatus,

    -- ** InvokeDeviceMethod
    invokeDeviceMethod_deviceMethodParameters,
    invokeDeviceMethod_deviceMethod,
    invokeDeviceMethod_deviceId,
    invokeDeviceMethodResponse_deviceMethodResponse,
    invokeDeviceMethodResponse_httpStatus,

    -- ** DescribeDevice
    describeDevice_deviceId,
    describeDeviceResponse_deviceDescription,
    describeDeviceResponse_httpStatus,

    -- ** ListDeviceEvents
    listDeviceEvents_nextToken,
    listDeviceEvents_maxResults,
    listDeviceEvents_deviceId,
    listDeviceEvents_fromTimeStamp,
    listDeviceEvents_toTimeStamp,
    listDeviceEventsResponse_nextToken,
    listDeviceEventsResponse_events,
    listDeviceEventsResponse_httpStatus,

    -- ** FinalizeDeviceClaim
    finalizeDeviceClaim_tags,
    finalizeDeviceClaim_deviceId,
    finalizeDeviceClaimResponse_state,
    finalizeDeviceClaimResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,

    -- ** UntagResource
    untagResource_tagKeys,
    untagResource_resourceArn,

    -- ** UpdateDeviceState
    updateDeviceState_enabled,
    updateDeviceState_deviceId,
    updateDeviceStateResponse_httpStatus,

    -- ** UnclaimDevice
    unclaimDevice_deviceId,
    unclaimDeviceResponse_state,
    unclaimDeviceResponse_httpStatus,

    -- ** ListDevices
    listDevices_nextToken,
    listDevices_deviceType,
    listDevices_maxResults,
    listDevicesResponse_nextToken,
    listDevicesResponse_devices,
    listDevicesResponse_httpStatus,

    -- * Types

    -- ** Attributes

    -- ** Device
    device_attributes,
    device_deviceId,
    device_type,

    -- ** DeviceDescription
    deviceDescription_remainingLife,
    deviceDescription_enabled,
    deviceDescription_arn,
    deviceDescription_attributes,
    deviceDescription_deviceId,
    deviceDescription_type,
    deviceDescription_tags,

    -- ** DeviceEvent
    deviceEvent_stdEvent,
    deviceEvent_device,

    -- ** DeviceMethod
    deviceMethod_methodName,
    deviceMethod_deviceType,
  )
where

import Amazonka.IoT1ClickDevices.ClaimDevicesByClaimCode
import Amazonka.IoT1ClickDevices.DescribeDevice
import Amazonka.IoT1ClickDevices.FinalizeDeviceClaim
import Amazonka.IoT1ClickDevices.GetDeviceMethods
import Amazonka.IoT1ClickDevices.InitiateDeviceClaim
import Amazonka.IoT1ClickDevices.InvokeDeviceMethod
import Amazonka.IoT1ClickDevices.ListDeviceEvents
import Amazonka.IoT1ClickDevices.ListDevices
import Amazonka.IoT1ClickDevices.ListTagsForResource
import Amazonka.IoT1ClickDevices.TagResource
import Amazonka.IoT1ClickDevices.Types.Attributes
import Amazonka.IoT1ClickDevices.Types.Device
import Amazonka.IoT1ClickDevices.Types.DeviceDescription
import Amazonka.IoT1ClickDevices.Types.DeviceEvent
import Amazonka.IoT1ClickDevices.Types.DeviceMethod
import Amazonka.IoT1ClickDevices.UnclaimDevice
import Amazonka.IoT1ClickDevices.UntagResource
import Amazonka.IoT1ClickDevices.UpdateDeviceState
