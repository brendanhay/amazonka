{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT1ClickDevices.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT1ClickDevices.Lens
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

import Network.AWS.IoT1ClickDevices.ClaimDevicesByClaimCode
import Network.AWS.IoT1ClickDevices.DescribeDevice
import Network.AWS.IoT1ClickDevices.FinalizeDeviceClaim
import Network.AWS.IoT1ClickDevices.GetDeviceMethods
import Network.AWS.IoT1ClickDevices.InitiateDeviceClaim
import Network.AWS.IoT1ClickDevices.InvokeDeviceMethod
import Network.AWS.IoT1ClickDevices.ListDeviceEvents
import Network.AWS.IoT1ClickDevices.ListDevices
import Network.AWS.IoT1ClickDevices.ListTagsForResource
import Network.AWS.IoT1ClickDevices.TagResource
import Network.AWS.IoT1ClickDevices.Types.Attributes
import Network.AWS.IoT1ClickDevices.Types.Device
import Network.AWS.IoT1ClickDevices.Types.DeviceDescription
import Network.AWS.IoT1ClickDevices.Types.DeviceEvent
import Network.AWS.IoT1ClickDevices.Types.DeviceMethod
import Network.AWS.IoT1ClickDevices.UnclaimDevice
import Network.AWS.IoT1ClickDevices.UntagResource
import Network.AWS.IoT1ClickDevices.UpdateDeviceState
