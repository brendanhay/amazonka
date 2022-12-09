{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.IoT1ClickDevices
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2018-05-14@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Describes all of the AWS IoT 1-Click device-related API operations for
-- the service. Also provides sample requests, responses, and errors for
-- the supported web services protocols.
module Amazonka.IoT1ClickDevices
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ForbiddenException
    _ForbiddenException,

    -- ** InternalFailureException
    _InternalFailureException,

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- ** PreconditionFailedException
    _PreconditionFailedException,

    -- ** RangeNotSatisfiableException
    _RangeNotSatisfiableException,

    -- ** ResourceConflictException
    _ResourceConflictException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ClaimDevicesByClaimCode
    ClaimDevicesByClaimCode (ClaimDevicesByClaimCode'),
    newClaimDevicesByClaimCode,
    ClaimDevicesByClaimCodeResponse (ClaimDevicesByClaimCodeResponse'),
    newClaimDevicesByClaimCodeResponse,

    -- ** DescribeDevice
    DescribeDevice (DescribeDevice'),
    newDescribeDevice,
    DescribeDeviceResponse (DescribeDeviceResponse'),
    newDescribeDeviceResponse,

    -- ** FinalizeDeviceClaim
    FinalizeDeviceClaim (FinalizeDeviceClaim'),
    newFinalizeDeviceClaim,
    FinalizeDeviceClaimResponse (FinalizeDeviceClaimResponse'),
    newFinalizeDeviceClaimResponse,

    -- ** GetDeviceMethods
    GetDeviceMethods (GetDeviceMethods'),
    newGetDeviceMethods,
    GetDeviceMethodsResponse (GetDeviceMethodsResponse'),
    newGetDeviceMethodsResponse,

    -- ** InitiateDeviceClaim
    InitiateDeviceClaim (InitiateDeviceClaim'),
    newInitiateDeviceClaim,
    InitiateDeviceClaimResponse (InitiateDeviceClaimResponse'),
    newInitiateDeviceClaimResponse,

    -- ** InvokeDeviceMethod
    InvokeDeviceMethod (InvokeDeviceMethod'),
    newInvokeDeviceMethod,
    InvokeDeviceMethodResponse (InvokeDeviceMethodResponse'),
    newInvokeDeviceMethodResponse,

    -- ** ListDeviceEvents (Paginated)
    ListDeviceEvents (ListDeviceEvents'),
    newListDeviceEvents,
    ListDeviceEventsResponse (ListDeviceEventsResponse'),
    newListDeviceEventsResponse,

    -- ** ListDevices (Paginated)
    ListDevices (ListDevices'),
    newListDevices,
    ListDevicesResponse (ListDevicesResponse'),
    newListDevicesResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UnclaimDevice
    UnclaimDevice (UnclaimDevice'),
    newUnclaimDevice,
    UnclaimDeviceResponse (UnclaimDeviceResponse'),
    newUnclaimDeviceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateDeviceState
    UpdateDeviceState (UpdateDeviceState'),
    newUpdateDeviceState,
    UpdateDeviceStateResponse (UpdateDeviceStateResponse'),
    newUpdateDeviceStateResponse,

    -- * Types

    -- ** Attributes
    Attributes (Attributes'),
    newAttributes,

    -- ** Device
    Device (Device'),
    newDevice,

    -- ** DeviceDescription
    DeviceDescription (DeviceDescription'),
    newDeviceDescription,

    -- ** DeviceEvent
    DeviceEvent (DeviceEvent'),
    newDeviceEvent,

    -- ** DeviceMethod
    DeviceMethod (DeviceMethod'),
    newDeviceMethod,
  )
where

import Amazonka.IoT1ClickDevices.ClaimDevicesByClaimCode
import Amazonka.IoT1ClickDevices.DescribeDevice
import Amazonka.IoT1ClickDevices.FinalizeDeviceClaim
import Amazonka.IoT1ClickDevices.GetDeviceMethods
import Amazonka.IoT1ClickDevices.InitiateDeviceClaim
import Amazonka.IoT1ClickDevices.InvokeDeviceMethod
import Amazonka.IoT1ClickDevices.Lens
import Amazonka.IoT1ClickDevices.ListDeviceEvents
import Amazonka.IoT1ClickDevices.ListDevices
import Amazonka.IoT1ClickDevices.ListTagsForResource
import Amazonka.IoT1ClickDevices.TagResource
import Amazonka.IoT1ClickDevices.Types
import Amazonka.IoT1ClickDevices.UnclaimDevice
import Amazonka.IoT1ClickDevices.UntagResource
import Amazonka.IoT1ClickDevices.UpdateDeviceState
import Amazonka.IoT1ClickDevices.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'IoT1ClickDevices'.

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
