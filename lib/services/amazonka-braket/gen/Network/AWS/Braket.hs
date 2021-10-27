{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.Braket
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2019-09-01@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- The Amazon Braket API Reference provides information about the
-- operations and structures supported in Amazon Braket.
module Network.AWS.Braket
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ValidationException
    _ValidationException,

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** DeviceOfflineException
    _DeviceOfflineException,

    -- ** DeviceRetiredException
    _DeviceRetiredException,

    -- ** ConflictException
    _ConflictException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** InternalServiceException
    _InternalServiceException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** GetQuantumTask
    GetQuantumTask (GetQuantumTask'),
    newGetQuantumTask,
    GetQuantumTaskResponse (GetQuantumTaskResponse'),
    newGetQuantumTaskResponse,

    -- ** SearchQuantumTasks (Paginated)
    SearchQuantumTasks (SearchQuantumTasks'),
    newSearchQuantumTasks,
    SearchQuantumTasksResponse (SearchQuantumTasksResponse'),
    newSearchQuantumTasksResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** GetDevice
    GetDevice (GetDevice'),
    newGetDevice,
    GetDeviceResponse (GetDeviceResponse'),
    newGetDeviceResponse,

    -- ** CancelQuantumTask
    CancelQuantumTask (CancelQuantumTask'),
    newCancelQuantumTask,
    CancelQuantumTaskResponse (CancelQuantumTaskResponse'),
    newCancelQuantumTaskResponse,

    -- ** SearchDevices (Paginated)
    SearchDevices (SearchDevices'),
    newSearchDevices,
    SearchDevicesResponse (SearchDevicesResponse'),
    newSearchDevicesResponse,

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

    -- ** CreateQuantumTask
    CreateQuantumTask (CreateQuantumTask'),
    newCreateQuantumTask,
    CreateQuantumTaskResponse (CreateQuantumTaskResponse'),
    newCreateQuantumTaskResponse,

    -- * Types

    -- ** CancellationStatus
    CancellationStatus (..),

    -- ** DeviceStatus
    DeviceStatus (..),

    -- ** DeviceType
    DeviceType (..),

    -- ** QuantumTaskStatus
    QuantumTaskStatus (..),

    -- ** SearchQuantumTasksFilterOperator
    SearchQuantumTasksFilterOperator (..),

    -- ** DeviceSummary
    DeviceSummary (DeviceSummary'),
    newDeviceSummary,

    -- ** QuantumTaskSummary
    QuantumTaskSummary (QuantumTaskSummary'),
    newQuantumTaskSummary,

    -- ** SearchDevicesFilter
    SearchDevicesFilter (SearchDevicesFilter'),
    newSearchDevicesFilter,

    -- ** SearchQuantumTasksFilter
    SearchQuantumTasksFilter (SearchQuantumTasksFilter'),
    newSearchQuantumTasksFilter,
  )
where

import Network.AWS.Braket.CancelQuantumTask
import Network.AWS.Braket.CreateQuantumTask
import Network.AWS.Braket.GetDevice
import Network.AWS.Braket.GetQuantumTask
import Network.AWS.Braket.Lens
import Network.AWS.Braket.ListTagsForResource
import Network.AWS.Braket.SearchDevices
import Network.AWS.Braket.SearchQuantumTasks
import Network.AWS.Braket.TagResource
import Network.AWS.Braket.Types
import Network.AWS.Braket.UntagResource
import Network.AWS.Braket.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'Braket'.

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
