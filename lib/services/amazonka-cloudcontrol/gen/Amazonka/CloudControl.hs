{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.CloudControl
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2021-09-30@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- For more information about Amazon Web Services Cloud Control API, see
-- the
-- <https://docs.aws.amazon.com/cloudcontrolapi/latest/userguide/what-is-cloudcontrolapi.html Amazon Web Services Cloud Control API User Guide>.
module Amazonka.CloudControl
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AlreadyExistsException
    _AlreadyExistsException,

    -- ** ClientTokenConflictException
    _ClientTokenConflictException,

    -- ** ConcurrentModificationException
    _ConcurrentModificationException,

    -- ** ConcurrentOperationException
    _ConcurrentOperationException,

    -- ** GeneralServiceException
    _GeneralServiceException,

    -- ** HandlerFailureException
    _HandlerFailureException,

    -- ** HandlerInternalFailureException
    _HandlerInternalFailureException,

    -- ** InvalidCredentialsException
    _InvalidCredentialsException,

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- ** NetworkFailureException
    _NetworkFailureException,

    -- ** NotStabilizedException
    _NotStabilizedException,

    -- ** NotUpdatableException
    _NotUpdatableException,

    -- ** PrivateTypeException
    _PrivateTypeException,

    -- ** RequestTokenNotFoundException
    _RequestTokenNotFoundException,

    -- ** ResourceConflictException
    _ResourceConflictException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ServiceInternalErrorException
    _ServiceInternalErrorException,

    -- ** ServiceLimitExceededException
    _ServiceLimitExceededException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** TypeNotFoundException
    _TypeNotFoundException,

    -- ** UnsupportedActionException
    _UnsupportedActionException,

    -- * Waiters
    -- $waiters

    -- ** ResourceRequestSuccess
    newResourceRequestSuccess,

    -- * Operations
    -- $operations

    -- ** CancelResourceRequest
    CancelResourceRequest (CancelResourceRequest'),
    newCancelResourceRequest,
    CancelResourceRequestResponse (CancelResourceRequestResponse'),
    newCancelResourceRequestResponse,

    -- ** CreateResource
    CreateResource (CreateResource'),
    newCreateResource,
    CreateResourceResponse (CreateResourceResponse'),
    newCreateResourceResponse,

    -- ** DeleteResource
    DeleteResource (DeleteResource'),
    newDeleteResource,
    DeleteResourceResponse (DeleteResourceResponse'),
    newDeleteResourceResponse,

    -- ** GetResource
    GetResource (GetResource'),
    newGetResource,
    GetResourceResponse (GetResourceResponse'),
    newGetResourceResponse,

    -- ** GetResourceRequestStatus
    GetResourceRequestStatus (GetResourceRequestStatus'),
    newGetResourceRequestStatus,
    GetResourceRequestStatusResponse (GetResourceRequestStatusResponse'),
    newGetResourceRequestStatusResponse,

    -- ** ListResourceRequests (Paginated)
    ListResourceRequests (ListResourceRequests'),
    newListResourceRequests,
    ListResourceRequestsResponse (ListResourceRequestsResponse'),
    newListResourceRequestsResponse,

    -- ** ListResources (Paginated)
    ListResources (ListResources'),
    newListResources,
    ListResourcesResponse (ListResourcesResponse'),
    newListResourcesResponse,

    -- ** UpdateResource
    UpdateResource (UpdateResource'),
    newUpdateResource,
    UpdateResourceResponse (UpdateResourceResponse'),
    newUpdateResourceResponse,

    -- * Types

    -- ** HandlerErrorCode
    HandlerErrorCode (..),

    -- ** Operation
    Operation (..),

    -- ** OperationStatus
    OperationStatus (..),

    -- ** ProgressEvent
    ProgressEvent (ProgressEvent'),
    newProgressEvent,

    -- ** ResourceDescription
    ResourceDescription (ResourceDescription'),
    newResourceDescription,

    -- ** ResourceRequestStatusFilter
    ResourceRequestStatusFilter (ResourceRequestStatusFilter'),
    newResourceRequestStatusFilter,
  )
where

import Amazonka.CloudControl.CancelResourceRequest
import Amazonka.CloudControl.CreateResource
import Amazonka.CloudControl.DeleteResource
import Amazonka.CloudControl.GetResource
import Amazonka.CloudControl.GetResourceRequestStatus
import Amazonka.CloudControl.Lens
import Amazonka.CloudControl.ListResourceRequests
import Amazonka.CloudControl.ListResources
import Amazonka.CloudControl.Types
import Amazonka.CloudControl.UpdateResource
import Amazonka.CloudControl.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'CloudControl'.

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
