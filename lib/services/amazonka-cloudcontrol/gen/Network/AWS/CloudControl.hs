{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.CloudControl
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2021-09-30@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Use Amazon Web Services Cloud Control API to create, read, update,
-- delete, and list (CRUD-L) your cloud resources that belong to a wide
-- range of services--both Amazon Web Services and third-party. With the
-- Cloud Control API standardized set of application programming interfaces
-- (APIs), you can perform CRUD-L operations on any supported resources in
-- your Amazon Web Services account. Using Cloud Control API, you won\'t
-- have to generate code or scripts specific to each individual service
-- responsible for those resources.
--
-- For more information about Amazon Web Services Cloud Control API, see
-- the
-- <https://docs.aws.amazon.com/cloudcontrolapi/latest/userguide/what-is-cloudcontrolapi.html Amazon Web Services Cloud Control API User Guide>.
module Network.AWS.CloudControl
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** TypeNotFoundException
    _TypeNotFoundException,

    -- ** GeneralServiceException
    _GeneralServiceException,

    -- ** HandlerFailureException
    _HandlerFailureException,

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- ** PrivateTypeException
    _PrivateTypeException,

    -- ** ServiceInternalErrorException
    _ServiceInternalErrorException,

    -- ** HandlerInternalFailureException
    _HandlerInternalFailureException,

    -- ** ClientTokenConflictException
    _ClientTokenConflictException,

    -- ** RequestTokenNotFoundException
    _RequestTokenNotFoundException,

    -- ** NetworkFailureException
    _NetworkFailureException,

    -- ** NotStabilizedException
    _NotStabilizedException,

    -- ** NotUpdatableException
    _NotUpdatableException,

    -- ** UnsupportedActionException
    _UnsupportedActionException,

    -- ** ServiceLimitExceededException
    _ServiceLimitExceededException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** ConcurrentModificationException
    _ConcurrentModificationException,

    -- ** ResourceConflictException
    _ResourceConflictException,

    -- ** InvalidCredentialsException
    _InvalidCredentialsException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ConcurrentOperationException
    _ConcurrentOperationException,

    -- ** AlreadyExistsException
    _AlreadyExistsException,

    -- * Waiters
    -- $waiters

    -- ** ResourceRequestSuccess
    newResourceRequestSuccess,

    -- * Operations
    -- $operations

    -- ** GetResource
    GetResource (GetResource'),
    newGetResource,
    GetResourceResponse (GetResourceResponse'),
    newGetResourceResponse,

    -- ** ListResourceRequests
    ListResourceRequests (ListResourceRequests'),
    newListResourceRequests,
    ListResourceRequestsResponse (ListResourceRequestsResponse'),
    newListResourceRequestsResponse,

    -- ** CancelResourceRequest
    CancelResourceRequest (CancelResourceRequest'),
    newCancelResourceRequest,
    CancelResourceRequestResponse (CancelResourceRequestResponse'),
    newCancelResourceRequestResponse,

    -- ** DeleteResource
    DeleteResource (DeleteResource'),
    newDeleteResource,
    DeleteResourceResponse (DeleteResourceResponse'),
    newDeleteResourceResponse,

    -- ** UpdateResource
    UpdateResource (UpdateResource'),
    newUpdateResource,
    UpdateResourceResponse (UpdateResourceResponse'),
    newUpdateResourceResponse,

    -- ** ListResources
    ListResources (ListResources'),
    newListResources,
    ListResourcesResponse (ListResourcesResponse'),
    newListResourcesResponse,

    -- ** CreateResource
    CreateResource (CreateResource'),
    newCreateResource,
    CreateResourceResponse (CreateResourceResponse'),
    newCreateResourceResponse,

    -- ** GetResourceRequestStatus
    GetResourceRequestStatus (GetResourceRequestStatus'),
    newGetResourceRequestStatus,
    GetResourceRequestStatusResponse (GetResourceRequestStatusResponse'),
    newGetResourceRequestStatusResponse,

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

import Network.AWS.CloudControl.CancelResourceRequest
import Network.AWS.CloudControl.CreateResource
import Network.AWS.CloudControl.DeleteResource
import Network.AWS.CloudControl.GetResource
import Network.AWS.CloudControl.GetResourceRequestStatus
import Network.AWS.CloudControl.Lens
import Network.AWS.CloudControl.ListResourceRequests
import Network.AWS.CloudControl.ListResources
import Network.AWS.CloudControl.Types
import Network.AWS.CloudControl.UpdateResource
import Network.AWS.CloudControl.Waiters

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
