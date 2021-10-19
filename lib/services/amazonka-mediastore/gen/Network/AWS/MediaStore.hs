{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.MediaStore
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2017-09-01@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- An AWS Elemental MediaStore container is a namespace that holds folders
-- and objects. You use a container endpoint to create, read, and delete
-- objects.
module Network.AWS.MediaStore
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** PolicyNotFoundException
    _PolicyNotFoundException,

    -- ** CorsPolicyNotFoundException
    _CorsPolicyNotFoundException,

    -- ** ContainerInUseException
    _ContainerInUseException,

    -- ** InternalServerError
    _InternalServerError,

    -- ** ContainerNotFoundException
    _ContainerNotFoundException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** StopAccessLogging
    StopAccessLogging (StopAccessLogging'),
    newStopAccessLogging,
    StopAccessLoggingResponse (StopAccessLoggingResponse'),
    newStopAccessLoggingResponse,

    -- ** PutLifecyclePolicy
    PutLifecyclePolicy (PutLifecyclePolicy'),
    newPutLifecyclePolicy,
    PutLifecyclePolicyResponse (PutLifecyclePolicyResponse'),
    newPutLifecyclePolicyResponse,

    -- ** DeleteLifecyclePolicy
    DeleteLifecyclePolicy (DeleteLifecyclePolicy'),
    newDeleteLifecyclePolicy,
    DeleteLifecyclePolicyResponse (DeleteLifecyclePolicyResponse'),
    newDeleteLifecyclePolicyResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** CreateContainer
    CreateContainer (CreateContainer'),
    newCreateContainer,
    CreateContainerResponse (CreateContainerResponse'),
    newCreateContainerResponse,

    -- ** ListContainers (Paginated)
    ListContainers (ListContainers'),
    newListContainers,
    ListContainersResponse (ListContainersResponse'),
    newListContainersResponse,

    -- ** DeleteContainer
    DeleteContainer (DeleteContainer'),
    newDeleteContainer,
    DeleteContainerResponse (DeleteContainerResponse'),
    newDeleteContainerResponse,

    -- ** PutCorsPolicy
    PutCorsPolicy (PutCorsPolicy'),
    newPutCorsPolicy,
    PutCorsPolicyResponse (PutCorsPolicyResponse'),
    newPutCorsPolicyResponse,

    -- ** DeleteCorsPolicy
    DeleteCorsPolicy (DeleteCorsPolicy'),
    newDeleteCorsPolicy,
    DeleteCorsPolicyResponse (DeleteCorsPolicyResponse'),
    newDeleteCorsPolicyResponse,

    -- ** StartAccessLogging
    StartAccessLogging (StartAccessLogging'),
    newStartAccessLogging,
    StartAccessLoggingResponse (StartAccessLoggingResponse'),
    newStartAccessLoggingResponse,

    -- ** DescribeContainer
    DescribeContainer (DescribeContainer'),
    newDescribeContainer,
    DescribeContainerResponse (DescribeContainerResponse'),
    newDescribeContainerResponse,

    -- ** GetMetricPolicy
    GetMetricPolicy (GetMetricPolicy'),
    newGetMetricPolicy,
    GetMetricPolicyResponse (GetMetricPolicyResponse'),
    newGetMetricPolicyResponse,

    -- ** DeleteMetricPolicy
    DeleteMetricPolicy (DeleteMetricPolicy'),
    newDeleteMetricPolicy,
    DeleteMetricPolicyResponse (DeleteMetricPolicyResponse'),
    newDeleteMetricPolicyResponse,

    -- ** PutMetricPolicy
    PutMetricPolicy (PutMetricPolicy'),
    newPutMetricPolicy,
    PutMetricPolicyResponse (PutMetricPolicyResponse'),
    newPutMetricPolicyResponse,

    -- ** GetLifecyclePolicy
    GetLifecyclePolicy (GetLifecyclePolicy'),
    newGetLifecyclePolicy,
    GetLifecyclePolicyResponse (GetLifecyclePolicyResponse'),
    newGetLifecyclePolicyResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** GetCorsPolicy
    GetCorsPolicy (GetCorsPolicy'),
    newGetCorsPolicy,
    GetCorsPolicyResponse (GetCorsPolicyResponse'),
    newGetCorsPolicyResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** DeleteContainerPolicy
    DeleteContainerPolicy (DeleteContainerPolicy'),
    newDeleteContainerPolicy,
    DeleteContainerPolicyResponse (DeleteContainerPolicyResponse'),
    newDeleteContainerPolicyResponse,

    -- ** PutContainerPolicy
    PutContainerPolicy (PutContainerPolicy'),
    newPutContainerPolicy,
    PutContainerPolicyResponse (PutContainerPolicyResponse'),
    newPutContainerPolicyResponse,

    -- ** GetContainerPolicy
    GetContainerPolicy (GetContainerPolicy'),
    newGetContainerPolicy,
    GetContainerPolicyResponse (GetContainerPolicyResponse'),
    newGetContainerPolicyResponse,

    -- * Types

    -- ** ContainerLevelMetrics
    ContainerLevelMetrics (..),

    -- ** ContainerStatus
    ContainerStatus (..),

    -- ** MethodName
    MethodName (..),

    -- ** Container
    Container (Container'),
    newContainer,

    -- ** CorsRule
    CorsRule (CorsRule'),
    newCorsRule,

    -- ** MetricPolicy
    MetricPolicy (MetricPolicy'),
    newMetricPolicy,

    -- ** MetricPolicyRule
    MetricPolicyRule (MetricPolicyRule'),
    newMetricPolicyRule,

    -- ** Tag
    Tag (Tag'),
    newTag,
  )
where

import Network.AWS.MediaStore.CreateContainer
import Network.AWS.MediaStore.DeleteContainer
import Network.AWS.MediaStore.DeleteContainerPolicy
import Network.AWS.MediaStore.DeleteCorsPolicy
import Network.AWS.MediaStore.DeleteLifecyclePolicy
import Network.AWS.MediaStore.DeleteMetricPolicy
import Network.AWS.MediaStore.DescribeContainer
import Network.AWS.MediaStore.GetContainerPolicy
import Network.AWS.MediaStore.GetCorsPolicy
import Network.AWS.MediaStore.GetLifecyclePolicy
import Network.AWS.MediaStore.GetMetricPolicy
import Network.AWS.MediaStore.Lens
import Network.AWS.MediaStore.ListContainers
import Network.AWS.MediaStore.ListTagsForResource
import Network.AWS.MediaStore.PutContainerPolicy
import Network.AWS.MediaStore.PutCorsPolicy
import Network.AWS.MediaStore.PutLifecyclePolicy
import Network.AWS.MediaStore.PutMetricPolicy
import Network.AWS.MediaStore.StartAccessLogging
import Network.AWS.MediaStore.StopAccessLogging
import Network.AWS.MediaStore.TagResource
import Network.AWS.MediaStore.Types
import Network.AWS.MediaStore.UntagResource
import Network.AWS.MediaStore.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'MediaStore'.

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
