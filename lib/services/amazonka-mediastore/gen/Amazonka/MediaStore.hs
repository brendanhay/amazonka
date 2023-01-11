{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.MediaStore
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.MediaStore
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ContainerInUseException
    _ContainerInUseException,

    -- ** ContainerNotFoundException
    _ContainerNotFoundException,

    -- ** CorsPolicyNotFoundException
    _CorsPolicyNotFoundException,

    -- ** InternalServerError
    _InternalServerError,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** PolicyNotFoundException
    _PolicyNotFoundException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateContainer
    CreateContainer (CreateContainer'),
    newCreateContainer,
    CreateContainerResponse (CreateContainerResponse'),
    newCreateContainerResponse,

    -- ** DeleteContainer
    DeleteContainer (DeleteContainer'),
    newDeleteContainer,
    DeleteContainerResponse (DeleteContainerResponse'),
    newDeleteContainerResponse,

    -- ** DeleteContainerPolicy
    DeleteContainerPolicy (DeleteContainerPolicy'),
    newDeleteContainerPolicy,
    DeleteContainerPolicyResponse (DeleteContainerPolicyResponse'),
    newDeleteContainerPolicyResponse,

    -- ** DeleteCorsPolicy
    DeleteCorsPolicy (DeleteCorsPolicy'),
    newDeleteCorsPolicy,
    DeleteCorsPolicyResponse (DeleteCorsPolicyResponse'),
    newDeleteCorsPolicyResponse,

    -- ** DeleteLifecyclePolicy
    DeleteLifecyclePolicy (DeleteLifecyclePolicy'),
    newDeleteLifecyclePolicy,
    DeleteLifecyclePolicyResponse (DeleteLifecyclePolicyResponse'),
    newDeleteLifecyclePolicyResponse,

    -- ** DeleteMetricPolicy
    DeleteMetricPolicy (DeleteMetricPolicy'),
    newDeleteMetricPolicy,
    DeleteMetricPolicyResponse (DeleteMetricPolicyResponse'),
    newDeleteMetricPolicyResponse,

    -- ** DescribeContainer
    DescribeContainer (DescribeContainer'),
    newDescribeContainer,
    DescribeContainerResponse (DescribeContainerResponse'),
    newDescribeContainerResponse,

    -- ** GetContainerPolicy
    GetContainerPolicy (GetContainerPolicy'),
    newGetContainerPolicy,
    GetContainerPolicyResponse (GetContainerPolicyResponse'),
    newGetContainerPolicyResponse,

    -- ** GetCorsPolicy
    GetCorsPolicy (GetCorsPolicy'),
    newGetCorsPolicy,
    GetCorsPolicyResponse (GetCorsPolicyResponse'),
    newGetCorsPolicyResponse,

    -- ** GetLifecyclePolicy
    GetLifecyclePolicy (GetLifecyclePolicy'),
    newGetLifecyclePolicy,
    GetLifecyclePolicyResponse (GetLifecyclePolicyResponse'),
    newGetLifecyclePolicyResponse,

    -- ** GetMetricPolicy
    GetMetricPolicy (GetMetricPolicy'),
    newGetMetricPolicy,
    GetMetricPolicyResponse (GetMetricPolicyResponse'),
    newGetMetricPolicyResponse,

    -- ** ListContainers (Paginated)
    ListContainers (ListContainers'),
    newListContainers,
    ListContainersResponse (ListContainersResponse'),
    newListContainersResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** PutContainerPolicy
    PutContainerPolicy (PutContainerPolicy'),
    newPutContainerPolicy,
    PutContainerPolicyResponse (PutContainerPolicyResponse'),
    newPutContainerPolicyResponse,

    -- ** PutCorsPolicy
    PutCorsPolicy (PutCorsPolicy'),
    newPutCorsPolicy,
    PutCorsPolicyResponse (PutCorsPolicyResponse'),
    newPutCorsPolicyResponse,

    -- ** PutLifecyclePolicy
    PutLifecyclePolicy (PutLifecyclePolicy'),
    newPutLifecyclePolicy,
    PutLifecyclePolicyResponse (PutLifecyclePolicyResponse'),
    newPutLifecyclePolicyResponse,

    -- ** PutMetricPolicy
    PutMetricPolicy (PutMetricPolicy'),
    newPutMetricPolicy,
    PutMetricPolicyResponse (PutMetricPolicyResponse'),
    newPutMetricPolicyResponse,

    -- ** StartAccessLogging
    StartAccessLogging (StartAccessLogging'),
    newStartAccessLogging,
    StartAccessLoggingResponse (StartAccessLoggingResponse'),
    newStartAccessLoggingResponse,

    -- ** StopAccessLogging
    StopAccessLogging (StopAccessLogging'),
    newStopAccessLogging,
    StopAccessLoggingResponse (StopAccessLoggingResponse'),
    newStopAccessLoggingResponse,

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

import Amazonka.MediaStore.CreateContainer
import Amazonka.MediaStore.DeleteContainer
import Amazonka.MediaStore.DeleteContainerPolicy
import Amazonka.MediaStore.DeleteCorsPolicy
import Amazonka.MediaStore.DeleteLifecyclePolicy
import Amazonka.MediaStore.DeleteMetricPolicy
import Amazonka.MediaStore.DescribeContainer
import Amazonka.MediaStore.GetContainerPolicy
import Amazonka.MediaStore.GetCorsPolicy
import Amazonka.MediaStore.GetLifecyclePolicy
import Amazonka.MediaStore.GetMetricPolicy
import Amazonka.MediaStore.Lens
import Amazonka.MediaStore.ListContainers
import Amazonka.MediaStore.ListTagsForResource
import Amazonka.MediaStore.PutContainerPolicy
import Amazonka.MediaStore.PutCorsPolicy
import Amazonka.MediaStore.PutLifecyclePolicy
import Amazonka.MediaStore.PutMetricPolicy
import Amazonka.MediaStore.StartAccessLogging
import Amazonka.MediaStore.StopAccessLogging
import Amazonka.MediaStore.TagResource
import Amazonka.MediaStore.Types
import Amazonka.MediaStore.UntagResource
import Amazonka.MediaStore.Waiters

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
