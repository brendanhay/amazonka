{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MediaStore.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaStore.Lens
  ( -- * Operations

    -- ** CreateContainer
    createContainer_tags,
    createContainer_containerName,
    createContainerResponse_httpStatus,
    createContainerResponse_container,

    -- ** DeleteContainer
    deleteContainer_containerName,
    deleteContainerResponse_httpStatus,

    -- ** DeleteContainerPolicy
    deleteContainerPolicy_containerName,
    deleteContainerPolicyResponse_httpStatus,

    -- ** DeleteCorsPolicy
    deleteCorsPolicy_containerName,
    deleteCorsPolicyResponse_httpStatus,

    -- ** DeleteLifecyclePolicy
    deleteLifecyclePolicy_containerName,
    deleteLifecyclePolicyResponse_httpStatus,

    -- ** DeleteMetricPolicy
    deleteMetricPolicy_containerName,
    deleteMetricPolicyResponse_httpStatus,

    -- ** DescribeContainer
    describeContainer_containerName,
    describeContainerResponse_container,
    describeContainerResponse_httpStatus,

    -- ** GetContainerPolicy
    getContainerPolicy_containerName,
    getContainerPolicyResponse_httpStatus,
    getContainerPolicyResponse_policy,

    -- ** GetCorsPolicy
    getCorsPolicy_containerName,
    getCorsPolicyResponse_httpStatus,
    getCorsPolicyResponse_corsPolicy,

    -- ** GetLifecyclePolicy
    getLifecyclePolicy_containerName,
    getLifecyclePolicyResponse_httpStatus,
    getLifecyclePolicyResponse_lifecyclePolicy,

    -- ** GetMetricPolicy
    getMetricPolicy_containerName,
    getMetricPolicyResponse_httpStatus,
    getMetricPolicyResponse_metricPolicy,

    -- ** ListContainers
    listContainers_maxResults,
    listContainers_nextToken,
    listContainersResponse_nextToken,
    listContainersResponse_httpStatus,
    listContainersResponse_containers,

    -- ** ListTagsForResource
    listTagsForResource_resource,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** PutContainerPolicy
    putContainerPolicy_containerName,
    putContainerPolicy_policy,
    putContainerPolicyResponse_httpStatus,

    -- ** PutCorsPolicy
    putCorsPolicy_containerName,
    putCorsPolicy_corsPolicy,
    putCorsPolicyResponse_httpStatus,

    -- ** PutLifecyclePolicy
    putLifecyclePolicy_containerName,
    putLifecyclePolicy_lifecyclePolicy,
    putLifecyclePolicyResponse_httpStatus,

    -- ** PutMetricPolicy
    putMetricPolicy_containerName,
    putMetricPolicy_metricPolicy,
    putMetricPolicyResponse_httpStatus,

    -- ** StartAccessLogging
    startAccessLogging_containerName,
    startAccessLoggingResponse_httpStatus,

    -- ** StopAccessLogging
    stopAccessLogging_containerName,
    stopAccessLoggingResponse_httpStatus,

    -- ** TagResource
    tagResource_resource,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resource,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- * Types

    -- ** Container
    container_arn,
    container_accessLoggingEnabled,
    container_creationTime,
    container_endpoint,
    container_name,
    container_status,

    -- ** CorsRule
    corsRule_allowedMethods,
    corsRule_exposeHeaders,
    corsRule_maxAgeSeconds,
    corsRule_allowedOrigins,
    corsRule_allowedHeaders,

    -- ** MetricPolicy
    metricPolicy_metricPolicyRules,
    metricPolicy_containerLevelMetrics,

    -- ** MetricPolicyRule
    metricPolicyRule_objectGroup,
    metricPolicyRule_objectGroupName,

    -- ** Tag
    tag_value,
    tag_key,
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
import Amazonka.MediaStore.ListContainers
import Amazonka.MediaStore.ListTagsForResource
import Amazonka.MediaStore.PutContainerPolicy
import Amazonka.MediaStore.PutCorsPolicy
import Amazonka.MediaStore.PutLifecyclePolicy
import Amazonka.MediaStore.PutMetricPolicy
import Amazonka.MediaStore.StartAccessLogging
import Amazonka.MediaStore.StopAccessLogging
import Amazonka.MediaStore.TagResource
import Amazonka.MediaStore.Types.Container
import Amazonka.MediaStore.Types.CorsRule
import Amazonka.MediaStore.Types.MetricPolicy
import Amazonka.MediaStore.Types.MetricPolicyRule
import Amazonka.MediaStore.Types.Tag
import Amazonka.MediaStore.UntagResource
