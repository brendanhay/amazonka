{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MediaStore.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaStore.Lens
  ( -- * Operations

    -- ** StopAccessLogging
    stopAccessLogging_containerName,
    stopAccessLoggingResponse_httpStatus,

    -- ** PutLifecyclePolicy
    putLifecyclePolicy_containerName,
    putLifecyclePolicy_lifecyclePolicy,
    putLifecyclePolicyResponse_httpStatus,

    -- ** DeleteLifecyclePolicy
    deleteLifecyclePolicy_containerName,
    deleteLifecyclePolicyResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resource,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** CreateContainer
    createContainer_tags,
    createContainer_containerName,
    createContainerResponse_httpStatus,
    createContainerResponse_container,

    -- ** ListContainers
    listContainers_nextToken,
    listContainers_maxResults,
    listContainersResponse_nextToken,
    listContainersResponse_httpStatus,
    listContainersResponse_containers,

    -- ** DeleteContainer
    deleteContainer_containerName,
    deleteContainerResponse_httpStatus,

    -- ** PutCorsPolicy
    putCorsPolicy_containerName,
    putCorsPolicy_corsPolicy,
    putCorsPolicyResponse_httpStatus,

    -- ** DeleteCorsPolicy
    deleteCorsPolicy_containerName,
    deleteCorsPolicyResponse_httpStatus,

    -- ** StartAccessLogging
    startAccessLogging_containerName,
    startAccessLoggingResponse_httpStatus,

    -- ** DescribeContainer
    describeContainer_containerName,
    describeContainerResponse_container,
    describeContainerResponse_httpStatus,

    -- ** GetMetricPolicy
    getMetricPolicy_containerName,
    getMetricPolicyResponse_httpStatus,
    getMetricPolicyResponse_metricPolicy,

    -- ** DeleteMetricPolicy
    deleteMetricPolicy_containerName,
    deleteMetricPolicyResponse_httpStatus,

    -- ** PutMetricPolicy
    putMetricPolicy_containerName,
    putMetricPolicy_metricPolicy,
    putMetricPolicyResponse_httpStatus,

    -- ** GetLifecyclePolicy
    getLifecyclePolicy_containerName,
    getLifecyclePolicyResponse_httpStatus,
    getLifecyclePolicyResponse_lifecyclePolicy,

    -- ** TagResource
    tagResource_resource,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** GetCorsPolicy
    getCorsPolicy_containerName,
    getCorsPolicyResponse_httpStatus,
    getCorsPolicyResponse_corsPolicy,

    -- ** UntagResource
    untagResource_resource,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** DeleteContainerPolicy
    deleteContainerPolicy_containerName,
    deleteContainerPolicyResponse_httpStatus,

    -- ** PutContainerPolicy
    putContainerPolicy_containerName,
    putContainerPolicy_policy,
    putContainerPolicyResponse_httpStatus,

    -- ** GetContainerPolicy
    getContainerPolicy_containerName,
    getContainerPolicyResponse_httpStatus,
    getContainerPolicyResponse_policy,

    -- * Types

    -- ** Container
    container_creationTime,
    container_status,
    container_accessLoggingEnabled,
    container_arn,
    container_name,
    container_endpoint,

    -- ** CorsRule
    corsRule_allowedMethods,
    corsRule_maxAgeSeconds,
    corsRule_exposeHeaders,
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
