{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStore.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaStore.Lens
  ( -- * Operations

    -- ** PutLifecyclePolicy
    putLifecyclePolicy_containerName,
    putLifecyclePolicy_lifecyclePolicy,
    putLifecyclePolicyResponse_httpStatus,

    -- ** PutCorsPolicy
    putCorsPolicy_containerName,
    putCorsPolicy_corsPolicy,
    putCorsPolicyResponse_httpStatus,

    -- ** DeleteContainer
    deleteContainer_containerName,
    deleteContainerResponse_httpStatus,

    -- ** PutContainerPolicy
    putContainerPolicy_containerName,
    putContainerPolicy_policy,
    putContainerPolicyResponse_httpStatus,

    -- ** GetCorsPolicy
    getCorsPolicy_containerName,
    getCorsPolicyResponse_httpStatus,
    getCorsPolicyResponse_corsPolicy,

    -- ** UntagResource
    untagResource_resource,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** TagResource
    tagResource_resource,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** DeleteMetricPolicy
    deleteMetricPolicy_containerName,
    deleteMetricPolicyResponse_httpStatus,

    -- ** DescribeContainer
    describeContainer_containerName,
    describeContainerResponse_container,
    describeContainerResponse_httpStatus,

    -- ** GetMetricPolicy
    getMetricPolicy_containerName,
    getMetricPolicyResponse_httpStatus,
    getMetricPolicyResponse_metricPolicy,

    -- ** StopAccessLogging
    stopAccessLogging_containerName,
    stopAccessLoggingResponse_httpStatus,

    -- ** StartAccessLogging
    startAccessLogging_containerName,
    startAccessLoggingResponse_httpStatus,

    -- ** DeleteLifecyclePolicy
    deleteLifecyclePolicy_containerName,
    deleteLifecyclePolicyResponse_httpStatus,

    -- ** DeleteCorsPolicy
    deleteCorsPolicy_containerName,
    deleteCorsPolicyResponse_httpStatus,

    -- ** GetContainerPolicy
    getContainerPolicy_containerName,
    getContainerPolicyResponse_httpStatus,
    getContainerPolicyResponse_policy,

    -- ** DeleteContainerPolicy
    deleteContainerPolicy_containerName,
    deleteContainerPolicyResponse_httpStatus,

    -- ** ListContainers
    listContainers_nextToken,
    listContainers_maxResults,
    listContainersResponse_nextToken,
    listContainersResponse_httpStatus,
    listContainersResponse_containers,

    -- ** CreateContainer
    createContainer_tags,
    createContainer_containerName,
    createContainerResponse_httpStatus,
    createContainerResponse_container,

    -- ** GetLifecyclePolicy
    getLifecyclePolicy_containerName,
    getLifecyclePolicyResponse_httpStatus,
    getLifecyclePolicyResponse_lifecyclePolicy,

    -- ** PutMetricPolicy
    putMetricPolicy_containerName,
    putMetricPolicy_metricPolicy,
    putMetricPolicyResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resource,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- * Types

    -- ** Container
    container_creationTime,
    container_status,
    container_arn,
    container_accessLoggingEnabled,
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
import Network.AWS.MediaStore.ListContainers
import Network.AWS.MediaStore.ListTagsForResource
import Network.AWS.MediaStore.PutContainerPolicy
import Network.AWS.MediaStore.PutCorsPolicy
import Network.AWS.MediaStore.PutLifecyclePolicy
import Network.AWS.MediaStore.PutMetricPolicy
import Network.AWS.MediaStore.StartAccessLogging
import Network.AWS.MediaStore.StopAccessLogging
import Network.AWS.MediaStore.TagResource
import Network.AWS.MediaStore.Types.Container
import Network.AWS.MediaStore.Types.CorsRule
import Network.AWS.MediaStore.Types.MetricPolicy
import Network.AWS.MediaStore.Types.MetricPolicyRule
import Network.AWS.MediaStore.Types.Tag
import Network.AWS.MediaStore.UntagResource
