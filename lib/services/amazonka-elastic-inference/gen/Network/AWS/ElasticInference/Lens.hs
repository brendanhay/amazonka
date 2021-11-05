{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticInference.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticInference.Lens
  ( -- * Operations

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** DescribeAcceleratorOfferings
    describeAcceleratorOfferings_acceleratorTypes,
    describeAcceleratorOfferings_locationType,
    describeAcceleratorOfferingsResponse_acceleratorTypeOfferings,
    describeAcceleratorOfferingsResponse_httpStatus,

    -- ** DescribeAccelerators
    describeAccelerators_filters,
    describeAccelerators_nextToken,
    describeAccelerators_maxResults,
    describeAccelerators_acceleratorIds,
    describeAcceleratorsResponse_acceleratorSet,
    describeAcceleratorsResponse_nextToken,
    describeAcceleratorsResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** DescribeAcceleratorTypes
    describeAcceleratorTypesResponse_acceleratorTypes,
    describeAcceleratorTypesResponse_httpStatus,

    -- * Types

    -- ** AcceleratorType
    acceleratorType_throughputInfo,
    acceleratorType_memoryInfo,
    acceleratorType_acceleratorTypeName,

    -- ** AcceleratorTypeOffering
    acceleratorTypeOffering_acceleratorType,
    acceleratorTypeOffering_location,
    acceleratorTypeOffering_locationType,

    -- ** ElasticInferenceAccelerator
    elasticInferenceAccelerator_acceleratorType,
    elasticInferenceAccelerator_acceleratorId,
    elasticInferenceAccelerator_attachedResource,
    elasticInferenceAccelerator_acceleratorHealth,
    elasticInferenceAccelerator_availabilityZone,

    -- ** ElasticInferenceAcceleratorHealth
    elasticInferenceAcceleratorHealth_status,

    -- ** Filter
    filter_values,
    filter_name,

    -- ** KeyValuePair
    keyValuePair_value,
    keyValuePair_key,

    -- ** MemoryInfo
    memoryInfo_sizeInMiB,
  )
where

import Network.AWS.ElasticInference.DescribeAcceleratorOfferings
import Network.AWS.ElasticInference.DescribeAcceleratorTypes
import Network.AWS.ElasticInference.DescribeAccelerators
import Network.AWS.ElasticInference.ListTagsForResource
import Network.AWS.ElasticInference.TagResource
import Network.AWS.ElasticInference.Types.AcceleratorType
import Network.AWS.ElasticInference.Types.AcceleratorTypeOffering
import Network.AWS.ElasticInference.Types.ElasticInferenceAccelerator
import Network.AWS.ElasticInference.Types.ElasticInferenceAcceleratorHealth
import Network.AWS.ElasticInference.Types.Filter
import Network.AWS.ElasticInference.Types.KeyValuePair
import Network.AWS.ElasticInference.Types.MemoryInfo
import Network.AWS.ElasticInference.UntagResource
