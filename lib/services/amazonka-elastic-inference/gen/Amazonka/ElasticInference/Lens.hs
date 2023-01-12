{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ElasticInference.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticInference.Lens
  ( -- * Operations

    -- ** DescribeAcceleratorOfferings
    describeAcceleratorOfferings_acceleratorTypes,
    describeAcceleratorOfferings_locationType,
    describeAcceleratorOfferingsResponse_acceleratorTypeOfferings,
    describeAcceleratorOfferingsResponse_httpStatus,

    -- ** DescribeAcceleratorTypes
    describeAcceleratorTypesResponse_acceleratorTypes,
    describeAcceleratorTypesResponse_httpStatus,

    -- ** DescribeAccelerators
    describeAccelerators_acceleratorIds,
    describeAccelerators_filters,
    describeAccelerators_maxResults,
    describeAccelerators_nextToken,
    describeAcceleratorsResponse_acceleratorSet,
    describeAcceleratorsResponse_nextToken,
    describeAcceleratorsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- * Types

    -- ** AcceleratorType
    acceleratorType_acceleratorTypeName,
    acceleratorType_memoryInfo,
    acceleratorType_throughputInfo,

    -- ** AcceleratorTypeOffering
    acceleratorTypeOffering_acceleratorType,
    acceleratorTypeOffering_location,
    acceleratorTypeOffering_locationType,

    -- ** ElasticInferenceAccelerator
    elasticInferenceAccelerator_acceleratorHealth,
    elasticInferenceAccelerator_acceleratorId,
    elasticInferenceAccelerator_acceleratorType,
    elasticInferenceAccelerator_attachedResource,
    elasticInferenceAccelerator_availabilityZone,

    -- ** ElasticInferenceAcceleratorHealth
    elasticInferenceAcceleratorHealth_status,

    -- ** Filter
    filter_name,
    filter_values,

    -- ** KeyValuePair
    keyValuePair_key,
    keyValuePair_value,

    -- ** MemoryInfo
    memoryInfo_sizeInMiB,
  )
where

import Amazonka.ElasticInference.DescribeAcceleratorOfferings
import Amazonka.ElasticInference.DescribeAcceleratorTypes
import Amazonka.ElasticInference.DescribeAccelerators
import Amazonka.ElasticInference.ListTagsForResource
import Amazonka.ElasticInference.TagResource
import Amazonka.ElasticInference.Types.AcceleratorType
import Amazonka.ElasticInference.Types.AcceleratorTypeOffering
import Amazonka.ElasticInference.Types.ElasticInferenceAccelerator
import Amazonka.ElasticInference.Types.ElasticInferenceAcceleratorHealth
import Amazonka.ElasticInference.Types.Filter
import Amazonka.ElasticInference.Types.KeyValuePair
import Amazonka.ElasticInference.Types.MemoryInfo
import Amazonka.ElasticInference.UntagResource
