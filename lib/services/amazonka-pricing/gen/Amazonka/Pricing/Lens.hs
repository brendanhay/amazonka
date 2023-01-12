{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Pricing.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pricing.Lens
  ( -- * Operations

    -- ** DescribeServices
    describeServices_formatVersion,
    describeServices_maxResults,
    describeServices_nextToken,
    describeServices_serviceCode,
    describeServicesResponse_formatVersion,
    describeServicesResponse_nextToken,
    describeServicesResponse_services,
    describeServicesResponse_httpStatus,

    -- ** GetAttributeValues
    getAttributeValues_maxResults,
    getAttributeValues_nextToken,
    getAttributeValues_serviceCode,
    getAttributeValues_attributeName,
    getAttributeValuesResponse_attributeValues,
    getAttributeValuesResponse_nextToken,
    getAttributeValuesResponse_httpStatus,

    -- ** GetProducts
    getProducts_filters,
    getProducts_formatVersion,
    getProducts_maxResults,
    getProducts_nextToken,
    getProducts_serviceCode,
    getProductsResponse_formatVersion,
    getProductsResponse_nextToken,
    getProductsResponse_priceList,
    getProductsResponse_httpStatus,

    -- * Types

    -- ** AttributeValue
    attributeValue_value,

    -- ** Filter
    filter_type,
    filter_field,
    filter_value,

    -- ** PricingService
    pricingService_attributeNames,
    pricingService_serviceCode,
  )
where

import Amazonka.Pricing.DescribeServices
import Amazonka.Pricing.GetAttributeValues
import Amazonka.Pricing.GetProducts
import Amazonka.Pricing.Types.AttributeValue
import Amazonka.Pricing.Types.Filter
import Amazonka.Pricing.Types.PricingService
