{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Pricing.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pricing.Lens
  ( -- * Operations

    -- ** DescribeServices
    describeServices_nextToken,
    describeServices_formatVersion,
    describeServices_serviceCode,
    describeServices_maxResults,
    describeServicesResponse_nextToken,
    describeServicesResponse_formatVersion,
    describeServicesResponse_services,
    describeServicesResponse_httpStatus,

    -- ** GetAttributeValues
    getAttributeValues_nextToken,
    getAttributeValues_maxResults,
    getAttributeValues_serviceCode,
    getAttributeValues_attributeName,
    getAttributeValuesResponse_nextToken,
    getAttributeValuesResponse_attributeValues,
    getAttributeValuesResponse_httpStatus,

    -- ** GetProducts
    getProducts_nextToken,
    getProducts_formatVersion,
    getProducts_filters,
    getProducts_maxResults,
    getProducts_serviceCode,
    getProductsResponse_nextToken,
    getProductsResponse_formatVersion,
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
