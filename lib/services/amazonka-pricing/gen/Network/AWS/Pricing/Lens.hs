{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pricing.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pricing.Lens
  ( -- * Operations

    -- ** GetProducts
    getProducts_nextToken,
    getProducts_maxResults,
    getProducts_serviceCode,
    getProducts_formatVersion,
    getProducts_filters,
    getProductsResponse_priceList,
    getProductsResponse_nextToken,
    getProductsResponse_formatVersion,
    getProductsResponse_httpStatus,

    -- ** DescribeServices
    describeServices_nextToken,
    describeServices_maxResults,
    describeServices_serviceCode,
    describeServices_formatVersion,
    describeServicesResponse_nextToken,
    describeServicesResponse_services,
    describeServicesResponse_formatVersion,
    describeServicesResponse_httpStatus,

    -- ** GetAttributeValues
    getAttributeValues_nextToken,
    getAttributeValues_maxResults,
    getAttributeValues_serviceCode,
    getAttributeValues_attributeName,
    getAttributeValuesResponse_nextToken,
    getAttributeValuesResponse_attributeValues,
    getAttributeValuesResponse_httpStatus,

    -- * Types

    -- ** AttributeValue
    attributeValue_value,

    -- ** Filter
    filter_type,
    filter_field,
    filter_value,

    -- ** PricingService
    pricingService_serviceCode,
    pricingService_attributeNames,
  )
where

import Network.AWS.Pricing.DescribeServices
import Network.AWS.Pricing.GetAttributeValues
import Network.AWS.Pricing.GetProducts
import Network.AWS.Pricing.Types.AttributeValue
import Network.AWS.Pricing.Types.Filter
import Network.AWS.Pricing.Types.PricingService
