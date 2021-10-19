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

    -- ** GetAttributeValues
    getAttributeValues_nextToken,
    getAttributeValues_maxResults,
    getAttributeValues_serviceCode,
    getAttributeValues_attributeName,
    getAttributeValuesResponse_attributeValues,
    getAttributeValuesResponse_nextToken,
    getAttributeValuesResponse_httpStatus,

    -- ** DescribeServices
    describeServices_formatVersion,
    describeServices_nextToken,
    describeServices_serviceCode,
    describeServices_maxResults,
    describeServicesResponse_formatVersion,
    describeServicesResponse_nextToken,
    describeServicesResponse_services,
    describeServicesResponse_httpStatus,

    -- ** GetProducts
    getProducts_filters,
    getProducts_formatVersion,
    getProducts_nextToken,
    getProducts_serviceCode,
    getProducts_maxResults,
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

import Network.AWS.Pricing.DescribeServices
import Network.AWS.Pricing.GetAttributeValues
import Network.AWS.Pricing.GetProducts
import Network.AWS.Pricing.Types.AttributeValue
import Network.AWS.Pricing.Types.Filter
import Network.AWS.Pricing.Types.PricingService
