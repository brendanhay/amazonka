{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Outposts.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Outposts.Lens
  ( -- * Operations

    -- ** DeleteOutpost
    deleteOutpost_outpostId,
    deleteOutpostResponse_httpStatus,

    -- ** DeleteSite
    deleteSite_siteId,
    deleteSiteResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListOutposts
    listOutposts_availabilityZoneFilter,
    listOutposts_lifeCycleStatusFilter,
    listOutposts_nextToken,
    listOutposts_availabilityZoneIdFilter,
    listOutposts_maxResults,
    listOutpostsResponse_nextToken,
    listOutpostsResponse_outposts,
    listOutpostsResponse_httpStatus,

    -- ** ListSites
    listSites_nextToken,
    listSites_maxResults,
    listSitesResponse_nextToken,
    listSitesResponse_sites,
    listSitesResponse_httpStatus,

    -- ** CreateOrder
    createOrder_paymentTerm,
    createOrder_outpostIdentifier,
    createOrder_lineItems,
    createOrder_paymentOption,
    createOrderResponse_order,
    createOrderResponse_httpStatus,

    -- ** GetOutpostInstanceTypes
    getOutpostInstanceTypes_nextToken,
    getOutpostInstanceTypes_maxResults,
    getOutpostInstanceTypes_outpostId,
    getOutpostInstanceTypesResponse_instanceTypes,
    getOutpostInstanceTypesResponse_outpostArn,
    getOutpostInstanceTypesResponse_nextToken,
    getOutpostInstanceTypesResponse_outpostId,
    getOutpostInstanceTypesResponse_httpStatus,

    -- ** CreateOutpost
    createOutpost_availabilityZoneId,
    createOutpost_availabilityZone,
    createOutpost_description,
    createOutpost_tags,
    createOutpost_name,
    createOutpost_siteId,
    createOutpostResponse_outpost,
    createOutpostResponse_httpStatus,

    -- ** GetOutpost
    getOutpost_outpostId,
    getOutpostResponse_outpost,
    getOutpostResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- * Types

    -- ** InstanceTypeItem
    instanceTypeItem_instanceType,

    -- ** LineItem
    lineItem_status,
    lineItem_quantity,
    lineItem_catalogItemId,
    lineItem_lineItemId,

    -- ** LineItemRequest
    lineItemRequest_quantity,
    lineItemRequest_catalogItemId,

    -- ** Order
    order_status,
    order_orderSubmissionDate,
    order_lineItems,
    order_orderFulfilledDate,
    order_orderId,
    order_outpostId,
    order_paymentOption,

    -- ** Outpost
    outpost_availabilityZoneId,
    outpost_outpostArn,
    outpost_ownerId,
    outpost_availabilityZone,
    outpost_name,
    outpost_lifeCycleStatus,
    outpost_outpostId,
    outpost_siteId,
    outpost_siteArn,
    outpost_description,
    outpost_tags,

    -- ** Site
    site_accountId,
    site_name,
    site_siteId,
    site_siteArn,
    site_description,
    site_tags,
  )
where

import Network.AWS.Outposts.CreateOrder
import Network.AWS.Outposts.CreateOutpost
import Network.AWS.Outposts.DeleteOutpost
import Network.AWS.Outposts.DeleteSite
import Network.AWS.Outposts.GetOutpost
import Network.AWS.Outposts.GetOutpostInstanceTypes
import Network.AWS.Outposts.ListOutposts
import Network.AWS.Outposts.ListSites
import Network.AWS.Outposts.ListTagsForResource
import Network.AWS.Outposts.TagResource
import Network.AWS.Outposts.Types.InstanceTypeItem
import Network.AWS.Outposts.Types.LineItem
import Network.AWS.Outposts.Types.LineItemRequest
import Network.AWS.Outposts.Types.Order
import Network.AWS.Outposts.Types.Outpost
import Network.AWS.Outposts.Types.Site
import Network.AWS.Outposts.UntagResource
