{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Outposts.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Outposts.Lens
  ( -- * Operations

    -- ** CreateOrder
    createOrder_paymentTerm,
    createOrder_outpostIdentifier,
    createOrder_lineItems,
    createOrder_paymentOption,
    createOrderResponse_order,
    createOrderResponse_httpStatus,

    -- ** CreateOutpost
    createOutpost_tags,
    createOutpost_availabilityZone,
    createOutpost_description,
    createOutpost_availabilityZoneId,
    createOutpost_name,
    createOutpost_siteId,
    createOutpostResponse_outpost,
    createOutpostResponse_httpStatus,

    -- ** DeleteOutpost
    deleteOutpost_outpostId,
    deleteOutpostResponse_httpStatus,

    -- ** DeleteSite
    deleteSite_siteId,
    deleteSiteResponse_httpStatus,

    -- ** GetOutpost
    getOutpost_outpostId,
    getOutpostResponse_outpost,
    getOutpostResponse_httpStatus,

    -- ** GetOutpostInstanceTypes
    getOutpostInstanceTypes_nextToken,
    getOutpostInstanceTypes_maxResults,
    getOutpostInstanceTypes_outpostId,
    getOutpostInstanceTypesResponse_nextToken,
    getOutpostInstanceTypesResponse_outpostId,
    getOutpostInstanceTypesResponse_outpostArn,
    getOutpostInstanceTypesResponse_instanceTypes,
    getOutpostInstanceTypesResponse_httpStatus,

    -- ** ListOutposts
    listOutposts_nextToken,
    listOutposts_maxResults,
    listOutposts_lifeCycleStatusFilter,
    listOutposts_availabilityZoneFilter,
    listOutposts_availabilityZoneIdFilter,
    listOutpostsResponse_nextToken,
    listOutpostsResponse_outposts,
    listOutpostsResponse_httpStatus,

    -- ** ListSites
    listSites_nextToken,
    listSites_maxResults,
    listSitesResponse_sites,
    listSitesResponse_nextToken,
    listSitesResponse_httpStatus,

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

    -- ** InstanceTypeItem
    instanceTypeItem_instanceType,

    -- ** LineItem
    lineItem_quantity,
    lineItem_status,
    lineItem_catalogItemId,
    lineItem_lineItemId,

    -- ** LineItemRequest
    lineItemRequest_quantity,
    lineItemRequest_catalogItemId,

    -- ** Order
    order_outpostId,
    order_orderFulfilledDate,
    order_lineItems,
    order_status,
    order_orderId,
    order_orderSubmissionDate,
    order_paymentOption,

    -- ** Outpost
    outpost_tags,
    outpost_name,
    outpost_outpostId,
    outpost_outpostArn,
    outpost_ownerId,
    outpost_siteArn,
    outpost_availabilityZone,
    outpost_description,
    outpost_siteId,
    outpost_lifeCycleStatus,
    outpost_availabilityZoneId,

    -- ** Site
    site_tags,
    site_name,
    site_siteArn,
    site_description,
    site_siteId,
    site_accountId,
  )
where

import Amazonka.Outposts.CreateOrder
import Amazonka.Outposts.CreateOutpost
import Amazonka.Outposts.DeleteOutpost
import Amazonka.Outposts.DeleteSite
import Amazonka.Outposts.GetOutpost
import Amazonka.Outposts.GetOutpostInstanceTypes
import Amazonka.Outposts.ListOutposts
import Amazonka.Outposts.ListSites
import Amazonka.Outposts.ListTagsForResource
import Amazonka.Outposts.TagResource
import Amazonka.Outposts.Types.InstanceTypeItem
import Amazonka.Outposts.Types.LineItem
import Amazonka.Outposts.Types.LineItemRequest
import Amazonka.Outposts.Types.Order
import Amazonka.Outposts.Types.Outpost
import Amazonka.Outposts.Types.Site
import Amazonka.Outposts.UntagResource
