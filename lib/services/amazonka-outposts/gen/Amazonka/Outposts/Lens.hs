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
