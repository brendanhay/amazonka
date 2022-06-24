{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.Outposts
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2019-12-03@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- AWS Outposts is a fully managed service that extends AWS infrastructure,
-- APIs, and tools to customer premises. By providing local access to AWS
-- managed infrastructure, AWS Outposts enables customers to build and run
-- applications on premises using the same programming interfaces as in AWS
-- Regions, while using local compute and storage resources for lower
-- latency and local data processing needs.
module Amazonka.Outposts
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** NotFoundException
    _NotFoundException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ConflictException
    _ConflictException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateOrder
    CreateOrder (CreateOrder'),
    newCreateOrder,
    CreateOrderResponse (CreateOrderResponse'),
    newCreateOrderResponse,

    -- ** CreateOutpost
    CreateOutpost (CreateOutpost'),
    newCreateOutpost,
    CreateOutpostResponse (CreateOutpostResponse'),
    newCreateOutpostResponse,

    -- ** DeleteOutpost
    DeleteOutpost (DeleteOutpost'),
    newDeleteOutpost,
    DeleteOutpostResponse (DeleteOutpostResponse'),
    newDeleteOutpostResponse,

    -- ** DeleteSite
    DeleteSite (DeleteSite'),
    newDeleteSite,
    DeleteSiteResponse (DeleteSiteResponse'),
    newDeleteSiteResponse,

    -- ** GetOutpost
    GetOutpost (GetOutpost'),
    newGetOutpost,
    GetOutpostResponse (GetOutpostResponse'),
    newGetOutpostResponse,

    -- ** GetOutpostInstanceTypes
    GetOutpostInstanceTypes (GetOutpostInstanceTypes'),
    newGetOutpostInstanceTypes,
    GetOutpostInstanceTypesResponse (GetOutpostInstanceTypesResponse'),
    newGetOutpostInstanceTypesResponse,

    -- ** ListOutposts
    ListOutposts (ListOutposts'),
    newListOutposts,
    ListOutpostsResponse (ListOutpostsResponse'),
    newListOutpostsResponse,

    -- ** ListSites
    ListSites (ListSites'),
    newListSites,
    ListSitesResponse (ListSitesResponse'),
    newListSitesResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- * Types

    -- ** OrderStatus
    OrderStatus (..),

    -- ** PaymentOption
    PaymentOption (..),

    -- ** PaymentTerm
    PaymentTerm (..),

    -- ** InstanceTypeItem
    InstanceTypeItem (InstanceTypeItem'),
    newInstanceTypeItem,

    -- ** LineItem
    LineItem (LineItem'),
    newLineItem,

    -- ** LineItemRequest
    LineItemRequest (LineItemRequest'),
    newLineItemRequest,

    -- ** Order
    Order (Order'),
    newOrder,

    -- ** Outpost
    Outpost (Outpost'),
    newOutpost,

    -- ** Site
    Site (Site'),
    newSite,
  )
where

import Amazonka.Outposts.CreateOrder
import Amazonka.Outposts.CreateOutpost
import Amazonka.Outposts.DeleteOutpost
import Amazonka.Outposts.DeleteSite
import Amazonka.Outposts.GetOutpost
import Amazonka.Outposts.GetOutpostInstanceTypes
import Amazonka.Outposts.Lens
import Amazonka.Outposts.ListOutposts
import Amazonka.Outposts.ListSites
import Amazonka.Outposts.ListTagsForResource
import Amazonka.Outposts.TagResource
import Amazonka.Outposts.Types
import Amazonka.Outposts.UntagResource
import Amazonka.Outposts.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'Outposts'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
