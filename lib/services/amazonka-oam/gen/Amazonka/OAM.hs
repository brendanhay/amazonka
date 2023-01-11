{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.OAM
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2022-06-10@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Use Amazon CloudWatch Observability Access Manager to create and manage
-- links between source accounts and monitoring accounts by using
-- /CloudWatch cross-account observability/. With CloudWatch cross-account
-- observability, you can monitor and troubleshoot applications that span
-- multiple accounts within a Region. Seamlessly search, visualize, and
-- analyze your metrics, logs, and traces in any of the linked accounts
-- without account boundaries.
--
-- >  <p>Set up one or more Amazon Web Services accounts as <i>monitoring accounts</i> and link them with multiple <i>source accounts</i>. A monitoring account is a central Amazon Web Services account that can view and interact with observability data generated from source accounts. A source account is an individual Amazon Web Services account that generates observability data for the resources that reside in it. Source accounts share their observability data with the monitoring account. The shared observability data can include metrics in Amazon CloudWatch, logs in Amazon CloudWatch Logs, and traces in X-Ray.</p>
module Amazonka.OAM
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ConflictException
    _ConflictException,

    -- ** InternalServiceFault
    _InternalServiceFault,

    -- ** InvalidParameterException
    _InvalidParameterException,

    -- ** MissingRequiredParameterException
    _MissingRequiredParameterException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** TooManyTagsException
    _TooManyTagsException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateLink
    CreateLink (CreateLink'),
    newCreateLink,
    CreateLinkResponse (CreateLinkResponse'),
    newCreateLinkResponse,

    -- ** CreateSink
    CreateSink (CreateSink'),
    newCreateSink,
    CreateSinkResponse (CreateSinkResponse'),
    newCreateSinkResponse,

    -- ** DeleteLink
    DeleteLink (DeleteLink'),
    newDeleteLink,
    DeleteLinkResponse (DeleteLinkResponse'),
    newDeleteLinkResponse,

    -- ** DeleteSink
    DeleteSink (DeleteSink'),
    newDeleteSink,
    DeleteSinkResponse (DeleteSinkResponse'),
    newDeleteSinkResponse,

    -- ** GetLink
    GetLink (GetLink'),
    newGetLink,
    GetLinkResponse (GetLinkResponse'),
    newGetLinkResponse,

    -- ** GetSink
    GetSink (GetSink'),
    newGetSink,
    GetSinkResponse (GetSinkResponse'),
    newGetSinkResponse,

    -- ** GetSinkPolicy
    GetSinkPolicy (GetSinkPolicy'),
    newGetSinkPolicy,
    GetSinkPolicyResponse (GetSinkPolicyResponse'),
    newGetSinkPolicyResponse,

    -- ** ListAttachedLinks (Paginated)
    ListAttachedLinks (ListAttachedLinks'),
    newListAttachedLinks,
    ListAttachedLinksResponse (ListAttachedLinksResponse'),
    newListAttachedLinksResponse,

    -- ** ListLinks (Paginated)
    ListLinks (ListLinks'),
    newListLinks,
    ListLinksResponse (ListLinksResponse'),
    newListLinksResponse,

    -- ** ListSinks (Paginated)
    ListSinks (ListSinks'),
    newListSinks,
    ListSinksResponse (ListSinksResponse'),
    newListSinksResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** PutSinkPolicy
    PutSinkPolicy (PutSinkPolicy'),
    newPutSinkPolicy,
    PutSinkPolicyResponse (PutSinkPolicyResponse'),
    newPutSinkPolicyResponse,

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

    -- ** UpdateLink
    UpdateLink (UpdateLink'),
    newUpdateLink,
    UpdateLinkResponse (UpdateLinkResponse'),
    newUpdateLinkResponse,

    -- * Types

    -- ** ResourceType
    ResourceType (..),

    -- ** ListAttachedLinksItem
    ListAttachedLinksItem (ListAttachedLinksItem'),
    newListAttachedLinksItem,

    -- ** ListLinksItem
    ListLinksItem (ListLinksItem'),
    newListLinksItem,

    -- ** ListSinksItem
    ListSinksItem (ListSinksItem'),
    newListSinksItem,
  )
where

import Amazonka.OAM.CreateLink
import Amazonka.OAM.CreateSink
import Amazonka.OAM.DeleteLink
import Amazonka.OAM.DeleteSink
import Amazonka.OAM.GetLink
import Amazonka.OAM.GetSink
import Amazonka.OAM.GetSinkPolicy
import Amazonka.OAM.Lens
import Amazonka.OAM.ListAttachedLinks
import Amazonka.OAM.ListLinks
import Amazonka.OAM.ListSinks
import Amazonka.OAM.ListTagsForResource
import Amazonka.OAM.PutSinkPolicy
import Amazonka.OAM.TagResource
import Amazonka.OAM.Types
import Amazonka.OAM.UntagResource
import Amazonka.OAM.UpdateLink
import Amazonka.OAM.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'OAM'.

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
