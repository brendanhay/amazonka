{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.MarketplaceCatalog
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2018-09-17@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Catalog API actions allow you to manage your entities through list,
-- describe, and update capabilities. An entity can be a product or an
-- offer on AWS Marketplace.
--
-- You can automate your entity update process by integrating the AWS
-- Marketplace Catalog API with your AWS Marketplace product build or
-- deployment pipelines. You can also create your own applications on top
-- of the Catalog API to manage your products on AWS Marketplace.
module Amazonka.MarketplaceCatalog
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** InternalServiceException
    _InternalServiceException,

    -- ** ResourceInUseException
    _ResourceInUseException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ResourceNotSupportedException
    _ResourceNotSupportedException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CancelChangeSet
    CancelChangeSet (CancelChangeSet'),
    newCancelChangeSet,
    CancelChangeSetResponse (CancelChangeSetResponse'),
    newCancelChangeSetResponse,

    -- ** DescribeChangeSet
    DescribeChangeSet (DescribeChangeSet'),
    newDescribeChangeSet,
    DescribeChangeSetResponse (DescribeChangeSetResponse'),
    newDescribeChangeSetResponse,

    -- ** DescribeEntity
    DescribeEntity (DescribeEntity'),
    newDescribeEntity,
    DescribeEntityResponse (DescribeEntityResponse'),
    newDescribeEntityResponse,

    -- ** ListChangeSets
    ListChangeSets (ListChangeSets'),
    newListChangeSets,
    ListChangeSetsResponse (ListChangeSetsResponse'),
    newListChangeSetsResponse,

    -- ** ListEntities
    ListEntities (ListEntities'),
    newListEntities,
    ListEntitiesResponse (ListEntitiesResponse'),
    newListEntitiesResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** StartChangeSet
    StartChangeSet (StartChangeSet'),
    newStartChangeSet,
    StartChangeSetResponse (StartChangeSetResponse'),
    newStartChangeSetResponse,

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

    -- ** ChangeStatus
    ChangeStatus (..),

    -- ** FailureCode
    FailureCode (..),

    -- ** SortOrder
    SortOrder (..),

    -- ** Change
    Change (Change'),
    newChange,

    -- ** ChangeSetSummaryListItem
    ChangeSetSummaryListItem (ChangeSetSummaryListItem'),
    newChangeSetSummaryListItem,

    -- ** ChangeSummary
    ChangeSummary (ChangeSummary'),
    newChangeSummary,

    -- ** Entity
    Entity (Entity'),
    newEntity,

    -- ** EntitySummary
    EntitySummary (EntitySummary'),
    newEntitySummary,

    -- ** ErrorDetail
    ErrorDetail (ErrorDetail'),
    newErrorDetail,

    -- ** Filter
    Filter (Filter'),
    newFilter,

    -- ** Sort
    Sort (Sort'),
    newSort,

    -- ** Tag
    Tag (Tag'),
    newTag,
  )
where

import Amazonka.MarketplaceCatalog.CancelChangeSet
import Amazonka.MarketplaceCatalog.DescribeChangeSet
import Amazonka.MarketplaceCatalog.DescribeEntity
import Amazonka.MarketplaceCatalog.Lens
import Amazonka.MarketplaceCatalog.ListChangeSets
import Amazonka.MarketplaceCatalog.ListEntities
import Amazonka.MarketplaceCatalog.ListTagsForResource
import Amazonka.MarketplaceCatalog.StartChangeSet
import Amazonka.MarketplaceCatalog.TagResource
import Amazonka.MarketplaceCatalog.Types
import Amazonka.MarketplaceCatalog.UntagResource
import Amazonka.MarketplaceCatalog.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'MarketplaceCatalog'.

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
