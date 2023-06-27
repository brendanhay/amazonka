{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.KendraRanking
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2022-10-19@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon Kendra Intelligent Ranking uses Amazon Kendra semantic search
-- capabilities to intelligently re-rank a search service\'s results.
module Amazonka.KendraRanking
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ConflictException
    _ConflictException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ResourceUnavailableException
    _ResourceUnavailableException,

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

    -- ** CreateRescoreExecutionPlan
    CreateRescoreExecutionPlan (CreateRescoreExecutionPlan'),
    newCreateRescoreExecutionPlan,
    CreateRescoreExecutionPlanResponse (CreateRescoreExecutionPlanResponse'),
    newCreateRescoreExecutionPlanResponse,

    -- ** DeleteRescoreExecutionPlan
    DeleteRescoreExecutionPlan (DeleteRescoreExecutionPlan'),
    newDeleteRescoreExecutionPlan,
    DeleteRescoreExecutionPlanResponse (DeleteRescoreExecutionPlanResponse'),
    newDeleteRescoreExecutionPlanResponse,

    -- ** DescribeRescoreExecutionPlan
    DescribeRescoreExecutionPlan (DescribeRescoreExecutionPlan'),
    newDescribeRescoreExecutionPlan,
    DescribeRescoreExecutionPlanResponse (DescribeRescoreExecutionPlanResponse'),
    newDescribeRescoreExecutionPlanResponse,

    -- ** ListRescoreExecutionPlans
    ListRescoreExecutionPlans (ListRescoreExecutionPlans'),
    newListRescoreExecutionPlans,
    ListRescoreExecutionPlansResponse (ListRescoreExecutionPlansResponse'),
    newListRescoreExecutionPlansResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** Rescore
    Rescore (Rescore'),
    newRescore,
    RescoreResponse (RescoreResponse'),
    newRescoreResponse,

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

    -- ** UpdateRescoreExecutionPlan
    UpdateRescoreExecutionPlan (UpdateRescoreExecutionPlan'),
    newUpdateRescoreExecutionPlan,
    UpdateRescoreExecutionPlanResponse (UpdateRescoreExecutionPlanResponse'),
    newUpdateRescoreExecutionPlanResponse,

    -- * Types

    -- ** RescoreExecutionPlanStatus
    RescoreExecutionPlanStatus (..),

    -- ** CapacityUnitsConfiguration
    CapacityUnitsConfiguration (CapacityUnitsConfiguration'),
    newCapacityUnitsConfiguration,

    -- ** Document
    Document (Document'),
    newDocument,

    -- ** RescoreExecutionPlanSummary
    RescoreExecutionPlanSummary (RescoreExecutionPlanSummary'),
    newRescoreExecutionPlanSummary,

    -- ** RescoreResultItem
    RescoreResultItem (RescoreResultItem'),
    newRescoreResultItem,

    -- ** Tag
    Tag (Tag'),
    newTag,
  )
where

import Amazonka.KendraRanking.CreateRescoreExecutionPlan
import Amazonka.KendraRanking.DeleteRescoreExecutionPlan
import Amazonka.KendraRanking.DescribeRescoreExecutionPlan
import Amazonka.KendraRanking.Lens
import Amazonka.KendraRanking.ListRescoreExecutionPlans
import Amazonka.KendraRanking.ListTagsForResource
import Amazonka.KendraRanking.Rescore
import Amazonka.KendraRanking.TagResource
import Amazonka.KendraRanking.Types
import Amazonka.KendraRanking.UntagResource
import Amazonka.KendraRanking.UpdateRescoreExecutionPlan
import Amazonka.KendraRanking.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'KendraRanking'.

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
