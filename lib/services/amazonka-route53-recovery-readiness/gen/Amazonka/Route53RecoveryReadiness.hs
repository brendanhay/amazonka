{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.Route53RecoveryReadiness
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2019-12-02@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Recovery readiness
module Amazonka.Route53RecoveryReadiness
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

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateCell
    CreateCell (CreateCell'),
    newCreateCell,
    CreateCellResponse (CreateCellResponse'),
    newCreateCellResponse,

    -- ** CreateCrossAccountAuthorization
    CreateCrossAccountAuthorization (CreateCrossAccountAuthorization'),
    newCreateCrossAccountAuthorization,
    CreateCrossAccountAuthorizationResponse (CreateCrossAccountAuthorizationResponse'),
    newCreateCrossAccountAuthorizationResponse,

    -- ** CreateReadinessCheck
    CreateReadinessCheck (CreateReadinessCheck'),
    newCreateReadinessCheck,
    CreateReadinessCheckResponse (CreateReadinessCheckResponse'),
    newCreateReadinessCheckResponse,

    -- ** CreateRecoveryGroup
    CreateRecoveryGroup (CreateRecoveryGroup'),
    newCreateRecoveryGroup,
    CreateRecoveryGroupResponse (CreateRecoveryGroupResponse'),
    newCreateRecoveryGroupResponse,

    -- ** CreateResourceSet
    CreateResourceSet (CreateResourceSet'),
    newCreateResourceSet,
    CreateResourceSetResponse (CreateResourceSetResponse'),
    newCreateResourceSetResponse,

    -- ** DeleteCell
    DeleteCell (DeleteCell'),
    newDeleteCell,
    DeleteCellResponse (DeleteCellResponse'),
    newDeleteCellResponse,

    -- ** DeleteCrossAccountAuthorization
    DeleteCrossAccountAuthorization (DeleteCrossAccountAuthorization'),
    newDeleteCrossAccountAuthorization,
    DeleteCrossAccountAuthorizationResponse (DeleteCrossAccountAuthorizationResponse'),
    newDeleteCrossAccountAuthorizationResponse,

    -- ** DeleteReadinessCheck
    DeleteReadinessCheck (DeleteReadinessCheck'),
    newDeleteReadinessCheck,
    DeleteReadinessCheckResponse (DeleteReadinessCheckResponse'),
    newDeleteReadinessCheckResponse,

    -- ** DeleteRecoveryGroup
    DeleteRecoveryGroup (DeleteRecoveryGroup'),
    newDeleteRecoveryGroup,
    DeleteRecoveryGroupResponse (DeleteRecoveryGroupResponse'),
    newDeleteRecoveryGroupResponse,

    -- ** DeleteResourceSet
    DeleteResourceSet (DeleteResourceSet'),
    newDeleteResourceSet,
    DeleteResourceSetResponse (DeleteResourceSetResponse'),
    newDeleteResourceSetResponse,

    -- ** GetArchitectureRecommendations
    GetArchitectureRecommendations (GetArchitectureRecommendations'),
    newGetArchitectureRecommendations,
    GetArchitectureRecommendationsResponse (GetArchitectureRecommendationsResponse'),
    newGetArchitectureRecommendationsResponse,

    -- ** GetCell
    GetCell (GetCell'),
    newGetCell,
    GetCellResponse (GetCellResponse'),
    newGetCellResponse,

    -- ** GetCellReadinessSummary (Paginated)
    GetCellReadinessSummary (GetCellReadinessSummary'),
    newGetCellReadinessSummary,
    GetCellReadinessSummaryResponse (GetCellReadinessSummaryResponse'),
    newGetCellReadinessSummaryResponse,

    -- ** GetReadinessCheck
    GetReadinessCheck (GetReadinessCheck'),
    newGetReadinessCheck,
    GetReadinessCheckResponse (GetReadinessCheckResponse'),
    newGetReadinessCheckResponse,

    -- ** GetReadinessCheckResourceStatus (Paginated)
    GetReadinessCheckResourceStatus (GetReadinessCheckResourceStatus'),
    newGetReadinessCheckResourceStatus,
    GetReadinessCheckResourceStatusResponse (GetReadinessCheckResourceStatusResponse'),
    newGetReadinessCheckResourceStatusResponse,

    -- ** GetReadinessCheckStatus (Paginated)
    GetReadinessCheckStatus (GetReadinessCheckStatus'),
    newGetReadinessCheckStatus,
    GetReadinessCheckStatusResponse (GetReadinessCheckStatusResponse'),
    newGetReadinessCheckStatusResponse,

    -- ** GetRecoveryGroup
    GetRecoveryGroup (GetRecoveryGroup'),
    newGetRecoveryGroup,
    GetRecoveryGroupResponse (GetRecoveryGroupResponse'),
    newGetRecoveryGroupResponse,

    -- ** GetRecoveryGroupReadinessSummary (Paginated)
    GetRecoveryGroupReadinessSummary (GetRecoveryGroupReadinessSummary'),
    newGetRecoveryGroupReadinessSummary,
    GetRecoveryGroupReadinessSummaryResponse (GetRecoveryGroupReadinessSummaryResponse'),
    newGetRecoveryGroupReadinessSummaryResponse,

    -- ** GetResourceSet
    GetResourceSet (GetResourceSet'),
    newGetResourceSet,
    GetResourceSetResponse (GetResourceSetResponse'),
    newGetResourceSetResponse,

    -- ** ListCells (Paginated)
    ListCells (ListCells'),
    newListCells,
    ListCellsResponse (ListCellsResponse'),
    newListCellsResponse,

    -- ** ListCrossAccountAuthorizations (Paginated)
    ListCrossAccountAuthorizations (ListCrossAccountAuthorizations'),
    newListCrossAccountAuthorizations,
    ListCrossAccountAuthorizationsResponse (ListCrossAccountAuthorizationsResponse'),
    newListCrossAccountAuthorizationsResponse,

    -- ** ListReadinessChecks (Paginated)
    ListReadinessChecks (ListReadinessChecks'),
    newListReadinessChecks,
    ListReadinessChecksResponse (ListReadinessChecksResponse'),
    newListReadinessChecksResponse,

    -- ** ListRecoveryGroups (Paginated)
    ListRecoveryGroups (ListRecoveryGroups'),
    newListRecoveryGroups,
    ListRecoveryGroupsResponse (ListRecoveryGroupsResponse'),
    newListRecoveryGroupsResponse,

    -- ** ListResourceSets (Paginated)
    ListResourceSets (ListResourceSets'),
    newListResourceSets,
    ListResourceSetsResponse (ListResourceSetsResponse'),
    newListResourceSetsResponse,

    -- ** ListRules (Paginated)
    ListRules (ListRules'),
    newListRules,
    ListRulesResponse (ListRulesResponse'),
    newListRulesResponse,

    -- ** ListTagsForResources
    ListTagsForResources (ListTagsForResources'),
    newListTagsForResources,
    ListTagsForResourcesResponse (ListTagsForResourcesResponse'),
    newListTagsForResourcesResponse,

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

    -- ** UpdateCell
    UpdateCell (UpdateCell'),
    newUpdateCell,
    UpdateCellResponse (UpdateCellResponse'),
    newUpdateCellResponse,

    -- ** UpdateReadinessCheck
    UpdateReadinessCheck (UpdateReadinessCheck'),
    newUpdateReadinessCheck,
    UpdateReadinessCheckResponse (UpdateReadinessCheckResponse'),
    newUpdateReadinessCheckResponse,

    -- ** UpdateRecoveryGroup
    UpdateRecoveryGroup (UpdateRecoveryGroup'),
    newUpdateRecoveryGroup,
    UpdateRecoveryGroupResponse (UpdateRecoveryGroupResponse'),
    newUpdateRecoveryGroupResponse,

    -- ** UpdateResourceSet
    UpdateResourceSet (UpdateResourceSet'),
    newUpdateResourceSet,
    UpdateResourceSetResponse (UpdateResourceSetResponse'),
    newUpdateResourceSetResponse,

    -- * Types

    -- ** Readiness
    Readiness (..),

    -- ** CellOutput
    CellOutput (CellOutput'),
    newCellOutput,

    -- ** DNSTargetResource
    DNSTargetResource (DNSTargetResource'),
    newDNSTargetResource,

    -- ** ListRulesOutput
    ListRulesOutput (ListRulesOutput'),
    newListRulesOutput,

    -- ** Message
    Message (Message'),
    newMessage,

    -- ** NLBResource
    NLBResource (NLBResource'),
    newNLBResource,

    -- ** R53ResourceRecord
    R53ResourceRecord (R53ResourceRecord'),
    newR53ResourceRecord,

    -- ** ReadinessCheckOutput
    ReadinessCheckOutput (ReadinessCheckOutput'),
    newReadinessCheckOutput,

    -- ** ReadinessCheckSummary
    ReadinessCheckSummary (ReadinessCheckSummary'),
    newReadinessCheckSummary,

    -- ** Recommendation
    Recommendation (Recommendation'),
    newRecommendation,

    -- ** RecoveryGroupOutput
    RecoveryGroupOutput (RecoveryGroupOutput'),
    newRecoveryGroupOutput,

    -- ** Resource
    Resource (Resource'),
    newResource,

    -- ** ResourceResult
    ResourceResult (ResourceResult'),
    newResourceResult,

    -- ** ResourceSetOutput
    ResourceSetOutput (ResourceSetOutput'),
    newResourceSetOutput,

    -- ** RuleResult
    RuleResult (RuleResult'),
    newRuleResult,

    -- ** TargetResource
    TargetResource (TargetResource'),
    newTargetResource,
  )
where

import Amazonka.Route53RecoveryReadiness.CreateCell
import Amazonka.Route53RecoveryReadiness.CreateCrossAccountAuthorization
import Amazonka.Route53RecoveryReadiness.CreateReadinessCheck
import Amazonka.Route53RecoveryReadiness.CreateRecoveryGroup
import Amazonka.Route53RecoveryReadiness.CreateResourceSet
import Amazonka.Route53RecoveryReadiness.DeleteCell
import Amazonka.Route53RecoveryReadiness.DeleteCrossAccountAuthorization
import Amazonka.Route53RecoveryReadiness.DeleteReadinessCheck
import Amazonka.Route53RecoveryReadiness.DeleteRecoveryGroup
import Amazonka.Route53RecoveryReadiness.DeleteResourceSet
import Amazonka.Route53RecoveryReadiness.GetArchitectureRecommendations
import Amazonka.Route53RecoveryReadiness.GetCell
import Amazonka.Route53RecoveryReadiness.GetCellReadinessSummary
import Amazonka.Route53RecoveryReadiness.GetReadinessCheck
import Amazonka.Route53RecoveryReadiness.GetReadinessCheckResourceStatus
import Amazonka.Route53RecoveryReadiness.GetReadinessCheckStatus
import Amazonka.Route53RecoveryReadiness.GetRecoveryGroup
import Amazonka.Route53RecoveryReadiness.GetRecoveryGroupReadinessSummary
import Amazonka.Route53RecoveryReadiness.GetResourceSet
import Amazonka.Route53RecoveryReadiness.Lens
import Amazonka.Route53RecoveryReadiness.ListCells
import Amazonka.Route53RecoveryReadiness.ListCrossAccountAuthorizations
import Amazonka.Route53RecoveryReadiness.ListReadinessChecks
import Amazonka.Route53RecoveryReadiness.ListRecoveryGroups
import Amazonka.Route53RecoveryReadiness.ListResourceSets
import Amazonka.Route53RecoveryReadiness.ListRules
import Amazonka.Route53RecoveryReadiness.ListTagsForResources
import Amazonka.Route53RecoveryReadiness.TagResource
import Amazonka.Route53RecoveryReadiness.Types
import Amazonka.Route53RecoveryReadiness.UntagResource
import Amazonka.Route53RecoveryReadiness.UpdateCell
import Amazonka.Route53RecoveryReadiness.UpdateReadinessCheck
import Amazonka.Route53RecoveryReadiness.UpdateRecoveryGroup
import Amazonka.Route53RecoveryReadiness.UpdateResourceSet
import Amazonka.Route53RecoveryReadiness.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'Route53RecoveryReadiness'.

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
