{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.Route53RecoveryReadiness
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2019-12-02@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- AWS Route53 Recovery Readiness
module Network.AWS.Route53RecoveryReadiness
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ValidationException
    _ValidationException,

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ConflictException
    _ConflictException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateResourceSet
    CreateResourceSet (CreateResourceSet'),
    newCreateResourceSet,
    CreateResourceSetResponse (CreateResourceSetResponse'),
    newCreateResourceSetResponse,

    -- ** GetReadinessCheckStatus (Paginated)
    GetReadinessCheckStatus (GetReadinessCheckStatus'),
    newGetReadinessCheckStatus,
    GetReadinessCheckStatusResponse (GetReadinessCheckStatusResponse'),
    newGetReadinessCheckStatusResponse,

    -- ** GetCellReadinessSummary (Paginated)
    GetCellReadinessSummary (GetCellReadinessSummary'),
    newGetCellReadinessSummary,
    GetCellReadinessSummaryResponse (GetCellReadinessSummaryResponse'),
    newGetCellReadinessSummaryResponse,

    -- ** UpdateCell
    UpdateCell (UpdateCell'),
    newUpdateCell,
    UpdateCellResponse (UpdateCellResponse'),
    newUpdateCellResponse,

    -- ** DeleteCell
    DeleteCell (DeleteCell'),
    newDeleteCell,
    DeleteCellResponse (DeleteCellResponse'),
    newDeleteCellResponse,

    -- ** UpdateReadinessCheck
    UpdateReadinessCheck (UpdateReadinessCheck'),
    newUpdateReadinessCheck,
    UpdateReadinessCheckResponse (UpdateReadinessCheckResponse'),
    newUpdateReadinessCheckResponse,

    -- ** DeleteReadinessCheck
    DeleteReadinessCheck (DeleteReadinessCheck'),
    newDeleteReadinessCheck,
    DeleteReadinessCheckResponse (DeleteReadinessCheckResponse'),
    newDeleteReadinessCheckResponse,

    -- ** ListCells (Paginated)
    ListCells (ListCells'),
    newListCells,
    ListCellsResponse (ListCellsResponse'),
    newListCellsResponse,

    -- ** ListReadinessChecks (Paginated)
    ListReadinessChecks (ListReadinessChecks'),
    newListReadinessChecks,
    ListReadinessChecksResponse (ListReadinessChecksResponse'),
    newListReadinessChecksResponse,

    -- ** ListRules (Paginated)
    ListRules (ListRules'),
    newListRules,
    ListRulesResponse (ListRulesResponse'),
    newListRulesResponse,

    -- ** CreateReadinessCheck
    CreateReadinessCheck (CreateReadinessCheck'),
    newCreateReadinessCheck,
    CreateReadinessCheckResponse (CreateReadinessCheckResponse'),
    newCreateReadinessCheckResponse,

    -- ** CreateCell
    CreateCell (CreateCell'),
    newCreateCell,
    CreateCellResponse (CreateCellResponse'),
    newCreateCellResponse,

    -- ** GetRecoveryGroup
    GetRecoveryGroup (GetRecoveryGroup'),
    newGetRecoveryGroup,
    GetRecoveryGroupResponse (GetRecoveryGroupResponse'),
    newGetRecoveryGroupResponse,

    -- ** ListRecoveryGroups (Paginated)
    ListRecoveryGroups (ListRecoveryGroups'),
    newListRecoveryGroups,
    ListRecoveryGroupsResponse (ListRecoveryGroupsResponse'),
    newListRecoveryGroupsResponse,

    -- ** ListCrossAccountAuthorizations (Paginated)
    ListCrossAccountAuthorizations (ListCrossAccountAuthorizations'),
    newListCrossAccountAuthorizations,
    ListCrossAccountAuthorizationsResponse (ListCrossAccountAuthorizationsResponse'),
    newListCrossAccountAuthorizationsResponse,

    -- ** GetCell
    GetCell (GetCell'),
    newGetCell,
    GetCellResponse (GetCellResponse'),
    newGetCellResponse,

    -- ** CreateCrossAccountAuthorization
    CreateCrossAccountAuthorization (CreateCrossAccountAuthorization'),
    newCreateCrossAccountAuthorization,
    CreateCrossAccountAuthorizationResponse (CreateCrossAccountAuthorizationResponse'),
    newCreateCrossAccountAuthorizationResponse,

    -- ** CreateRecoveryGroup
    CreateRecoveryGroup (CreateRecoveryGroup'),
    newCreateRecoveryGroup,
    CreateRecoveryGroupResponse (CreateRecoveryGroupResponse'),
    newCreateRecoveryGroupResponse,

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

    -- ** ListResourceSets (Paginated)
    ListResourceSets (ListResourceSets'),
    newListResourceSets,
    ListResourceSetsResponse (ListResourceSetsResponse'),
    newListResourceSetsResponse,

    -- ** GetArchitectureRecommendations
    GetArchitectureRecommendations (GetArchitectureRecommendations'),
    newGetArchitectureRecommendations,
    GetArchitectureRecommendationsResponse (GetArchitectureRecommendationsResponse'),
    newGetArchitectureRecommendationsResponse,

    -- ** DeleteCrossAccountAuthorization
    DeleteCrossAccountAuthorization (DeleteCrossAccountAuthorization'),
    newDeleteCrossAccountAuthorization,
    DeleteCrossAccountAuthorizationResponse (DeleteCrossAccountAuthorizationResponse'),
    newDeleteCrossAccountAuthorizationResponse,

    -- ** DeleteRecoveryGroup
    DeleteRecoveryGroup (DeleteRecoveryGroup'),
    newDeleteRecoveryGroup,
    DeleteRecoveryGroupResponse (DeleteRecoveryGroupResponse'),
    newDeleteRecoveryGroupResponse,

    -- ** UpdateRecoveryGroup
    UpdateRecoveryGroup (UpdateRecoveryGroup'),
    newUpdateRecoveryGroup,
    UpdateRecoveryGroupResponse (UpdateRecoveryGroupResponse'),
    newUpdateRecoveryGroupResponse,

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

    -- ** ListTagsForResources
    ListTagsForResources (ListTagsForResources'),
    newListTagsForResources,
    ListTagsForResourcesResponse (ListTagsForResourcesResponse'),
    newListTagsForResourcesResponse,

    -- ** UpdateResourceSet
    UpdateResourceSet (UpdateResourceSet'),
    newUpdateResourceSet,
    UpdateResourceSetResponse (UpdateResourceSetResponse'),
    newUpdateResourceSetResponse,

    -- ** DeleteResourceSet
    DeleteResourceSet (DeleteResourceSet'),
    newDeleteResourceSet,
    DeleteResourceSetResponse (DeleteResourceSetResponse'),
    newDeleteResourceSetResponse,

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

import Network.AWS.Route53RecoveryReadiness.CreateCell
import Network.AWS.Route53RecoveryReadiness.CreateCrossAccountAuthorization
import Network.AWS.Route53RecoveryReadiness.CreateReadinessCheck
import Network.AWS.Route53RecoveryReadiness.CreateRecoveryGroup
import Network.AWS.Route53RecoveryReadiness.CreateResourceSet
import Network.AWS.Route53RecoveryReadiness.DeleteCell
import Network.AWS.Route53RecoveryReadiness.DeleteCrossAccountAuthorization
import Network.AWS.Route53RecoveryReadiness.DeleteReadinessCheck
import Network.AWS.Route53RecoveryReadiness.DeleteRecoveryGroup
import Network.AWS.Route53RecoveryReadiness.DeleteResourceSet
import Network.AWS.Route53RecoveryReadiness.GetArchitectureRecommendations
import Network.AWS.Route53RecoveryReadiness.GetCell
import Network.AWS.Route53RecoveryReadiness.GetCellReadinessSummary
import Network.AWS.Route53RecoveryReadiness.GetReadinessCheck
import Network.AWS.Route53RecoveryReadiness.GetReadinessCheckResourceStatus
import Network.AWS.Route53RecoveryReadiness.GetReadinessCheckStatus
import Network.AWS.Route53RecoveryReadiness.GetRecoveryGroup
import Network.AWS.Route53RecoveryReadiness.GetRecoveryGroupReadinessSummary
import Network.AWS.Route53RecoveryReadiness.GetResourceSet
import Network.AWS.Route53RecoveryReadiness.Lens
import Network.AWS.Route53RecoveryReadiness.ListCells
import Network.AWS.Route53RecoveryReadiness.ListCrossAccountAuthorizations
import Network.AWS.Route53RecoveryReadiness.ListReadinessChecks
import Network.AWS.Route53RecoveryReadiness.ListRecoveryGroups
import Network.AWS.Route53RecoveryReadiness.ListResourceSets
import Network.AWS.Route53RecoveryReadiness.ListRules
import Network.AWS.Route53RecoveryReadiness.ListTagsForResources
import Network.AWS.Route53RecoveryReadiness.TagResource
import Network.AWS.Route53RecoveryReadiness.Types
import Network.AWS.Route53RecoveryReadiness.UntagResource
import Network.AWS.Route53RecoveryReadiness.UpdateCell
import Network.AWS.Route53RecoveryReadiness.UpdateReadinessCheck
import Network.AWS.Route53RecoveryReadiness.UpdateRecoveryGroup
import Network.AWS.Route53RecoveryReadiness.UpdateResourceSet
import Network.AWS.Route53RecoveryReadiness.Waiters

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
