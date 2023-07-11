{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.Route53RecoveryControlConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2020-11-02@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Recovery Control Configuration API Reference for Amazon Route 53
-- Application Recovery Controller
module Amazonka.Route53RecoveryControlConfig
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

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- ** ClusterCreated
    newClusterCreated,

    -- ** ClusterDeleted
    newClusterDeleted,

    -- ** ControlPanelCreated
    newControlPanelCreated,

    -- ** ControlPanelDeleted
    newControlPanelDeleted,

    -- ** RoutingControlCreated
    newRoutingControlCreated,

    -- ** RoutingControlDeleted
    newRoutingControlDeleted,

    -- * Operations
    -- $operations

    -- ** CreateCluster
    CreateCluster (CreateCluster'),
    newCreateCluster,
    CreateClusterResponse (CreateClusterResponse'),
    newCreateClusterResponse,

    -- ** CreateControlPanel
    CreateControlPanel (CreateControlPanel'),
    newCreateControlPanel,
    CreateControlPanelResponse (CreateControlPanelResponse'),
    newCreateControlPanelResponse,

    -- ** CreateRoutingControl
    CreateRoutingControl (CreateRoutingControl'),
    newCreateRoutingControl,
    CreateRoutingControlResponse (CreateRoutingControlResponse'),
    newCreateRoutingControlResponse,

    -- ** CreateSafetyRule
    CreateSafetyRule (CreateSafetyRule'),
    newCreateSafetyRule,
    CreateSafetyRuleResponse (CreateSafetyRuleResponse'),
    newCreateSafetyRuleResponse,

    -- ** DeleteCluster
    DeleteCluster (DeleteCluster'),
    newDeleteCluster,
    DeleteClusterResponse (DeleteClusterResponse'),
    newDeleteClusterResponse,

    -- ** DeleteControlPanel
    DeleteControlPanel (DeleteControlPanel'),
    newDeleteControlPanel,
    DeleteControlPanelResponse (DeleteControlPanelResponse'),
    newDeleteControlPanelResponse,

    -- ** DeleteRoutingControl
    DeleteRoutingControl (DeleteRoutingControl'),
    newDeleteRoutingControl,
    DeleteRoutingControlResponse (DeleteRoutingControlResponse'),
    newDeleteRoutingControlResponse,

    -- ** DeleteSafetyRule
    DeleteSafetyRule (DeleteSafetyRule'),
    newDeleteSafetyRule,
    DeleteSafetyRuleResponse (DeleteSafetyRuleResponse'),
    newDeleteSafetyRuleResponse,

    -- ** DescribeCluster
    DescribeCluster (DescribeCluster'),
    newDescribeCluster,
    DescribeClusterResponse (DescribeClusterResponse'),
    newDescribeClusterResponse,

    -- ** DescribeControlPanel
    DescribeControlPanel (DescribeControlPanel'),
    newDescribeControlPanel,
    DescribeControlPanelResponse (DescribeControlPanelResponse'),
    newDescribeControlPanelResponse,

    -- ** DescribeRoutingControl
    DescribeRoutingControl (DescribeRoutingControl'),
    newDescribeRoutingControl,
    DescribeRoutingControlResponse (DescribeRoutingControlResponse'),
    newDescribeRoutingControlResponse,

    -- ** DescribeSafetyRule
    DescribeSafetyRule (DescribeSafetyRule'),
    newDescribeSafetyRule,
    DescribeSafetyRuleResponse (DescribeSafetyRuleResponse'),
    newDescribeSafetyRuleResponse,

    -- ** ListAssociatedRoute53HealthChecks (Paginated)
    ListAssociatedRoute53HealthChecks (ListAssociatedRoute53HealthChecks'),
    newListAssociatedRoute53HealthChecks,
    ListAssociatedRoute53HealthChecksResponse (ListAssociatedRoute53HealthChecksResponse'),
    newListAssociatedRoute53HealthChecksResponse,

    -- ** ListClusters (Paginated)
    ListClusters (ListClusters'),
    newListClusters,
    ListClustersResponse (ListClustersResponse'),
    newListClustersResponse,

    -- ** ListControlPanels (Paginated)
    ListControlPanels (ListControlPanels'),
    newListControlPanels,
    ListControlPanelsResponse (ListControlPanelsResponse'),
    newListControlPanelsResponse,

    -- ** ListRoutingControls (Paginated)
    ListRoutingControls (ListRoutingControls'),
    newListRoutingControls,
    ListRoutingControlsResponse (ListRoutingControlsResponse'),
    newListRoutingControlsResponse,

    -- ** ListSafetyRules (Paginated)
    ListSafetyRules (ListSafetyRules'),
    newListSafetyRules,
    ListSafetyRulesResponse (ListSafetyRulesResponse'),
    newListSafetyRulesResponse,

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

    -- ** UpdateControlPanel
    UpdateControlPanel (UpdateControlPanel'),
    newUpdateControlPanel,
    UpdateControlPanelResponse (UpdateControlPanelResponse'),
    newUpdateControlPanelResponse,

    -- ** UpdateRoutingControl
    UpdateRoutingControl (UpdateRoutingControl'),
    newUpdateRoutingControl,
    UpdateRoutingControlResponse (UpdateRoutingControlResponse'),
    newUpdateRoutingControlResponse,

    -- ** UpdateSafetyRule
    UpdateSafetyRule (UpdateSafetyRule'),
    newUpdateSafetyRule,
    UpdateSafetyRuleResponse (UpdateSafetyRuleResponse'),
    newUpdateSafetyRuleResponse,

    -- * Types

    -- ** RuleType
    RuleType (..),

    -- ** Status
    Status (..),

    -- ** AssertionRule
    AssertionRule (AssertionRule'),
    newAssertionRule,

    -- ** AssertionRuleUpdate
    AssertionRuleUpdate (AssertionRuleUpdate'),
    newAssertionRuleUpdate,

    -- ** Cluster
    Cluster (Cluster'),
    newCluster,

    -- ** ClusterEndpoint
    ClusterEndpoint (ClusterEndpoint'),
    newClusterEndpoint,

    -- ** ControlPanel
    ControlPanel (ControlPanel'),
    newControlPanel,

    -- ** GatingRule
    GatingRule (GatingRule'),
    newGatingRule,

    -- ** GatingRuleUpdate
    GatingRuleUpdate (GatingRuleUpdate'),
    newGatingRuleUpdate,

    -- ** NewAssertionRule
    NewAssertionRule (NewAssertionRule'),
    newNewAssertionRule,

    -- ** NewGatingRule
    NewGatingRule (NewGatingRule'),
    newNewGatingRule,

    -- ** RoutingControl
    RoutingControl (RoutingControl'),
    newRoutingControl,

    -- ** Rule
    Rule (Rule'),
    newRule,

    -- ** RuleConfig
    RuleConfig (RuleConfig'),
    newRuleConfig,
  )
where

import Amazonka.Route53RecoveryControlConfig.CreateCluster
import Amazonka.Route53RecoveryControlConfig.CreateControlPanel
import Amazonka.Route53RecoveryControlConfig.CreateRoutingControl
import Amazonka.Route53RecoveryControlConfig.CreateSafetyRule
import Amazonka.Route53RecoveryControlConfig.DeleteCluster
import Amazonka.Route53RecoveryControlConfig.DeleteControlPanel
import Amazonka.Route53RecoveryControlConfig.DeleteRoutingControl
import Amazonka.Route53RecoveryControlConfig.DeleteSafetyRule
import Amazonka.Route53RecoveryControlConfig.DescribeCluster
import Amazonka.Route53RecoveryControlConfig.DescribeControlPanel
import Amazonka.Route53RecoveryControlConfig.DescribeRoutingControl
import Amazonka.Route53RecoveryControlConfig.DescribeSafetyRule
import Amazonka.Route53RecoveryControlConfig.Lens
import Amazonka.Route53RecoveryControlConfig.ListAssociatedRoute53HealthChecks
import Amazonka.Route53RecoveryControlConfig.ListClusters
import Amazonka.Route53RecoveryControlConfig.ListControlPanels
import Amazonka.Route53RecoveryControlConfig.ListRoutingControls
import Amazonka.Route53RecoveryControlConfig.ListSafetyRules
import Amazonka.Route53RecoveryControlConfig.ListTagsForResource
import Amazonka.Route53RecoveryControlConfig.TagResource
import Amazonka.Route53RecoveryControlConfig.Types
import Amazonka.Route53RecoveryControlConfig.UntagResource
import Amazonka.Route53RecoveryControlConfig.UpdateControlPanel
import Amazonka.Route53RecoveryControlConfig.UpdateRoutingControl
import Amazonka.Route53RecoveryControlConfig.UpdateSafetyRule
import Amazonka.Route53RecoveryControlConfig.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'Route53RecoveryControlConfig'.

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
