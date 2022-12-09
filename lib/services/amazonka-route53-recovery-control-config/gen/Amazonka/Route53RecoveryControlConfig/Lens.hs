{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Route53RecoveryControlConfig.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53RecoveryControlConfig.Lens
  ( -- * Operations

    -- ** CreateCluster
    createCluster_clientToken,
    createCluster_tags,
    createCluster_clusterName,
    createClusterResponse_cluster,
    createClusterResponse_httpStatus,

    -- ** CreateControlPanel
    createControlPanel_clientToken,
    createControlPanel_tags,
    createControlPanel_clusterArn,
    createControlPanel_controlPanelName,
    createControlPanelResponse_controlPanel,
    createControlPanelResponse_httpStatus,

    -- ** CreateRoutingControl
    createRoutingControl_clientToken,
    createRoutingControl_controlPanelArn,
    createRoutingControl_clusterArn,
    createRoutingControl_routingControlName,
    createRoutingControlResponse_routingControl,
    createRoutingControlResponse_httpStatus,

    -- ** CreateSafetyRule
    createSafetyRule_assertionRule,
    createSafetyRule_clientToken,
    createSafetyRule_gatingRule,
    createSafetyRule_tags,
    createSafetyRuleResponse_assertionRule,
    createSafetyRuleResponse_gatingRule,
    createSafetyRuleResponse_httpStatus,

    -- ** DeleteCluster
    deleteCluster_clusterArn,
    deleteClusterResponse_httpStatus,

    -- ** DeleteControlPanel
    deleteControlPanel_controlPanelArn,
    deleteControlPanelResponse_httpStatus,

    -- ** DeleteRoutingControl
    deleteRoutingControl_routingControlArn,
    deleteRoutingControlResponse_httpStatus,

    -- ** DeleteSafetyRule
    deleteSafetyRule_safetyRuleArn,
    deleteSafetyRuleResponse_httpStatus,

    -- ** DescribeCluster
    describeCluster_clusterArn,
    describeClusterResponse_cluster,
    describeClusterResponse_httpStatus,

    -- ** DescribeControlPanel
    describeControlPanel_controlPanelArn,
    describeControlPanelResponse_controlPanel,
    describeControlPanelResponse_httpStatus,

    -- ** DescribeRoutingControl
    describeRoutingControl_routingControlArn,
    describeRoutingControlResponse_routingControl,
    describeRoutingControlResponse_httpStatus,

    -- ** DescribeSafetyRule
    describeSafetyRule_safetyRuleArn,
    describeSafetyRuleResponse_assertionRule,
    describeSafetyRuleResponse_gatingRule,
    describeSafetyRuleResponse_httpStatus,

    -- ** ListAssociatedRoute53HealthChecks
    listAssociatedRoute53HealthChecks_maxResults,
    listAssociatedRoute53HealthChecks_nextToken,
    listAssociatedRoute53HealthChecks_routingControlArn,
    listAssociatedRoute53HealthChecksResponse_healthCheckIds,
    listAssociatedRoute53HealthChecksResponse_nextToken,
    listAssociatedRoute53HealthChecksResponse_httpStatus,

    -- ** ListClusters
    listClusters_maxResults,
    listClusters_nextToken,
    listClustersResponse_clusters,
    listClustersResponse_nextToken,
    listClustersResponse_httpStatus,

    -- ** ListControlPanels
    listControlPanels_clusterArn,
    listControlPanels_maxResults,
    listControlPanels_nextToken,
    listControlPanelsResponse_controlPanels,
    listControlPanelsResponse_nextToken,
    listControlPanelsResponse_httpStatus,

    -- ** ListRoutingControls
    listRoutingControls_maxResults,
    listRoutingControls_nextToken,
    listRoutingControls_controlPanelArn,
    listRoutingControlsResponse_nextToken,
    listRoutingControlsResponse_routingControls,
    listRoutingControlsResponse_httpStatus,

    -- ** ListSafetyRules
    listSafetyRules_maxResults,
    listSafetyRules_nextToken,
    listSafetyRules_controlPanelArn,
    listSafetyRulesResponse_nextToken,
    listSafetyRulesResponse_safetyRules,
    listSafetyRulesResponse_httpStatus,

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

    -- ** UpdateControlPanel
    updateControlPanel_controlPanelArn,
    updateControlPanel_controlPanelName,
    updateControlPanelResponse_controlPanel,
    updateControlPanelResponse_httpStatus,

    -- ** UpdateRoutingControl
    updateRoutingControl_routingControlName,
    updateRoutingControl_routingControlArn,
    updateRoutingControlResponse_routingControl,
    updateRoutingControlResponse_httpStatus,

    -- ** UpdateSafetyRule
    updateSafetyRule_assertionRuleUpdate,
    updateSafetyRule_gatingRuleUpdate,
    updateSafetyRuleResponse_assertionRule,
    updateSafetyRuleResponse_gatingRule,
    updateSafetyRuleResponse_httpStatus,

    -- * Types

    -- ** AssertionRule
    assertionRule_status,
    assertionRule_controlPanelArn,
    assertionRule_safetyRuleArn,
    assertionRule_assertedControls,
    assertionRule_ruleConfig,
    assertionRule_waitPeriodMs,
    assertionRule_name,

    -- ** AssertionRuleUpdate
    assertionRuleUpdate_safetyRuleArn,
    assertionRuleUpdate_waitPeriodMs,
    assertionRuleUpdate_name,

    -- ** Cluster
    cluster_clusterArn,
    cluster_clusterEndpoints,
    cluster_name,
    cluster_status,

    -- ** ClusterEndpoint
    clusterEndpoint_endpoint,
    clusterEndpoint_region,

    -- ** ControlPanel
    controlPanel_clusterArn,
    controlPanel_controlPanelArn,
    controlPanel_defaultControlPanel,
    controlPanel_name,
    controlPanel_routingControlCount,
    controlPanel_status,

    -- ** GatingRule
    gatingRule_status,
    gatingRule_targetControls,
    gatingRule_controlPanelArn,
    gatingRule_safetyRuleArn,
    gatingRule_gatingControls,
    gatingRule_ruleConfig,
    gatingRule_waitPeriodMs,
    gatingRule_name,

    -- ** GatingRuleUpdate
    gatingRuleUpdate_safetyRuleArn,
    gatingRuleUpdate_waitPeriodMs,
    gatingRuleUpdate_name,

    -- ** NewAssertionRule
    newAssertionRule_controlPanelArn,
    newAssertionRule_assertedControls,
    newAssertionRule_ruleConfig,
    newAssertionRule_waitPeriodMs,
    newAssertionRule_name,

    -- ** NewGatingRule
    newGatingRule_targetControls,
    newGatingRule_controlPanelArn,
    newGatingRule_gatingControls,
    newGatingRule_ruleConfig,
    newGatingRule_waitPeriodMs,
    newGatingRule_name,

    -- ** RoutingControl
    routingControl_controlPanelArn,
    routingControl_name,
    routingControl_routingControlArn,
    routingControl_status,

    -- ** Rule
    rule_assertion,
    rule_gating,

    -- ** RuleConfig
    ruleConfig_type,
    ruleConfig_inverted,
    ruleConfig_threshold,
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
import Amazonka.Route53RecoveryControlConfig.ListAssociatedRoute53HealthChecks
import Amazonka.Route53RecoveryControlConfig.ListClusters
import Amazonka.Route53RecoveryControlConfig.ListControlPanels
import Amazonka.Route53RecoveryControlConfig.ListRoutingControls
import Amazonka.Route53RecoveryControlConfig.ListSafetyRules
import Amazonka.Route53RecoveryControlConfig.ListTagsForResource
import Amazonka.Route53RecoveryControlConfig.TagResource
import Amazonka.Route53RecoveryControlConfig.Types.AssertionRule
import Amazonka.Route53RecoveryControlConfig.Types.AssertionRuleUpdate
import Amazonka.Route53RecoveryControlConfig.Types.Cluster
import Amazonka.Route53RecoveryControlConfig.Types.ClusterEndpoint
import Amazonka.Route53RecoveryControlConfig.Types.ControlPanel
import Amazonka.Route53RecoveryControlConfig.Types.GatingRule
import Amazonka.Route53RecoveryControlConfig.Types.GatingRuleUpdate
import Amazonka.Route53RecoveryControlConfig.Types.NewAssertionRule
import Amazonka.Route53RecoveryControlConfig.Types.NewGatingRule
import Amazonka.Route53RecoveryControlConfig.Types.RoutingControl
import Amazonka.Route53RecoveryControlConfig.Types.Rule
import Amazonka.Route53RecoveryControlConfig.Types.RuleConfig
import Amazonka.Route53RecoveryControlConfig.UntagResource
import Amazonka.Route53RecoveryControlConfig.UpdateControlPanel
import Amazonka.Route53RecoveryControlConfig.UpdateRoutingControl
import Amazonka.Route53RecoveryControlConfig.UpdateSafetyRule
