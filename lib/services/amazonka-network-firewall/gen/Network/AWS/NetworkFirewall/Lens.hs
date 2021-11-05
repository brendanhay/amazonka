{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.NetworkFirewall.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.NetworkFirewall.Lens
  ( -- * Operations

    -- ** AssociateSubnets
    associateSubnets_updateToken,
    associateSubnets_firewallArn,
    associateSubnets_firewallName,
    associateSubnets_subnetMappings,
    associateSubnetsResponse_subnetMappings,
    associateSubnetsResponse_updateToken,
    associateSubnetsResponse_firewallArn,
    associateSubnetsResponse_firewallName,
    associateSubnetsResponse_httpStatus,

    -- ** UpdateSubnetChangeProtection
    updateSubnetChangeProtection_updateToken,
    updateSubnetChangeProtection_firewallArn,
    updateSubnetChangeProtection_firewallName,
    updateSubnetChangeProtection_subnetChangeProtection,
    updateSubnetChangeProtectionResponse_updateToken,
    updateSubnetChangeProtectionResponse_firewallArn,
    updateSubnetChangeProtectionResponse_subnetChangeProtection,
    updateSubnetChangeProtectionResponse_firewallName,
    updateSubnetChangeProtectionResponse_httpStatus,

    -- ** UpdateFirewallPolicy
    updateFirewallPolicy_firewallPolicyName,
    updateFirewallPolicy_firewallPolicyArn,
    updateFirewallPolicy_description,
    updateFirewallPolicy_dryRun,
    updateFirewallPolicy_updateToken,
    updateFirewallPolicy_firewallPolicy,
    updateFirewallPolicyResponse_httpStatus,
    updateFirewallPolicyResponse_updateToken,
    updateFirewallPolicyResponse_firewallPolicyResponse,

    -- ** DeleteFirewallPolicy
    deleteFirewallPolicy_firewallPolicyName,
    deleteFirewallPolicy_firewallPolicyArn,
    deleteFirewallPolicyResponse_httpStatus,
    deleteFirewallPolicyResponse_firewallPolicyResponse,

    -- ** CreateFirewallPolicy
    createFirewallPolicy_description,
    createFirewallPolicy_dryRun,
    createFirewallPolicy_tags,
    createFirewallPolicy_firewallPolicyName,
    createFirewallPolicy_firewallPolicy,
    createFirewallPolicyResponse_httpStatus,
    createFirewallPolicyResponse_updateToken,
    createFirewallPolicyResponse_firewallPolicyResponse,

    -- ** UpdateLoggingConfiguration
    updateLoggingConfiguration_firewallArn,
    updateLoggingConfiguration_loggingConfiguration,
    updateLoggingConfiguration_firewallName,
    updateLoggingConfigurationResponse_firewallArn,
    updateLoggingConfigurationResponse_loggingConfiguration,
    updateLoggingConfigurationResponse_firewallName,
    updateLoggingConfigurationResponse_httpStatus,

    -- ** DisassociateSubnets
    disassociateSubnets_updateToken,
    disassociateSubnets_firewallArn,
    disassociateSubnets_firewallName,
    disassociateSubnets_subnetIds,
    disassociateSubnetsResponse_subnetMappings,
    disassociateSubnetsResponse_updateToken,
    disassociateSubnetsResponse_firewallArn,
    disassociateSubnetsResponse_firewallName,
    disassociateSubnetsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_nextToken,
    listTagsForResource_maxResults,
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_nextToken,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListFirewallPolicies
    listFirewallPolicies_nextToken,
    listFirewallPolicies_maxResults,
    listFirewallPoliciesResponse_firewallPolicies,
    listFirewallPoliciesResponse_nextToken,
    listFirewallPoliciesResponse_httpStatus,

    -- ** UpdateFirewallDeleteProtection
    updateFirewallDeleteProtection_updateToken,
    updateFirewallDeleteProtection_firewallArn,
    updateFirewallDeleteProtection_firewallName,
    updateFirewallDeleteProtection_deleteProtection,
    updateFirewallDeleteProtectionResponse_updateToken,
    updateFirewallDeleteProtectionResponse_firewallArn,
    updateFirewallDeleteProtectionResponse_deleteProtection,
    updateFirewallDeleteProtectionResponse_firewallName,
    updateFirewallDeleteProtectionResponse_httpStatus,

    -- ** CreateRuleGroup
    createRuleGroup_rules,
    createRuleGroup_description,
    createRuleGroup_dryRun,
    createRuleGroup_tags,
    createRuleGroup_ruleGroup,
    createRuleGroup_ruleGroupName,
    createRuleGroup_type,
    createRuleGroup_capacity,
    createRuleGroupResponse_httpStatus,
    createRuleGroupResponse_updateToken,
    createRuleGroupResponse_ruleGroupResponse,

    -- ** DescribeFirewallPolicy
    describeFirewallPolicy_firewallPolicyName,
    describeFirewallPolicy_firewallPolicyArn,
    describeFirewallPolicyResponse_firewallPolicy,
    describeFirewallPolicyResponse_httpStatus,
    describeFirewallPolicyResponse_updateToken,
    describeFirewallPolicyResponse_firewallPolicyResponse,

    -- ** UpdateFirewallDescription
    updateFirewallDescription_updateToken,
    updateFirewallDescription_firewallArn,
    updateFirewallDescription_description,
    updateFirewallDescription_firewallName,
    updateFirewallDescriptionResponse_updateToken,
    updateFirewallDescriptionResponse_firewallArn,
    updateFirewallDescriptionResponse_description,
    updateFirewallDescriptionResponse_firewallName,
    updateFirewallDescriptionResponse_httpStatus,

    -- ** DescribeRuleGroup
    describeRuleGroup_ruleGroupArn,
    describeRuleGroup_type,
    describeRuleGroup_ruleGroupName,
    describeRuleGroupResponse_ruleGroup,
    describeRuleGroupResponse_httpStatus,
    describeRuleGroupResponse_updateToken,
    describeRuleGroupResponse_ruleGroupResponse,

    -- ** DeleteFirewall
    deleteFirewall_firewallArn,
    deleteFirewall_firewallName,
    deleteFirewallResponse_firewallStatus,
    deleteFirewallResponse_firewall,
    deleteFirewallResponse_httpStatus,

    -- ** ListFirewalls
    listFirewalls_nextToken,
    listFirewalls_vpcIds,
    listFirewalls_maxResults,
    listFirewallsResponse_nextToken,
    listFirewallsResponse_firewalls,
    listFirewallsResponse_httpStatus,

    -- ** DescribeResourcePolicy
    describeResourcePolicy_resourceArn,
    describeResourcePolicyResponse_policy,
    describeResourcePolicyResponse_httpStatus,

    -- ** AssociateFirewallPolicy
    associateFirewallPolicy_updateToken,
    associateFirewallPolicy_firewallArn,
    associateFirewallPolicy_firewallName,
    associateFirewallPolicy_firewallPolicyArn,
    associateFirewallPolicyResponse_updateToken,
    associateFirewallPolicyResponse_firewallArn,
    associateFirewallPolicyResponse_firewallPolicyArn,
    associateFirewallPolicyResponse_firewallName,
    associateFirewallPolicyResponse_httpStatus,

    -- ** UpdateFirewallPolicyChangeProtection
    updateFirewallPolicyChangeProtection_updateToken,
    updateFirewallPolicyChangeProtection_firewallArn,
    updateFirewallPolicyChangeProtection_firewallName,
    updateFirewallPolicyChangeProtection_firewallPolicyChangeProtection,
    updateFirewallPolicyChangeProtectionResponse_updateToken,
    updateFirewallPolicyChangeProtectionResponse_firewallArn,
    updateFirewallPolicyChangeProtectionResponse_firewallPolicyChangeProtection,
    updateFirewallPolicyChangeProtectionResponse_firewallName,
    updateFirewallPolicyChangeProtectionResponse_httpStatus,

    -- ** CreateFirewall
    createFirewall_firewallPolicyChangeProtection,
    createFirewall_subnetChangeProtection,
    createFirewall_deleteProtection,
    createFirewall_description,
    createFirewall_tags,
    createFirewall_firewallName,
    createFirewall_firewallPolicyArn,
    createFirewall_vpcId,
    createFirewall_subnetMappings,
    createFirewallResponse_firewallStatus,
    createFirewallResponse_firewall,
    createFirewallResponse_httpStatus,

    -- ** ListRuleGroups
    listRuleGroups_nextToken,
    listRuleGroups_maxResults,
    listRuleGroupsResponse_nextToken,
    listRuleGroupsResponse_ruleGroups,
    listRuleGroupsResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** DeleteRuleGroup
    deleteRuleGroup_ruleGroupArn,
    deleteRuleGroup_type,
    deleteRuleGroup_ruleGroupName,
    deleteRuleGroupResponse_httpStatus,
    deleteRuleGroupResponse_ruleGroupResponse,

    -- ** UpdateRuleGroup
    updateRuleGroup_ruleGroupArn,
    updateRuleGroup_rules,
    updateRuleGroup_type,
    updateRuleGroup_description,
    updateRuleGroup_ruleGroupName,
    updateRuleGroup_dryRun,
    updateRuleGroup_ruleGroup,
    updateRuleGroup_updateToken,
    updateRuleGroupResponse_httpStatus,
    updateRuleGroupResponse_updateToken,
    updateRuleGroupResponse_ruleGroupResponse,

    -- ** PutResourcePolicy
    putResourcePolicy_resourceArn,
    putResourcePolicy_policy,
    putResourcePolicyResponse_httpStatus,

    -- ** DescribeFirewall
    describeFirewall_firewallArn,
    describeFirewall_firewallName,
    describeFirewallResponse_firewallStatus,
    describeFirewallResponse_updateToken,
    describeFirewallResponse_firewall,
    describeFirewallResponse_httpStatus,

    -- ** DeleteResourcePolicy
    deleteResourcePolicy_resourceArn,
    deleteResourcePolicyResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** DescribeLoggingConfiguration
    describeLoggingConfiguration_firewallArn,
    describeLoggingConfiguration_firewallName,
    describeLoggingConfigurationResponse_firewallArn,
    describeLoggingConfigurationResponse_loggingConfiguration,
    describeLoggingConfigurationResponse_httpStatus,

    -- * Types

    -- ** ActionDefinition
    actionDefinition_publishMetricAction,

    -- ** Address
    address_addressDefinition,

    -- ** Attachment
    attachment_status,
    attachment_subnetId,
    attachment_endpointId,

    -- ** CustomAction
    customAction_actionName,
    customAction_actionDefinition,

    -- ** Dimension
    dimension_value,

    -- ** Firewall
    firewall_firewallArn,
    firewall_firewallPolicyChangeProtection,
    firewall_subnetChangeProtection,
    firewall_deleteProtection,
    firewall_description,
    firewall_tags,
    firewall_firewallName,
    firewall_firewallPolicyArn,
    firewall_vpcId,
    firewall_subnetMappings,
    firewall_firewallId,

    -- ** FirewallMetadata
    firewallMetadata_firewallArn,
    firewallMetadata_firewallName,

    -- ** FirewallPolicy
    firewallPolicy_statefulEngineOptions,
    firewallPolicy_statefulRuleGroupReferences,
    firewallPolicy_statelessRuleGroupReferences,
    firewallPolicy_statelessCustomActions,
    firewallPolicy_statefulDefaultActions,
    firewallPolicy_statelessDefaultActions,
    firewallPolicy_statelessFragmentDefaultActions,

    -- ** FirewallPolicyMetadata
    firewallPolicyMetadata_arn,
    firewallPolicyMetadata_name,

    -- ** FirewallPolicyResponse
    firewallPolicyResponse_consumedStatelessRuleCapacity,
    firewallPolicyResponse_numberOfAssociations,
    firewallPolicyResponse_firewallPolicyStatus,
    firewallPolicyResponse_consumedStatefulRuleCapacity,
    firewallPolicyResponse_description,
    firewallPolicyResponse_tags,
    firewallPolicyResponse_firewallPolicyName,
    firewallPolicyResponse_firewallPolicyArn,
    firewallPolicyResponse_firewallPolicyId,

    -- ** FirewallStatus
    firewallStatus_syncStates,
    firewallStatus_status,
    firewallStatus_configurationSyncStateSummary,

    -- ** Header
    header_protocol,
    header_source,
    header_sourcePort,
    header_direction,
    header_destination,
    header_destinationPort,

    -- ** IPSet
    iPSet_definition,

    -- ** LogDestinationConfig
    logDestinationConfig_logType,
    logDestinationConfig_logDestinationType,
    logDestinationConfig_logDestination,

    -- ** LoggingConfiguration
    loggingConfiguration_logDestinationConfigs,

    -- ** MatchAttributes
    matchAttributes_protocols,
    matchAttributes_tCPFlags,
    matchAttributes_destinationPorts,
    matchAttributes_sources,
    matchAttributes_sourcePorts,
    matchAttributes_destinations,

    -- ** PerObjectStatus
    perObjectStatus_updateToken,
    perObjectStatus_syncStatus,

    -- ** PortRange
    portRange_fromPort,
    portRange_toPort,

    -- ** PortSet
    portSet_definition,

    -- ** PublishMetricAction
    publishMetricAction_dimensions,

    -- ** RuleDefinition
    ruleDefinition_matchAttributes,
    ruleDefinition_actions,

    -- ** RuleGroup
    ruleGroup_statefulRuleOptions,
    ruleGroup_ruleVariables,
    ruleGroup_rulesSource,

    -- ** RuleGroupMetadata
    ruleGroupMetadata_arn,
    ruleGroupMetadata_name,

    -- ** RuleGroupResponse
    ruleGroupResponse_numberOfAssociations,
    ruleGroupResponse_capacity,
    ruleGroupResponse_consumedCapacity,
    ruleGroupResponse_ruleGroupStatus,
    ruleGroupResponse_type,
    ruleGroupResponse_description,
    ruleGroupResponse_tags,
    ruleGroupResponse_ruleGroupArn,
    ruleGroupResponse_ruleGroupName,
    ruleGroupResponse_ruleGroupId,

    -- ** RuleOption
    ruleOption_settings,
    ruleOption_keyword,

    -- ** RuleVariables
    ruleVariables_portSets,
    ruleVariables_iPSets,

    -- ** RulesSource
    rulesSource_rulesString,
    rulesSource_rulesSourceList,
    rulesSource_statefulRules,
    rulesSource_statelessRulesAndCustomActions,

    -- ** RulesSourceList
    rulesSourceList_targets,
    rulesSourceList_targetTypes,
    rulesSourceList_generatedRulesType,

    -- ** StatefulEngineOptions
    statefulEngineOptions_ruleOrder,

    -- ** StatefulRule
    statefulRule_action,
    statefulRule_header,
    statefulRule_ruleOptions,

    -- ** StatefulRuleGroupReference
    statefulRuleGroupReference_priority,
    statefulRuleGroupReference_resourceArn,

    -- ** StatefulRuleOptions
    statefulRuleOptions_ruleOrder,

    -- ** StatelessRule
    statelessRule_ruleDefinition,
    statelessRule_priority,

    -- ** StatelessRuleGroupReference
    statelessRuleGroupReference_resourceArn,
    statelessRuleGroupReference_priority,

    -- ** StatelessRulesAndCustomActions
    statelessRulesAndCustomActions_customActions,
    statelessRulesAndCustomActions_statelessRules,

    -- ** SubnetMapping
    subnetMapping_subnetId,

    -- ** SyncState
    syncState_config,
    syncState_attachment,

    -- ** TCPFlagField
    tCPFlagField_masks,
    tCPFlagField_flags,

    -- ** Tag
    tag_key,
    tag_value,
  )
where

import Network.AWS.NetworkFirewall.AssociateFirewallPolicy
import Network.AWS.NetworkFirewall.AssociateSubnets
import Network.AWS.NetworkFirewall.CreateFirewall
import Network.AWS.NetworkFirewall.CreateFirewallPolicy
import Network.AWS.NetworkFirewall.CreateRuleGroup
import Network.AWS.NetworkFirewall.DeleteFirewall
import Network.AWS.NetworkFirewall.DeleteFirewallPolicy
import Network.AWS.NetworkFirewall.DeleteResourcePolicy
import Network.AWS.NetworkFirewall.DeleteRuleGroup
import Network.AWS.NetworkFirewall.DescribeFirewall
import Network.AWS.NetworkFirewall.DescribeFirewallPolicy
import Network.AWS.NetworkFirewall.DescribeLoggingConfiguration
import Network.AWS.NetworkFirewall.DescribeResourcePolicy
import Network.AWS.NetworkFirewall.DescribeRuleGroup
import Network.AWS.NetworkFirewall.DisassociateSubnets
import Network.AWS.NetworkFirewall.ListFirewallPolicies
import Network.AWS.NetworkFirewall.ListFirewalls
import Network.AWS.NetworkFirewall.ListRuleGroups
import Network.AWS.NetworkFirewall.ListTagsForResource
import Network.AWS.NetworkFirewall.PutResourcePolicy
import Network.AWS.NetworkFirewall.TagResource
import Network.AWS.NetworkFirewall.Types.ActionDefinition
import Network.AWS.NetworkFirewall.Types.Address
import Network.AWS.NetworkFirewall.Types.Attachment
import Network.AWS.NetworkFirewall.Types.CustomAction
import Network.AWS.NetworkFirewall.Types.Dimension
import Network.AWS.NetworkFirewall.Types.Firewall
import Network.AWS.NetworkFirewall.Types.FirewallMetadata
import Network.AWS.NetworkFirewall.Types.FirewallPolicy
import Network.AWS.NetworkFirewall.Types.FirewallPolicyMetadata
import Network.AWS.NetworkFirewall.Types.FirewallPolicyResponse
import Network.AWS.NetworkFirewall.Types.FirewallStatus
import Network.AWS.NetworkFirewall.Types.Header
import Network.AWS.NetworkFirewall.Types.IPSet
import Network.AWS.NetworkFirewall.Types.LogDestinationConfig
import Network.AWS.NetworkFirewall.Types.LoggingConfiguration
import Network.AWS.NetworkFirewall.Types.MatchAttributes
import Network.AWS.NetworkFirewall.Types.PerObjectStatus
import Network.AWS.NetworkFirewall.Types.PortRange
import Network.AWS.NetworkFirewall.Types.PortSet
import Network.AWS.NetworkFirewall.Types.PublishMetricAction
import Network.AWS.NetworkFirewall.Types.RuleDefinition
import Network.AWS.NetworkFirewall.Types.RuleGroup
import Network.AWS.NetworkFirewall.Types.RuleGroupMetadata
import Network.AWS.NetworkFirewall.Types.RuleGroupResponse
import Network.AWS.NetworkFirewall.Types.RuleOption
import Network.AWS.NetworkFirewall.Types.RuleVariables
import Network.AWS.NetworkFirewall.Types.RulesSource
import Network.AWS.NetworkFirewall.Types.RulesSourceList
import Network.AWS.NetworkFirewall.Types.StatefulEngineOptions
import Network.AWS.NetworkFirewall.Types.StatefulRule
import Network.AWS.NetworkFirewall.Types.StatefulRuleGroupReference
import Network.AWS.NetworkFirewall.Types.StatefulRuleOptions
import Network.AWS.NetworkFirewall.Types.StatelessRule
import Network.AWS.NetworkFirewall.Types.StatelessRuleGroupReference
import Network.AWS.NetworkFirewall.Types.StatelessRulesAndCustomActions
import Network.AWS.NetworkFirewall.Types.SubnetMapping
import Network.AWS.NetworkFirewall.Types.SyncState
import Network.AWS.NetworkFirewall.Types.TCPFlagField
import Network.AWS.NetworkFirewall.Types.Tag
import Network.AWS.NetworkFirewall.UntagResource
import Network.AWS.NetworkFirewall.UpdateFirewallDeleteProtection
import Network.AWS.NetworkFirewall.UpdateFirewallDescription
import Network.AWS.NetworkFirewall.UpdateFirewallPolicy
import Network.AWS.NetworkFirewall.UpdateFirewallPolicyChangeProtection
import Network.AWS.NetworkFirewall.UpdateLoggingConfiguration
import Network.AWS.NetworkFirewall.UpdateRuleGroup
import Network.AWS.NetworkFirewall.UpdateSubnetChangeProtection
