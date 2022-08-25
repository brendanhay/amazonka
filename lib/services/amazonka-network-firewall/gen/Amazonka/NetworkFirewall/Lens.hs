{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.NetworkFirewall.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkFirewall.Lens
  ( -- * Operations

    -- ** AssociateFirewallPolicy
    associateFirewallPolicy_updateToken,
    associateFirewallPolicy_firewallArn,
    associateFirewallPolicy_firewallName,
    associateFirewallPolicy_firewallPolicyArn,
    associateFirewallPolicyResponse_updateToken,
    associateFirewallPolicyResponse_firewallArn,
    associateFirewallPolicyResponse_firewallName,
    associateFirewallPolicyResponse_firewallPolicyArn,
    associateFirewallPolicyResponse_httpStatus,

    -- ** AssociateSubnets
    associateSubnets_updateToken,
    associateSubnets_firewallArn,
    associateSubnets_firewallName,
    associateSubnets_subnetMappings,
    associateSubnetsResponse_updateToken,
    associateSubnetsResponse_firewallArn,
    associateSubnetsResponse_subnetMappings,
    associateSubnetsResponse_firewallName,
    associateSubnetsResponse_httpStatus,

    -- ** CreateFirewall
    createFirewall_tags,
    createFirewall_deleteProtection,
    createFirewall_subnetChangeProtection,
    createFirewall_description,
    createFirewall_firewallPolicyChangeProtection,
    createFirewall_encryptionConfiguration,
    createFirewall_firewallName,
    createFirewall_firewallPolicyArn,
    createFirewall_vpcId,
    createFirewall_subnetMappings,
    createFirewallResponse_firewall,
    createFirewallResponse_firewallStatus,
    createFirewallResponse_httpStatus,

    -- ** CreateFirewallPolicy
    createFirewallPolicy_tags,
    createFirewallPolicy_description,
    createFirewallPolicy_dryRun,
    createFirewallPolicy_encryptionConfiguration,
    createFirewallPolicy_firewallPolicyName,
    createFirewallPolicy_firewallPolicy,
    createFirewallPolicyResponse_httpStatus,
    createFirewallPolicyResponse_updateToken,
    createFirewallPolicyResponse_firewallPolicyResponse,

    -- ** CreateRuleGroup
    createRuleGroup_tags,
    createRuleGroup_ruleGroup,
    createRuleGroup_rules,
    createRuleGroup_description,
    createRuleGroup_dryRun,
    createRuleGroup_sourceMetadata,
    createRuleGroup_encryptionConfiguration,
    createRuleGroup_ruleGroupName,
    createRuleGroup_type,
    createRuleGroup_capacity,
    createRuleGroupResponse_httpStatus,
    createRuleGroupResponse_updateToken,
    createRuleGroupResponse_ruleGroupResponse,

    -- ** DeleteFirewall
    deleteFirewall_firewallArn,
    deleteFirewall_firewallName,
    deleteFirewallResponse_firewall,
    deleteFirewallResponse_firewallStatus,
    deleteFirewallResponse_httpStatus,

    -- ** DeleteFirewallPolicy
    deleteFirewallPolicy_firewallPolicyName,
    deleteFirewallPolicy_firewallPolicyArn,
    deleteFirewallPolicyResponse_httpStatus,
    deleteFirewallPolicyResponse_firewallPolicyResponse,

    -- ** DeleteResourcePolicy
    deleteResourcePolicy_resourceArn,
    deleteResourcePolicyResponse_httpStatus,

    -- ** DeleteRuleGroup
    deleteRuleGroup_ruleGroupName,
    deleteRuleGroup_type,
    deleteRuleGroup_ruleGroupArn,
    deleteRuleGroupResponse_httpStatus,
    deleteRuleGroupResponse_ruleGroupResponse,

    -- ** DescribeFirewall
    describeFirewall_firewallArn,
    describeFirewall_firewallName,
    describeFirewallResponse_updateToken,
    describeFirewallResponse_firewall,
    describeFirewallResponse_firewallStatus,
    describeFirewallResponse_httpStatus,

    -- ** DescribeFirewallPolicy
    describeFirewallPolicy_firewallPolicyName,
    describeFirewallPolicy_firewallPolicyArn,
    describeFirewallPolicyResponse_firewallPolicy,
    describeFirewallPolicyResponse_httpStatus,
    describeFirewallPolicyResponse_updateToken,
    describeFirewallPolicyResponse_firewallPolicyResponse,

    -- ** DescribeLoggingConfiguration
    describeLoggingConfiguration_firewallArn,
    describeLoggingConfiguration_firewallName,
    describeLoggingConfigurationResponse_firewallArn,
    describeLoggingConfigurationResponse_loggingConfiguration,
    describeLoggingConfigurationResponse_httpStatus,

    -- ** DescribeResourcePolicy
    describeResourcePolicy_resourceArn,
    describeResourcePolicyResponse_policy,
    describeResourcePolicyResponse_httpStatus,

    -- ** DescribeRuleGroup
    describeRuleGroup_ruleGroupName,
    describeRuleGroup_type,
    describeRuleGroup_ruleGroupArn,
    describeRuleGroupResponse_ruleGroup,
    describeRuleGroupResponse_httpStatus,
    describeRuleGroupResponse_updateToken,
    describeRuleGroupResponse_ruleGroupResponse,

    -- ** DescribeRuleGroupMetadata
    describeRuleGroupMetadata_ruleGroupName,
    describeRuleGroupMetadata_type,
    describeRuleGroupMetadata_ruleGroupArn,
    describeRuleGroupMetadataResponse_type,
    describeRuleGroupMetadataResponse_statefulRuleOptions,
    describeRuleGroupMetadataResponse_description,
    describeRuleGroupMetadataResponse_lastModifiedTime,
    describeRuleGroupMetadataResponse_capacity,
    describeRuleGroupMetadataResponse_httpStatus,
    describeRuleGroupMetadataResponse_ruleGroupArn,
    describeRuleGroupMetadataResponse_ruleGroupName,

    -- ** DisassociateSubnets
    disassociateSubnets_updateToken,
    disassociateSubnets_firewallArn,
    disassociateSubnets_firewallName,
    disassociateSubnets_subnetIds,
    disassociateSubnetsResponse_updateToken,
    disassociateSubnetsResponse_firewallArn,
    disassociateSubnetsResponse_subnetMappings,
    disassociateSubnetsResponse_firewallName,
    disassociateSubnetsResponse_httpStatus,

    -- ** ListFirewallPolicies
    listFirewallPolicies_nextToken,
    listFirewallPolicies_maxResults,
    listFirewallPoliciesResponse_nextToken,
    listFirewallPoliciesResponse_firewallPolicies,
    listFirewallPoliciesResponse_httpStatus,

    -- ** ListFirewalls
    listFirewalls_nextToken,
    listFirewalls_vpcIds,
    listFirewalls_maxResults,
    listFirewallsResponse_nextToken,
    listFirewallsResponse_firewalls,
    listFirewallsResponse_httpStatus,

    -- ** ListRuleGroups
    listRuleGroups_nextToken,
    listRuleGroups_type,
    listRuleGroups_managedType,
    listRuleGroups_maxResults,
    listRuleGroups_scope,
    listRuleGroupsResponse_nextToken,
    listRuleGroupsResponse_ruleGroups,
    listRuleGroupsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_nextToken,
    listTagsForResource_maxResults,
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_nextToken,
    listTagsForResourceResponse_httpStatus,

    -- ** PutResourcePolicy
    putResourcePolicy_resourceArn,
    putResourcePolicy_policy,
    putResourcePolicyResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateFirewallDeleteProtection
    updateFirewallDeleteProtection_updateToken,
    updateFirewallDeleteProtection_firewallArn,
    updateFirewallDeleteProtection_firewallName,
    updateFirewallDeleteProtection_deleteProtection,
    updateFirewallDeleteProtectionResponse_updateToken,
    updateFirewallDeleteProtectionResponse_deleteProtection,
    updateFirewallDeleteProtectionResponse_firewallArn,
    updateFirewallDeleteProtectionResponse_firewallName,
    updateFirewallDeleteProtectionResponse_httpStatus,

    -- ** UpdateFirewallDescription
    updateFirewallDescription_updateToken,
    updateFirewallDescription_description,
    updateFirewallDescription_firewallArn,
    updateFirewallDescription_firewallName,
    updateFirewallDescriptionResponse_updateToken,
    updateFirewallDescriptionResponse_description,
    updateFirewallDescriptionResponse_firewallArn,
    updateFirewallDescriptionResponse_firewallName,
    updateFirewallDescriptionResponse_httpStatus,

    -- ** UpdateFirewallEncryptionConfiguration
    updateFirewallEncryptionConfiguration_updateToken,
    updateFirewallEncryptionConfiguration_firewallArn,
    updateFirewallEncryptionConfiguration_encryptionConfiguration,
    updateFirewallEncryptionConfiguration_firewallName,
    updateFirewallEncryptionConfigurationResponse_updateToken,
    updateFirewallEncryptionConfigurationResponse_firewallArn,
    updateFirewallEncryptionConfigurationResponse_encryptionConfiguration,
    updateFirewallEncryptionConfigurationResponse_firewallName,
    updateFirewallEncryptionConfigurationResponse_httpStatus,

    -- ** UpdateFirewallPolicy
    updateFirewallPolicy_firewallPolicyName,
    updateFirewallPolicy_description,
    updateFirewallPolicy_dryRun,
    updateFirewallPolicy_encryptionConfiguration,
    updateFirewallPolicy_firewallPolicyArn,
    updateFirewallPolicy_updateToken,
    updateFirewallPolicy_firewallPolicy,
    updateFirewallPolicyResponse_httpStatus,
    updateFirewallPolicyResponse_updateToken,
    updateFirewallPolicyResponse_firewallPolicyResponse,

    -- ** UpdateFirewallPolicyChangeProtection
    updateFirewallPolicyChangeProtection_updateToken,
    updateFirewallPolicyChangeProtection_firewallArn,
    updateFirewallPolicyChangeProtection_firewallName,
    updateFirewallPolicyChangeProtection_firewallPolicyChangeProtection,
    updateFirewallPolicyChangeProtectionResponse_updateToken,
    updateFirewallPolicyChangeProtectionResponse_firewallPolicyChangeProtection,
    updateFirewallPolicyChangeProtectionResponse_firewallArn,
    updateFirewallPolicyChangeProtectionResponse_firewallName,
    updateFirewallPolicyChangeProtectionResponse_httpStatus,

    -- ** UpdateLoggingConfiguration
    updateLoggingConfiguration_firewallArn,
    updateLoggingConfiguration_firewallName,
    updateLoggingConfiguration_loggingConfiguration,
    updateLoggingConfigurationResponse_firewallArn,
    updateLoggingConfigurationResponse_firewallName,
    updateLoggingConfigurationResponse_loggingConfiguration,
    updateLoggingConfigurationResponse_httpStatus,

    -- ** UpdateRuleGroup
    updateRuleGroup_ruleGroupName,
    updateRuleGroup_ruleGroup,
    updateRuleGroup_type,
    updateRuleGroup_rules,
    updateRuleGroup_description,
    updateRuleGroup_dryRun,
    updateRuleGroup_sourceMetadata,
    updateRuleGroup_encryptionConfiguration,
    updateRuleGroup_ruleGroupArn,
    updateRuleGroup_updateToken,
    updateRuleGroupResponse_httpStatus,
    updateRuleGroupResponse_updateToken,
    updateRuleGroupResponse_ruleGroupResponse,

    -- ** UpdateSubnetChangeProtection
    updateSubnetChangeProtection_updateToken,
    updateSubnetChangeProtection_firewallArn,
    updateSubnetChangeProtection_firewallName,
    updateSubnetChangeProtection_subnetChangeProtection,
    updateSubnetChangeProtectionResponse_updateToken,
    updateSubnetChangeProtectionResponse_subnetChangeProtection,
    updateSubnetChangeProtectionResponse_firewallArn,
    updateSubnetChangeProtectionResponse_firewallName,
    updateSubnetChangeProtectionResponse_httpStatus,

    -- * Types

    -- ** ActionDefinition
    actionDefinition_publishMetricAction,

    -- ** Address
    address_addressDefinition,

    -- ** Attachment
    attachment_endpointId,
    attachment_subnetId,
    attachment_status,

    -- ** CIDRSummary
    cIDRSummary_availableCIDRCount,
    cIDRSummary_utilizedCIDRCount,
    cIDRSummary_iPSetReferences,

    -- ** CapacityUsageSummary
    capacityUsageSummary_cIDRs,

    -- ** CustomAction
    customAction_actionName,
    customAction_actionDefinition,

    -- ** Dimension
    dimension_value,

    -- ** EncryptionConfiguration
    encryptionConfiguration_keyId,
    encryptionConfiguration_type,

    -- ** Firewall
    firewall_tags,
    firewall_deleteProtection,
    firewall_subnetChangeProtection,
    firewall_description,
    firewall_firewallPolicyChangeProtection,
    firewall_firewallArn,
    firewall_encryptionConfiguration,
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
    firewallPolicy_statelessCustomActions,
    firewallPolicy_statefulRuleGroupReferences,
    firewallPolicy_statelessRuleGroupReferences,
    firewallPolicy_statefulDefaultActions,
    firewallPolicy_statelessDefaultActions,
    firewallPolicy_statelessFragmentDefaultActions,

    -- ** FirewallPolicyMetadata
    firewallPolicyMetadata_name,
    firewallPolicyMetadata_arn,

    -- ** FirewallPolicyResponse
    firewallPolicyResponse_tags,
    firewallPolicyResponse_firewallPolicyStatus,
    firewallPolicyResponse_consumedStatefulRuleCapacity,
    firewallPolicyResponse_description,
    firewallPolicyResponse_lastModifiedTime,
    firewallPolicyResponse_encryptionConfiguration,
    firewallPolicyResponse_numberOfAssociations,
    firewallPolicyResponse_consumedStatelessRuleCapacity,
    firewallPolicyResponse_firewallPolicyName,
    firewallPolicyResponse_firewallPolicyArn,
    firewallPolicyResponse_firewallPolicyId,

    -- ** FirewallStatus
    firewallStatus_syncStates,
    firewallStatus_capacityUsageSummary,
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

    -- ** IPSetMetadata
    iPSetMetadata_resolvedCIDRCount,

    -- ** IPSetReference
    iPSetReference_referenceArn,

    -- ** LogDestinationConfig
    logDestinationConfig_logType,
    logDestinationConfig_logDestinationType,
    logDestinationConfig_logDestination,

    -- ** LoggingConfiguration
    loggingConfiguration_logDestinationConfigs,

    -- ** MatchAttributes
    matchAttributes_destinationPorts,
    matchAttributes_sources,
    matchAttributes_tCPFlags,
    matchAttributes_protocols,
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

    -- ** ReferenceSets
    referenceSets_iPSetReferences,

    -- ** RuleDefinition
    ruleDefinition_matchAttributes,
    ruleDefinition_actions,

    -- ** RuleGroup
    ruleGroup_statefulRuleOptions,
    ruleGroup_referenceSets,
    ruleGroup_ruleVariables,
    ruleGroup_rulesSource,

    -- ** RuleGroupMetadata
    ruleGroupMetadata_name,
    ruleGroupMetadata_arn,

    -- ** RuleGroupResponse
    ruleGroupResponse_tags,
    ruleGroupResponse_type,
    ruleGroupResponse_snsTopic,
    ruleGroupResponse_description,
    ruleGroupResponse_lastModifiedTime,
    ruleGroupResponse_sourceMetadata,
    ruleGroupResponse_consumedCapacity,
    ruleGroupResponse_encryptionConfiguration,
    ruleGroupResponse_numberOfAssociations,
    ruleGroupResponse_capacity,
    ruleGroupResponse_ruleGroupStatus,
    ruleGroupResponse_ruleGroupArn,
    ruleGroupResponse_ruleGroupName,
    ruleGroupResponse_ruleGroupId,

    -- ** RuleOption
    ruleOption_settings,
    ruleOption_keyword,

    -- ** RuleVariables
    ruleVariables_iPSets,
    ruleVariables_portSets,

    -- ** RulesSource
    rulesSource_rulesString,
    rulesSource_statefulRules,
    rulesSource_rulesSourceList,
    rulesSource_statelessRulesAndCustomActions,

    -- ** RulesSourceList
    rulesSourceList_targets,
    rulesSourceList_targetTypes,
    rulesSourceList_generatedRulesType,

    -- ** SourceMetadata
    sourceMetadata_sourceArn,
    sourceMetadata_sourceUpdateToken,

    -- ** StatefulEngineOptions
    statefulEngineOptions_ruleOrder,

    -- ** StatefulRule
    statefulRule_action,
    statefulRule_header,
    statefulRule_ruleOptions,

    -- ** StatefulRuleGroupOverride
    statefulRuleGroupOverride_action,

    -- ** StatefulRuleGroupReference
    statefulRuleGroupReference_override,
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
    syncState_attachment,
    syncState_config,

    -- ** TCPFlagField
    tCPFlagField_masks,
    tCPFlagField_flags,

    -- ** Tag
    tag_key,
    tag_value,
  )
where

import Amazonka.NetworkFirewall.AssociateFirewallPolicy
import Amazonka.NetworkFirewall.AssociateSubnets
import Amazonka.NetworkFirewall.CreateFirewall
import Amazonka.NetworkFirewall.CreateFirewallPolicy
import Amazonka.NetworkFirewall.CreateRuleGroup
import Amazonka.NetworkFirewall.DeleteFirewall
import Amazonka.NetworkFirewall.DeleteFirewallPolicy
import Amazonka.NetworkFirewall.DeleteResourcePolicy
import Amazonka.NetworkFirewall.DeleteRuleGroup
import Amazonka.NetworkFirewall.DescribeFirewall
import Amazonka.NetworkFirewall.DescribeFirewallPolicy
import Amazonka.NetworkFirewall.DescribeLoggingConfiguration
import Amazonka.NetworkFirewall.DescribeResourcePolicy
import Amazonka.NetworkFirewall.DescribeRuleGroup
import Amazonka.NetworkFirewall.DescribeRuleGroupMetadata
import Amazonka.NetworkFirewall.DisassociateSubnets
import Amazonka.NetworkFirewall.ListFirewallPolicies
import Amazonka.NetworkFirewall.ListFirewalls
import Amazonka.NetworkFirewall.ListRuleGroups
import Amazonka.NetworkFirewall.ListTagsForResource
import Amazonka.NetworkFirewall.PutResourcePolicy
import Amazonka.NetworkFirewall.TagResource
import Amazonka.NetworkFirewall.Types.ActionDefinition
import Amazonka.NetworkFirewall.Types.Address
import Amazonka.NetworkFirewall.Types.Attachment
import Amazonka.NetworkFirewall.Types.CIDRSummary
import Amazonka.NetworkFirewall.Types.CapacityUsageSummary
import Amazonka.NetworkFirewall.Types.CustomAction
import Amazonka.NetworkFirewall.Types.Dimension
import Amazonka.NetworkFirewall.Types.EncryptionConfiguration
import Amazonka.NetworkFirewall.Types.Firewall
import Amazonka.NetworkFirewall.Types.FirewallMetadata
import Amazonka.NetworkFirewall.Types.FirewallPolicy
import Amazonka.NetworkFirewall.Types.FirewallPolicyMetadata
import Amazonka.NetworkFirewall.Types.FirewallPolicyResponse
import Amazonka.NetworkFirewall.Types.FirewallStatus
import Amazonka.NetworkFirewall.Types.Header
import Amazonka.NetworkFirewall.Types.IPSet
import Amazonka.NetworkFirewall.Types.IPSetMetadata
import Amazonka.NetworkFirewall.Types.IPSetReference
import Amazonka.NetworkFirewall.Types.LogDestinationConfig
import Amazonka.NetworkFirewall.Types.LoggingConfiguration
import Amazonka.NetworkFirewall.Types.MatchAttributes
import Amazonka.NetworkFirewall.Types.PerObjectStatus
import Amazonka.NetworkFirewall.Types.PortRange
import Amazonka.NetworkFirewall.Types.PortSet
import Amazonka.NetworkFirewall.Types.PublishMetricAction
import Amazonka.NetworkFirewall.Types.ReferenceSets
import Amazonka.NetworkFirewall.Types.RuleDefinition
import Amazonka.NetworkFirewall.Types.RuleGroup
import Amazonka.NetworkFirewall.Types.RuleGroupMetadata
import Amazonka.NetworkFirewall.Types.RuleGroupResponse
import Amazonka.NetworkFirewall.Types.RuleOption
import Amazonka.NetworkFirewall.Types.RuleVariables
import Amazonka.NetworkFirewall.Types.RulesSource
import Amazonka.NetworkFirewall.Types.RulesSourceList
import Amazonka.NetworkFirewall.Types.SourceMetadata
import Amazonka.NetworkFirewall.Types.StatefulEngineOptions
import Amazonka.NetworkFirewall.Types.StatefulRule
import Amazonka.NetworkFirewall.Types.StatefulRuleGroupOverride
import Amazonka.NetworkFirewall.Types.StatefulRuleGroupReference
import Amazonka.NetworkFirewall.Types.StatefulRuleOptions
import Amazonka.NetworkFirewall.Types.StatelessRule
import Amazonka.NetworkFirewall.Types.StatelessRuleGroupReference
import Amazonka.NetworkFirewall.Types.StatelessRulesAndCustomActions
import Amazonka.NetworkFirewall.Types.SubnetMapping
import Amazonka.NetworkFirewall.Types.SyncState
import Amazonka.NetworkFirewall.Types.TCPFlagField
import Amazonka.NetworkFirewall.Types.Tag
import Amazonka.NetworkFirewall.UntagResource
import Amazonka.NetworkFirewall.UpdateFirewallDeleteProtection
import Amazonka.NetworkFirewall.UpdateFirewallDescription
import Amazonka.NetworkFirewall.UpdateFirewallEncryptionConfiguration
import Amazonka.NetworkFirewall.UpdateFirewallPolicy
import Amazonka.NetworkFirewall.UpdateFirewallPolicyChangeProtection
import Amazonka.NetworkFirewall.UpdateLoggingConfiguration
import Amazonka.NetworkFirewall.UpdateRuleGroup
import Amazonka.NetworkFirewall.UpdateSubnetChangeProtection
