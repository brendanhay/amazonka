{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.NetworkFirewall.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkFirewall.Lens
  ( -- * Operations

    -- ** AssociateFirewallPolicy
    associateFirewallPolicy_firewallArn,
    associateFirewallPolicy_firewallName,
    associateFirewallPolicy_updateToken,
    associateFirewallPolicy_firewallPolicyArn,
    associateFirewallPolicyResponse_firewallArn,
    associateFirewallPolicyResponse_firewallName,
    associateFirewallPolicyResponse_firewallPolicyArn,
    associateFirewallPolicyResponse_updateToken,
    associateFirewallPolicyResponse_httpStatus,

    -- ** AssociateSubnets
    associateSubnets_firewallArn,
    associateSubnets_firewallName,
    associateSubnets_updateToken,
    associateSubnets_subnetMappings,
    associateSubnetsResponse_firewallArn,
    associateSubnetsResponse_firewallName,
    associateSubnetsResponse_subnetMappings,
    associateSubnetsResponse_updateToken,
    associateSubnetsResponse_httpStatus,

    -- ** CreateFirewall
    createFirewall_deleteProtection,
    createFirewall_description,
    createFirewall_encryptionConfiguration,
    createFirewall_firewallPolicyChangeProtection,
    createFirewall_subnetChangeProtection,
    createFirewall_tags,
    createFirewall_firewallName,
    createFirewall_firewallPolicyArn,
    createFirewall_vpcId,
    createFirewall_subnetMappings,
    createFirewallResponse_firewall,
    createFirewallResponse_firewallStatus,
    createFirewallResponse_httpStatus,

    -- ** CreateFirewallPolicy
    createFirewallPolicy_description,
    createFirewallPolicy_dryRun,
    createFirewallPolicy_encryptionConfiguration,
    createFirewallPolicy_tags,
    createFirewallPolicy_firewallPolicyName,
    createFirewallPolicy_firewallPolicy,
    createFirewallPolicyResponse_httpStatus,
    createFirewallPolicyResponse_updateToken,
    createFirewallPolicyResponse_firewallPolicyResponse,

    -- ** CreateRuleGroup
    createRuleGroup_description,
    createRuleGroup_dryRun,
    createRuleGroup_encryptionConfiguration,
    createRuleGroup_ruleGroup,
    createRuleGroup_rules,
    createRuleGroup_sourceMetadata,
    createRuleGroup_tags,
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
    deleteFirewallPolicy_firewallPolicyArn,
    deleteFirewallPolicy_firewallPolicyName,
    deleteFirewallPolicyResponse_httpStatus,
    deleteFirewallPolicyResponse_firewallPolicyResponse,

    -- ** DeleteResourcePolicy
    deleteResourcePolicy_resourceArn,
    deleteResourcePolicyResponse_httpStatus,

    -- ** DeleteRuleGroup
    deleteRuleGroup_ruleGroupArn,
    deleteRuleGroup_ruleGroupName,
    deleteRuleGroup_type,
    deleteRuleGroupResponse_httpStatus,
    deleteRuleGroupResponse_ruleGroupResponse,

    -- ** DescribeFirewall
    describeFirewall_firewallArn,
    describeFirewall_firewallName,
    describeFirewallResponse_firewall,
    describeFirewallResponse_firewallStatus,
    describeFirewallResponse_updateToken,
    describeFirewallResponse_httpStatus,

    -- ** DescribeFirewallPolicy
    describeFirewallPolicy_firewallPolicyArn,
    describeFirewallPolicy_firewallPolicyName,
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
    describeRuleGroup_ruleGroupArn,
    describeRuleGroup_ruleGroupName,
    describeRuleGroup_type,
    describeRuleGroupResponse_ruleGroup,
    describeRuleGroupResponse_httpStatus,
    describeRuleGroupResponse_updateToken,
    describeRuleGroupResponse_ruleGroupResponse,

    -- ** DescribeRuleGroupMetadata
    describeRuleGroupMetadata_ruleGroupArn,
    describeRuleGroupMetadata_ruleGroupName,
    describeRuleGroupMetadata_type,
    describeRuleGroupMetadataResponse_capacity,
    describeRuleGroupMetadataResponse_description,
    describeRuleGroupMetadataResponse_lastModifiedTime,
    describeRuleGroupMetadataResponse_statefulRuleOptions,
    describeRuleGroupMetadataResponse_type,
    describeRuleGroupMetadataResponse_httpStatus,
    describeRuleGroupMetadataResponse_ruleGroupArn,
    describeRuleGroupMetadataResponse_ruleGroupName,

    -- ** DisassociateSubnets
    disassociateSubnets_firewallArn,
    disassociateSubnets_firewallName,
    disassociateSubnets_updateToken,
    disassociateSubnets_subnetIds,
    disassociateSubnetsResponse_firewallArn,
    disassociateSubnetsResponse_firewallName,
    disassociateSubnetsResponse_subnetMappings,
    disassociateSubnetsResponse_updateToken,
    disassociateSubnetsResponse_httpStatus,

    -- ** ListFirewallPolicies
    listFirewallPolicies_maxResults,
    listFirewallPolicies_nextToken,
    listFirewallPoliciesResponse_firewallPolicies,
    listFirewallPoliciesResponse_nextToken,
    listFirewallPoliciesResponse_httpStatus,

    -- ** ListFirewalls
    listFirewalls_maxResults,
    listFirewalls_nextToken,
    listFirewalls_vpcIds,
    listFirewallsResponse_firewalls,
    listFirewallsResponse_nextToken,
    listFirewallsResponse_httpStatus,

    -- ** ListRuleGroups
    listRuleGroups_managedType,
    listRuleGroups_maxResults,
    listRuleGroups_nextToken,
    listRuleGroups_scope,
    listRuleGroups_type,
    listRuleGroupsResponse_nextToken,
    listRuleGroupsResponse_ruleGroups,
    listRuleGroupsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_maxResults,
    listTagsForResource_nextToken,
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_nextToken,
    listTagsForResourceResponse_tags,
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
    updateFirewallDeleteProtection_firewallArn,
    updateFirewallDeleteProtection_firewallName,
    updateFirewallDeleteProtection_updateToken,
    updateFirewallDeleteProtection_deleteProtection,
    updateFirewallDeleteProtectionResponse_deleteProtection,
    updateFirewallDeleteProtectionResponse_firewallArn,
    updateFirewallDeleteProtectionResponse_firewallName,
    updateFirewallDeleteProtectionResponse_updateToken,
    updateFirewallDeleteProtectionResponse_httpStatus,

    -- ** UpdateFirewallDescription
    updateFirewallDescription_description,
    updateFirewallDescription_firewallArn,
    updateFirewallDescription_firewallName,
    updateFirewallDescription_updateToken,
    updateFirewallDescriptionResponse_description,
    updateFirewallDescriptionResponse_firewallArn,
    updateFirewallDescriptionResponse_firewallName,
    updateFirewallDescriptionResponse_updateToken,
    updateFirewallDescriptionResponse_httpStatus,

    -- ** UpdateFirewallEncryptionConfiguration
    updateFirewallEncryptionConfiguration_encryptionConfiguration,
    updateFirewallEncryptionConfiguration_firewallArn,
    updateFirewallEncryptionConfiguration_firewallName,
    updateFirewallEncryptionConfiguration_updateToken,
    updateFirewallEncryptionConfigurationResponse_encryptionConfiguration,
    updateFirewallEncryptionConfigurationResponse_firewallArn,
    updateFirewallEncryptionConfigurationResponse_firewallName,
    updateFirewallEncryptionConfigurationResponse_updateToken,
    updateFirewallEncryptionConfigurationResponse_httpStatus,

    -- ** UpdateFirewallPolicy
    updateFirewallPolicy_description,
    updateFirewallPolicy_dryRun,
    updateFirewallPolicy_encryptionConfiguration,
    updateFirewallPolicy_firewallPolicyArn,
    updateFirewallPolicy_firewallPolicyName,
    updateFirewallPolicy_updateToken,
    updateFirewallPolicy_firewallPolicy,
    updateFirewallPolicyResponse_httpStatus,
    updateFirewallPolicyResponse_updateToken,
    updateFirewallPolicyResponse_firewallPolicyResponse,

    -- ** UpdateFirewallPolicyChangeProtection
    updateFirewallPolicyChangeProtection_firewallArn,
    updateFirewallPolicyChangeProtection_firewallName,
    updateFirewallPolicyChangeProtection_updateToken,
    updateFirewallPolicyChangeProtection_firewallPolicyChangeProtection,
    updateFirewallPolicyChangeProtectionResponse_firewallArn,
    updateFirewallPolicyChangeProtectionResponse_firewallName,
    updateFirewallPolicyChangeProtectionResponse_firewallPolicyChangeProtection,
    updateFirewallPolicyChangeProtectionResponse_updateToken,
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
    updateRuleGroup_description,
    updateRuleGroup_dryRun,
    updateRuleGroup_encryptionConfiguration,
    updateRuleGroup_ruleGroup,
    updateRuleGroup_ruleGroupArn,
    updateRuleGroup_ruleGroupName,
    updateRuleGroup_rules,
    updateRuleGroup_sourceMetadata,
    updateRuleGroup_type,
    updateRuleGroup_updateToken,
    updateRuleGroupResponse_httpStatus,
    updateRuleGroupResponse_updateToken,
    updateRuleGroupResponse_ruleGroupResponse,

    -- ** UpdateSubnetChangeProtection
    updateSubnetChangeProtection_firewallArn,
    updateSubnetChangeProtection_firewallName,
    updateSubnetChangeProtection_updateToken,
    updateSubnetChangeProtection_subnetChangeProtection,
    updateSubnetChangeProtectionResponse_firewallArn,
    updateSubnetChangeProtectionResponse_firewallName,
    updateSubnetChangeProtectionResponse_subnetChangeProtection,
    updateSubnetChangeProtectionResponse_updateToken,
    updateSubnetChangeProtectionResponse_httpStatus,

    -- * Types

    -- ** ActionDefinition
    actionDefinition_publishMetricAction,

    -- ** Address
    address_addressDefinition,

    -- ** Attachment
    attachment_endpointId,
    attachment_status,
    attachment_statusMessage,
    attachment_subnetId,

    -- ** CIDRSummary
    cIDRSummary_availableCIDRCount,
    cIDRSummary_iPSetReferences,
    cIDRSummary_utilizedCIDRCount,

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
    firewall_deleteProtection,
    firewall_description,
    firewall_encryptionConfiguration,
    firewall_firewallArn,
    firewall_firewallName,
    firewall_firewallPolicyChangeProtection,
    firewall_subnetChangeProtection,
    firewall_tags,
    firewall_firewallPolicyArn,
    firewall_vpcId,
    firewall_subnetMappings,
    firewall_firewallId,

    -- ** FirewallMetadata
    firewallMetadata_firewallArn,
    firewallMetadata_firewallName,

    -- ** FirewallPolicy
    firewallPolicy_statefulDefaultActions,
    firewallPolicy_statefulEngineOptions,
    firewallPolicy_statefulRuleGroupReferences,
    firewallPolicy_statelessCustomActions,
    firewallPolicy_statelessRuleGroupReferences,
    firewallPolicy_statelessDefaultActions,
    firewallPolicy_statelessFragmentDefaultActions,

    -- ** FirewallPolicyMetadata
    firewallPolicyMetadata_arn,
    firewallPolicyMetadata_name,

    -- ** FirewallPolicyResponse
    firewallPolicyResponse_consumedStatefulRuleCapacity,
    firewallPolicyResponse_consumedStatelessRuleCapacity,
    firewallPolicyResponse_description,
    firewallPolicyResponse_encryptionConfiguration,
    firewallPolicyResponse_firewallPolicyStatus,
    firewallPolicyResponse_lastModifiedTime,
    firewallPolicyResponse_numberOfAssociations,
    firewallPolicyResponse_tags,
    firewallPolicyResponse_firewallPolicyName,
    firewallPolicyResponse_firewallPolicyArn,
    firewallPolicyResponse_firewallPolicyId,

    -- ** FirewallStatus
    firewallStatus_capacityUsageSummary,
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
    matchAttributes_destinations,
    matchAttributes_protocols,
    matchAttributes_sourcePorts,
    matchAttributes_sources,
    matchAttributes_tCPFlags,

    -- ** PerObjectStatus
    perObjectStatus_syncStatus,
    perObjectStatus_updateToken,

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
    ruleGroup_referenceSets,
    ruleGroup_ruleVariables,
    ruleGroup_statefulRuleOptions,
    ruleGroup_rulesSource,

    -- ** RuleGroupMetadata
    ruleGroupMetadata_arn,
    ruleGroupMetadata_name,

    -- ** RuleGroupResponse
    ruleGroupResponse_capacity,
    ruleGroupResponse_consumedCapacity,
    ruleGroupResponse_description,
    ruleGroupResponse_encryptionConfiguration,
    ruleGroupResponse_lastModifiedTime,
    ruleGroupResponse_numberOfAssociations,
    ruleGroupResponse_ruleGroupStatus,
    ruleGroupResponse_snsTopic,
    ruleGroupResponse_sourceMetadata,
    ruleGroupResponse_tags,
    ruleGroupResponse_type,
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
    rulesSource_rulesSourceList,
    rulesSource_rulesString,
    rulesSource_statefulRules,
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
    statefulEngineOptions_streamExceptionPolicy,

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
