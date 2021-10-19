{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Lens
  ( -- * Operations

    -- ** ListPolicies
    listPolicies_nextToken,
    listPolicies_maxResults,
    listPoliciesResponse_nextToken,
    listPoliciesResponse_policyList,
    listPoliciesResponse_httpStatus,

    -- ** GetComplianceDetail
    getComplianceDetail_policyId,
    getComplianceDetail_memberAccount,
    getComplianceDetailResponse_policyComplianceDetail,
    getComplianceDetailResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tagList,
    listTagsForResourceResponse_httpStatus,

    -- ** GetNotificationChannel
    getNotificationChannelResponse_snsTopicArn,
    getNotificationChannelResponse_snsRoleName,
    getNotificationChannelResponse_httpStatus,

    -- ** GetAdminAccount
    getAdminAccountResponse_adminAccount,
    getAdminAccountResponse_roleStatus,
    getAdminAccountResponse_httpStatus,

    -- ** ListComplianceStatus
    listComplianceStatus_nextToken,
    listComplianceStatus_maxResults,
    listComplianceStatus_policyId,
    listComplianceStatusResponse_nextToken,
    listComplianceStatusResponse_policyComplianceStatusList,
    listComplianceStatusResponse_httpStatus,

    -- ** GetAppsList
    getAppsList_defaultList,
    getAppsList_listId,
    getAppsListResponse_appsListArn,
    getAppsListResponse_appsList,
    getAppsListResponse_httpStatus,

    -- ** PutPolicy
    putPolicy_tagList,
    putPolicy_policy,
    putPolicyResponse_policyArn,
    putPolicyResponse_policy,
    putPolicyResponse_httpStatus,

    -- ** DeletePolicy
    deletePolicy_deleteAllPolicyResources,
    deletePolicy_policyId,

    -- ** DisassociateAdminAccount

    -- ** PutNotificationChannel
    putNotificationChannel_snsTopicArn,
    putNotificationChannel_snsRoleName,

    -- ** DeleteNotificationChannel

    -- ** AssociateAdminAccount
    associateAdminAccount_adminAccount,

    -- ** GetViolationDetails
    getViolationDetails_policyId,
    getViolationDetails_memberAccount,
    getViolationDetails_resourceId,
    getViolationDetails_resourceType,
    getViolationDetailsResponse_violationDetail,
    getViolationDetailsResponse_httpStatus,

    -- ** ListMemberAccounts
    listMemberAccounts_nextToken,
    listMemberAccounts_maxResults,
    listMemberAccountsResponse_nextToken,
    listMemberAccountsResponse_memberAccounts,
    listMemberAccountsResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tagList,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** DeleteProtocolsList
    deleteProtocolsList_listId,

    -- ** GetPolicy
    getPolicy_policyId,
    getPolicyResponse_policyArn,
    getPolicyResponse_policy,
    getPolicyResponse_httpStatus,

    -- ** ListProtocolsLists
    listProtocolsLists_defaultLists,
    listProtocolsLists_nextToken,
    listProtocolsLists_maxResults,
    listProtocolsListsResponse_protocolsLists,
    listProtocolsListsResponse_nextToken,
    listProtocolsListsResponse_httpStatus,

    -- ** PutProtocolsList
    putProtocolsList_tagList,
    putProtocolsList_protocolsList,
    putProtocolsListResponse_protocolsList,
    putProtocolsListResponse_protocolsListArn,
    putProtocolsListResponse_httpStatus,

    -- ** PutAppsList
    putAppsList_tagList,
    putAppsList_appsList,
    putAppsListResponse_appsListArn,
    putAppsListResponse_appsList,
    putAppsListResponse_httpStatus,

    -- ** DeleteAppsList
    deleteAppsList_listId,

    -- ** ListAppsLists
    listAppsLists_defaultLists,
    listAppsLists_nextToken,
    listAppsLists_maxResults,
    listAppsListsResponse_nextToken,
    listAppsListsResponse_appsLists,
    listAppsListsResponse_httpStatus,

    -- ** GetProtocolsList
    getProtocolsList_defaultList,
    getProtocolsList_listId,
    getProtocolsListResponse_protocolsList,
    getProtocolsListResponse_protocolsListArn,
    getProtocolsListResponse_httpStatus,

    -- ** GetProtectionStatus
    getProtectionStatus_memberAccountId,
    getProtectionStatus_startTime,
    getProtectionStatus_nextToken,
    getProtectionStatus_endTime,
    getProtectionStatus_maxResults,
    getProtectionStatus_policyId,
    getProtectionStatusResponse_data,
    getProtectionStatusResponse_adminAccountId,
    getProtectionStatusResponse_nextToken,
    getProtectionStatusResponse_serviceType,
    getProtectionStatusResponse_httpStatus,

    -- * Types

    -- ** ActionTarget
    actionTarget_resourceId,
    actionTarget_description,

    -- ** App
    app_appName,
    app_protocol,
    app_port,

    -- ** AppsListData
    appsListData_listUpdateToken,
    appsListData_listId,
    appsListData_lastUpdateTime,
    appsListData_previousAppsList,
    appsListData_createTime,
    appsListData_listName,
    appsListData_appsList,

    -- ** AppsListDataSummary
    appsListDataSummary_listArn,
    appsListDataSummary_appsList,
    appsListDataSummary_listId,
    appsListDataSummary_listName,

    -- ** AwsEc2InstanceViolation
    awsEc2InstanceViolation_violationTarget,
    awsEc2InstanceViolation_awsEc2NetworkInterfaceViolations,

    -- ** AwsEc2NetworkInterfaceViolation
    awsEc2NetworkInterfaceViolation_violatingSecurityGroups,
    awsEc2NetworkInterfaceViolation_violationTarget,

    -- ** AwsVPCSecurityGroupViolation
    awsVPCSecurityGroupViolation_violationTargetDescription,
    awsVPCSecurityGroupViolation_possibleSecurityGroupRemediationActions,
    awsVPCSecurityGroupViolation_violationTarget,
    awsVPCSecurityGroupViolation_partialMatches,

    -- ** ComplianceViolator
    complianceViolator_resourceId,
    complianceViolator_resourceType,
    complianceViolator_violationReason,

    -- ** DnsDuplicateRuleGroupViolation
    dnsDuplicateRuleGroupViolation_violationTargetDescription,
    dnsDuplicateRuleGroupViolation_violationTarget,

    -- ** DnsRuleGroupLimitExceededViolation
    dnsRuleGroupLimitExceededViolation_violationTargetDescription,
    dnsRuleGroupLimitExceededViolation_violationTarget,
    dnsRuleGroupLimitExceededViolation_numberOfRuleGroupsAlreadyAssociated,

    -- ** DnsRuleGroupPriorityConflictViolation
    dnsRuleGroupPriorityConflictViolation_conflictingPriority,
    dnsRuleGroupPriorityConflictViolation_conflictingPolicyId,
    dnsRuleGroupPriorityConflictViolation_violationTargetDescription,
    dnsRuleGroupPriorityConflictViolation_violationTarget,
    dnsRuleGroupPriorityConflictViolation_unavailablePriorities,

    -- ** EC2AssociateRouteTableAction
    eC2AssociateRouteTableAction_subnetId,
    eC2AssociateRouteTableAction_gatewayId,
    eC2AssociateRouteTableAction_description,
    eC2AssociateRouteTableAction_routeTableId,

    -- ** EC2CopyRouteTableAction
    eC2CopyRouteTableAction_description,
    eC2CopyRouteTableAction_vpcId,
    eC2CopyRouteTableAction_routeTableId,

    -- ** EC2CreateRouteAction
    eC2CreateRouteAction_destinationIpv6CidrBlock,
    eC2CreateRouteAction_gatewayId,
    eC2CreateRouteAction_vpcEndpointId,
    eC2CreateRouteAction_destinationPrefixListId,
    eC2CreateRouteAction_description,
    eC2CreateRouteAction_destinationCidrBlock,
    eC2CreateRouteAction_routeTableId,

    -- ** EC2CreateRouteTableAction
    eC2CreateRouteTableAction_description,
    eC2CreateRouteTableAction_vpcId,

    -- ** EC2DeleteRouteAction
    eC2DeleteRouteAction_destinationIpv6CidrBlock,
    eC2DeleteRouteAction_destinationPrefixListId,
    eC2DeleteRouteAction_description,
    eC2DeleteRouteAction_destinationCidrBlock,
    eC2DeleteRouteAction_routeTableId,

    -- ** EC2ReplaceRouteAction
    eC2ReplaceRouteAction_destinationIpv6CidrBlock,
    eC2ReplaceRouteAction_gatewayId,
    eC2ReplaceRouteAction_destinationPrefixListId,
    eC2ReplaceRouteAction_description,
    eC2ReplaceRouteAction_destinationCidrBlock,
    eC2ReplaceRouteAction_routeTableId,

    -- ** EC2ReplaceRouteTableAssociationAction
    eC2ReplaceRouteTableAssociationAction_description,
    eC2ReplaceRouteTableAssociationAction_associationId,
    eC2ReplaceRouteTableAssociationAction_routeTableId,

    -- ** EvaluationResult
    evaluationResult_violatorCount,
    evaluationResult_complianceStatus,
    evaluationResult_evaluationLimitExceeded,

    -- ** ExpectedRoute
    expectedRoute_ipV4Cidr,
    expectedRoute_routeTableId,
    expectedRoute_allowedTargets,
    expectedRoute_prefixListId,
    expectedRoute_ipV6Cidr,
    expectedRoute_contributingSubnets,

    -- ** NetworkFirewallBlackHoleRouteDetectedViolation
    networkFirewallBlackHoleRouteDetectedViolation_routeTableId,
    networkFirewallBlackHoleRouteDetectedViolation_vpcId,
    networkFirewallBlackHoleRouteDetectedViolation_violatingRoutes,
    networkFirewallBlackHoleRouteDetectedViolation_violationTarget,

    -- ** NetworkFirewallInternetTrafficNotInspectedViolation
    networkFirewallInternetTrafficNotInspectedViolation_actualInternetGatewayRoutes,
    networkFirewallInternetTrafficNotInspectedViolation_routeTableId,
    networkFirewallInternetTrafficNotInspectedViolation_vpcId,
    networkFirewallInternetTrafficNotInspectedViolation_violatingRoutes,
    networkFirewallInternetTrafficNotInspectedViolation_subnetId,
    networkFirewallInternetTrafficNotInspectedViolation_expectedFirewallSubnetRoutes,
    networkFirewallInternetTrafficNotInspectedViolation_subnetAvailabilityZone,
    networkFirewallInternetTrafficNotInspectedViolation_expectedInternetGatewayRoutes,
    networkFirewallInternetTrafficNotInspectedViolation_currentInternetGatewayRouteTable,
    networkFirewallInternetTrafficNotInspectedViolation_firewallSubnetId,
    networkFirewallInternetTrafficNotInspectedViolation_currentFirewallSubnetRouteTable,
    networkFirewallInternetTrafficNotInspectedViolation_expectedFirewallEndpoint,
    networkFirewallInternetTrafficNotInspectedViolation_isRouteTableUsedInDifferentAZ,
    networkFirewallInternetTrafficNotInspectedViolation_internetGatewayId,
    networkFirewallInternetTrafficNotInspectedViolation_actualFirewallSubnetRoutes,

    -- ** NetworkFirewallInvalidRouteConfigurationViolation
    networkFirewallInvalidRouteConfigurationViolation_actualInternetGatewayRoutes,
    networkFirewallInvalidRouteConfigurationViolation_routeTableId,
    networkFirewallInvalidRouteConfigurationViolation_affectedSubnets,
    networkFirewallInvalidRouteConfigurationViolation_vpcId,
    networkFirewallInvalidRouteConfigurationViolation_actualFirewallEndpoint,
    networkFirewallInvalidRouteConfigurationViolation_expectedFirewallSubnetId,
    networkFirewallInvalidRouteConfigurationViolation_expectedFirewallSubnetRoutes,
    networkFirewallInvalidRouteConfigurationViolation_expectedInternetGatewayRoutes,
    networkFirewallInvalidRouteConfigurationViolation_currentInternetGatewayRouteTable,
    networkFirewallInvalidRouteConfigurationViolation_violatingRoute,
    networkFirewallInvalidRouteConfigurationViolation_currentFirewallSubnetRouteTable,
    networkFirewallInvalidRouteConfigurationViolation_expectedFirewallEndpoint,
    networkFirewallInvalidRouteConfigurationViolation_isRouteTableUsedInDifferentAZ,
    networkFirewallInvalidRouteConfigurationViolation_actualFirewallSubnetId,
    networkFirewallInvalidRouteConfigurationViolation_internetGatewayId,
    networkFirewallInvalidRouteConfigurationViolation_actualFirewallSubnetRoutes,

    -- ** NetworkFirewallMissingExpectedRTViolation
    networkFirewallMissingExpectedRTViolation_currentRouteTable,
    networkFirewallMissingExpectedRTViolation_availabilityZone,
    networkFirewallMissingExpectedRTViolation_vpc,
    networkFirewallMissingExpectedRTViolation_violationTarget,
    networkFirewallMissingExpectedRTViolation_expectedRouteTable,

    -- ** NetworkFirewallMissingExpectedRoutesViolation
    networkFirewallMissingExpectedRoutesViolation_expectedRoutes,
    networkFirewallMissingExpectedRoutesViolation_vpcId,
    networkFirewallMissingExpectedRoutesViolation_violationTarget,

    -- ** NetworkFirewallMissingFirewallViolation
    networkFirewallMissingFirewallViolation_targetViolationReason,
    networkFirewallMissingFirewallViolation_availabilityZone,
    networkFirewallMissingFirewallViolation_vpc,
    networkFirewallMissingFirewallViolation_violationTarget,

    -- ** NetworkFirewallMissingSubnetViolation
    networkFirewallMissingSubnetViolation_targetViolationReason,
    networkFirewallMissingSubnetViolation_availabilityZone,
    networkFirewallMissingSubnetViolation_vpc,
    networkFirewallMissingSubnetViolation_violationTarget,

    -- ** NetworkFirewallPolicyDescription
    networkFirewallPolicyDescription_statefulRuleGroups,
    networkFirewallPolicyDescription_statelessRuleGroups,
    networkFirewallPolicyDescription_statelessFragmentDefaultActions,
    networkFirewallPolicyDescription_statelessCustomActions,
    networkFirewallPolicyDescription_statelessDefaultActions,

    -- ** NetworkFirewallPolicyModifiedViolation
    networkFirewallPolicyModifiedViolation_currentPolicyDescription,
    networkFirewallPolicyModifiedViolation_violationTarget,
    networkFirewallPolicyModifiedViolation_expectedPolicyDescription,

    -- ** NetworkFirewallUnexpectedFirewallRoutesViolation
    networkFirewallUnexpectedFirewallRoutesViolation_routeTableId,
    networkFirewallUnexpectedFirewallRoutesViolation_vpcId,
    networkFirewallUnexpectedFirewallRoutesViolation_violatingRoutes,
    networkFirewallUnexpectedFirewallRoutesViolation_firewallEndpoint,
    networkFirewallUnexpectedFirewallRoutesViolation_firewallSubnetId,

    -- ** NetworkFirewallUnexpectedGatewayRoutesViolation
    networkFirewallUnexpectedGatewayRoutesViolation_routeTableId,
    networkFirewallUnexpectedGatewayRoutesViolation_vpcId,
    networkFirewallUnexpectedGatewayRoutesViolation_violatingRoutes,
    networkFirewallUnexpectedGatewayRoutesViolation_gatewayId,

    -- ** PartialMatch
    partialMatch_targetViolationReasons,
    partialMatch_reference,

    -- ** Policy
    policy_policyId,
    policy_resourceTypeList,
    policy_resourceTags,
    policy_policyUpdateToken,
    policy_deleteUnusedFMManagedResources,
    policy_excludeMap,
    policy_includeMap,
    policy_policyName,
    policy_securityServicePolicyData,
    policy_resourceType,
    policy_excludeResourceTags,
    policy_remediationEnabled,

    -- ** PolicyComplianceDetail
    policyComplianceDetail_expiredAt,
    policyComplianceDetail_policyId,
    policyComplianceDetail_violators,
    policyComplianceDetail_evaluationLimitExceeded,
    policyComplianceDetail_issueInfoMap,
    policyComplianceDetail_policyOwner,
    policyComplianceDetail_memberAccount,

    -- ** PolicyComplianceStatus
    policyComplianceStatus_evaluationResults,
    policyComplianceStatus_lastUpdated,
    policyComplianceStatus_policyName,
    policyComplianceStatus_policyId,
    policyComplianceStatus_issueInfoMap,
    policyComplianceStatus_policyOwner,
    policyComplianceStatus_memberAccount,

    -- ** PolicySummary
    policySummary_policyName,
    policySummary_remediationEnabled,
    policySummary_resourceType,
    policySummary_policyId,
    policySummary_deleteUnusedFMManagedResources,
    policySummary_policyArn,
    policySummary_securityServiceType,

    -- ** PossibleRemediationAction
    possibleRemediationAction_isDefaultAction,
    possibleRemediationAction_description,
    possibleRemediationAction_orderedRemediationActions,

    -- ** PossibleRemediationActions
    possibleRemediationActions_actions,
    possibleRemediationActions_description,

    -- ** ProtocolsListData
    protocolsListData_listUpdateToken,
    protocolsListData_listId,
    protocolsListData_lastUpdateTime,
    protocolsListData_previousProtocolsList,
    protocolsListData_createTime,
    protocolsListData_listName,
    protocolsListData_protocolsList,

    -- ** ProtocolsListDataSummary
    protocolsListDataSummary_protocolsList,
    protocolsListDataSummary_listArn,
    protocolsListDataSummary_listId,
    protocolsListDataSummary_listName,

    -- ** RemediationAction
    remediationAction_eC2CreateRouteAction,
    remediationAction_eC2CopyRouteTableAction,
    remediationAction_eC2ReplaceRouteTableAssociationAction,
    remediationAction_eC2AssociateRouteTableAction,
    remediationAction_eC2ReplaceRouteAction,
    remediationAction_eC2DeleteRouteAction,
    remediationAction_description,
    remediationAction_eC2CreateRouteTableAction,

    -- ** RemediationActionWithOrder
    remediationActionWithOrder_remediationAction,
    remediationActionWithOrder_order,

    -- ** ResourceTag
    resourceTag_value,
    resourceTag_key,

    -- ** ResourceViolation
    resourceViolation_possibleRemediationActions,
    resourceViolation_networkFirewallBlackHoleRouteDetectedViolation,
    resourceViolation_dnsRuleGroupLimitExceededViolation,
    resourceViolation_networkFirewallMissingExpectedRTViolation,
    resourceViolation_networkFirewallInternetTrafficNotInspectedViolation,
    resourceViolation_networkFirewallMissingFirewallViolation,
    resourceViolation_networkFirewallMissingSubnetViolation,
    resourceViolation_awsEc2InstanceViolation,
    resourceViolation_networkFirewallMissingExpectedRoutesViolation,
    resourceViolation_dnsRuleGroupPriorityConflictViolation,
    resourceViolation_awsVPCSecurityGroupViolation,
    resourceViolation_networkFirewallPolicyModifiedViolation,
    resourceViolation_networkFirewallUnexpectedFirewallRoutesViolation,
    resourceViolation_awsEc2NetworkInterfaceViolation,
    resourceViolation_networkFirewallUnexpectedGatewayRoutesViolation,
    resourceViolation_dnsDuplicateRuleGroupViolation,
    resourceViolation_networkFirewallInvalidRouteConfigurationViolation,

    -- ** Route
    route_destination,
    route_targetType,
    route_destinationType,
    route_target,

    -- ** SecurityGroupRemediationAction
    securityGroupRemediationAction_isDefaultAction,
    securityGroupRemediationAction_remediationResult,
    securityGroupRemediationAction_description,
    securityGroupRemediationAction_remediationActionType,

    -- ** SecurityGroupRuleDescription
    securityGroupRuleDescription_fromPort,
    securityGroupRuleDescription_protocol,
    securityGroupRuleDescription_iPV4Range,
    securityGroupRuleDescription_prefixListId,
    securityGroupRuleDescription_toPort,
    securityGroupRuleDescription_iPV6Range,

    -- ** SecurityServicePolicyData
    securityServicePolicyData_managedServiceData,
    securityServicePolicyData_type,

    -- ** StatefulRuleGroup
    statefulRuleGroup_resourceId,
    statefulRuleGroup_ruleGroupName,

    -- ** StatelessRuleGroup
    statelessRuleGroup_resourceId,
    statelessRuleGroup_priority,
    statelessRuleGroup_ruleGroupName,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** ViolationDetail
    violationDetail_resourceTags,
    violationDetail_resourceDescription,
    violationDetail_policyId,
    violationDetail_memberAccount,
    violationDetail_resourceId,
    violationDetail_resourceType,
    violationDetail_resourceViolations,
  )
where

import Network.AWS.FMS.AssociateAdminAccount
import Network.AWS.FMS.DeleteAppsList
import Network.AWS.FMS.DeleteNotificationChannel
import Network.AWS.FMS.DeletePolicy
import Network.AWS.FMS.DeleteProtocolsList
import Network.AWS.FMS.DisassociateAdminAccount
import Network.AWS.FMS.GetAdminAccount
import Network.AWS.FMS.GetAppsList
import Network.AWS.FMS.GetComplianceDetail
import Network.AWS.FMS.GetNotificationChannel
import Network.AWS.FMS.GetPolicy
import Network.AWS.FMS.GetProtectionStatus
import Network.AWS.FMS.GetProtocolsList
import Network.AWS.FMS.GetViolationDetails
import Network.AWS.FMS.ListAppsLists
import Network.AWS.FMS.ListComplianceStatus
import Network.AWS.FMS.ListMemberAccounts
import Network.AWS.FMS.ListPolicies
import Network.AWS.FMS.ListProtocolsLists
import Network.AWS.FMS.ListTagsForResource
import Network.AWS.FMS.PutAppsList
import Network.AWS.FMS.PutNotificationChannel
import Network.AWS.FMS.PutPolicy
import Network.AWS.FMS.PutProtocolsList
import Network.AWS.FMS.TagResource
import Network.AWS.FMS.Types.ActionTarget
import Network.AWS.FMS.Types.App
import Network.AWS.FMS.Types.AppsListData
import Network.AWS.FMS.Types.AppsListDataSummary
import Network.AWS.FMS.Types.AwsEc2InstanceViolation
import Network.AWS.FMS.Types.AwsEc2NetworkInterfaceViolation
import Network.AWS.FMS.Types.AwsVPCSecurityGroupViolation
import Network.AWS.FMS.Types.ComplianceViolator
import Network.AWS.FMS.Types.DnsDuplicateRuleGroupViolation
import Network.AWS.FMS.Types.DnsRuleGroupLimitExceededViolation
import Network.AWS.FMS.Types.DnsRuleGroupPriorityConflictViolation
import Network.AWS.FMS.Types.EC2AssociateRouteTableAction
import Network.AWS.FMS.Types.EC2CopyRouteTableAction
import Network.AWS.FMS.Types.EC2CreateRouteAction
import Network.AWS.FMS.Types.EC2CreateRouteTableAction
import Network.AWS.FMS.Types.EC2DeleteRouteAction
import Network.AWS.FMS.Types.EC2ReplaceRouteAction
import Network.AWS.FMS.Types.EC2ReplaceRouteTableAssociationAction
import Network.AWS.FMS.Types.EvaluationResult
import Network.AWS.FMS.Types.ExpectedRoute
import Network.AWS.FMS.Types.NetworkFirewallBlackHoleRouteDetectedViolation
import Network.AWS.FMS.Types.NetworkFirewallInternetTrafficNotInspectedViolation
import Network.AWS.FMS.Types.NetworkFirewallInvalidRouteConfigurationViolation
import Network.AWS.FMS.Types.NetworkFirewallMissingExpectedRTViolation
import Network.AWS.FMS.Types.NetworkFirewallMissingExpectedRoutesViolation
import Network.AWS.FMS.Types.NetworkFirewallMissingFirewallViolation
import Network.AWS.FMS.Types.NetworkFirewallMissingSubnetViolation
import Network.AWS.FMS.Types.NetworkFirewallPolicyDescription
import Network.AWS.FMS.Types.NetworkFirewallPolicyModifiedViolation
import Network.AWS.FMS.Types.NetworkFirewallUnexpectedFirewallRoutesViolation
import Network.AWS.FMS.Types.NetworkFirewallUnexpectedGatewayRoutesViolation
import Network.AWS.FMS.Types.PartialMatch
import Network.AWS.FMS.Types.Policy
import Network.AWS.FMS.Types.PolicyComplianceDetail
import Network.AWS.FMS.Types.PolicyComplianceStatus
import Network.AWS.FMS.Types.PolicySummary
import Network.AWS.FMS.Types.PossibleRemediationAction
import Network.AWS.FMS.Types.PossibleRemediationActions
import Network.AWS.FMS.Types.ProtocolsListData
import Network.AWS.FMS.Types.ProtocolsListDataSummary
import Network.AWS.FMS.Types.RemediationAction
import Network.AWS.FMS.Types.RemediationActionWithOrder
import Network.AWS.FMS.Types.ResourceTag
import Network.AWS.FMS.Types.ResourceViolation
import Network.AWS.FMS.Types.Route
import Network.AWS.FMS.Types.SecurityGroupRemediationAction
import Network.AWS.FMS.Types.SecurityGroupRuleDescription
import Network.AWS.FMS.Types.SecurityServicePolicyData
import Network.AWS.FMS.Types.StatefulRuleGroup
import Network.AWS.FMS.Types.StatelessRuleGroup
import Network.AWS.FMS.Types.Tag
import Network.AWS.FMS.Types.ViolationDetail
import Network.AWS.FMS.UntagResource
