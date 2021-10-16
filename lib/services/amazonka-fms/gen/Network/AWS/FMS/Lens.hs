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

    -- ** DeleteNotificationChannel

    -- ** ListPolicies
    listPolicies_nextToken,
    listPolicies_maxResults,
    listPoliciesResponse_nextToken,
    listPoliciesResponse_policyList,
    listPoliciesResponse_httpStatus,

    -- ** DeletePolicy
    deletePolicy_deleteAllPolicyResources,
    deletePolicy_policyId,

    -- ** GetProtectionStatus
    getProtectionStatus_nextToken,
    getProtectionStatus_maxResults,
    getProtectionStatus_startTime,
    getProtectionStatus_endTime,
    getProtectionStatus_memberAccountId,
    getProtectionStatus_policyId,
    getProtectionStatusResponse_nextToken,
    getProtectionStatusResponse_adminAccountId,
    getProtectionStatusResponse_data,
    getProtectionStatusResponse_serviceType,
    getProtectionStatusResponse_httpStatus,

    -- ** ListAppsLists
    listAppsLists_nextToken,
    listAppsLists_defaultLists,
    listAppsLists_maxResults,
    listAppsListsResponse_nextToken,
    listAppsListsResponse_appsLists,
    listAppsListsResponse_httpStatus,

    -- ** PutAppsList
    putAppsList_tagList,
    putAppsList_appsList,
    putAppsListResponse_appsList,
    putAppsListResponse_appsListArn,
    putAppsListResponse_httpStatus,

    -- ** GetAppsList
    getAppsList_defaultList,
    getAppsList_listId,
    getAppsListResponse_appsList,
    getAppsListResponse_appsListArn,
    getAppsListResponse_httpStatus,

    -- ** ListProtocolsLists
    listProtocolsLists_nextToken,
    listProtocolsLists_defaultLists,
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

    -- ** ListComplianceStatus
    listComplianceStatus_nextToken,
    listComplianceStatus_maxResults,
    listComplianceStatus_policyId,
    listComplianceStatusResponse_nextToken,
    listComplianceStatusResponse_policyComplianceStatusList,
    listComplianceStatusResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tagList,
    tagResourceResponse_httpStatus,

    -- ** GetNotificationChannel
    getNotificationChannelResponse_snsRoleName,
    getNotificationChannelResponse_snsTopicArn,
    getNotificationChannelResponse_httpStatus,

    -- ** AssociateAdminAccount
    associateAdminAccount_adminAccount,

    -- ** PutNotificationChannel
    putNotificationChannel_snsTopicArn,
    putNotificationChannel_snsRoleName,

    -- ** DisassociateAdminAccount

    -- ** PutPolicy
    putPolicy_tagList,
    putPolicy_policy,
    putPolicyResponse_policy,
    putPolicyResponse_policyArn,
    putPolicyResponse_httpStatus,

    -- ** GetProtocolsList
    getProtocolsList_defaultList,
    getProtocolsList_listId,
    getProtocolsListResponse_protocolsList,
    getProtocolsListResponse_protocolsListArn,
    getProtocolsListResponse_httpStatus,

    -- ** DeleteAppsList
    deleteAppsList_listId,

    -- ** GetPolicy
    getPolicy_policyId,
    getPolicyResponse_policy,
    getPolicyResponse_policyArn,
    getPolicyResponse_httpStatus,

    -- ** DeleteProtocolsList
    deleteProtocolsList_listId,

    -- ** GetAdminAccount
    getAdminAccountResponse_adminAccount,
    getAdminAccountResponse_roleStatus,
    getAdminAccountResponse_httpStatus,

    -- ** ListMemberAccounts
    listMemberAccounts_nextToken,
    listMemberAccounts_maxResults,
    listMemberAccountsResponse_nextToken,
    listMemberAccountsResponse_memberAccounts,
    listMemberAccountsResponse_httpStatus,

    -- ** GetViolationDetails
    getViolationDetails_policyId,
    getViolationDetails_memberAccount,
    getViolationDetails_resourceId,
    getViolationDetails_resourceType,
    getViolationDetailsResponse_violationDetail,
    getViolationDetailsResponse_httpStatus,

    -- ** GetComplianceDetail
    getComplianceDetail_policyId,
    getComplianceDetail_memberAccount,
    getComplianceDetailResponse_policyComplianceDetail,
    getComplianceDetailResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tagList,
    listTagsForResourceResponse_httpStatus,

    -- * Types

    -- ** ActionTarget
    actionTarget_resourceId,
    actionTarget_description,

    -- ** App
    app_appName,
    app_protocol,
    app_port,

    -- ** AppsListData
    appsListData_lastUpdateTime,
    appsListData_listId,
    appsListData_createTime,
    appsListData_previousAppsList,
    appsListData_listUpdateToken,
    appsListData_listName,
    appsListData_appsList,

    -- ** AppsListDataSummary
    appsListDataSummary_appsList,
    appsListDataSummary_listName,
    appsListDataSummary_listId,
    appsListDataSummary_listArn,

    -- ** AwsEc2InstanceViolation
    awsEc2InstanceViolation_violationTarget,
    awsEc2InstanceViolation_awsEc2NetworkInterfaceViolations,

    -- ** AwsEc2NetworkInterfaceViolation
    awsEc2NetworkInterfaceViolation_violationTarget,
    awsEc2NetworkInterfaceViolation_violatingSecurityGroups,

    -- ** AwsVPCSecurityGroupViolation
    awsVPCSecurityGroupViolation_possibleSecurityGroupRemediationActions,
    awsVPCSecurityGroupViolation_partialMatches,
    awsVPCSecurityGroupViolation_violationTarget,
    awsVPCSecurityGroupViolation_violationTargetDescription,

    -- ** ComplianceViolator
    complianceViolator_resourceId,
    complianceViolator_resourceType,
    complianceViolator_violationReason,

    -- ** DnsDuplicateRuleGroupViolation
    dnsDuplicateRuleGroupViolation_violationTarget,
    dnsDuplicateRuleGroupViolation_violationTargetDescription,

    -- ** DnsRuleGroupLimitExceededViolation
    dnsRuleGroupLimitExceededViolation_violationTarget,
    dnsRuleGroupLimitExceededViolation_violationTargetDescription,
    dnsRuleGroupLimitExceededViolation_numberOfRuleGroupsAlreadyAssociated,

    -- ** DnsRuleGroupPriorityConflictViolation
    dnsRuleGroupPriorityConflictViolation_violationTarget,
    dnsRuleGroupPriorityConflictViolation_conflictingPriority,
    dnsRuleGroupPriorityConflictViolation_violationTargetDescription,
    dnsRuleGroupPriorityConflictViolation_unavailablePriorities,
    dnsRuleGroupPriorityConflictViolation_conflictingPolicyId,

    -- ** EC2AssociateRouteTableAction
    eC2AssociateRouteTableAction_description,
    eC2AssociateRouteTableAction_subnetId,
    eC2AssociateRouteTableAction_gatewayId,
    eC2AssociateRouteTableAction_routeTableId,

    -- ** EC2CopyRouteTableAction
    eC2CopyRouteTableAction_description,
    eC2CopyRouteTableAction_vpcId,
    eC2CopyRouteTableAction_routeTableId,

    -- ** EC2CreateRouteAction
    eC2CreateRouteAction_vpcEndpointId,
    eC2CreateRouteAction_destinationPrefixListId,
    eC2CreateRouteAction_destinationIpv6CidrBlock,
    eC2CreateRouteAction_destinationCidrBlock,
    eC2CreateRouteAction_description,
    eC2CreateRouteAction_gatewayId,
    eC2CreateRouteAction_routeTableId,

    -- ** EC2CreateRouteTableAction
    eC2CreateRouteTableAction_description,
    eC2CreateRouteTableAction_vpcId,

    -- ** EC2DeleteRouteAction
    eC2DeleteRouteAction_destinationPrefixListId,
    eC2DeleteRouteAction_destinationIpv6CidrBlock,
    eC2DeleteRouteAction_destinationCidrBlock,
    eC2DeleteRouteAction_description,
    eC2DeleteRouteAction_routeTableId,

    -- ** EC2ReplaceRouteAction
    eC2ReplaceRouteAction_destinationPrefixListId,
    eC2ReplaceRouteAction_destinationIpv6CidrBlock,
    eC2ReplaceRouteAction_destinationCidrBlock,
    eC2ReplaceRouteAction_description,
    eC2ReplaceRouteAction_gatewayId,
    eC2ReplaceRouteAction_routeTableId,

    -- ** EC2ReplaceRouteTableAssociationAction
    eC2ReplaceRouteTableAssociationAction_description,
    eC2ReplaceRouteTableAssociationAction_associationId,
    eC2ReplaceRouteTableAssociationAction_routeTableId,

    -- ** EvaluationResult
    evaluationResult_complianceStatus,
    evaluationResult_evaluationLimitExceeded,
    evaluationResult_violatorCount,

    -- ** ExpectedRoute
    expectedRoute_ipV4Cidr,
    expectedRoute_ipV6Cidr,
    expectedRoute_allowedTargets,
    expectedRoute_routeTableId,
    expectedRoute_prefixListId,
    expectedRoute_contributingSubnets,

    -- ** NetworkFirewallBlackHoleRouteDetectedViolation
    networkFirewallBlackHoleRouteDetectedViolation_violationTarget,
    networkFirewallBlackHoleRouteDetectedViolation_routeTableId,
    networkFirewallBlackHoleRouteDetectedViolation_violatingRoutes,
    networkFirewallBlackHoleRouteDetectedViolation_vpcId,

    -- ** NetworkFirewallInternetTrafficNotInspectedViolation
    networkFirewallInternetTrafficNotInspectedViolation_expectedInternetGatewayRoutes,
    networkFirewallInternetTrafficNotInspectedViolation_expectedFirewallSubnetRoutes,
    networkFirewallInternetTrafficNotInspectedViolation_isRouteTableUsedInDifferentAZ,
    networkFirewallInternetTrafficNotInspectedViolation_currentFirewallSubnetRouteTable,
    networkFirewallInternetTrafficNotInspectedViolation_currentInternetGatewayRouteTable,
    networkFirewallInternetTrafficNotInspectedViolation_routeTableId,
    networkFirewallInternetTrafficNotInspectedViolation_actualInternetGatewayRoutes,
    networkFirewallInternetTrafficNotInspectedViolation_subnetAvailabilityZone,
    networkFirewallInternetTrafficNotInspectedViolation_actualFirewallSubnetRoutes,
    networkFirewallInternetTrafficNotInspectedViolation_subnetId,
    networkFirewallInternetTrafficNotInspectedViolation_internetGatewayId,
    networkFirewallInternetTrafficNotInspectedViolation_violatingRoutes,
    networkFirewallInternetTrafficNotInspectedViolation_expectedFirewallEndpoint,
    networkFirewallInternetTrafficNotInspectedViolation_firewallSubnetId,
    networkFirewallInternetTrafficNotInspectedViolation_vpcId,

    -- ** NetworkFirewallInvalidRouteConfigurationViolation
    networkFirewallInvalidRouteConfigurationViolation_expectedInternetGatewayRoutes,
    networkFirewallInvalidRouteConfigurationViolation_expectedFirewallSubnetRoutes,
    networkFirewallInvalidRouteConfigurationViolation_actualFirewallSubnetId,
    networkFirewallInvalidRouteConfigurationViolation_isRouteTableUsedInDifferentAZ,
    networkFirewallInvalidRouteConfigurationViolation_actualFirewallEndpoint,
    networkFirewallInvalidRouteConfigurationViolation_currentFirewallSubnetRouteTable,
    networkFirewallInvalidRouteConfigurationViolation_currentInternetGatewayRouteTable,
    networkFirewallInvalidRouteConfigurationViolation_routeTableId,
    networkFirewallInvalidRouteConfigurationViolation_actualInternetGatewayRoutes,
    networkFirewallInvalidRouteConfigurationViolation_actualFirewallSubnetRoutes,
    networkFirewallInvalidRouteConfigurationViolation_internetGatewayId,
    networkFirewallInvalidRouteConfigurationViolation_expectedFirewallSubnetId,
    networkFirewallInvalidRouteConfigurationViolation_expectedFirewallEndpoint,
    networkFirewallInvalidRouteConfigurationViolation_violatingRoute,
    networkFirewallInvalidRouteConfigurationViolation_vpcId,
    networkFirewallInvalidRouteConfigurationViolation_affectedSubnets,

    -- ** NetworkFirewallMissingExpectedRTViolation
    networkFirewallMissingExpectedRTViolation_currentRouteTable,
    networkFirewallMissingExpectedRTViolation_violationTarget,
    networkFirewallMissingExpectedRTViolation_availabilityZone,
    networkFirewallMissingExpectedRTViolation_expectedRouteTable,
    networkFirewallMissingExpectedRTViolation_vpc,

    -- ** NetworkFirewallMissingExpectedRoutesViolation
    networkFirewallMissingExpectedRoutesViolation_expectedRoutes,
    networkFirewallMissingExpectedRoutesViolation_violationTarget,
    networkFirewallMissingExpectedRoutesViolation_vpcId,

    -- ** NetworkFirewallMissingFirewallViolation
    networkFirewallMissingFirewallViolation_targetViolationReason,
    networkFirewallMissingFirewallViolation_violationTarget,
    networkFirewallMissingFirewallViolation_availabilityZone,
    networkFirewallMissingFirewallViolation_vpc,

    -- ** NetworkFirewallMissingSubnetViolation
    networkFirewallMissingSubnetViolation_targetViolationReason,
    networkFirewallMissingSubnetViolation_violationTarget,
    networkFirewallMissingSubnetViolation_availabilityZone,
    networkFirewallMissingSubnetViolation_vpc,

    -- ** NetworkFirewallPolicyDescription
    networkFirewallPolicyDescription_statelessRuleGroups,
    networkFirewallPolicyDescription_statelessDefaultActions,
    networkFirewallPolicyDescription_statelessFragmentDefaultActions,
    networkFirewallPolicyDescription_statefulRuleGroups,
    networkFirewallPolicyDescription_statelessCustomActions,

    -- ** NetworkFirewallPolicyModifiedViolation
    networkFirewallPolicyModifiedViolation_currentPolicyDescription,
    networkFirewallPolicyModifiedViolation_violationTarget,
    networkFirewallPolicyModifiedViolation_expectedPolicyDescription,

    -- ** NetworkFirewallUnexpectedFirewallRoutesViolation
    networkFirewallUnexpectedFirewallRoutesViolation_firewallEndpoint,
    networkFirewallUnexpectedFirewallRoutesViolation_routeTableId,
    networkFirewallUnexpectedFirewallRoutesViolation_violatingRoutes,
    networkFirewallUnexpectedFirewallRoutesViolation_firewallSubnetId,
    networkFirewallUnexpectedFirewallRoutesViolation_vpcId,

    -- ** NetworkFirewallUnexpectedGatewayRoutesViolation
    networkFirewallUnexpectedGatewayRoutesViolation_routeTableId,
    networkFirewallUnexpectedGatewayRoutesViolation_violatingRoutes,
    networkFirewallUnexpectedGatewayRoutesViolation_vpcId,
    networkFirewallUnexpectedGatewayRoutesViolation_gatewayId,

    -- ** PartialMatch
    partialMatch_targetViolationReasons,
    partialMatch_reference,

    -- ** Policy
    policy_resourceTags,
    policy_resourceTypeList,
    policy_policyUpdateToken,
    policy_includeMap,
    policy_policyId,
    policy_excludeMap,
    policy_deleteUnusedFMManagedResources,
    policy_policyName,
    policy_securityServicePolicyData,
    policy_resourceType,
    policy_excludeResourceTags,
    policy_remediationEnabled,

    -- ** PolicyComplianceDetail
    policyComplianceDetail_violators,
    policyComplianceDetail_policyOwner,
    policyComplianceDetail_memberAccount,
    policyComplianceDetail_evaluationLimitExceeded,
    policyComplianceDetail_issueInfoMap,
    policyComplianceDetail_policyId,
    policyComplianceDetail_expiredAt,

    -- ** PolicyComplianceStatus
    policyComplianceStatus_policyName,
    policyComplianceStatus_policyOwner,
    policyComplianceStatus_memberAccount,
    policyComplianceStatus_lastUpdated,
    policyComplianceStatus_evaluationResults,
    policyComplianceStatus_issueInfoMap,
    policyComplianceStatus_policyId,

    -- ** PolicySummary
    policySummary_policyName,
    policySummary_securityServiceType,
    policySummary_resourceType,
    policySummary_remediationEnabled,
    policySummary_policyArn,
    policySummary_policyId,
    policySummary_deleteUnusedFMManagedResources,

    -- ** PossibleRemediationAction
    possibleRemediationAction_isDefaultAction,
    possibleRemediationAction_description,
    possibleRemediationAction_orderedRemediationActions,

    -- ** PossibleRemediationActions
    possibleRemediationActions_actions,
    possibleRemediationActions_description,

    -- ** ProtocolsListData
    protocolsListData_lastUpdateTime,
    protocolsListData_listId,
    protocolsListData_createTime,
    protocolsListData_previousProtocolsList,
    protocolsListData_listUpdateToken,
    protocolsListData_listName,
    protocolsListData_protocolsList,

    -- ** ProtocolsListDataSummary
    protocolsListDataSummary_listName,
    protocolsListDataSummary_listId,
    protocolsListDataSummary_listArn,
    protocolsListDataSummary_protocolsList,

    -- ** RemediationAction
    remediationAction_eC2CreateRouteAction,
    remediationAction_eC2ReplaceRouteAction,
    remediationAction_eC2AssociateRouteTableAction,
    remediationAction_eC2CreateRouteTableAction,
    remediationAction_description,
    remediationAction_eC2CopyRouteTableAction,
    remediationAction_eC2ReplaceRouteTableAssociationAction,
    remediationAction_eC2DeleteRouteAction,

    -- ** RemediationActionWithOrder
    remediationActionWithOrder_remediationAction,
    remediationActionWithOrder_order,

    -- ** ResourceTag
    resourceTag_value,
    resourceTag_key,

    -- ** ResourceViolation
    resourceViolation_networkFirewallBlackHoleRouteDetectedViolation,
    resourceViolation_possibleRemediationActions,
    resourceViolation_dnsRuleGroupLimitExceededViolation,
    resourceViolation_networkFirewallInvalidRouteConfigurationViolation,
    resourceViolation_networkFirewallMissingSubnetViolation,
    resourceViolation_dnsDuplicateRuleGroupViolation,
    resourceViolation_awsEc2NetworkInterfaceViolation,
    resourceViolation_networkFirewallInternetTrafficNotInspectedViolation,
    resourceViolation_networkFirewallMissingExpectedRTViolation,
    resourceViolation_networkFirewallMissingExpectedRoutesViolation,
    resourceViolation_awsEc2InstanceViolation,
    resourceViolation_networkFirewallUnexpectedGatewayRoutesViolation,
    resourceViolation_networkFirewallMissingFirewallViolation,
    resourceViolation_networkFirewallUnexpectedFirewallRoutesViolation,
    resourceViolation_awsVPCSecurityGroupViolation,
    resourceViolation_networkFirewallPolicyModifiedViolation,
    resourceViolation_dnsRuleGroupPriorityConflictViolation,

    -- ** Route
    route_destinationType,
    route_targetType,
    route_destination,
    route_target,

    -- ** SecurityGroupRemediationAction
    securityGroupRemediationAction_remediationActionType,
    securityGroupRemediationAction_remediationResult,
    securityGroupRemediationAction_isDefaultAction,
    securityGroupRemediationAction_description,

    -- ** SecurityGroupRuleDescription
    securityGroupRuleDescription_fromPort,
    securityGroupRuleDescription_iPV4Range,
    securityGroupRuleDescription_prefixListId,
    securityGroupRuleDescription_protocol,
    securityGroupRuleDescription_iPV6Range,
    securityGroupRuleDescription_toPort,

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
    violationDetail_resourceDescription,
    violationDetail_resourceTags,
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
