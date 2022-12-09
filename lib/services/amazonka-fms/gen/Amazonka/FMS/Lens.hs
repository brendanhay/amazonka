{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.FMS.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Lens
  ( -- * Operations

    -- ** AssociateAdminAccount
    associateAdminAccount_adminAccount,

    -- ** AssociateThirdPartyFirewall
    associateThirdPartyFirewall_thirdPartyFirewall,
    associateThirdPartyFirewallResponse_thirdPartyFirewallStatus,
    associateThirdPartyFirewallResponse_httpStatus,

    -- ** BatchAssociateResource
    batchAssociateResource_resourceSetIdentifier,
    batchAssociateResource_items,
    batchAssociateResourceResponse_httpStatus,
    batchAssociateResourceResponse_resourceSetIdentifier,
    batchAssociateResourceResponse_failedItems,

    -- ** BatchDisassociateResource
    batchDisassociateResource_resourceSetIdentifier,
    batchDisassociateResource_items,
    batchDisassociateResourceResponse_httpStatus,
    batchDisassociateResourceResponse_resourceSetIdentifier,
    batchDisassociateResourceResponse_failedItems,

    -- ** DeleteAppsList
    deleteAppsList_listId,

    -- ** DeleteNotificationChannel

    -- ** DeletePolicy
    deletePolicy_deleteAllPolicyResources,
    deletePolicy_policyId,

    -- ** DeleteProtocolsList
    deleteProtocolsList_listId,

    -- ** DeleteResourceSet
    deleteResourceSet_identifier,

    -- ** DisassociateAdminAccount

    -- ** DisassociateThirdPartyFirewall
    disassociateThirdPartyFirewall_thirdPartyFirewall,
    disassociateThirdPartyFirewallResponse_thirdPartyFirewallStatus,
    disassociateThirdPartyFirewallResponse_httpStatus,

    -- ** GetAdminAccount
    getAdminAccountResponse_adminAccount,
    getAdminAccountResponse_roleStatus,
    getAdminAccountResponse_httpStatus,

    -- ** GetAppsList
    getAppsList_defaultList,
    getAppsList_listId,
    getAppsListResponse_appsList,
    getAppsListResponse_appsListArn,
    getAppsListResponse_httpStatus,

    -- ** GetComplianceDetail
    getComplianceDetail_policyId,
    getComplianceDetail_memberAccount,
    getComplianceDetailResponse_policyComplianceDetail,
    getComplianceDetailResponse_httpStatus,

    -- ** GetNotificationChannel
    getNotificationChannelResponse_snsRoleName,
    getNotificationChannelResponse_snsTopicArn,
    getNotificationChannelResponse_httpStatus,

    -- ** GetPolicy
    getPolicy_policyId,
    getPolicyResponse_policy,
    getPolicyResponse_policyArn,
    getPolicyResponse_httpStatus,

    -- ** GetProtectionStatus
    getProtectionStatus_endTime,
    getProtectionStatus_maxResults,
    getProtectionStatus_memberAccountId,
    getProtectionStatus_nextToken,
    getProtectionStatus_startTime,
    getProtectionStatus_policyId,
    getProtectionStatusResponse_adminAccountId,
    getProtectionStatusResponse_data,
    getProtectionStatusResponse_nextToken,
    getProtectionStatusResponse_serviceType,
    getProtectionStatusResponse_httpStatus,

    -- ** GetProtocolsList
    getProtocolsList_defaultList,
    getProtocolsList_listId,
    getProtocolsListResponse_protocolsList,
    getProtocolsListResponse_protocolsListArn,
    getProtocolsListResponse_httpStatus,

    -- ** GetResourceSet
    getResourceSet_identifier,
    getResourceSetResponse_httpStatus,
    getResourceSetResponse_resourceSet,
    getResourceSetResponse_resourceSetArn,

    -- ** GetThirdPartyFirewallAssociationStatus
    getThirdPartyFirewallAssociationStatus_thirdPartyFirewall,
    getThirdPartyFirewallAssociationStatusResponse_marketplaceOnboardingStatus,
    getThirdPartyFirewallAssociationStatusResponse_thirdPartyFirewallStatus,
    getThirdPartyFirewallAssociationStatusResponse_httpStatus,

    -- ** GetViolationDetails
    getViolationDetails_policyId,
    getViolationDetails_memberAccount,
    getViolationDetails_resourceId,
    getViolationDetails_resourceType,
    getViolationDetailsResponse_violationDetail,
    getViolationDetailsResponse_httpStatus,

    -- ** ListAppsLists
    listAppsLists_defaultLists,
    listAppsLists_nextToken,
    listAppsLists_maxResults,
    listAppsListsResponse_appsLists,
    listAppsListsResponse_nextToken,
    listAppsListsResponse_httpStatus,

    -- ** ListComplianceStatus
    listComplianceStatus_maxResults,
    listComplianceStatus_nextToken,
    listComplianceStatus_policyId,
    listComplianceStatusResponse_nextToken,
    listComplianceStatusResponse_policyComplianceStatusList,
    listComplianceStatusResponse_httpStatus,

    -- ** ListDiscoveredResources
    listDiscoveredResources_maxResults,
    listDiscoveredResources_nextToken,
    listDiscoveredResources_memberAccountIds,
    listDiscoveredResources_resourceType,
    listDiscoveredResourcesResponse_items,
    listDiscoveredResourcesResponse_nextToken,
    listDiscoveredResourcesResponse_httpStatus,

    -- ** ListMemberAccounts
    listMemberAccounts_maxResults,
    listMemberAccounts_nextToken,
    listMemberAccountsResponse_memberAccounts,
    listMemberAccountsResponse_nextToken,
    listMemberAccountsResponse_httpStatus,

    -- ** ListPolicies
    listPolicies_maxResults,
    listPolicies_nextToken,
    listPoliciesResponse_nextToken,
    listPoliciesResponse_policyList,
    listPoliciesResponse_httpStatus,

    -- ** ListProtocolsLists
    listProtocolsLists_defaultLists,
    listProtocolsLists_nextToken,
    listProtocolsLists_maxResults,
    listProtocolsListsResponse_nextToken,
    listProtocolsListsResponse_protocolsLists,
    listProtocolsListsResponse_httpStatus,

    -- ** ListResourceSetResources
    listResourceSetResources_maxResults,
    listResourceSetResources_nextToken,
    listResourceSetResources_identifier,
    listResourceSetResourcesResponse_nextToken,
    listResourceSetResourcesResponse_httpStatus,
    listResourceSetResourcesResponse_items,

    -- ** ListResourceSets
    listResourceSets_maxResults,
    listResourceSets_nextToken,
    listResourceSetsResponse_nextToken,
    listResourceSetsResponse_resourceSets,
    listResourceSetsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tagList,
    listTagsForResourceResponse_httpStatus,

    -- ** ListThirdPartyFirewallFirewallPolicies
    listThirdPartyFirewallFirewallPolicies_nextToken,
    listThirdPartyFirewallFirewallPolicies_thirdPartyFirewall,
    listThirdPartyFirewallFirewallPolicies_maxResults,
    listThirdPartyFirewallFirewallPoliciesResponse_nextToken,
    listThirdPartyFirewallFirewallPoliciesResponse_thirdPartyFirewallFirewallPolicies,
    listThirdPartyFirewallFirewallPoliciesResponse_httpStatus,

    -- ** PutAppsList
    putAppsList_tagList,
    putAppsList_appsList,
    putAppsListResponse_appsList,
    putAppsListResponse_appsListArn,
    putAppsListResponse_httpStatus,

    -- ** PutNotificationChannel
    putNotificationChannel_snsTopicArn,
    putNotificationChannel_snsRoleName,

    -- ** PutPolicy
    putPolicy_tagList,
    putPolicy_policy,
    putPolicyResponse_policy,
    putPolicyResponse_policyArn,
    putPolicyResponse_httpStatus,

    -- ** PutProtocolsList
    putProtocolsList_tagList,
    putProtocolsList_protocolsList,
    putProtocolsListResponse_protocolsList,
    putProtocolsListResponse_protocolsListArn,
    putProtocolsListResponse_httpStatus,

    -- ** PutResourceSet
    putResourceSet_tagList,
    putResourceSet_resourceSet,
    putResourceSetResponse_httpStatus,
    putResourceSetResponse_resourceSet,
    putResourceSetResponse_resourceSetArn,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tagList,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- * Types

    -- ** ActionTarget
    actionTarget_description,
    actionTarget_resourceId,

    -- ** App
    app_appName,
    app_protocol,
    app_port,

    -- ** AppsListData
    appsListData_createTime,
    appsListData_lastUpdateTime,
    appsListData_listId,
    appsListData_listUpdateToken,
    appsListData_previousAppsList,
    appsListData_listName,
    appsListData_appsList,

    -- ** AppsListDataSummary
    appsListDataSummary_appsList,
    appsListDataSummary_listArn,
    appsListDataSummary_listId,
    appsListDataSummary_listName,

    -- ** AwsEc2InstanceViolation
    awsEc2InstanceViolation_awsEc2NetworkInterfaceViolations,
    awsEc2InstanceViolation_violationTarget,

    -- ** AwsEc2NetworkInterfaceViolation
    awsEc2NetworkInterfaceViolation_violatingSecurityGroups,
    awsEc2NetworkInterfaceViolation_violationTarget,

    -- ** AwsVPCSecurityGroupViolation
    awsVPCSecurityGroupViolation_partialMatches,
    awsVPCSecurityGroupViolation_possibleSecurityGroupRemediationActions,
    awsVPCSecurityGroupViolation_violationTarget,
    awsVPCSecurityGroupViolation_violationTargetDescription,

    -- ** ComplianceViolator
    complianceViolator_metadata,
    complianceViolator_resourceId,
    complianceViolator_resourceType,
    complianceViolator_violationReason,

    -- ** DiscoveredResource
    discoveredResource_accountId,
    discoveredResource_name,
    discoveredResource_type,
    discoveredResource_uri,

    -- ** DnsDuplicateRuleGroupViolation
    dnsDuplicateRuleGroupViolation_violationTarget,
    dnsDuplicateRuleGroupViolation_violationTargetDescription,

    -- ** DnsRuleGroupLimitExceededViolation
    dnsRuleGroupLimitExceededViolation_numberOfRuleGroupsAlreadyAssociated,
    dnsRuleGroupLimitExceededViolation_violationTarget,
    dnsRuleGroupLimitExceededViolation_violationTargetDescription,

    -- ** DnsRuleGroupPriorityConflictViolation
    dnsRuleGroupPriorityConflictViolation_conflictingPolicyId,
    dnsRuleGroupPriorityConflictViolation_conflictingPriority,
    dnsRuleGroupPriorityConflictViolation_unavailablePriorities,
    dnsRuleGroupPriorityConflictViolation_violationTarget,
    dnsRuleGroupPriorityConflictViolation_violationTargetDescription,

    -- ** EC2AssociateRouteTableAction
    eC2AssociateRouteTableAction_description,
    eC2AssociateRouteTableAction_gatewayId,
    eC2AssociateRouteTableAction_subnetId,
    eC2AssociateRouteTableAction_routeTableId,

    -- ** EC2CopyRouteTableAction
    eC2CopyRouteTableAction_description,
    eC2CopyRouteTableAction_vpcId,
    eC2CopyRouteTableAction_routeTableId,

    -- ** EC2CreateRouteAction
    eC2CreateRouteAction_description,
    eC2CreateRouteAction_destinationCidrBlock,
    eC2CreateRouteAction_destinationIpv6CidrBlock,
    eC2CreateRouteAction_destinationPrefixListId,
    eC2CreateRouteAction_gatewayId,
    eC2CreateRouteAction_vpcEndpointId,
    eC2CreateRouteAction_routeTableId,

    -- ** EC2CreateRouteTableAction
    eC2CreateRouteTableAction_description,
    eC2CreateRouteTableAction_vpcId,

    -- ** EC2DeleteRouteAction
    eC2DeleteRouteAction_description,
    eC2DeleteRouteAction_destinationCidrBlock,
    eC2DeleteRouteAction_destinationIpv6CidrBlock,
    eC2DeleteRouteAction_destinationPrefixListId,
    eC2DeleteRouteAction_routeTableId,

    -- ** EC2ReplaceRouteAction
    eC2ReplaceRouteAction_description,
    eC2ReplaceRouteAction_destinationCidrBlock,
    eC2ReplaceRouteAction_destinationIpv6CidrBlock,
    eC2ReplaceRouteAction_destinationPrefixListId,
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
    expectedRoute_allowedTargets,
    expectedRoute_contributingSubnets,
    expectedRoute_ipV4Cidr,
    expectedRoute_ipV6Cidr,
    expectedRoute_prefixListId,
    expectedRoute_routeTableId,

    -- ** FMSPolicyUpdateFirewallCreationConfigAction
    fMSPolicyUpdateFirewallCreationConfigAction_description,
    fMSPolicyUpdateFirewallCreationConfigAction_firewallCreationConfig,

    -- ** FailedItem
    failedItem_reason,
    failedItem_uri,

    -- ** FirewallSubnetIsOutOfScopeViolation
    firewallSubnetIsOutOfScopeViolation_firewallSubnetId,
    firewallSubnetIsOutOfScopeViolation_subnetAvailabilityZone,
    firewallSubnetIsOutOfScopeViolation_subnetAvailabilityZoneId,
    firewallSubnetIsOutOfScopeViolation_vpcEndpointId,
    firewallSubnetIsOutOfScopeViolation_vpcId,

    -- ** FirewallSubnetMissingVPCEndpointViolation
    firewallSubnetMissingVPCEndpointViolation_firewallSubnetId,
    firewallSubnetMissingVPCEndpointViolation_subnetAvailabilityZone,
    firewallSubnetMissingVPCEndpointViolation_subnetAvailabilityZoneId,
    firewallSubnetMissingVPCEndpointViolation_vpcId,

    -- ** NetworkFirewallBlackHoleRouteDetectedViolation
    networkFirewallBlackHoleRouteDetectedViolation_routeTableId,
    networkFirewallBlackHoleRouteDetectedViolation_violatingRoutes,
    networkFirewallBlackHoleRouteDetectedViolation_violationTarget,
    networkFirewallBlackHoleRouteDetectedViolation_vpcId,

    -- ** NetworkFirewallInternetTrafficNotInspectedViolation
    networkFirewallInternetTrafficNotInspectedViolation_actualFirewallSubnetRoutes,
    networkFirewallInternetTrafficNotInspectedViolation_actualInternetGatewayRoutes,
    networkFirewallInternetTrafficNotInspectedViolation_currentFirewallSubnetRouteTable,
    networkFirewallInternetTrafficNotInspectedViolation_currentInternetGatewayRouteTable,
    networkFirewallInternetTrafficNotInspectedViolation_expectedFirewallEndpoint,
    networkFirewallInternetTrafficNotInspectedViolation_expectedFirewallSubnetRoutes,
    networkFirewallInternetTrafficNotInspectedViolation_expectedInternetGatewayRoutes,
    networkFirewallInternetTrafficNotInspectedViolation_firewallSubnetId,
    networkFirewallInternetTrafficNotInspectedViolation_internetGatewayId,
    networkFirewallInternetTrafficNotInspectedViolation_isRouteTableUsedInDifferentAZ,
    networkFirewallInternetTrafficNotInspectedViolation_routeTableId,
    networkFirewallInternetTrafficNotInspectedViolation_subnetAvailabilityZone,
    networkFirewallInternetTrafficNotInspectedViolation_subnetId,
    networkFirewallInternetTrafficNotInspectedViolation_violatingRoutes,
    networkFirewallInternetTrafficNotInspectedViolation_vpcId,

    -- ** NetworkFirewallInvalidRouteConfigurationViolation
    networkFirewallInvalidRouteConfigurationViolation_actualFirewallEndpoint,
    networkFirewallInvalidRouteConfigurationViolation_actualFirewallSubnetId,
    networkFirewallInvalidRouteConfigurationViolation_actualFirewallSubnetRoutes,
    networkFirewallInvalidRouteConfigurationViolation_actualInternetGatewayRoutes,
    networkFirewallInvalidRouteConfigurationViolation_affectedSubnets,
    networkFirewallInvalidRouteConfigurationViolation_currentFirewallSubnetRouteTable,
    networkFirewallInvalidRouteConfigurationViolation_currentInternetGatewayRouteTable,
    networkFirewallInvalidRouteConfigurationViolation_expectedFirewallEndpoint,
    networkFirewallInvalidRouteConfigurationViolation_expectedFirewallSubnetId,
    networkFirewallInvalidRouteConfigurationViolation_expectedFirewallSubnetRoutes,
    networkFirewallInvalidRouteConfigurationViolation_expectedInternetGatewayRoutes,
    networkFirewallInvalidRouteConfigurationViolation_internetGatewayId,
    networkFirewallInvalidRouteConfigurationViolation_isRouteTableUsedInDifferentAZ,
    networkFirewallInvalidRouteConfigurationViolation_routeTableId,
    networkFirewallInvalidRouteConfigurationViolation_violatingRoute,
    networkFirewallInvalidRouteConfigurationViolation_vpcId,

    -- ** NetworkFirewallMissingExpectedRTViolation
    networkFirewallMissingExpectedRTViolation_availabilityZone,
    networkFirewallMissingExpectedRTViolation_currentRouteTable,
    networkFirewallMissingExpectedRTViolation_expectedRouteTable,
    networkFirewallMissingExpectedRTViolation_vpc,
    networkFirewallMissingExpectedRTViolation_violationTarget,

    -- ** NetworkFirewallMissingExpectedRoutesViolation
    networkFirewallMissingExpectedRoutesViolation_expectedRoutes,
    networkFirewallMissingExpectedRoutesViolation_violationTarget,
    networkFirewallMissingExpectedRoutesViolation_vpcId,

    -- ** NetworkFirewallMissingFirewallViolation
    networkFirewallMissingFirewallViolation_availabilityZone,
    networkFirewallMissingFirewallViolation_targetViolationReason,
    networkFirewallMissingFirewallViolation_vpc,
    networkFirewallMissingFirewallViolation_violationTarget,

    -- ** NetworkFirewallMissingSubnetViolation
    networkFirewallMissingSubnetViolation_availabilityZone,
    networkFirewallMissingSubnetViolation_targetViolationReason,
    networkFirewallMissingSubnetViolation_vpc,
    networkFirewallMissingSubnetViolation_violationTarget,

    -- ** NetworkFirewallPolicy
    networkFirewallPolicy_firewallDeploymentModel,

    -- ** NetworkFirewallPolicyDescription
    networkFirewallPolicyDescription_statefulDefaultActions,
    networkFirewallPolicyDescription_statefulEngineOptions,
    networkFirewallPolicyDescription_statefulRuleGroups,
    networkFirewallPolicyDescription_statelessCustomActions,
    networkFirewallPolicyDescription_statelessDefaultActions,
    networkFirewallPolicyDescription_statelessFragmentDefaultActions,
    networkFirewallPolicyDescription_statelessRuleGroups,

    -- ** NetworkFirewallPolicyModifiedViolation
    networkFirewallPolicyModifiedViolation_currentPolicyDescription,
    networkFirewallPolicyModifiedViolation_expectedPolicyDescription,
    networkFirewallPolicyModifiedViolation_violationTarget,

    -- ** NetworkFirewallStatefulRuleGroupOverride
    networkFirewallStatefulRuleGroupOverride_action,

    -- ** NetworkFirewallUnexpectedFirewallRoutesViolation
    networkFirewallUnexpectedFirewallRoutesViolation_firewallEndpoint,
    networkFirewallUnexpectedFirewallRoutesViolation_firewallSubnetId,
    networkFirewallUnexpectedFirewallRoutesViolation_routeTableId,
    networkFirewallUnexpectedFirewallRoutesViolation_violatingRoutes,
    networkFirewallUnexpectedFirewallRoutesViolation_vpcId,

    -- ** NetworkFirewallUnexpectedGatewayRoutesViolation
    networkFirewallUnexpectedGatewayRoutesViolation_gatewayId,
    networkFirewallUnexpectedGatewayRoutesViolation_routeTableId,
    networkFirewallUnexpectedGatewayRoutesViolation_violatingRoutes,
    networkFirewallUnexpectedGatewayRoutesViolation_vpcId,

    -- ** PartialMatch
    partialMatch_reference,
    partialMatch_targetViolationReasons,

    -- ** Policy
    policy_deleteUnusedFMManagedResources,
    policy_excludeMap,
    policy_includeMap,
    policy_policyDescription,
    policy_policyId,
    policy_policyUpdateToken,
    policy_resourceSetIds,
    policy_resourceTags,
    policy_resourceTypeList,
    policy_policyName,
    policy_securityServicePolicyData,
    policy_resourceType,
    policy_excludeResourceTags,
    policy_remediationEnabled,

    -- ** PolicyComplianceDetail
    policyComplianceDetail_evaluationLimitExceeded,
    policyComplianceDetail_expiredAt,
    policyComplianceDetail_issueInfoMap,
    policyComplianceDetail_memberAccount,
    policyComplianceDetail_policyId,
    policyComplianceDetail_policyOwner,
    policyComplianceDetail_violators,

    -- ** PolicyComplianceStatus
    policyComplianceStatus_evaluationResults,
    policyComplianceStatus_issueInfoMap,
    policyComplianceStatus_lastUpdated,
    policyComplianceStatus_memberAccount,
    policyComplianceStatus_policyId,
    policyComplianceStatus_policyName,
    policyComplianceStatus_policyOwner,

    -- ** PolicyOption
    policyOption_networkFirewallPolicy,
    policyOption_thirdPartyFirewallPolicy,

    -- ** PolicySummary
    policySummary_deleteUnusedFMManagedResources,
    policySummary_policyArn,
    policySummary_policyId,
    policySummary_policyName,
    policySummary_remediationEnabled,
    policySummary_resourceType,
    policySummary_securityServiceType,

    -- ** PossibleRemediationAction
    possibleRemediationAction_description,
    possibleRemediationAction_isDefaultAction,
    possibleRemediationAction_orderedRemediationActions,

    -- ** PossibleRemediationActions
    possibleRemediationActions_actions,
    possibleRemediationActions_description,

    -- ** ProtocolsListData
    protocolsListData_createTime,
    protocolsListData_lastUpdateTime,
    protocolsListData_listId,
    protocolsListData_listUpdateToken,
    protocolsListData_previousProtocolsList,
    protocolsListData_listName,
    protocolsListData_protocolsList,

    -- ** ProtocolsListDataSummary
    protocolsListDataSummary_listArn,
    protocolsListDataSummary_listId,
    protocolsListDataSummary_listName,
    protocolsListDataSummary_protocolsList,

    -- ** RemediationAction
    remediationAction_description,
    remediationAction_eC2AssociateRouteTableAction,
    remediationAction_eC2CopyRouteTableAction,
    remediationAction_eC2CreateRouteAction,
    remediationAction_eC2CreateRouteTableAction,
    remediationAction_eC2DeleteRouteAction,
    remediationAction_eC2ReplaceRouteAction,
    remediationAction_eC2ReplaceRouteTableAssociationAction,
    remediationAction_fMSPolicyUpdateFirewallCreationConfigAction,

    -- ** RemediationActionWithOrder
    remediationActionWithOrder_order,
    remediationActionWithOrder_remediationAction,

    -- ** Resource
    resource_accountId,
    resource_uri,

    -- ** ResourceSet
    resourceSet_description,
    resourceSet_id,
    resourceSet_lastUpdateTime,
    resourceSet_updateToken,
    resourceSet_name,
    resourceSet_resourceTypeList,

    -- ** ResourceSetSummary
    resourceSetSummary_description,
    resourceSetSummary_id,
    resourceSetSummary_lastUpdateTime,
    resourceSetSummary_name,

    -- ** ResourceTag
    resourceTag_value,
    resourceTag_key,

    -- ** ResourceViolation
    resourceViolation_awsEc2InstanceViolation,
    resourceViolation_awsEc2NetworkInterfaceViolation,
    resourceViolation_awsVPCSecurityGroupViolation,
    resourceViolation_dnsDuplicateRuleGroupViolation,
    resourceViolation_dnsRuleGroupLimitExceededViolation,
    resourceViolation_dnsRuleGroupPriorityConflictViolation,
    resourceViolation_firewallSubnetIsOutOfScopeViolation,
    resourceViolation_firewallSubnetMissingVPCEndpointViolation,
    resourceViolation_networkFirewallBlackHoleRouteDetectedViolation,
    resourceViolation_networkFirewallInternetTrafficNotInspectedViolation,
    resourceViolation_networkFirewallInvalidRouteConfigurationViolation,
    resourceViolation_networkFirewallMissingExpectedRTViolation,
    resourceViolation_networkFirewallMissingExpectedRoutesViolation,
    resourceViolation_networkFirewallMissingFirewallViolation,
    resourceViolation_networkFirewallMissingSubnetViolation,
    resourceViolation_networkFirewallPolicyModifiedViolation,
    resourceViolation_networkFirewallUnexpectedFirewallRoutesViolation,
    resourceViolation_networkFirewallUnexpectedGatewayRoutesViolation,
    resourceViolation_possibleRemediationActions,
    resourceViolation_routeHasOutOfScopeEndpointViolation,
    resourceViolation_thirdPartyFirewallMissingExpectedRouteTableViolation,
    resourceViolation_thirdPartyFirewallMissingFirewallViolation,
    resourceViolation_thirdPartyFirewallMissingSubnetViolation,

    -- ** Route
    route_destination,
    route_destinationType,
    route_target,
    route_targetType,

    -- ** RouteHasOutOfScopeEndpointViolation
    routeHasOutOfScopeEndpointViolation_currentFirewallSubnetRouteTable,
    routeHasOutOfScopeEndpointViolation_currentInternetGatewayRouteTable,
    routeHasOutOfScopeEndpointViolation_firewallSubnetId,
    routeHasOutOfScopeEndpointViolation_firewallSubnetRoutes,
    routeHasOutOfScopeEndpointViolation_internetGatewayId,
    routeHasOutOfScopeEndpointViolation_internetGatewayRoutes,
    routeHasOutOfScopeEndpointViolation_routeTableId,
    routeHasOutOfScopeEndpointViolation_subnetAvailabilityZone,
    routeHasOutOfScopeEndpointViolation_subnetAvailabilityZoneId,
    routeHasOutOfScopeEndpointViolation_subnetId,
    routeHasOutOfScopeEndpointViolation_violatingRoutes,
    routeHasOutOfScopeEndpointViolation_vpcId,

    -- ** SecurityGroupRemediationAction
    securityGroupRemediationAction_description,
    securityGroupRemediationAction_isDefaultAction,
    securityGroupRemediationAction_remediationActionType,
    securityGroupRemediationAction_remediationResult,

    -- ** SecurityGroupRuleDescription
    securityGroupRuleDescription_fromPort,
    securityGroupRuleDescription_iPV4Range,
    securityGroupRuleDescription_iPV6Range,
    securityGroupRuleDescription_prefixListId,
    securityGroupRuleDescription_protocol,
    securityGroupRuleDescription_toPort,

    -- ** SecurityServicePolicyData
    securityServicePolicyData_managedServiceData,
    securityServicePolicyData_policyOption,
    securityServicePolicyData_type,

    -- ** StatefulEngineOptions
    statefulEngineOptions_ruleOrder,

    -- ** StatefulRuleGroup
    statefulRuleGroup_override,
    statefulRuleGroup_priority,
    statefulRuleGroup_resourceId,
    statefulRuleGroup_ruleGroupName,

    -- ** StatelessRuleGroup
    statelessRuleGroup_priority,
    statelessRuleGroup_resourceId,
    statelessRuleGroup_ruleGroupName,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** ThirdPartyFirewallFirewallPolicy
    thirdPartyFirewallFirewallPolicy_firewallPolicyId,
    thirdPartyFirewallFirewallPolicy_firewallPolicyName,

    -- ** ThirdPartyFirewallMissingExpectedRouteTableViolation
    thirdPartyFirewallMissingExpectedRouteTableViolation_availabilityZone,
    thirdPartyFirewallMissingExpectedRouteTableViolation_currentRouteTable,
    thirdPartyFirewallMissingExpectedRouteTableViolation_expectedRouteTable,
    thirdPartyFirewallMissingExpectedRouteTableViolation_vpc,
    thirdPartyFirewallMissingExpectedRouteTableViolation_violationTarget,

    -- ** ThirdPartyFirewallMissingFirewallViolation
    thirdPartyFirewallMissingFirewallViolation_availabilityZone,
    thirdPartyFirewallMissingFirewallViolation_targetViolationReason,
    thirdPartyFirewallMissingFirewallViolation_vpc,
    thirdPartyFirewallMissingFirewallViolation_violationTarget,

    -- ** ThirdPartyFirewallMissingSubnetViolation
    thirdPartyFirewallMissingSubnetViolation_availabilityZone,
    thirdPartyFirewallMissingSubnetViolation_targetViolationReason,
    thirdPartyFirewallMissingSubnetViolation_vpc,
    thirdPartyFirewallMissingSubnetViolation_violationTarget,

    -- ** ThirdPartyFirewallPolicy
    thirdPartyFirewallPolicy_firewallDeploymentModel,

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

import Amazonka.FMS.AssociateAdminAccount
import Amazonka.FMS.AssociateThirdPartyFirewall
import Amazonka.FMS.BatchAssociateResource
import Amazonka.FMS.BatchDisassociateResource
import Amazonka.FMS.DeleteAppsList
import Amazonka.FMS.DeleteNotificationChannel
import Amazonka.FMS.DeletePolicy
import Amazonka.FMS.DeleteProtocolsList
import Amazonka.FMS.DeleteResourceSet
import Amazonka.FMS.DisassociateAdminAccount
import Amazonka.FMS.DisassociateThirdPartyFirewall
import Amazonka.FMS.GetAdminAccount
import Amazonka.FMS.GetAppsList
import Amazonka.FMS.GetComplianceDetail
import Amazonka.FMS.GetNotificationChannel
import Amazonka.FMS.GetPolicy
import Amazonka.FMS.GetProtectionStatus
import Amazonka.FMS.GetProtocolsList
import Amazonka.FMS.GetResourceSet
import Amazonka.FMS.GetThirdPartyFirewallAssociationStatus
import Amazonka.FMS.GetViolationDetails
import Amazonka.FMS.ListAppsLists
import Amazonka.FMS.ListComplianceStatus
import Amazonka.FMS.ListDiscoveredResources
import Amazonka.FMS.ListMemberAccounts
import Amazonka.FMS.ListPolicies
import Amazonka.FMS.ListProtocolsLists
import Amazonka.FMS.ListResourceSetResources
import Amazonka.FMS.ListResourceSets
import Amazonka.FMS.ListTagsForResource
import Amazonka.FMS.ListThirdPartyFirewallFirewallPolicies
import Amazonka.FMS.PutAppsList
import Amazonka.FMS.PutNotificationChannel
import Amazonka.FMS.PutPolicy
import Amazonka.FMS.PutProtocolsList
import Amazonka.FMS.PutResourceSet
import Amazonka.FMS.TagResource
import Amazonka.FMS.Types.ActionTarget
import Amazonka.FMS.Types.App
import Amazonka.FMS.Types.AppsListData
import Amazonka.FMS.Types.AppsListDataSummary
import Amazonka.FMS.Types.AwsEc2InstanceViolation
import Amazonka.FMS.Types.AwsEc2NetworkInterfaceViolation
import Amazonka.FMS.Types.AwsVPCSecurityGroupViolation
import Amazonka.FMS.Types.ComplianceViolator
import Amazonka.FMS.Types.DiscoveredResource
import Amazonka.FMS.Types.DnsDuplicateRuleGroupViolation
import Amazonka.FMS.Types.DnsRuleGroupLimitExceededViolation
import Amazonka.FMS.Types.DnsRuleGroupPriorityConflictViolation
import Amazonka.FMS.Types.EC2AssociateRouteTableAction
import Amazonka.FMS.Types.EC2CopyRouteTableAction
import Amazonka.FMS.Types.EC2CreateRouteAction
import Amazonka.FMS.Types.EC2CreateRouteTableAction
import Amazonka.FMS.Types.EC2DeleteRouteAction
import Amazonka.FMS.Types.EC2ReplaceRouteAction
import Amazonka.FMS.Types.EC2ReplaceRouteTableAssociationAction
import Amazonka.FMS.Types.EvaluationResult
import Amazonka.FMS.Types.ExpectedRoute
import Amazonka.FMS.Types.FMSPolicyUpdateFirewallCreationConfigAction
import Amazonka.FMS.Types.FailedItem
import Amazonka.FMS.Types.FirewallSubnetIsOutOfScopeViolation
import Amazonka.FMS.Types.FirewallSubnetMissingVPCEndpointViolation
import Amazonka.FMS.Types.NetworkFirewallBlackHoleRouteDetectedViolation
import Amazonka.FMS.Types.NetworkFirewallInternetTrafficNotInspectedViolation
import Amazonka.FMS.Types.NetworkFirewallInvalidRouteConfigurationViolation
import Amazonka.FMS.Types.NetworkFirewallMissingExpectedRTViolation
import Amazonka.FMS.Types.NetworkFirewallMissingExpectedRoutesViolation
import Amazonka.FMS.Types.NetworkFirewallMissingFirewallViolation
import Amazonka.FMS.Types.NetworkFirewallMissingSubnetViolation
import Amazonka.FMS.Types.NetworkFirewallPolicy
import Amazonka.FMS.Types.NetworkFirewallPolicyDescription
import Amazonka.FMS.Types.NetworkFirewallPolicyModifiedViolation
import Amazonka.FMS.Types.NetworkFirewallStatefulRuleGroupOverride
import Amazonka.FMS.Types.NetworkFirewallUnexpectedFirewallRoutesViolation
import Amazonka.FMS.Types.NetworkFirewallUnexpectedGatewayRoutesViolation
import Amazonka.FMS.Types.PartialMatch
import Amazonka.FMS.Types.Policy
import Amazonka.FMS.Types.PolicyComplianceDetail
import Amazonka.FMS.Types.PolicyComplianceStatus
import Amazonka.FMS.Types.PolicyOption
import Amazonka.FMS.Types.PolicySummary
import Amazonka.FMS.Types.PossibleRemediationAction
import Amazonka.FMS.Types.PossibleRemediationActions
import Amazonka.FMS.Types.ProtocolsListData
import Amazonka.FMS.Types.ProtocolsListDataSummary
import Amazonka.FMS.Types.RemediationAction
import Amazonka.FMS.Types.RemediationActionWithOrder
import Amazonka.FMS.Types.Resource
import Amazonka.FMS.Types.ResourceSet
import Amazonka.FMS.Types.ResourceSetSummary
import Amazonka.FMS.Types.ResourceTag
import Amazonka.FMS.Types.ResourceViolation
import Amazonka.FMS.Types.Route
import Amazonka.FMS.Types.RouteHasOutOfScopeEndpointViolation
import Amazonka.FMS.Types.SecurityGroupRemediationAction
import Amazonka.FMS.Types.SecurityGroupRuleDescription
import Amazonka.FMS.Types.SecurityServicePolicyData
import Amazonka.FMS.Types.StatefulEngineOptions
import Amazonka.FMS.Types.StatefulRuleGroup
import Amazonka.FMS.Types.StatelessRuleGroup
import Amazonka.FMS.Types.Tag
import Amazonka.FMS.Types.ThirdPartyFirewallFirewallPolicy
import Amazonka.FMS.Types.ThirdPartyFirewallMissingExpectedRouteTableViolation
import Amazonka.FMS.Types.ThirdPartyFirewallMissingFirewallViolation
import Amazonka.FMS.Types.ThirdPartyFirewallMissingSubnetViolation
import Amazonka.FMS.Types.ThirdPartyFirewallPolicy
import Amazonka.FMS.Types.ViolationDetail
import Amazonka.FMS.UntagResource
