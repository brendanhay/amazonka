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
    getProtectionStatus_nextToken,
    getProtectionStatus_endTime,
    getProtectionStatus_maxResults,
    getProtectionStatus_memberAccountId,
    getProtectionStatus_startTime,
    getProtectionStatus_policyId,
    getProtectionStatusResponse_nextToken,
    getProtectionStatusResponse_serviceType,
    getProtectionStatusResponse_adminAccountId,
    getProtectionStatusResponse_data,
    getProtectionStatusResponse_httpStatus,

    -- ** GetProtocolsList
    getProtocolsList_defaultList,
    getProtocolsList_listId,
    getProtocolsListResponse_protocolsListArn,
    getProtocolsListResponse_protocolsList,
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
    listAppsListsResponse_nextToken,
    listAppsListsResponse_appsLists,
    listAppsListsResponse_httpStatus,

    -- ** ListComplianceStatus
    listComplianceStatus_nextToken,
    listComplianceStatus_maxResults,
    listComplianceStatus_policyId,
    listComplianceStatusResponse_nextToken,
    listComplianceStatusResponse_policyComplianceStatusList,
    listComplianceStatusResponse_httpStatus,

    -- ** ListDiscoveredResources
    listDiscoveredResources_nextToken,
    listDiscoveredResources_maxResults,
    listDiscoveredResources_memberAccountIds,
    listDiscoveredResources_resourceType,
    listDiscoveredResourcesResponse_items,
    listDiscoveredResourcesResponse_nextToken,
    listDiscoveredResourcesResponse_httpStatus,

    -- ** ListMemberAccounts
    listMemberAccounts_nextToken,
    listMemberAccounts_maxResults,
    listMemberAccountsResponse_nextToken,
    listMemberAccountsResponse_memberAccounts,
    listMemberAccountsResponse_httpStatus,

    -- ** ListPolicies
    listPolicies_nextToken,
    listPolicies_maxResults,
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
    listResourceSetResources_nextToken,
    listResourceSetResources_maxResults,
    listResourceSetResources_identifier,
    listResourceSetResourcesResponse_nextToken,
    listResourceSetResourcesResponse_httpStatus,
    listResourceSetResourcesResponse_items,

    -- ** ListResourceSets
    listResourceSets_nextToken,
    listResourceSets_maxResults,
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
    putProtocolsListResponse_protocolsListArn,
    putProtocolsListResponse_protocolsList,
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
    actionTarget_resourceId,
    actionTarget_description,

    -- ** App
    app_appName,
    app_protocol,
    app_port,

    -- ** AppsListData
    appsListData_listId,
    appsListData_listUpdateToken,
    appsListData_previousAppsList,
    appsListData_lastUpdateTime,
    appsListData_createTime,
    appsListData_listName,
    appsListData_appsList,

    -- ** AppsListDataSummary
    appsListDataSummary_listId,
    appsListDataSummary_appsList,
    appsListDataSummary_listName,
    appsListDataSummary_listArn,

    -- ** AwsEc2InstanceViolation
    awsEc2InstanceViolation_awsEc2NetworkInterfaceViolations,
    awsEc2InstanceViolation_violationTarget,

    -- ** AwsEc2NetworkInterfaceViolation
    awsEc2NetworkInterfaceViolation_violationTarget,
    awsEc2NetworkInterfaceViolation_violatingSecurityGroups,

    -- ** AwsVPCSecurityGroupViolation
    awsVPCSecurityGroupViolation_partialMatches,
    awsVPCSecurityGroupViolation_violationTarget,
    awsVPCSecurityGroupViolation_possibleSecurityGroupRemediationActions,
    awsVPCSecurityGroupViolation_violationTargetDescription,

    -- ** ComplianceViolator
    complianceViolator_resourceId,
    complianceViolator_resourceType,
    complianceViolator_metadata,
    complianceViolator_violationReason,

    -- ** DiscoveredResource
    discoveredResource_name,
    discoveredResource_type,
    discoveredResource_uri,
    discoveredResource_accountId,

    -- ** DnsDuplicateRuleGroupViolation
    dnsDuplicateRuleGroupViolation_violationTarget,
    dnsDuplicateRuleGroupViolation_violationTargetDescription,

    -- ** DnsRuleGroupLimitExceededViolation
    dnsRuleGroupLimitExceededViolation_violationTarget,
    dnsRuleGroupLimitExceededViolation_numberOfRuleGroupsAlreadyAssociated,
    dnsRuleGroupLimitExceededViolation_violationTargetDescription,

    -- ** DnsRuleGroupPriorityConflictViolation
    dnsRuleGroupPriorityConflictViolation_unavailablePriorities,
    dnsRuleGroupPriorityConflictViolation_violationTarget,
    dnsRuleGroupPriorityConflictViolation_conflictingPriority,
    dnsRuleGroupPriorityConflictViolation_conflictingPolicyId,
    dnsRuleGroupPriorityConflictViolation_violationTargetDescription,

    -- ** EC2AssociateRouteTableAction
    eC2AssociateRouteTableAction_subnetId,
    eC2AssociateRouteTableAction_description,
    eC2AssociateRouteTableAction_gatewayId,
    eC2AssociateRouteTableAction_routeTableId,

    -- ** EC2CopyRouteTableAction
    eC2CopyRouteTableAction_description,
    eC2CopyRouteTableAction_vpcId,
    eC2CopyRouteTableAction_routeTableId,

    -- ** EC2CreateRouteAction
    eC2CreateRouteAction_destinationPrefixListId,
    eC2CreateRouteAction_description,
    eC2CreateRouteAction_vpcEndpointId,
    eC2CreateRouteAction_destinationCidrBlock,
    eC2CreateRouteAction_gatewayId,
    eC2CreateRouteAction_destinationIpv6CidrBlock,
    eC2CreateRouteAction_routeTableId,

    -- ** EC2CreateRouteTableAction
    eC2CreateRouteTableAction_description,
    eC2CreateRouteTableAction_vpcId,

    -- ** EC2DeleteRouteAction
    eC2DeleteRouteAction_destinationPrefixListId,
    eC2DeleteRouteAction_description,
    eC2DeleteRouteAction_destinationCidrBlock,
    eC2DeleteRouteAction_destinationIpv6CidrBlock,
    eC2DeleteRouteAction_routeTableId,

    -- ** EC2ReplaceRouteAction
    eC2ReplaceRouteAction_destinationPrefixListId,
    eC2ReplaceRouteAction_description,
    eC2ReplaceRouteAction_destinationCidrBlock,
    eC2ReplaceRouteAction_gatewayId,
    eC2ReplaceRouteAction_destinationIpv6CidrBlock,
    eC2ReplaceRouteAction_routeTableId,

    -- ** EC2ReplaceRouteTableAssociationAction
    eC2ReplaceRouteTableAssociationAction_description,
    eC2ReplaceRouteTableAssociationAction_associationId,
    eC2ReplaceRouteTableAssociationAction_routeTableId,

    -- ** EvaluationResult
    evaluationResult_evaluationLimitExceeded,
    evaluationResult_complianceStatus,
    evaluationResult_violatorCount,

    -- ** ExpectedRoute
    expectedRoute_allowedTargets,
    expectedRoute_prefixListId,
    expectedRoute_routeTableId,
    expectedRoute_contributingSubnets,
    expectedRoute_ipV6Cidr,
    expectedRoute_ipV4Cidr,

    -- ** FMSPolicyUpdateFirewallCreationConfigAction
    fMSPolicyUpdateFirewallCreationConfigAction_firewallCreationConfig,
    fMSPolicyUpdateFirewallCreationConfigAction_description,

    -- ** FailedItem
    failedItem_uri,
    failedItem_reason,

    -- ** FirewallSubnetIsOutOfScopeViolation
    firewallSubnetIsOutOfScopeViolation_subnetAvailabilityZoneId,
    firewallSubnetIsOutOfScopeViolation_vpcEndpointId,
    firewallSubnetIsOutOfScopeViolation_subnetAvailabilityZone,
    firewallSubnetIsOutOfScopeViolation_firewallSubnetId,
    firewallSubnetIsOutOfScopeViolation_vpcId,

    -- ** FirewallSubnetMissingVPCEndpointViolation
    firewallSubnetMissingVPCEndpointViolation_subnetAvailabilityZoneId,
    firewallSubnetMissingVPCEndpointViolation_subnetAvailabilityZone,
    firewallSubnetMissingVPCEndpointViolation_firewallSubnetId,
    firewallSubnetMissingVPCEndpointViolation_vpcId,

    -- ** NetworkFirewallBlackHoleRouteDetectedViolation
    networkFirewallBlackHoleRouteDetectedViolation_violatingRoutes,
    networkFirewallBlackHoleRouteDetectedViolation_violationTarget,
    networkFirewallBlackHoleRouteDetectedViolation_routeTableId,
    networkFirewallBlackHoleRouteDetectedViolation_vpcId,

    -- ** NetworkFirewallInternetTrafficNotInspectedViolation
    networkFirewallInternetTrafficNotInspectedViolation_violatingRoutes,
    networkFirewallInternetTrafficNotInspectedViolation_actualInternetGatewayRoutes,
    networkFirewallInternetTrafficNotInspectedViolation_subnetId,
    networkFirewallInternetTrafficNotInspectedViolation_currentFirewallSubnetRouteTable,
    networkFirewallInternetTrafficNotInspectedViolation_expectedFirewallEndpoint,
    networkFirewallInternetTrafficNotInspectedViolation_actualFirewallSubnetRoutes,
    networkFirewallInternetTrafficNotInspectedViolation_expectedInternetGatewayRoutes,
    networkFirewallInternetTrafficNotInspectedViolation_expectedFirewallSubnetRoutes,
    networkFirewallInternetTrafficNotInspectedViolation_currentInternetGatewayRouteTable,
    networkFirewallInternetTrafficNotInspectedViolation_routeTableId,
    networkFirewallInternetTrafficNotInspectedViolation_internetGatewayId,
    networkFirewallInternetTrafficNotInspectedViolation_subnetAvailabilityZone,
    networkFirewallInternetTrafficNotInspectedViolation_firewallSubnetId,
    networkFirewallInternetTrafficNotInspectedViolation_vpcId,
    networkFirewallInternetTrafficNotInspectedViolation_isRouteTableUsedInDifferentAZ,

    -- ** NetworkFirewallInvalidRouteConfigurationViolation
    networkFirewallInvalidRouteConfigurationViolation_actualInternetGatewayRoutes,
    networkFirewallInvalidRouteConfigurationViolation_currentFirewallSubnetRouteTable,
    networkFirewallInvalidRouteConfigurationViolation_violatingRoute,
    networkFirewallInvalidRouteConfigurationViolation_expectedFirewallEndpoint,
    networkFirewallInvalidRouteConfigurationViolation_actualFirewallSubnetRoutes,
    networkFirewallInvalidRouteConfigurationViolation_expectedInternetGatewayRoutes,
    networkFirewallInvalidRouteConfigurationViolation_expectedFirewallSubnetRoutes,
    networkFirewallInvalidRouteConfigurationViolation_currentInternetGatewayRouteTable,
    networkFirewallInvalidRouteConfigurationViolation_routeTableId,
    networkFirewallInvalidRouteConfigurationViolation_internetGatewayId,
    networkFirewallInvalidRouteConfigurationViolation_actualFirewallEndpoint,
    networkFirewallInvalidRouteConfigurationViolation_expectedFirewallSubnetId,
    networkFirewallInvalidRouteConfigurationViolation_vpcId,
    networkFirewallInvalidRouteConfigurationViolation_affectedSubnets,
    networkFirewallInvalidRouteConfigurationViolation_actualFirewallSubnetId,
    networkFirewallInvalidRouteConfigurationViolation_isRouteTableUsedInDifferentAZ,

    -- ** NetworkFirewallMissingExpectedRTViolation
    networkFirewallMissingExpectedRTViolation_vpc,
    networkFirewallMissingExpectedRTViolation_violationTarget,
    networkFirewallMissingExpectedRTViolation_currentRouteTable,
    networkFirewallMissingExpectedRTViolation_availabilityZone,
    networkFirewallMissingExpectedRTViolation_expectedRouteTable,

    -- ** NetworkFirewallMissingExpectedRoutesViolation
    networkFirewallMissingExpectedRoutesViolation_violationTarget,
    networkFirewallMissingExpectedRoutesViolation_expectedRoutes,
    networkFirewallMissingExpectedRoutesViolation_vpcId,

    -- ** NetworkFirewallMissingFirewallViolation
    networkFirewallMissingFirewallViolation_vpc,
    networkFirewallMissingFirewallViolation_violationTarget,
    networkFirewallMissingFirewallViolation_availabilityZone,
    networkFirewallMissingFirewallViolation_targetViolationReason,

    -- ** NetworkFirewallMissingSubnetViolation
    networkFirewallMissingSubnetViolation_vpc,
    networkFirewallMissingSubnetViolation_violationTarget,
    networkFirewallMissingSubnetViolation_availabilityZone,
    networkFirewallMissingSubnetViolation_targetViolationReason,

    -- ** NetworkFirewallPolicy
    networkFirewallPolicy_firewallDeploymentModel,

    -- ** NetworkFirewallPolicyDescription
    networkFirewallPolicyDescription_statefulEngineOptions,
    networkFirewallPolicyDescription_statelessCustomActions,
    networkFirewallPolicyDescription_statefulDefaultActions,
    networkFirewallPolicyDescription_statelessDefaultActions,
    networkFirewallPolicyDescription_statelessRuleGroups,
    networkFirewallPolicyDescription_statelessFragmentDefaultActions,
    networkFirewallPolicyDescription_statefulRuleGroups,

    -- ** NetworkFirewallPolicyModifiedViolation
    networkFirewallPolicyModifiedViolation_violationTarget,
    networkFirewallPolicyModifiedViolation_currentPolicyDescription,
    networkFirewallPolicyModifiedViolation_expectedPolicyDescription,

    -- ** NetworkFirewallStatefulRuleGroupOverride
    networkFirewallStatefulRuleGroupOverride_action,

    -- ** NetworkFirewallUnexpectedFirewallRoutesViolation
    networkFirewallUnexpectedFirewallRoutesViolation_violatingRoutes,
    networkFirewallUnexpectedFirewallRoutesViolation_firewallEndpoint,
    networkFirewallUnexpectedFirewallRoutesViolation_routeTableId,
    networkFirewallUnexpectedFirewallRoutesViolation_firewallSubnetId,
    networkFirewallUnexpectedFirewallRoutesViolation_vpcId,

    -- ** NetworkFirewallUnexpectedGatewayRoutesViolation
    networkFirewallUnexpectedGatewayRoutesViolation_violatingRoutes,
    networkFirewallUnexpectedGatewayRoutesViolation_routeTableId,
    networkFirewallUnexpectedGatewayRoutesViolation_gatewayId,
    networkFirewallUnexpectedGatewayRoutesViolation_vpcId,

    -- ** PartialMatch
    partialMatch_reference,
    partialMatch_targetViolationReasons,

    -- ** Policy
    policy_policyId,
    policy_policyUpdateToken,
    policy_resourceTags,
    policy_excludeMap,
    policy_resourceSetIds,
    policy_deleteUnusedFMManagedResources,
    policy_policyDescription,
    policy_includeMap,
    policy_resourceTypeList,
    policy_policyName,
    policy_securityServicePolicyData,
    policy_resourceType,
    policy_excludeResourceTags,
    policy_remediationEnabled,

    -- ** PolicyComplianceDetail
    policyComplianceDetail_evaluationLimitExceeded,
    policyComplianceDetail_policyId,
    policyComplianceDetail_violators,
    policyComplianceDetail_policyOwner,
    policyComplianceDetail_issueInfoMap,
    policyComplianceDetail_expiredAt,
    policyComplianceDetail_memberAccount,

    -- ** PolicyComplianceStatus
    policyComplianceStatus_policyName,
    policyComplianceStatus_policyId,
    policyComplianceStatus_evaluationResults,
    policyComplianceStatus_policyOwner,
    policyComplianceStatus_lastUpdated,
    policyComplianceStatus_issueInfoMap,
    policyComplianceStatus_memberAccount,

    -- ** PolicyOption
    policyOption_thirdPartyFirewallPolicy,
    policyOption_networkFirewallPolicy,

    -- ** PolicySummary
    policySummary_policyName,
    policySummary_policyId,
    policySummary_resourceType,
    policySummary_securityServiceType,
    policySummary_remediationEnabled,
    policySummary_policyArn,
    policySummary_deleteUnusedFMManagedResources,

    -- ** PossibleRemediationAction
    possibleRemediationAction_description,
    possibleRemediationAction_isDefaultAction,
    possibleRemediationAction_orderedRemediationActions,

    -- ** PossibleRemediationActions
    possibleRemediationActions_description,
    possibleRemediationActions_actions,

    -- ** ProtocolsListData
    protocolsListData_listId,
    protocolsListData_previousProtocolsList,
    protocolsListData_listUpdateToken,
    protocolsListData_lastUpdateTime,
    protocolsListData_createTime,
    protocolsListData_listName,
    protocolsListData_protocolsList,

    -- ** ProtocolsListDataSummary
    protocolsListDataSummary_listId,
    protocolsListDataSummary_listName,
    protocolsListDataSummary_protocolsList,
    protocolsListDataSummary_listArn,

    -- ** RemediationAction
    remediationAction_eC2AssociateRouteTableAction,
    remediationAction_eC2DeleteRouteAction,
    remediationAction_eC2ReplaceRouteAction,
    remediationAction_fMSPolicyUpdateFirewallCreationConfigAction,
    remediationAction_description,
    remediationAction_eC2CreateRouteTableAction,
    remediationAction_eC2CopyRouteTableAction,
    remediationAction_eC2ReplaceRouteTableAssociationAction,
    remediationAction_eC2CreateRouteAction,

    -- ** RemediationActionWithOrder
    remediationActionWithOrder_remediationAction,
    remediationActionWithOrder_order,

    -- ** Resource
    resource_accountId,
    resource_uri,

    -- ** ResourceSet
    resourceSet_updateToken,
    resourceSet_id,
    resourceSet_description,
    resourceSet_lastUpdateTime,
    resourceSet_name,
    resourceSet_resourceTypeList,

    -- ** ResourceSetSummary
    resourceSetSummary_name,
    resourceSetSummary_id,
    resourceSetSummary_description,
    resourceSetSummary_lastUpdateTime,

    -- ** ResourceTag
    resourceTag_value,
    resourceTag_key,

    -- ** ResourceViolation
    resourceViolation_thirdPartyFirewallMissingSubnetViolation,
    resourceViolation_dnsRuleGroupPriorityConflictViolation,
    resourceViolation_thirdPartyFirewallMissingExpectedRouteTableViolation,
    resourceViolation_networkFirewallMissingExpectedRoutesViolation,
    resourceViolation_networkFirewallUnexpectedFirewallRoutesViolation,
    resourceViolation_dnsDuplicateRuleGroupViolation,
    resourceViolation_dnsRuleGroupLimitExceededViolation,
    resourceViolation_networkFirewallMissingFirewallViolation,
    resourceViolation_networkFirewallInternetTrafficNotInspectedViolation,
    resourceViolation_awsEc2InstanceViolation,
    resourceViolation_firewallSubnetMissingVPCEndpointViolation,
    resourceViolation_thirdPartyFirewallMissingFirewallViolation,
    resourceViolation_possibleRemediationActions,
    resourceViolation_networkFirewallMissingSubnetViolation,
    resourceViolation_networkFirewallPolicyModifiedViolation,
    resourceViolation_networkFirewallUnexpectedGatewayRoutesViolation,
    resourceViolation_networkFirewallBlackHoleRouteDetectedViolation,
    resourceViolation_firewallSubnetIsOutOfScopeViolation,
    resourceViolation_routeHasOutOfScopeEndpointViolation,
    resourceViolation_networkFirewallInvalidRouteConfigurationViolation,
    resourceViolation_awsEc2NetworkInterfaceViolation,
    resourceViolation_awsVPCSecurityGroupViolation,
    resourceViolation_networkFirewallMissingExpectedRTViolation,

    -- ** Route
    route_destination,
    route_destinationType,
    route_target,
    route_targetType,

    -- ** RouteHasOutOfScopeEndpointViolation
    routeHasOutOfScopeEndpointViolation_violatingRoutes,
    routeHasOutOfScopeEndpointViolation_subnetAvailabilityZoneId,
    routeHasOutOfScopeEndpointViolation_internetGatewayRoutes,
    routeHasOutOfScopeEndpointViolation_subnetId,
    routeHasOutOfScopeEndpointViolation_currentFirewallSubnetRouteTable,
    routeHasOutOfScopeEndpointViolation_firewallSubnetRoutes,
    routeHasOutOfScopeEndpointViolation_currentInternetGatewayRouteTable,
    routeHasOutOfScopeEndpointViolation_routeTableId,
    routeHasOutOfScopeEndpointViolation_internetGatewayId,
    routeHasOutOfScopeEndpointViolation_subnetAvailabilityZone,
    routeHasOutOfScopeEndpointViolation_firewallSubnetId,
    routeHasOutOfScopeEndpointViolation_vpcId,

    -- ** SecurityGroupRemediationAction
    securityGroupRemediationAction_description,
    securityGroupRemediationAction_isDefaultAction,
    securityGroupRemediationAction_remediationActionType,
    securityGroupRemediationAction_remediationResult,

    -- ** SecurityGroupRuleDescription
    securityGroupRuleDescription_iPV4Range,
    securityGroupRuleDescription_toPort,
    securityGroupRuleDescription_prefixListId,
    securityGroupRuleDescription_iPV6Range,
    securityGroupRuleDescription_protocol,
    securityGroupRuleDescription_fromPort,

    -- ** SecurityServicePolicyData
    securityServicePolicyData_managedServiceData,
    securityServicePolicyData_policyOption,
    securityServicePolicyData_type,

    -- ** StatefulEngineOptions
    statefulEngineOptions_ruleOrder,

    -- ** StatefulRuleGroup
    statefulRuleGroup_resourceId,
    statefulRuleGroup_ruleGroupName,
    statefulRuleGroup_override,
    statefulRuleGroup_priority,

    -- ** StatelessRuleGroup
    statelessRuleGroup_resourceId,
    statelessRuleGroup_ruleGroupName,
    statelessRuleGroup_priority,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** ThirdPartyFirewallFirewallPolicy
    thirdPartyFirewallFirewallPolicy_firewallPolicyName,
    thirdPartyFirewallFirewallPolicy_firewallPolicyId,

    -- ** ThirdPartyFirewallMissingExpectedRouteTableViolation
    thirdPartyFirewallMissingExpectedRouteTableViolation_vpc,
    thirdPartyFirewallMissingExpectedRouteTableViolation_violationTarget,
    thirdPartyFirewallMissingExpectedRouteTableViolation_currentRouteTable,
    thirdPartyFirewallMissingExpectedRouteTableViolation_availabilityZone,
    thirdPartyFirewallMissingExpectedRouteTableViolation_expectedRouteTable,

    -- ** ThirdPartyFirewallMissingFirewallViolation
    thirdPartyFirewallMissingFirewallViolation_vpc,
    thirdPartyFirewallMissingFirewallViolation_violationTarget,
    thirdPartyFirewallMissingFirewallViolation_availabilityZone,
    thirdPartyFirewallMissingFirewallViolation_targetViolationReason,

    -- ** ThirdPartyFirewallMissingSubnetViolation
    thirdPartyFirewallMissingSubnetViolation_vpc,
    thirdPartyFirewallMissingSubnetViolation_violationTarget,
    thirdPartyFirewallMissingSubnetViolation_availabilityZone,
    thirdPartyFirewallMissingSubnetViolation_targetViolationReason,

    -- ** ThirdPartyFirewallPolicy
    thirdPartyFirewallPolicy_firewallDeploymentModel,

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
