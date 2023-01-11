{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.FMS.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InternalErrorException,
    _InvalidInputException,
    _InvalidOperationException,
    _InvalidTypeException,
    _LimitExceededException,
    _ResourceNotFoundException,

    -- * AccountRoleStatus
    AccountRoleStatus (..),

    -- * CustomerPolicyScopeIdType
    CustomerPolicyScopeIdType (..),

    -- * DependentServiceName
    DependentServiceName (..),

    -- * DestinationType
    DestinationType (..),

    -- * FailedItemReason
    FailedItemReason (..),

    -- * FirewallDeploymentModel
    FirewallDeploymentModel (..),

    -- * MarketplaceSubscriptionOnboardingStatus
    MarketplaceSubscriptionOnboardingStatus (..),

    -- * NetworkFirewallOverrideAction
    NetworkFirewallOverrideAction (..),

    -- * PolicyComplianceStatusType
    PolicyComplianceStatusType (..),

    -- * RemediationActionType
    RemediationActionType (..),

    -- * RuleOrder
    RuleOrder (..),

    -- * SecurityServiceType
    SecurityServiceType (..),

    -- * TargetType
    TargetType (..),

    -- * ThirdPartyFirewall
    ThirdPartyFirewall (..),

    -- * ThirdPartyFirewallAssociationStatus
    ThirdPartyFirewallAssociationStatus (..),

    -- * ViolationReason
    ViolationReason (..),

    -- * ActionTarget
    ActionTarget (..),
    newActionTarget,
    actionTarget_description,
    actionTarget_resourceId,

    -- * App
    App (..),
    newApp,
    app_appName,
    app_protocol,
    app_port,

    -- * AppsListData
    AppsListData (..),
    newAppsListData,
    appsListData_createTime,
    appsListData_lastUpdateTime,
    appsListData_listId,
    appsListData_listUpdateToken,
    appsListData_previousAppsList,
    appsListData_listName,
    appsListData_appsList,

    -- * AppsListDataSummary
    AppsListDataSummary (..),
    newAppsListDataSummary,
    appsListDataSummary_appsList,
    appsListDataSummary_listArn,
    appsListDataSummary_listId,
    appsListDataSummary_listName,

    -- * AwsEc2InstanceViolation
    AwsEc2InstanceViolation (..),
    newAwsEc2InstanceViolation,
    awsEc2InstanceViolation_awsEc2NetworkInterfaceViolations,
    awsEc2InstanceViolation_violationTarget,

    -- * AwsEc2NetworkInterfaceViolation
    AwsEc2NetworkInterfaceViolation (..),
    newAwsEc2NetworkInterfaceViolation,
    awsEc2NetworkInterfaceViolation_violatingSecurityGroups,
    awsEc2NetworkInterfaceViolation_violationTarget,

    -- * AwsVPCSecurityGroupViolation
    AwsVPCSecurityGroupViolation (..),
    newAwsVPCSecurityGroupViolation,
    awsVPCSecurityGroupViolation_partialMatches,
    awsVPCSecurityGroupViolation_possibleSecurityGroupRemediationActions,
    awsVPCSecurityGroupViolation_violationTarget,
    awsVPCSecurityGroupViolation_violationTargetDescription,

    -- * ComplianceViolator
    ComplianceViolator (..),
    newComplianceViolator,
    complianceViolator_metadata,
    complianceViolator_resourceId,
    complianceViolator_resourceType,
    complianceViolator_violationReason,

    -- * DiscoveredResource
    DiscoveredResource (..),
    newDiscoveredResource,
    discoveredResource_accountId,
    discoveredResource_name,
    discoveredResource_type,
    discoveredResource_uri,

    -- * DnsDuplicateRuleGroupViolation
    DnsDuplicateRuleGroupViolation (..),
    newDnsDuplicateRuleGroupViolation,
    dnsDuplicateRuleGroupViolation_violationTarget,
    dnsDuplicateRuleGroupViolation_violationTargetDescription,

    -- * DnsRuleGroupLimitExceededViolation
    DnsRuleGroupLimitExceededViolation (..),
    newDnsRuleGroupLimitExceededViolation,
    dnsRuleGroupLimitExceededViolation_numberOfRuleGroupsAlreadyAssociated,
    dnsRuleGroupLimitExceededViolation_violationTarget,
    dnsRuleGroupLimitExceededViolation_violationTargetDescription,

    -- * DnsRuleGroupPriorityConflictViolation
    DnsRuleGroupPriorityConflictViolation (..),
    newDnsRuleGroupPriorityConflictViolation,
    dnsRuleGroupPriorityConflictViolation_conflictingPolicyId,
    dnsRuleGroupPriorityConflictViolation_conflictingPriority,
    dnsRuleGroupPriorityConflictViolation_unavailablePriorities,
    dnsRuleGroupPriorityConflictViolation_violationTarget,
    dnsRuleGroupPriorityConflictViolation_violationTargetDescription,

    -- * EC2AssociateRouteTableAction
    EC2AssociateRouteTableAction (..),
    newEC2AssociateRouteTableAction,
    eC2AssociateRouteTableAction_description,
    eC2AssociateRouteTableAction_gatewayId,
    eC2AssociateRouteTableAction_subnetId,
    eC2AssociateRouteTableAction_routeTableId,

    -- * EC2CopyRouteTableAction
    EC2CopyRouteTableAction (..),
    newEC2CopyRouteTableAction,
    eC2CopyRouteTableAction_description,
    eC2CopyRouteTableAction_vpcId,
    eC2CopyRouteTableAction_routeTableId,

    -- * EC2CreateRouteAction
    EC2CreateRouteAction (..),
    newEC2CreateRouteAction,
    eC2CreateRouteAction_description,
    eC2CreateRouteAction_destinationCidrBlock,
    eC2CreateRouteAction_destinationIpv6CidrBlock,
    eC2CreateRouteAction_destinationPrefixListId,
    eC2CreateRouteAction_gatewayId,
    eC2CreateRouteAction_vpcEndpointId,
    eC2CreateRouteAction_routeTableId,

    -- * EC2CreateRouteTableAction
    EC2CreateRouteTableAction (..),
    newEC2CreateRouteTableAction,
    eC2CreateRouteTableAction_description,
    eC2CreateRouteTableAction_vpcId,

    -- * EC2DeleteRouteAction
    EC2DeleteRouteAction (..),
    newEC2DeleteRouteAction,
    eC2DeleteRouteAction_description,
    eC2DeleteRouteAction_destinationCidrBlock,
    eC2DeleteRouteAction_destinationIpv6CidrBlock,
    eC2DeleteRouteAction_destinationPrefixListId,
    eC2DeleteRouteAction_routeTableId,

    -- * EC2ReplaceRouteAction
    EC2ReplaceRouteAction (..),
    newEC2ReplaceRouteAction,
    eC2ReplaceRouteAction_description,
    eC2ReplaceRouteAction_destinationCidrBlock,
    eC2ReplaceRouteAction_destinationIpv6CidrBlock,
    eC2ReplaceRouteAction_destinationPrefixListId,
    eC2ReplaceRouteAction_gatewayId,
    eC2ReplaceRouteAction_routeTableId,

    -- * EC2ReplaceRouteTableAssociationAction
    EC2ReplaceRouteTableAssociationAction (..),
    newEC2ReplaceRouteTableAssociationAction,
    eC2ReplaceRouteTableAssociationAction_description,
    eC2ReplaceRouteTableAssociationAction_associationId,
    eC2ReplaceRouteTableAssociationAction_routeTableId,

    -- * EvaluationResult
    EvaluationResult (..),
    newEvaluationResult,
    evaluationResult_complianceStatus,
    evaluationResult_evaluationLimitExceeded,
    evaluationResult_violatorCount,

    -- * ExpectedRoute
    ExpectedRoute (..),
    newExpectedRoute,
    expectedRoute_allowedTargets,
    expectedRoute_contributingSubnets,
    expectedRoute_ipV4Cidr,
    expectedRoute_ipV6Cidr,
    expectedRoute_prefixListId,
    expectedRoute_routeTableId,

    -- * FMSPolicyUpdateFirewallCreationConfigAction
    FMSPolicyUpdateFirewallCreationConfigAction (..),
    newFMSPolicyUpdateFirewallCreationConfigAction,
    fMSPolicyUpdateFirewallCreationConfigAction_description,
    fMSPolicyUpdateFirewallCreationConfigAction_firewallCreationConfig,

    -- * FailedItem
    FailedItem (..),
    newFailedItem,
    failedItem_reason,
    failedItem_uri,

    -- * FirewallSubnetIsOutOfScopeViolation
    FirewallSubnetIsOutOfScopeViolation (..),
    newFirewallSubnetIsOutOfScopeViolation,
    firewallSubnetIsOutOfScopeViolation_firewallSubnetId,
    firewallSubnetIsOutOfScopeViolation_subnetAvailabilityZone,
    firewallSubnetIsOutOfScopeViolation_subnetAvailabilityZoneId,
    firewallSubnetIsOutOfScopeViolation_vpcEndpointId,
    firewallSubnetIsOutOfScopeViolation_vpcId,

    -- * FirewallSubnetMissingVPCEndpointViolation
    FirewallSubnetMissingVPCEndpointViolation (..),
    newFirewallSubnetMissingVPCEndpointViolation,
    firewallSubnetMissingVPCEndpointViolation_firewallSubnetId,
    firewallSubnetMissingVPCEndpointViolation_subnetAvailabilityZone,
    firewallSubnetMissingVPCEndpointViolation_subnetAvailabilityZoneId,
    firewallSubnetMissingVPCEndpointViolation_vpcId,

    -- * NetworkFirewallBlackHoleRouteDetectedViolation
    NetworkFirewallBlackHoleRouteDetectedViolation (..),
    newNetworkFirewallBlackHoleRouteDetectedViolation,
    networkFirewallBlackHoleRouteDetectedViolation_routeTableId,
    networkFirewallBlackHoleRouteDetectedViolation_violatingRoutes,
    networkFirewallBlackHoleRouteDetectedViolation_violationTarget,
    networkFirewallBlackHoleRouteDetectedViolation_vpcId,

    -- * NetworkFirewallInternetTrafficNotInspectedViolation
    NetworkFirewallInternetTrafficNotInspectedViolation (..),
    newNetworkFirewallInternetTrafficNotInspectedViolation,
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

    -- * NetworkFirewallInvalidRouteConfigurationViolation
    NetworkFirewallInvalidRouteConfigurationViolation (..),
    newNetworkFirewallInvalidRouteConfigurationViolation,
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

    -- * NetworkFirewallMissingExpectedRTViolation
    NetworkFirewallMissingExpectedRTViolation (..),
    newNetworkFirewallMissingExpectedRTViolation,
    networkFirewallMissingExpectedRTViolation_availabilityZone,
    networkFirewallMissingExpectedRTViolation_currentRouteTable,
    networkFirewallMissingExpectedRTViolation_expectedRouteTable,
    networkFirewallMissingExpectedRTViolation_vpc,
    networkFirewallMissingExpectedRTViolation_violationTarget,

    -- * NetworkFirewallMissingExpectedRoutesViolation
    NetworkFirewallMissingExpectedRoutesViolation (..),
    newNetworkFirewallMissingExpectedRoutesViolation,
    networkFirewallMissingExpectedRoutesViolation_expectedRoutes,
    networkFirewallMissingExpectedRoutesViolation_violationTarget,
    networkFirewallMissingExpectedRoutesViolation_vpcId,

    -- * NetworkFirewallMissingFirewallViolation
    NetworkFirewallMissingFirewallViolation (..),
    newNetworkFirewallMissingFirewallViolation,
    networkFirewallMissingFirewallViolation_availabilityZone,
    networkFirewallMissingFirewallViolation_targetViolationReason,
    networkFirewallMissingFirewallViolation_vpc,
    networkFirewallMissingFirewallViolation_violationTarget,

    -- * NetworkFirewallMissingSubnetViolation
    NetworkFirewallMissingSubnetViolation (..),
    newNetworkFirewallMissingSubnetViolation,
    networkFirewallMissingSubnetViolation_availabilityZone,
    networkFirewallMissingSubnetViolation_targetViolationReason,
    networkFirewallMissingSubnetViolation_vpc,
    networkFirewallMissingSubnetViolation_violationTarget,

    -- * NetworkFirewallPolicy
    NetworkFirewallPolicy (..),
    newNetworkFirewallPolicy,
    networkFirewallPolicy_firewallDeploymentModel,

    -- * NetworkFirewallPolicyDescription
    NetworkFirewallPolicyDescription (..),
    newNetworkFirewallPolicyDescription,
    networkFirewallPolicyDescription_statefulDefaultActions,
    networkFirewallPolicyDescription_statefulEngineOptions,
    networkFirewallPolicyDescription_statefulRuleGroups,
    networkFirewallPolicyDescription_statelessCustomActions,
    networkFirewallPolicyDescription_statelessDefaultActions,
    networkFirewallPolicyDescription_statelessFragmentDefaultActions,
    networkFirewallPolicyDescription_statelessRuleGroups,

    -- * NetworkFirewallPolicyModifiedViolation
    NetworkFirewallPolicyModifiedViolation (..),
    newNetworkFirewallPolicyModifiedViolation,
    networkFirewallPolicyModifiedViolation_currentPolicyDescription,
    networkFirewallPolicyModifiedViolation_expectedPolicyDescription,
    networkFirewallPolicyModifiedViolation_violationTarget,

    -- * NetworkFirewallStatefulRuleGroupOverride
    NetworkFirewallStatefulRuleGroupOverride (..),
    newNetworkFirewallStatefulRuleGroupOverride,
    networkFirewallStatefulRuleGroupOverride_action,

    -- * NetworkFirewallUnexpectedFirewallRoutesViolation
    NetworkFirewallUnexpectedFirewallRoutesViolation (..),
    newNetworkFirewallUnexpectedFirewallRoutesViolation,
    networkFirewallUnexpectedFirewallRoutesViolation_firewallEndpoint,
    networkFirewallUnexpectedFirewallRoutesViolation_firewallSubnetId,
    networkFirewallUnexpectedFirewallRoutesViolation_routeTableId,
    networkFirewallUnexpectedFirewallRoutesViolation_violatingRoutes,
    networkFirewallUnexpectedFirewallRoutesViolation_vpcId,

    -- * NetworkFirewallUnexpectedGatewayRoutesViolation
    NetworkFirewallUnexpectedGatewayRoutesViolation (..),
    newNetworkFirewallUnexpectedGatewayRoutesViolation,
    networkFirewallUnexpectedGatewayRoutesViolation_gatewayId,
    networkFirewallUnexpectedGatewayRoutesViolation_routeTableId,
    networkFirewallUnexpectedGatewayRoutesViolation_violatingRoutes,
    networkFirewallUnexpectedGatewayRoutesViolation_vpcId,

    -- * PartialMatch
    PartialMatch (..),
    newPartialMatch,
    partialMatch_reference,
    partialMatch_targetViolationReasons,

    -- * Policy
    Policy (..),
    newPolicy,
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

    -- * PolicyComplianceDetail
    PolicyComplianceDetail (..),
    newPolicyComplianceDetail,
    policyComplianceDetail_evaluationLimitExceeded,
    policyComplianceDetail_expiredAt,
    policyComplianceDetail_issueInfoMap,
    policyComplianceDetail_memberAccount,
    policyComplianceDetail_policyId,
    policyComplianceDetail_policyOwner,
    policyComplianceDetail_violators,

    -- * PolicyComplianceStatus
    PolicyComplianceStatus (..),
    newPolicyComplianceStatus,
    policyComplianceStatus_evaluationResults,
    policyComplianceStatus_issueInfoMap,
    policyComplianceStatus_lastUpdated,
    policyComplianceStatus_memberAccount,
    policyComplianceStatus_policyId,
    policyComplianceStatus_policyName,
    policyComplianceStatus_policyOwner,

    -- * PolicyOption
    PolicyOption (..),
    newPolicyOption,
    policyOption_networkFirewallPolicy,
    policyOption_thirdPartyFirewallPolicy,

    -- * PolicySummary
    PolicySummary (..),
    newPolicySummary,
    policySummary_deleteUnusedFMManagedResources,
    policySummary_policyArn,
    policySummary_policyId,
    policySummary_policyName,
    policySummary_remediationEnabled,
    policySummary_resourceType,
    policySummary_securityServiceType,

    -- * PossibleRemediationAction
    PossibleRemediationAction (..),
    newPossibleRemediationAction,
    possibleRemediationAction_description,
    possibleRemediationAction_isDefaultAction,
    possibleRemediationAction_orderedRemediationActions,

    -- * PossibleRemediationActions
    PossibleRemediationActions (..),
    newPossibleRemediationActions,
    possibleRemediationActions_actions,
    possibleRemediationActions_description,

    -- * ProtocolsListData
    ProtocolsListData (..),
    newProtocolsListData,
    protocolsListData_createTime,
    protocolsListData_lastUpdateTime,
    protocolsListData_listId,
    protocolsListData_listUpdateToken,
    protocolsListData_previousProtocolsList,
    protocolsListData_listName,
    protocolsListData_protocolsList,

    -- * ProtocolsListDataSummary
    ProtocolsListDataSummary (..),
    newProtocolsListDataSummary,
    protocolsListDataSummary_listArn,
    protocolsListDataSummary_listId,
    protocolsListDataSummary_listName,
    protocolsListDataSummary_protocolsList,

    -- * RemediationAction
    RemediationAction (..),
    newRemediationAction,
    remediationAction_description,
    remediationAction_eC2AssociateRouteTableAction,
    remediationAction_eC2CopyRouteTableAction,
    remediationAction_eC2CreateRouteAction,
    remediationAction_eC2CreateRouteTableAction,
    remediationAction_eC2DeleteRouteAction,
    remediationAction_eC2ReplaceRouteAction,
    remediationAction_eC2ReplaceRouteTableAssociationAction,
    remediationAction_fMSPolicyUpdateFirewallCreationConfigAction,

    -- * RemediationActionWithOrder
    RemediationActionWithOrder (..),
    newRemediationActionWithOrder,
    remediationActionWithOrder_order,
    remediationActionWithOrder_remediationAction,

    -- * Resource
    Resource (..),
    newResource,
    resource_accountId,
    resource_uri,

    -- * ResourceSet
    ResourceSet (..),
    newResourceSet,
    resourceSet_description,
    resourceSet_id,
    resourceSet_lastUpdateTime,
    resourceSet_updateToken,
    resourceSet_name,
    resourceSet_resourceTypeList,

    -- * ResourceSetSummary
    ResourceSetSummary (..),
    newResourceSetSummary,
    resourceSetSummary_description,
    resourceSetSummary_id,
    resourceSetSummary_lastUpdateTime,
    resourceSetSummary_name,

    -- * ResourceTag
    ResourceTag (..),
    newResourceTag,
    resourceTag_value,
    resourceTag_key,

    -- * ResourceViolation
    ResourceViolation (..),
    newResourceViolation,
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

    -- * Route
    Route (..),
    newRoute,
    route_destination,
    route_destinationType,
    route_target,
    route_targetType,

    -- * RouteHasOutOfScopeEndpointViolation
    RouteHasOutOfScopeEndpointViolation (..),
    newRouteHasOutOfScopeEndpointViolation,
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

    -- * SecurityGroupRemediationAction
    SecurityGroupRemediationAction (..),
    newSecurityGroupRemediationAction,
    securityGroupRemediationAction_description,
    securityGroupRemediationAction_isDefaultAction,
    securityGroupRemediationAction_remediationActionType,
    securityGroupRemediationAction_remediationResult,

    -- * SecurityGroupRuleDescription
    SecurityGroupRuleDescription (..),
    newSecurityGroupRuleDescription,
    securityGroupRuleDescription_fromPort,
    securityGroupRuleDescription_iPV4Range,
    securityGroupRuleDescription_iPV6Range,
    securityGroupRuleDescription_prefixListId,
    securityGroupRuleDescription_protocol,
    securityGroupRuleDescription_toPort,

    -- * SecurityServicePolicyData
    SecurityServicePolicyData (..),
    newSecurityServicePolicyData,
    securityServicePolicyData_managedServiceData,
    securityServicePolicyData_policyOption,
    securityServicePolicyData_type,

    -- * StatefulEngineOptions
    StatefulEngineOptions (..),
    newStatefulEngineOptions,
    statefulEngineOptions_ruleOrder,

    -- * StatefulRuleGroup
    StatefulRuleGroup (..),
    newStatefulRuleGroup,
    statefulRuleGroup_override,
    statefulRuleGroup_priority,
    statefulRuleGroup_resourceId,
    statefulRuleGroup_ruleGroupName,

    -- * StatelessRuleGroup
    StatelessRuleGroup (..),
    newStatelessRuleGroup,
    statelessRuleGroup_priority,
    statelessRuleGroup_resourceId,
    statelessRuleGroup_ruleGroupName,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * ThirdPartyFirewallFirewallPolicy
    ThirdPartyFirewallFirewallPolicy (..),
    newThirdPartyFirewallFirewallPolicy,
    thirdPartyFirewallFirewallPolicy_firewallPolicyId,
    thirdPartyFirewallFirewallPolicy_firewallPolicyName,

    -- * ThirdPartyFirewallMissingExpectedRouteTableViolation
    ThirdPartyFirewallMissingExpectedRouteTableViolation (..),
    newThirdPartyFirewallMissingExpectedRouteTableViolation,
    thirdPartyFirewallMissingExpectedRouteTableViolation_availabilityZone,
    thirdPartyFirewallMissingExpectedRouteTableViolation_currentRouteTable,
    thirdPartyFirewallMissingExpectedRouteTableViolation_expectedRouteTable,
    thirdPartyFirewallMissingExpectedRouteTableViolation_vpc,
    thirdPartyFirewallMissingExpectedRouteTableViolation_violationTarget,

    -- * ThirdPartyFirewallMissingFirewallViolation
    ThirdPartyFirewallMissingFirewallViolation (..),
    newThirdPartyFirewallMissingFirewallViolation,
    thirdPartyFirewallMissingFirewallViolation_availabilityZone,
    thirdPartyFirewallMissingFirewallViolation_targetViolationReason,
    thirdPartyFirewallMissingFirewallViolation_vpc,
    thirdPartyFirewallMissingFirewallViolation_violationTarget,

    -- * ThirdPartyFirewallMissingSubnetViolation
    ThirdPartyFirewallMissingSubnetViolation (..),
    newThirdPartyFirewallMissingSubnetViolation,
    thirdPartyFirewallMissingSubnetViolation_availabilityZone,
    thirdPartyFirewallMissingSubnetViolation_targetViolationReason,
    thirdPartyFirewallMissingSubnetViolation_vpc,
    thirdPartyFirewallMissingSubnetViolation_violationTarget,

    -- * ThirdPartyFirewallPolicy
    ThirdPartyFirewallPolicy (..),
    newThirdPartyFirewallPolicy,
    thirdPartyFirewallPolicy_firewallDeploymentModel,

    -- * ViolationDetail
    ViolationDetail (..),
    newViolationDetail,
    violationDetail_resourceDescription,
    violationDetail_resourceTags,
    violationDetail_policyId,
    violationDetail_memberAccount,
    violationDetail_resourceId,
    violationDetail_resourceType,
    violationDetail_resourceViolations,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.FMS.Types.AccountRoleStatus
import Amazonka.FMS.Types.ActionTarget
import Amazonka.FMS.Types.App
import Amazonka.FMS.Types.AppsListData
import Amazonka.FMS.Types.AppsListDataSummary
import Amazonka.FMS.Types.AwsEc2InstanceViolation
import Amazonka.FMS.Types.AwsEc2NetworkInterfaceViolation
import Amazonka.FMS.Types.AwsVPCSecurityGroupViolation
import Amazonka.FMS.Types.ComplianceViolator
import Amazonka.FMS.Types.CustomerPolicyScopeIdType
import Amazonka.FMS.Types.DependentServiceName
import Amazonka.FMS.Types.DestinationType
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
import Amazonka.FMS.Types.FailedItemReason
import Amazonka.FMS.Types.FirewallDeploymentModel
import Amazonka.FMS.Types.FirewallSubnetIsOutOfScopeViolation
import Amazonka.FMS.Types.FirewallSubnetMissingVPCEndpointViolation
import Amazonka.FMS.Types.MarketplaceSubscriptionOnboardingStatus
import Amazonka.FMS.Types.NetworkFirewallBlackHoleRouteDetectedViolation
import Amazonka.FMS.Types.NetworkFirewallInternetTrafficNotInspectedViolation
import Amazonka.FMS.Types.NetworkFirewallInvalidRouteConfigurationViolation
import Amazonka.FMS.Types.NetworkFirewallMissingExpectedRTViolation
import Amazonka.FMS.Types.NetworkFirewallMissingExpectedRoutesViolation
import Amazonka.FMS.Types.NetworkFirewallMissingFirewallViolation
import Amazonka.FMS.Types.NetworkFirewallMissingSubnetViolation
import Amazonka.FMS.Types.NetworkFirewallOverrideAction
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
import Amazonka.FMS.Types.PolicyComplianceStatusType
import Amazonka.FMS.Types.PolicyOption
import Amazonka.FMS.Types.PolicySummary
import Amazonka.FMS.Types.PossibleRemediationAction
import Amazonka.FMS.Types.PossibleRemediationActions
import Amazonka.FMS.Types.ProtocolsListData
import Amazonka.FMS.Types.ProtocolsListDataSummary
import Amazonka.FMS.Types.RemediationAction
import Amazonka.FMS.Types.RemediationActionType
import Amazonka.FMS.Types.RemediationActionWithOrder
import Amazonka.FMS.Types.Resource
import Amazonka.FMS.Types.ResourceSet
import Amazonka.FMS.Types.ResourceSetSummary
import Amazonka.FMS.Types.ResourceTag
import Amazonka.FMS.Types.ResourceViolation
import Amazonka.FMS.Types.Route
import Amazonka.FMS.Types.RouteHasOutOfScopeEndpointViolation
import Amazonka.FMS.Types.RuleOrder
import Amazonka.FMS.Types.SecurityGroupRemediationAction
import Amazonka.FMS.Types.SecurityGroupRuleDescription
import Amazonka.FMS.Types.SecurityServicePolicyData
import Amazonka.FMS.Types.SecurityServiceType
import Amazonka.FMS.Types.StatefulEngineOptions
import Amazonka.FMS.Types.StatefulRuleGroup
import Amazonka.FMS.Types.StatelessRuleGroup
import Amazonka.FMS.Types.Tag
import Amazonka.FMS.Types.TargetType
import Amazonka.FMS.Types.ThirdPartyFirewall
import Amazonka.FMS.Types.ThirdPartyFirewallAssociationStatus
import Amazonka.FMS.Types.ThirdPartyFirewallFirewallPolicy
import Amazonka.FMS.Types.ThirdPartyFirewallMissingExpectedRouteTableViolation
import Amazonka.FMS.Types.ThirdPartyFirewallMissingFirewallViolation
import Amazonka.FMS.Types.ThirdPartyFirewallMissingSubnetViolation
import Amazonka.FMS.Types.ThirdPartyFirewallPolicy
import Amazonka.FMS.Types.ViolationDetail
import Amazonka.FMS.Types.ViolationReason
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2018-01-01@ of the Amazon Firewall Management Service SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "FMS",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "fms",
      Core.signingName = "fms",
      Core.version = "2018-01-01",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "FMS",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | The operation failed because of a system problem, even though the
-- request was valid. Retry your request.
_InternalErrorException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InternalErrorException =
  Core._MatchServiceError
    defaultService
    "InternalErrorException"

-- | The parameters of the request were invalid.
_InvalidInputException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidInputException =
  Core._MatchServiceError
    defaultService
    "InvalidInputException"

-- | The operation failed because there was nothing to do or the operation
-- wasn\'t possible. For example, you might have submitted an
-- @AssociateAdminAccount@ request for an account ID that was already set
-- as the Firewall Manager administrator. Or you might have tried to access
-- a Region that\'s disabled by default, and that you need to enable for
-- the Firewall Manager administrator account and for Organizations before
-- you can access it.
_InvalidOperationException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidOperationException =
  Core._MatchServiceError
    defaultService
    "InvalidOperationException"

-- | The value of the @Type@ parameter is invalid.
_InvalidTypeException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidTypeException =
  Core._MatchServiceError
    defaultService
    "InvalidTypeException"

-- | The operation exceeds a resource limit, for example, the maximum number
-- of @policy@ objects that you can create for an Amazon Web Services
-- account. For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/fms-limits.html Firewall Manager Limits>
-- in the /WAF Developer Guide/.
_LimitExceededException :: Core.AsError a => Lens.Fold a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | The specified resource was not found.
_ResourceNotFoundException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
