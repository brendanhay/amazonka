{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.FMS.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InvalidInputException,
    _InvalidOperationException,
    _InternalErrorException,
    _ResourceNotFoundException,
    _InvalidTypeException,
    _LimitExceededException,

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
    actionTarget_resourceId,
    actionTarget_description,

    -- * App
    App (..),
    newApp,
    app_appName,
    app_protocol,
    app_port,

    -- * AppsListData
    AppsListData (..),
    newAppsListData,
    appsListData_listId,
    appsListData_listUpdateToken,
    appsListData_previousAppsList,
    appsListData_lastUpdateTime,
    appsListData_createTime,
    appsListData_listName,
    appsListData_appsList,

    -- * AppsListDataSummary
    AppsListDataSummary (..),
    newAppsListDataSummary,
    appsListDataSummary_listId,
    appsListDataSummary_appsList,
    appsListDataSummary_listName,
    appsListDataSummary_listArn,

    -- * AwsEc2InstanceViolation
    AwsEc2InstanceViolation (..),
    newAwsEc2InstanceViolation,
    awsEc2InstanceViolation_awsEc2NetworkInterfaceViolations,
    awsEc2InstanceViolation_violationTarget,

    -- * AwsEc2NetworkInterfaceViolation
    AwsEc2NetworkInterfaceViolation (..),
    newAwsEc2NetworkInterfaceViolation,
    awsEc2NetworkInterfaceViolation_violationTarget,
    awsEc2NetworkInterfaceViolation_violatingSecurityGroups,

    -- * AwsVPCSecurityGroupViolation
    AwsVPCSecurityGroupViolation (..),
    newAwsVPCSecurityGroupViolation,
    awsVPCSecurityGroupViolation_partialMatches,
    awsVPCSecurityGroupViolation_violationTarget,
    awsVPCSecurityGroupViolation_possibleSecurityGroupRemediationActions,
    awsVPCSecurityGroupViolation_violationTargetDescription,

    -- * ComplianceViolator
    ComplianceViolator (..),
    newComplianceViolator,
    complianceViolator_resourceId,
    complianceViolator_resourceType,
    complianceViolator_metadata,
    complianceViolator_violationReason,

    -- * DiscoveredResource
    DiscoveredResource (..),
    newDiscoveredResource,
    discoveredResource_name,
    discoveredResource_type,
    discoveredResource_uri,
    discoveredResource_accountId,

    -- * DnsDuplicateRuleGroupViolation
    DnsDuplicateRuleGroupViolation (..),
    newDnsDuplicateRuleGroupViolation,
    dnsDuplicateRuleGroupViolation_violationTarget,
    dnsDuplicateRuleGroupViolation_violationTargetDescription,

    -- * DnsRuleGroupLimitExceededViolation
    DnsRuleGroupLimitExceededViolation (..),
    newDnsRuleGroupLimitExceededViolation,
    dnsRuleGroupLimitExceededViolation_violationTarget,
    dnsRuleGroupLimitExceededViolation_numberOfRuleGroupsAlreadyAssociated,
    dnsRuleGroupLimitExceededViolation_violationTargetDescription,

    -- * DnsRuleGroupPriorityConflictViolation
    DnsRuleGroupPriorityConflictViolation (..),
    newDnsRuleGroupPriorityConflictViolation,
    dnsRuleGroupPriorityConflictViolation_unavailablePriorities,
    dnsRuleGroupPriorityConflictViolation_violationTarget,
    dnsRuleGroupPriorityConflictViolation_conflictingPriority,
    dnsRuleGroupPriorityConflictViolation_conflictingPolicyId,
    dnsRuleGroupPriorityConflictViolation_violationTargetDescription,

    -- * EC2AssociateRouteTableAction
    EC2AssociateRouteTableAction (..),
    newEC2AssociateRouteTableAction,
    eC2AssociateRouteTableAction_subnetId,
    eC2AssociateRouteTableAction_description,
    eC2AssociateRouteTableAction_gatewayId,
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
    eC2CreateRouteAction_destinationPrefixListId,
    eC2CreateRouteAction_description,
    eC2CreateRouteAction_vpcEndpointId,
    eC2CreateRouteAction_destinationCidrBlock,
    eC2CreateRouteAction_gatewayId,
    eC2CreateRouteAction_destinationIpv6CidrBlock,
    eC2CreateRouteAction_routeTableId,

    -- * EC2CreateRouteTableAction
    EC2CreateRouteTableAction (..),
    newEC2CreateRouteTableAction,
    eC2CreateRouteTableAction_description,
    eC2CreateRouteTableAction_vpcId,

    -- * EC2DeleteRouteAction
    EC2DeleteRouteAction (..),
    newEC2DeleteRouteAction,
    eC2DeleteRouteAction_destinationPrefixListId,
    eC2DeleteRouteAction_description,
    eC2DeleteRouteAction_destinationCidrBlock,
    eC2DeleteRouteAction_destinationIpv6CidrBlock,
    eC2DeleteRouteAction_routeTableId,

    -- * EC2ReplaceRouteAction
    EC2ReplaceRouteAction (..),
    newEC2ReplaceRouteAction,
    eC2ReplaceRouteAction_destinationPrefixListId,
    eC2ReplaceRouteAction_description,
    eC2ReplaceRouteAction_destinationCidrBlock,
    eC2ReplaceRouteAction_gatewayId,
    eC2ReplaceRouteAction_destinationIpv6CidrBlock,
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
    evaluationResult_evaluationLimitExceeded,
    evaluationResult_complianceStatus,
    evaluationResult_violatorCount,

    -- * ExpectedRoute
    ExpectedRoute (..),
    newExpectedRoute,
    expectedRoute_allowedTargets,
    expectedRoute_prefixListId,
    expectedRoute_routeTableId,
    expectedRoute_contributingSubnets,
    expectedRoute_ipV6Cidr,
    expectedRoute_ipV4Cidr,

    -- * FMSPolicyUpdateFirewallCreationConfigAction
    FMSPolicyUpdateFirewallCreationConfigAction (..),
    newFMSPolicyUpdateFirewallCreationConfigAction,
    fMSPolicyUpdateFirewallCreationConfigAction_firewallCreationConfig,
    fMSPolicyUpdateFirewallCreationConfigAction_description,

    -- * FailedItem
    FailedItem (..),
    newFailedItem,
    failedItem_uri,
    failedItem_reason,

    -- * FirewallSubnetIsOutOfScopeViolation
    FirewallSubnetIsOutOfScopeViolation (..),
    newFirewallSubnetIsOutOfScopeViolation,
    firewallSubnetIsOutOfScopeViolation_subnetAvailabilityZoneId,
    firewallSubnetIsOutOfScopeViolation_vpcEndpointId,
    firewallSubnetIsOutOfScopeViolation_subnetAvailabilityZone,
    firewallSubnetIsOutOfScopeViolation_firewallSubnetId,
    firewallSubnetIsOutOfScopeViolation_vpcId,

    -- * FirewallSubnetMissingVPCEndpointViolation
    FirewallSubnetMissingVPCEndpointViolation (..),
    newFirewallSubnetMissingVPCEndpointViolation,
    firewallSubnetMissingVPCEndpointViolation_subnetAvailabilityZoneId,
    firewallSubnetMissingVPCEndpointViolation_subnetAvailabilityZone,
    firewallSubnetMissingVPCEndpointViolation_firewallSubnetId,
    firewallSubnetMissingVPCEndpointViolation_vpcId,

    -- * NetworkFirewallBlackHoleRouteDetectedViolation
    NetworkFirewallBlackHoleRouteDetectedViolation (..),
    newNetworkFirewallBlackHoleRouteDetectedViolation,
    networkFirewallBlackHoleRouteDetectedViolation_violatingRoutes,
    networkFirewallBlackHoleRouteDetectedViolation_violationTarget,
    networkFirewallBlackHoleRouteDetectedViolation_routeTableId,
    networkFirewallBlackHoleRouteDetectedViolation_vpcId,

    -- * NetworkFirewallInternetTrafficNotInspectedViolation
    NetworkFirewallInternetTrafficNotInspectedViolation (..),
    newNetworkFirewallInternetTrafficNotInspectedViolation,
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

    -- * NetworkFirewallInvalidRouteConfigurationViolation
    NetworkFirewallInvalidRouteConfigurationViolation (..),
    newNetworkFirewallInvalidRouteConfigurationViolation,
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

    -- * NetworkFirewallMissingExpectedRTViolation
    NetworkFirewallMissingExpectedRTViolation (..),
    newNetworkFirewallMissingExpectedRTViolation,
    networkFirewallMissingExpectedRTViolation_vpc,
    networkFirewallMissingExpectedRTViolation_violationTarget,
    networkFirewallMissingExpectedRTViolation_currentRouteTable,
    networkFirewallMissingExpectedRTViolation_availabilityZone,
    networkFirewallMissingExpectedRTViolation_expectedRouteTable,

    -- * NetworkFirewallMissingExpectedRoutesViolation
    NetworkFirewallMissingExpectedRoutesViolation (..),
    newNetworkFirewallMissingExpectedRoutesViolation,
    networkFirewallMissingExpectedRoutesViolation_violationTarget,
    networkFirewallMissingExpectedRoutesViolation_expectedRoutes,
    networkFirewallMissingExpectedRoutesViolation_vpcId,

    -- * NetworkFirewallMissingFirewallViolation
    NetworkFirewallMissingFirewallViolation (..),
    newNetworkFirewallMissingFirewallViolation,
    networkFirewallMissingFirewallViolation_vpc,
    networkFirewallMissingFirewallViolation_violationTarget,
    networkFirewallMissingFirewallViolation_availabilityZone,
    networkFirewallMissingFirewallViolation_targetViolationReason,

    -- * NetworkFirewallMissingSubnetViolation
    NetworkFirewallMissingSubnetViolation (..),
    newNetworkFirewallMissingSubnetViolation,
    networkFirewallMissingSubnetViolation_vpc,
    networkFirewallMissingSubnetViolation_violationTarget,
    networkFirewallMissingSubnetViolation_availabilityZone,
    networkFirewallMissingSubnetViolation_targetViolationReason,

    -- * NetworkFirewallPolicy
    NetworkFirewallPolicy (..),
    newNetworkFirewallPolicy,
    networkFirewallPolicy_firewallDeploymentModel,

    -- * NetworkFirewallPolicyDescription
    NetworkFirewallPolicyDescription (..),
    newNetworkFirewallPolicyDescription,
    networkFirewallPolicyDescription_statefulEngineOptions,
    networkFirewallPolicyDescription_statelessCustomActions,
    networkFirewallPolicyDescription_statefulDefaultActions,
    networkFirewallPolicyDescription_statelessDefaultActions,
    networkFirewallPolicyDescription_statelessRuleGroups,
    networkFirewallPolicyDescription_statelessFragmentDefaultActions,
    networkFirewallPolicyDescription_statefulRuleGroups,

    -- * NetworkFirewallPolicyModifiedViolation
    NetworkFirewallPolicyModifiedViolation (..),
    newNetworkFirewallPolicyModifiedViolation,
    networkFirewallPolicyModifiedViolation_violationTarget,
    networkFirewallPolicyModifiedViolation_currentPolicyDescription,
    networkFirewallPolicyModifiedViolation_expectedPolicyDescription,

    -- * NetworkFirewallStatefulRuleGroupOverride
    NetworkFirewallStatefulRuleGroupOverride (..),
    newNetworkFirewallStatefulRuleGroupOverride,
    networkFirewallStatefulRuleGroupOverride_action,

    -- * NetworkFirewallUnexpectedFirewallRoutesViolation
    NetworkFirewallUnexpectedFirewallRoutesViolation (..),
    newNetworkFirewallUnexpectedFirewallRoutesViolation,
    networkFirewallUnexpectedFirewallRoutesViolation_violatingRoutes,
    networkFirewallUnexpectedFirewallRoutesViolation_firewallEndpoint,
    networkFirewallUnexpectedFirewallRoutesViolation_routeTableId,
    networkFirewallUnexpectedFirewallRoutesViolation_firewallSubnetId,
    networkFirewallUnexpectedFirewallRoutesViolation_vpcId,

    -- * NetworkFirewallUnexpectedGatewayRoutesViolation
    NetworkFirewallUnexpectedGatewayRoutesViolation (..),
    newNetworkFirewallUnexpectedGatewayRoutesViolation,
    networkFirewallUnexpectedGatewayRoutesViolation_violatingRoutes,
    networkFirewallUnexpectedGatewayRoutesViolation_routeTableId,
    networkFirewallUnexpectedGatewayRoutesViolation_gatewayId,
    networkFirewallUnexpectedGatewayRoutesViolation_vpcId,

    -- * PartialMatch
    PartialMatch (..),
    newPartialMatch,
    partialMatch_reference,
    partialMatch_targetViolationReasons,

    -- * Policy
    Policy (..),
    newPolicy,
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

    -- * PolicyComplianceDetail
    PolicyComplianceDetail (..),
    newPolicyComplianceDetail,
    policyComplianceDetail_evaluationLimitExceeded,
    policyComplianceDetail_policyId,
    policyComplianceDetail_violators,
    policyComplianceDetail_policyOwner,
    policyComplianceDetail_issueInfoMap,
    policyComplianceDetail_expiredAt,
    policyComplianceDetail_memberAccount,

    -- * PolicyComplianceStatus
    PolicyComplianceStatus (..),
    newPolicyComplianceStatus,
    policyComplianceStatus_policyName,
    policyComplianceStatus_policyId,
    policyComplianceStatus_evaluationResults,
    policyComplianceStatus_policyOwner,
    policyComplianceStatus_lastUpdated,
    policyComplianceStatus_issueInfoMap,
    policyComplianceStatus_memberAccount,

    -- * PolicyOption
    PolicyOption (..),
    newPolicyOption,
    policyOption_thirdPartyFirewallPolicy,
    policyOption_networkFirewallPolicy,

    -- * PolicySummary
    PolicySummary (..),
    newPolicySummary,
    policySummary_policyName,
    policySummary_policyId,
    policySummary_resourceType,
    policySummary_securityServiceType,
    policySummary_remediationEnabled,
    policySummary_policyArn,
    policySummary_deleteUnusedFMManagedResources,

    -- * PossibleRemediationAction
    PossibleRemediationAction (..),
    newPossibleRemediationAction,
    possibleRemediationAction_description,
    possibleRemediationAction_isDefaultAction,
    possibleRemediationAction_orderedRemediationActions,

    -- * PossibleRemediationActions
    PossibleRemediationActions (..),
    newPossibleRemediationActions,
    possibleRemediationActions_description,
    possibleRemediationActions_actions,

    -- * ProtocolsListData
    ProtocolsListData (..),
    newProtocolsListData,
    protocolsListData_listId,
    protocolsListData_previousProtocolsList,
    protocolsListData_listUpdateToken,
    protocolsListData_lastUpdateTime,
    protocolsListData_createTime,
    protocolsListData_listName,
    protocolsListData_protocolsList,

    -- * ProtocolsListDataSummary
    ProtocolsListDataSummary (..),
    newProtocolsListDataSummary,
    protocolsListDataSummary_listId,
    protocolsListDataSummary_listName,
    protocolsListDataSummary_protocolsList,
    protocolsListDataSummary_listArn,

    -- * RemediationAction
    RemediationAction (..),
    newRemediationAction,
    remediationAction_eC2AssociateRouteTableAction,
    remediationAction_eC2DeleteRouteAction,
    remediationAction_eC2ReplaceRouteAction,
    remediationAction_fMSPolicyUpdateFirewallCreationConfigAction,
    remediationAction_description,
    remediationAction_eC2CreateRouteTableAction,
    remediationAction_eC2CopyRouteTableAction,
    remediationAction_eC2ReplaceRouteTableAssociationAction,
    remediationAction_eC2CreateRouteAction,

    -- * RemediationActionWithOrder
    RemediationActionWithOrder (..),
    newRemediationActionWithOrder,
    remediationActionWithOrder_remediationAction,
    remediationActionWithOrder_order,

    -- * Resource
    Resource (..),
    newResource,
    resource_accountId,
    resource_uri,

    -- * ResourceSet
    ResourceSet (..),
    newResourceSet,
    resourceSet_updateToken,
    resourceSet_id,
    resourceSet_description,
    resourceSet_lastUpdateTime,
    resourceSet_name,
    resourceSet_resourceTypeList,

    -- * ResourceSetSummary
    ResourceSetSummary (..),
    newResourceSetSummary,
    resourceSetSummary_name,
    resourceSetSummary_id,
    resourceSetSummary_description,
    resourceSetSummary_lastUpdateTime,

    -- * ResourceTag
    ResourceTag (..),
    newResourceTag,
    resourceTag_value,
    resourceTag_key,

    -- * ResourceViolation
    ResourceViolation (..),
    newResourceViolation,
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
    securityGroupRuleDescription_iPV4Range,
    securityGroupRuleDescription_toPort,
    securityGroupRuleDescription_prefixListId,
    securityGroupRuleDescription_iPV6Range,
    securityGroupRuleDescription_protocol,
    securityGroupRuleDescription_fromPort,

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
    statefulRuleGroup_resourceId,
    statefulRuleGroup_ruleGroupName,
    statefulRuleGroup_override,
    statefulRuleGroup_priority,

    -- * StatelessRuleGroup
    StatelessRuleGroup (..),
    newStatelessRuleGroup,
    statelessRuleGroup_resourceId,
    statelessRuleGroup_ruleGroupName,
    statelessRuleGroup_priority,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * ThirdPartyFirewallFirewallPolicy
    ThirdPartyFirewallFirewallPolicy (..),
    newThirdPartyFirewallFirewallPolicy,
    thirdPartyFirewallFirewallPolicy_firewallPolicyName,
    thirdPartyFirewallFirewallPolicy_firewallPolicyId,

    -- * ThirdPartyFirewallMissingExpectedRouteTableViolation
    ThirdPartyFirewallMissingExpectedRouteTableViolation (..),
    newThirdPartyFirewallMissingExpectedRouteTableViolation,
    thirdPartyFirewallMissingExpectedRouteTableViolation_vpc,
    thirdPartyFirewallMissingExpectedRouteTableViolation_violationTarget,
    thirdPartyFirewallMissingExpectedRouteTableViolation_currentRouteTable,
    thirdPartyFirewallMissingExpectedRouteTableViolation_availabilityZone,
    thirdPartyFirewallMissingExpectedRouteTableViolation_expectedRouteTable,

    -- * ThirdPartyFirewallMissingFirewallViolation
    ThirdPartyFirewallMissingFirewallViolation (..),
    newThirdPartyFirewallMissingFirewallViolation,
    thirdPartyFirewallMissingFirewallViolation_vpc,
    thirdPartyFirewallMissingFirewallViolation_violationTarget,
    thirdPartyFirewallMissingFirewallViolation_availabilityZone,
    thirdPartyFirewallMissingFirewallViolation_targetViolationReason,

    -- * ThirdPartyFirewallMissingSubnetViolation
    ThirdPartyFirewallMissingSubnetViolation (..),
    newThirdPartyFirewallMissingSubnetViolation,
    thirdPartyFirewallMissingSubnetViolation_vpc,
    thirdPartyFirewallMissingSubnetViolation_violationTarget,
    thirdPartyFirewallMissingSubnetViolation_availabilityZone,
    thirdPartyFirewallMissingSubnetViolation_targetViolationReason,

    -- * ThirdPartyFirewallPolicy
    ThirdPartyFirewallPolicy (..),
    newThirdPartyFirewallPolicy,
    thirdPartyFirewallPolicy_firewallDeploymentModel,

    -- * ViolationDetail
    ViolationDetail (..),
    newViolationDetail,
    violationDetail_resourceTags,
    violationDetail_resourceDescription,
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
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | The parameters of the request were invalid.
_InvalidInputException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
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
_InvalidOperationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidOperationException =
  Core._MatchServiceError
    defaultService
    "InvalidOperationException"

-- | The operation failed because of a system problem, even though the
-- request was valid. Retry your request.
_InternalErrorException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalErrorException =
  Core._MatchServiceError
    defaultService
    "InternalErrorException"

-- | The specified resource was not found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | The value of the @Type@ parameter is invalid.
_InvalidTypeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidTypeException =
  Core._MatchServiceError
    defaultService
    "InvalidTypeException"

-- | The operation exceeds a resource limit, for example, the maximum number
-- of @policy@ objects that you can create for an Amazon Web Services
-- account. For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/fms-limits.html Firewall Manager Limits>
-- in the /WAF Developer Guide/.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"
