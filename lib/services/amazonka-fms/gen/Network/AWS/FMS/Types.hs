{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InvalidTypeException,
    _InvalidOperationException,
    _InternalErrorException,
    _InvalidInputException,
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

    -- * PolicyComplianceStatusType
    PolicyComplianceStatusType (..),

    -- * RemediationActionType
    RemediationActionType (..),

    -- * SecurityServiceType
    SecurityServiceType (..),

    -- * TargetType
    TargetType (..),

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
    appsListData_lastUpdateTime,
    appsListData_listId,
    appsListData_createTime,
    appsListData_previousAppsList,
    appsListData_listUpdateToken,
    appsListData_listName,
    appsListData_appsList,

    -- * AppsListDataSummary
    AppsListDataSummary (..),
    newAppsListDataSummary,
    appsListDataSummary_appsList,
    appsListDataSummary_listName,
    appsListDataSummary_listId,
    appsListDataSummary_listArn,

    -- * AwsEc2InstanceViolation
    AwsEc2InstanceViolation (..),
    newAwsEc2InstanceViolation,
    awsEc2InstanceViolation_violationTarget,
    awsEc2InstanceViolation_awsEc2NetworkInterfaceViolations,

    -- * AwsEc2NetworkInterfaceViolation
    AwsEc2NetworkInterfaceViolation (..),
    newAwsEc2NetworkInterfaceViolation,
    awsEc2NetworkInterfaceViolation_violationTarget,
    awsEc2NetworkInterfaceViolation_violatingSecurityGroups,

    -- * AwsVPCSecurityGroupViolation
    AwsVPCSecurityGroupViolation (..),
    newAwsVPCSecurityGroupViolation,
    awsVPCSecurityGroupViolation_possibleSecurityGroupRemediationActions,
    awsVPCSecurityGroupViolation_partialMatches,
    awsVPCSecurityGroupViolation_violationTarget,
    awsVPCSecurityGroupViolation_violationTargetDescription,

    -- * ComplianceViolator
    ComplianceViolator (..),
    newComplianceViolator,
    complianceViolator_resourceId,
    complianceViolator_resourceType,
    complianceViolator_violationReason,

    -- * DnsDuplicateRuleGroupViolation
    DnsDuplicateRuleGroupViolation (..),
    newDnsDuplicateRuleGroupViolation,
    dnsDuplicateRuleGroupViolation_violationTarget,
    dnsDuplicateRuleGroupViolation_violationTargetDescription,

    -- * DnsRuleGroupLimitExceededViolation
    DnsRuleGroupLimitExceededViolation (..),
    newDnsRuleGroupLimitExceededViolation,
    dnsRuleGroupLimitExceededViolation_violationTarget,
    dnsRuleGroupLimitExceededViolation_violationTargetDescription,
    dnsRuleGroupLimitExceededViolation_numberOfRuleGroupsAlreadyAssociated,

    -- * DnsRuleGroupPriorityConflictViolation
    DnsRuleGroupPriorityConflictViolation (..),
    newDnsRuleGroupPriorityConflictViolation,
    dnsRuleGroupPriorityConflictViolation_violationTarget,
    dnsRuleGroupPriorityConflictViolation_conflictingPriority,
    dnsRuleGroupPriorityConflictViolation_violationTargetDescription,
    dnsRuleGroupPriorityConflictViolation_unavailablePriorities,
    dnsRuleGroupPriorityConflictViolation_conflictingPolicyId,

    -- * EC2AssociateRouteTableAction
    EC2AssociateRouteTableAction (..),
    newEC2AssociateRouteTableAction,
    eC2AssociateRouteTableAction_description,
    eC2AssociateRouteTableAction_subnetId,
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
    eC2CreateRouteAction_vpcEndpointId,
    eC2CreateRouteAction_destinationPrefixListId,
    eC2CreateRouteAction_destinationIpv6CidrBlock,
    eC2CreateRouteAction_destinationCidrBlock,
    eC2CreateRouteAction_description,
    eC2CreateRouteAction_gatewayId,
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
    eC2DeleteRouteAction_destinationIpv6CidrBlock,
    eC2DeleteRouteAction_destinationCidrBlock,
    eC2DeleteRouteAction_description,
    eC2DeleteRouteAction_routeTableId,

    -- * EC2ReplaceRouteAction
    EC2ReplaceRouteAction (..),
    newEC2ReplaceRouteAction,
    eC2ReplaceRouteAction_destinationPrefixListId,
    eC2ReplaceRouteAction_destinationIpv6CidrBlock,
    eC2ReplaceRouteAction_destinationCidrBlock,
    eC2ReplaceRouteAction_description,
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
    expectedRoute_ipV4Cidr,
    expectedRoute_ipV6Cidr,
    expectedRoute_allowedTargets,
    expectedRoute_routeTableId,
    expectedRoute_prefixListId,
    expectedRoute_contributingSubnets,

    -- * NetworkFirewallBlackHoleRouteDetectedViolation
    NetworkFirewallBlackHoleRouteDetectedViolation (..),
    newNetworkFirewallBlackHoleRouteDetectedViolation,
    networkFirewallBlackHoleRouteDetectedViolation_violationTarget,
    networkFirewallBlackHoleRouteDetectedViolation_routeTableId,
    networkFirewallBlackHoleRouteDetectedViolation_violatingRoutes,
    networkFirewallBlackHoleRouteDetectedViolation_vpcId,

    -- * NetworkFirewallInternetTrafficNotInspectedViolation
    NetworkFirewallInternetTrafficNotInspectedViolation (..),
    newNetworkFirewallInternetTrafficNotInspectedViolation,
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

    -- * NetworkFirewallInvalidRouteConfigurationViolation
    NetworkFirewallInvalidRouteConfigurationViolation (..),
    newNetworkFirewallInvalidRouteConfigurationViolation,
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

    -- * NetworkFirewallMissingExpectedRTViolation
    NetworkFirewallMissingExpectedRTViolation (..),
    newNetworkFirewallMissingExpectedRTViolation,
    networkFirewallMissingExpectedRTViolation_currentRouteTable,
    networkFirewallMissingExpectedRTViolation_violationTarget,
    networkFirewallMissingExpectedRTViolation_availabilityZone,
    networkFirewallMissingExpectedRTViolation_expectedRouteTable,
    networkFirewallMissingExpectedRTViolation_vpc,

    -- * NetworkFirewallMissingExpectedRoutesViolation
    NetworkFirewallMissingExpectedRoutesViolation (..),
    newNetworkFirewallMissingExpectedRoutesViolation,
    networkFirewallMissingExpectedRoutesViolation_expectedRoutes,
    networkFirewallMissingExpectedRoutesViolation_violationTarget,
    networkFirewallMissingExpectedRoutesViolation_vpcId,

    -- * NetworkFirewallMissingFirewallViolation
    NetworkFirewallMissingFirewallViolation (..),
    newNetworkFirewallMissingFirewallViolation,
    networkFirewallMissingFirewallViolation_targetViolationReason,
    networkFirewallMissingFirewallViolation_violationTarget,
    networkFirewallMissingFirewallViolation_availabilityZone,
    networkFirewallMissingFirewallViolation_vpc,

    -- * NetworkFirewallMissingSubnetViolation
    NetworkFirewallMissingSubnetViolation (..),
    newNetworkFirewallMissingSubnetViolation,
    networkFirewallMissingSubnetViolation_targetViolationReason,
    networkFirewallMissingSubnetViolation_violationTarget,
    networkFirewallMissingSubnetViolation_availabilityZone,
    networkFirewallMissingSubnetViolation_vpc,

    -- * NetworkFirewallPolicyDescription
    NetworkFirewallPolicyDescription (..),
    newNetworkFirewallPolicyDescription,
    networkFirewallPolicyDescription_statelessRuleGroups,
    networkFirewallPolicyDescription_statelessDefaultActions,
    networkFirewallPolicyDescription_statelessFragmentDefaultActions,
    networkFirewallPolicyDescription_statefulRuleGroups,
    networkFirewallPolicyDescription_statelessCustomActions,

    -- * NetworkFirewallPolicyModifiedViolation
    NetworkFirewallPolicyModifiedViolation (..),
    newNetworkFirewallPolicyModifiedViolation,
    networkFirewallPolicyModifiedViolation_currentPolicyDescription,
    networkFirewallPolicyModifiedViolation_violationTarget,
    networkFirewallPolicyModifiedViolation_expectedPolicyDescription,

    -- * NetworkFirewallUnexpectedFirewallRoutesViolation
    NetworkFirewallUnexpectedFirewallRoutesViolation (..),
    newNetworkFirewallUnexpectedFirewallRoutesViolation,
    networkFirewallUnexpectedFirewallRoutesViolation_firewallEndpoint,
    networkFirewallUnexpectedFirewallRoutesViolation_routeTableId,
    networkFirewallUnexpectedFirewallRoutesViolation_violatingRoutes,
    networkFirewallUnexpectedFirewallRoutesViolation_firewallSubnetId,
    networkFirewallUnexpectedFirewallRoutesViolation_vpcId,

    -- * NetworkFirewallUnexpectedGatewayRoutesViolation
    NetworkFirewallUnexpectedGatewayRoutesViolation (..),
    newNetworkFirewallUnexpectedGatewayRoutesViolation,
    networkFirewallUnexpectedGatewayRoutesViolation_routeTableId,
    networkFirewallUnexpectedGatewayRoutesViolation_violatingRoutes,
    networkFirewallUnexpectedGatewayRoutesViolation_vpcId,
    networkFirewallUnexpectedGatewayRoutesViolation_gatewayId,

    -- * PartialMatch
    PartialMatch (..),
    newPartialMatch,
    partialMatch_targetViolationReasons,
    partialMatch_reference,

    -- * Policy
    Policy (..),
    newPolicy,
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

    -- * PolicyComplianceDetail
    PolicyComplianceDetail (..),
    newPolicyComplianceDetail,
    policyComplianceDetail_violators,
    policyComplianceDetail_policyOwner,
    policyComplianceDetail_memberAccount,
    policyComplianceDetail_evaluationLimitExceeded,
    policyComplianceDetail_issueInfoMap,
    policyComplianceDetail_policyId,
    policyComplianceDetail_expiredAt,

    -- * PolicyComplianceStatus
    PolicyComplianceStatus (..),
    newPolicyComplianceStatus,
    policyComplianceStatus_policyName,
    policyComplianceStatus_policyOwner,
    policyComplianceStatus_memberAccount,
    policyComplianceStatus_lastUpdated,
    policyComplianceStatus_evaluationResults,
    policyComplianceStatus_issueInfoMap,
    policyComplianceStatus_policyId,

    -- * PolicySummary
    PolicySummary (..),
    newPolicySummary,
    policySummary_policyName,
    policySummary_securityServiceType,
    policySummary_resourceType,
    policySummary_remediationEnabled,
    policySummary_policyArn,
    policySummary_policyId,
    policySummary_deleteUnusedFMManagedResources,

    -- * PossibleRemediationAction
    PossibleRemediationAction (..),
    newPossibleRemediationAction,
    possibleRemediationAction_isDefaultAction,
    possibleRemediationAction_description,
    possibleRemediationAction_orderedRemediationActions,

    -- * PossibleRemediationActions
    PossibleRemediationActions (..),
    newPossibleRemediationActions,
    possibleRemediationActions_actions,
    possibleRemediationActions_description,

    -- * ProtocolsListData
    ProtocolsListData (..),
    newProtocolsListData,
    protocolsListData_lastUpdateTime,
    protocolsListData_listId,
    protocolsListData_createTime,
    protocolsListData_previousProtocolsList,
    protocolsListData_listUpdateToken,
    protocolsListData_listName,
    protocolsListData_protocolsList,

    -- * ProtocolsListDataSummary
    ProtocolsListDataSummary (..),
    newProtocolsListDataSummary,
    protocolsListDataSummary_listName,
    protocolsListDataSummary_listId,
    protocolsListDataSummary_listArn,
    protocolsListDataSummary_protocolsList,

    -- * RemediationAction
    RemediationAction (..),
    newRemediationAction,
    remediationAction_eC2CreateRouteAction,
    remediationAction_eC2ReplaceRouteAction,
    remediationAction_eC2AssociateRouteTableAction,
    remediationAction_eC2CreateRouteTableAction,
    remediationAction_description,
    remediationAction_eC2CopyRouteTableAction,
    remediationAction_eC2ReplaceRouteTableAssociationAction,
    remediationAction_eC2DeleteRouteAction,

    -- * RemediationActionWithOrder
    RemediationActionWithOrder (..),
    newRemediationActionWithOrder,
    remediationActionWithOrder_remediationAction,
    remediationActionWithOrder_order,

    -- * ResourceTag
    ResourceTag (..),
    newResourceTag,
    resourceTag_value,
    resourceTag_key,

    -- * ResourceViolation
    ResourceViolation (..),
    newResourceViolation,
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

    -- * Route
    Route (..),
    newRoute,
    route_destinationType,
    route_targetType,
    route_destination,
    route_target,

    -- * SecurityGroupRemediationAction
    SecurityGroupRemediationAction (..),
    newSecurityGroupRemediationAction,
    securityGroupRemediationAction_remediationActionType,
    securityGroupRemediationAction_remediationResult,
    securityGroupRemediationAction_isDefaultAction,
    securityGroupRemediationAction_description,

    -- * SecurityGroupRuleDescription
    SecurityGroupRuleDescription (..),
    newSecurityGroupRuleDescription,
    securityGroupRuleDescription_fromPort,
    securityGroupRuleDescription_iPV4Range,
    securityGroupRuleDescription_prefixListId,
    securityGroupRuleDescription_protocol,
    securityGroupRuleDescription_iPV6Range,
    securityGroupRuleDescription_toPort,

    -- * SecurityServicePolicyData
    SecurityServicePolicyData (..),
    newSecurityServicePolicyData,
    securityServicePolicyData_managedServiceData,
    securityServicePolicyData_type,

    -- * StatefulRuleGroup
    StatefulRuleGroup (..),
    newStatefulRuleGroup,
    statefulRuleGroup_resourceId,
    statefulRuleGroup_ruleGroupName,

    -- * StatelessRuleGroup
    StatelessRuleGroup (..),
    newStatelessRuleGroup,
    statelessRuleGroup_resourceId,
    statelessRuleGroup_priority,
    statelessRuleGroup_ruleGroupName,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

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

import qualified Network.AWS.Core as Core
import Network.AWS.FMS.Types.AccountRoleStatus
import Network.AWS.FMS.Types.ActionTarget
import Network.AWS.FMS.Types.App
import Network.AWS.FMS.Types.AppsListData
import Network.AWS.FMS.Types.AppsListDataSummary
import Network.AWS.FMS.Types.AwsEc2InstanceViolation
import Network.AWS.FMS.Types.AwsEc2NetworkInterfaceViolation
import Network.AWS.FMS.Types.AwsVPCSecurityGroupViolation
import Network.AWS.FMS.Types.ComplianceViolator
import Network.AWS.FMS.Types.CustomerPolicyScopeIdType
import Network.AWS.FMS.Types.DependentServiceName
import Network.AWS.FMS.Types.DestinationType
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
import Network.AWS.FMS.Types.PolicyComplianceStatusType
import Network.AWS.FMS.Types.PolicySummary
import Network.AWS.FMS.Types.PossibleRemediationAction
import Network.AWS.FMS.Types.PossibleRemediationActions
import Network.AWS.FMS.Types.ProtocolsListData
import Network.AWS.FMS.Types.ProtocolsListDataSummary
import Network.AWS.FMS.Types.RemediationAction
import Network.AWS.FMS.Types.RemediationActionType
import Network.AWS.FMS.Types.RemediationActionWithOrder
import Network.AWS.FMS.Types.ResourceTag
import Network.AWS.FMS.Types.ResourceViolation
import Network.AWS.FMS.Types.Route
import Network.AWS.FMS.Types.SecurityGroupRemediationAction
import Network.AWS.FMS.Types.SecurityGroupRuleDescription
import Network.AWS.FMS.Types.SecurityServicePolicyData
import Network.AWS.FMS.Types.SecurityServiceType
import Network.AWS.FMS.Types.StatefulRuleGroup
import Network.AWS.FMS.Types.StatelessRuleGroup
import Network.AWS.FMS.Types.Tag
import Network.AWS.FMS.Types.TargetType
import Network.AWS.FMS.Types.ViolationDetail
import Network.AWS.FMS.Types.ViolationReason
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2018-01-01@ of the Amazon Firewall Management Service SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "FMS",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "fms",
      Core._serviceSigningName = "fms",
      Core._serviceVersion = "2018-01-01",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "FMS",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing

-- | The value of the @Type@ parameter is invalid.
_InvalidTypeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidTypeException =
  Core._MatchServiceError
    defaultService
    "InvalidTypeException"

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

-- | The parameters of the request were invalid.
_InvalidInputException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidInputException =
  Core._MatchServiceError
    defaultService
    "InvalidInputException"

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

-- | The specified resource was not found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
