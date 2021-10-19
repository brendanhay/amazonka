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
    _InternalErrorException,
    _InvalidInputException,
    _InvalidOperationException,
    _InvalidTypeException,
    _ResourceNotFoundException,
    _LimitExceededException,

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
    appsListData_listUpdateToken,
    appsListData_listId,
    appsListData_lastUpdateTime,
    appsListData_previousAppsList,
    appsListData_createTime,
    appsListData_listName,
    appsListData_appsList,

    -- * AppsListDataSummary
    AppsListDataSummary (..),
    newAppsListDataSummary,
    appsListDataSummary_listArn,
    appsListDataSummary_appsList,
    appsListDataSummary_listId,
    appsListDataSummary_listName,

    -- * AwsEc2InstanceViolation
    AwsEc2InstanceViolation (..),
    newAwsEc2InstanceViolation,
    awsEc2InstanceViolation_violationTarget,
    awsEc2InstanceViolation_awsEc2NetworkInterfaceViolations,

    -- * AwsEc2NetworkInterfaceViolation
    AwsEc2NetworkInterfaceViolation (..),
    newAwsEc2NetworkInterfaceViolation,
    awsEc2NetworkInterfaceViolation_violatingSecurityGroups,
    awsEc2NetworkInterfaceViolation_violationTarget,

    -- * AwsVPCSecurityGroupViolation
    AwsVPCSecurityGroupViolation (..),
    newAwsVPCSecurityGroupViolation,
    awsVPCSecurityGroupViolation_violationTargetDescription,
    awsVPCSecurityGroupViolation_possibleSecurityGroupRemediationActions,
    awsVPCSecurityGroupViolation_violationTarget,
    awsVPCSecurityGroupViolation_partialMatches,

    -- * ComplianceViolator
    ComplianceViolator (..),
    newComplianceViolator,
    complianceViolator_resourceId,
    complianceViolator_resourceType,
    complianceViolator_violationReason,

    -- * DnsDuplicateRuleGroupViolation
    DnsDuplicateRuleGroupViolation (..),
    newDnsDuplicateRuleGroupViolation,
    dnsDuplicateRuleGroupViolation_violationTargetDescription,
    dnsDuplicateRuleGroupViolation_violationTarget,

    -- * DnsRuleGroupLimitExceededViolation
    DnsRuleGroupLimitExceededViolation (..),
    newDnsRuleGroupLimitExceededViolation,
    dnsRuleGroupLimitExceededViolation_violationTargetDescription,
    dnsRuleGroupLimitExceededViolation_violationTarget,
    dnsRuleGroupLimitExceededViolation_numberOfRuleGroupsAlreadyAssociated,

    -- * DnsRuleGroupPriorityConflictViolation
    DnsRuleGroupPriorityConflictViolation (..),
    newDnsRuleGroupPriorityConflictViolation,
    dnsRuleGroupPriorityConflictViolation_conflictingPriority,
    dnsRuleGroupPriorityConflictViolation_conflictingPolicyId,
    dnsRuleGroupPriorityConflictViolation_violationTargetDescription,
    dnsRuleGroupPriorityConflictViolation_violationTarget,
    dnsRuleGroupPriorityConflictViolation_unavailablePriorities,

    -- * EC2AssociateRouteTableAction
    EC2AssociateRouteTableAction (..),
    newEC2AssociateRouteTableAction,
    eC2AssociateRouteTableAction_subnetId,
    eC2AssociateRouteTableAction_gatewayId,
    eC2AssociateRouteTableAction_description,
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
    eC2CreateRouteAction_destinationIpv6CidrBlock,
    eC2CreateRouteAction_gatewayId,
    eC2CreateRouteAction_vpcEndpointId,
    eC2CreateRouteAction_destinationPrefixListId,
    eC2CreateRouteAction_description,
    eC2CreateRouteAction_destinationCidrBlock,
    eC2CreateRouteAction_routeTableId,

    -- * EC2CreateRouteTableAction
    EC2CreateRouteTableAction (..),
    newEC2CreateRouteTableAction,
    eC2CreateRouteTableAction_description,
    eC2CreateRouteTableAction_vpcId,

    -- * EC2DeleteRouteAction
    EC2DeleteRouteAction (..),
    newEC2DeleteRouteAction,
    eC2DeleteRouteAction_destinationIpv6CidrBlock,
    eC2DeleteRouteAction_destinationPrefixListId,
    eC2DeleteRouteAction_description,
    eC2DeleteRouteAction_destinationCidrBlock,
    eC2DeleteRouteAction_routeTableId,

    -- * EC2ReplaceRouteAction
    EC2ReplaceRouteAction (..),
    newEC2ReplaceRouteAction,
    eC2ReplaceRouteAction_destinationIpv6CidrBlock,
    eC2ReplaceRouteAction_gatewayId,
    eC2ReplaceRouteAction_destinationPrefixListId,
    eC2ReplaceRouteAction_description,
    eC2ReplaceRouteAction_destinationCidrBlock,
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
    evaluationResult_violatorCount,
    evaluationResult_complianceStatus,
    evaluationResult_evaluationLimitExceeded,

    -- * ExpectedRoute
    ExpectedRoute (..),
    newExpectedRoute,
    expectedRoute_ipV4Cidr,
    expectedRoute_routeTableId,
    expectedRoute_allowedTargets,
    expectedRoute_prefixListId,
    expectedRoute_ipV6Cidr,
    expectedRoute_contributingSubnets,

    -- * NetworkFirewallBlackHoleRouteDetectedViolation
    NetworkFirewallBlackHoleRouteDetectedViolation (..),
    newNetworkFirewallBlackHoleRouteDetectedViolation,
    networkFirewallBlackHoleRouteDetectedViolation_routeTableId,
    networkFirewallBlackHoleRouteDetectedViolation_vpcId,
    networkFirewallBlackHoleRouteDetectedViolation_violatingRoutes,
    networkFirewallBlackHoleRouteDetectedViolation_violationTarget,

    -- * NetworkFirewallInternetTrafficNotInspectedViolation
    NetworkFirewallInternetTrafficNotInspectedViolation (..),
    newNetworkFirewallInternetTrafficNotInspectedViolation,
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

    -- * NetworkFirewallInvalidRouteConfigurationViolation
    NetworkFirewallInvalidRouteConfigurationViolation (..),
    newNetworkFirewallInvalidRouteConfigurationViolation,
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

    -- * NetworkFirewallMissingExpectedRTViolation
    NetworkFirewallMissingExpectedRTViolation (..),
    newNetworkFirewallMissingExpectedRTViolation,
    networkFirewallMissingExpectedRTViolation_currentRouteTable,
    networkFirewallMissingExpectedRTViolation_availabilityZone,
    networkFirewallMissingExpectedRTViolation_vpc,
    networkFirewallMissingExpectedRTViolation_violationTarget,
    networkFirewallMissingExpectedRTViolation_expectedRouteTable,

    -- * NetworkFirewallMissingExpectedRoutesViolation
    NetworkFirewallMissingExpectedRoutesViolation (..),
    newNetworkFirewallMissingExpectedRoutesViolation,
    networkFirewallMissingExpectedRoutesViolation_expectedRoutes,
    networkFirewallMissingExpectedRoutesViolation_vpcId,
    networkFirewallMissingExpectedRoutesViolation_violationTarget,

    -- * NetworkFirewallMissingFirewallViolation
    NetworkFirewallMissingFirewallViolation (..),
    newNetworkFirewallMissingFirewallViolation,
    networkFirewallMissingFirewallViolation_targetViolationReason,
    networkFirewallMissingFirewallViolation_availabilityZone,
    networkFirewallMissingFirewallViolation_vpc,
    networkFirewallMissingFirewallViolation_violationTarget,

    -- * NetworkFirewallMissingSubnetViolation
    NetworkFirewallMissingSubnetViolation (..),
    newNetworkFirewallMissingSubnetViolation,
    networkFirewallMissingSubnetViolation_targetViolationReason,
    networkFirewallMissingSubnetViolation_availabilityZone,
    networkFirewallMissingSubnetViolation_vpc,
    networkFirewallMissingSubnetViolation_violationTarget,

    -- * NetworkFirewallPolicyDescription
    NetworkFirewallPolicyDescription (..),
    newNetworkFirewallPolicyDescription,
    networkFirewallPolicyDescription_statefulRuleGroups,
    networkFirewallPolicyDescription_statelessRuleGroups,
    networkFirewallPolicyDescription_statelessFragmentDefaultActions,
    networkFirewallPolicyDescription_statelessCustomActions,
    networkFirewallPolicyDescription_statelessDefaultActions,

    -- * NetworkFirewallPolicyModifiedViolation
    NetworkFirewallPolicyModifiedViolation (..),
    newNetworkFirewallPolicyModifiedViolation,
    networkFirewallPolicyModifiedViolation_currentPolicyDescription,
    networkFirewallPolicyModifiedViolation_violationTarget,
    networkFirewallPolicyModifiedViolation_expectedPolicyDescription,

    -- * NetworkFirewallUnexpectedFirewallRoutesViolation
    NetworkFirewallUnexpectedFirewallRoutesViolation (..),
    newNetworkFirewallUnexpectedFirewallRoutesViolation,
    networkFirewallUnexpectedFirewallRoutesViolation_routeTableId,
    networkFirewallUnexpectedFirewallRoutesViolation_vpcId,
    networkFirewallUnexpectedFirewallRoutesViolation_violatingRoutes,
    networkFirewallUnexpectedFirewallRoutesViolation_firewallEndpoint,
    networkFirewallUnexpectedFirewallRoutesViolation_firewallSubnetId,

    -- * NetworkFirewallUnexpectedGatewayRoutesViolation
    NetworkFirewallUnexpectedGatewayRoutesViolation (..),
    newNetworkFirewallUnexpectedGatewayRoutesViolation,
    networkFirewallUnexpectedGatewayRoutesViolation_routeTableId,
    networkFirewallUnexpectedGatewayRoutesViolation_vpcId,
    networkFirewallUnexpectedGatewayRoutesViolation_violatingRoutes,
    networkFirewallUnexpectedGatewayRoutesViolation_gatewayId,

    -- * PartialMatch
    PartialMatch (..),
    newPartialMatch,
    partialMatch_targetViolationReasons,
    partialMatch_reference,

    -- * Policy
    Policy (..),
    newPolicy,
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

    -- * PolicyComplianceDetail
    PolicyComplianceDetail (..),
    newPolicyComplianceDetail,
    policyComplianceDetail_expiredAt,
    policyComplianceDetail_policyId,
    policyComplianceDetail_violators,
    policyComplianceDetail_evaluationLimitExceeded,
    policyComplianceDetail_issueInfoMap,
    policyComplianceDetail_policyOwner,
    policyComplianceDetail_memberAccount,

    -- * PolicyComplianceStatus
    PolicyComplianceStatus (..),
    newPolicyComplianceStatus,
    policyComplianceStatus_evaluationResults,
    policyComplianceStatus_lastUpdated,
    policyComplianceStatus_policyName,
    policyComplianceStatus_policyId,
    policyComplianceStatus_issueInfoMap,
    policyComplianceStatus_policyOwner,
    policyComplianceStatus_memberAccount,

    -- * PolicySummary
    PolicySummary (..),
    newPolicySummary,
    policySummary_policyName,
    policySummary_remediationEnabled,
    policySummary_resourceType,
    policySummary_policyId,
    policySummary_deleteUnusedFMManagedResources,
    policySummary_policyArn,
    policySummary_securityServiceType,

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
    protocolsListData_listUpdateToken,
    protocolsListData_listId,
    protocolsListData_lastUpdateTime,
    protocolsListData_previousProtocolsList,
    protocolsListData_createTime,
    protocolsListData_listName,
    protocolsListData_protocolsList,

    -- * ProtocolsListDataSummary
    ProtocolsListDataSummary (..),
    newProtocolsListDataSummary,
    protocolsListDataSummary_protocolsList,
    protocolsListDataSummary_listArn,
    protocolsListDataSummary_listId,
    protocolsListDataSummary_listName,

    -- * RemediationAction
    RemediationAction (..),
    newRemediationAction,
    remediationAction_eC2CreateRouteAction,
    remediationAction_eC2CopyRouteTableAction,
    remediationAction_eC2ReplaceRouteTableAssociationAction,
    remediationAction_eC2AssociateRouteTableAction,
    remediationAction_eC2ReplaceRouteAction,
    remediationAction_eC2DeleteRouteAction,
    remediationAction_description,
    remediationAction_eC2CreateRouteTableAction,

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

    -- * Route
    Route (..),
    newRoute,
    route_destination,
    route_targetType,
    route_destinationType,
    route_target,

    -- * SecurityGroupRemediationAction
    SecurityGroupRemediationAction (..),
    newSecurityGroupRemediationAction,
    securityGroupRemediationAction_isDefaultAction,
    securityGroupRemediationAction_remediationResult,
    securityGroupRemediationAction_description,
    securityGroupRemediationAction_remediationActionType,

    -- * SecurityGroupRuleDescription
    SecurityGroupRuleDescription (..),
    newSecurityGroupRuleDescription,
    securityGroupRuleDescription_fromPort,
    securityGroupRuleDescription_protocol,
    securityGroupRuleDescription_iPV4Range,
    securityGroupRuleDescription_prefixListId,
    securityGroupRuleDescription_toPort,
    securityGroupRuleDescription_iPV6Range,

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
    violationDetail_resourceTags,
    violationDetail_resourceDescription,
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
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
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
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

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

-- | The value of the @Type@ parameter is invalid.
_InvalidTypeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidTypeException =
  Core._MatchServiceError
    defaultService
    "InvalidTypeException"

-- | The specified resource was not found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

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
