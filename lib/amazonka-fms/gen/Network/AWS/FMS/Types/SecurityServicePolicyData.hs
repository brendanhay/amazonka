{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.SecurityServicePolicyData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.SecurityServicePolicyData
  ( SecurityServicePolicyData (..),

    -- * Smart constructor
    mkSecurityServicePolicyData,

    -- * Lenses
    sspdManagedServiceData,
    sspdType,
  )
where

import Network.AWS.FMS.Types.SecurityServiceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Details about the security service that is being used to protect the resources.
--
-- /See:/ 'mkSecurityServicePolicyData' smart constructor.
data SecurityServicePolicyData = SecurityServicePolicyData'
  { managedServiceData ::
      Lude.Maybe Lude.Text,
    type' :: SecurityServiceType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SecurityServicePolicyData' with the minimum fields required to make a request.
--
-- * 'managedServiceData' - Details about the service that are specific to the service type, in JSON format. For service type @SHIELD_ADVANCED@ , this is an empty string.
--
--
--     * Example: @NETWORK_FIREWALL@
-- @"{\"type\":\"NETWORK_FIREWALL\",\"networkFirewallStatelessRuleGroupReferences\":[{\"resourceARN\":\"arn:aws:network-firewall:us-west-1:1234567891011:stateless-rulegroup/rulegroup2\",\"priority\":10}],\"networkFirewallStatelessDefaultActions\":[\"aws:pass\",\"custom1\"],\"networkFirewallStatelessFragmentDefaultActions\":[\"custom2\",\"aws:pass\"],\"networkFirewallStatelessCustomActions\":[{\"actionName\":\"custom1\",\"actionDefinition\":{\"publishMetricAction\":{\"dimensions\":[{\"value\":\"dimension1\"}]}}},{\"actionName\":\"custom2\",\"actionDefinition\":{\"publishMetricAction\":{\"dimensions\":[{\"value\":\"dimension2\"}]}}}],\"networkFirewallStatefulRuleGroupReferences\":[{\"resourceARN\":\"arn:aws:network-firewall:us-west-1:1234567891011:stateful-rulegroup/rulegroup1\"}],\"networkFirewallOrchestrationConfig\":{\"singleFirewallEndpointPerVPC\":true,\"allowedIPV4CidrList\":[\"10.24.34.0/28\"]} }"@
--
--
--     * Example: @WAFV2@
-- @"{\"type\":\"WAFV2\",\"preProcessRuleGroups\":[{\"ruleGroupArn\":null,\"overrideAction\":{\"type\":\"NONE\"},\"managedRuleGroupIdentifier\":{\"version\":null,\"vendorName\":\"AWS\",\"managedRuleGroupName\":\"AWSManagedRulesAmazonIpReputationList\"},\"ruleGroupType\":\"ManagedRuleGroup\",\"excludeRules\":[]}],\"postProcessRuleGroups\":[],\"defaultAction\":{\"type\":\"ALLOW\"},\"overrideCustomerWebACLAssociation\":false,\"loggingConfiguration\":{\"logDestinationConfigs\":[\"arn:aws:firehose:us-west-2:12345678912:deliverystream/aws-waf-logs-fms-admin-destination\"],\"redactedFields\":[{\"redactedFieldType\":\"SingleHeader\",\"redactedFieldValue\":\"Cookies\"},{\"redactedFieldType\":\"Method\"}]}}"@
-- In the @loggingConfiguration@ , you can specify one @logDestinationConfigs@ , you can optionally provide up to 20 @redactedFields@ , and the @RedactedFieldType@ must be one of @URI@ , @QUERY_STRING@ , @HEADER@ , or @METHOD@ .
--
--
--     * Example: @WAF Classic@
-- @"{\"type\": \"WAF\", \"ruleGroups\": [{\"id\":\"12345678-1bcd-9012-efga-0987654321ab\", \"overrideAction\" : {\"type\": \"COUNT\"}}], \"defaultAction\": {\"type\": \"BLOCK\"}}"@
--
--
--     * Example: @SECURITY_GROUPS_COMMON@
-- @"{\"type\":\"SECURITY_GROUPS_COMMON\",\"revertManualSecurityGroupChanges\":false,\"exclusiveResourceSecurityGroupManagement\":false, \"applyToAllEC2InstanceENIs\":false,\"securityGroups\":[{\"id\":\" sg-000e55995d61a06bd\"}]}"@
--
--
--     * Example: @SECURITY_GROUPS_CONTENT_AUDIT@
-- @"{\"type\":\"SECURITY_GROUPS_CONTENT_AUDIT\",\"securityGroups\":[{\"id\":\"sg-000e55995d61a06bd\"}],\"securityGroupAction\":{\"type\":\"ALLOW\"}}"@
-- The security group action for content audit can be @ALLOW@ or @DENY@ . For @ALLOW@ , all in-scope security group rules must be within the allowed range of the policy's security group rules. For @DENY@ , all in-scope security group rules must not contain a value or a range that matches a rule value or range in the policy security group.
--
--
--     * Example: @SECURITY_GROUPS_USAGE_AUDIT@
-- @"{\"type\":\"SECURITY_GROUPS_USAGE_AUDIT\",\"deleteUnusedSecurityGroups\":true,\"coalesceRedundantSecurityGroups\":true}"@
--
--
-- * 'type'' - The service that the policy is using to protect the resources. This specifies the type of policy that is created, either an AWS WAF policy, a Shield Advanced policy, or a security group policy. For security group policies, Firewall Manager supports one security group for each common policy and for each content audit policy. This is an adjustable limit that you can increase by contacting AWS Support.
mkSecurityServicePolicyData ::
  -- | 'type''
  SecurityServiceType ->
  SecurityServicePolicyData
mkSecurityServicePolicyData pType_ =
  SecurityServicePolicyData'
    { managedServiceData = Lude.Nothing,
      type' = pType_
    }

-- | Details about the service that are specific to the service type, in JSON format. For service type @SHIELD_ADVANCED@ , this is an empty string.
--
--
--     * Example: @NETWORK_FIREWALL@
-- @"{\"type\":\"NETWORK_FIREWALL\",\"networkFirewallStatelessRuleGroupReferences\":[{\"resourceARN\":\"arn:aws:network-firewall:us-west-1:1234567891011:stateless-rulegroup/rulegroup2\",\"priority\":10}],\"networkFirewallStatelessDefaultActions\":[\"aws:pass\",\"custom1\"],\"networkFirewallStatelessFragmentDefaultActions\":[\"custom2\",\"aws:pass\"],\"networkFirewallStatelessCustomActions\":[{\"actionName\":\"custom1\",\"actionDefinition\":{\"publishMetricAction\":{\"dimensions\":[{\"value\":\"dimension1\"}]}}},{\"actionName\":\"custom2\",\"actionDefinition\":{\"publishMetricAction\":{\"dimensions\":[{\"value\":\"dimension2\"}]}}}],\"networkFirewallStatefulRuleGroupReferences\":[{\"resourceARN\":\"arn:aws:network-firewall:us-west-1:1234567891011:stateful-rulegroup/rulegroup1\"}],\"networkFirewallOrchestrationConfig\":{\"singleFirewallEndpointPerVPC\":true,\"allowedIPV4CidrList\":[\"10.24.34.0/28\"]} }"@
--
--
--     * Example: @WAFV2@
-- @"{\"type\":\"WAFV2\",\"preProcessRuleGroups\":[{\"ruleGroupArn\":null,\"overrideAction\":{\"type\":\"NONE\"},\"managedRuleGroupIdentifier\":{\"version\":null,\"vendorName\":\"AWS\",\"managedRuleGroupName\":\"AWSManagedRulesAmazonIpReputationList\"},\"ruleGroupType\":\"ManagedRuleGroup\",\"excludeRules\":[]}],\"postProcessRuleGroups\":[],\"defaultAction\":{\"type\":\"ALLOW\"},\"overrideCustomerWebACLAssociation\":false,\"loggingConfiguration\":{\"logDestinationConfigs\":[\"arn:aws:firehose:us-west-2:12345678912:deliverystream/aws-waf-logs-fms-admin-destination\"],\"redactedFields\":[{\"redactedFieldType\":\"SingleHeader\",\"redactedFieldValue\":\"Cookies\"},{\"redactedFieldType\":\"Method\"}]}}"@
-- In the @loggingConfiguration@ , you can specify one @logDestinationConfigs@ , you can optionally provide up to 20 @redactedFields@ , and the @RedactedFieldType@ must be one of @URI@ , @QUERY_STRING@ , @HEADER@ , or @METHOD@ .
--
--
--     * Example: @WAF Classic@
-- @"{\"type\": \"WAF\", \"ruleGroups\": [{\"id\":\"12345678-1bcd-9012-efga-0987654321ab\", \"overrideAction\" : {\"type\": \"COUNT\"}}], \"defaultAction\": {\"type\": \"BLOCK\"}}"@
--
--
--     * Example: @SECURITY_GROUPS_COMMON@
-- @"{\"type\":\"SECURITY_GROUPS_COMMON\",\"revertManualSecurityGroupChanges\":false,\"exclusiveResourceSecurityGroupManagement\":false, \"applyToAllEC2InstanceENIs\":false,\"securityGroups\":[{\"id\":\" sg-000e55995d61a06bd\"}]}"@
--
--
--     * Example: @SECURITY_GROUPS_CONTENT_AUDIT@
-- @"{\"type\":\"SECURITY_GROUPS_CONTENT_AUDIT\",\"securityGroups\":[{\"id\":\"sg-000e55995d61a06bd\"}],\"securityGroupAction\":{\"type\":\"ALLOW\"}}"@
-- The security group action for content audit can be @ALLOW@ or @DENY@ . For @ALLOW@ , all in-scope security group rules must be within the allowed range of the policy's security group rules. For @DENY@ , all in-scope security group rules must not contain a value or a range that matches a rule value or range in the policy security group.
--
--
--     * Example: @SECURITY_GROUPS_USAGE_AUDIT@
-- @"{\"type\":\"SECURITY_GROUPS_USAGE_AUDIT\",\"deleteUnusedSecurityGroups\":true,\"coalesceRedundantSecurityGroups\":true}"@
--
--
--
-- /Note:/ Consider using 'managedServiceData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sspdManagedServiceData :: Lens.Lens' SecurityServicePolicyData (Lude.Maybe Lude.Text)
sspdManagedServiceData = Lens.lens (managedServiceData :: SecurityServicePolicyData -> Lude.Maybe Lude.Text) (\s a -> s {managedServiceData = a} :: SecurityServicePolicyData)
{-# DEPRECATED sspdManagedServiceData "Use generic-lens or generic-optics with 'managedServiceData' instead." #-}

-- | The service that the policy is using to protect the resources. This specifies the type of policy that is created, either an AWS WAF policy, a Shield Advanced policy, or a security group policy. For security group policies, Firewall Manager supports one security group for each common policy and for each content audit policy. This is an adjustable limit that you can increase by contacting AWS Support.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sspdType :: Lens.Lens' SecurityServicePolicyData SecurityServiceType
sspdType = Lens.lens (type' :: SecurityServicePolicyData -> SecurityServiceType) (\s a -> s {type' = a} :: SecurityServicePolicyData)
{-# DEPRECATED sspdType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON SecurityServicePolicyData where
  parseJSON =
    Lude.withObject
      "SecurityServicePolicyData"
      ( \x ->
          SecurityServicePolicyData'
            Lude.<$> (x Lude..:? "ManagedServiceData") Lude.<*> (x Lude..: "Type")
      )

instance Lude.ToJSON SecurityServicePolicyData where
  toJSON SecurityServicePolicyData' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ManagedServiceData" Lude..=) Lude.<$> managedServiceData,
            Lude.Just ("Type" Lude..= type')
          ]
      )
