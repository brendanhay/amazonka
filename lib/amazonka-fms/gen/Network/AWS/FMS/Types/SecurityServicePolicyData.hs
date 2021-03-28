{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.SecurityServicePolicyData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.FMS.Types.SecurityServicePolicyData
  ( SecurityServicePolicyData (..)
  -- * Smart constructor
  , mkSecurityServicePolicyData
  -- * Lenses
  , sspdType
  , sspdManagedServiceData
  ) where

import qualified Network.AWS.FMS.Types.ManagedServiceData as Types
import qualified Network.AWS.FMS.Types.SecurityServiceType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Details about the security service that is being used to protect the resources.
--
-- /See:/ 'mkSecurityServicePolicyData' smart constructor.
data SecurityServicePolicyData = SecurityServicePolicyData'
  { type' :: Types.SecurityServiceType
    -- ^ The service that the policy is using to protect the resources. This specifies the type of policy that is created, either an AWS WAF policy, a Shield Advanced policy, or a security group policy. For security group policies, Firewall Manager supports one security group for each common policy and for each content audit policy. This is an adjustable limit that you can increase by contacting AWS Support.
  , managedServiceData :: Core.Maybe Types.ManagedServiceData
    -- ^ Details about the service that are specific to the service type, in JSON format. For service type @SHIELD_ADVANCED@ , this is an empty string.
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SecurityServicePolicyData' value with any optional fields omitted.
mkSecurityServicePolicyData
    :: Types.SecurityServiceType -- ^ 'type\''
    -> SecurityServicePolicyData
mkSecurityServicePolicyData type'
  = SecurityServicePolicyData'{type',
                               managedServiceData = Core.Nothing}

-- | The service that the policy is using to protect the resources. This specifies the type of policy that is created, either an AWS WAF policy, a Shield Advanced policy, or a security group policy. For security group policies, Firewall Manager supports one security group for each common policy and for each content audit policy. This is an adjustable limit that you can increase by contacting AWS Support.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sspdType :: Lens.Lens' SecurityServicePolicyData Types.SecurityServiceType
sspdType = Lens.field @"type'"
{-# INLINEABLE sspdType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

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
sspdManagedServiceData :: Lens.Lens' SecurityServicePolicyData (Core.Maybe Types.ManagedServiceData)
sspdManagedServiceData = Lens.field @"managedServiceData"
{-# INLINEABLE sspdManagedServiceData #-}
{-# DEPRECATED managedServiceData "Use generic-lens or generic-optics with 'managedServiceData' instead"  #-}

instance Core.FromJSON SecurityServicePolicyData where
        toJSON SecurityServicePolicyData{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Type" Core..= type'),
                  ("ManagedServiceData" Core..=) Core.<$> managedServiceData])

instance Core.FromJSON SecurityServicePolicyData where
        parseJSON
          = Core.withObject "SecurityServicePolicyData" Core.$
              \ x ->
                SecurityServicePolicyData' Core.<$>
                  (x Core..: "Type") Core.<*> x Core..:? "ManagedServiceData"
