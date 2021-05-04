{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.SecurityServicePolicyData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.SecurityServicePolicyData where

import Network.AWS.FMS.Types.SecurityServiceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Details about the security service that is being used to protect the
-- resources.
--
-- /See:/ 'newSecurityServicePolicyData' smart constructor.
data SecurityServicePolicyData = SecurityServicePolicyData'
  { -- | Details about the service that are specific to the service type, in JSON
    -- format. For service type @SHIELD_ADVANCED@, this is an empty string.
    --
    -- -   Example: @NETWORK_FIREWALL@
    --
    --     @\"{\\\"type\\\":\\\"NETWORK_FIREWALL\\\",\\\"networkFirewallStatelessRuleGroupReferences\\\":[{\\\"resourceARN\\\":\\\"arn:aws:network-firewall:us-west-1:1234567891011:stateless-rulegroup\/rulegroup2\\\",\\\"priority\\\":10}],\\\"networkFirewallStatelessDefaultActions\\\":[\\\"aws:pass\\\",\\\"custom1\\\"],\\\"networkFirewallStatelessFragmentDefaultActions\\\":[\\\"custom2\\\",\\\"aws:pass\\\"],\\\"networkFirewallStatelessCustomActions\\\":[{\\\"actionName\\\":\\\"custom1\\\",\\\"actionDefinition\\\":{\\\"publishMetricAction\\\":{\\\"dimensions\\\":[{\\\"value\\\":\\\"dimension1\\\"}]}}},{\\\"actionName\\\":\\\"custom2\\\",\\\"actionDefinition\\\":{\\\"publishMetricAction\\\":{\\\"dimensions\\\":[{\\\"value\\\":\\\"dimension2\\\"}]}}}],\\\"networkFirewallStatefulRuleGroupReferences\\\":[{\\\"resourceARN\\\":\\\"arn:aws:network-firewall:us-west-1:1234567891011:stateful-rulegroup\/rulegroup1\\\"}],\\\"networkFirewallOrchestrationConfig\\\":{\\\"singleFirewallEndpointPerVPC\\\":true,\\\"allowedIPV4CidrList\\\":[\\\"10.24.34.0\/28\\\"]} }\"@
    --
    -- -   Example: @WAFV2@
    --
    --     @\"{\\\"type\\\":\\\"WAFV2\\\",\\\"preProcessRuleGroups\\\":[{\\\"ruleGroupArn\\\":null,\\\"overrideAction\\\":{\\\"type\\\":\\\"NONE\\\"},\\\"managedRuleGroupIdentifier\\\":{\\\"version\\\":null,\\\"vendorName\\\":\\\"AWS\\\",\\\"managedRuleGroupName\\\":\\\"AWSManagedRulesAmazonIpReputationList\\\"},\\\"ruleGroupType\\\":\\\"ManagedRuleGroup\\\",\\\"excludeRules\\\":[]}],\\\"postProcessRuleGroups\\\":[],\\\"defaultAction\\\":{\\\"type\\\":\\\"ALLOW\\\"},\\\"overrideCustomerWebACLAssociation\\\":false,\\\"loggingConfiguration\\\":{\\\"logDestinationConfigs\\\":[\\\"arn:aws:firehose:us-west-2:12345678912:deliverystream\/aws-waf-logs-fms-admin-destination\\\"],\\\"redactedFields\\\":[{\\\"redactedFieldType\\\":\\\"SingleHeader\\\",\\\"redactedFieldValue\\\":\\\"Cookies\\\"},{\\\"redactedFieldType\\\":\\\"Method\\\"}]}}\"@
    --
    --     In the @loggingConfiguration@, you can specify one
    --     @logDestinationConfigs@, you can optionally provide up to 20
    --     @redactedFields@, and the @RedactedFieldType@ must be one of @URI@,
    --     @QUERY_STRING@, @HEADER@, or @METHOD@.
    --
    -- -   Example: @WAF Classic@
    --
    --     @\"{\\\"type\\\": \\\"WAF\\\", \\\"ruleGroups\\\": [{\\\"id\\\":\\\"12345678-1bcd-9012-efga-0987654321ab\\\", \\\"overrideAction\\\" : {\\\"type\\\": \\\"COUNT\\\"}}], \\\"defaultAction\\\": {\\\"type\\\": \\\"BLOCK\\\"}}\"@
    --
    -- -   Example: @SECURITY_GROUPS_COMMON@
    --
    --     @\"{\\\"type\\\":\\\"SECURITY_GROUPS_COMMON\\\",\\\"revertManualSecurityGroupChanges\\\":false,\\\"exclusiveResourceSecurityGroupManagement\\\":false, \\\"applyToAllEC2InstanceENIs\\\":false,\\\"securityGroups\\\":[{\\\"id\\\":\\\" sg-000e55995d61a06bd\\\"}]}\"@
    --
    -- -   Example: @SECURITY_GROUPS_CONTENT_AUDIT@
    --
    --     @\"{\\\"type\\\":\\\"SECURITY_GROUPS_CONTENT_AUDIT\\\",\\\"securityGroups\\\":[{\\\"id\\\":\\\"sg-000e55995d61a06bd\\\"}],\\\"securityGroupAction\\\":{\\\"type\\\":\\\"ALLOW\\\"}}\"@
    --
    --     The security group action for content audit can be @ALLOW@ or
    --     @DENY@. For @ALLOW@, all in-scope security group rules must be
    --     within the allowed range of the policy\'s security group rules. For
    --     @DENY@, all in-scope security group rules must not contain a value
    --     or a range that matches a rule value or range in the policy security
    --     group.
    --
    -- -   Example: @SECURITY_GROUPS_USAGE_AUDIT@
    --
    --     @\"{\\\"type\\\":\\\"SECURITY_GROUPS_USAGE_AUDIT\\\",\\\"deleteUnusedSecurityGroups\\\":true,\\\"coalesceRedundantSecurityGroups\\\":true}\"@
    managedServiceData :: Prelude.Maybe Prelude.Text,
    -- | The service that the policy is using to protect the resources. This
    -- specifies the type of policy that is created, either an AWS WAF policy,
    -- a Shield Advanced policy, or a security group policy. For security group
    -- policies, Firewall Manager supports one security group for each common
    -- policy and for each content audit policy. This is an adjustable limit
    -- that you can increase by contacting AWS Support.
    type' :: SecurityServiceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SecurityServicePolicyData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'managedServiceData', 'securityServicePolicyData_managedServiceData' - Details about the service that are specific to the service type, in JSON
-- format. For service type @SHIELD_ADVANCED@, this is an empty string.
--
-- -   Example: @NETWORK_FIREWALL@
--
--     @\"{\\\"type\\\":\\\"NETWORK_FIREWALL\\\",\\\"networkFirewallStatelessRuleGroupReferences\\\":[{\\\"resourceARN\\\":\\\"arn:aws:network-firewall:us-west-1:1234567891011:stateless-rulegroup\/rulegroup2\\\",\\\"priority\\\":10}],\\\"networkFirewallStatelessDefaultActions\\\":[\\\"aws:pass\\\",\\\"custom1\\\"],\\\"networkFirewallStatelessFragmentDefaultActions\\\":[\\\"custom2\\\",\\\"aws:pass\\\"],\\\"networkFirewallStatelessCustomActions\\\":[{\\\"actionName\\\":\\\"custom1\\\",\\\"actionDefinition\\\":{\\\"publishMetricAction\\\":{\\\"dimensions\\\":[{\\\"value\\\":\\\"dimension1\\\"}]}}},{\\\"actionName\\\":\\\"custom2\\\",\\\"actionDefinition\\\":{\\\"publishMetricAction\\\":{\\\"dimensions\\\":[{\\\"value\\\":\\\"dimension2\\\"}]}}}],\\\"networkFirewallStatefulRuleGroupReferences\\\":[{\\\"resourceARN\\\":\\\"arn:aws:network-firewall:us-west-1:1234567891011:stateful-rulegroup\/rulegroup1\\\"}],\\\"networkFirewallOrchestrationConfig\\\":{\\\"singleFirewallEndpointPerVPC\\\":true,\\\"allowedIPV4CidrList\\\":[\\\"10.24.34.0\/28\\\"]} }\"@
--
-- -   Example: @WAFV2@
--
--     @\"{\\\"type\\\":\\\"WAFV2\\\",\\\"preProcessRuleGroups\\\":[{\\\"ruleGroupArn\\\":null,\\\"overrideAction\\\":{\\\"type\\\":\\\"NONE\\\"},\\\"managedRuleGroupIdentifier\\\":{\\\"version\\\":null,\\\"vendorName\\\":\\\"AWS\\\",\\\"managedRuleGroupName\\\":\\\"AWSManagedRulesAmazonIpReputationList\\\"},\\\"ruleGroupType\\\":\\\"ManagedRuleGroup\\\",\\\"excludeRules\\\":[]}],\\\"postProcessRuleGroups\\\":[],\\\"defaultAction\\\":{\\\"type\\\":\\\"ALLOW\\\"},\\\"overrideCustomerWebACLAssociation\\\":false,\\\"loggingConfiguration\\\":{\\\"logDestinationConfigs\\\":[\\\"arn:aws:firehose:us-west-2:12345678912:deliverystream\/aws-waf-logs-fms-admin-destination\\\"],\\\"redactedFields\\\":[{\\\"redactedFieldType\\\":\\\"SingleHeader\\\",\\\"redactedFieldValue\\\":\\\"Cookies\\\"},{\\\"redactedFieldType\\\":\\\"Method\\\"}]}}\"@
--
--     In the @loggingConfiguration@, you can specify one
--     @logDestinationConfigs@, you can optionally provide up to 20
--     @redactedFields@, and the @RedactedFieldType@ must be one of @URI@,
--     @QUERY_STRING@, @HEADER@, or @METHOD@.
--
-- -   Example: @WAF Classic@
--
--     @\"{\\\"type\\\": \\\"WAF\\\", \\\"ruleGroups\\\": [{\\\"id\\\":\\\"12345678-1bcd-9012-efga-0987654321ab\\\", \\\"overrideAction\\\" : {\\\"type\\\": \\\"COUNT\\\"}}], \\\"defaultAction\\\": {\\\"type\\\": \\\"BLOCK\\\"}}\"@
--
-- -   Example: @SECURITY_GROUPS_COMMON@
--
--     @\"{\\\"type\\\":\\\"SECURITY_GROUPS_COMMON\\\",\\\"revertManualSecurityGroupChanges\\\":false,\\\"exclusiveResourceSecurityGroupManagement\\\":false, \\\"applyToAllEC2InstanceENIs\\\":false,\\\"securityGroups\\\":[{\\\"id\\\":\\\" sg-000e55995d61a06bd\\\"}]}\"@
--
-- -   Example: @SECURITY_GROUPS_CONTENT_AUDIT@
--
--     @\"{\\\"type\\\":\\\"SECURITY_GROUPS_CONTENT_AUDIT\\\",\\\"securityGroups\\\":[{\\\"id\\\":\\\"sg-000e55995d61a06bd\\\"}],\\\"securityGroupAction\\\":{\\\"type\\\":\\\"ALLOW\\\"}}\"@
--
--     The security group action for content audit can be @ALLOW@ or
--     @DENY@. For @ALLOW@, all in-scope security group rules must be
--     within the allowed range of the policy\'s security group rules. For
--     @DENY@, all in-scope security group rules must not contain a value
--     or a range that matches a rule value or range in the policy security
--     group.
--
-- -   Example: @SECURITY_GROUPS_USAGE_AUDIT@
--
--     @\"{\\\"type\\\":\\\"SECURITY_GROUPS_USAGE_AUDIT\\\",\\\"deleteUnusedSecurityGroups\\\":true,\\\"coalesceRedundantSecurityGroups\\\":true}\"@
--
-- 'type'', 'securityServicePolicyData_type' - The service that the policy is using to protect the resources. This
-- specifies the type of policy that is created, either an AWS WAF policy,
-- a Shield Advanced policy, or a security group policy. For security group
-- policies, Firewall Manager supports one security group for each common
-- policy and for each content audit policy. This is an adjustable limit
-- that you can increase by contacting AWS Support.
newSecurityServicePolicyData ::
  -- | 'type''
  SecurityServiceType ->
  SecurityServicePolicyData
newSecurityServicePolicyData pType_ =
  SecurityServicePolicyData'
    { managedServiceData =
        Prelude.Nothing,
      type' = pType_
    }

-- | Details about the service that are specific to the service type, in JSON
-- format. For service type @SHIELD_ADVANCED@, this is an empty string.
--
-- -   Example: @NETWORK_FIREWALL@
--
--     @\"{\\\"type\\\":\\\"NETWORK_FIREWALL\\\",\\\"networkFirewallStatelessRuleGroupReferences\\\":[{\\\"resourceARN\\\":\\\"arn:aws:network-firewall:us-west-1:1234567891011:stateless-rulegroup\/rulegroup2\\\",\\\"priority\\\":10}],\\\"networkFirewallStatelessDefaultActions\\\":[\\\"aws:pass\\\",\\\"custom1\\\"],\\\"networkFirewallStatelessFragmentDefaultActions\\\":[\\\"custom2\\\",\\\"aws:pass\\\"],\\\"networkFirewallStatelessCustomActions\\\":[{\\\"actionName\\\":\\\"custom1\\\",\\\"actionDefinition\\\":{\\\"publishMetricAction\\\":{\\\"dimensions\\\":[{\\\"value\\\":\\\"dimension1\\\"}]}}},{\\\"actionName\\\":\\\"custom2\\\",\\\"actionDefinition\\\":{\\\"publishMetricAction\\\":{\\\"dimensions\\\":[{\\\"value\\\":\\\"dimension2\\\"}]}}}],\\\"networkFirewallStatefulRuleGroupReferences\\\":[{\\\"resourceARN\\\":\\\"arn:aws:network-firewall:us-west-1:1234567891011:stateful-rulegroup\/rulegroup1\\\"}],\\\"networkFirewallOrchestrationConfig\\\":{\\\"singleFirewallEndpointPerVPC\\\":true,\\\"allowedIPV4CidrList\\\":[\\\"10.24.34.0\/28\\\"]} }\"@
--
-- -   Example: @WAFV2@
--
--     @\"{\\\"type\\\":\\\"WAFV2\\\",\\\"preProcessRuleGroups\\\":[{\\\"ruleGroupArn\\\":null,\\\"overrideAction\\\":{\\\"type\\\":\\\"NONE\\\"},\\\"managedRuleGroupIdentifier\\\":{\\\"version\\\":null,\\\"vendorName\\\":\\\"AWS\\\",\\\"managedRuleGroupName\\\":\\\"AWSManagedRulesAmazonIpReputationList\\\"},\\\"ruleGroupType\\\":\\\"ManagedRuleGroup\\\",\\\"excludeRules\\\":[]}],\\\"postProcessRuleGroups\\\":[],\\\"defaultAction\\\":{\\\"type\\\":\\\"ALLOW\\\"},\\\"overrideCustomerWebACLAssociation\\\":false,\\\"loggingConfiguration\\\":{\\\"logDestinationConfigs\\\":[\\\"arn:aws:firehose:us-west-2:12345678912:deliverystream\/aws-waf-logs-fms-admin-destination\\\"],\\\"redactedFields\\\":[{\\\"redactedFieldType\\\":\\\"SingleHeader\\\",\\\"redactedFieldValue\\\":\\\"Cookies\\\"},{\\\"redactedFieldType\\\":\\\"Method\\\"}]}}\"@
--
--     In the @loggingConfiguration@, you can specify one
--     @logDestinationConfigs@, you can optionally provide up to 20
--     @redactedFields@, and the @RedactedFieldType@ must be one of @URI@,
--     @QUERY_STRING@, @HEADER@, or @METHOD@.
--
-- -   Example: @WAF Classic@
--
--     @\"{\\\"type\\\": \\\"WAF\\\", \\\"ruleGroups\\\": [{\\\"id\\\":\\\"12345678-1bcd-9012-efga-0987654321ab\\\", \\\"overrideAction\\\" : {\\\"type\\\": \\\"COUNT\\\"}}], \\\"defaultAction\\\": {\\\"type\\\": \\\"BLOCK\\\"}}\"@
--
-- -   Example: @SECURITY_GROUPS_COMMON@
--
--     @\"{\\\"type\\\":\\\"SECURITY_GROUPS_COMMON\\\",\\\"revertManualSecurityGroupChanges\\\":false,\\\"exclusiveResourceSecurityGroupManagement\\\":false, \\\"applyToAllEC2InstanceENIs\\\":false,\\\"securityGroups\\\":[{\\\"id\\\":\\\" sg-000e55995d61a06bd\\\"}]}\"@
--
-- -   Example: @SECURITY_GROUPS_CONTENT_AUDIT@
--
--     @\"{\\\"type\\\":\\\"SECURITY_GROUPS_CONTENT_AUDIT\\\",\\\"securityGroups\\\":[{\\\"id\\\":\\\"sg-000e55995d61a06bd\\\"}],\\\"securityGroupAction\\\":{\\\"type\\\":\\\"ALLOW\\\"}}\"@
--
--     The security group action for content audit can be @ALLOW@ or
--     @DENY@. For @ALLOW@, all in-scope security group rules must be
--     within the allowed range of the policy\'s security group rules. For
--     @DENY@, all in-scope security group rules must not contain a value
--     or a range that matches a rule value or range in the policy security
--     group.
--
-- -   Example: @SECURITY_GROUPS_USAGE_AUDIT@
--
--     @\"{\\\"type\\\":\\\"SECURITY_GROUPS_USAGE_AUDIT\\\",\\\"deleteUnusedSecurityGroups\\\":true,\\\"coalesceRedundantSecurityGroups\\\":true}\"@
securityServicePolicyData_managedServiceData :: Lens.Lens' SecurityServicePolicyData (Prelude.Maybe Prelude.Text)
securityServicePolicyData_managedServiceData = Lens.lens (\SecurityServicePolicyData' {managedServiceData} -> managedServiceData) (\s@SecurityServicePolicyData' {} a -> s {managedServiceData = a} :: SecurityServicePolicyData)

-- | The service that the policy is using to protect the resources. This
-- specifies the type of policy that is created, either an AWS WAF policy,
-- a Shield Advanced policy, or a security group policy. For security group
-- policies, Firewall Manager supports one security group for each common
-- policy and for each content audit policy. This is an adjustable limit
-- that you can increase by contacting AWS Support.
securityServicePolicyData_type :: Lens.Lens' SecurityServicePolicyData SecurityServiceType
securityServicePolicyData_type = Lens.lens (\SecurityServicePolicyData' {type'} -> type') (\s@SecurityServicePolicyData' {} a -> s {type' = a} :: SecurityServicePolicyData)

instance Prelude.FromJSON SecurityServicePolicyData where
  parseJSON =
    Prelude.withObject
      "SecurityServicePolicyData"
      ( \x ->
          SecurityServicePolicyData'
            Prelude.<$> (x Prelude..:? "ManagedServiceData")
            Prelude.<*> (x Prelude..: "Type")
      )

instance Prelude.Hashable SecurityServicePolicyData

instance Prelude.NFData SecurityServicePolicyData

instance Prelude.ToJSON SecurityServicePolicyData where
  toJSON SecurityServicePolicyData' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ManagedServiceData" Prelude..=)
              Prelude.<$> managedServiceData,
            Prelude.Just ("Type" Prelude..= type')
          ]
      )
