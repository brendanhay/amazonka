{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.ViolationReason
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.ViolationReason where

import Network.AWS.Prelude

data ViolationReason
  = MissingExpectedRouteTable
  | MissingFirewall
  | MissingFirewallSubnetInAz
  | NetworkFirewallPolicyModified
  | ResourceIncorrectWebACL
  | ResourceMissingSecurityGroup
  | ResourceMissingShieldProtection
  | ResourceMissingWebACL
  | ResourceMissingWebACLOrShieldProtection
  | ResourceViolatesAuditSecurityGroup
  | SecurityGroupRedundant
  | SecurityGroupUnused
  | WebACLMissingRuleGroup
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText ViolationReason where
  parser =
    takeLowerText >>= \case
      "missing_expected_route_table" -> pure MissingExpectedRouteTable
      "missing_firewall" -> pure MissingFirewall
      "missing_firewall_subnet_in_az" -> pure MissingFirewallSubnetInAz
      "network_firewall_policy_modified" -> pure NetworkFirewallPolicyModified
      "resource_incorrect_web_acl" -> pure ResourceIncorrectWebACL
      "resource_missing_security_group" -> pure ResourceMissingSecurityGroup
      "resource_missing_shield_protection" -> pure ResourceMissingShieldProtection
      "resource_missing_web_acl" -> pure ResourceMissingWebACL
      "resource_missing_web_acl_or_shield_protection" -> pure ResourceMissingWebACLOrShieldProtection
      "resource_violates_audit_security_group" -> pure ResourceViolatesAuditSecurityGroup
      "security_group_redundant" -> pure SecurityGroupRedundant
      "security_group_unused" -> pure SecurityGroupUnused
      "web_acl_missing_rule_group" -> pure WebACLMissingRuleGroup
      e ->
        fromTextError $
          "Failure parsing ViolationReason from value: '" <> e
            <> "'. Accepted values: missing_expected_route_table, missing_firewall, missing_firewall_subnet_in_az, network_firewall_policy_modified, resource_incorrect_web_acl, resource_missing_security_group, resource_missing_shield_protection, resource_missing_web_acl, resource_missing_web_acl_or_shield_protection, resource_violates_audit_security_group, security_group_redundant, security_group_unused, web_acl_missing_rule_group"

instance ToText ViolationReason where
  toText = \case
    MissingExpectedRouteTable -> "MISSING_EXPECTED_ROUTE_TABLE"
    MissingFirewall -> "MISSING_FIREWALL"
    MissingFirewallSubnetInAz -> "MISSING_FIREWALL_SUBNET_IN_AZ"
    NetworkFirewallPolicyModified -> "NETWORK_FIREWALL_POLICY_MODIFIED"
    ResourceIncorrectWebACL -> "RESOURCE_INCORRECT_WEB_ACL"
    ResourceMissingSecurityGroup -> "RESOURCE_MISSING_SECURITY_GROUP"
    ResourceMissingShieldProtection -> "RESOURCE_MISSING_SHIELD_PROTECTION"
    ResourceMissingWebACL -> "RESOURCE_MISSING_WEB_ACL"
    ResourceMissingWebACLOrShieldProtection -> "RESOURCE_MISSING_WEB_ACL_OR_SHIELD_PROTECTION"
    ResourceViolatesAuditSecurityGroup -> "RESOURCE_VIOLATES_AUDIT_SECURITY_GROUP"
    SecurityGroupRedundant -> "SECURITY_GROUP_REDUNDANT"
    SecurityGroupUnused -> "SECURITY_GROUP_UNUSED"
    WebACLMissingRuleGroup -> "WEB_ACL_MISSING_RULE_GROUP"

instance Hashable ViolationReason

instance NFData ViolationReason

instance ToByteString ViolationReason

instance ToQuery ViolationReason

instance ToHeader ViolationReason

instance FromJSON ViolationReason where
  parseJSON = parseJSONText "ViolationReason"
