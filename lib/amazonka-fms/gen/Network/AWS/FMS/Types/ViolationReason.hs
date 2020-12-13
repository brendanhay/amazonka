{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.ViolationReason
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.ViolationReason
  ( ViolationReason
      ( ViolationReason',
        WebACLMissingRuleGroup,
        ResourceMissingWebACL,
        ResourceIncorrectWebACL,
        ResourceMissingShieldProtection,
        ResourceMissingWebACLOrShieldProtection,
        ResourceMissingSecurityGroup,
        ResourceViolatesAuditSecurityGroup,
        SecurityGroupUnused,
        SecurityGroupRedundant,
        MissingFirewall,
        MissingFirewallSubnetInAz,
        MissingExpectedRouteTable,
        NetworkFirewallPolicyModified
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ViolationReason = ViolationReason' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern WebACLMissingRuleGroup :: ViolationReason
pattern WebACLMissingRuleGroup = ViolationReason' "WEB_ACL_MISSING_RULE_GROUP"

pattern ResourceMissingWebACL :: ViolationReason
pattern ResourceMissingWebACL = ViolationReason' "RESOURCE_MISSING_WEB_ACL"

pattern ResourceIncorrectWebACL :: ViolationReason
pattern ResourceIncorrectWebACL = ViolationReason' "RESOURCE_INCORRECT_WEB_ACL"

pattern ResourceMissingShieldProtection :: ViolationReason
pattern ResourceMissingShieldProtection = ViolationReason' "RESOURCE_MISSING_SHIELD_PROTECTION"

pattern ResourceMissingWebACLOrShieldProtection :: ViolationReason
pattern ResourceMissingWebACLOrShieldProtection = ViolationReason' "RESOURCE_MISSING_WEB_ACL_OR_SHIELD_PROTECTION"

pattern ResourceMissingSecurityGroup :: ViolationReason
pattern ResourceMissingSecurityGroup = ViolationReason' "RESOURCE_MISSING_SECURITY_GROUP"

pattern ResourceViolatesAuditSecurityGroup :: ViolationReason
pattern ResourceViolatesAuditSecurityGroup = ViolationReason' "RESOURCE_VIOLATES_AUDIT_SECURITY_GROUP"

pattern SecurityGroupUnused :: ViolationReason
pattern SecurityGroupUnused = ViolationReason' "SECURITY_GROUP_UNUSED"

pattern SecurityGroupRedundant :: ViolationReason
pattern SecurityGroupRedundant = ViolationReason' "SECURITY_GROUP_REDUNDANT"

pattern MissingFirewall :: ViolationReason
pattern MissingFirewall = ViolationReason' "MISSING_FIREWALL"

pattern MissingFirewallSubnetInAz :: ViolationReason
pattern MissingFirewallSubnetInAz = ViolationReason' "MISSING_FIREWALL_SUBNET_IN_AZ"

pattern MissingExpectedRouteTable :: ViolationReason
pattern MissingExpectedRouteTable = ViolationReason' "MISSING_EXPECTED_ROUTE_TABLE"

pattern NetworkFirewallPolicyModified :: ViolationReason
pattern NetworkFirewallPolicyModified = ViolationReason' "NETWORK_FIREWALL_POLICY_MODIFIED"

{-# COMPLETE
  WebACLMissingRuleGroup,
  ResourceMissingWebACL,
  ResourceIncorrectWebACL,
  ResourceMissingShieldProtection,
  ResourceMissingWebACLOrShieldProtection,
  ResourceMissingSecurityGroup,
  ResourceViolatesAuditSecurityGroup,
  SecurityGroupUnused,
  SecurityGroupRedundant,
  MissingFirewall,
  MissingFirewallSubnetInAz,
  MissingExpectedRouteTable,
  NetworkFirewallPolicyModified,
  ViolationReason'
  #-}
