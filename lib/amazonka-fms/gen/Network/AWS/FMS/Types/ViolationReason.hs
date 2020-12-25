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
        ViolationReasonWebAclMissingRuleGroup,
        ViolationReasonResourceMissingWebAcl,
        ViolationReasonResourceIncorrectWebAcl,
        ViolationReasonResourceMissingShieldProtection,
        ViolationReasonResourceMissingWebAclOrShieldProtection,
        ViolationReasonResourceMissingSecurityGroup,
        ViolationReasonResourceViolatesAuditSecurityGroup,
        ViolationReasonSecurityGroupUnused,
        ViolationReasonSecurityGroupRedundant,
        ViolationReasonMissingFirewall,
        ViolationReasonMissingFirewallSubnetInAz,
        ViolationReasonMissingExpectedRouteTable,
        ViolationReasonNetworkFirewallPolicyModified,
        fromViolationReason
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ViolationReason = ViolationReason'
  { fromViolationReason ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern ViolationReasonWebAclMissingRuleGroup :: ViolationReason
pattern ViolationReasonWebAclMissingRuleGroup = ViolationReason' "WEB_ACL_MISSING_RULE_GROUP"

pattern ViolationReasonResourceMissingWebAcl :: ViolationReason
pattern ViolationReasonResourceMissingWebAcl = ViolationReason' "RESOURCE_MISSING_WEB_ACL"

pattern ViolationReasonResourceIncorrectWebAcl :: ViolationReason
pattern ViolationReasonResourceIncorrectWebAcl = ViolationReason' "RESOURCE_INCORRECT_WEB_ACL"

pattern ViolationReasonResourceMissingShieldProtection :: ViolationReason
pattern ViolationReasonResourceMissingShieldProtection = ViolationReason' "RESOURCE_MISSING_SHIELD_PROTECTION"

pattern ViolationReasonResourceMissingWebAclOrShieldProtection :: ViolationReason
pattern ViolationReasonResourceMissingWebAclOrShieldProtection = ViolationReason' "RESOURCE_MISSING_WEB_ACL_OR_SHIELD_PROTECTION"

pattern ViolationReasonResourceMissingSecurityGroup :: ViolationReason
pattern ViolationReasonResourceMissingSecurityGroup = ViolationReason' "RESOURCE_MISSING_SECURITY_GROUP"

pattern ViolationReasonResourceViolatesAuditSecurityGroup :: ViolationReason
pattern ViolationReasonResourceViolatesAuditSecurityGroup = ViolationReason' "RESOURCE_VIOLATES_AUDIT_SECURITY_GROUP"

pattern ViolationReasonSecurityGroupUnused :: ViolationReason
pattern ViolationReasonSecurityGroupUnused = ViolationReason' "SECURITY_GROUP_UNUSED"

pattern ViolationReasonSecurityGroupRedundant :: ViolationReason
pattern ViolationReasonSecurityGroupRedundant = ViolationReason' "SECURITY_GROUP_REDUNDANT"

pattern ViolationReasonMissingFirewall :: ViolationReason
pattern ViolationReasonMissingFirewall = ViolationReason' "MISSING_FIREWALL"

pattern ViolationReasonMissingFirewallSubnetInAz :: ViolationReason
pattern ViolationReasonMissingFirewallSubnetInAz = ViolationReason' "MISSING_FIREWALL_SUBNET_IN_AZ"

pattern ViolationReasonMissingExpectedRouteTable :: ViolationReason
pattern ViolationReasonMissingExpectedRouteTable = ViolationReason' "MISSING_EXPECTED_ROUTE_TABLE"

pattern ViolationReasonNetworkFirewallPolicyModified :: ViolationReason
pattern ViolationReasonNetworkFirewallPolicyModified = ViolationReason' "NETWORK_FIREWALL_POLICY_MODIFIED"

{-# COMPLETE
  ViolationReasonWebAclMissingRuleGroup,
  ViolationReasonResourceMissingWebAcl,
  ViolationReasonResourceIncorrectWebAcl,
  ViolationReasonResourceMissingShieldProtection,
  ViolationReasonResourceMissingWebAclOrShieldProtection,
  ViolationReasonResourceMissingSecurityGroup,
  ViolationReasonResourceViolatesAuditSecurityGroup,
  ViolationReasonSecurityGroupUnused,
  ViolationReasonSecurityGroupRedundant,
  ViolationReasonMissingFirewall,
  ViolationReasonMissingFirewallSubnetInAz,
  ViolationReasonMissingExpectedRouteTable,
  ViolationReasonNetworkFirewallPolicyModified,
  ViolationReason'
  #-}
