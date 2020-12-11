-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.SecurityServiceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.SecurityServiceType
  ( SecurityServiceType
      ( SecurityServiceType',
        NetworkFirewall,
        SecurityGroupsCommon,
        SecurityGroupsContentAudit,
        SecurityGroupsUsageAudit,
        ShieldAdvanced,
        WAFV2,
        Waf
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype SecurityServiceType = SecurityServiceType' Lude.Text
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

pattern NetworkFirewall :: SecurityServiceType
pattern NetworkFirewall = SecurityServiceType' "NETWORK_FIREWALL"

pattern SecurityGroupsCommon :: SecurityServiceType
pattern SecurityGroupsCommon = SecurityServiceType' "SECURITY_GROUPS_COMMON"

pattern SecurityGroupsContentAudit :: SecurityServiceType
pattern SecurityGroupsContentAudit = SecurityServiceType' "SECURITY_GROUPS_CONTENT_AUDIT"

pattern SecurityGroupsUsageAudit :: SecurityServiceType
pattern SecurityGroupsUsageAudit = SecurityServiceType' "SECURITY_GROUPS_USAGE_AUDIT"

pattern ShieldAdvanced :: SecurityServiceType
pattern ShieldAdvanced = SecurityServiceType' "SHIELD_ADVANCED"

pattern WAFV2 :: SecurityServiceType
pattern WAFV2 = SecurityServiceType' "WAFV2"

pattern Waf :: SecurityServiceType
pattern Waf = SecurityServiceType' "WAF"

{-# COMPLETE
  NetworkFirewall,
  SecurityGroupsCommon,
  SecurityGroupsContentAudit,
  SecurityGroupsUsageAudit,
  ShieldAdvanced,
  WAFV2,
  Waf,
  SecurityServiceType'
  #-}
