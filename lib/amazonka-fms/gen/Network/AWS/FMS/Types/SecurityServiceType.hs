{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        Waf,
        WAFV2,
        ShieldAdvanced,
        SecurityGroupsCommon,
        SecurityGroupsContentAudit,
        SecurityGroupsUsageAudit,
        NetworkFirewall
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

pattern Waf :: SecurityServiceType
pattern Waf = SecurityServiceType' "WAF"

pattern WAFV2 :: SecurityServiceType
pattern WAFV2 = SecurityServiceType' "WAFV2"

pattern ShieldAdvanced :: SecurityServiceType
pattern ShieldAdvanced = SecurityServiceType' "SHIELD_ADVANCED"

pattern SecurityGroupsCommon :: SecurityServiceType
pattern SecurityGroupsCommon = SecurityServiceType' "SECURITY_GROUPS_COMMON"

pattern SecurityGroupsContentAudit :: SecurityServiceType
pattern SecurityGroupsContentAudit = SecurityServiceType' "SECURITY_GROUPS_CONTENT_AUDIT"

pattern SecurityGroupsUsageAudit :: SecurityServiceType
pattern SecurityGroupsUsageAudit = SecurityServiceType' "SECURITY_GROUPS_USAGE_AUDIT"

pattern NetworkFirewall :: SecurityServiceType
pattern NetworkFirewall = SecurityServiceType' "NETWORK_FIREWALL"

{-# COMPLETE
  Waf,
  WAFV2,
  ShieldAdvanced,
  SecurityGroupsCommon,
  SecurityGroupsContentAudit,
  SecurityGroupsUsageAudit,
  NetworkFirewall,
  SecurityServiceType'
  #-}
