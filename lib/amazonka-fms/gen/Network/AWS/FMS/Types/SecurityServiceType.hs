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
        SecurityServiceTypeWaf,
        SecurityServiceTypeWAFV2,
        SecurityServiceTypeShieldAdvanced,
        SecurityServiceTypeSecurityGroupsCommon,
        SecurityServiceTypeSecurityGroupsContentAudit,
        SecurityServiceTypeSecurityGroupsUsageAudit,
        SecurityServiceTypeNetworkFirewall,
        fromSecurityServiceType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype SecurityServiceType = SecurityServiceType'
  { fromSecurityServiceType ::
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

pattern SecurityServiceTypeWaf :: SecurityServiceType
pattern SecurityServiceTypeWaf = SecurityServiceType' "WAF"

pattern SecurityServiceTypeWAFV2 :: SecurityServiceType
pattern SecurityServiceTypeWAFV2 = SecurityServiceType' "WAFV2"

pattern SecurityServiceTypeShieldAdvanced :: SecurityServiceType
pattern SecurityServiceTypeShieldAdvanced = SecurityServiceType' "SHIELD_ADVANCED"

pattern SecurityServiceTypeSecurityGroupsCommon :: SecurityServiceType
pattern SecurityServiceTypeSecurityGroupsCommon = SecurityServiceType' "SECURITY_GROUPS_COMMON"

pattern SecurityServiceTypeSecurityGroupsContentAudit :: SecurityServiceType
pattern SecurityServiceTypeSecurityGroupsContentAudit = SecurityServiceType' "SECURITY_GROUPS_CONTENT_AUDIT"

pattern SecurityServiceTypeSecurityGroupsUsageAudit :: SecurityServiceType
pattern SecurityServiceTypeSecurityGroupsUsageAudit = SecurityServiceType' "SECURITY_GROUPS_USAGE_AUDIT"

pattern SecurityServiceTypeNetworkFirewall :: SecurityServiceType
pattern SecurityServiceTypeNetworkFirewall = SecurityServiceType' "NETWORK_FIREWALL"

{-# COMPLETE
  SecurityServiceTypeWaf,
  SecurityServiceTypeWAFV2,
  SecurityServiceTypeShieldAdvanced,
  SecurityServiceTypeSecurityGroupsCommon,
  SecurityServiceTypeSecurityGroupsContentAudit,
  SecurityServiceTypeSecurityGroupsUsageAudit,
  SecurityServiceTypeNetworkFirewall,
  SecurityServiceType'
  #-}
