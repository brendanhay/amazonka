{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.SecurityServiceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.SecurityServiceType where

import Network.AWS.Prelude

data SecurityServiceType
  = NetworkFirewall
  | SecurityGroupsCommon
  | SecurityGroupsContentAudit
  | SecurityGroupsUsageAudit
  | ShieldAdvanced
  | WAFV2
  | Waf
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

instance FromText SecurityServiceType where
  parser =
    takeLowerText >>= \case
      "network_firewall" -> pure NetworkFirewall
      "security_groups_common" -> pure SecurityGroupsCommon
      "security_groups_content_audit" -> pure SecurityGroupsContentAudit
      "security_groups_usage_audit" -> pure SecurityGroupsUsageAudit
      "shield_advanced" -> pure ShieldAdvanced
      "wafv2" -> pure WAFV2
      "waf" -> pure Waf
      e ->
        fromTextError $
          "Failure parsing SecurityServiceType from value: '" <> e
            <> "'. Accepted values: network_firewall, security_groups_common, security_groups_content_audit, security_groups_usage_audit, shield_advanced, wafv2, waf"

instance ToText SecurityServiceType where
  toText = \case
    NetworkFirewall -> "NETWORK_FIREWALL"
    SecurityGroupsCommon -> "SECURITY_GROUPS_COMMON"
    SecurityGroupsContentAudit -> "SECURITY_GROUPS_CONTENT_AUDIT"
    SecurityGroupsUsageAudit -> "SECURITY_GROUPS_USAGE_AUDIT"
    ShieldAdvanced -> "SHIELD_ADVANCED"
    WAFV2 -> "WAFV2"
    Waf -> "WAF"

instance Hashable SecurityServiceType

instance NFData SecurityServiceType

instance ToByteString SecurityServiceType

instance ToQuery SecurityServiceType

instance ToHeader SecurityServiceType

instance ToJSON SecurityServiceType where
  toJSON = toJSONText

instance FromJSON SecurityServiceType where
  parseJSON = parseJSONText "SecurityServiceType"
