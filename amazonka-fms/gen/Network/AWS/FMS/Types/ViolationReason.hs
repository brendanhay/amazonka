{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.ViolationReason
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.ViolationReason
  ( ViolationReason
      ( ..,
        ViolationReason_MISSING_EXPECTED_ROUTE_TABLE,
        ViolationReason_MISSING_FIREWALL,
        ViolationReason_MISSING_FIREWALL_SUBNET_IN_AZ,
        ViolationReason_NETWORK_FIREWALL_POLICY_MODIFIED,
        ViolationReason_RESOURCE_INCORRECT_WEB_ACL,
        ViolationReason_RESOURCE_MISSING_SECURITY_GROUP,
        ViolationReason_RESOURCE_MISSING_SHIELD_PROTECTION,
        ViolationReason_RESOURCE_MISSING_WEB_ACL,
        ViolationReason_RESOURCE_MISSING_WEB_ACL_OR_SHIELD_PROTECTION,
        ViolationReason_RESOURCE_VIOLATES_AUDIT_SECURITY_GROUP,
        ViolationReason_SECURITY_GROUP_REDUNDANT,
        ViolationReason_SECURITY_GROUP_UNUSED,
        ViolationReason_WEB_ACL_MISSING_RULE_GROUP
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ViolationReason = ViolationReason'
  { fromViolationReason ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern ViolationReason_MISSING_EXPECTED_ROUTE_TABLE :: ViolationReason
pattern ViolationReason_MISSING_EXPECTED_ROUTE_TABLE = ViolationReason' "MISSING_EXPECTED_ROUTE_TABLE"

pattern ViolationReason_MISSING_FIREWALL :: ViolationReason
pattern ViolationReason_MISSING_FIREWALL = ViolationReason' "MISSING_FIREWALL"

pattern ViolationReason_MISSING_FIREWALL_SUBNET_IN_AZ :: ViolationReason
pattern ViolationReason_MISSING_FIREWALL_SUBNET_IN_AZ = ViolationReason' "MISSING_FIREWALL_SUBNET_IN_AZ"

pattern ViolationReason_NETWORK_FIREWALL_POLICY_MODIFIED :: ViolationReason
pattern ViolationReason_NETWORK_FIREWALL_POLICY_MODIFIED = ViolationReason' "NETWORK_FIREWALL_POLICY_MODIFIED"

pattern ViolationReason_RESOURCE_INCORRECT_WEB_ACL :: ViolationReason
pattern ViolationReason_RESOURCE_INCORRECT_WEB_ACL = ViolationReason' "RESOURCE_INCORRECT_WEB_ACL"

pattern ViolationReason_RESOURCE_MISSING_SECURITY_GROUP :: ViolationReason
pattern ViolationReason_RESOURCE_MISSING_SECURITY_GROUP = ViolationReason' "RESOURCE_MISSING_SECURITY_GROUP"

pattern ViolationReason_RESOURCE_MISSING_SHIELD_PROTECTION :: ViolationReason
pattern ViolationReason_RESOURCE_MISSING_SHIELD_PROTECTION = ViolationReason' "RESOURCE_MISSING_SHIELD_PROTECTION"

pattern ViolationReason_RESOURCE_MISSING_WEB_ACL :: ViolationReason
pattern ViolationReason_RESOURCE_MISSING_WEB_ACL = ViolationReason' "RESOURCE_MISSING_WEB_ACL"

pattern ViolationReason_RESOURCE_MISSING_WEB_ACL_OR_SHIELD_PROTECTION :: ViolationReason
pattern ViolationReason_RESOURCE_MISSING_WEB_ACL_OR_SHIELD_PROTECTION = ViolationReason' "RESOURCE_MISSING_WEB_ACL_OR_SHIELD_PROTECTION"

pattern ViolationReason_RESOURCE_VIOLATES_AUDIT_SECURITY_GROUP :: ViolationReason
pattern ViolationReason_RESOURCE_VIOLATES_AUDIT_SECURITY_GROUP = ViolationReason' "RESOURCE_VIOLATES_AUDIT_SECURITY_GROUP"

pattern ViolationReason_SECURITY_GROUP_REDUNDANT :: ViolationReason
pattern ViolationReason_SECURITY_GROUP_REDUNDANT = ViolationReason' "SECURITY_GROUP_REDUNDANT"

pattern ViolationReason_SECURITY_GROUP_UNUSED :: ViolationReason
pattern ViolationReason_SECURITY_GROUP_UNUSED = ViolationReason' "SECURITY_GROUP_UNUSED"

pattern ViolationReason_WEB_ACL_MISSING_RULE_GROUP :: ViolationReason
pattern ViolationReason_WEB_ACL_MISSING_RULE_GROUP = ViolationReason' "WEB_ACL_MISSING_RULE_GROUP"

{-# COMPLETE
  ViolationReason_MISSING_EXPECTED_ROUTE_TABLE,
  ViolationReason_MISSING_FIREWALL,
  ViolationReason_MISSING_FIREWALL_SUBNET_IN_AZ,
  ViolationReason_NETWORK_FIREWALL_POLICY_MODIFIED,
  ViolationReason_RESOURCE_INCORRECT_WEB_ACL,
  ViolationReason_RESOURCE_MISSING_SECURITY_GROUP,
  ViolationReason_RESOURCE_MISSING_SHIELD_PROTECTION,
  ViolationReason_RESOURCE_MISSING_WEB_ACL,
  ViolationReason_RESOURCE_MISSING_WEB_ACL_OR_SHIELD_PROTECTION,
  ViolationReason_RESOURCE_VIOLATES_AUDIT_SECURITY_GROUP,
  ViolationReason_SECURITY_GROUP_REDUNDANT,
  ViolationReason_SECURITY_GROUP_UNUSED,
  ViolationReason_WEB_ACL_MISSING_RULE_GROUP,
  ViolationReason'
  #-}
