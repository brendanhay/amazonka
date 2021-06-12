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
-- Module      : Network.AWS.FMS.Types.SecurityServiceType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.SecurityServiceType
  ( SecurityServiceType
      ( ..,
        SecurityServiceType_NETWORK_FIREWALL,
        SecurityServiceType_SECURITY_GROUPS_COMMON,
        SecurityServiceType_SECURITY_GROUPS_CONTENT_AUDIT,
        SecurityServiceType_SECURITY_GROUPS_USAGE_AUDIT,
        SecurityServiceType_SHIELD_ADVANCED,
        SecurityServiceType_WAF,
        SecurityServiceType_WAFV2
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype SecurityServiceType = SecurityServiceType'
  { fromSecurityServiceType ::
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

pattern SecurityServiceType_NETWORK_FIREWALL :: SecurityServiceType
pattern SecurityServiceType_NETWORK_FIREWALL = SecurityServiceType' "NETWORK_FIREWALL"

pattern SecurityServiceType_SECURITY_GROUPS_COMMON :: SecurityServiceType
pattern SecurityServiceType_SECURITY_GROUPS_COMMON = SecurityServiceType' "SECURITY_GROUPS_COMMON"

pattern SecurityServiceType_SECURITY_GROUPS_CONTENT_AUDIT :: SecurityServiceType
pattern SecurityServiceType_SECURITY_GROUPS_CONTENT_AUDIT = SecurityServiceType' "SECURITY_GROUPS_CONTENT_AUDIT"

pattern SecurityServiceType_SECURITY_GROUPS_USAGE_AUDIT :: SecurityServiceType
pattern SecurityServiceType_SECURITY_GROUPS_USAGE_AUDIT = SecurityServiceType' "SECURITY_GROUPS_USAGE_AUDIT"

pattern SecurityServiceType_SHIELD_ADVANCED :: SecurityServiceType
pattern SecurityServiceType_SHIELD_ADVANCED = SecurityServiceType' "SHIELD_ADVANCED"

pattern SecurityServiceType_WAF :: SecurityServiceType
pattern SecurityServiceType_WAF = SecurityServiceType' "WAF"

pattern SecurityServiceType_WAFV2 :: SecurityServiceType
pattern SecurityServiceType_WAFV2 = SecurityServiceType' "WAFV2"

{-# COMPLETE
  SecurityServiceType_NETWORK_FIREWALL,
  SecurityServiceType_SECURITY_GROUPS_COMMON,
  SecurityServiceType_SECURITY_GROUPS_CONTENT_AUDIT,
  SecurityServiceType_SECURITY_GROUPS_USAGE_AUDIT,
  SecurityServiceType_SHIELD_ADVANCED,
  SecurityServiceType_WAF,
  SecurityServiceType_WAFV2,
  SecurityServiceType'
  #-}
