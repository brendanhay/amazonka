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
-- Module      : Amazonka.FMS.Types.SecurityServiceType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.SecurityServiceType
  ( SecurityServiceType
      ( ..,
        SecurityServiceType_DNS_FIREWALL,
        SecurityServiceType_IMPORT_NETWORK_FIREWALL,
        SecurityServiceType_NETWORK_FIREWALL,
        SecurityServiceType_SECURITY_GROUPS_COMMON,
        SecurityServiceType_SECURITY_GROUPS_CONTENT_AUDIT,
        SecurityServiceType_SECURITY_GROUPS_USAGE_AUDIT,
        SecurityServiceType_SHIELD_ADVANCED,
        SecurityServiceType_THIRD_PARTY_FIREWALL,
        SecurityServiceType_WAF,
        SecurityServiceType_WAFV2
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype SecurityServiceType = SecurityServiceType'
  { fromSecurityServiceType ::
      Core.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
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

pattern SecurityServiceType_DNS_FIREWALL :: SecurityServiceType
pattern SecurityServiceType_DNS_FIREWALL = SecurityServiceType' "DNS_FIREWALL"

pattern SecurityServiceType_IMPORT_NETWORK_FIREWALL :: SecurityServiceType
pattern SecurityServiceType_IMPORT_NETWORK_FIREWALL = SecurityServiceType' "IMPORT_NETWORK_FIREWALL"

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

pattern SecurityServiceType_THIRD_PARTY_FIREWALL :: SecurityServiceType
pattern SecurityServiceType_THIRD_PARTY_FIREWALL = SecurityServiceType' "THIRD_PARTY_FIREWALL"

pattern SecurityServiceType_WAF :: SecurityServiceType
pattern SecurityServiceType_WAF = SecurityServiceType' "WAF"

pattern SecurityServiceType_WAFV2 :: SecurityServiceType
pattern SecurityServiceType_WAFV2 = SecurityServiceType' "WAFV2"

{-# COMPLETE
  SecurityServiceType_DNS_FIREWALL,
  SecurityServiceType_IMPORT_NETWORK_FIREWALL,
  SecurityServiceType_NETWORK_FIREWALL,
  SecurityServiceType_SECURITY_GROUPS_COMMON,
  SecurityServiceType_SECURITY_GROUPS_CONTENT_AUDIT,
  SecurityServiceType_SECURITY_GROUPS_USAGE_AUDIT,
  SecurityServiceType_SHIELD_ADVANCED,
  SecurityServiceType_THIRD_PARTY_FIREWALL,
  SecurityServiceType_WAF,
  SecurityServiceType_WAFV2,
  SecurityServiceType'
  #-}
