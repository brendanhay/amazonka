{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

newtype SecurityServiceType = SecurityServiceType'
  { fromSecurityServiceType ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
