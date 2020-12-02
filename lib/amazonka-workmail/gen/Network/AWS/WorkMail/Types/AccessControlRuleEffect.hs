{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.Types.AccessControlRuleEffect
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkMail.Types.AccessControlRuleEffect where

import Network.AWS.Prelude

data AccessControlRuleEffect
  = Allow
  | Deny
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

instance FromText AccessControlRuleEffect where
  parser =
    takeLowerText >>= \case
      "allow" -> pure Allow
      "deny" -> pure Deny
      e ->
        fromTextError $
          "Failure parsing AccessControlRuleEffect from value: '" <> e
            <> "'. Accepted values: allow, deny"

instance ToText AccessControlRuleEffect where
  toText = \case
    Allow -> "ALLOW"
    Deny -> "DENY"

instance Hashable AccessControlRuleEffect

instance NFData AccessControlRuleEffect

instance ToByteString AccessControlRuleEffect

instance ToQuery AccessControlRuleEffect

instance ToHeader AccessControlRuleEffect

instance ToJSON AccessControlRuleEffect where
  toJSON = toJSONText

instance FromJSON AccessControlRuleEffect where
  parseJSON = parseJSONText "AccessControlRuleEffect"
