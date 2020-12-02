{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.DefaultAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.DefaultAction where

import Network.AWS.Prelude

data DefaultAction
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

instance FromText DefaultAction where
  parser =
    takeLowerText >>= \case
      "allow" -> pure Allow
      "deny" -> pure Deny
      e ->
        fromTextError $
          "Failure parsing DefaultAction from value: '" <> e
            <> "'. Accepted values: allow, deny"

instance ToText DefaultAction where
  toText = \case
    Allow -> "ALLOW"
    Deny -> "DENY"

instance Hashable DefaultAction

instance NFData DefaultAction

instance ToByteString DefaultAction

instance ToQuery DefaultAction

instance ToHeader DefaultAction

instance ToJSON DefaultAction where
  toJSON = toJSONText

instance FromJSON DefaultAction where
  parseJSON = parseJSONText "DefaultAction"
