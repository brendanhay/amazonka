{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.IAMUserAccessToBilling
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.IAMUserAccessToBilling where

import Network.AWS.Prelude

data IAMUserAccessToBilling
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

instance FromText IAMUserAccessToBilling where
  parser =
    takeLowerText >>= \case
      "allow" -> pure Allow
      "deny" -> pure Deny
      e ->
        fromTextError $
          "Failure parsing IAMUserAccessToBilling from value: '" <> e
            <> "'. Accepted values: allow, deny"

instance ToText IAMUserAccessToBilling where
  toText = \case
    Allow -> "ALLOW"
    Deny -> "DENY"

instance Hashable IAMUserAccessToBilling

instance NFData IAMUserAccessToBilling

instance ToByteString IAMUserAccessToBilling

instance ToQuery IAMUserAccessToBilling

instance ToHeader IAMUserAccessToBilling

instance ToJSON IAMUserAccessToBilling where
  toJSON = toJSONText
