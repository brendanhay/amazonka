{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.GameServerProtectionPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.GameServerProtectionPolicy where

import Network.AWS.Prelude

data GameServerProtectionPolicy
  = FullProtection
  | NoProtection
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

instance FromText GameServerProtectionPolicy where
  parser =
    takeLowerText >>= \case
      "full_protection" -> pure FullProtection
      "no_protection" -> pure NoProtection
      e ->
        fromTextError $
          "Failure parsing GameServerProtectionPolicy from value: '" <> e
            <> "'. Accepted values: full_protection, no_protection"

instance ToText GameServerProtectionPolicy where
  toText = \case
    FullProtection -> "FULL_PROTECTION"
    NoProtection -> "NO_PROTECTION"

instance Hashable GameServerProtectionPolicy

instance NFData GameServerProtectionPolicy

instance ToByteString GameServerProtectionPolicy

instance ToQuery GameServerProtectionPolicy

instance ToHeader GameServerProtectionPolicy

instance ToJSON GameServerProtectionPolicy where
  toJSON = toJSONText

instance FromJSON GameServerProtectionPolicy where
  parseJSON = parseJSONText "GameServerProtectionPolicy"
