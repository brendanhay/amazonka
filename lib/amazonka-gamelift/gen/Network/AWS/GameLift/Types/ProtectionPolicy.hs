{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.ProtectionPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.ProtectionPolicy where

import Network.AWS.Prelude

data ProtectionPolicy
  = PPFullProtection
  | PPNoProtection
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

instance FromText ProtectionPolicy where
  parser =
    takeLowerText >>= \case
      "fullprotection" -> pure PPFullProtection
      "noprotection" -> pure PPNoProtection
      e ->
        fromTextError $
          "Failure parsing ProtectionPolicy from value: '" <> e
            <> "'. Accepted values: fullprotection, noprotection"

instance ToText ProtectionPolicy where
  toText = \case
    PPFullProtection -> "FullProtection"
    PPNoProtection -> "NoProtection"

instance Hashable ProtectionPolicy

instance NFData ProtectionPolicy

instance ToByteString ProtectionPolicy

instance ToQuery ProtectionPolicy

instance ToHeader ProtectionPolicy

instance ToJSON ProtectionPolicy where
  toJSON = toJSONText

instance FromJSON ProtectionPolicy where
  parseJSON = parseJSONText "ProtectionPolicy"
