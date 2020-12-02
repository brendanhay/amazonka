{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Eac3SurroundMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Eac3SurroundMode where

import Network.AWS.Prelude

-- | Eac3 Surround Mode
data Eac3SurroundMode
  = ESMDisabled
  | ESMEnabled
  | ESMNotIndicated
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

instance FromText Eac3SurroundMode where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure ESMDisabled
      "enabled" -> pure ESMEnabled
      "not_indicated" -> pure ESMNotIndicated
      e ->
        fromTextError $
          "Failure parsing Eac3SurroundMode from value: '" <> e
            <> "'. Accepted values: disabled, enabled, not_indicated"

instance ToText Eac3SurroundMode where
  toText = \case
    ESMDisabled -> "DISABLED"
    ESMEnabled -> "ENABLED"
    ESMNotIndicated -> "NOT_INDICATED"

instance Hashable Eac3SurroundMode

instance NFData Eac3SurroundMode

instance ToByteString Eac3SurroundMode

instance ToQuery Eac3SurroundMode

instance ToHeader Eac3SurroundMode

instance ToJSON Eac3SurroundMode where
  toJSON = toJSONText

instance FromJSON Eac3SurroundMode where
  parseJSON = parseJSONText "Eac3SurroundMode"
