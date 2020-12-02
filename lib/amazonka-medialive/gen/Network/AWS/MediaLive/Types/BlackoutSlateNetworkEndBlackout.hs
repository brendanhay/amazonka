{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.BlackoutSlateNetworkEndBlackout
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.BlackoutSlateNetworkEndBlackout where

import Network.AWS.Prelude

-- | Blackout Slate Network End Blackout
data BlackoutSlateNetworkEndBlackout
  = BSNEBDisabled
  | BSNEBEnabled
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

instance FromText BlackoutSlateNetworkEndBlackout where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure BSNEBDisabled
      "enabled" -> pure BSNEBEnabled
      e ->
        fromTextError $
          "Failure parsing BlackoutSlateNetworkEndBlackout from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText BlackoutSlateNetworkEndBlackout where
  toText = \case
    BSNEBDisabled -> "DISABLED"
    BSNEBEnabled -> "ENABLED"

instance Hashable BlackoutSlateNetworkEndBlackout

instance NFData BlackoutSlateNetworkEndBlackout

instance ToByteString BlackoutSlateNetworkEndBlackout

instance ToQuery BlackoutSlateNetworkEndBlackout

instance ToHeader BlackoutSlateNetworkEndBlackout

instance ToJSON BlackoutSlateNetworkEndBlackout where
  toJSON = toJSONText

instance FromJSON BlackoutSlateNetworkEndBlackout where
  parseJSON = parseJSONText "BlackoutSlateNetworkEndBlackout"
