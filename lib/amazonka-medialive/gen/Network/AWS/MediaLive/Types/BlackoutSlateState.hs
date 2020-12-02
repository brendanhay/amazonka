{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.BlackoutSlateState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.BlackoutSlateState where

import Network.AWS.Prelude

-- | Blackout Slate State
data BlackoutSlateState
  = BSSDisabled
  | BSSEnabled
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

instance FromText BlackoutSlateState where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure BSSDisabled
      "enabled" -> pure BSSEnabled
      e ->
        fromTextError $
          "Failure parsing BlackoutSlateState from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText BlackoutSlateState where
  toText = \case
    BSSDisabled -> "DISABLED"
    BSSEnabled -> "ENABLED"

instance Hashable BlackoutSlateState

instance NFData BlackoutSlateState

instance ToByteString BlackoutSlateState

instance ToQuery BlackoutSlateState

instance ToHeader BlackoutSlateState

instance ToJSON BlackoutSlateState where
  toJSON = toJSONText

instance FromJSON BlackoutSlateState where
  parseJSON = parseJSONText "BlackoutSlateState"
