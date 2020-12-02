{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AvailBlankingState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AvailBlankingState where

import Network.AWS.Prelude

-- | Avail Blanking State
data AvailBlankingState
  = ABSDisabled
  | ABSEnabled
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

instance FromText AvailBlankingState where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure ABSDisabled
      "enabled" -> pure ABSEnabled
      e ->
        fromTextError $
          "Failure parsing AvailBlankingState from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText AvailBlankingState where
  toText = \case
    ABSDisabled -> "DISABLED"
    ABSEnabled -> "ENABLED"

instance Hashable AvailBlankingState

instance NFData AvailBlankingState

instance ToByteString AvailBlankingState

instance ToQuery AvailBlankingState

instance ToHeader AvailBlankingState

instance ToJSON AvailBlankingState where
  toJSON = toJSONText

instance FromJSON AvailBlankingState where
  parseJSON = parseJSONText "AvailBlankingState"
