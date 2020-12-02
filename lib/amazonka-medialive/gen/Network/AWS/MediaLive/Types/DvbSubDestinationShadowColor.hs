{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.DvbSubDestinationShadowColor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.DvbSubDestinationShadowColor where

import Network.AWS.Prelude

-- | Dvb Sub Destination Shadow Color
data DvbSubDestinationShadowColor
  = DSDSCBlack
  | DSDSCNone
  | DSDSCWhite
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

instance FromText DvbSubDestinationShadowColor where
  parser =
    takeLowerText >>= \case
      "black" -> pure DSDSCBlack
      "none" -> pure DSDSCNone
      "white" -> pure DSDSCWhite
      e ->
        fromTextError $
          "Failure parsing DvbSubDestinationShadowColor from value: '" <> e
            <> "'. Accepted values: black, none, white"

instance ToText DvbSubDestinationShadowColor where
  toText = \case
    DSDSCBlack -> "BLACK"
    DSDSCNone -> "NONE"
    DSDSCWhite -> "WHITE"

instance Hashable DvbSubDestinationShadowColor

instance NFData DvbSubDestinationShadowColor

instance ToByteString DvbSubDestinationShadowColor

instance ToQuery DvbSubDestinationShadowColor

instance ToHeader DvbSubDestinationShadowColor

instance ToJSON DvbSubDestinationShadowColor where
  toJSON = toJSONText

instance FromJSON DvbSubDestinationShadowColor where
  parseJSON = parseJSONText "DvbSubDestinationShadowColor"
