{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.DvbSubDestinationBackgroundColor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.DvbSubDestinationBackgroundColor where

import Network.AWS.Prelude

-- | Dvb Sub Destination Background Color
data DvbSubDestinationBackgroundColor
  = DSDBCBlack
  | DSDBCNone
  | DSDBCWhite
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

instance FromText DvbSubDestinationBackgroundColor where
  parser =
    takeLowerText >>= \case
      "black" -> pure DSDBCBlack
      "none" -> pure DSDBCNone
      "white" -> pure DSDBCWhite
      e ->
        fromTextError $
          "Failure parsing DvbSubDestinationBackgroundColor from value: '" <> e
            <> "'. Accepted values: black, none, white"

instance ToText DvbSubDestinationBackgroundColor where
  toText = \case
    DSDBCBlack -> "BLACK"
    DSDBCNone -> "NONE"
    DSDBCWhite -> "WHITE"

instance Hashable DvbSubDestinationBackgroundColor

instance NFData DvbSubDestinationBackgroundColor

instance ToByteString DvbSubDestinationBackgroundColor

instance ToQuery DvbSubDestinationBackgroundColor

instance ToHeader DvbSubDestinationBackgroundColor

instance ToJSON DvbSubDestinationBackgroundColor where
  toJSON = toJSONText

instance FromJSON DvbSubDestinationBackgroundColor where
  parseJSON = parseJSONText "DvbSubDestinationBackgroundColor"
