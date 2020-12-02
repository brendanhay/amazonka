{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.VideoDescriptionRespondToAfd
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.VideoDescriptionRespondToAfd where

import Network.AWS.Prelude

-- | Video Description Respond To Afd
data VideoDescriptionRespondToAfd
  = None
  | Passthrough
  | Respond
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

instance FromText VideoDescriptionRespondToAfd where
  parser =
    takeLowerText >>= \case
      "none" -> pure None
      "passthrough" -> pure Passthrough
      "respond" -> pure Respond
      e ->
        fromTextError $
          "Failure parsing VideoDescriptionRespondToAfd from value: '" <> e
            <> "'. Accepted values: none, passthrough, respond"

instance ToText VideoDescriptionRespondToAfd where
  toText = \case
    None -> "NONE"
    Passthrough -> "PASSTHROUGH"
    Respond -> "RESPOND"

instance Hashable VideoDescriptionRespondToAfd

instance NFData VideoDescriptionRespondToAfd

instance ToByteString VideoDescriptionRespondToAfd

instance ToQuery VideoDescriptionRespondToAfd

instance ToHeader VideoDescriptionRespondToAfd

instance ToJSON VideoDescriptionRespondToAfd where
  toJSON = toJSONText

instance FromJSON VideoDescriptionRespondToAfd where
  parseJSON = parseJSONText "VideoDescriptionRespondToAfd"
