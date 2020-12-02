{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.PlaylistType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.PlaylistType where

import Network.AWS.Prelude

data PlaylistType
  = PTEvent
  | PTNone
  | PTVod
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

instance FromText PlaylistType where
  parser =
    takeLowerText >>= \case
      "event" -> pure PTEvent
      "none" -> pure PTNone
      "vod" -> pure PTVod
      e ->
        fromTextError $
          "Failure parsing PlaylistType from value: '" <> e
            <> "'. Accepted values: event, none, vod"

instance ToText PlaylistType where
  toText = \case
    PTEvent -> "EVENT"
    PTNone -> "NONE"
    PTVod -> "VOD"

instance Hashable PlaylistType

instance NFData PlaylistType

instance ToByteString PlaylistType

instance ToQuery PlaylistType

instance ToHeader PlaylistType

instance ToJSON PlaylistType where
  toJSON = toJSONText

instance FromJSON PlaylistType where
  parseJSON = parseJSONText "PlaylistType"
