{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.M3u8NielsenId3Behavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.M3u8NielsenId3Behavior where

import Network.AWS.Prelude

-- | M3u8 Nielsen Id3 Behavior
data M3u8NielsenId3Behavior
  = MNoPassthrough
  | MPassthrough
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

instance FromText M3u8NielsenId3Behavior where
  parser =
    takeLowerText >>= \case
      "no_passthrough" -> pure MNoPassthrough
      "passthrough" -> pure MPassthrough
      e ->
        fromTextError $
          "Failure parsing M3u8NielsenId3Behavior from value: '" <> e
            <> "'. Accepted values: no_passthrough, passthrough"

instance ToText M3u8NielsenId3Behavior where
  toText = \case
    MNoPassthrough -> "NO_PASSTHROUGH"
    MPassthrough -> "PASSTHROUGH"

instance Hashable M3u8NielsenId3Behavior

instance NFData M3u8NielsenId3Behavior

instance ToByteString M3u8NielsenId3Behavior

instance ToQuery M3u8NielsenId3Behavior

instance ToHeader M3u8NielsenId3Behavior

instance ToJSON M3u8NielsenId3Behavior where
  toJSON = toJSONText

instance FromJSON M3u8NielsenId3Behavior where
  parseJSON = parseJSONText "M3u8NielsenId3Behavior"
