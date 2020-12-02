{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.M3u8NielsenId3
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.M3u8NielsenId3 where

import Network.AWS.Prelude

-- | If INSERT, Nielsen inaudible tones for media tracking will be detected in the input audio and an equivalent ID3 tag will be inserted in the output.
data M3u8NielsenId3
  = M3uInsert
  | M3uNone
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

instance FromText M3u8NielsenId3 where
  parser =
    takeLowerText >>= \case
      "insert" -> pure M3uInsert
      "none" -> pure M3uNone
      e ->
        fromTextError $
          "Failure parsing M3u8NielsenId3 from value: '" <> e
            <> "'. Accepted values: insert, none"

instance ToText M3u8NielsenId3 where
  toText = \case
    M3uInsert -> "INSERT"
    M3uNone -> "NONE"

instance Hashable M3u8NielsenId3

instance NFData M3u8NielsenId3

instance ToByteString M3u8NielsenId3

instance ToQuery M3u8NielsenId3

instance ToHeader M3u8NielsenId3

instance ToJSON M3u8NielsenId3 where
  toJSON = toJSONText

instance FromJSON M3u8NielsenId3 where
  parseJSON = parseJSONText "M3u8NielsenId3"
