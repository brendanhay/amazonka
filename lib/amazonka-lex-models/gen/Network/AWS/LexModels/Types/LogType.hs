{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.LogType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.LogType where

import Network.AWS.Prelude

data LogType
  = Audio
  | Text
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

instance FromText LogType where
  parser =
    takeLowerText >>= \case
      "audio" -> pure Audio
      "text" -> pure Text
      e ->
        fromTextError $
          "Failure parsing LogType from value: '" <> e
            <> "'. Accepted values: audio, text"

instance ToText LogType where
  toText = \case
    Audio -> "AUDIO"
    Text -> "TEXT"

instance Hashable LogType

instance NFData LogType

instance ToByteString LogType

instance ToQuery LogType

instance ToHeader LogType

instance ToJSON LogType where
  toJSON = toJSONText

instance FromJSON LogType where
  parseJSON = parseJSONText "LogType"
