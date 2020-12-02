{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.TextTypes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.TextTypes where

import Network.AWS.Prelude

data TextTypes
  = Line
  | Word
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

instance FromText TextTypes where
  parser =
    takeLowerText >>= \case
      "line" -> pure Line
      "word" -> pure Word
      e ->
        fromTextError $
          "Failure parsing TextTypes from value: '" <> e
            <> "'. Accepted values: line, word"

instance ToText TextTypes where
  toText = \case
    Line -> "LINE"
    Word -> "WORD"

instance Hashable TextTypes

instance NFData TextTypes

instance ToByteString TextTypes

instance ToQuery TextTypes

instance ToHeader TextTypes

instance FromJSON TextTypes where
  parseJSON = parseJSONText "TextTypes"
