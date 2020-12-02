{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Polly.Types.TextType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Polly.Types.TextType where

import Network.AWS.Prelude

data TextType
  = TTSsml
  | TTText
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

instance FromText TextType where
  parser =
    takeLowerText >>= \case
      "ssml" -> pure TTSsml
      "text" -> pure TTText
      e ->
        fromTextError $
          "Failure parsing TextType from value: '" <> e
            <> "'. Accepted values: ssml, text"

instance ToText TextType where
  toText = \case
    TTSsml -> "ssml"
    TTText -> "text"

instance Hashable TextType

instance NFData TextType

instance ToByteString TextType

instance ToQuery TextType

instance ToHeader TextType

instance ToJSON TextType where
  toJSON = toJSONText

instance FromJSON TextType where
  parseJSON = parseJSONText "TextType"
