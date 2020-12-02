{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.ContentHandlingStrategy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.ContentHandlingStrategy where

import Network.AWS.Prelude

data ContentHandlingStrategy
  = ConvertToBinary
  | ConvertToText
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

instance FromText ContentHandlingStrategy where
  parser =
    takeLowerText >>= \case
      "convert_to_binary" -> pure ConvertToBinary
      "convert_to_text" -> pure ConvertToText
      e ->
        fromTextError $
          "Failure parsing ContentHandlingStrategy from value: '" <> e
            <> "'. Accepted values: convert_to_binary, convert_to_text"

instance ToText ContentHandlingStrategy where
  toText = \case
    ConvertToBinary -> "CONVERT_TO_BINARY"
    ConvertToText -> "CONVERT_TO_TEXT"

instance Hashable ContentHandlingStrategy

instance NFData ContentHandlingStrategy

instance ToByteString ContentHandlingStrategy

instance ToQuery ContentHandlingStrategy

instance ToHeader ContentHandlingStrategy

instance ToJSON ContentHandlingStrategy where
  toJSON = toJSONText

instance FromJSON ContentHandlingStrategy where
  parseJSON = parseJSONText "ContentHandlingStrategy"
