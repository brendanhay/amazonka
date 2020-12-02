{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexRuntime.Types.MessageFormatType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexRuntime.Types.MessageFormatType where

import Network.AWS.Prelude

data MessageFormatType
  = Composite
  | CustomPayload
  | PlainText
  | Ssml
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

instance FromText MessageFormatType where
  parser =
    takeLowerText >>= \case
      "composite" -> pure Composite
      "custompayload" -> pure CustomPayload
      "plaintext" -> pure PlainText
      "ssml" -> pure Ssml
      e ->
        fromTextError $
          "Failure parsing MessageFormatType from value: '" <> e
            <> "'. Accepted values: composite, custompayload, plaintext, ssml"

instance ToText MessageFormatType where
  toText = \case
    Composite -> "Composite"
    CustomPayload -> "CustomPayload"
    PlainText -> "PlainText"
    Ssml -> "SSML"

instance Hashable MessageFormatType

instance NFData MessageFormatType

instance ToByteString MessageFormatType

instance ToQuery MessageFormatType

instance ToHeader MessageFormatType

instance ToJSON MessageFormatType where
  toJSON = toJSONText

instance FromJSON MessageFormatType where
  parseJSON = parseJSONText "MessageFormatType"
