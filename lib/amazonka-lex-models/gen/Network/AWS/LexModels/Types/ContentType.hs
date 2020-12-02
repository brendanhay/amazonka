{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.ContentType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.ContentType where

import Network.AWS.Prelude

data ContentType
  = CustomPayload
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

instance FromText ContentType where
  parser =
    takeLowerText >>= \case
      "custompayload" -> pure CustomPayload
      "plaintext" -> pure PlainText
      "ssml" -> pure Ssml
      e ->
        fromTextError $
          "Failure parsing ContentType from value: '" <> e
            <> "'. Accepted values: custompayload, plaintext, ssml"

instance ToText ContentType where
  toText = \case
    CustomPayload -> "CustomPayload"
    PlainText -> "PlainText"
    Ssml -> "SSML"

instance Hashable ContentType

instance NFData ContentType

instance ToByteString ContentType

instance ToQuery ContentType

instance ToHeader ContentType

instance ToJSON ContentType where
  toJSON = toJSONText

instance FromJSON ContentType where
  parseJSON = parseJSONText "ContentType"
