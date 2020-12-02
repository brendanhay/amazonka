{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.DocumentFormat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.DocumentFormat where

import Network.AWS.Prelude

data DocumentFormat
  = JSON
  | Text
  | Yaml
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

instance FromText DocumentFormat where
  parser =
    takeLowerText >>= \case
      "json" -> pure JSON
      "text" -> pure Text
      "yaml" -> pure Yaml
      e ->
        fromTextError $
          "Failure parsing DocumentFormat from value: '" <> e
            <> "'. Accepted values: json, text, yaml"

instance ToText DocumentFormat where
  toText = \case
    JSON -> "JSON"
    Text -> "TEXT"
    Yaml -> "YAML"

instance Hashable DocumentFormat

instance NFData DocumentFormat

instance ToByteString DocumentFormat

instance ToQuery DocumentFormat

instance ToHeader DocumentFormat

instance ToJSON DocumentFormat where
  toJSON = toJSONText

instance FromJSON DocumentFormat where
  parseJSON = parseJSONText "DocumentFormat"
