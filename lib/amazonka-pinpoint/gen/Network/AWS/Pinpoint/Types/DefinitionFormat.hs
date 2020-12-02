{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.DefinitionFormat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.DefinitionFormat where

import Network.AWS.Prelude

data DefinitionFormat
  = CSV
  | JSON
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

instance FromText DefinitionFormat where
  parser =
    takeLowerText >>= \case
      "csv" -> pure CSV
      "json" -> pure JSON
      e ->
        fromTextError $
          "Failure parsing DefinitionFormat from value: '" <> e
            <> "'. Accepted values: csv, json"

instance ToText DefinitionFormat where
  toText = \case
    CSV -> "CSV"
    JSON -> "JSON"

instance Hashable DefinitionFormat

instance NFData DefinitionFormat

instance ToByteString DefinitionFormat

instance ToQuery DefinitionFormat

instance ToHeader DefinitionFormat

instance ToJSON DefinitionFormat where
  toJSON = toJSONText

instance FromJSON DefinitionFormat where
  parseJSON = parseJSONText "DefinitionFormat"
