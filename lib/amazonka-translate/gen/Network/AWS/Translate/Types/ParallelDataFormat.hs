{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.Types.ParallelDataFormat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Translate.Types.ParallelDataFormat where

import Network.AWS.Prelude

data ParallelDataFormat
  = CSV
  | Tmx
  | Tsv
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

instance FromText ParallelDataFormat where
  parser =
    takeLowerText >>= \case
      "csv" -> pure CSV
      "tmx" -> pure Tmx
      "tsv" -> pure Tsv
      e ->
        fromTextError $
          "Failure parsing ParallelDataFormat from value: '" <> e
            <> "'. Accepted values: csv, tmx, tsv"

instance ToText ParallelDataFormat where
  toText = \case
    CSV -> "CSV"
    Tmx -> "TMX"
    Tsv -> "TSV"

instance Hashable ParallelDataFormat

instance NFData ParallelDataFormat

instance ToByteString ParallelDataFormat

instance ToQuery ParallelDataFormat

instance ToHeader ParallelDataFormat

instance ToJSON ParallelDataFormat where
  toJSON = toJSONText

instance FromJSON ParallelDataFormat where
  parseJSON = parseJSONText "ParallelDataFormat"
