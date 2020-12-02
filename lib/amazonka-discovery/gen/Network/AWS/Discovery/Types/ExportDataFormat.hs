{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.ExportDataFormat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Discovery.Types.ExportDataFormat where

import Network.AWS.Prelude

data ExportDataFormat
  = CSV
  | Graphml
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

instance FromText ExportDataFormat where
  parser =
    takeLowerText >>= \case
      "csv" -> pure CSV
      "graphml" -> pure Graphml
      e ->
        fromTextError $
          "Failure parsing ExportDataFormat from value: '" <> e
            <> "'. Accepted values: csv, graphml"

instance ToText ExportDataFormat where
  toText = \case
    CSV -> "CSV"
    Graphml -> "GRAPHML"

instance Hashable ExportDataFormat

instance NFData ExportDataFormat

instance ToByteString ExportDataFormat

instance ToQuery ExportDataFormat

instance ToHeader ExportDataFormat

instance ToJSON ExportDataFormat where
  toJSON = toJSONText
