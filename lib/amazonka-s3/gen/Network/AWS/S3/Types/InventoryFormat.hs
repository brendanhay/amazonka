{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.InventoryFormat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.InventoryFormat where

import Network.AWS.Prelude
import Network.AWS.S3.Internal

data InventoryFormat
  = IFCSV
  | IFOrc
  | IFParquet
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

instance FromText InventoryFormat where
  parser =
    takeLowerText >>= \case
      "csv" -> pure IFCSV
      "orc" -> pure IFOrc
      "parquet" -> pure IFParquet
      e ->
        fromTextError $
          "Failure parsing InventoryFormat from value: '" <> e
            <> "'. Accepted values: csv, orc, parquet"

instance ToText InventoryFormat where
  toText = \case
    IFCSV -> "CSV"
    IFOrc -> "ORC"
    IFParquet -> "Parquet"

instance Hashable InventoryFormat

instance NFData InventoryFormat

instance ToByteString InventoryFormat

instance ToQuery InventoryFormat

instance ToHeader InventoryFormat

instance FromXML InventoryFormat where
  parseXML = parseXMLText "InventoryFormat"

instance ToXML InventoryFormat where
  toXML = toXMLText
