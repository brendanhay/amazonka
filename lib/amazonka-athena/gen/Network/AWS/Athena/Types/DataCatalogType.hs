{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.DataCatalogType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.DataCatalogType where

import Network.AWS.Prelude

data DataCatalogType
  = Glue
  | Hive
  | Lambda
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

instance FromText DataCatalogType where
  parser =
    takeLowerText >>= \case
      "glue" -> pure Glue
      "hive" -> pure Hive
      "lambda" -> pure Lambda
      e ->
        fromTextError $
          "Failure parsing DataCatalogType from value: '" <> e
            <> "'. Accepted values: glue, hive, lambda"

instance ToText DataCatalogType where
  toText = \case
    Glue -> "GLUE"
    Hive -> "HIVE"
    Lambda -> "LAMBDA"

instance Hashable DataCatalogType

instance NFData DataCatalogType

instance ToByteString DataCatalogType

instance ToQuery DataCatalogType

instance ToHeader DataCatalogType

instance ToJSON DataCatalogType where
  toJSON = toJSONText

instance FromJSON DataCatalogType where
  parseJSON = parseJSONText "DataCatalogType"
