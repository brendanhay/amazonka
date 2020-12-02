{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.DataFormat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.DataFormat where

import Network.AWS.Prelude

data DataFormat = Avro
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

instance FromText DataFormat where
  parser =
    takeLowerText >>= \case
      "avro" -> pure Avro
      e ->
        fromTextError $
          "Failure parsing DataFormat from value: '" <> e
            <> "'. Accepted values: avro"

instance ToText DataFormat where
  toText = \case
    Avro -> "AVRO"

instance Hashable DataFormat

instance NFData DataFormat

instance ToByteString DataFormat

instance ToQuery DataFormat

instance ToHeader DataFormat

instance ToJSON DataFormat where
  toJSON = toJSONText

instance FromJSON DataFormat where
  parseJSON = parseJSONText "DataFormat"
