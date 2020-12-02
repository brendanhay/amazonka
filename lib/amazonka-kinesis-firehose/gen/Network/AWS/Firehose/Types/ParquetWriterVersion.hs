{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.ParquetWriterVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.ParquetWriterVersion where

import Network.AWS.Prelude

data ParquetWriterVersion
  = V1
  | V2
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

instance FromText ParquetWriterVersion where
  parser =
    takeLowerText >>= \case
      "v1" -> pure V1
      "v2" -> pure V2
      e ->
        fromTextError $
          "Failure parsing ParquetWriterVersion from value: '" <> e
            <> "'. Accepted values: v1, v2"

instance ToText ParquetWriterVersion where
  toText = \case
    V1 -> "V1"
    V2 -> "V2"

instance Hashable ParquetWriterVersion

instance NFData ParquetWriterVersion

instance ToByteString ParquetWriterVersion

instance ToQuery ParquetWriterVersion

instance ToHeader ParquetWriterVersion

instance ToJSON ParquetWriterVersion where
  toJSON = toJSONText

instance FromJSON ParquetWriterVersion where
  parseJSON = parseJSONText "ParquetWriterVersion"
