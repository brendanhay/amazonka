{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.Types.TerminologyDataFormat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Translate.Types.TerminologyDataFormat where

import Network.AWS.Prelude

data TerminologyDataFormat
  = TDFCSV
  | TDFTmx
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

instance FromText TerminologyDataFormat where
  parser =
    takeLowerText >>= \case
      "csv" -> pure TDFCSV
      "tmx" -> pure TDFTmx
      e ->
        fromTextError $
          "Failure parsing TerminologyDataFormat from value: '" <> e
            <> "'. Accepted values: csv, tmx"

instance ToText TerminologyDataFormat where
  toText = \case
    TDFCSV -> "CSV"
    TDFTmx -> "TMX"

instance Hashable TerminologyDataFormat

instance NFData TerminologyDataFormat

instance ToByteString TerminologyDataFormat

instance ToQuery TerminologyDataFormat

instance ToHeader TerminologyDataFormat

instance ToJSON TerminologyDataFormat where
  toJSON = toJSONText
