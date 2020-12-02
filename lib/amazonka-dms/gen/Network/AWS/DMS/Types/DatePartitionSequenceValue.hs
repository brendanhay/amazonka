{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.DatePartitionSequenceValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.DatePartitionSequenceValue where

import Network.AWS.Prelude

data DatePartitionSequenceValue
  = Ddmmyyyy
  | Mmyyyydd
  | Yyyymm
  | Yyyymmdd
  | Yyyymmddhh
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

instance FromText DatePartitionSequenceValue where
  parser =
    takeLowerText >>= \case
      "ddmmyyyy" -> pure Ddmmyyyy
      "mmyyyydd" -> pure Mmyyyydd
      "yyyymm" -> pure Yyyymm
      "yyyymmdd" -> pure Yyyymmdd
      "yyyymmddhh" -> pure Yyyymmddhh
      e ->
        fromTextError $
          "Failure parsing DatePartitionSequenceValue from value: '" <> e
            <> "'. Accepted values: ddmmyyyy, mmyyyydd, yyyymm, yyyymmdd, yyyymmddhh"

instance ToText DatePartitionSequenceValue where
  toText = \case
    Ddmmyyyy -> "DDMMYYYY"
    Mmyyyydd -> "MMYYYYDD"
    Yyyymm -> "YYYYMM"
    Yyyymmdd -> "YYYYMMDD"
    Yyyymmddhh -> "YYYYMMDDHH"

instance Hashable DatePartitionSequenceValue

instance NFData DatePartitionSequenceValue

instance ToByteString DatePartitionSequenceValue

instance ToQuery DatePartitionSequenceValue

instance ToHeader DatePartitionSequenceValue

instance ToJSON DatePartitionSequenceValue where
  toJSON = toJSONText

instance FromJSON DatePartitionSequenceValue where
  parseJSON = parseJSONText "DatePartitionSequenceValue"
