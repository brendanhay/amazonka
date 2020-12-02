{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.ContextKeyTypeEnum
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.ContextKeyTypeEnum where

import Network.AWS.Prelude

data ContextKeyTypeEnum
  = Binary
  | BinaryList
  | Boolean
  | BooleanList
  | Date
  | DateList
  | IP
  | IPList
  | Numeric
  | NumericList
  | String
  | StringList
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

instance FromText ContextKeyTypeEnum where
  parser =
    takeLowerText >>= \case
      "binary" -> pure Binary
      "binarylist" -> pure BinaryList
      "boolean" -> pure Boolean
      "booleanlist" -> pure BooleanList
      "date" -> pure Date
      "datelist" -> pure DateList
      "ip" -> pure IP
      "iplist" -> pure IPList
      "numeric" -> pure Numeric
      "numericlist" -> pure NumericList
      "string" -> pure String
      "stringlist" -> pure StringList
      e ->
        fromTextError $
          "Failure parsing ContextKeyTypeEnum from value: '" <> e
            <> "'. Accepted values: binary, binarylist, boolean, booleanlist, date, datelist, ip, iplist, numeric, numericlist, string, stringlist"

instance ToText ContextKeyTypeEnum where
  toText = \case
    Binary -> "binary"
    BinaryList -> "binaryList"
    Boolean -> "boolean"
    BooleanList -> "booleanList"
    Date -> "date"
    DateList -> "dateList"
    IP -> "ip"
    IPList -> "ipList"
    Numeric -> "numeric"
    NumericList -> "numericList"
    String -> "string"
    StringList -> "stringList"

instance Hashable ContextKeyTypeEnum

instance NFData ContextKeyTypeEnum

instance ToByteString ContextKeyTypeEnum

instance ToQuery ContextKeyTypeEnum

instance ToHeader ContextKeyTypeEnum
