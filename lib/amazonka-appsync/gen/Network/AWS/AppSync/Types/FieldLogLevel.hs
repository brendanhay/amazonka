{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.FieldLogLevel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.FieldLogLevel where

import Network.AWS.Prelude

data FieldLogLevel
  = FLLAll
  | FLLError'
  | FLLNone
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

instance FromText FieldLogLevel where
  parser =
    takeLowerText >>= \case
      "all" -> pure FLLAll
      "error" -> pure FLLError'
      "none" -> pure FLLNone
      e ->
        fromTextError $
          "Failure parsing FieldLogLevel from value: '" <> e
            <> "'. Accepted values: all, error, none"

instance ToText FieldLogLevel where
  toText = \case
    FLLAll -> "ALL"
    FLLError' -> "ERROR"
    FLLNone -> "NONE"

instance Hashable FieldLogLevel

instance NFData FieldLogLevel

instance ToByteString FieldLogLevel

instance ToQuery FieldLogLevel

instance ToHeader FieldLogLevel

instance ToJSON FieldLogLevel where
  toJSON = toJSONText

instance FromJSON FieldLogLevel where
  parseJSON = parseJSONText "FieldLogLevel"
