{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.MessageFormat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.MessageFormat where

import Network.AWS.Prelude

data MessageFormat
  = JSON
  | Raw
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

instance FromText MessageFormat where
  parser =
    takeLowerText >>= \case
      "json" -> pure JSON
      "raw" -> pure Raw
      e ->
        fromTextError $
          "Failure parsing MessageFormat from value: '" <> e
            <> "'. Accepted values: json, raw"

instance ToText MessageFormat where
  toText = \case
    JSON -> "JSON"
    Raw -> "RAW"

instance Hashable MessageFormat

instance NFData MessageFormat

instance ToByteString MessageFormat

instance ToQuery MessageFormat

instance ToHeader MessageFormat

instance ToJSON MessageFormat where
  toJSON = toJSONText

instance FromJSON MessageFormat where
  parseJSON = parseJSONText "MessageFormat"
