{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.OrcCompression
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.OrcCompression where

import Network.AWS.Prelude

data OrcCompression
  = OCNone
  | OCSnappy
  | OCZlib
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

instance FromText OrcCompression where
  parser =
    takeLowerText >>= \case
      "none" -> pure OCNone
      "snappy" -> pure OCSnappy
      "zlib" -> pure OCZlib
      e ->
        fromTextError $
          "Failure parsing OrcCompression from value: '" <> e
            <> "'. Accepted values: none, snappy, zlib"

instance ToText OrcCompression where
  toText = \case
    OCNone -> "NONE"
    OCSnappy -> "SNAPPY"
    OCZlib -> "ZLIB"

instance Hashable OrcCompression

instance NFData OrcCompression

instance ToByteString OrcCompression

instance ToQuery OrcCompression

instance ToHeader OrcCompression

instance ToJSON OrcCompression where
  toJSON = toJSONText

instance FromJSON OrcCompression where
  parseJSON = parseJSONText "OrcCompression"
