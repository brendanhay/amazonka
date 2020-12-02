{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.CompressionTypeValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.CompressionTypeValue where

import Network.AWS.Prelude

data CompressionTypeValue
  = CTVGzip
  | CTVNone
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

instance FromText CompressionTypeValue where
  parser =
    takeLowerText >>= \case
      "gzip" -> pure CTVGzip
      "none" -> pure CTVNone
      e ->
        fromTextError $
          "Failure parsing CompressionTypeValue from value: '" <> e
            <> "'. Accepted values: gzip, none"

instance ToText CompressionTypeValue where
  toText = \case
    CTVGzip -> "gzip"
    CTVNone -> "none"

instance Hashable CompressionTypeValue

instance NFData CompressionTypeValue

instance ToByteString CompressionTypeValue

instance ToQuery CompressionTypeValue

instance ToHeader CompressionTypeValue

instance ToJSON CompressionTypeValue where
  toJSON = toJSONText

instance FromJSON CompressionTypeValue where
  parseJSON = parseJSONText "CompressionTypeValue"
