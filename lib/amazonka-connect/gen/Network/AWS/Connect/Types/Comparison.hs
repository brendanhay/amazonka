{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.Comparison
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.Comparison where

import Network.AWS.Prelude

data Comparison = LT'
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

instance FromText Comparison where
  parser =
    takeLowerText >>= \case
      "lt" -> pure LT'
      e ->
        fromTextError $
          "Failure parsing Comparison from value: '" <> e
            <> "'. Accepted values: lt"

instance ToText Comparison where
  toText = \case
    LT' -> "LT"

instance Hashable Comparison

instance NFData Comparison

instance ToByteString Comparison

instance ToQuery Comparison

instance ToHeader Comparison

instance ToJSON Comparison where
  toJSON = toJSONText

instance FromJSON Comparison where
  parseJSON = parseJSONText "Comparison"
