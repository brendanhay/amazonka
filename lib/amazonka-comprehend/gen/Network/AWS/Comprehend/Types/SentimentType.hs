{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.SentimentType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.SentimentType where

import Network.AWS.Prelude

data SentimentType
  = Mixed
  | Negative
  | Neutral
  | Positive
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

instance FromText SentimentType where
  parser =
    takeLowerText >>= \case
      "mixed" -> pure Mixed
      "negative" -> pure Negative
      "neutral" -> pure Neutral
      "positive" -> pure Positive
      e ->
        fromTextError $
          "Failure parsing SentimentType from value: '" <> e
            <> "'. Accepted values: mixed, negative, neutral, positive"

instance ToText SentimentType where
  toText = \case
    Mixed -> "MIXED"
    Negative -> "NEGATIVE"
    Neutral -> "NEUTRAL"
    Positive -> "POSITIVE"

instance Hashable SentimentType

instance NFData SentimentType

instance ToByteString SentimentType

instance ToQuery SentimentType

instance ToHeader SentimentType

instance FromJSON SentimentType where
  parseJSON = parseJSONText "SentimentType"
