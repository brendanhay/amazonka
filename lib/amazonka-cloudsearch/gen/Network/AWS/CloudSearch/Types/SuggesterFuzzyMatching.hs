{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.SuggesterFuzzyMatching
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.SuggesterFuzzyMatching where

import Network.AWS.Prelude

data SuggesterFuzzyMatching
  = High
  | Low
  | None
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

instance FromText SuggesterFuzzyMatching where
  parser =
    takeLowerText >>= \case
      "high" -> pure High
      "low" -> pure Low
      "none" -> pure None
      e ->
        fromTextError $
          "Failure parsing SuggesterFuzzyMatching from value: '" <> e
            <> "'. Accepted values: high, low, none"

instance ToText SuggesterFuzzyMatching where
  toText = \case
    High -> "high"
    Low -> "low"
    None -> "none"

instance Hashable SuggesterFuzzyMatching

instance NFData SuggesterFuzzyMatching

instance ToByteString SuggesterFuzzyMatching

instance ToQuery SuggesterFuzzyMatching

instance ToHeader SuggesterFuzzyMatching

instance FromXML SuggesterFuzzyMatching where
  parseXML = parseXMLText "SuggesterFuzzyMatching"
