{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.MatchOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.MatchOption where

import Network.AWS.Prelude

data MatchOption
  = CaseInsensitive
  | CaseSensitive
  | Contains
  | EndsWith
  | Equals
  | StartsWith
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

instance FromText MatchOption where
  parser =
    takeLowerText >>= \case
      "case_insensitive" -> pure CaseInsensitive
      "case_sensitive" -> pure CaseSensitive
      "contains" -> pure Contains
      "ends_with" -> pure EndsWith
      "equals" -> pure Equals
      "starts_with" -> pure StartsWith
      e ->
        fromTextError $
          "Failure parsing MatchOption from value: '" <> e
            <> "'. Accepted values: case_insensitive, case_sensitive, contains, ends_with, equals, starts_with"

instance ToText MatchOption where
  toText = \case
    CaseInsensitive -> "CASE_INSENSITIVE"
    CaseSensitive -> "CASE_SENSITIVE"
    Contains -> "CONTAINS"
    EndsWith -> "ENDS_WITH"
    Equals -> "EQUALS"
    StartsWith -> "STARTS_WITH"

instance Hashable MatchOption

instance NFData MatchOption

instance ToByteString MatchOption

instance ToQuery MatchOption

instance ToHeader MatchOption

instance ToJSON MatchOption where
  toJSON = toJSONText

instance FromJSON MatchOption where
  parseJSON = parseJSONText "MatchOption"
