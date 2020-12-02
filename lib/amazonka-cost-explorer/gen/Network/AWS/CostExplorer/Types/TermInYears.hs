{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.TermInYears
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.TermInYears where

import Network.AWS.Prelude

data TermInYears
  = OneYear
  | ThreeYears
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

instance FromText TermInYears where
  parser =
    takeLowerText >>= \case
      "one_year" -> pure OneYear
      "three_years" -> pure ThreeYears
      e ->
        fromTextError $
          "Failure parsing TermInYears from value: '" <> e
            <> "'. Accepted values: one_year, three_years"

instance ToText TermInYears where
  toText = \case
    OneYear -> "ONE_YEAR"
    ThreeYears -> "THREE_YEARS"

instance Hashable TermInYears

instance NFData TermInYears

instance ToByteString TermInYears

instance ToQuery TermInYears

instance ToHeader TermInYears

instance ToJSON TermInYears where
  toJSON = toJSONText

instance FromJSON TermInYears where
  parseJSON = parseJSONText "TermInYears"
