{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.Granularity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.Granularity where

import Network.AWS.Prelude

data Granularity
  = Daily
  | Hourly
  | Monthly
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

instance FromText Granularity where
  parser =
    takeLowerText >>= \case
      "daily" -> pure Daily
      "hourly" -> pure Hourly
      "monthly" -> pure Monthly
      e ->
        fromTextError $
          "Failure parsing Granularity from value: '" <> e
            <> "'. Accepted values: daily, hourly, monthly"

instance ToText Granularity where
  toText = \case
    Daily -> "DAILY"
    Hourly -> "HOURLY"
    Monthly -> "MONTHLY"

instance Hashable Granularity

instance NFData Granularity

instance ToByteString Granularity

instance ToQuery Granularity

instance ToHeader Granularity

instance ToJSON Granularity where
  toJSON = toJSONText
