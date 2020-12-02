{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.ProtectionGroupAggregation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.ProtectionGroupAggregation where

import Network.AWS.Prelude

data ProtectionGroupAggregation
  = Max
  | Mean
  | Sum
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

instance FromText ProtectionGroupAggregation where
  parser =
    takeLowerText >>= \case
      "max" -> pure Max
      "mean" -> pure Mean
      "sum" -> pure Sum
      e ->
        fromTextError $
          "Failure parsing ProtectionGroupAggregation from value: '" <> e
            <> "'. Accepted values: max, mean, sum"

instance ToText ProtectionGroupAggregation where
  toText = \case
    Max -> "MAX"
    Mean -> "MEAN"
    Sum -> "SUM"

instance Hashable ProtectionGroupAggregation

instance NFData ProtectionGroupAggregation

instance ToByteString ProtectionGroupAggregation

instance ToQuery ProtectionGroupAggregation

instance ToHeader ProtectionGroupAggregation

instance ToJSON ProtectionGroupAggregation where
  toJSON = toJSONText

instance FromJSON ProtectionGroupAggregation where
  parseJSON = parseJSONText "ProtectionGroupAggregation"
