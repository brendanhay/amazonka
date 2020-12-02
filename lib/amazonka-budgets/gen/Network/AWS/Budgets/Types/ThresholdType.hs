{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.ThresholdType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.ThresholdType where

import Network.AWS.Prelude

-- | The type of threshold for a notification.
data ThresholdType
  = AbsoluteValue
  | Percentage
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

instance FromText ThresholdType where
  parser =
    takeLowerText >>= \case
      "absolute_value" -> pure AbsoluteValue
      "percentage" -> pure Percentage
      e ->
        fromTextError $
          "Failure parsing ThresholdType from value: '" <> e
            <> "'. Accepted values: absolute_value, percentage"

instance ToText ThresholdType where
  toText = \case
    AbsoluteValue -> "ABSOLUTE_VALUE"
    Percentage -> "PERCENTAGE"

instance Hashable ThresholdType

instance NFData ThresholdType

instance ToByteString ThresholdType

instance ToQuery ThresholdType

instance ToHeader ThresholdType

instance ToJSON ThresholdType where
  toJSON = toJSONText

instance FromJSON ThresholdType where
  parseJSON = parseJSONText "ThresholdType"
