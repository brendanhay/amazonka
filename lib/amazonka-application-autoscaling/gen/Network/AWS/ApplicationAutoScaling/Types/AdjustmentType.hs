{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApplicationAutoScaling.Types.AdjustmentType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ApplicationAutoScaling.Types.AdjustmentType where

import Network.AWS.Prelude

data AdjustmentType
  = ChangeInCapacity
  | ExactCapacity
  | PercentChangeInCapacity
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

instance FromText AdjustmentType where
  parser =
    takeLowerText >>= \case
      "changeincapacity" -> pure ChangeInCapacity
      "exactcapacity" -> pure ExactCapacity
      "percentchangeincapacity" -> pure PercentChangeInCapacity
      e ->
        fromTextError $
          "Failure parsing AdjustmentType from value: '" <> e
            <> "'. Accepted values: changeincapacity, exactcapacity, percentchangeincapacity"

instance ToText AdjustmentType where
  toText = \case
    ChangeInCapacity -> "ChangeInCapacity"
    ExactCapacity -> "ExactCapacity"
    PercentChangeInCapacity -> "PercentChangeInCapacity"

instance Hashable AdjustmentType

instance NFData AdjustmentType

instance ToByteString AdjustmentType

instance ToQuery AdjustmentType

instance ToHeader AdjustmentType

instance ToJSON AdjustmentType where
  toJSON = toJSONText

instance FromJSON AdjustmentType where
  parseJSON = parseJSONText "AdjustmentType"
