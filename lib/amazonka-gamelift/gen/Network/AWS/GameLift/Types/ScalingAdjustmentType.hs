{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.ScalingAdjustmentType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.ScalingAdjustmentType where

import Network.AWS.Prelude

data ScalingAdjustmentType
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

instance FromText ScalingAdjustmentType where
  parser =
    takeLowerText >>= \case
      "changeincapacity" -> pure ChangeInCapacity
      "exactcapacity" -> pure ExactCapacity
      "percentchangeincapacity" -> pure PercentChangeInCapacity
      e ->
        fromTextError $
          "Failure parsing ScalingAdjustmentType from value: '" <> e
            <> "'. Accepted values: changeincapacity, exactcapacity, percentchangeincapacity"

instance ToText ScalingAdjustmentType where
  toText = \case
    ChangeInCapacity -> "ChangeInCapacity"
    ExactCapacity -> "ExactCapacity"
    PercentChangeInCapacity -> "PercentChangeInCapacity"

instance Hashable ScalingAdjustmentType

instance NFData ScalingAdjustmentType

instance ToByteString ScalingAdjustmentType

instance ToQuery ScalingAdjustmentType

instance ToHeader ScalingAdjustmentType

instance ToJSON ScalingAdjustmentType where
  toJSON = toJSONText

instance FromJSON ScalingAdjustmentType where
  parseJSON = parseJSONText "ScalingAdjustmentType"
