{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.InputStartingPosition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.InputStartingPosition where

import Network.AWS.Prelude

data InputStartingPosition
  = LastStoppedPoint
  | Now
  | TrimHorizon
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

instance FromText InputStartingPosition where
  parser =
    takeLowerText >>= \case
      "last_stopped_point" -> pure LastStoppedPoint
      "now" -> pure Now
      "trim_horizon" -> pure TrimHorizon
      e ->
        fromTextError $
          "Failure parsing InputStartingPosition from value: '" <> e
            <> "'. Accepted values: last_stopped_point, now, trim_horizon"

instance ToText InputStartingPosition where
  toText = \case
    LastStoppedPoint -> "LAST_STOPPED_POINT"
    Now -> "NOW"
    TrimHorizon -> "TRIM_HORIZON"

instance Hashable InputStartingPosition

instance NFData InputStartingPosition

instance ToByteString InputStartingPosition

instance ToQuery InputStartingPosition

instance ToHeader InputStartingPosition

instance ToJSON InputStartingPosition where
  toJSON = toJSONText

instance FromJSON InputStartingPosition where
  parseJSON = parseJSONText "InputStartingPosition"
