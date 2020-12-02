{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.Duration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.Duration where

import Network.AWS.Prelude

data Duration
  = Day14
  | Day30
  | Day7
  | Hr24
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

instance FromText Duration where
  parser =
    takeLowerText >>= \case
      "day_14" -> pure Day14
      "day_30" -> pure Day30
      "day_7" -> pure Day7
      "hr_24" -> pure Hr24
      e ->
        fromTextError $
          "Failure parsing Duration from value: '" <> e
            <> "'. Accepted values: day_14, day_30, day_7, hr_24"

instance ToText Duration where
  toText = \case
    Day14 -> "DAY_14"
    Day30 -> "DAY_30"
    Day7 -> "DAY_7"
    Hr24 -> "HR_24"

instance Hashable Duration

instance NFData Duration

instance ToByteString Duration

instance ToQuery Duration

instance ToHeader Duration

instance ToJSON Duration where
  toJSON = toJSONText

instance FromJSON Duration where
  parseJSON = parseJSONText "Duration"
