{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.Types.TransitionToIARules
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EFS.Types.TransitionToIARules where

import Network.AWS.Prelude

data TransitionToIARules
  = After14Days
  | After30Days
  | After60Days
  | After7Days
  | After90Days
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

instance FromText TransitionToIARules where
  parser =
    takeLowerText >>= \case
      "after_14_days" -> pure After14Days
      "after_30_days" -> pure After30Days
      "after_60_days" -> pure After60Days
      "after_7_days" -> pure After7Days
      "after_90_days" -> pure After90Days
      e ->
        fromTextError $
          "Failure parsing TransitionToIARules from value: '" <> e
            <> "'. Accepted values: after_14_days, after_30_days, after_60_days, after_7_days, after_90_days"

instance ToText TransitionToIARules where
  toText = \case
    After14Days -> "AFTER_14_DAYS"
    After30Days -> "AFTER_30_DAYS"
    After60Days -> "AFTER_60_DAYS"
    After7Days -> "AFTER_7_DAYS"
    After90Days -> "AFTER_90_DAYS"

instance Hashable TransitionToIARules

instance NFData TransitionToIARules

instance ToByteString TransitionToIARules

instance ToQuery TransitionToIARules

instance ToHeader TransitionToIARules

instance ToJSON TransitionToIARules where
  toJSON = toJSONText

instance FromJSON TransitionToIARules where
  parseJSON = parseJSONText "TransitionToIARules"
