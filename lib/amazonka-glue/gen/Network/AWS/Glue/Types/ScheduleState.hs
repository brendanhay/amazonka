{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.ScheduleState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.ScheduleState where

import Network.AWS.Prelude

data ScheduleState
  = SSNotScheduled
  | SSScheduled
  | SSTransitioning
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

instance FromText ScheduleState where
  parser =
    takeLowerText >>= \case
      "not_scheduled" -> pure SSNotScheduled
      "scheduled" -> pure SSScheduled
      "transitioning" -> pure SSTransitioning
      e ->
        fromTextError $
          "Failure parsing ScheduleState from value: '" <> e
            <> "'. Accepted values: not_scheduled, scheduled, transitioning"

instance ToText ScheduleState where
  toText = \case
    SSNotScheduled -> "NOT_SCHEDULED"
    SSScheduled -> "SCHEDULED"
    SSTransitioning -> "TRANSITIONING"

instance Hashable ScheduleState

instance NFData ScheduleState

instance ToByteString ScheduleState

instance ToQuery ScheduleState

instance ToHeader ScheduleState

instance FromJSON ScheduleState where
  parseJSON = parseJSONText "ScheduleState"
