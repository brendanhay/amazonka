{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ScheduleState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ScheduleState where

import Network.AWS.Prelude
import Network.AWS.Redshift.Internal

data ScheduleState
  = Active
  | Failed
  | Modifying
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
      "active" -> pure Active
      "failed" -> pure Failed
      "modifying" -> pure Modifying
      e ->
        fromTextError $
          "Failure parsing ScheduleState from value: '" <> e
            <> "'. Accepted values: active, failed, modifying"

instance ToText ScheduleState where
  toText = \case
    Active -> "ACTIVE"
    Failed -> "FAILED"
    Modifying -> "MODIFYING"

instance Hashable ScheduleState

instance NFData ScheduleState

instance ToByteString ScheduleState

instance ToQuery ScheduleState

instance ToHeader ScheduleState

instance FromXML ScheduleState where
  parseXML = parseXMLText "ScheduleState"
