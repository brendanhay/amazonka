{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.ReprocessingStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.ReprocessingStatus where

import Network.AWS.Prelude

data ReprocessingStatus
  = Cancelled
  | Failed
  | Running
  | Succeeded
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

instance FromText ReprocessingStatus where
  parser =
    takeLowerText >>= \case
      "cancelled" -> pure Cancelled
      "failed" -> pure Failed
      "running" -> pure Running
      "succeeded" -> pure Succeeded
      e ->
        fromTextError $
          "Failure parsing ReprocessingStatus from value: '" <> e
            <> "'. Accepted values: cancelled, failed, running, succeeded"

instance ToText ReprocessingStatus where
  toText = \case
    Cancelled -> "CANCELLED"
    Failed -> "FAILED"
    Running -> "RUNNING"
    Succeeded -> "SUCCEEDED"

instance Hashable ReprocessingStatus

instance NFData ReprocessingStatus

instance ToByteString ReprocessingStatus

instance ToQuery ReprocessingStatus

instance ToHeader ReprocessingStatus

instance FromJSON ReprocessingStatus where
  parseJSON = parseJSONText "ReprocessingStatus"
