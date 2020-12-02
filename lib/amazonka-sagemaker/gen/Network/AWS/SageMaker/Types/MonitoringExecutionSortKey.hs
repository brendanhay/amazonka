{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.MonitoringExecutionSortKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.MonitoringExecutionSortKey where

import Network.AWS.Prelude

data MonitoringExecutionSortKey
  = MESKCreationTime
  | MESKScheduledTime
  | MESKStatus
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

instance FromText MonitoringExecutionSortKey where
  parser =
    takeLowerText >>= \case
      "creationtime" -> pure MESKCreationTime
      "scheduledtime" -> pure MESKScheduledTime
      "status" -> pure MESKStatus
      e ->
        fromTextError $
          "Failure parsing MonitoringExecutionSortKey from value: '" <> e
            <> "'. Accepted values: creationtime, scheduledtime, status"

instance ToText MonitoringExecutionSortKey where
  toText = \case
    MESKCreationTime -> "CreationTime"
    MESKScheduledTime -> "ScheduledTime"
    MESKStatus -> "Status"

instance Hashable MonitoringExecutionSortKey

instance NFData MonitoringExecutionSortKey

instance ToByteString MonitoringExecutionSortKey

instance ToQuery MonitoringExecutionSortKey

instance ToHeader MonitoringExecutionSortKey

instance ToJSON MonitoringExecutionSortKey where
  toJSON = toJSONText
