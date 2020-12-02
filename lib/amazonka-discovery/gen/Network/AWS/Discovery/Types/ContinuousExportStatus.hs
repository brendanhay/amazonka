{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.ContinuousExportStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Discovery.Types.ContinuousExportStatus where

import Network.AWS.Prelude

data ContinuousExportStatus
  = Active
  | Error'
  | Inactive
  | StartFailed
  | StartInProgress
  | StopFailed
  | StopInProgress
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

instance FromText ContinuousExportStatus where
  parser =
    takeLowerText >>= \case
      "active" -> pure Active
      "error" -> pure Error'
      "inactive" -> pure Inactive
      "start_failed" -> pure StartFailed
      "start_in_progress" -> pure StartInProgress
      "stop_failed" -> pure StopFailed
      "stop_in_progress" -> pure StopInProgress
      e ->
        fromTextError $
          "Failure parsing ContinuousExportStatus from value: '" <> e
            <> "'. Accepted values: active, error, inactive, start_failed, start_in_progress, stop_failed, stop_in_progress"

instance ToText ContinuousExportStatus where
  toText = \case
    Active -> "ACTIVE"
    Error' -> "ERROR"
    Inactive -> "INACTIVE"
    StartFailed -> "START_FAILED"
    StartInProgress -> "START_IN_PROGRESS"
    StopFailed -> "STOP_FAILED"
    StopInProgress -> "STOP_IN_PROGRESS"

instance Hashable ContinuousExportStatus

instance NFData ContinuousExportStatus

instance ToByteString ContinuousExportStatus

instance ToQuery ContinuousExportStatus

instance ToHeader ContinuousExportStatus

instance FromJSON ContinuousExportStatus where
  parseJSON = parseJSONText "ContinuousExportStatus"
