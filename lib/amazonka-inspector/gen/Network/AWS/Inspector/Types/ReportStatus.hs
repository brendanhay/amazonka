{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.ReportStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.ReportStatus where

import Network.AWS.Prelude

data ReportStatus
  = RSCompleted
  | RSFailed
  | RSWorkInProgress
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

instance FromText ReportStatus where
  parser =
    takeLowerText >>= \case
      "completed" -> pure RSCompleted
      "failed" -> pure RSFailed
      "work_in_progress" -> pure RSWorkInProgress
      e ->
        fromTextError $
          "Failure parsing ReportStatus from value: '" <> e
            <> "'. Accepted values: completed, failed, work_in_progress"

instance ToText ReportStatus where
  toText = \case
    RSCompleted -> "COMPLETED"
    RSFailed -> "FAILED"
    RSWorkInProgress -> "WORK_IN_PROGRESS"

instance Hashable ReportStatus

instance NFData ReportStatus

instance ToByteString ReportStatus

instance ToQuery ReportStatus

instance ToHeader ReportStatus

instance FromJSON ReportStatus where
  parseJSON = parseJSONText "ReportStatus"
