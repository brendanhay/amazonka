{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MigrationHub.Types.MigrationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MigrationHub.Types.MigrationStatus where

import Network.AWS.Prelude

data MigrationStatus
  = MSCompleted
  | MSFailed
  | MSInProgress
  | MSNotStarted
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

instance FromText MigrationStatus where
  parser =
    takeLowerText >>= \case
      "completed" -> pure MSCompleted
      "failed" -> pure MSFailed
      "in_progress" -> pure MSInProgress
      "not_started" -> pure MSNotStarted
      e ->
        fromTextError $
          "Failure parsing MigrationStatus from value: '" <> e
            <> "'. Accepted values: completed, failed, in_progress, not_started"

instance ToText MigrationStatus where
  toText = \case
    MSCompleted -> "COMPLETED"
    MSFailed -> "FAILED"
    MSInProgress -> "IN_PROGRESS"
    MSNotStarted -> "NOT_STARTED"

instance Hashable MigrationStatus

instance NFData MigrationStatus

instance ToByteString MigrationStatus

instance ToQuery MigrationStatus

instance ToHeader MigrationStatus

instance ToJSON MigrationStatus where
  toJSON = toJSONText

instance FromJSON MigrationStatus where
  parseJSON = parseJSONText "MigrationStatus"
