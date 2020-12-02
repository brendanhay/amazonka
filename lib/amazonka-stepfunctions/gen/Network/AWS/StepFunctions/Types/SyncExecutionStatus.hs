{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.SyncExecutionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.SyncExecutionStatus where

import Network.AWS.Prelude

data SyncExecutionStatus
  = SESFailed
  | SESSucceeded
  | SESTimedOut
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

instance FromText SyncExecutionStatus where
  parser =
    takeLowerText >>= \case
      "failed" -> pure SESFailed
      "succeeded" -> pure SESSucceeded
      "timed_out" -> pure SESTimedOut
      e ->
        fromTextError $
          "Failure parsing SyncExecutionStatus from value: '" <> e
            <> "'. Accepted values: failed, succeeded, timed_out"

instance ToText SyncExecutionStatus where
  toText = \case
    SESFailed -> "FAILED"
    SESSucceeded -> "SUCCEEDED"
    SESTimedOut -> "TIMED_OUT"

instance Hashable SyncExecutionStatus

instance NFData SyncExecutionStatus

instance ToByteString SyncExecutionStatus

instance ToQuery SyncExecutionStatus

instance ToHeader SyncExecutionStatus

instance FromJSON SyncExecutionStatus where
  parseJSON = parseJSONText "SyncExecutionStatus"
