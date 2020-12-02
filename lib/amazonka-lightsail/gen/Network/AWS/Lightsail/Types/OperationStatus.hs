{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.OperationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.OperationStatus where

import Network.AWS.Prelude

data OperationStatus
  = OSCompleted
  | OSFailed
  | OSNotStarted
  | OSStarted
  | OSSucceeded
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

instance FromText OperationStatus where
  parser =
    takeLowerText >>= \case
      "completed" -> pure OSCompleted
      "failed" -> pure OSFailed
      "notstarted" -> pure OSNotStarted
      "started" -> pure OSStarted
      "succeeded" -> pure OSSucceeded
      e ->
        fromTextError $
          "Failure parsing OperationStatus from value: '" <> e
            <> "'. Accepted values: completed, failed, notstarted, started, succeeded"

instance ToText OperationStatus where
  toText = \case
    OSCompleted -> "Completed"
    OSFailed -> "Failed"
    OSNotStarted -> "NotStarted"
    OSStarted -> "Started"
    OSSucceeded -> "Succeeded"

instance Hashable OperationStatus

instance NFData OperationStatus

instance ToByteString OperationStatus

instance ToQuery OperationStatus

instance ToHeader OperationStatus

instance FromJSON OperationStatus where
  parseJSON = parseJSONText "OperationStatus"
