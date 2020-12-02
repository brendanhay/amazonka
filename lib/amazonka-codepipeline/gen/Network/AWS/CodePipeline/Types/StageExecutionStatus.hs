{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.StageExecutionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.StageExecutionStatus where

import Network.AWS.Prelude

data StageExecutionStatus
  = SESFailed
  | SESInProgress
  | SESStopped
  | SESStopping
  | SESSucceeded
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

instance FromText StageExecutionStatus where
  parser =
    takeLowerText >>= \case
      "failed" -> pure SESFailed
      "inprogress" -> pure SESInProgress
      "stopped" -> pure SESStopped
      "stopping" -> pure SESStopping
      "succeeded" -> pure SESSucceeded
      e ->
        fromTextError $
          "Failure parsing StageExecutionStatus from value: '" <> e
            <> "'. Accepted values: failed, inprogress, stopped, stopping, succeeded"

instance ToText StageExecutionStatus where
  toText = \case
    SESFailed -> "Failed"
    SESInProgress -> "InProgress"
    SESStopped -> "Stopped"
    SESStopping -> "Stopping"
    SESSucceeded -> "Succeeded"

instance Hashable StageExecutionStatus

instance NFData StageExecutionStatus

instance ToByteString StageExecutionStatus

instance ToQuery StageExecutionStatus

instance ToHeader StageExecutionStatus

instance FromJSON StageExecutionStatus where
  parseJSON = parseJSONText "StageExecutionStatus"
