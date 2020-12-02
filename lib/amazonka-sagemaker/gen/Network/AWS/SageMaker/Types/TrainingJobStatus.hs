{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TrainingJobStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TrainingJobStatus where

import Network.AWS.Prelude

data TrainingJobStatus
  = TJSCompleted
  | TJSFailed
  | TJSInProgress
  | TJSStopped
  | TJSStopping
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

instance FromText TrainingJobStatus where
  parser =
    takeLowerText >>= \case
      "completed" -> pure TJSCompleted
      "failed" -> pure TJSFailed
      "inprogress" -> pure TJSInProgress
      "stopped" -> pure TJSStopped
      "stopping" -> pure TJSStopping
      e ->
        fromTextError $
          "Failure parsing TrainingJobStatus from value: '" <> e
            <> "'. Accepted values: completed, failed, inprogress, stopped, stopping"

instance ToText TrainingJobStatus where
  toText = \case
    TJSCompleted -> "Completed"
    TJSFailed -> "Failed"
    TJSInProgress -> "InProgress"
    TJSStopped -> "Stopped"
    TJSStopping -> "Stopping"

instance Hashable TrainingJobStatus

instance NFData TrainingJobStatus

instance ToByteString TrainingJobStatus

instance ToQuery TrainingJobStatus

instance ToHeader TrainingJobStatus

instance ToJSON TrainingJobStatus where
  toJSON = toJSONText

instance FromJSON TrainingJobStatus where
  parseJSON = parseJSONText "TrainingJobStatus"
