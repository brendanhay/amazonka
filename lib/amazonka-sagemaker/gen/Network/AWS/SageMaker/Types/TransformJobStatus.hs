{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TransformJobStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TransformJobStatus where

import Network.AWS.Prelude

data TransformJobStatus
  = TCompleted
  | TFailed
  | TInProgress
  | TStopped
  | TStopping
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

instance FromText TransformJobStatus where
  parser =
    takeLowerText >>= \case
      "completed" -> pure TCompleted
      "failed" -> pure TFailed
      "inprogress" -> pure TInProgress
      "stopped" -> pure TStopped
      "stopping" -> pure TStopping
      e ->
        fromTextError $
          "Failure parsing TransformJobStatus from value: '" <> e
            <> "'. Accepted values: completed, failed, inprogress, stopped, stopping"

instance ToText TransformJobStatus where
  toText = \case
    TCompleted -> "Completed"
    TFailed -> "Failed"
    TInProgress -> "InProgress"
    TStopped -> "Stopped"
    TStopping -> "Stopping"

instance Hashable TransformJobStatus

instance NFData TransformJobStatus

instance ToByteString TransformJobStatus

instance ToQuery TransformJobStatus

instance ToHeader TransformJobStatus

instance ToJSON TransformJobStatus where
  toJSON = toJSONText

instance FromJSON TransformJobStatus where
  parseJSON = parseJSONText "TransformJobStatus"
