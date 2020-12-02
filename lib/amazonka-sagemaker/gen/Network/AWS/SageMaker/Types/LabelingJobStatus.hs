{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.LabelingJobStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.LabelingJobStatus where

import Network.AWS.Prelude

data LabelingJobStatus
  = LJSCompleted
  | LJSFailed
  | LJSInProgress
  | LJSInitializing
  | LJSStopped
  | LJSStopping
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

instance FromText LabelingJobStatus where
  parser =
    takeLowerText >>= \case
      "completed" -> pure LJSCompleted
      "failed" -> pure LJSFailed
      "inprogress" -> pure LJSInProgress
      "initializing" -> pure LJSInitializing
      "stopped" -> pure LJSStopped
      "stopping" -> pure LJSStopping
      e ->
        fromTextError $
          "Failure parsing LabelingJobStatus from value: '" <> e
            <> "'. Accepted values: completed, failed, inprogress, initializing, stopped, stopping"

instance ToText LabelingJobStatus where
  toText = \case
    LJSCompleted -> "Completed"
    LJSFailed -> "Failed"
    LJSInProgress -> "InProgress"
    LJSInitializing -> "Initializing"
    LJSStopped -> "Stopped"
    LJSStopping -> "Stopping"

instance Hashable LabelingJobStatus

instance NFData LabelingJobStatus

instance ToByteString LabelingJobStatus

instance ToQuery LabelingJobStatus

instance ToHeader LabelingJobStatus

instance ToJSON LabelingJobStatus where
  toJSON = toJSONText

instance FromJSON LabelingJobStatus where
  parseJSON = parseJSONText "LabelingJobStatus"
