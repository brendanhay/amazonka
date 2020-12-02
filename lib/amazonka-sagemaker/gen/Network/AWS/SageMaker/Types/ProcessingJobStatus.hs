{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ProcessingJobStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ProcessingJobStatus where

import Network.AWS.Prelude

data ProcessingJobStatus
  = PJSCompleted
  | PJSFailed
  | PJSInProgress
  | PJSStopped
  | PJSStopping
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

instance FromText ProcessingJobStatus where
  parser =
    takeLowerText >>= \case
      "completed" -> pure PJSCompleted
      "failed" -> pure PJSFailed
      "inprogress" -> pure PJSInProgress
      "stopped" -> pure PJSStopped
      "stopping" -> pure PJSStopping
      e ->
        fromTextError $
          "Failure parsing ProcessingJobStatus from value: '" <> e
            <> "'. Accepted values: completed, failed, inprogress, stopped, stopping"

instance ToText ProcessingJobStatus where
  toText = \case
    PJSCompleted -> "Completed"
    PJSFailed -> "Failed"
    PJSInProgress -> "InProgress"
    PJSStopped -> "Stopped"
    PJSStopping -> "Stopping"

instance Hashable ProcessingJobStatus

instance NFData ProcessingJobStatus

instance ToByteString ProcessingJobStatus

instance ToQuery ProcessingJobStatus

instance ToHeader ProcessingJobStatus

instance ToJSON ProcessingJobStatus where
  toJSON = toJSONText

instance FromJSON ProcessingJobStatus where
  parseJSON = parseJSONText "ProcessingJobStatus"
