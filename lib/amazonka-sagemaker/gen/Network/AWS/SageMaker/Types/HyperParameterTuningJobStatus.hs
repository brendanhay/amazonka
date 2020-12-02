{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.HyperParameterTuningJobStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.HyperParameterTuningJobStatus where

import Network.AWS.Prelude

data HyperParameterTuningJobStatus
  = HPTJSCompleted
  | HPTJSFailed
  | HPTJSInProgress
  | HPTJSStopped
  | HPTJSStopping
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

instance FromText HyperParameterTuningJobStatus where
  parser =
    takeLowerText >>= \case
      "completed" -> pure HPTJSCompleted
      "failed" -> pure HPTJSFailed
      "inprogress" -> pure HPTJSInProgress
      "stopped" -> pure HPTJSStopped
      "stopping" -> pure HPTJSStopping
      e ->
        fromTextError $
          "Failure parsing HyperParameterTuningJobStatus from value: '" <> e
            <> "'. Accepted values: completed, failed, inprogress, stopped, stopping"

instance ToText HyperParameterTuningJobStatus where
  toText = \case
    HPTJSCompleted -> "Completed"
    HPTJSFailed -> "Failed"
    HPTJSInProgress -> "InProgress"
    HPTJSStopped -> "Stopped"
    HPTJSStopping -> "Stopping"

instance Hashable HyperParameterTuningJobStatus

instance NFData HyperParameterTuningJobStatus

instance ToByteString HyperParameterTuningJobStatus

instance ToQuery HyperParameterTuningJobStatus

instance ToHeader HyperParameterTuningJobStatus

instance ToJSON HyperParameterTuningJobStatus where
  toJSON = toJSONText

instance FromJSON HyperParameterTuningJobStatus where
  parseJSON = parseJSONText "HyperParameterTuningJobStatus"
