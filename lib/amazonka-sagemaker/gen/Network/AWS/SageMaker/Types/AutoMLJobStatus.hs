{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AutoMLJobStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AutoMLJobStatus where

import Network.AWS.Prelude

data AutoMLJobStatus
  = AMLJSCompleted
  | AMLJSFailed
  | AMLJSInProgress
  | AMLJSStopped
  | AMLJSStopping
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

instance FromText AutoMLJobStatus where
  parser =
    takeLowerText >>= \case
      "completed" -> pure AMLJSCompleted
      "failed" -> pure AMLJSFailed
      "inprogress" -> pure AMLJSInProgress
      "stopped" -> pure AMLJSStopped
      "stopping" -> pure AMLJSStopping
      e ->
        fromTextError $
          "Failure parsing AutoMLJobStatus from value: '" <> e
            <> "'. Accepted values: completed, failed, inprogress, stopped, stopping"

instance ToText AutoMLJobStatus where
  toText = \case
    AMLJSCompleted -> "Completed"
    AMLJSFailed -> "Failed"
    AMLJSInProgress -> "InProgress"
    AMLJSStopped -> "Stopped"
    AMLJSStopping -> "Stopping"

instance Hashable AutoMLJobStatus

instance NFData AutoMLJobStatus

instance ToByteString AutoMLJobStatus

instance ToQuery AutoMLJobStatus

instance ToHeader AutoMLJobStatus

instance ToJSON AutoMLJobStatus where
  toJSON = toJSONText

instance FromJSON AutoMLJobStatus where
  parseJSON = parseJSONText "AutoMLJobStatus"
