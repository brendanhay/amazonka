{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.JobStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.JobStatus where

import Network.AWS.Prelude

data JobStatus
  = JSCanceled
  | JSCompleted
  | JSDeletionInProgress
  | JSInProgress
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

instance FromText JobStatus where
  parser =
    takeLowerText >>= \case
      "canceled" -> pure JSCanceled
      "completed" -> pure JSCompleted
      "deletion_in_progress" -> pure JSDeletionInProgress
      "in_progress" -> pure JSInProgress
      e ->
        fromTextError $
          "Failure parsing JobStatus from value: '" <> e
            <> "'. Accepted values: canceled, completed, deletion_in_progress, in_progress"

instance ToText JobStatus where
  toText = \case
    JSCanceled -> "CANCELED"
    JSCompleted -> "COMPLETED"
    JSDeletionInProgress -> "DELETION_IN_PROGRESS"
    JSInProgress -> "IN_PROGRESS"

instance Hashable JobStatus

instance NFData JobStatus

instance ToByteString JobStatus

instance ToQuery JobStatus

instance ToHeader JobStatus

instance ToJSON JobStatus where
  toJSON = toJSONText

instance FromJSON JobStatus where
  parseJSON = parseJSONText "JobStatus"
