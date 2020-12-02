{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types.TranscriptionJobStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.TranscriptionJobStatus where

import Network.AWS.Prelude

data TranscriptionJobStatus
  = TJSCompleted
  | TJSFailed
  | TJSInProgress
  | TJSQueued
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

instance FromText TranscriptionJobStatus where
  parser =
    takeLowerText >>= \case
      "completed" -> pure TJSCompleted
      "failed" -> pure TJSFailed
      "in_progress" -> pure TJSInProgress
      "queued" -> pure TJSQueued
      e ->
        fromTextError $
          "Failure parsing TranscriptionJobStatus from value: '" <> e
            <> "'. Accepted values: completed, failed, in_progress, queued"

instance ToText TranscriptionJobStatus where
  toText = \case
    TJSCompleted -> "COMPLETED"
    TJSFailed -> "FAILED"
    TJSInProgress -> "IN_PROGRESS"
    TJSQueued -> "QUEUED"

instance Hashable TranscriptionJobStatus

instance NFData TranscriptionJobStatus

instance ToByteString TranscriptionJobStatus

instance ToQuery TranscriptionJobStatus

instance ToHeader TranscriptionJobStatus

instance ToJSON TranscriptionJobStatus where
  toJSON = toJSONText

instance FromJSON TranscriptionJobStatus where
  parseJSON = parseJSONText "TranscriptionJobStatus"
