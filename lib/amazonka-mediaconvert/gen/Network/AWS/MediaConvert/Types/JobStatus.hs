{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.JobStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.JobStatus where

import Network.AWS.Prelude

-- | A job's status can be SUBMITTED, PROGRESSING, COMPLETE, CANCELED, or ERROR.
data JobStatus
  = Canceled
  | Complete
  | Error'
  | Progressing
  | Submitted
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
      "canceled" -> pure Canceled
      "complete" -> pure Complete
      "error" -> pure Error'
      "progressing" -> pure Progressing
      "submitted" -> pure Submitted
      e ->
        fromTextError $
          "Failure parsing JobStatus from value: '" <> e
            <> "'. Accepted values: canceled, complete, error, progressing, submitted"

instance ToText JobStatus where
  toText = \case
    Canceled -> "CANCELED"
    Complete -> "COMPLETE"
    Error' -> "ERROR"
    Progressing -> "PROGRESSING"
    Submitted -> "SUBMITTED"

instance Hashable JobStatus

instance NFData JobStatus

instance ToByteString JobStatus

instance ToQuery JobStatus

instance ToHeader JobStatus

instance ToJSON JobStatus where
  toJSON = toJSONText

instance FromJSON JobStatus where
  parseJSON = parseJSONText "JobStatus"
