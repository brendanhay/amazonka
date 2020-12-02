{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types.ModelStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.ModelStatus where

import Network.AWS.Prelude

data ModelStatus
  = MSCompleted
  | MSFailed
  | MSInProgress
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

instance FromText ModelStatus where
  parser =
    takeLowerText >>= \case
      "completed" -> pure MSCompleted
      "failed" -> pure MSFailed
      "in_progress" -> pure MSInProgress
      e ->
        fromTextError $
          "Failure parsing ModelStatus from value: '" <> e
            <> "'. Accepted values: completed, failed, in_progress"

instance ToText ModelStatus where
  toText = \case
    MSCompleted -> "COMPLETED"
    MSFailed -> "FAILED"
    MSInProgress -> "IN_PROGRESS"

instance Hashable ModelStatus

instance NFData ModelStatus

instance ToByteString ModelStatus

instance ToQuery ModelStatus

instance ToHeader ModelStatus

instance ToJSON ModelStatus where
  toJSON = toJSONText

instance FromJSON ModelStatus where
  parseJSON = parseJSONText "ModelStatus"
