{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.Types.BulkPublishStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoSync.Types.BulkPublishStatus where

import Network.AWS.Prelude

data BulkPublishStatus
  = Failed
  | InProgress
  | NotStarted
  | Succeeded
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

instance FromText BulkPublishStatus where
  parser =
    takeLowerText >>= \case
      "failed" -> pure Failed
      "in_progress" -> pure InProgress
      "not_started" -> pure NotStarted
      "succeeded" -> pure Succeeded
      e ->
        fromTextError $
          "Failure parsing BulkPublishStatus from value: '" <> e
            <> "'. Accepted values: failed, in_progress, not_started, succeeded"

instance ToText BulkPublishStatus where
  toText = \case
    Failed -> "FAILED"
    InProgress -> "IN_PROGRESS"
    NotStarted -> "NOT_STARTED"
    Succeeded -> "SUCCEEDED"

instance Hashable BulkPublishStatus

instance NFData BulkPublishStatus

instance ToByteString BulkPublishStatus

instance ToQuery BulkPublishStatus

instance ToHeader BulkPublishStatus

instance FromJSON BulkPublishStatus where
  parseJSON = parseJSONText "BulkPublishStatus"
