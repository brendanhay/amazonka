{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.RecordStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.RecordStatus where

import Network.AWS.Prelude

data RecordStatus
  = RSCreated
  | RSFailed
  | RSInProgress
  | RSInProgressInError
  | RSSucceeded
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

instance FromText RecordStatus where
  parser =
    takeLowerText >>= \case
      "created" -> pure RSCreated
      "failed" -> pure RSFailed
      "in_progress" -> pure RSInProgress
      "in_progress_in_error" -> pure RSInProgressInError
      "succeeded" -> pure RSSucceeded
      e ->
        fromTextError $
          "Failure parsing RecordStatus from value: '" <> e
            <> "'. Accepted values: created, failed, in_progress, in_progress_in_error, succeeded"

instance ToText RecordStatus where
  toText = \case
    RSCreated -> "CREATED"
    RSFailed -> "FAILED"
    RSInProgress -> "IN_PROGRESS"
    RSInProgressInError -> "IN_PROGRESS_IN_ERROR"
    RSSucceeded -> "SUCCEEDED"

instance Hashable RecordStatus

instance NFData RecordStatus

instance ToByteString RecordStatus

instance ToQuery RecordStatus

instance ToHeader RecordStatus

instance FromJSON RecordStatus where
  parseJSON = parseJSONText "RecordStatus"
