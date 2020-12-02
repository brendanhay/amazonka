{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.ImportStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Discovery.Types.ImportStatus where

import Network.AWS.Prelude

data ImportStatus
  = DeleteComplete
  | DeleteFailed
  | DeleteFailedLimitExceeded
  | DeleteInProgress
  | ImportComplete
  | ImportCompleteWithErrors
  | ImportFailed
  | ImportFailedRecordLimitExceeded
  | ImportFailedServerLimitExceeded
  | ImportInProgress
  | InternalError
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

instance FromText ImportStatus where
  parser =
    takeLowerText >>= \case
      "delete_complete" -> pure DeleteComplete
      "delete_failed" -> pure DeleteFailed
      "delete_failed_limit_exceeded" -> pure DeleteFailedLimitExceeded
      "delete_in_progress" -> pure DeleteInProgress
      "import_complete" -> pure ImportComplete
      "import_complete_with_errors" -> pure ImportCompleteWithErrors
      "import_failed" -> pure ImportFailed
      "import_failed_record_limit_exceeded" -> pure ImportFailedRecordLimitExceeded
      "import_failed_server_limit_exceeded" -> pure ImportFailedServerLimitExceeded
      "import_in_progress" -> pure ImportInProgress
      "internal_error" -> pure InternalError
      e ->
        fromTextError $
          "Failure parsing ImportStatus from value: '" <> e
            <> "'. Accepted values: delete_complete, delete_failed, delete_failed_limit_exceeded, delete_in_progress, import_complete, import_complete_with_errors, import_failed, import_failed_record_limit_exceeded, import_failed_server_limit_exceeded, import_in_progress, internal_error"

instance ToText ImportStatus where
  toText = \case
    DeleteComplete -> "DELETE_COMPLETE"
    DeleteFailed -> "DELETE_FAILED"
    DeleteFailedLimitExceeded -> "DELETE_FAILED_LIMIT_EXCEEDED"
    DeleteInProgress -> "DELETE_IN_PROGRESS"
    ImportComplete -> "IMPORT_COMPLETE"
    ImportCompleteWithErrors -> "IMPORT_COMPLETE_WITH_ERRORS"
    ImportFailed -> "IMPORT_FAILED"
    ImportFailedRecordLimitExceeded -> "IMPORT_FAILED_RECORD_LIMIT_EXCEEDED"
    ImportFailedServerLimitExceeded -> "IMPORT_FAILED_SERVER_LIMIT_EXCEEDED"
    ImportInProgress -> "IMPORT_IN_PROGRESS"
    InternalError -> "INTERNAL_ERROR"

instance Hashable ImportStatus

instance NFData ImportStatus

instance ToByteString ImportStatus

instance ToQuery ImportStatus

instance ToHeader ImportStatus

instance FromJSON ImportStatus where
  parseJSON = parseJSONText "ImportStatus"
