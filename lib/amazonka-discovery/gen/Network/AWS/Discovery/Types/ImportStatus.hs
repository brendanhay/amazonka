{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.ImportStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Discovery.Types.ImportStatus
  ( ImportStatus
      ( ImportStatus',
        ImportStatusImportInProgress,
        ImportStatusImportComplete,
        ImportStatusImportCompleteWithErrors,
        ImportStatusImportFailed,
        ImportStatusImportFailedServerLimitExceeded,
        ImportStatusImportFailedRecordLimitExceeded,
        ImportStatusDeleteInProgress,
        ImportStatusDeleteComplete,
        ImportStatusDeleteFailed,
        ImportStatusDeleteFailedLimitExceeded,
        ImportStatusInternalError,
        fromImportStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ImportStatus = ImportStatus' {fromImportStatus :: Core.Text}
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern ImportStatusImportInProgress :: ImportStatus
pattern ImportStatusImportInProgress = ImportStatus' "IMPORT_IN_PROGRESS"

pattern ImportStatusImportComplete :: ImportStatus
pattern ImportStatusImportComplete = ImportStatus' "IMPORT_COMPLETE"

pattern ImportStatusImportCompleteWithErrors :: ImportStatus
pattern ImportStatusImportCompleteWithErrors = ImportStatus' "IMPORT_COMPLETE_WITH_ERRORS"

pattern ImportStatusImportFailed :: ImportStatus
pattern ImportStatusImportFailed = ImportStatus' "IMPORT_FAILED"

pattern ImportStatusImportFailedServerLimitExceeded :: ImportStatus
pattern ImportStatusImportFailedServerLimitExceeded = ImportStatus' "IMPORT_FAILED_SERVER_LIMIT_EXCEEDED"

pattern ImportStatusImportFailedRecordLimitExceeded :: ImportStatus
pattern ImportStatusImportFailedRecordLimitExceeded = ImportStatus' "IMPORT_FAILED_RECORD_LIMIT_EXCEEDED"

pattern ImportStatusDeleteInProgress :: ImportStatus
pattern ImportStatusDeleteInProgress = ImportStatus' "DELETE_IN_PROGRESS"

pattern ImportStatusDeleteComplete :: ImportStatus
pattern ImportStatusDeleteComplete = ImportStatus' "DELETE_COMPLETE"

pattern ImportStatusDeleteFailed :: ImportStatus
pattern ImportStatusDeleteFailed = ImportStatus' "DELETE_FAILED"

pattern ImportStatusDeleteFailedLimitExceeded :: ImportStatus
pattern ImportStatusDeleteFailedLimitExceeded = ImportStatus' "DELETE_FAILED_LIMIT_EXCEEDED"

pattern ImportStatusInternalError :: ImportStatus
pattern ImportStatusInternalError = ImportStatus' "INTERNAL_ERROR"

{-# COMPLETE
  ImportStatusImportInProgress,
  ImportStatusImportComplete,
  ImportStatusImportCompleteWithErrors,
  ImportStatusImportFailed,
  ImportStatusImportFailedServerLimitExceeded,
  ImportStatusImportFailedRecordLimitExceeded,
  ImportStatusDeleteInProgress,
  ImportStatusDeleteComplete,
  ImportStatusDeleteFailed,
  ImportStatusDeleteFailedLimitExceeded,
  ImportStatusInternalError,
  ImportStatus'
  #-}
