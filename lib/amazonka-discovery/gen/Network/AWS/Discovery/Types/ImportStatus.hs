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
        DeleteComplete,
        DeleteFailed,
        DeleteFailedLimitExceeded,
        DeleteInProgress,
        ImportComplete,
        ImportCompleteWithErrors,
        ImportFailed,
        ImportFailedRecordLimitExceeded,
        ImportFailedServerLimitExceeded,
        ImportInProgress,
        InternalError
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ImportStatus = ImportStatus' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern DeleteComplete :: ImportStatus
pattern DeleteComplete = ImportStatus' "DELETE_COMPLETE"

pattern DeleteFailed :: ImportStatus
pattern DeleteFailed = ImportStatus' "DELETE_FAILED"

pattern DeleteFailedLimitExceeded :: ImportStatus
pattern DeleteFailedLimitExceeded = ImportStatus' "DELETE_FAILED_LIMIT_EXCEEDED"

pattern DeleteInProgress :: ImportStatus
pattern DeleteInProgress = ImportStatus' "DELETE_IN_PROGRESS"

pattern ImportComplete :: ImportStatus
pattern ImportComplete = ImportStatus' "IMPORT_COMPLETE"

pattern ImportCompleteWithErrors :: ImportStatus
pattern ImportCompleteWithErrors = ImportStatus' "IMPORT_COMPLETE_WITH_ERRORS"

pattern ImportFailed :: ImportStatus
pattern ImportFailed = ImportStatus' "IMPORT_FAILED"

pattern ImportFailedRecordLimitExceeded :: ImportStatus
pattern ImportFailedRecordLimitExceeded = ImportStatus' "IMPORT_FAILED_RECORD_LIMIT_EXCEEDED"

pattern ImportFailedServerLimitExceeded :: ImportStatus
pattern ImportFailedServerLimitExceeded = ImportStatus' "IMPORT_FAILED_SERVER_LIMIT_EXCEEDED"

pattern ImportInProgress :: ImportStatus
pattern ImportInProgress = ImportStatus' "IMPORT_IN_PROGRESS"

pattern InternalError :: ImportStatus
pattern InternalError = ImportStatus' "INTERNAL_ERROR"

{-# COMPLETE
  DeleteComplete,
  DeleteFailed,
  DeleteFailedLimitExceeded,
  DeleteInProgress,
  ImportComplete,
  ImportCompleteWithErrors,
  ImportFailed,
  ImportFailedRecordLimitExceeded,
  ImportFailedServerLimitExceeded,
  ImportInProgress,
  InternalError,
  ImportStatus'
  #-}
