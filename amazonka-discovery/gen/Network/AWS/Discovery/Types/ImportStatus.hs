{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.ImportStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Discovery.Types.ImportStatus
  ( ImportStatus
      ( ..,
        ImportStatus_DELETE_COMPLETE,
        ImportStatus_DELETE_FAILED,
        ImportStatus_DELETE_FAILED_LIMIT_EXCEEDED,
        ImportStatus_DELETE_IN_PROGRESS,
        ImportStatus_IMPORT_COMPLETE,
        ImportStatus_IMPORT_COMPLETE_WITH_ERRORS,
        ImportStatus_IMPORT_FAILED,
        ImportStatus_IMPORT_FAILED_RECORD_LIMIT_EXCEEDED,
        ImportStatus_IMPORT_FAILED_SERVER_LIMIT_EXCEEDED,
        ImportStatus_IMPORT_IN_PROGRESS,
        ImportStatus_INTERNAL_ERROR
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype ImportStatus = ImportStatus'
  { fromImportStatus ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern ImportStatus_DELETE_COMPLETE :: ImportStatus
pattern ImportStatus_DELETE_COMPLETE = ImportStatus' "DELETE_COMPLETE"

pattern ImportStatus_DELETE_FAILED :: ImportStatus
pattern ImportStatus_DELETE_FAILED = ImportStatus' "DELETE_FAILED"

pattern ImportStatus_DELETE_FAILED_LIMIT_EXCEEDED :: ImportStatus
pattern ImportStatus_DELETE_FAILED_LIMIT_EXCEEDED = ImportStatus' "DELETE_FAILED_LIMIT_EXCEEDED"

pattern ImportStatus_DELETE_IN_PROGRESS :: ImportStatus
pattern ImportStatus_DELETE_IN_PROGRESS = ImportStatus' "DELETE_IN_PROGRESS"

pattern ImportStatus_IMPORT_COMPLETE :: ImportStatus
pattern ImportStatus_IMPORT_COMPLETE = ImportStatus' "IMPORT_COMPLETE"

pattern ImportStatus_IMPORT_COMPLETE_WITH_ERRORS :: ImportStatus
pattern ImportStatus_IMPORT_COMPLETE_WITH_ERRORS = ImportStatus' "IMPORT_COMPLETE_WITH_ERRORS"

pattern ImportStatus_IMPORT_FAILED :: ImportStatus
pattern ImportStatus_IMPORT_FAILED = ImportStatus' "IMPORT_FAILED"

pattern ImportStatus_IMPORT_FAILED_RECORD_LIMIT_EXCEEDED :: ImportStatus
pattern ImportStatus_IMPORT_FAILED_RECORD_LIMIT_EXCEEDED = ImportStatus' "IMPORT_FAILED_RECORD_LIMIT_EXCEEDED"

pattern ImportStatus_IMPORT_FAILED_SERVER_LIMIT_EXCEEDED :: ImportStatus
pattern ImportStatus_IMPORT_FAILED_SERVER_LIMIT_EXCEEDED = ImportStatus' "IMPORT_FAILED_SERVER_LIMIT_EXCEEDED"

pattern ImportStatus_IMPORT_IN_PROGRESS :: ImportStatus
pattern ImportStatus_IMPORT_IN_PROGRESS = ImportStatus' "IMPORT_IN_PROGRESS"

pattern ImportStatus_INTERNAL_ERROR :: ImportStatus
pattern ImportStatus_INTERNAL_ERROR = ImportStatus' "INTERNAL_ERROR"

{-# COMPLETE
  ImportStatus_DELETE_COMPLETE,
  ImportStatus_DELETE_FAILED,
  ImportStatus_DELETE_FAILED_LIMIT_EXCEEDED,
  ImportStatus_DELETE_IN_PROGRESS,
  ImportStatus_IMPORT_COMPLETE,
  ImportStatus_IMPORT_COMPLETE_WITH_ERRORS,
  ImportStatus_IMPORT_FAILED,
  ImportStatus_IMPORT_FAILED_RECORD_LIMIT_EXCEEDED,
  ImportStatus_IMPORT_FAILED_SERVER_LIMIT_EXCEEDED,
  ImportStatus_IMPORT_IN_PROGRESS,
  ImportStatus_INTERNAL_ERROR,
  ImportStatus'
  #-}
