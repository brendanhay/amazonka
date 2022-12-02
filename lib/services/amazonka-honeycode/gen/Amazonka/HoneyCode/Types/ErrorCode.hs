{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.HoneyCode.Types.ErrorCode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.HoneyCode.Types.ErrorCode
  ( ErrorCode
      ( ..,
        ErrorCode_ACCESS_DENIED,
        ErrorCode_FILE_EMPTY_ERROR,
        ErrorCode_FILE_NOT_FOUND_ERROR,
        ErrorCode_FILE_PARSING_ERROR,
        ErrorCode_FILE_SIZE_LIMIT_ERROR,
        ErrorCode_INVALID_FILE_TYPE_ERROR,
        ErrorCode_INVALID_IMPORT_OPTIONS_ERROR,
        ErrorCode_INVALID_TABLE_COLUMN_ID_ERROR,
        ErrorCode_INVALID_TABLE_ID_ERROR,
        ErrorCode_INVALID_URL_ERROR,
        ErrorCode_RESOURCE_NOT_FOUND_ERROR,
        ErrorCode_SYSTEM_LIMIT_ERROR,
        ErrorCode_TABLE_NOT_FOUND_ERROR,
        ErrorCode_UNKNOWN_ERROR
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ErrorCode = ErrorCode'
  { fromErrorCode ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern ErrorCode_ACCESS_DENIED :: ErrorCode
pattern ErrorCode_ACCESS_DENIED = ErrorCode' "ACCESS_DENIED"

pattern ErrorCode_FILE_EMPTY_ERROR :: ErrorCode
pattern ErrorCode_FILE_EMPTY_ERROR = ErrorCode' "FILE_EMPTY_ERROR"

pattern ErrorCode_FILE_NOT_FOUND_ERROR :: ErrorCode
pattern ErrorCode_FILE_NOT_FOUND_ERROR = ErrorCode' "FILE_NOT_FOUND_ERROR"

pattern ErrorCode_FILE_PARSING_ERROR :: ErrorCode
pattern ErrorCode_FILE_PARSING_ERROR = ErrorCode' "FILE_PARSING_ERROR"

pattern ErrorCode_FILE_SIZE_LIMIT_ERROR :: ErrorCode
pattern ErrorCode_FILE_SIZE_LIMIT_ERROR = ErrorCode' "FILE_SIZE_LIMIT_ERROR"

pattern ErrorCode_INVALID_FILE_TYPE_ERROR :: ErrorCode
pattern ErrorCode_INVALID_FILE_TYPE_ERROR = ErrorCode' "INVALID_FILE_TYPE_ERROR"

pattern ErrorCode_INVALID_IMPORT_OPTIONS_ERROR :: ErrorCode
pattern ErrorCode_INVALID_IMPORT_OPTIONS_ERROR = ErrorCode' "INVALID_IMPORT_OPTIONS_ERROR"

pattern ErrorCode_INVALID_TABLE_COLUMN_ID_ERROR :: ErrorCode
pattern ErrorCode_INVALID_TABLE_COLUMN_ID_ERROR = ErrorCode' "INVALID_TABLE_COLUMN_ID_ERROR"

pattern ErrorCode_INVALID_TABLE_ID_ERROR :: ErrorCode
pattern ErrorCode_INVALID_TABLE_ID_ERROR = ErrorCode' "INVALID_TABLE_ID_ERROR"

pattern ErrorCode_INVALID_URL_ERROR :: ErrorCode
pattern ErrorCode_INVALID_URL_ERROR = ErrorCode' "INVALID_URL_ERROR"

pattern ErrorCode_RESOURCE_NOT_FOUND_ERROR :: ErrorCode
pattern ErrorCode_RESOURCE_NOT_FOUND_ERROR = ErrorCode' "RESOURCE_NOT_FOUND_ERROR"

pattern ErrorCode_SYSTEM_LIMIT_ERROR :: ErrorCode
pattern ErrorCode_SYSTEM_LIMIT_ERROR = ErrorCode' "SYSTEM_LIMIT_ERROR"

pattern ErrorCode_TABLE_NOT_FOUND_ERROR :: ErrorCode
pattern ErrorCode_TABLE_NOT_FOUND_ERROR = ErrorCode' "TABLE_NOT_FOUND_ERROR"

pattern ErrorCode_UNKNOWN_ERROR :: ErrorCode
pattern ErrorCode_UNKNOWN_ERROR = ErrorCode' "UNKNOWN_ERROR"

{-# COMPLETE
  ErrorCode_ACCESS_DENIED,
  ErrorCode_FILE_EMPTY_ERROR,
  ErrorCode_FILE_NOT_FOUND_ERROR,
  ErrorCode_FILE_PARSING_ERROR,
  ErrorCode_FILE_SIZE_LIMIT_ERROR,
  ErrorCode_INVALID_FILE_TYPE_ERROR,
  ErrorCode_INVALID_IMPORT_OPTIONS_ERROR,
  ErrorCode_INVALID_TABLE_COLUMN_ID_ERROR,
  ErrorCode_INVALID_TABLE_ID_ERROR,
  ErrorCode_INVALID_URL_ERROR,
  ErrorCode_RESOURCE_NOT_FOUND_ERROR,
  ErrorCode_SYSTEM_LIMIT_ERROR,
  ErrorCode_TABLE_NOT_FOUND_ERROR,
  ErrorCode_UNKNOWN_ERROR,
  ErrorCode'
  #-}
