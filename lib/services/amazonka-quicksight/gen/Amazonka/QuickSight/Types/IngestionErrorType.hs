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
-- Module      : Amazonka.QuickSight.Types.IngestionErrorType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.IngestionErrorType
  ( IngestionErrorType
      ( ..,
        IngestionErrorType_ACCOUNT_CAPACITY_LIMIT_EXCEEDED,
        IngestionErrorType_CONNECTION_FAILURE,
        IngestionErrorType_CURSOR_NOT_ENABLED,
        IngestionErrorType_CUSTOMER_ERROR,
        IngestionErrorType_DATA_SET_DELETED,
        IngestionErrorType_DATA_SET_NOT_SPICE,
        IngestionErrorType_DATA_SET_SIZE_LIMIT_EXCEEDED,
        IngestionErrorType_DATA_SOURCE_AUTH_FAILED,
        IngestionErrorType_DATA_SOURCE_CONNECTION_FAILED,
        IngestionErrorType_DATA_SOURCE_NOT_FOUND,
        IngestionErrorType_DATA_TOLERANCE_EXCEPTION,
        IngestionErrorType_DUPLICATE_COLUMN_NAMES_FOUND,
        IngestionErrorType_ELASTICSEARCH_CURSOR_NOT_ENABLED,
        IngestionErrorType_FAILURE_TO_ASSUME_ROLE,
        IngestionErrorType_FAILURE_TO_PROCESS_JSON_FILE,
        IngestionErrorType_IAM_ROLE_NOT_AVAILABLE,
        IngestionErrorType_INGESTION_CANCELED,
        IngestionErrorType_INGESTION_SUPERSEDED,
        IngestionErrorType_INTERNAL_SERVICE_ERROR,
        IngestionErrorType_INVALID_DATAPREP_SYNTAX,
        IngestionErrorType_INVALID_DATA_SOURCE_CONFIG,
        IngestionErrorType_INVALID_DATE_FORMAT,
        IngestionErrorType_IOT_DATA_SET_FILE_EMPTY,
        IngestionErrorType_IOT_FILE_NOT_FOUND,
        IngestionErrorType_OAUTH_TOKEN_FAILURE,
        IngestionErrorType_PASSWORD_AUTHENTICATION_FAILURE,
        IngestionErrorType_PERMISSION_DENIED,
        IngestionErrorType_PERMISSION_NOT_FOUND,
        IngestionErrorType_QUERY_TIMEOUT,
        IngestionErrorType_REFRESH_SUPPRESSED_BY_EDIT,
        IngestionErrorType_ROW_SIZE_LIMIT_EXCEEDED,
        IngestionErrorType_S3_FILE_INACCESSIBLE,
        IngestionErrorType_S3_MANIFEST_ERROR,
        IngestionErrorType_S3_UPLOADED_FILE_DELETED,
        IngestionErrorType_SOURCE_API_LIMIT_EXCEEDED_FAILURE,
        IngestionErrorType_SOURCE_RESOURCE_LIMIT_EXCEEDED,
        IngestionErrorType_SPICE_TABLE_NOT_FOUND,
        IngestionErrorType_SQL_EXCEPTION,
        IngestionErrorType_SQL_INVALID_PARAMETER_VALUE,
        IngestionErrorType_SQL_NUMERIC_OVERFLOW,
        IngestionErrorType_SQL_SCHEMA_MISMATCH_ERROR,
        IngestionErrorType_SQL_TABLE_NOT_FOUND,
        IngestionErrorType_SSL_CERTIFICATE_VALIDATION_FAILURE,
        IngestionErrorType_UNRESOLVABLE_HOST,
        IngestionErrorType_UNROUTABLE_HOST
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype IngestionErrorType = IngestionErrorType'
  { fromIngestionErrorType ::
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

pattern IngestionErrorType_ACCOUNT_CAPACITY_LIMIT_EXCEEDED :: IngestionErrorType
pattern IngestionErrorType_ACCOUNT_CAPACITY_LIMIT_EXCEEDED = IngestionErrorType' "ACCOUNT_CAPACITY_LIMIT_EXCEEDED"

pattern IngestionErrorType_CONNECTION_FAILURE :: IngestionErrorType
pattern IngestionErrorType_CONNECTION_FAILURE = IngestionErrorType' "CONNECTION_FAILURE"

pattern IngestionErrorType_CURSOR_NOT_ENABLED :: IngestionErrorType
pattern IngestionErrorType_CURSOR_NOT_ENABLED = IngestionErrorType' "CURSOR_NOT_ENABLED"

pattern IngestionErrorType_CUSTOMER_ERROR :: IngestionErrorType
pattern IngestionErrorType_CUSTOMER_ERROR = IngestionErrorType' "CUSTOMER_ERROR"

pattern IngestionErrorType_DATA_SET_DELETED :: IngestionErrorType
pattern IngestionErrorType_DATA_SET_DELETED = IngestionErrorType' "DATA_SET_DELETED"

pattern IngestionErrorType_DATA_SET_NOT_SPICE :: IngestionErrorType
pattern IngestionErrorType_DATA_SET_NOT_SPICE = IngestionErrorType' "DATA_SET_NOT_SPICE"

pattern IngestionErrorType_DATA_SET_SIZE_LIMIT_EXCEEDED :: IngestionErrorType
pattern IngestionErrorType_DATA_SET_SIZE_LIMIT_EXCEEDED = IngestionErrorType' "DATA_SET_SIZE_LIMIT_EXCEEDED"

pattern IngestionErrorType_DATA_SOURCE_AUTH_FAILED :: IngestionErrorType
pattern IngestionErrorType_DATA_SOURCE_AUTH_FAILED = IngestionErrorType' "DATA_SOURCE_AUTH_FAILED"

pattern IngestionErrorType_DATA_SOURCE_CONNECTION_FAILED :: IngestionErrorType
pattern IngestionErrorType_DATA_SOURCE_CONNECTION_FAILED = IngestionErrorType' "DATA_SOURCE_CONNECTION_FAILED"

pattern IngestionErrorType_DATA_SOURCE_NOT_FOUND :: IngestionErrorType
pattern IngestionErrorType_DATA_SOURCE_NOT_FOUND = IngestionErrorType' "DATA_SOURCE_NOT_FOUND"

pattern IngestionErrorType_DATA_TOLERANCE_EXCEPTION :: IngestionErrorType
pattern IngestionErrorType_DATA_TOLERANCE_EXCEPTION = IngestionErrorType' "DATA_TOLERANCE_EXCEPTION"

pattern IngestionErrorType_DUPLICATE_COLUMN_NAMES_FOUND :: IngestionErrorType
pattern IngestionErrorType_DUPLICATE_COLUMN_NAMES_FOUND = IngestionErrorType' "DUPLICATE_COLUMN_NAMES_FOUND"

pattern IngestionErrorType_ELASTICSEARCH_CURSOR_NOT_ENABLED :: IngestionErrorType
pattern IngestionErrorType_ELASTICSEARCH_CURSOR_NOT_ENABLED = IngestionErrorType' "ELASTICSEARCH_CURSOR_NOT_ENABLED"

pattern IngestionErrorType_FAILURE_TO_ASSUME_ROLE :: IngestionErrorType
pattern IngestionErrorType_FAILURE_TO_ASSUME_ROLE = IngestionErrorType' "FAILURE_TO_ASSUME_ROLE"

pattern IngestionErrorType_FAILURE_TO_PROCESS_JSON_FILE :: IngestionErrorType
pattern IngestionErrorType_FAILURE_TO_PROCESS_JSON_FILE = IngestionErrorType' "FAILURE_TO_PROCESS_JSON_FILE"

pattern IngestionErrorType_IAM_ROLE_NOT_AVAILABLE :: IngestionErrorType
pattern IngestionErrorType_IAM_ROLE_NOT_AVAILABLE = IngestionErrorType' "IAM_ROLE_NOT_AVAILABLE"

pattern IngestionErrorType_INGESTION_CANCELED :: IngestionErrorType
pattern IngestionErrorType_INGESTION_CANCELED = IngestionErrorType' "INGESTION_CANCELED"

pattern IngestionErrorType_INGESTION_SUPERSEDED :: IngestionErrorType
pattern IngestionErrorType_INGESTION_SUPERSEDED = IngestionErrorType' "INGESTION_SUPERSEDED"

pattern IngestionErrorType_INTERNAL_SERVICE_ERROR :: IngestionErrorType
pattern IngestionErrorType_INTERNAL_SERVICE_ERROR = IngestionErrorType' "INTERNAL_SERVICE_ERROR"

pattern IngestionErrorType_INVALID_DATAPREP_SYNTAX :: IngestionErrorType
pattern IngestionErrorType_INVALID_DATAPREP_SYNTAX = IngestionErrorType' "INVALID_DATAPREP_SYNTAX"

pattern IngestionErrorType_INVALID_DATA_SOURCE_CONFIG :: IngestionErrorType
pattern IngestionErrorType_INVALID_DATA_SOURCE_CONFIG = IngestionErrorType' "INVALID_DATA_SOURCE_CONFIG"

pattern IngestionErrorType_INVALID_DATE_FORMAT :: IngestionErrorType
pattern IngestionErrorType_INVALID_DATE_FORMAT = IngestionErrorType' "INVALID_DATE_FORMAT"

pattern IngestionErrorType_IOT_DATA_SET_FILE_EMPTY :: IngestionErrorType
pattern IngestionErrorType_IOT_DATA_SET_FILE_EMPTY = IngestionErrorType' "IOT_DATA_SET_FILE_EMPTY"

pattern IngestionErrorType_IOT_FILE_NOT_FOUND :: IngestionErrorType
pattern IngestionErrorType_IOT_FILE_NOT_FOUND = IngestionErrorType' "IOT_FILE_NOT_FOUND"

pattern IngestionErrorType_OAUTH_TOKEN_FAILURE :: IngestionErrorType
pattern IngestionErrorType_OAUTH_TOKEN_FAILURE = IngestionErrorType' "OAUTH_TOKEN_FAILURE"

pattern IngestionErrorType_PASSWORD_AUTHENTICATION_FAILURE :: IngestionErrorType
pattern IngestionErrorType_PASSWORD_AUTHENTICATION_FAILURE = IngestionErrorType' "PASSWORD_AUTHENTICATION_FAILURE"

pattern IngestionErrorType_PERMISSION_DENIED :: IngestionErrorType
pattern IngestionErrorType_PERMISSION_DENIED = IngestionErrorType' "PERMISSION_DENIED"

pattern IngestionErrorType_PERMISSION_NOT_FOUND :: IngestionErrorType
pattern IngestionErrorType_PERMISSION_NOT_FOUND = IngestionErrorType' "PERMISSION_NOT_FOUND"

pattern IngestionErrorType_QUERY_TIMEOUT :: IngestionErrorType
pattern IngestionErrorType_QUERY_TIMEOUT = IngestionErrorType' "QUERY_TIMEOUT"

pattern IngestionErrorType_REFRESH_SUPPRESSED_BY_EDIT :: IngestionErrorType
pattern IngestionErrorType_REFRESH_SUPPRESSED_BY_EDIT = IngestionErrorType' "REFRESH_SUPPRESSED_BY_EDIT"

pattern IngestionErrorType_ROW_SIZE_LIMIT_EXCEEDED :: IngestionErrorType
pattern IngestionErrorType_ROW_SIZE_LIMIT_EXCEEDED = IngestionErrorType' "ROW_SIZE_LIMIT_EXCEEDED"

pattern IngestionErrorType_S3_FILE_INACCESSIBLE :: IngestionErrorType
pattern IngestionErrorType_S3_FILE_INACCESSIBLE = IngestionErrorType' "S3_FILE_INACCESSIBLE"

pattern IngestionErrorType_S3_MANIFEST_ERROR :: IngestionErrorType
pattern IngestionErrorType_S3_MANIFEST_ERROR = IngestionErrorType' "S3_MANIFEST_ERROR"

pattern IngestionErrorType_S3_UPLOADED_FILE_DELETED :: IngestionErrorType
pattern IngestionErrorType_S3_UPLOADED_FILE_DELETED = IngestionErrorType' "S3_UPLOADED_FILE_DELETED"

pattern IngestionErrorType_SOURCE_API_LIMIT_EXCEEDED_FAILURE :: IngestionErrorType
pattern IngestionErrorType_SOURCE_API_LIMIT_EXCEEDED_FAILURE = IngestionErrorType' "SOURCE_API_LIMIT_EXCEEDED_FAILURE"

pattern IngestionErrorType_SOURCE_RESOURCE_LIMIT_EXCEEDED :: IngestionErrorType
pattern IngestionErrorType_SOURCE_RESOURCE_LIMIT_EXCEEDED = IngestionErrorType' "SOURCE_RESOURCE_LIMIT_EXCEEDED"

pattern IngestionErrorType_SPICE_TABLE_NOT_FOUND :: IngestionErrorType
pattern IngestionErrorType_SPICE_TABLE_NOT_FOUND = IngestionErrorType' "SPICE_TABLE_NOT_FOUND"

pattern IngestionErrorType_SQL_EXCEPTION :: IngestionErrorType
pattern IngestionErrorType_SQL_EXCEPTION = IngestionErrorType' "SQL_EXCEPTION"

pattern IngestionErrorType_SQL_INVALID_PARAMETER_VALUE :: IngestionErrorType
pattern IngestionErrorType_SQL_INVALID_PARAMETER_VALUE = IngestionErrorType' "SQL_INVALID_PARAMETER_VALUE"

pattern IngestionErrorType_SQL_NUMERIC_OVERFLOW :: IngestionErrorType
pattern IngestionErrorType_SQL_NUMERIC_OVERFLOW = IngestionErrorType' "SQL_NUMERIC_OVERFLOW"

pattern IngestionErrorType_SQL_SCHEMA_MISMATCH_ERROR :: IngestionErrorType
pattern IngestionErrorType_SQL_SCHEMA_MISMATCH_ERROR = IngestionErrorType' "SQL_SCHEMA_MISMATCH_ERROR"

pattern IngestionErrorType_SQL_TABLE_NOT_FOUND :: IngestionErrorType
pattern IngestionErrorType_SQL_TABLE_NOT_FOUND = IngestionErrorType' "SQL_TABLE_NOT_FOUND"

pattern IngestionErrorType_SSL_CERTIFICATE_VALIDATION_FAILURE :: IngestionErrorType
pattern IngestionErrorType_SSL_CERTIFICATE_VALIDATION_FAILURE = IngestionErrorType' "SSL_CERTIFICATE_VALIDATION_FAILURE"

pattern IngestionErrorType_UNRESOLVABLE_HOST :: IngestionErrorType
pattern IngestionErrorType_UNRESOLVABLE_HOST = IngestionErrorType' "UNRESOLVABLE_HOST"

pattern IngestionErrorType_UNROUTABLE_HOST :: IngestionErrorType
pattern IngestionErrorType_UNROUTABLE_HOST = IngestionErrorType' "UNROUTABLE_HOST"

{-# COMPLETE
  IngestionErrorType_ACCOUNT_CAPACITY_LIMIT_EXCEEDED,
  IngestionErrorType_CONNECTION_FAILURE,
  IngestionErrorType_CURSOR_NOT_ENABLED,
  IngestionErrorType_CUSTOMER_ERROR,
  IngestionErrorType_DATA_SET_DELETED,
  IngestionErrorType_DATA_SET_NOT_SPICE,
  IngestionErrorType_DATA_SET_SIZE_LIMIT_EXCEEDED,
  IngestionErrorType_DATA_SOURCE_AUTH_FAILED,
  IngestionErrorType_DATA_SOURCE_CONNECTION_FAILED,
  IngestionErrorType_DATA_SOURCE_NOT_FOUND,
  IngestionErrorType_DATA_TOLERANCE_EXCEPTION,
  IngestionErrorType_DUPLICATE_COLUMN_NAMES_FOUND,
  IngestionErrorType_ELASTICSEARCH_CURSOR_NOT_ENABLED,
  IngestionErrorType_FAILURE_TO_ASSUME_ROLE,
  IngestionErrorType_FAILURE_TO_PROCESS_JSON_FILE,
  IngestionErrorType_IAM_ROLE_NOT_AVAILABLE,
  IngestionErrorType_INGESTION_CANCELED,
  IngestionErrorType_INGESTION_SUPERSEDED,
  IngestionErrorType_INTERNAL_SERVICE_ERROR,
  IngestionErrorType_INVALID_DATAPREP_SYNTAX,
  IngestionErrorType_INVALID_DATA_SOURCE_CONFIG,
  IngestionErrorType_INVALID_DATE_FORMAT,
  IngestionErrorType_IOT_DATA_SET_FILE_EMPTY,
  IngestionErrorType_IOT_FILE_NOT_FOUND,
  IngestionErrorType_OAUTH_TOKEN_FAILURE,
  IngestionErrorType_PASSWORD_AUTHENTICATION_FAILURE,
  IngestionErrorType_PERMISSION_DENIED,
  IngestionErrorType_PERMISSION_NOT_FOUND,
  IngestionErrorType_QUERY_TIMEOUT,
  IngestionErrorType_REFRESH_SUPPRESSED_BY_EDIT,
  IngestionErrorType_ROW_SIZE_LIMIT_EXCEEDED,
  IngestionErrorType_S3_FILE_INACCESSIBLE,
  IngestionErrorType_S3_MANIFEST_ERROR,
  IngestionErrorType_S3_UPLOADED_FILE_DELETED,
  IngestionErrorType_SOURCE_API_LIMIT_EXCEEDED_FAILURE,
  IngestionErrorType_SOURCE_RESOURCE_LIMIT_EXCEEDED,
  IngestionErrorType_SPICE_TABLE_NOT_FOUND,
  IngestionErrorType_SQL_EXCEPTION,
  IngestionErrorType_SQL_INVALID_PARAMETER_VALUE,
  IngestionErrorType_SQL_NUMERIC_OVERFLOW,
  IngestionErrorType_SQL_SCHEMA_MISMATCH_ERROR,
  IngestionErrorType_SQL_TABLE_NOT_FOUND,
  IngestionErrorType_SSL_CERTIFICATE_VALIDATION_FAILURE,
  IngestionErrorType_UNRESOLVABLE_HOST,
  IngestionErrorType_UNROUTABLE_HOST,
  IngestionErrorType'
  #-}
