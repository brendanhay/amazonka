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
-- Module      : Amazonka.Glue.Types.JDBCDataType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.JDBCDataType
  ( JDBCDataType
      ( ..,
        JDBCDataType_ARRAY,
        JDBCDataType_BIGINT,
        JDBCDataType_BINARY,
        JDBCDataType_BIT,
        JDBCDataType_BLOB,
        JDBCDataType_BOOLEAN,
        JDBCDataType_CHAR,
        JDBCDataType_CLOB,
        JDBCDataType_DATALINK,
        JDBCDataType_DATE,
        JDBCDataType_DECIMAL,
        JDBCDataType_DISTINCT,
        JDBCDataType_DOUBLE,
        JDBCDataType_FLOAT,
        JDBCDataType_INTEGER,
        JDBCDataType_JAVA_OBJECT,
        JDBCDataType_LONGNVARCHAR,
        JDBCDataType_LONGVARBINARY,
        JDBCDataType_LONGVARCHAR,
        JDBCDataType_NCHAR,
        JDBCDataType_NCLOB,
        JDBCDataType_NULL,
        JDBCDataType_NUMERIC,
        JDBCDataType_NVARCHAR,
        JDBCDataType_OTHER,
        JDBCDataType_REAL,
        JDBCDataType_REF,
        JDBCDataType_REF_CURSOR,
        JDBCDataType_ROWID,
        JDBCDataType_SMALLINT,
        JDBCDataType_SQLXML,
        JDBCDataType_STRUCT,
        JDBCDataType_TIME,
        JDBCDataType_TIMESTAMP,
        JDBCDataType_TIMESTAMP_WITH_TIMEZONE,
        JDBCDataType_TIME_WITH_TIMEZONE,
        JDBCDataType_TINYINT,
        JDBCDataType_VARBINARY,
        JDBCDataType_VARCHAR
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype JDBCDataType = JDBCDataType'
  { fromJDBCDataType ::
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

pattern JDBCDataType_ARRAY :: JDBCDataType
pattern JDBCDataType_ARRAY = JDBCDataType' "ARRAY"

pattern JDBCDataType_BIGINT :: JDBCDataType
pattern JDBCDataType_BIGINT = JDBCDataType' "BIGINT"

pattern JDBCDataType_BINARY :: JDBCDataType
pattern JDBCDataType_BINARY = JDBCDataType' "BINARY"

pattern JDBCDataType_BIT :: JDBCDataType
pattern JDBCDataType_BIT = JDBCDataType' "BIT"

pattern JDBCDataType_BLOB :: JDBCDataType
pattern JDBCDataType_BLOB = JDBCDataType' "BLOB"

pattern JDBCDataType_BOOLEAN :: JDBCDataType
pattern JDBCDataType_BOOLEAN = JDBCDataType' "BOOLEAN"

pattern JDBCDataType_CHAR :: JDBCDataType
pattern JDBCDataType_CHAR = JDBCDataType' "CHAR"

pattern JDBCDataType_CLOB :: JDBCDataType
pattern JDBCDataType_CLOB = JDBCDataType' "CLOB"

pattern JDBCDataType_DATALINK :: JDBCDataType
pattern JDBCDataType_DATALINK = JDBCDataType' "DATALINK"

pattern JDBCDataType_DATE :: JDBCDataType
pattern JDBCDataType_DATE = JDBCDataType' "DATE"

pattern JDBCDataType_DECIMAL :: JDBCDataType
pattern JDBCDataType_DECIMAL = JDBCDataType' "DECIMAL"

pattern JDBCDataType_DISTINCT :: JDBCDataType
pattern JDBCDataType_DISTINCT = JDBCDataType' "DISTINCT"

pattern JDBCDataType_DOUBLE :: JDBCDataType
pattern JDBCDataType_DOUBLE = JDBCDataType' "DOUBLE"

pattern JDBCDataType_FLOAT :: JDBCDataType
pattern JDBCDataType_FLOAT = JDBCDataType' "FLOAT"

pattern JDBCDataType_INTEGER :: JDBCDataType
pattern JDBCDataType_INTEGER = JDBCDataType' "INTEGER"

pattern JDBCDataType_JAVA_OBJECT :: JDBCDataType
pattern JDBCDataType_JAVA_OBJECT = JDBCDataType' "JAVA_OBJECT"

pattern JDBCDataType_LONGNVARCHAR :: JDBCDataType
pattern JDBCDataType_LONGNVARCHAR = JDBCDataType' "LONGNVARCHAR"

pattern JDBCDataType_LONGVARBINARY :: JDBCDataType
pattern JDBCDataType_LONGVARBINARY = JDBCDataType' "LONGVARBINARY"

pattern JDBCDataType_LONGVARCHAR :: JDBCDataType
pattern JDBCDataType_LONGVARCHAR = JDBCDataType' "LONGVARCHAR"

pattern JDBCDataType_NCHAR :: JDBCDataType
pattern JDBCDataType_NCHAR = JDBCDataType' "NCHAR"

pattern JDBCDataType_NCLOB :: JDBCDataType
pattern JDBCDataType_NCLOB = JDBCDataType' "NCLOB"

pattern JDBCDataType_NULL :: JDBCDataType
pattern JDBCDataType_NULL = JDBCDataType' "NULL"

pattern JDBCDataType_NUMERIC :: JDBCDataType
pattern JDBCDataType_NUMERIC = JDBCDataType' "NUMERIC"

pattern JDBCDataType_NVARCHAR :: JDBCDataType
pattern JDBCDataType_NVARCHAR = JDBCDataType' "NVARCHAR"

pattern JDBCDataType_OTHER :: JDBCDataType
pattern JDBCDataType_OTHER = JDBCDataType' "OTHER"

pattern JDBCDataType_REAL :: JDBCDataType
pattern JDBCDataType_REAL = JDBCDataType' "REAL"

pattern JDBCDataType_REF :: JDBCDataType
pattern JDBCDataType_REF = JDBCDataType' "REF"

pattern JDBCDataType_REF_CURSOR :: JDBCDataType
pattern JDBCDataType_REF_CURSOR = JDBCDataType' "REF_CURSOR"

pattern JDBCDataType_ROWID :: JDBCDataType
pattern JDBCDataType_ROWID = JDBCDataType' "ROWID"

pattern JDBCDataType_SMALLINT :: JDBCDataType
pattern JDBCDataType_SMALLINT = JDBCDataType' "SMALLINT"

pattern JDBCDataType_SQLXML :: JDBCDataType
pattern JDBCDataType_SQLXML = JDBCDataType' "SQLXML"

pattern JDBCDataType_STRUCT :: JDBCDataType
pattern JDBCDataType_STRUCT = JDBCDataType' "STRUCT"

pattern JDBCDataType_TIME :: JDBCDataType
pattern JDBCDataType_TIME = JDBCDataType' "TIME"

pattern JDBCDataType_TIMESTAMP :: JDBCDataType
pattern JDBCDataType_TIMESTAMP = JDBCDataType' "TIMESTAMP"

pattern JDBCDataType_TIMESTAMP_WITH_TIMEZONE :: JDBCDataType
pattern JDBCDataType_TIMESTAMP_WITH_TIMEZONE = JDBCDataType' "TIMESTAMP_WITH_TIMEZONE"

pattern JDBCDataType_TIME_WITH_TIMEZONE :: JDBCDataType
pattern JDBCDataType_TIME_WITH_TIMEZONE = JDBCDataType' "TIME_WITH_TIMEZONE"

pattern JDBCDataType_TINYINT :: JDBCDataType
pattern JDBCDataType_TINYINT = JDBCDataType' "TINYINT"

pattern JDBCDataType_VARBINARY :: JDBCDataType
pattern JDBCDataType_VARBINARY = JDBCDataType' "VARBINARY"

pattern JDBCDataType_VARCHAR :: JDBCDataType
pattern JDBCDataType_VARCHAR = JDBCDataType' "VARCHAR"

{-# COMPLETE
  JDBCDataType_ARRAY,
  JDBCDataType_BIGINT,
  JDBCDataType_BINARY,
  JDBCDataType_BIT,
  JDBCDataType_BLOB,
  JDBCDataType_BOOLEAN,
  JDBCDataType_CHAR,
  JDBCDataType_CLOB,
  JDBCDataType_DATALINK,
  JDBCDataType_DATE,
  JDBCDataType_DECIMAL,
  JDBCDataType_DISTINCT,
  JDBCDataType_DOUBLE,
  JDBCDataType_FLOAT,
  JDBCDataType_INTEGER,
  JDBCDataType_JAVA_OBJECT,
  JDBCDataType_LONGNVARCHAR,
  JDBCDataType_LONGVARBINARY,
  JDBCDataType_LONGVARCHAR,
  JDBCDataType_NCHAR,
  JDBCDataType_NCLOB,
  JDBCDataType_NULL,
  JDBCDataType_NUMERIC,
  JDBCDataType_NVARCHAR,
  JDBCDataType_OTHER,
  JDBCDataType_REAL,
  JDBCDataType_REF,
  JDBCDataType_REF_CURSOR,
  JDBCDataType_ROWID,
  JDBCDataType_SMALLINT,
  JDBCDataType_SQLXML,
  JDBCDataType_STRUCT,
  JDBCDataType_TIME,
  JDBCDataType_TIMESTAMP,
  JDBCDataType_TIMESTAMP_WITH_TIMEZONE,
  JDBCDataType_TIME_WITH_TIMEZONE,
  JDBCDataType_TINYINT,
  JDBCDataType_VARBINARY,
  JDBCDataType_VARCHAR,
  JDBCDataType'
  #-}
