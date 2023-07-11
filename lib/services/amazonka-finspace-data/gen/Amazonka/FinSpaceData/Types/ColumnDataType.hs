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
-- Module      : Amazonka.FinSpaceData.Types.ColumnDataType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FinSpaceData.Types.ColumnDataType
  ( ColumnDataType
      ( ..,
        ColumnDataType_BIGINT,
        ColumnDataType_BINARY,
        ColumnDataType_BOOLEAN,
        ColumnDataType_CHAR,
        ColumnDataType_DATE,
        ColumnDataType_DATETIME,
        ColumnDataType_DOUBLE,
        ColumnDataType_FLOAT,
        ColumnDataType_INTEGER,
        ColumnDataType_SMALLINT,
        ColumnDataType_STRING,
        ColumnDataType_TINYINT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Data type of a column.
newtype ColumnDataType = ColumnDataType'
  { fromColumnDataType ::
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

pattern ColumnDataType_BIGINT :: ColumnDataType
pattern ColumnDataType_BIGINT = ColumnDataType' "BIGINT"

pattern ColumnDataType_BINARY :: ColumnDataType
pattern ColumnDataType_BINARY = ColumnDataType' "BINARY"

pattern ColumnDataType_BOOLEAN :: ColumnDataType
pattern ColumnDataType_BOOLEAN = ColumnDataType' "BOOLEAN"

pattern ColumnDataType_CHAR :: ColumnDataType
pattern ColumnDataType_CHAR = ColumnDataType' "CHAR"

pattern ColumnDataType_DATE :: ColumnDataType
pattern ColumnDataType_DATE = ColumnDataType' "DATE"

pattern ColumnDataType_DATETIME :: ColumnDataType
pattern ColumnDataType_DATETIME = ColumnDataType' "DATETIME"

pattern ColumnDataType_DOUBLE :: ColumnDataType
pattern ColumnDataType_DOUBLE = ColumnDataType' "DOUBLE"

pattern ColumnDataType_FLOAT :: ColumnDataType
pattern ColumnDataType_FLOAT = ColumnDataType' "FLOAT"

pattern ColumnDataType_INTEGER :: ColumnDataType
pattern ColumnDataType_INTEGER = ColumnDataType' "INTEGER"

pattern ColumnDataType_SMALLINT :: ColumnDataType
pattern ColumnDataType_SMALLINT = ColumnDataType' "SMALLINT"

pattern ColumnDataType_STRING :: ColumnDataType
pattern ColumnDataType_STRING = ColumnDataType' "STRING"

pattern ColumnDataType_TINYINT :: ColumnDataType
pattern ColumnDataType_TINYINT = ColumnDataType' "TINYINT"

{-# COMPLETE
  ColumnDataType_BIGINT,
  ColumnDataType_BINARY,
  ColumnDataType_BOOLEAN,
  ColumnDataType_CHAR,
  ColumnDataType_DATE,
  ColumnDataType_DATETIME,
  ColumnDataType_DOUBLE,
  ColumnDataType_FLOAT,
  ColumnDataType_INTEGER,
  ColumnDataType_SMALLINT,
  ColumnDataType_STRING,
  ColumnDataType_TINYINT,
  ColumnDataType'
  #-}
