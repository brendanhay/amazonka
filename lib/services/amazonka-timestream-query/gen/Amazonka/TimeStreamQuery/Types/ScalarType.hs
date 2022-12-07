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
-- Module      : Amazonka.TimeStreamQuery.Types.ScalarType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TimeStreamQuery.Types.ScalarType
  ( ScalarType
      ( ..,
        ScalarType_BIGINT,
        ScalarType_BOOLEAN,
        ScalarType_DATE,
        ScalarType_DOUBLE,
        ScalarType_INTEGER,
        ScalarType_INTERVAL_DAY_TO_SECOND,
        ScalarType_INTERVAL_YEAR_TO_MONTH,
        ScalarType_TIME,
        ScalarType_TIMESTAMP,
        ScalarType_UNKNOWN,
        ScalarType_VARCHAR
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ScalarType = ScalarType'
  { fromScalarType ::
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

pattern ScalarType_BIGINT :: ScalarType
pattern ScalarType_BIGINT = ScalarType' "BIGINT"

pattern ScalarType_BOOLEAN :: ScalarType
pattern ScalarType_BOOLEAN = ScalarType' "BOOLEAN"

pattern ScalarType_DATE :: ScalarType
pattern ScalarType_DATE = ScalarType' "DATE"

pattern ScalarType_DOUBLE :: ScalarType
pattern ScalarType_DOUBLE = ScalarType' "DOUBLE"

pattern ScalarType_INTEGER :: ScalarType
pattern ScalarType_INTEGER = ScalarType' "INTEGER"

pattern ScalarType_INTERVAL_DAY_TO_SECOND :: ScalarType
pattern ScalarType_INTERVAL_DAY_TO_SECOND = ScalarType' "INTERVAL_DAY_TO_SECOND"

pattern ScalarType_INTERVAL_YEAR_TO_MONTH :: ScalarType
pattern ScalarType_INTERVAL_YEAR_TO_MONTH = ScalarType' "INTERVAL_YEAR_TO_MONTH"

pattern ScalarType_TIME :: ScalarType
pattern ScalarType_TIME = ScalarType' "TIME"

pattern ScalarType_TIMESTAMP :: ScalarType
pattern ScalarType_TIMESTAMP = ScalarType' "TIMESTAMP"

pattern ScalarType_UNKNOWN :: ScalarType
pattern ScalarType_UNKNOWN = ScalarType' "UNKNOWN"

pattern ScalarType_VARCHAR :: ScalarType
pattern ScalarType_VARCHAR = ScalarType' "VARCHAR"

{-# COMPLETE
  ScalarType_BIGINT,
  ScalarType_BOOLEAN,
  ScalarType_DATE,
  ScalarType_DOUBLE,
  ScalarType_INTEGER,
  ScalarType_INTERVAL_DAY_TO_SECOND,
  ScalarType_INTERVAL_YEAR_TO_MONTH,
  ScalarType_TIME,
  ScalarType_TIMESTAMP,
  ScalarType_UNKNOWN,
  ScalarType_VARCHAR,
  ScalarType'
  #-}
