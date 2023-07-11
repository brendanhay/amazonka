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
-- Module      : Amazonka.Glue.Types.ColumnStatisticsType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.ColumnStatisticsType
  ( ColumnStatisticsType
      ( ..,
        ColumnStatisticsType_BINARY,
        ColumnStatisticsType_BOOLEAN,
        ColumnStatisticsType_DATE,
        ColumnStatisticsType_DECIMAL,
        ColumnStatisticsType_DOUBLE,
        ColumnStatisticsType_LONG,
        ColumnStatisticsType_STRING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ColumnStatisticsType = ColumnStatisticsType'
  { fromColumnStatisticsType ::
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

pattern ColumnStatisticsType_BINARY :: ColumnStatisticsType
pattern ColumnStatisticsType_BINARY = ColumnStatisticsType' "BINARY"

pattern ColumnStatisticsType_BOOLEAN :: ColumnStatisticsType
pattern ColumnStatisticsType_BOOLEAN = ColumnStatisticsType' "BOOLEAN"

pattern ColumnStatisticsType_DATE :: ColumnStatisticsType
pattern ColumnStatisticsType_DATE = ColumnStatisticsType' "DATE"

pattern ColumnStatisticsType_DECIMAL :: ColumnStatisticsType
pattern ColumnStatisticsType_DECIMAL = ColumnStatisticsType' "DECIMAL"

pattern ColumnStatisticsType_DOUBLE :: ColumnStatisticsType
pattern ColumnStatisticsType_DOUBLE = ColumnStatisticsType' "DOUBLE"

pattern ColumnStatisticsType_LONG :: ColumnStatisticsType
pattern ColumnStatisticsType_LONG = ColumnStatisticsType' "LONG"

pattern ColumnStatisticsType_STRING :: ColumnStatisticsType
pattern ColumnStatisticsType_STRING = ColumnStatisticsType' "STRING"

{-# COMPLETE
  ColumnStatisticsType_BINARY,
  ColumnStatisticsType_BOOLEAN,
  ColumnStatisticsType_DATE,
  ColumnStatisticsType_DECIMAL,
  ColumnStatisticsType_DOUBLE,
  ColumnStatisticsType_LONG,
  ColumnStatisticsType_STRING,
  ColumnStatisticsType'
  #-}
