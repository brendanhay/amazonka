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
-- Module      : Network.AWS.Glue.Types.ColumnStatisticsType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.ColumnStatisticsType
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ColumnStatisticsType = ColumnStatisticsType'
  { fromColumnStatisticsType ::
      Core.Text
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
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
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
