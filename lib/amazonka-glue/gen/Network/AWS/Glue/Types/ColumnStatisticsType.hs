{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.ColumnStatisticsType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.ColumnStatisticsType
  ( ColumnStatisticsType
      ( ColumnStatisticsType',
        ColumnStatisticsTypeBoolean,
        ColumnStatisticsTypeDate,
        ColumnStatisticsTypeDecimal,
        ColumnStatisticsTypeDouble,
        ColumnStatisticsTypeLong,
        ColumnStatisticsTypeString,
        ColumnStatisticsTypeBinary,
        fromColumnStatisticsType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ColumnStatisticsType = ColumnStatisticsType'
  { fromColumnStatisticsType ::
      Core.Text
  }
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

pattern ColumnStatisticsTypeBoolean :: ColumnStatisticsType
pattern ColumnStatisticsTypeBoolean = ColumnStatisticsType' "BOOLEAN"

pattern ColumnStatisticsTypeDate :: ColumnStatisticsType
pattern ColumnStatisticsTypeDate = ColumnStatisticsType' "DATE"

pattern ColumnStatisticsTypeDecimal :: ColumnStatisticsType
pattern ColumnStatisticsTypeDecimal = ColumnStatisticsType' "DECIMAL"

pattern ColumnStatisticsTypeDouble :: ColumnStatisticsType
pattern ColumnStatisticsTypeDouble = ColumnStatisticsType' "DOUBLE"

pattern ColumnStatisticsTypeLong :: ColumnStatisticsType
pattern ColumnStatisticsTypeLong = ColumnStatisticsType' "LONG"

pattern ColumnStatisticsTypeString :: ColumnStatisticsType
pattern ColumnStatisticsTypeString = ColumnStatisticsType' "STRING"

pattern ColumnStatisticsTypeBinary :: ColumnStatisticsType
pattern ColumnStatisticsTypeBinary = ColumnStatisticsType' "BINARY"

{-# COMPLETE
  ColumnStatisticsTypeBoolean,
  ColumnStatisticsTypeDate,
  ColumnStatisticsTypeDecimal,
  ColumnStatisticsTypeDouble,
  ColumnStatisticsTypeLong,
  ColumnStatisticsTypeString,
  ColumnStatisticsTypeBinary,
  ColumnStatisticsType'
  #-}
