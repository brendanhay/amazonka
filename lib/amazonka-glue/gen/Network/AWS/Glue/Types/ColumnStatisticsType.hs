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
        Binary,
        Boolean,
        Date,
        Decimal,
        Double,
        Long,
        String
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ColumnStatisticsType = ColumnStatisticsType' Lude.Text
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

pattern Binary :: ColumnStatisticsType
pattern Binary = ColumnStatisticsType' "BINARY"

pattern Boolean :: ColumnStatisticsType
pattern Boolean = ColumnStatisticsType' "BOOLEAN"

pattern Date :: ColumnStatisticsType
pattern Date = ColumnStatisticsType' "DATE"

pattern Decimal :: ColumnStatisticsType
pattern Decimal = ColumnStatisticsType' "DECIMAL"

pattern Double :: ColumnStatisticsType
pattern Double = ColumnStatisticsType' "DOUBLE"

pattern Long :: ColumnStatisticsType
pattern Long = ColumnStatisticsType' "LONG"

pattern String :: ColumnStatisticsType
pattern String = ColumnStatisticsType' "STRING"

{-# COMPLETE
  Binary,
  Boolean,
  Date,
  Decimal,
  Double,
  Long,
  String,
  ColumnStatisticsType'
  #-}
