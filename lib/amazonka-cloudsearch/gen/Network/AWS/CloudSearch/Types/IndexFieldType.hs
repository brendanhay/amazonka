-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.IndexFieldType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.IndexFieldType
  ( IndexFieldType
      ( IndexFieldType',
        Date,
        DateArray,
        Double,
        DoubleArray,
        Int,
        IntArray,
        Latlon,
        Literal,
        LiteralArray,
        Text,
        TextArray
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | The type of field. The valid options for a field depend on the field type. For more information about the supported field types, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-index-fields.html Configuring Index Fields> in the /Amazon CloudSearch Developer Guide/ .
newtype IndexFieldType = IndexFieldType' Lude.Text
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

pattern Date :: IndexFieldType
pattern Date = IndexFieldType' "date"

pattern DateArray :: IndexFieldType
pattern DateArray = IndexFieldType' "date-array"

pattern Double :: IndexFieldType
pattern Double = IndexFieldType' "double"

pattern DoubleArray :: IndexFieldType
pattern DoubleArray = IndexFieldType' "double-array"

pattern Int :: IndexFieldType
pattern Int = IndexFieldType' "int"

pattern IntArray :: IndexFieldType
pattern IntArray = IndexFieldType' "int-array"

pattern Latlon :: IndexFieldType
pattern Latlon = IndexFieldType' "latlon"

pattern Literal :: IndexFieldType
pattern Literal = IndexFieldType' "literal"

pattern LiteralArray :: IndexFieldType
pattern LiteralArray = IndexFieldType' "literal-array"

pattern Text :: IndexFieldType
pattern Text = IndexFieldType' "text"

pattern TextArray :: IndexFieldType
pattern TextArray = IndexFieldType' "text-array"

{-# COMPLETE
  Date,
  DateArray,
  Double,
  DoubleArray,
  Int,
  IntArray,
  Latlon,
  Literal,
  LiteralArray,
  Text,
  TextArray,
  IndexFieldType'
  #-}
