{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        IndexFieldTypeInt,
        IndexFieldTypeDouble,
        IndexFieldTypeLiteral,
        IndexFieldTypeText,
        IndexFieldTypeDate,
        IndexFieldTypeLatlon,
        IndexFieldTypeIntArray,
        IndexFieldTypeDoubleArray,
        IndexFieldTypeLiteralArray,
        IndexFieldTypeTextArray,
        IndexFieldTypeDateArray,
        fromIndexFieldType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | The type of field. The valid options for a field depend on the field type. For more information about the supported field types, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-index-fields.html Configuring Index Fields> in the /Amazon CloudSearch Developer Guide/ .
newtype IndexFieldType = IndexFieldType'
  { fromIndexFieldType ::
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

pattern IndexFieldTypeInt :: IndexFieldType
pattern IndexFieldTypeInt = IndexFieldType' "int"

pattern IndexFieldTypeDouble :: IndexFieldType
pattern IndexFieldTypeDouble = IndexFieldType' "double"

pattern IndexFieldTypeLiteral :: IndexFieldType
pattern IndexFieldTypeLiteral = IndexFieldType' "literal"

pattern IndexFieldTypeText :: IndexFieldType
pattern IndexFieldTypeText = IndexFieldType' "text"

pattern IndexFieldTypeDate :: IndexFieldType
pattern IndexFieldTypeDate = IndexFieldType' "date"

pattern IndexFieldTypeLatlon :: IndexFieldType
pattern IndexFieldTypeLatlon = IndexFieldType' "latlon"

pattern IndexFieldTypeIntArray :: IndexFieldType
pattern IndexFieldTypeIntArray = IndexFieldType' "int-array"

pattern IndexFieldTypeDoubleArray :: IndexFieldType
pattern IndexFieldTypeDoubleArray = IndexFieldType' "double-array"

pattern IndexFieldTypeLiteralArray :: IndexFieldType
pattern IndexFieldTypeLiteralArray = IndexFieldType' "literal-array"

pattern IndexFieldTypeTextArray :: IndexFieldType
pattern IndexFieldTypeTextArray = IndexFieldType' "text-array"

pattern IndexFieldTypeDateArray :: IndexFieldType
pattern IndexFieldTypeDateArray = IndexFieldType' "date-array"

{-# COMPLETE
  IndexFieldTypeInt,
  IndexFieldTypeDouble,
  IndexFieldTypeLiteral,
  IndexFieldTypeText,
  IndexFieldTypeDate,
  IndexFieldTypeLatlon,
  IndexFieldTypeIntArray,
  IndexFieldTypeDoubleArray,
  IndexFieldTypeLiteralArray,
  IndexFieldTypeTextArray,
  IndexFieldTypeDateArray,
  IndexFieldType'
  #-}
