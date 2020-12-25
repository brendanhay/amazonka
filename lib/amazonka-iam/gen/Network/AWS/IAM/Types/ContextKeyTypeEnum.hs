{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.ContextKeyTypeEnum
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.ContextKeyTypeEnum
  ( ContextKeyTypeEnum
      ( ContextKeyTypeEnum',
        ContextKeyTypeEnumString,
        ContextKeyTypeEnumStringList,
        ContextKeyTypeEnumNumeric,
        ContextKeyTypeEnumNumericList,
        ContextKeyTypeEnumBoolean,
        ContextKeyTypeEnumBooleanList,
        ContextKeyTypeEnumIP,
        ContextKeyTypeEnumIpList,
        ContextKeyTypeEnumBinary,
        ContextKeyTypeEnumBinaryList,
        ContextKeyTypeEnumDate,
        ContextKeyTypeEnumDateList,
        fromContextKeyTypeEnum
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ContextKeyTypeEnum = ContextKeyTypeEnum'
  { fromContextKeyTypeEnum ::
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

pattern ContextKeyTypeEnumString :: ContextKeyTypeEnum
pattern ContextKeyTypeEnumString = ContextKeyTypeEnum' "string"

pattern ContextKeyTypeEnumStringList :: ContextKeyTypeEnum
pattern ContextKeyTypeEnumStringList = ContextKeyTypeEnum' "stringList"

pattern ContextKeyTypeEnumNumeric :: ContextKeyTypeEnum
pattern ContextKeyTypeEnumNumeric = ContextKeyTypeEnum' "numeric"

pattern ContextKeyTypeEnumNumericList :: ContextKeyTypeEnum
pattern ContextKeyTypeEnumNumericList = ContextKeyTypeEnum' "numericList"

pattern ContextKeyTypeEnumBoolean :: ContextKeyTypeEnum
pattern ContextKeyTypeEnumBoolean = ContextKeyTypeEnum' "boolean"

pattern ContextKeyTypeEnumBooleanList :: ContextKeyTypeEnum
pattern ContextKeyTypeEnumBooleanList = ContextKeyTypeEnum' "booleanList"

pattern ContextKeyTypeEnumIP :: ContextKeyTypeEnum
pattern ContextKeyTypeEnumIP = ContextKeyTypeEnum' "ip"

pattern ContextKeyTypeEnumIpList :: ContextKeyTypeEnum
pattern ContextKeyTypeEnumIpList = ContextKeyTypeEnum' "ipList"

pattern ContextKeyTypeEnumBinary :: ContextKeyTypeEnum
pattern ContextKeyTypeEnumBinary = ContextKeyTypeEnum' "binary"

pattern ContextKeyTypeEnumBinaryList :: ContextKeyTypeEnum
pattern ContextKeyTypeEnumBinaryList = ContextKeyTypeEnum' "binaryList"

pattern ContextKeyTypeEnumDate :: ContextKeyTypeEnum
pattern ContextKeyTypeEnumDate = ContextKeyTypeEnum' "date"

pattern ContextKeyTypeEnumDateList :: ContextKeyTypeEnum
pattern ContextKeyTypeEnumDateList = ContextKeyTypeEnum' "dateList"

{-# COMPLETE
  ContextKeyTypeEnumString,
  ContextKeyTypeEnumStringList,
  ContextKeyTypeEnumNumeric,
  ContextKeyTypeEnumNumericList,
  ContextKeyTypeEnumBoolean,
  ContextKeyTypeEnumBooleanList,
  ContextKeyTypeEnumIP,
  ContextKeyTypeEnumIpList,
  ContextKeyTypeEnumBinary,
  ContextKeyTypeEnumBinaryList,
  ContextKeyTypeEnumDate,
  ContextKeyTypeEnumDateList,
  ContextKeyTypeEnum'
  #-}
