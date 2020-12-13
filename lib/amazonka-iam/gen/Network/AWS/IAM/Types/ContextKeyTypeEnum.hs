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
        String,
        StringList,
        Numeric,
        NumericList,
        Boolean,
        BooleanList,
        IP,
        IPList,
        Binary,
        BinaryList,
        Date,
        DateList
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ContextKeyTypeEnum = ContextKeyTypeEnum' Lude.Text
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

pattern String :: ContextKeyTypeEnum
pattern String = ContextKeyTypeEnum' "string"

pattern StringList :: ContextKeyTypeEnum
pattern StringList = ContextKeyTypeEnum' "stringList"

pattern Numeric :: ContextKeyTypeEnum
pattern Numeric = ContextKeyTypeEnum' "numeric"

pattern NumericList :: ContextKeyTypeEnum
pattern NumericList = ContextKeyTypeEnum' "numericList"

pattern Boolean :: ContextKeyTypeEnum
pattern Boolean = ContextKeyTypeEnum' "boolean"

pattern BooleanList :: ContextKeyTypeEnum
pattern BooleanList = ContextKeyTypeEnum' "booleanList"

pattern IP :: ContextKeyTypeEnum
pattern IP = ContextKeyTypeEnum' "ip"

pattern IPList :: ContextKeyTypeEnum
pattern IPList = ContextKeyTypeEnum' "ipList"

pattern Binary :: ContextKeyTypeEnum
pattern Binary = ContextKeyTypeEnum' "binary"

pattern BinaryList :: ContextKeyTypeEnum
pattern BinaryList = ContextKeyTypeEnum' "binaryList"

pattern Date :: ContextKeyTypeEnum
pattern Date = ContextKeyTypeEnum' "date"

pattern DateList :: ContextKeyTypeEnum
pattern DateList = ContextKeyTypeEnum' "dateList"

{-# COMPLETE
  String,
  StringList,
  Numeric,
  NumericList,
  Boolean,
  BooleanList,
  IP,
  IPList,
  Binary,
  BinaryList,
  Date,
  DateList,
  ContextKeyTypeEnum'
  #-}
