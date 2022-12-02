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
-- Module      : Amazonka.IAM.Types.ContextKeyTypeEnum
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IAM.Types.ContextKeyTypeEnum
  ( ContextKeyTypeEnum
      ( ..,
        ContextKeyTypeEnum_Binary,
        ContextKeyTypeEnum_BinaryList,
        ContextKeyTypeEnum_Boolean,
        ContextKeyTypeEnum_BooleanList,
        ContextKeyTypeEnum_Date,
        ContextKeyTypeEnum_DateList,
        ContextKeyTypeEnum_Ip,
        ContextKeyTypeEnum_IpList,
        ContextKeyTypeEnum_Numeric,
        ContextKeyTypeEnum_NumericList,
        ContextKeyTypeEnum_String,
        ContextKeyTypeEnum_StringList
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ContextKeyTypeEnum = ContextKeyTypeEnum'
  { fromContextKeyTypeEnum ::
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

pattern ContextKeyTypeEnum_Binary :: ContextKeyTypeEnum
pattern ContextKeyTypeEnum_Binary = ContextKeyTypeEnum' "binary"

pattern ContextKeyTypeEnum_BinaryList :: ContextKeyTypeEnum
pattern ContextKeyTypeEnum_BinaryList = ContextKeyTypeEnum' "binaryList"

pattern ContextKeyTypeEnum_Boolean :: ContextKeyTypeEnum
pattern ContextKeyTypeEnum_Boolean = ContextKeyTypeEnum' "boolean"

pattern ContextKeyTypeEnum_BooleanList :: ContextKeyTypeEnum
pattern ContextKeyTypeEnum_BooleanList = ContextKeyTypeEnum' "booleanList"

pattern ContextKeyTypeEnum_Date :: ContextKeyTypeEnum
pattern ContextKeyTypeEnum_Date = ContextKeyTypeEnum' "date"

pattern ContextKeyTypeEnum_DateList :: ContextKeyTypeEnum
pattern ContextKeyTypeEnum_DateList = ContextKeyTypeEnum' "dateList"

pattern ContextKeyTypeEnum_Ip :: ContextKeyTypeEnum
pattern ContextKeyTypeEnum_Ip = ContextKeyTypeEnum' "ip"

pattern ContextKeyTypeEnum_IpList :: ContextKeyTypeEnum
pattern ContextKeyTypeEnum_IpList = ContextKeyTypeEnum' "ipList"

pattern ContextKeyTypeEnum_Numeric :: ContextKeyTypeEnum
pattern ContextKeyTypeEnum_Numeric = ContextKeyTypeEnum' "numeric"

pattern ContextKeyTypeEnum_NumericList :: ContextKeyTypeEnum
pattern ContextKeyTypeEnum_NumericList = ContextKeyTypeEnum' "numericList"

pattern ContextKeyTypeEnum_String :: ContextKeyTypeEnum
pattern ContextKeyTypeEnum_String = ContextKeyTypeEnum' "string"

pattern ContextKeyTypeEnum_StringList :: ContextKeyTypeEnum
pattern ContextKeyTypeEnum_StringList = ContextKeyTypeEnum' "stringList"

{-# COMPLETE
  ContextKeyTypeEnum_Binary,
  ContextKeyTypeEnum_BinaryList,
  ContextKeyTypeEnum_Boolean,
  ContextKeyTypeEnum_BooleanList,
  ContextKeyTypeEnum_Date,
  ContextKeyTypeEnum_DateList,
  ContextKeyTypeEnum_Ip,
  ContextKeyTypeEnum_IpList,
  ContextKeyTypeEnum_Numeric,
  ContextKeyTypeEnum_NumericList,
  ContextKeyTypeEnum_String,
  ContextKeyTypeEnum_StringList,
  ContextKeyTypeEnum'
  #-}
