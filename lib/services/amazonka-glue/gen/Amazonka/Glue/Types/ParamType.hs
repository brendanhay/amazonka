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
-- Module      : Amazonka.Glue.Types.ParamType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.ParamType
  ( ParamType
      ( ..,
        ParamType_Bool,
        ParamType_Complex,
        ParamType_Float,
        ParamType_Int,
        ParamType_List,
        ParamType_Null,
        ParamType_Str
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ParamType = ParamType'
  { fromParamType ::
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

pattern ParamType_Bool :: ParamType
pattern ParamType_Bool = ParamType' "bool"

pattern ParamType_Complex :: ParamType
pattern ParamType_Complex = ParamType' "complex"

pattern ParamType_Float :: ParamType
pattern ParamType_Float = ParamType' "float"

pattern ParamType_Int :: ParamType
pattern ParamType_Int = ParamType' "int"

pattern ParamType_List :: ParamType
pattern ParamType_List = ParamType' "list"

pattern ParamType_Null :: ParamType
pattern ParamType_Null = ParamType' "null"

pattern ParamType_Str :: ParamType
pattern ParamType_Str = ParamType' "str"

{-# COMPLETE
  ParamType_Bool,
  ParamType_Complex,
  ParamType_Float,
  ParamType_Int,
  ParamType_List,
  ParamType_Null,
  ParamType_Str,
  ParamType'
  #-}
