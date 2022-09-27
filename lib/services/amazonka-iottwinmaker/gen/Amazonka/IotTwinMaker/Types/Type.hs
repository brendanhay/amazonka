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
-- Module      : Amazonka.IotTwinMaker.Types.Type
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IotTwinMaker.Types.Type
  ( Type
      ( ..,
        Type_BOOLEAN,
        Type_DOUBLE,
        Type_INTEGER,
        Type_LIST,
        Type_LONG,
        Type_MAP,
        Type_RELATIONSHIP,
        Type_STRING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype Type = Type' {fromType :: Core.Text}
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

pattern Type_BOOLEAN :: Type
pattern Type_BOOLEAN = Type' "BOOLEAN"

pattern Type_DOUBLE :: Type
pattern Type_DOUBLE = Type' "DOUBLE"

pattern Type_INTEGER :: Type
pattern Type_INTEGER = Type' "INTEGER"

pattern Type_LIST :: Type
pattern Type_LIST = Type' "LIST"

pattern Type_LONG :: Type
pattern Type_LONG = Type' "LONG"

pattern Type_MAP :: Type
pattern Type_MAP = Type' "MAP"

pattern Type_RELATIONSHIP :: Type
pattern Type_RELATIONSHIP = Type' "RELATIONSHIP"

pattern Type_STRING :: Type
pattern Type_STRING = Type' "STRING"

{-# COMPLETE
  Type_BOOLEAN,
  Type_DOUBLE,
  Type_INTEGER,
  Type_LIST,
  Type_LONG,
  Type_MAP,
  Type_RELATIONSHIP,
  Type_STRING,
  Type'
  #-}
