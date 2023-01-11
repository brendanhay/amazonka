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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype Type = Type' {fromType :: Data.Text}
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
