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
-- Module      : Amazonka.Omics.Types.SchemaValueType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.SchemaValueType
  ( SchemaValueType
      ( ..,
        SchemaValueType_BOOLEAN,
        SchemaValueType_DOUBLE,
        SchemaValueType_FLOAT,
        SchemaValueType_INT,
        SchemaValueType_LONG,
        SchemaValueType_STRING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SchemaValueType = SchemaValueType'
  { fromSchemaValueType ::
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

pattern SchemaValueType_BOOLEAN :: SchemaValueType
pattern SchemaValueType_BOOLEAN = SchemaValueType' "BOOLEAN"

pattern SchemaValueType_DOUBLE :: SchemaValueType
pattern SchemaValueType_DOUBLE = SchemaValueType' "DOUBLE"

pattern SchemaValueType_FLOAT :: SchemaValueType
pattern SchemaValueType_FLOAT = SchemaValueType' "FLOAT"

pattern SchemaValueType_INT :: SchemaValueType
pattern SchemaValueType_INT = SchemaValueType' "INT"

pattern SchemaValueType_LONG :: SchemaValueType
pattern SchemaValueType_LONG = SchemaValueType' "LONG"

pattern SchemaValueType_STRING :: SchemaValueType
pattern SchemaValueType_STRING = SchemaValueType' "STRING"

{-# COMPLETE
  SchemaValueType_BOOLEAN,
  SchemaValueType_DOUBLE,
  SchemaValueType_FLOAT,
  SchemaValueType_INT,
  SchemaValueType_LONG,
  SchemaValueType_STRING,
  SchemaValueType'
  #-}
