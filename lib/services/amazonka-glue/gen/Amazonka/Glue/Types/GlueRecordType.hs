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
-- Module      : Amazonka.Glue.Types.GlueRecordType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.GlueRecordType
  ( GlueRecordType
      ( ..,
        GlueRecordType_BIGDECIMAL,
        GlueRecordType_BYTE,
        GlueRecordType_DATE,
        GlueRecordType_DOUBLE,
        GlueRecordType_FLOAT,
        GlueRecordType_INT,
        GlueRecordType_LONG,
        GlueRecordType_SHORT,
        GlueRecordType_STRING,
        GlueRecordType_TIMESTAMP
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype GlueRecordType = GlueRecordType'
  { fromGlueRecordType ::
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

pattern GlueRecordType_BIGDECIMAL :: GlueRecordType
pattern GlueRecordType_BIGDECIMAL = GlueRecordType' "BIGDECIMAL"

pattern GlueRecordType_BYTE :: GlueRecordType
pattern GlueRecordType_BYTE = GlueRecordType' "BYTE"

pattern GlueRecordType_DATE :: GlueRecordType
pattern GlueRecordType_DATE = GlueRecordType' "DATE"

pattern GlueRecordType_DOUBLE :: GlueRecordType
pattern GlueRecordType_DOUBLE = GlueRecordType' "DOUBLE"

pattern GlueRecordType_FLOAT :: GlueRecordType
pattern GlueRecordType_FLOAT = GlueRecordType' "FLOAT"

pattern GlueRecordType_INT :: GlueRecordType
pattern GlueRecordType_INT = GlueRecordType' "INT"

pattern GlueRecordType_LONG :: GlueRecordType
pattern GlueRecordType_LONG = GlueRecordType' "LONG"

pattern GlueRecordType_SHORT :: GlueRecordType
pattern GlueRecordType_SHORT = GlueRecordType' "SHORT"

pattern GlueRecordType_STRING :: GlueRecordType
pattern GlueRecordType_STRING = GlueRecordType' "STRING"

pattern GlueRecordType_TIMESTAMP :: GlueRecordType
pattern GlueRecordType_TIMESTAMP = GlueRecordType' "TIMESTAMP"

{-# COMPLETE
  GlueRecordType_BIGDECIMAL,
  GlueRecordType_BYTE,
  GlueRecordType_DATE,
  GlueRecordType_DOUBLE,
  GlueRecordType_FLOAT,
  GlueRecordType_INT,
  GlueRecordType_LONG,
  GlueRecordType_SHORT,
  GlueRecordType_STRING,
  GlueRecordType_TIMESTAMP,
  GlueRecordType'
  #-}
