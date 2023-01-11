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
-- Module      : Amazonka.TimeStreamQuery.Types.ScalarMeasureValueType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TimeStreamQuery.Types.ScalarMeasureValueType
  ( ScalarMeasureValueType
      ( ..,
        ScalarMeasureValueType_BIGINT,
        ScalarMeasureValueType_BOOLEAN,
        ScalarMeasureValueType_DOUBLE,
        ScalarMeasureValueType_TIMESTAMP,
        ScalarMeasureValueType_VARCHAR
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ScalarMeasureValueType = ScalarMeasureValueType'
  { fromScalarMeasureValueType ::
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

pattern ScalarMeasureValueType_BIGINT :: ScalarMeasureValueType
pattern ScalarMeasureValueType_BIGINT = ScalarMeasureValueType' "BIGINT"

pattern ScalarMeasureValueType_BOOLEAN :: ScalarMeasureValueType
pattern ScalarMeasureValueType_BOOLEAN = ScalarMeasureValueType' "BOOLEAN"

pattern ScalarMeasureValueType_DOUBLE :: ScalarMeasureValueType
pattern ScalarMeasureValueType_DOUBLE = ScalarMeasureValueType' "DOUBLE"

pattern ScalarMeasureValueType_TIMESTAMP :: ScalarMeasureValueType
pattern ScalarMeasureValueType_TIMESTAMP = ScalarMeasureValueType' "TIMESTAMP"

pattern ScalarMeasureValueType_VARCHAR :: ScalarMeasureValueType
pattern ScalarMeasureValueType_VARCHAR = ScalarMeasureValueType' "VARCHAR"

{-# COMPLETE
  ScalarMeasureValueType_BIGINT,
  ScalarMeasureValueType_BOOLEAN,
  ScalarMeasureValueType_DOUBLE,
  ScalarMeasureValueType_TIMESTAMP,
  ScalarMeasureValueType_VARCHAR,
  ScalarMeasureValueType'
  #-}
