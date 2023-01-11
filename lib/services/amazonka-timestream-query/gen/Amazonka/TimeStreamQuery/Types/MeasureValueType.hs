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
-- Module      : Amazonka.TimeStreamQuery.Types.MeasureValueType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TimeStreamQuery.Types.MeasureValueType
  ( MeasureValueType
      ( ..,
        MeasureValueType_BIGINT,
        MeasureValueType_BOOLEAN,
        MeasureValueType_DOUBLE,
        MeasureValueType_MULTI,
        MeasureValueType_VARCHAR
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype MeasureValueType = MeasureValueType'
  { fromMeasureValueType ::
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

pattern MeasureValueType_BIGINT :: MeasureValueType
pattern MeasureValueType_BIGINT = MeasureValueType' "BIGINT"

pattern MeasureValueType_BOOLEAN :: MeasureValueType
pattern MeasureValueType_BOOLEAN = MeasureValueType' "BOOLEAN"

pattern MeasureValueType_DOUBLE :: MeasureValueType
pattern MeasureValueType_DOUBLE = MeasureValueType' "DOUBLE"

pattern MeasureValueType_MULTI :: MeasureValueType
pattern MeasureValueType_MULTI = MeasureValueType' "MULTI"

pattern MeasureValueType_VARCHAR :: MeasureValueType
pattern MeasureValueType_VARCHAR = MeasureValueType' "VARCHAR"

{-# COMPLETE
  MeasureValueType_BIGINT,
  MeasureValueType_BOOLEAN,
  MeasureValueType_DOUBLE,
  MeasureValueType_MULTI,
  MeasureValueType_VARCHAR,
  MeasureValueType'
  #-}
