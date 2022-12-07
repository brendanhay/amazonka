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
-- Module      : Amazonka.QuickSight.Types.ColumnDataType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ColumnDataType
  ( ColumnDataType
      ( ..,
        ColumnDataType_DATETIME,
        ColumnDataType_DECIMAL,
        ColumnDataType_INTEGER,
        ColumnDataType_STRING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ColumnDataType = ColumnDataType'
  { fromColumnDataType ::
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

pattern ColumnDataType_DATETIME :: ColumnDataType
pattern ColumnDataType_DATETIME = ColumnDataType' "DATETIME"

pattern ColumnDataType_DECIMAL :: ColumnDataType
pattern ColumnDataType_DECIMAL = ColumnDataType' "DECIMAL"

pattern ColumnDataType_INTEGER :: ColumnDataType
pattern ColumnDataType_INTEGER = ColumnDataType' "INTEGER"

pattern ColumnDataType_STRING :: ColumnDataType
pattern ColumnDataType_STRING = ColumnDataType' "STRING"

{-# COMPLETE
  ColumnDataType_DATETIME,
  ColumnDataType_DECIMAL,
  ColumnDataType_INTEGER,
  ColumnDataType_STRING,
  ColumnDataType'
  #-}
