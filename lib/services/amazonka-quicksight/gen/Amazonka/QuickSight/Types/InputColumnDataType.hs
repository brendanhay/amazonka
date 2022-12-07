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
-- Module      : Amazonka.QuickSight.Types.InputColumnDataType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.InputColumnDataType
  ( InputColumnDataType
      ( ..,
        InputColumnDataType_BIT,
        InputColumnDataType_BOOLEAN,
        InputColumnDataType_DATETIME,
        InputColumnDataType_DECIMAL,
        InputColumnDataType_INTEGER,
        InputColumnDataType_JSON,
        InputColumnDataType_STRING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype InputColumnDataType = InputColumnDataType'
  { fromInputColumnDataType ::
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

pattern InputColumnDataType_BIT :: InputColumnDataType
pattern InputColumnDataType_BIT = InputColumnDataType' "BIT"

pattern InputColumnDataType_BOOLEAN :: InputColumnDataType
pattern InputColumnDataType_BOOLEAN = InputColumnDataType' "BOOLEAN"

pattern InputColumnDataType_DATETIME :: InputColumnDataType
pattern InputColumnDataType_DATETIME = InputColumnDataType' "DATETIME"

pattern InputColumnDataType_DECIMAL :: InputColumnDataType
pattern InputColumnDataType_DECIMAL = InputColumnDataType' "DECIMAL"

pattern InputColumnDataType_INTEGER :: InputColumnDataType
pattern InputColumnDataType_INTEGER = InputColumnDataType' "INTEGER"

pattern InputColumnDataType_JSON :: InputColumnDataType
pattern InputColumnDataType_JSON = InputColumnDataType' "JSON"

pattern InputColumnDataType_STRING :: InputColumnDataType
pattern InputColumnDataType_STRING = InputColumnDataType' "STRING"

{-# COMPLETE
  InputColumnDataType_BIT,
  InputColumnDataType_BOOLEAN,
  InputColumnDataType_DATETIME,
  InputColumnDataType_DECIMAL,
  InputColumnDataType_INTEGER,
  InputColumnDataType_JSON,
  InputColumnDataType_STRING,
  InputColumnDataType'
  #-}
