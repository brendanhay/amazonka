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
-- Module      : Amazonka.IoTSiteWise.Types.PropertyDataType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.PropertyDataType
  ( PropertyDataType
      ( ..,
        PropertyDataType_BOOLEAN,
        PropertyDataType_DOUBLE,
        PropertyDataType_INTEGER,
        PropertyDataType_STRING,
        PropertyDataType_STRUCT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PropertyDataType = PropertyDataType'
  { fromPropertyDataType ::
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

pattern PropertyDataType_BOOLEAN :: PropertyDataType
pattern PropertyDataType_BOOLEAN = PropertyDataType' "BOOLEAN"

pattern PropertyDataType_DOUBLE :: PropertyDataType
pattern PropertyDataType_DOUBLE = PropertyDataType' "DOUBLE"

pattern PropertyDataType_INTEGER :: PropertyDataType
pattern PropertyDataType_INTEGER = PropertyDataType' "INTEGER"

pattern PropertyDataType_STRING :: PropertyDataType
pattern PropertyDataType_STRING = PropertyDataType' "STRING"

pattern PropertyDataType_STRUCT :: PropertyDataType
pattern PropertyDataType_STRUCT = PropertyDataType' "STRUCT"

{-# COMPLETE
  PropertyDataType_BOOLEAN,
  PropertyDataType_DOUBLE,
  PropertyDataType_INTEGER,
  PropertyDataType_STRING,
  PropertyDataType_STRUCT,
  PropertyDataType'
  #-}
