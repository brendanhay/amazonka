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
-- Module      : Amazonka.Forecast.Types.AttributeType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Forecast.Types.AttributeType
  ( AttributeType
      ( ..,
        AttributeType_Float,
        AttributeType_Geolocation,
        AttributeType_Integer,
        AttributeType_String,
        AttributeType_Timestamp
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AttributeType = AttributeType'
  { fromAttributeType ::
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

pattern AttributeType_Float :: AttributeType
pattern AttributeType_Float = AttributeType' "float"

pattern AttributeType_Geolocation :: AttributeType
pattern AttributeType_Geolocation = AttributeType' "geolocation"

pattern AttributeType_Integer :: AttributeType
pattern AttributeType_Integer = AttributeType' "integer"

pattern AttributeType_String :: AttributeType
pattern AttributeType_String = AttributeType' "string"

pattern AttributeType_Timestamp :: AttributeType
pattern AttributeType_Timestamp = AttributeType' "timestamp"

{-# COMPLETE
  AttributeType_Float,
  AttributeType_Geolocation,
  AttributeType_Integer,
  AttributeType_String,
  AttributeType_Timestamp,
  AttributeType'
  #-}
