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
-- Module      : Network.AWS.Forecast.Types.AttributeType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Forecast.Types.AttributeType
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype AttributeType = AttributeType'
  { fromAttributeType ::
      Core.Text
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
