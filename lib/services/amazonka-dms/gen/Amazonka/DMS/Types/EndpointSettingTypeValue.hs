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
-- Module      : Amazonka.DMS.Types.EndpointSettingTypeValue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.EndpointSettingTypeValue
  ( EndpointSettingTypeValue
      ( ..,
        EndpointSettingTypeValue_Boolean,
        EndpointSettingTypeValue_Enum,
        EndpointSettingTypeValue_Integer,
        EndpointSettingTypeValue_String
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype EndpointSettingTypeValue = EndpointSettingTypeValue'
  { fromEndpointSettingTypeValue ::
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

pattern EndpointSettingTypeValue_Boolean :: EndpointSettingTypeValue
pattern EndpointSettingTypeValue_Boolean = EndpointSettingTypeValue' "boolean"

pattern EndpointSettingTypeValue_Enum :: EndpointSettingTypeValue
pattern EndpointSettingTypeValue_Enum = EndpointSettingTypeValue' "enum"

pattern EndpointSettingTypeValue_Integer :: EndpointSettingTypeValue
pattern EndpointSettingTypeValue_Integer = EndpointSettingTypeValue' "integer"

pattern EndpointSettingTypeValue_String :: EndpointSettingTypeValue
pattern EndpointSettingTypeValue_String = EndpointSettingTypeValue' "string"

{-# COMPLETE
  EndpointSettingTypeValue_Boolean,
  EndpointSettingTypeValue_Enum,
  EndpointSettingTypeValue_Integer,
  EndpointSettingTypeValue_String,
  EndpointSettingTypeValue'
  #-}
