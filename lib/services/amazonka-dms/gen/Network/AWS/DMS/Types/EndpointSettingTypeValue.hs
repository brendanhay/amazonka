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
-- Module      : Network.AWS.DMS.Types.EndpointSettingTypeValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.EndpointSettingTypeValue
  ( EndpointSettingTypeValue
      ( ..,
        EndpointSettingTypeValue_Boolean,
        EndpointSettingTypeValue_Enum,
        EndpointSettingTypeValue_Integer,
        EndpointSettingTypeValue_String
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype EndpointSettingTypeValue = EndpointSettingTypeValue'
  { fromEndpointSettingTypeValue ::
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
