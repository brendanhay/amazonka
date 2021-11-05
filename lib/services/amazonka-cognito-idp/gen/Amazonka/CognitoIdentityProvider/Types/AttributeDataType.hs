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
-- Module      : Amazonka.CognitoIdentityProvider.Types.AttributeDataType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.AttributeDataType
  ( AttributeDataType
      ( ..,
        AttributeDataType_Boolean,
        AttributeDataType_DateTime,
        AttributeDataType_Number,
        AttributeDataType_String
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype AttributeDataType = AttributeDataType'
  { fromAttributeDataType ::
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

pattern AttributeDataType_Boolean :: AttributeDataType
pattern AttributeDataType_Boolean = AttributeDataType' "Boolean"

pattern AttributeDataType_DateTime :: AttributeDataType
pattern AttributeDataType_DateTime = AttributeDataType' "DateTime"

pattern AttributeDataType_Number :: AttributeDataType
pattern AttributeDataType_Number = AttributeDataType' "Number"

pattern AttributeDataType_String :: AttributeDataType
pattern AttributeDataType_String = AttributeDataType' "String"

{-# COMPLETE
  AttributeDataType_Boolean,
  AttributeDataType_DateTime,
  AttributeDataType_Number,
  AttributeDataType_String,
  AttributeDataType'
  #-}
