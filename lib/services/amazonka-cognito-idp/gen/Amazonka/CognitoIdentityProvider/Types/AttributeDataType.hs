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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AttributeDataType = AttributeDataType'
  { fromAttributeDataType ::
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
