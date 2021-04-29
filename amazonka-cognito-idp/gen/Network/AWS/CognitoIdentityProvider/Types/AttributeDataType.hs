{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.AttributeDataType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.AttributeDataType
  ( AttributeDataType
      ( ..,
        AttributeDataType_Boolean,
        AttributeDataType_DateTime,
        AttributeDataType_Number,
        AttributeDataType_String
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype AttributeDataType = AttributeDataType'
  { fromAttributeDataType ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
