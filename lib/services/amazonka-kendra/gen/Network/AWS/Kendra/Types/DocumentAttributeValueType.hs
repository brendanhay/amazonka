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
-- Module      : Network.AWS.Kendra.Types.DocumentAttributeValueType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kendra.Types.DocumentAttributeValueType
  ( DocumentAttributeValueType
      ( ..,
        DocumentAttributeValueType_DATE_VALUE,
        DocumentAttributeValueType_LONG_VALUE,
        DocumentAttributeValueType_STRING_LIST_VALUE,
        DocumentAttributeValueType_STRING_VALUE
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype DocumentAttributeValueType = DocumentAttributeValueType'
  { fromDocumentAttributeValueType ::
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

pattern DocumentAttributeValueType_DATE_VALUE :: DocumentAttributeValueType
pattern DocumentAttributeValueType_DATE_VALUE = DocumentAttributeValueType' "DATE_VALUE"

pattern DocumentAttributeValueType_LONG_VALUE :: DocumentAttributeValueType
pattern DocumentAttributeValueType_LONG_VALUE = DocumentAttributeValueType' "LONG_VALUE"

pattern DocumentAttributeValueType_STRING_LIST_VALUE :: DocumentAttributeValueType
pattern DocumentAttributeValueType_STRING_LIST_VALUE = DocumentAttributeValueType' "STRING_LIST_VALUE"

pattern DocumentAttributeValueType_STRING_VALUE :: DocumentAttributeValueType
pattern DocumentAttributeValueType_STRING_VALUE = DocumentAttributeValueType' "STRING_VALUE"

{-# COMPLETE
  DocumentAttributeValueType_DATE_VALUE,
  DocumentAttributeValueType_LONG_VALUE,
  DocumentAttributeValueType_STRING_LIST_VALUE,
  DocumentAttributeValueType_STRING_VALUE,
  DocumentAttributeValueType'
  #-}
