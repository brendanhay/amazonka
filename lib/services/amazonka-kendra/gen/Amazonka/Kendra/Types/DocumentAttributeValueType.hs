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
-- Module      : Amazonka.Kendra.Types.DocumentAttributeValueType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.DocumentAttributeValueType
  ( DocumentAttributeValueType
      ( ..,
        DocumentAttributeValueType_DATE_VALUE,
        DocumentAttributeValueType_LONG_VALUE,
        DocumentAttributeValueType_STRING_LIST_VALUE,
        DocumentAttributeValueType_STRING_VALUE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DocumentAttributeValueType = DocumentAttributeValueType'
  { fromDocumentAttributeValueType ::
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
