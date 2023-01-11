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
-- Module      : Amazonka.ConnectCases.Types.FieldType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectCases.Types.FieldType
  ( FieldType
      ( ..,
        FieldType_Boolean,
        FieldType_DateTime,
        FieldType_Number,
        FieldType_SingleSelect,
        FieldType_Text
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype FieldType = FieldType'
  { fromFieldType ::
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

pattern FieldType_Boolean :: FieldType
pattern FieldType_Boolean = FieldType' "Boolean"

pattern FieldType_DateTime :: FieldType
pattern FieldType_DateTime = FieldType' "DateTime"

pattern FieldType_Number :: FieldType
pattern FieldType_Number = FieldType' "Number"

pattern FieldType_SingleSelect :: FieldType
pattern FieldType_SingleSelect = FieldType' "SingleSelect"

pattern FieldType_Text :: FieldType
pattern FieldType_Text = FieldType' "Text"

{-# COMPLETE
  FieldType_Boolean,
  FieldType_DateTime,
  FieldType_Number,
  FieldType_SingleSelect,
  FieldType_Text,
  FieldType'
  #-}
