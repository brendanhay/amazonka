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
-- Module      : Amazonka.Evidently.Types.VariationValueType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Evidently.Types.VariationValueType
  ( VariationValueType
      ( ..,
        VariationValueType_BOOLEAN,
        VariationValueType_DOUBLE,
        VariationValueType_LONG,
        VariationValueType_STRING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype VariationValueType = VariationValueType'
  { fromVariationValueType ::
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

pattern VariationValueType_BOOLEAN :: VariationValueType
pattern VariationValueType_BOOLEAN = VariationValueType' "BOOLEAN"

pattern VariationValueType_DOUBLE :: VariationValueType
pattern VariationValueType_DOUBLE = VariationValueType' "DOUBLE"

pattern VariationValueType_LONG :: VariationValueType
pattern VariationValueType_LONG = VariationValueType' "LONG"

pattern VariationValueType_STRING :: VariationValueType
pattern VariationValueType_STRING = VariationValueType' "STRING"

{-# COMPLETE
  VariationValueType_BOOLEAN,
  VariationValueType_DOUBLE,
  VariationValueType_LONG,
  VariationValueType_STRING,
  VariationValueType'
  #-}
