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
-- Module      : Amazonka.QuickSight.Types.NamedFilterType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.NamedFilterType
  ( NamedFilterType
      ( ..,
        NamedFilterType_CATEGORY_FILTER,
        NamedFilterType_DATE_RANGE_FILTER,
        NamedFilterType_NUMERIC_EQUALITY_FILTER,
        NamedFilterType_NUMERIC_RANGE_FILTER,
        NamedFilterType_RELATIVE_DATE_FILTER
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype NamedFilterType = NamedFilterType'
  { fromNamedFilterType ::
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

pattern NamedFilterType_CATEGORY_FILTER :: NamedFilterType
pattern NamedFilterType_CATEGORY_FILTER = NamedFilterType' "CATEGORY_FILTER"

pattern NamedFilterType_DATE_RANGE_FILTER :: NamedFilterType
pattern NamedFilterType_DATE_RANGE_FILTER = NamedFilterType' "DATE_RANGE_FILTER"

pattern NamedFilterType_NUMERIC_EQUALITY_FILTER :: NamedFilterType
pattern NamedFilterType_NUMERIC_EQUALITY_FILTER = NamedFilterType' "NUMERIC_EQUALITY_FILTER"

pattern NamedFilterType_NUMERIC_RANGE_FILTER :: NamedFilterType
pattern NamedFilterType_NUMERIC_RANGE_FILTER = NamedFilterType' "NUMERIC_RANGE_FILTER"

pattern NamedFilterType_RELATIVE_DATE_FILTER :: NamedFilterType
pattern NamedFilterType_RELATIVE_DATE_FILTER = NamedFilterType' "RELATIVE_DATE_FILTER"

{-# COMPLETE
  NamedFilterType_CATEGORY_FILTER,
  NamedFilterType_DATE_RANGE_FILTER,
  NamedFilterType_NUMERIC_EQUALITY_FILTER,
  NamedFilterType_NUMERIC_RANGE_FILTER,
  NamedFilterType_RELATIVE_DATE_FILTER,
  NamedFilterType'
  #-}
