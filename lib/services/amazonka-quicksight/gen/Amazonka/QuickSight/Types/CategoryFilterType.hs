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
-- Module      : Amazonka.QuickSight.Types.CategoryFilterType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.CategoryFilterType
  ( CategoryFilterType
      ( ..,
        CategoryFilterType_CUSTOM_FILTER,
        CategoryFilterType_CUSTOM_FILTER_LIST,
        CategoryFilterType_FILTER_LIST
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype CategoryFilterType = CategoryFilterType'
  { fromCategoryFilterType ::
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

pattern CategoryFilterType_CUSTOM_FILTER :: CategoryFilterType
pattern CategoryFilterType_CUSTOM_FILTER = CategoryFilterType' "CUSTOM_FILTER"

pattern CategoryFilterType_CUSTOM_FILTER_LIST :: CategoryFilterType
pattern CategoryFilterType_CUSTOM_FILTER_LIST = CategoryFilterType' "CUSTOM_FILTER_LIST"

pattern CategoryFilterType_FILTER_LIST :: CategoryFilterType
pattern CategoryFilterType_FILTER_LIST = CategoryFilterType' "FILTER_LIST"

{-# COMPLETE
  CategoryFilterType_CUSTOM_FILTER,
  CategoryFilterType_CUSTOM_FILTER_LIST,
  CategoryFilterType_FILTER_LIST,
  CategoryFilterType'
  #-}
