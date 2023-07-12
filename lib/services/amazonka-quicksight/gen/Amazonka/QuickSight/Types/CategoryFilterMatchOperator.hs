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
-- Module      : Amazonka.QuickSight.Types.CategoryFilterMatchOperator
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.CategoryFilterMatchOperator
  ( CategoryFilterMatchOperator
      ( ..,
        CategoryFilterMatchOperator_CONTAINS,
        CategoryFilterMatchOperator_DOES_NOT_CONTAIN,
        CategoryFilterMatchOperator_DOES_NOT_EQUAL,
        CategoryFilterMatchOperator_ENDS_WITH,
        CategoryFilterMatchOperator_EQUALS,
        CategoryFilterMatchOperator_STARTS_WITH
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype CategoryFilterMatchOperator = CategoryFilterMatchOperator'
  { fromCategoryFilterMatchOperator ::
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

pattern CategoryFilterMatchOperator_CONTAINS :: CategoryFilterMatchOperator
pattern CategoryFilterMatchOperator_CONTAINS = CategoryFilterMatchOperator' "CONTAINS"

pattern CategoryFilterMatchOperator_DOES_NOT_CONTAIN :: CategoryFilterMatchOperator
pattern CategoryFilterMatchOperator_DOES_NOT_CONTAIN = CategoryFilterMatchOperator' "DOES_NOT_CONTAIN"

pattern CategoryFilterMatchOperator_DOES_NOT_EQUAL :: CategoryFilterMatchOperator
pattern CategoryFilterMatchOperator_DOES_NOT_EQUAL = CategoryFilterMatchOperator' "DOES_NOT_EQUAL"

pattern CategoryFilterMatchOperator_ENDS_WITH :: CategoryFilterMatchOperator
pattern CategoryFilterMatchOperator_ENDS_WITH = CategoryFilterMatchOperator' "ENDS_WITH"

pattern CategoryFilterMatchOperator_EQUALS :: CategoryFilterMatchOperator
pattern CategoryFilterMatchOperator_EQUALS = CategoryFilterMatchOperator' "EQUALS"

pattern CategoryFilterMatchOperator_STARTS_WITH :: CategoryFilterMatchOperator
pattern CategoryFilterMatchOperator_STARTS_WITH = CategoryFilterMatchOperator' "STARTS_WITH"

{-# COMPLETE
  CategoryFilterMatchOperator_CONTAINS,
  CategoryFilterMatchOperator_DOES_NOT_CONTAIN,
  CategoryFilterMatchOperator_DOES_NOT_EQUAL,
  CategoryFilterMatchOperator_ENDS_WITH,
  CategoryFilterMatchOperator_EQUALS,
  CategoryFilterMatchOperator_STARTS_WITH,
  CategoryFilterMatchOperator'
  #-}
