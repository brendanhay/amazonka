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
-- Module      : Amazonka.CostExplorer.Types.CostCategoryInheritedValueDimensionName
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.CostCategoryInheritedValueDimensionName
  ( CostCategoryInheritedValueDimensionName
      ( ..,
        CostCategoryInheritedValueDimensionName_LINKED_ACCOUNT_NAME,
        CostCategoryInheritedValueDimensionName_TAG
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype CostCategoryInheritedValueDimensionName = CostCategoryInheritedValueDimensionName'
  { fromCostCategoryInheritedValueDimensionName ::
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

pattern CostCategoryInheritedValueDimensionName_LINKED_ACCOUNT_NAME :: CostCategoryInheritedValueDimensionName
pattern CostCategoryInheritedValueDimensionName_LINKED_ACCOUNT_NAME = CostCategoryInheritedValueDimensionName' "LINKED_ACCOUNT_NAME"

pattern CostCategoryInheritedValueDimensionName_TAG :: CostCategoryInheritedValueDimensionName
pattern CostCategoryInheritedValueDimensionName_TAG = CostCategoryInheritedValueDimensionName' "TAG"

{-# COMPLETE
  CostCategoryInheritedValueDimensionName_LINKED_ACCOUNT_NAME,
  CostCategoryInheritedValueDimensionName_TAG,
  CostCategoryInheritedValueDimensionName'
  #-}
