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
-- Module      : Network.AWS.CostExplorer.Types.CostCategoryInheritedValueDimensionName
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.CostCategoryInheritedValueDimensionName
  ( CostCategoryInheritedValueDimensionName
      ( ..,
        CostCategoryInheritedValueDimensionName_LINKED_ACCOUNT_NAME,
        CostCategoryInheritedValueDimensionName_TAG
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype CostCategoryInheritedValueDimensionName = CostCategoryInheritedValueDimensionName'
  { fromCostCategoryInheritedValueDimensionName ::
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

pattern CostCategoryInheritedValueDimensionName_LINKED_ACCOUNT_NAME :: CostCategoryInheritedValueDimensionName
pattern CostCategoryInheritedValueDimensionName_LINKED_ACCOUNT_NAME = CostCategoryInheritedValueDimensionName' "LINKED_ACCOUNT_NAME"

pattern CostCategoryInheritedValueDimensionName_TAG :: CostCategoryInheritedValueDimensionName
pattern CostCategoryInheritedValueDimensionName_TAG = CostCategoryInheritedValueDimensionName' "TAG"

{-# COMPLETE
  CostCategoryInheritedValueDimensionName_LINKED_ACCOUNT_NAME,
  CostCategoryInheritedValueDimensionName_TAG,
  CostCategoryInheritedValueDimensionName'
  #-}
