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
-- Module      : Amazonka.CostExplorer.Types.CostCategorySplitChargeMethod
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.CostCategorySplitChargeMethod
  ( CostCategorySplitChargeMethod
      ( ..,
        CostCategorySplitChargeMethod_EVEN,
        CostCategorySplitChargeMethod_FIXED,
        CostCategorySplitChargeMethod_PROPORTIONAL
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype CostCategorySplitChargeMethod = CostCategorySplitChargeMethod'
  { fromCostCategorySplitChargeMethod ::
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

pattern CostCategorySplitChargeMethod_EVEN :: CostCategorySplitChargeMethod
pattern CostCategorySplitChargeMethod_EVEN = CostCategorySplitChargeMethod' "EVEN"

pattern CostCategorySplitChargeMethod_FIXED :: CostCategorySplitChargeMethod
pattern CostCategorySplitChargeMethod_FIXED = CostCategorySplitChargeMethod' "FIXED"

pattern CostCategorySplitChargeMethod_PROPORTIONAL :: CostCategorySplitChargeMethod
pattern CostCategorySplitChargeMethod_PROPORTIONAL = CostCategorySplitChargeMethod' "PROPORTIONAL"

{-# COMPLETE
  CostCategorySplitChargeMethod_EVEN,
  CostCategorySplitChargeMethod_FIXED,
  CostCategorySplitChargeMethod_PROPORTIONAL,
  CostCategorySplitChargeMethod'
  #-}
