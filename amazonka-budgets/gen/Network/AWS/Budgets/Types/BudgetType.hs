{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.BudgetType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.BudgetType
  ( BudgetType
      ( ..,
        BudgetType_COST,
        BudgetType_RI_COVERAGE,
        BudgetType_RI_UTILIZATION,
        BudgetType_SAVINGS_PLANS_COVERAGE,
        BudgetType_SAVINGS_PLANS_UTILIZATION,
        BudgetType_USAGE
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | The type of a budget. It must be one of the following types:
--
-- @COST@, @USAGE@, @RI_UTILIZATION@, @RI_COVERAGE@,
-- @SAVINGS_PLANS_UTILIZATION@, or @SAVINGS_PLANS_COVERAGE@.
newtype BudgetType = BudgetType'
  { fromBudgetType ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern BudgetType_COST :: BudgetType
pattern BudgetType_COST = BudgetType' "COST"

pattern BudgetType_RI_COVERAGE :: BudgetType
pattern BudgetType_RI_COVERAGE = BudgetType' "RI_COVERAGE"

pattern BudgetType_RI_UTILIZATION :: BudgetType
pattern BudgetType_RI_UTILIZATION = BudgetType' "RI_UTILIZATION"

pattern BudgetType_SAVINGS_PLANS_COVERAGE :: BudgetType
pattern BudgetType_SAVINGS_PLANS_COVERAGE = BudgetType' "SAVINGS_PLANS_COVERAGE"

pattern BudgetType_SAVINGS_PLANS_UTILIZATION :: BudgetType
pattern BudgetType_SAVINGS_PLANS_UTILIZATION = BudgetType' "SAVINGS_PLANS_UTILIZATION"

pattern BudgetType_USAGE :: BudgetType
pattern BudgetType_USAGE = BudgetType' "USAGE"

{-# COMPLETE
  BudgetType_COST,
  BudgetType_RI_COVERAGE,
  BudgetType_RI_UTILIZATION,
  BudgetType_SAVINGS_PLANS_COVERAGE,
  BudgetType_SAVINGS_PLANS_UTILIZATION,
  BudgetType_USAGE,
  BudgetType'
  #-}
