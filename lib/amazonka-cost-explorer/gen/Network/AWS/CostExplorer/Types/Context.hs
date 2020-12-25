{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.Context
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.Context
  ( Context
      ( Context',
        ContextCostAndUsage,
        ContextReservations,
        ContextSavingsPlans,
        fromContext
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype Context = Context' {fromContext :: Core.Text}
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern ContextCostAndUsage :: Context
pattern ContextCostAndUsage = Context' "COST_AND_USAGE"

pattern ContextReservations :: Context
pattern ContextReservations = Context' "RESERVATIONS"

pattern ContextSavingsPlans :: Context
pattern ContextSavingsPlans = Context' "SAVINGS_PLANS"

{-# COMPLETE
  ContextCostAndUsage,
  ContextReservations,
  ContextSavingsPlans,
  Context'
  #-}
