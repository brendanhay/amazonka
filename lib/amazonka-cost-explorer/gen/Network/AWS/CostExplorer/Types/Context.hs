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
        CostAndUsage,
        Reservations,
        SavingsPlans
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype Context = Context' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern CostAndUsage :: Context
pattern CostAndUsage = Context' "COST_AND_USAGE"

pattern Reservations :: Context
pattern Reservations = Context' "RESERVATIONS"

pattern SavingsPlans :: Context
pattern SavingsPlans = Context' "SAVINGS_PLANS"

{-# COMPLETE
  CostAndUsage,
  Reservations,
  SavingsPlans,
  Context'
  #-}
