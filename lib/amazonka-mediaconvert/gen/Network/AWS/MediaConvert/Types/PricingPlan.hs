{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.PricingPlan
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.PricingPlan
  ( PricingPlan
    ( PricingPlan'
    , PricingPlanOnDemand
    , PricingPlanReserved
    , fromPricingPlan
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Specifies whether the pricing plan for the queue is on-demand or reserved. For on-demand, you pay per minute, billed in increments of .01 minute. For reserved, you pay for the transcoding capacity of the entire queue, regardless of how much or how little you use it. Reserved pricing requires a 12-month commitment.
newtype PricingPlan = PricingPlan'{fromPricingPlan :: Core.Text}
                        deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                        Core.Generic)
                        deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                          Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                          Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                          Core.FromText, Core.ToByteString, Core.ToQuery,
                                          Core.ToHeader)

pattern PricingPlanOnDemand :: PricingPlan
pattern PricingPlanOnDemand = PricingPlan' "ON_DEMAND"

pattern PricingPlanReserved :: PricingPlan
pattern PricingPlanReserved = PricingPlan' "RESERVED"

{-# COMPLETE 
  PricingPlanOnDemand,

  PricingPlanReserved,
  PricingPlan'
  #-}
