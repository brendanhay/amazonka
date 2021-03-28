{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.BillingDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.StepFunctions.Types.BillingDetails
  ( BillingDetails (..)
  -- * Smart constructor
  , mkBillingDetails
  -- * Lenses
  , bdBilledDurationInMilliseconds
  , bdBilledMemoryUsedInMB
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object that describes workflow billing details.
--
-- /See:/ 'mkBillingDetails' smart constructor.
data BillingDetails = BillingDetails'
  { billedDurationInMilliseconds :: Core.Maybe Core.Natural
    -- ^ Billed duration of your workflow, in milliseconds.
  , billedMemoryUsedInMB :: Core.Maybe Core.Natural
    -- ^ Billed memory consumption of your workflow, in MB.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BillingDetails' value with any optional fields omitted.
mkBillingDetails
    :: BillingDetails
mkBillingDetails
  = BillingDetails'{billedDurationInMilliseconds = Core.Nothing,
                    billedMemoryUsedInMB = Core.Nothing}

-- | Billed duration of your workflow, in milliseconds.
--
-- /Note:/ Consider using 'billedDurationInMilliseconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdBilledDurationInMilliseconds :: Lens.Lens' BillingDetails (Core.Maybe Core.Natural)
bdBilledDurationInMilliseconds = Lens.field @"billedDurationInMilliseconds"
{-# INLINEABLE bdBilledDurationInMilliseconds #-}
{-# DEPRECATED billedDurationInMilliseconds "Use generic-lens or generic-optics with 'billedDurationInMilliseconds' instead"  #-}

-- | Billed memory consumption of your workflow, in MB.
--
-- /Note:/ Consider using 'billedMemoryUsedInMB' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdBilledMemoryUsedInMB :: Lens.Lens' BillingDetails (Core.Maybe Core.Natural)
bdBilledMemoryUsedInMB = Lens.field @"billedMemoryUsedInMB"
{-# INLINEABLE bdBilledMemoryUsedInMB #-}
{-# DEPRECATED billedMemoryUsedInMB "Use generic-lens or generic-optics with 'billedMemoryUsedInMB' instead"  #-}

instance Core.FromJSON BillingDetails where
        parseJSON
          = Core.withObject "BillingDetails" Core.$
              \ x ->
                BillingDetails' Core.<$>
                  (x Core..:? "billedDurationInMilliseconds") Core.<*>
                    x Core..:? "billedMemoryUsedInMB"
