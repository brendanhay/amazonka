{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.BillingGroupProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.BillingGroupProperties
  ( BillingGroupProperties (..)
  -- * Smart constructor
  , mkBillingGroupProperties
  -- * Lenses
  , bgpBillingGroupDescription
  ) where

import qualified Network.AWS.IoT.Types.BillingGroupDescription as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The properties of a billing group.
--
-- /See:/ 'mkBillingGroupProperties' smart constructor.
newtype BillingGroupProperties = BillingGroupProperties'
  { billingGroupDescription :: Core.Maybe Types.BillingGroupDescription
    -- ^ The description of the billing group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'BillingGroupProperties' value with any optional fields omitted.
mkBillingGroupProperties
    :: BillingGroupProperties
mkBillingGroupProperties
  = BillingGroupProperties'{billingGroupDescription = Core.Nothing}

-- | The description of the billing group.
--
-- /Note:/ Consider using 'billingGroupDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgpBillingGroupDescription :: Lens.Lens' BillingGroupProperties (Core.Maybe Types.BillingGroupDescription)
bgpBillingGroupDescription = Lens.field @"billingGroupDescription"
{-# INLINEABLE bgpBillingGroupDescription #-}
{-# DEPRECATED billingGroupDescription "Use generic-lens or generic-optics with 'billingGroupDescription' instead"  #-}

instance Core.FromJSON BillingGroupProperties where
        toJSON BillingGroupProperties{..}
          = Core.object
              (Core.catMaybes
                 [("billingGroupDescription" Core..=) Core.<$>
                    billingGroupDescription])

instance Core.FromJSON BillingGroupProperties where
        parseJSON
          = Core.withObject "BillingGroupProperties" Core.$
              \ x ->
                BillingGroupProperties' Core.<$>
                  (x Core..:? "billingGroupDescription")
