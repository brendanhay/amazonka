{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.OfferingPromotion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DeviceFarm.Types.OfferingPromotion
  ( OfferingPromotion (..)
  -- * Smart constructor
  , mkOfferingPromotion
  -- * Lenses
  , opDescription
  , opId
  ) where

import qualified Network.AWS.DeviceFarm.Types.Description as Types
import qualified Network.AWS.DeviceFarm.Types.OfferingPromotionIdentifier as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents information about an offering promotion.
--
-- /See:/ 'mkOfferingPromotion' smart constructor.
data OfferingPromotion = OfferingPromotion'
  { description :: Core.Maybe Types.Description
    -- ^ A string that describes the offering promotion.
  , id :: Core.Maybe Types.OfferingPromotionIdentifier
    -- ^ The ID of the offering promotion.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OfferingPromotion' value with any optional fields omitted.
mkOfferingPromotion
    :: OfferingPromotion
mkOfferingPromotion
  = OfferingPromotion'{description = Core.Nothing, id = Core.Nothing}

-- | A string that describes the offering promotion.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
opDescription :: Lens.Lens' OfferingPromotion (Core.Maybe Types.Description)
opDescription = Lens.field @"description"
{-# INLINEABLE opDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The ID of the offering promotion.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
opId :: Lens.Lens' OfferingPromotion (Core.Maybe Types.OfferingPromotionIdentifier)
opId = Lens.field @"id"
{-# INLINEABLE opId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

instance Core.FromJSON OfferingPromotion where
        parseJSON
          = Core.withObject "OfferingPromotion" Core.$
              \ x ->
                OfferingPromotion' Core.<$>
                  (x Core..:? "description") Core.<*> x Core..:? "id"
