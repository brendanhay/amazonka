{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.SubResourceSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Shield.Types.SubResourceSummary
  ( SubResourceSummary (..)
  -- * Smart constructor
  , mkSubResourceSummary
  -- * Lenses
  , srsAttackVectors
  , srsCounters
  , srsId
  , srsType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Shield.Types.SubResourceType as Types
import qualified Network.AWS.Shield.Types.SummarizedAttackVector as Types
import qualified Network.AWS.Shield.Types.SummarizedCounter as Types

-- | The attack information for the specified SubResource.
--
-- /See:/ 'mkSubResourceSummary' smart constructor.
data SubResourceSummary = SubResourceSummary'
  { attackVectors :: Core.Maybe [Types.SummarizedAttackVector]
    -- ^ The list of attack types and associated counters.
  , counters :: Core.Maybe [Types.SummarizedCounter]
    -- ^ The counters that describe the details of the attack.
  , id :: Core.Maybe Core.Text
    -- ^ The unique identifier (ID) of the @SubResource@ .
  , type' :: Core.Maybe Types.SubResourceType
    -- ^ The @SubResource@ type.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SubResourceSummary' value with any optional fields omitted.
mkSubResourceSummary
    :: SubResourceSummary
mkSubResourceSummary
  = SubResourceSummary'{attackVectors = Core.Nothing,
                        counters = Core.Nothing, id = Core.Nothing, type' = Core.Nothing}

-- | The list of attack types and associated counters.
--
-- /Note:/ Consider using 'attackVectors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsAttackVectors :: Lens.Lens' SubResourceSummary (Core.Maybe [Types.SummarizedAttackVector])
srsAttackVectors = Lens.field @"attackVectors"
{-# INLINEABLE srsAttackVectors #-}
{-# DEPRECATED attackVectors "Use generic-lens or generic-optics with 'attackVectors' instead"  #-}

-- | The counters that describe the details of the attack.
--
-- /Note:/ Consider using 'counters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsCounters :: Lens.Lens' SubResourceSummary (Core.Maybe [Types.SummarizedCounter])
srsCounters = Lens.field @"counters"
{-# INLINEABLE srsCounters #-}
{-# DEPRECATED counters "Use generic-lens or generic-optics with 'counters' instead"  #-}

-- | The unique identifier (ID) of the @SubResource@ .
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsId :: Lens.Lens' SubResourceSummary (Core.Maybe Core.Text)
srsId = Lens.field @"id"
{-# INLINEABLE srsId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The @SubResource@ type.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsType :: Lens.Lens' SubResourceSummary (Core.Maybe Types.SubResourceType)
srsType = Lens.field @"type'"
{-# INLINEABLE srsType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromJSON SubResourceSummary where
        parseJSON
          = Core.withObject "SubResourceSummary" Core.$
              \ x ->
                SubResourceSummary' Core.<$>
                  (x Core..:? "AttackVectors") Core.<*> x Core..:? "Counters"
                    Core.<*> x Core..:? "Id"
                    Core.<*> x Core..:? "Type"
