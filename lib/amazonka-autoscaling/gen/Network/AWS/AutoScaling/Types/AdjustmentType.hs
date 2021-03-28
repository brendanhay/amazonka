{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.AdjustmentType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AutoScaling.Types.AdjustmentType
  ( AdjustmentType (..)
  -- * Smart constructor
  , mkAdjustmentType
  -- * Lenses
  , atAdjustmentType
  ) where

import qualified Network.AWS.AutoScaling.Types.XmlStringMaxLen255 as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a policy adjustment type.
--
-- /See:/ 'mkAdjustmentType' smart constructor.
newtype AdjustmentType = AdjustmentType'
  { adjustmentType :: Core.Maybe Types.XmlStringMaxLen255
    -- ^ The policy adjustment type. The valid values are @ChangeInCapacity@ , @ExactCapacity@ , and @PercentChangeInCapacity@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AdjustmentType' value with any optional fields omitted.
mkAdjustmentType
    :: AdjustmentType
mkAdjustmentType = AdjustmentType'{adjustmentType = Core.Nothing}

-- | The policy adjustment type. The valid values are @ChangeInCapacity@ , @ExactCapacity@ , and @PercentChangeInCapacity@ .
--
-- /Note:/ Consider using 'adjustmentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atAdjustmentType :: Lens.Lens' AdjustmentType (Core.Maybe Types.XmlStringMaxLen255)
atAdjustmentType = Lens.field @"adjustmentType"
{-# INLINEABLE atAdjustmentType #-}
{-# DEPRECATED adjustmentType "Use generic-lens or generic-optics with 'adjustmentType' instead"  #-}

instance Core.FromXML AdjustmentType where
        parseXML x = AdjustmentType' Core.<$> (x Core..@? "AdjustmentType")
