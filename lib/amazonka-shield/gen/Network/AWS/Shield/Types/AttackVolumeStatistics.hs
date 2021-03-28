{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.AttackVolumeStatistics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Shield.Types.AttackVolumeStatistics
  ( AttackVolumeStatistics (..)
  -- * Smart constructor
  , mkAttackVolumeStatistics
  -- * Lenses
  , avsMax
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Statistics objects for the various data types in 'AttackVolume' . 
--
-- /See:/ 'mkAttackVolumeStatistics' smart constructor.
newtype AttackVolumeStatistics = AttackVolumeStatistics'
  { max :: Core.Double
    -- ^ The maximum attack volume observed for the given unit.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AttackVolumeStatistics' value with any optional fields omitted.
mkAttackVolumeStatistics
    :: Core.Double -- ^ 'max'
    -> AttackVolumeStatistics
mkAttackVolumeStatistics max = AttackVolumeStatistics'{max}

-- | The maximum attack volume observed for the given unit.
--
-- /Note:/ Consider using 'max' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avsMax :: Lens.Lens' AttackVolumeStatistics Core.Double
avsMax = Lens.field @"max"
{-# INLINEABLE avsMax #-}
{-# DEPRECATED max "Use generic-lens or generic-optics with 'max' instead"  #-}

instance Core.FromJSON AttackVolumeStatistics where
        parseJSON
          = Core.withObject "AttackVolumeStatistics" Core.$
              \ x -> AttackVolumeStatistics' Core.<$> (x Core..: "Max")
