{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.TimeRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Shield.Types.TimeRange
  ( TimeRange (..)
  -- * Smart constructor
  , mkTimeRange
  -- * Lenses
  , trFromInclusive
  , trToExclusive
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The time range. 
--
-- /See:/ 'mkTimeRange' smart constructor.
data TimeRange = TimeRange'
  { fromInclusive :: Core.Maybe Core.NominalDiffTime
    -- ^ The start time, in Unix time in seconds. For more information see <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp> .
  , toExclusive :: Core.Maybe Core.NominalDiffTime
    -- ^ The end time, in Unix time in seconds. For more information see <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'TimeRange' value with any optional fields omitted.
mkTimeRange
    :: TimeRange
mkTimeRange
  = TimeRange'{fromInclusive = Core.Nothing,
               toExclusive = Core.Nothing}

-- | The start time, in Unix time in seconds. For more information see <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp> .
--
-- /Note:/ Consider using 'fromInclusive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trFromInclusive :: Lens.Lens' TimeRange (Core.Maybe Core.NominalDiffTime)
trFromInclusive = Lens.field @"fromInclusive"
{-# INLINEABLE trFromInclusive #-}
{-# DEPRECATED fromInclusive "Use generic-lens or generic-optics with 'fromInclusive' instead"  #-}

-- | The end time, in Unix time in seconds. For more information see <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp> .
--
-- /Note:/ Consider using 'toExclusive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trToExclusive :: Lens.Lens' TimeRange (Core.Maybe Core.NominalDiffTime)
trToExclusive = Lens.field @"toExclusive"
{-# INLINEABLE trToExclusive #-}
{-# DEPRECATED toExclusive "Use generic-lens or generic-optics with 'toExclusive' instead"  #-}

instance Core.FromJSON TimeRange where
        toJSON TimeRange{..}
          = Core.object
              (Core.catMaybes
                 [("FromInclusive" Core..=) Core.<$> fromInclusive,
                  ("ToExclusive" Core..=) Core.<$> toExclusive])

instance Core.FromJSON TimeRange where
        parseJSON
          = Core.withObject "TimeRange" Core.$
              \ x ->
                TimeRange' Core.<$>
                  (x Core..:? "FromInclusive") Core.<*> x Core..:? "ToExclusive"
