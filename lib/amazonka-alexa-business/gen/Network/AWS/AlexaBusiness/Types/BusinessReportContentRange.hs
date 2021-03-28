{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.BusinessReportContentRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AlexaBusiness.Types.BusinessReportContentRange
  ( BusinessReportContentRange (..)
  -- * Smart constructor
  , mkBusinessReportContentRange
  -- * Lenses
  , brcrInterval
  ) where

import qualified Network.AWS.AlexaBusiness.Types.BusinessReportInterval as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The content range of the report.
--
-- /See:/ 'mkBusinessReportContentRange' smart constructor.
newtype BusinessReportContentRange = BusinessReportContentRange'
  { interval :: Types.BusinessReportInterval
    -- ^ The interval of the content range.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'BusinessReportContentRange' value with any optional fields omitted.
mkBusinessReportContentRange
    :: Types.BusinessReportInterval -- ^ 'interval'
    -> BusinessReportContentRange
mkBusinessReportContentRange interval
  = BusinessReportContentRange'{interval}

-- | The interval of the content range.
--
-- /Note:/ Consider using 'interval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brcrInterval :: Lens.Lens' BusinessReportContentRange Types.BusinessReportInterval
brcrInterval = Lens.field @"interval"
{-# INLINEABLE brcrInterval #-}
{-# DEPRECATED interval "Use generic-lens or generic-optics with 'interval' instead"  #-}

instance Core.FromJSON BusinessReportContentRange where
        toJSON BusinessReportContentRange{..}
          = Core.object
              (Core.catMaybes [Core.Just ("Interval" Core..= interval)])

instance Core.FromJSON BusinessReportContentRange where
        parseJSON
          = Core.withObject "BusinessReportContentRange" Core.$
              \ x -> BusinessReportContentRange' Core.<$> (x Core..: "Interval")
