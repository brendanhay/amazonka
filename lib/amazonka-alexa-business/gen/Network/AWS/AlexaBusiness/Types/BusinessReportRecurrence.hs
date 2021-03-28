{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.BusinessReportRecurrence
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AlexaBusiness.Types.BusinessReportRecurrence
  ( BusinessReportRecurrence (..)
  -- * Smart constructor
  , mkBusinessReportRecurrence
  -- * Lenses
  , brrStartDate
  ) where

import qualified Network.AWS.AlexaBusiness.Types.StartDate as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The recurrence of the reports.
--
-- /See:/ 'mkBusinessReportRecurrence' smart constructor.
newtype BusinessReportRecurrence = BusinessReportRecurrence'
  { startDate :: Core.Maybe Types.StartDate
    -- ^ The start date.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'BusinessReportRecurrence' value with any optional fields omitted.
mkBusinessReportRecurrence
    :: BusinessReportRecurrence
mkBusinessReportRecurrence
  = BusinessReportRecurrence'{startDate = Core.Nothing}

-- | The start date.
--
-- /Note:/ Consider using 'startDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brrStartDate :: Lens.Lens' BusinessReportRecurrence (Core.Maybe Types.StartDate)
brrStartDate = Lens.field @"startDate"
{-# INLINEABLE brrStartDate #-}
{-# DEPRECATED startDate "Use generic-lens or generic-optics with 'startDate' instead"  #-}

instance Core.FromJSON BusinessReportRecurrence where
        toJSON BusinessReportRecurrence{..}
          = Core.object
              (Core.catMaybes [("StartDate" Core..=) Core.<$> startDate])

instance Core.FromJSON BusinessReportRecurrence where
        parseJSON
          = Core.withObject "BusinessReportRecurrence" Core.$
              \ x -> BusinessReportRecurrence' Core.<$> (x Core..:? "StartDate")
