{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.StatusReport
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53.Types.StatusReport
  ( StatusReport (..)
  -- * Smart constructor
  , mkStatusReport
  -- * Lenses
  , srCheckedTime
  , srStatus
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Route53.Internal as Types
import qualified Network.AWS.Route53.Types.Status as Types

-- | A complex type that contains the status that one Amazon Route 53 health checker reports and the time of the health check.
--
-- /See:/ 'mkStatusReport' smart constructor.
data StatusReport = StatusReport'
  { checkedTime :: Core.Maybe Core.UTCTime
    -- ^ The date and time that the health checker performed the health check in <https://en.wikipedia.org/wiki/ISO_8601 ISO 8601 format> and Coordinated Universal Time (UTC). For example, the value @2017-03-27T17:48:16.751Z@ represents March 27, 2017 at 17:48:16.751 UTC.
  , status :: Core.Maybe Types.Status
    -- ^ A description of the status of the health check endpoint as reported by one of the Amazon Route 53 health checkers.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'StatusReport' value with any optional fields omitted.
mkStatusReport
    :: StatusReport
mkStatusReport
  = StatusReport'{checkedTime = Core.Nothing, status = Core.Nothing}

-- | The date and time that the health checker performed the health check in <https://en.wikipedia.org/wiki/ISO_8601 ISO 8601 format> and Coordinated Universal Time (UTC). For example, the value @2017-03-27T17:48:16.751Z@ represents March 27, 2017 at 17:48:16.751 UTC.
--
-- /Note:/ Consider using 'checkedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srCheckedTime :: Lens.Lens' StatusReport (Core.Maybe Core.UTCTime)
srCheckedTime = Lens.field @"checkedTime"
{-# INLINEABLE srCheckedTime #-}
{-# DEPRECATED checkedTime "Use generic-lens or generic-optics with 'checkedTime' instead"  #-}

-- | A description of the status of the health check endpoint as reported by one of the Amazon Route 53 health checkers.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srStatus :: Lens.Lens' StatusReport (Core.Maybe Types.Status)
srStatus = Lens.field @"status"
{-# INLINEABLE srStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.FromXML StatusReport where
        parseXML x
          = StatusReport' Core.<$>
              (x Core..@? "CheckedTime") Core.<*> x Core..@? "Status"
