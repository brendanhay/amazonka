{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.FaultStatistics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.FaultStatistics
  ( FaultStatistics (..),

    -- * Smart constructor
    mkFaultStatistics,

    -- * Lenses
    fsOtherCount,
    fsTotalCount,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about requests that failed with a 5xx Server Error status code.
--
-- /See:/ 'mkFaultStatistics' smart constructor.
data FaultStatistics = FaultStatistics'
  { -- | The number of requests that failed with untracked 5xx Server Error status codes.
    otherCount :: Core.Maybe Core.Integer,
    -- | The total number of requests that failed with a 5xx Server Error status code.
    totalCount :: Core.Maybe Core.Integer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FaultStatistics' value with any optional fields omitted.
mkFaultStatistics ::
  FaultStatistics
mkFaultStatistics =
  FaultStatistics'
    { otherCount = Core.Nothing,
      totalCount = Core.Nothing
    }

-- | The number of requests that failed with untracked 5xx Server Error status codes.
--
-- /Note:/ Consider using 'otherCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsOtherCount :: Lens.Lens' FaultStatistics (Core.Maybe Core.Integer)
fsOtherCount = Lens.field @"otherCount"
{-# DEPRECATED fsOtherCount "Use generic-lens or generic-optics with 'otherCount' instead." #-}

-- | The total number of requests that failed with a 5xx Server Error status code.
--
-- /Note:/ Consider using 'totalCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsTotalCount :: Lens.Lens' FaultStatistics (Core.Maybe Core.Integer)
fsTotalCount = Lens.field @"totalCount"
{-# DEPRECATED fsTotalCount "Use generic-lens or generic-optics with 'totalCount' instead." #-}

instance Core.FromJSON FaultStatistics where
  parseJSON =
    Core.withObject "FaultStatistics" Core.$
      \x ->
        FaultStatistics'
          Core.<$> (x Core..:? "OtherCount") Core.<*> (x Core..:? "TotalCount")
