{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.ReportGroupTrendStats
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.ReportGroupTrendStats
  ( ReportGroupTrendStats (..),

    -- * Smart constructor
    mkReportGroupTrendStats,

    -- * Lenses
    rgtsAverage,
    rgtsMax,
    rgtsMin,
  )
where

import qualified Network.AWS.CodeBuild.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | /See:/ 'mkReportGroupTrendStats' smart constructor.
data ReportGroupTrendStats = ReportGroupTrendStats'
  { average :: Core.Maybe Types.String,
    max :: Core.Maybe Types.String,
    min :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReportGroupTrendStats' value with any optional fields omitted.
mkReportGroupTrendStats ::
  ReportGroupTrendStats
mkReportGroupTrendStats =
  ReportGroupTrendStats'
    { average = Core.Nothing,
      max = Core.Nothing,
      min = Core.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'average' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgtsAverage :: Lens.Lens' ReportGroupTrendStats (Core.Maybe Types.String)
rgtsAverage = Lens.field @"average"
{-# DEPRECATED rgtsAverage "Use generic-lens or generic-optics with 'average' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'max' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgtsMax :: Lens.Lens' ReportGroupTrendStats (Core.Maybe Types.String)
rgtsMax = Lens.field @"max"
{-# DEPRECATED rgtsMax "Use generic-lens or generic-optics with 'max' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'min' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgtsMin :: Lens.Lens' ReportGroupTrendStats (Core.Maybe Types.String)
rgtsMin = Lens.field @"min"
{-# DEPRECATED rgtsMin "Use generic-lens or generic-optics with 'min' instead." #-}

instance Core.FromJSON ReportGroupTrendStats where
  parseJSON =
    Core.withObject "ReportGroupTrendStats" Core.$
      \x ->
        ReportGroupTrendStats'
          Core.<$> (x Core..:? "average")
          Core.<*> (x Core..:? "max")
          Core.<*> (x Core..:? "min")
