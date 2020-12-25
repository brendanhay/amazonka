{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.DashboardEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.DashboardEntry
  ( DashboardEntry (..),

    -- * Smart constructor
    mkDashboardEntry,

    -- * Lenses
    deDashboardArn,
    deDashboardName,
    deLastModified,
    deSize,
  )
where

import qualified Network.AWS.CloudWatch.Types.DashboardArn as Types
import qualified Network.AWS.CloudWatch.Types.DashboardName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a specific dashboard.
--
-- /See:/ 'mkDashboardEntry' smart constructor.
data DashboardEntry = DashboardEntry'
  { -- | The Amazon Resource Name (ARN) of the dashboard.
    dashboardArn :: Core.Maybe Types.DashboardArn,
    -- | The name of the dashboard.
    dashboardName :: Core.Maybe Types.DashboardName,
    -- | The time stamp of when the dashboard was last modified, either by an API call or through the console. This number is expressed as the number of milliseconds since Jan 1, 1970 00:00:00 UTC.
    lastModified :: Core.Maybe Core.UTCTime,
    -- | The size of the dashboard, in bytes.
    size :: Core.Maybe Core.Integer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DashboardEntry' value with any optional fields omitted.
mkDashboardEntry ::
  DashboardEntry
mkDashboardEntry =
  DashboardEntry'
    { dashboardArn = Core.Nothing,
      dashboardName = Core.Nothing,
      lastModified = Core.Nothing,
      size = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the dashboard.
--
-- /Note:/ Consider using 'dashboardArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deDashboardArn :: Lens.Lens' DashboardEntry (Core.Maybe Types.DashboardArn)
deDashboardArn = Lens.field @"dashboardArn"
{-# DEPRECATED deDashboardArn "Use generic-lens or generic-optics with 'dashboardArn' instead." #-}

-- | The name of the dashboard.
--
-- /Note:/ Consider using 'dashboardName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deDashboardName :: Lens.Lens' DashboardEntry (Core.Maybe Types.DashboardName)
deDashboardName = Lens.field @"dashboardName"
{-# DEPRECATED deDashboardName "Use generic-lens or generic-optics with 'dashboardName' instead." #-}

-- | The time stamp of when the dashboard was last modified, either by an API call or through the console. This number is expressed as the number of milliseconds since Jan 1, 1970 00:00:00 UTC.
--
-- /Note:/ Consider using 'lastModified' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deLastModified :: Lens.Lens' DashboardEntry (Core.Maybe Core.UTCTime)
deLastModified = Lens.field @"lastModified"
{-# DEPRECATED deLastModified "Use generic-lens or generic-optics with 'lastModified' instead." #-}

-- | The size of the dashboard, in bytes.
--
-- /Note:/ Consider using 'size' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deSize :: Lens.Lens' DashboardEntry (Core.Maybe Core.Integer)
deSize = Lens.field @"size"
{-# DEPRECATED deSize "Use generic-lens or generic-optics with 'size' instead." #-}

instance Core.FromXML DashboardEntry where
  parseXML x =
    DashboardEntry'
      Core.<$> (x Core..@? "DashboardArn")
      Core.<*> (x Core..@? "DashboardName")
      Core.<*> (x Core..@? "LastModified")
      Core.<*> (x Core..@? "Size")
