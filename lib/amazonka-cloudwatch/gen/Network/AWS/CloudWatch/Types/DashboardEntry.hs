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
    deSize,
    deDashboardName,
    deLastModified,
    deDashboardARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a specific dashboard.
--
-- /See:/ 'mkDashboardEntry' smart constructor.
data DashboardEntry = DashboardEntry'
  { -- | The size of the dashboard, in bytes.
    size :: Lude.Maybe Lude.Integer,
    -- | The name of the dashboard.
    dashboardName :: Lude.Maybe Lude.Text,
    -- | The time stamp of when the dashboard was last modified, either by an API call or through the console. This number is expressed as the number of milliseconds since Jan 1, 1970 00:00:00 UTC.
    lastModified :: Lude.Maybe Lude.DateTime,
    -- | The Amazon Resource Name (ARN) of the dashboard.
    dashboardARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DashboardEntry' with the minimum fields required to make a request.
--
-- * 'size' - The size of the dashboard, in bytes.
-- * 'dashboardName' - The name of the dashboard.
-- * 'lastModified' - The time stamp of when the dashboard was last modified, either by an API call or through the console. This number is expressed as the number of milliseconds since Jan 1, 1970 00:00:00 UTC.
-- * 'dashboardARN' - The Amazon Resource Name (ARN) of the dashboard.
mkDashboardEntry ::
  DashboardEntry
mkDashboardEntry =
  DashboardEntry'
    { size = Lude.Nothing,
      dashboardName = Lude.Nothing,
      lastModified = Lude.Nothing,
      dashboardARN = Lude.Nothing
    }

-- | The size of the dashboard, in bytes.
--
-- /Note:/ Consider using 'size' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deSize :: Lens.Lens' DashboardEntry (Lude.Maybe Lude.Integer)
deSize = Lens.lens (size :: DashboardEntry -> Lude.Maybe Lude.Integer) (\s a -> s {size = a} :: DashboardEntry)
{-# DEPRECATED deSize "Use generic-lens or generic-optics with 'size' instead." #-}

-- | The name of the dashboard.
--
-- /Note:/ Consider using 'dashboardName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deDashboardName :: Lens.Lens' DashboardEntry (Lude.Maybe Lude.Text)
deDashboardName = Lens.lens (dashboardName :: DashboardEntry -> Lude.Maybe Lude.Text) (\s a -> s {dashboardName = a} :: DashboardEntry)
{-# DEPRECATED deDashboardName "Use generic-lens or generic-optics with 'dashboardName' instead." #-}

-- | The time stamp of when the dashboard was last modified, either by an API call or through the console. This number is expressed as the number of milliseconds since Jan 1, 1970 00:00:00 UTC.
--
-- /Note:/ Consider using 'lastModified' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deLastModified :: Lens.Lens' DashboardEntry (Lude.Maybe Lude.DateTime)
deLastModified = Lens.lens (lastModified :: DashboardEntry -> Lude.Maybe Lude.DateTime) (\s a -> s {lastModified = a} :: DashboardEntry)
{-# DEPRECATED deLastModified "Use generic-lens or generic-optics with 'lastModified' instead." #-}

-- | The Amazon Resource Name (ARN) of the dashboard.
--
-- /Note:/ Consider using 'dashboardARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deDashboardARN :: Lens.Lens' DashboardEntry (Lude.Maybe Lude.Text)
deDashboardARN = Lens.lens (dashboardARN :: DashboardEntry -> Lude.Maybe Lude.Text) (\s a -> s {dashboardARN = a} :: DashboardEntry)
{-# DEPRECATED deDashboardARN "Use generic-lens or generic-optics with 'dashboardARN' instead." #-}

instance Lude.FromXML DashboardEntry where
  parseXML x =
    DashboardEntry'
      Lude.<$> (x Lude..@? "Size")
      Lude.<*> (x Lude..@? "DashboardName")
      Lude.<*> (x Lude..@? "LastModified")
      Lude.<*> (x Lude..@? "DashboardArn")
