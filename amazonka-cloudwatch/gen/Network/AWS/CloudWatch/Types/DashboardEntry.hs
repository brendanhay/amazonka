{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.DashboardEntry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.DashboardEntry where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents a specific dashboard.
--
-- /See:/ 'newDashboardEntry' smart constructor.
data DashboardEntry = DashboardEntry'
  { -- | The Amazon Resource Name (ARN) of the dashboard.
    dashboardArn :: Core.Maybe Core.Text,
    -- | The time stamp of when the dashboard was last modified, either by an API
    -- call or through the console. This number is expressed as the number of
    -- milliseconds since Jan 1, 1970 00:00:00 UTC.
    lastModified :: Core.Maybe Core.ISO8601,
    -- | The name of the dashboard.
    dashboardName :: Core.Maybe Core.Text,
    -- | The size of the dashboard, in bytes.
    size :: Core.Maybe Core.Integer
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DashboardEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dashboardArn', 'dashboardEntry_dashboardArn' - The Amazon Resource Name (ARN) of the dashboard.
--
-- 'lastModified', 'dashboardEntry_lastModified' - The time stamp of when the dashboard was last modified, either by an API
-- call or through the console. This number is expressed as the number of
-- milliseconds since Jan 1, 1970 00:00:00 UTC.
--
-- 'dashboardName', 'dashboardEntry_dashboardName' - The name of the dashboard.
--
-- 'size', 'dashboardEntry_size' - The size of the dashboard, in bytes.
newDashboardEntry ::
  DashboardEntry
newDashboardEntry =
  DashboardEntry'
    { dashboardArn = Core.Nothing,
      lastModified = Core.Nothing,
      dashboardName = Core.Nothing,
      size = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the dashboard.
dashboardEntry_dashboardArn :: Lens.Lens' DashboardEntry (Core.Maybe Core.Text)
dashboardEntry_dashboardArn = Lens.lens (\DashboardEntry' {dashboardArn} -> dashboardArn) (\s@DashboardEntry' {} a -> s {dashboardArn = a} :: DashboardEntry)

-- | The time stamp of when the dashboard was last modified, either by an API
-- call or through the console. This number is expressed as the number of
-- milliseconds since Jan 1, 1970 00:00:00 UTC.
dashboardEntry_lastModified :: Lens.Lens' DashboardEntry (Core.Maybe Core.UTCTime)
dashboardEntry_lastModified = Lens.lens (\DashboardEntry' {lastModified} -> lastModified) (\s@DashboardEntry' {} a -> s {lastModified = a} :: DashboardEntry) Core.. Lens.mapping Core._Time

-- | The name of the dashboard.
dashboardEntry_dashboardName :: Lens.Lens' DashboardEntry (Core.Maybe Core.Text)
dashboardEntry_dashboardName = Lens.lens (\DashboardEntry' {dashboardName} -> dashboardName) (\s@DashboardEntry' {} a -> s {dashboardName = a} :: DashboardEntry)

-- | The size of the dashboard, in bytes.
dashboardEntry_size :: Lens.Lens' DashboardEntry (Core.Maybe Core.Integer)
dashboardEntry_size = Lens.lens (\DashboardEntry' {size} -> size) (\s@DashboardEntry' {} a -> s {size = a} :: DashboardEntry)

instance Core.FromXML DashboardEntry where
  parseXML x =
    DashboardEntry'
      Core.<$> (x Core..@? "DashboardArn")
      Core.<*> (x Core..@? "LastModified")
      Core.<*> (x Core..@? "DashboardName")
      Core.<*> (x Core..@? "Size")

instance Core.Hashable DashboardEntry

instance Core.NFData DashboardEntry
