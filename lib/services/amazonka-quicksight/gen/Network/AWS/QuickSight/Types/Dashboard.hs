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
-- Module      : Network.AWS.QuickSight.Types.Dashboard
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.QuickSight.Types.Dashboard where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.QuickSight.Types.DashboardVersion

-- | Dashboard.
--
-- /See:/ 'newDashboard' smart constructor.
data Dashboard = Dashboard'
  { -- | The last time that this dataset was updated.
    lastUpdatedTime :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the resource.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time that this dataset was created.
    createdTime :: Prelude.Maybe Core.POSIX,
    -- | Dashboard ID.
    dashboardId :: Prelude.Maybe Prelude.Text,
    -- | The last time that this dataset was published.
    lastPublishedTime :: Prelude.Maybe Core.POSIX,
    -- | A display name for the dashboard.
    name :: Prelude.Maybe Prelude.Text,
    -- | Version.
    version :: Prelude.Maybe DashboardVersion
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Dashboard' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastUpdatedTime', 'dashboard_lastUpdatedTime' - The last time that this dataset was updated.
--
-- 'arn', 'dashboard_arn' - The Amazon Resource Name (ARN) of the resource.
--
-- 'createdTime', 'dashboard_createdTime' - The time that this dataset was created.
--
-- 'dashboardId', 'dashboard_dashboardId' - Dashboard ID.
--
-- 'lastPublishedTime', 'dashboard_lastPublishedTime' - The last time that this dataset was published.
--
-- 'name', 'dashboard_name' - A display name for the dashboard.
--
-- 'version', 'dashboard_version' - Version.
newDashboard ::
  Dashboard
newDashboard =
  Dashboard'
    { lastUpdatedTime = Prelude.Nothing,
      arn = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      dashboardId = Prelude.Nothing,
      lastPublishedTime = Prelude.Nothing,
      name = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | The last time that this dataset was updated.
dashboard_lastUpdatedTime :: Lens.Lens' Dashboard (Prelude.Maybe Prelude.UTCTime)
dashboard_lastUpdatedTime = Lens.lens (\Dashboard' {lastUpdatedTime} -> lastUpdatedTime) (\s@Dashboard' {} a -> s {lastUpdatedTime = a} :: Dashboard) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the resource.
dashboard_arn :: Lens.Lens' Dashboard (Prelude.Maybe Prelude.Text)
dashboard_arn = Lens.lens (\Dashboard' {arn} -> arn) (\s@Dashboard' {} a -> s {arn = a} :: Dashboard)

-- | The time that this dataset was created.
dashboard_createdTime :: Lens.Lens' Dashboard (Prelude.Maybe Prelude.UTCTime)
dashboard_createdTime = Lens.lens (\Dashboard' {createdTime} -> createdTime) (\s@Dashboard' {} a -> s {createdTime = a} :: Dashboard) Prelude.. Lens.mapping Core._Time

-- | Dashboard ID.
dashboard_dashboardId :: Lens.Lens' Dashboard (Prelude.Maybe Prelude.Text)
dashboard_dashboardId = Lens.lens (\Dashboard' {dashboardId} -> dashboardId) (\s@Dashboard' {} a -> s {dashboardId = a} :: Dashboard)

-- | The last time that this dataset was published.
dashboard_lastPublishedTime :: Lens.Lens' Dashboard (Prelude.Maybe Prelude.UTCTime)
dashboard_lastPublishedTime = Lens.lens (\Dashboard' {lastPublishedTime} -> lastPublishedTime) (\s@Dashboard' {} a -> s {lastPublishedTime = a} :: Dashboard) Prelude.. Lens.mapping Core._Time

-- | A display name for the dashboard.
dashboard_name :: Lens.Lens' Dashboard (Prelude.Maybe Prelude.Text)
dashboard_name = Lens.lens (\Dashboard' {name} -> name) (\s@Dashboard' {} a -> s {name = a} :: Dashboard)

-- | Version.
dashboard_version :: Lens.Lens' Dashboard (Prelude.Maybe DashboardVersion)
dashboard_version = Lens.lens (\Dashboard' {version} -> version) (\s@Dashboard' {} a -> s {version = a} :: Dashboard)

instance Core.FromJSON Dashboard where
  parseJSON =
    Core.withObject
      "Dashboard"
      ( \x ->
          Dashboard'
            Prelude.<$> (x Core..:? "LastUpdatedTime")
            Prelude.<*> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "CreatedTime")
            Prelude.<*> (x Core..:? "DashboardId")
            Prelude.<*> (x Core..:? "LastPublishedTime")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Version")
      )

instance Prelude.Hashable Dashboard

instance Prelude.NFData Dashboard
