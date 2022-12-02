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
-- Module      : Amazonka.QuickSight.Types.Dashboard
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.Dashboard where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DashboardVersion

-- | Dashboard.
--
-- /See:/ 'newDashboard' smart constructor.
data Dashboard = Dashboard'
  { -- | A display name for the dashboard.
    name :: Prelude.Maybe Prelude.Text,
    -- | The time that this dataset was created.
    createdTime :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) of the resource.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The last time that this dataset was updated.
    lastUpdatedTime :: Prelude.Maybe Data.POSIX,
    -- | Dashboard ID.
    dashboardId :: Prelude.Maybe Prelude.Text,
    -- | Version.
    version :: Prelude.Maybe DashboardVersion,
    -- | The last time that this dataset was published.
    lastPublishedTime :: Prelude.Maybe Data.POSIX
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
-- 'name', 'dashboard_name' - A display name for the dashboard.
--
-- 'createdTime', 'dashboard_createdTime' - The time that this dataset was created.
--
-- 'arn', 'dashboard_arn' - The Amazon Resource Name (ARN) of the resource.
--
-- 'lastUpdatedTime', 'dashboard_lastUpdatedTime' - The last time that this dataset was updated.
--
-- 'dashboardId', 'dashboard_dashboardId' - Dashboard ID.
--
-- 'version', 'dashboard_version' - Version.
--
-- 'lastPublishedTime', 'dashboard_lastPublishedTime' - The last time that this dataset was published.
newDashboard ::
  Dashboard
newDashboard =
  Dashboard'
    { name = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      arn = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      dashboardId = Prelude.Nothing,
      version = Prelude.Nothing,
      lastPublishedTime = Prelude.Nothing
    }

-- | A display name for the dashboard.
dashboard_name :: Lens.Lens' Dashboard (Prelude.Maybe Prelude.Text)
dashboard_name = Lens.lens (\Dashboard' {name} -> name) (\s@Dashboard' {} a -> s {name = a} :: Dashboard)

-- | The time that this dataset was created.
dashboard_createdTime :: Lens.Lens' Dashboard (Prelude.Maybe Prelude.UTCTime)
dashboard_createdTime = Lens.lens (\Dashboard' {createdTime} -> createdTime) (\s@Dashboard' {} a -> s {createdTime = a} :: Dashboard) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of the resource.
dashboard_arn :: Lens.Lens' Dashboard (Prelude.Maybe Prelude.Text)
dashboard_arn = Lens.lens (\Dashboard' {arn} -> arn) (\s@Dashboard' {} a -> s {arn = a} :: Dashboard)

-- | The last time that this dataset was updated.
dashboard_lastUpdatedTime :: Lens.Lens' Dashboard (Prelude.Maybe Prelude.UTCTime)
dashboard_lastUpdatedTime = Lens.lens (\Dashboard' {lastUpdatedTime} -> lastUpdatedTime) (\s@Dashboard' {} a -> s {lastUpdatedTime = a} :: Dashboard) Prelude.. Lens.mapping Data._Time

-- | Dashboard ID.
dashboard_dashboardId :: Lens.Lens' Dashboard (Prelude.Maybe Prelude.Text)
dashboard_dashboardId = Lens.lens (\Dashboard' {dashboardId} -> dashboardId) (\s@Dashboard' {} a -> s {dashboardId = a} :: Dashboard)

-- | Version.
dashboard_version :: Lens.Lens' Dashboard (Prelude.Maybe DashboardVersion)
dashboard_version = Lens.lens (\Dashboard' {version} -> version) (\s@Dashboard' {} a -> s {version = a} :: Dashboard)

-- | The last time that this dataset was published.
dashboard_lastPublishedTime :: Lens.Lens' Dashboard (Prelude.Maybe Prelude.UTCTime)
dashboard_lastPublishedTime = Lens.lens (\Dashboard' {lastPublishedTime} -> lastPublishedTime) (\s@Dashboard' {} a -> s {lastPublishedTime = a} :: Dashboard) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON Dashboard where
  parseJSON =
    Data.withObject
      "Dashboard"
      ( \x ->
          Dashboard'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "CreatedTime")
            Prelude.<*> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "LastUpdatedTime")
            Prelude.<*> (x Data..:? "DashboardId")
            Prelude.<*> (x Data..:? "Version")
            Prelude.<*> (x Data..:? "LastPublishedTime")
      )

instance Prelude.Hashable Dashboard where
  hashWithSalt _salt Dashboard' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` dashboardId
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` lastPublishedTime

instance Prelude.NFData Dashboard where
  rnf Dashboard' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf dashboardId
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf lastPublishedTime
