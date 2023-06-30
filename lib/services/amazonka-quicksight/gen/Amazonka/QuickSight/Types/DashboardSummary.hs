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
-- Module      : Amazonka.QuickSight.Types.DashboardSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DashboardSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Dashboard summary.
--
-- /See:/ 'newDashboardSummary' smart constructor.
data DashboardSummary = DashboardSummary'
  { -- | The Amazon Resource Name (ARN) of the resource.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time that this dashboard was created.
    createdTime :: Prelude.Maybe Data.POSIX,
    -- | Dashboard ID.
    dashboardId :: Prelude.Maybe Prelude.Text,
    -- | The last time that this dashboard was published.
    lastPublishedTime :: Prelude.Maybe Data.POSIX,
    -- | The last time that this dashboard was updated.
    lastUpdatedTime :: Prelude.Maybe Data.POSIX,
    -- | A display name for the dashboard.
    name :: Prelude.Maybe Prelude.Text,
    -- | Published version number.
    publishedVersionNumber :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DashboardSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'dashboardSummary_arn' - The Amazon Resource Name (ARN) of the resource.
--
-- 'createdTime', 'dashboardSummary_createdTime' - The time that this dashboard was created.
--
-- 'dashboardId', 'dashboardSummary_dashboardId' - Dashboard ID.
--
-- 'lastPublishedTime', 'dashboardSummary_lastPublishedTime' - The last time that this dashboard was published.
--
-- 'lastUpdatedTime', 'dashboardSummary_lastUpdatedTime' - The last time that this dashboard was updated.
--
-- 'name', 'dashboardSummary_name' - A display name for the dashboard.
--
-- 'publishedVersionNumber', 'dashboardSummary_publishedVersionNumber' - Published version number.
newDashboardSummary ::
  DashboardSummary
newDashboardSummary =
  DashboardSummary'
    { arn = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      dashboardId = Prelude.Nothing,
      lastPublishedTime = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      name = Prelude.Nothing,
      publishedVersionNumber = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the resource.
dashboardSummary_arn :: Lens.Lens' DashboardSummary (Prelude.Maybe Prelude.Text)
dashboardSummary_arn = Lens.lens (\DashboardSummary' {arn} -> arn) (\s@DashboardSummary' {} a -> s {arn = a} :: DashboardSummary)

-- | The time that this dashboard was created.
dashboardSummary_createdTime :: Lens.Lens' DashboardSummary (Prelude.Maybe Prelude.UTCTime)
dashboardSummary_createdTime = Lens.lens (\DashboardSummary' {createdTime} -> createdTime) (\s@DashboardSummary' {} a -> s {createdTime = a} :: DashboardSummary) Prelude.. Lens.mapping Data._Time

-- | Dashboard ID.
dashboardSummary_dashboardId :: Lens.Lens' DashboardSummary (Prelude.Maybe Prelude.Text)
dashboardSummary_dashboardId = Lens.lens (\DashboardSummary' {dashboardId} -> dashboardId) (\s@DashboardSummary' {} a -> s {dashboardId = a} :: DashboardSummary)

-- | The last time that this dashboard was published.
dashboardSummary_lastPublishedTime :: Lens.Lens' DashboardSummary (Prelude.Maybe Prelude.UTCTime)
dashboardSummary_lastPublishedTime = Lens.lens (\DashboardSummary' {lastPublishedTime} -> lastPublishedTime) (\s@DashboardSummary' {} a -> s {lastPublishedTime = a} :: DashboardSummary) Prelude.. Lens.mapping Data._Time

-- | The last time that this dashboard was updated.
dashboardSummary_lastUpdatedTime :: Lens.Lens' DashboardSummary (Prelude.Maybe Prelude.UTCTime)
dashboardSummary_lastUpdatedTime = Lens.lens (\DashboardSummary' {lastUpdatedTime} -> lastUpdatedTime) (\s@DashboardSummary' {} a -> s {lastUpdatedTime = a} :: DashboardSummary) Prelude.. Lens.mapping Data._Time

-- | A display name for the dashboard.
dashboardSummary_name :: Lens.Lens' DashboardSummary (Prelude.Maybe Prelude.Text)
dashboardSummary_name = Lens.lens (\DashboardSummary' {name} -> name) (\s@DashboardSummary' {} a -> s {name = a} :: DashboardSummary)

-- | Published version number.
dashboardSummary_publishedVersionNumber :: Lens.Lens' DashboardSummary (Prelude.Maybe Prelude.Natural)
dashboardSummary_publishedVersionNumber = Lens.lens (\DashboardSummary' {publishedVersionNumber} -> publishedVersionNumber) (\s@DashboardSummary' {} a -> s {publishedVersionNumber = a} :: DashboardSummary)

instance Data.FromJSON DashboardSummary where
  parseJSON =
    Data.withObject
      "DashboardSummary"
      ( \x ->
          DashboardSummary'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "CreatedTime")
            Prelude.<*> (x Data..:? "DashboardId")
            Prelude.<*> (x Data..:? "LastPublishedTime")
            Prelude.<*> (x Data..:? "LastUpdatedTime")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "PublishedVersionNumber")
      )

instance Prelude.Hashable DashboardSummary where
  hashWithSalt _salt DashboardSummary' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` dashboardId
      `Prelude.hashWithSalt` lastPublishedTime
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` publishedVersionNumber

instance Prelude.NFData DashboardSummary where
  rnf DashboardSummary' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf dashboardId
      `Prelude.seq` Prelude.rnf lastPublishedTime
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf publishedVersionNumber
