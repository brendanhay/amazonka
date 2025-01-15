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
-- Module      : Amazonka.CloudWatch.Types.DashboardEntry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatch.Types.DashboardEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents a specific dashboard.
--
-- /See:/ 'newDashboardEntry' smart constructor.
data DashboardEntry = DashboardEntry'
  { -- | The Amazon Resource Name (ARN) of the dashboard.
    dashboardArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the dashboard.
    dashboardName :: Prelude.Maybe Prelude.Text,
    -- | The time stamp of when the dashboard was last modified, either by an API
    -- call or through the console. This number is expressed as the number of
    -- milliseconds since Jan 1, 1970 00:00:00 UTC.
    lastModified :: Prelude.Maybe Data.ISO8601,
    -- | The size of the dashboard, in bytes.
    size :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'dashboardName', 'dashboardEntry_dashboardName' - The name of the dashboard.
--
-- 'lastModified', 'dashboardEntry_lastModified' - The time stamp of when the dashboard was last modified, either by an API
-- call or through the console. This number is expressed as the number of
-- milliseconds since Jan 1, 1970 00:00:00 UTC.
--
-- 'size', 'dashboardEntry_size' - The size of the dashboard, in bytes.
newDashboardEntry ::
  DashboardEntry
newDashboardEntry =
  DashboardEntry'
    { dashboardArn = Prelude.Nothing,
      dashboardName = Prelude.Nothing,
      lastModified = Prelude.Nothing,
      size = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the dashboard.
dashboardEntry_dashboardArn :: Lens.Lens' DashboardEntry (Prelude.Maybe Prelude.Text)
dashboardEntry_dashboardArn = Lens.lens (\DashboardEntry' {dashboardArn} -> dashboardArn) (\s@DashboardEntry' {} a -> s {dashboardArn = a} :: DashboardEntry)

-- | The name of the dashboard.
dashboardEntry_dashboardName :: Lens.Lens' DashboardEntry (Prelude.Maybe Prelude.Text)
dashboardEntry_dashboardName = Lens.lens (\DashboardEntry' {dashboardName} -> dashboardName) (\s@DashboardEntry' {} a -> s {dashboardName = a} :: DashboardEntry)

-- | The time stamp of when the dashboard was last modified, either by an API
-- call or through the console. This number is expressed as the number of
-- milliseconds since Jan 1, 1970 00:00:00 UTC.
dashboardEntry_lastModified :: Lens.Lens' DashboardEntry (Prelude.Maybe Prelude.UTCTime)
dashboardEntry_lastModified = Lens.lens (\DashboardEntry' {lastModified} -> lastModified) (\s@DashboardEntry' {} a -> s {lastModified = a} :: DashboardEntry) Prelude.. Lens.mapping Data._Time

-- | The size of the dashboard, in bytes.
dashboardEntry_size :: Lens.Lens' DashboardEntry (Prelude.Maybe Prelude.Integer)
dashboardEntry_size = Lens.lens (\DashboardEntry' {size} -> size) (\s@DashboardEntry' {} a -> s {size = a} :: DashboardEntry)

instance Data.FromXML DashboardEntry where
  parseXML x =
    DashboardEntry'
      Prelude.<$> (x Data..@? "DashboardArn")
      Prelude.<*> (x Data..@? "DashboardName")
      Prelude.<*> (x Data..@? "LastModified")
      Prelude.<*> (x Data..@? "Size")

instance Prelude.Hashable DashboardEntry where
  hashWithSalt _salt DashboardEntry' {..} =
    _salt
      `Prelude.hashWithSalt` dashboardArn
      `Prelude.hashWithSalt` dashboardName
      `Prelude.hashWithSalt` lastModified
      `Prelude.hashWithSalt` size

instance Prelude.NFData DashboardEntry where
  rnf DashboardEntry' {..} =
    Prelude.rnf dashboardArn `Prelude.seq`
      Prelude.rnf dashboardName `Prelude.seq`
        Prelude.rnf lastModified `Prelude.seq`
          Prelude.rnf size
