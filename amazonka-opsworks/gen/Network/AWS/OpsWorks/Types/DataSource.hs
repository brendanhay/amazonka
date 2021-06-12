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
-- Module      : Network.AWS.OpsWorks.Types.DataSource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.DataSource where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes an app\'s data source.
--
-- /See:/ 'newDataSource' smart constructor.
data DataSource = DataSource'
  { -- | The data source\'s ARN.
    arn :: Core.Maybe Core.Text,
    -- | The data source\'s type, @AutoSelectOpsworksMysqlInstance@,
    -- @OpsworksMysqlInstance@, @RdsDbInstance@, or @None@.
    type' :: Core.Maybe Core.Text,
    -- | The database name.
    databaseName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DataSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'dataSource_arn' - The data source\'s ARN.
--
-- 'type'', 'dataSource_type' - The data source\'s type, @AutoSelectOpsworksMysqlInstance@,
-- @OpsworksMysqlInstance@, @RdsDbInstance@, or @None@.
--
-- 'databaseName', 'dataSource_databaseName' - The database name.
newDataSource ::
  DataSource
newDataSource =
  DataSource'
    { arn = Core.Nothing,
      type' = Core.Nothing,
      databaseName = Core.Nothing
    }

-- | The data source\'s ARN.
dataSource_arn :: Lens.Lens' DataSource (Core.Maybe Core.Text)
dataSource_arn = Lens.lens (\DataSource' {arn} -> arn) (\s@DataSource' {} a -> s {arn = a} :: DataSource)

-- | The data source\'s type, @AutoSelectOpsworksMysqlInstance@,
-- @OpsworksMysqlInstance@, @RdsDbInstance@, or @None@.
dataSource_type :: Lens.Lens' DataSource (Core.Maybe Core.Text)
dataSource_type = Lens.lens (\DataSource' {type'} -> type') (\s@DataSource' {} a -> s {type' = a} :: DataSource)

-- | The database name.
dataSource_databaseName :: Lens.Lens' DataSource (Core.Maybe Core.Text)
dataSource_databaseName = Lens.lens (\DataSource' {databaseName} -> databaseName) (\s@DataSource' {} a -> s {databaseName = a} :: DataSource)

instance Core.FromJSON DataSource where
  parseJSON =
    Core.withObject
      "DataSource"
      ( \x ->
          DataSource'
            Core.<$> (x Core..:? "Arn")
            Core.<*> (x Core..:? "Type")
            Core.<*> (x Core..:? "DatabaseName")
      )

instance Core.Hashable DataSource

instance Core.NFData DataSource

instance Core.ToJSON DataSource where
  toJSON DataSource' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Arn" Core..=) Core.<$> arn,
            ("Type" Core..=) Core.<$> type',
            ("DatabaseName" Core..=) Core.<$> databaseName
          ]
      )
