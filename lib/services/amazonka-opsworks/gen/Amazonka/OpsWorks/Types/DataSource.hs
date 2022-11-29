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
-- Module      : Amazonka.OpsWorks.Types.DataSource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpsWorks.Types.DataSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes an app\'s data source.
--
-- /See:/ 'newDataSource' smart constructor.
data DataSource = DataSource'
  { -- | The data source\'s type, @AutoSelectOpsworksMysqlInstance@,
    -- @OpsworksMysqlInstance@, @RdsDbInstance@, or @None@.
    type' :: Prelude.Maybe Prelude.Text,
    -- | The database name.
    databaseName :: Prelude.Maybe Prelude.Text,
    -- | The data source\'s ARN.
    arn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'dataSource_type' - The data source\'s type, @AutoSelectOpsworksMysqlInstance@,
-- @OpsworksMysqlInstance@, @RdsDbInstance@, or @None@.
--
-- 'databaseName', 'dataSource_databaseName' - The database name.
--
-- 'arn', 'dataSource_arn' - The data source\'s ARN.
newDataSource ::
  DataSource
newDataSource =
  DataSource'
    { type' = Prelude.Nothing,
      databaseName = Prelude.Nothing,
      arn = Prelude.Nothing
    }

-- | The data source\'s type, @AutoSelectOpsworksMysqlInstance@,
-- @OpsworksMysqlInstance@, @RdsDbInstance@, or @None@.
dataSource_type :: Lens.Lens' DataSource (Prelude.Maybe Prelude.Text)
dataSource_type = Lens.lens (\DataSource' {type'} -> type') (\s@DataSource' {} a -> s {type' = a} :: DataSource)

-- | The database name.
dataSource_databaseName :: Lens.Lens' DataSource (Prelude.Maybe Prelude.Text)
dataSource_databaseName = Lens.lens (\DataSource' {databaseName} -> databaseName) (\s@DataSource' {} a -> s {databaseName = a} :: DataSource)

-- | The data source\'s ARN.
dataSource_arn :: Lens.Lens' DataSource (Prelude.Maybe Prelude.Text)
dataSource_arn = Lens.lens (\DataSource' {arn} -> arn) (\s@DataSource' {} a -> s {arn = a} :: DataSource)

instance Core.FromJSON DataSource where
  parseJSON =
    Core.withObject
      "DataSource"
      ( \x ->
          DataSource'
            Prelude.<$> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "DatabaseName")
            Prelude.<*> (x Core..:? "Arn")
      )

instance Prelude.Hashable DataSource where
  hashWithSalt _salt DataSource' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` arn

instance Prelude.NFData DataSource where
  rnf DataSource' {..} =
    Prelude.rnf type'
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf arn

instance Core.ToJSON DataSource where
  toJSON DataSource' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Type" Core..=) Prelude.<$> type',
            ("DatabaseName" Core..=) Prelude.<$> databaseName,
            ("Arn" Core..=) Prelude.<$> arn
          ]
      )
