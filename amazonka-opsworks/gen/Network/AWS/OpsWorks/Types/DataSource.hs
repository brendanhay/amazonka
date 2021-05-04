{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an app\'s data source.
--
-- /See:/ 'newDataSource' smart constructor.
data DataSource = DataSource'
  { -- | The data source\'s ARN.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The data source\'s type, @AutoSelectOpsworksMysqlInstance@,
    -- @OpsworksMysqlInstance@, @RdsDbInstance@, or @None@.
    type' :: Prelude.Maybe Prelude.Text,
    -- | The database name.
    databaseName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { arn = Prelude.Nothing,
      type' = Prelude.Nothing,
      databaseName = Prelude.Nothing
    }

-- | The data source\'s ARN.
dataSource_arn :: Lens.Lens' DataSource (Prelude.Maybe Prelude.Text)
dataSource_arn = Lens.lens (\DataSource' {arn} -> arn) (\s@DataSource' {} a -> s {arn = a} :: DataSource)

-- | The data source\'s type, @AutoSelectOpsworksMysqlInstance@,
-- @OpsworksMysqlInstance@, @RdsDbInstance@, or @None@.
dataSource_type :: Lens.Lens' DataSource (Prelude.Maybe Prelude.Text)
dataSource_type = Lens.lens (\DataSource' {type'} -> type') (\s@DataSource' {} a -> s {type' = a} :: DataSource)

-- | The database name.
dataSource_databaseName :: Lens.Lens' DataSource (Prelude.Maybe Prelude.Text)
dataSource_databaseName = Lens.lens (\DataSource' {databaseName} -> databaseName) (\s@DataSource' {} a -> s {databaseName = a} :: DataSource)

instance Prelude.FromJSON DataSource where
  parseJSON =
    Prelude.withObject
      "DataSource"
      ( \x ->
          DataSource'
            Prelude.<$> (x Prelude..:? "Arn")
            Prelude.<*> (x Prelude..:? "Type")
            Prelude.<*> (x Prelude..:? "DatabaseName")
      )

instance Prelude.Hashable DataSource

instance Prelude.NFData DataSource

instance Prelude.ToJSON DataSource where
  toJSON DataSource' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Arn" Prelude..=) Prelude.<$> arn,
            ("Type" Prelude..=) Prelude.<$> type',
            ("DatabaseName" Prelude..=)
              Prelude.<$> databaseName
          ]
      )
