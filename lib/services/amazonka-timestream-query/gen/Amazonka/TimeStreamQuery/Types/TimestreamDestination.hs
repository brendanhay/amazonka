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
-- Module      : Amazonka.TimeStreamQuery.Types.TimestreamDestination
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TimeStreamQuery.Types.TimestreamDestination where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Destination for scheduled query.
--
-- /See:/ 'newTimestreamDestination' smart constructor.
data TimestreamDestination = TimestreamDestination'
  { -- | Timestream database name.
    databaseName :: Prelude.Maybe Prelude.Text,
    -- | Timestream table name.
    tableName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TimestreamDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'databaseName', 'timestreamDestination_databaseName' - Timestream database name.
--
-- 'tableName', 'timestreamDestination_tableName' - Timestream table name.
newTimestreamDestination ::
  TimestreamDestination
newTimestreamDestination =
  TimestreamDestination'
    { databaseName =
        Prelude.Nothing,
      tableName = Prelude.Nothing
    }

-- | Timestream database name.
timestreamDestination_databaseName :: Lens.Lens' TimestreamDestination (Prelude.Maybe Prelude.Text)
timestreamDestination_databaseName = Lens.lens (\TimestreamDestination' {databaseName} -> databaseName) (\s@TimestreamDestination' {} a -> s {databaseName = a} :: TimestreamDestination)

-- | Timestream table name.
timestreamDestination_tableName :: Lens.Lens' TimestreamDestination (Prelude.Maybe Prelude.Text)
timestreamDestination_tableName = Lens.lens (\TimestreamDestination' {tableName} -> tableName) (\s@TimestreamDestination' {} a -> s {tableName = a} :: TimestreamDestination)

instance Data.FromJSON TimestreamDestination where
  parseJSON =
    Data.withObject
      "TimestreamDestination"
      ( \x ->
          TimestreamDestination'
            Prelude.<$> (x Data..:? "DatabaseName")
            Prelude.<*> (x Data..:? "TableName")
      )

instance Prelude.Hashable TimestreamDestination where
  hashWithSalt _salt TimestreamDestination' {..} =
    _salt
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` tableName

instance Prelude.NFData TimestreamDestination where
  rnf TimestreamDestination' {..} =
    Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf tableName
