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
-- Module      : Amazonka.Glue.Types.DataQualityTargetTable
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.DataQualityTargetTable where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object representing an Glue table.
--
-- /See:/ 'newDataQualityTargetTable' smart constructor.
data DataQualityTargetTable = DataQualityTargetTable'
  { -- | The name of the Glue table.
    tableName :: Prelude.Text,
    -- | The name of the database where the Glue table exists.
    databaseName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataQualityTargetTable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tableName', 'dataQualityTargetTable_tableName' - The name of the Glue table.
--
-- 'databaseName', 'dataQualityTargetTable_databaseName' - The name of the database where the Glue table exists.
newDataQualityTargetTable ::
  -- | 'tableName'
  Prelude.Text ->
  -- | 'databaseName'
  Prelude.Text ->
  DataQualityTargetTable
newDataQualityTargetTable pTableName_ pDatabaseName_ =
  DataQualityTargetTable'
    { tableName = pTableName_,
      databaseName = pDatabaseName_
    }

-- | The name of the Glue table.
dataQualityTargetTable_tableName :: Lens.Lens' DataQualityTargetTable Prelude.Text
dataQualityTargetTable_tableName = Lens.lens (\DataQualityTargetTable' {tableName} -> tableName) (\s@DataQualityTargetTable' {} a -> s {tableName = a} :: DataQualityTargetTable)

-- | The name of the database where the Glue table exists.
dataQualityTargetTable_databaseName :: Lens.Lens' DataQualityTargetTable Prelude.Text
dataQualityTargetTable_databaseName = Lens.lens (\DataQualityTargetTable' {databaseName} -> databaseName) (\s@DataQualityTargetTable' {} a -> s {databaseName = a} :: DataQualityTargetTable)

instance Data.FromJSON DataQualityTargetTable where
  parseJSON =
    Data.withObject
      "DataQualityTargetTable"
      ( \x ->
          DataQualityTargetTable'
            Prelude.<$> (x Data..: "TableName")
            Prelude.<*> (x Data..: "DatabaseName")
      )

instance Prelude.Hashable DataQualityTargetTable where
  hashWithSalt _salt DataQualityTargetTable' {..} =
    _salt
      `Prelude.hashWithSalt` tableName
      `Prelude.hashWithSalt` databaseName

instance Prelude.NFData DataQualityTargetTable where
  rnf DataQualityTargetTable' {..} =
    Prelude.rnf tableName `Prelude.seq`
      Prelude.rnf databaseName

instance Data.ToJSON DataQualityTargetTable where
  toJSON DataQualityTargetTable' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("TableName" Data..= tableName),
            Prelude.Just ("DatabaseName" Data..= databaseName)
          ]
      )
