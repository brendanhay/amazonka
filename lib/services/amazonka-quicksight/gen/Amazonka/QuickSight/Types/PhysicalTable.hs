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
-- Module      : Amazonka.QuickSight.Types.PhysicalTable
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.PhysicalTable where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.CustomSql
import Amazonka.QuickSight.Types.RelationalTable
import Amazonka.QuickSight.Types.S3Source

-- | A view of a data source that contains information about the shape of the
-- data in the underlying source. This is a variant type structure. For
-- this structure to be valid, only one of the attributes can be non-null.
--
-- /See:/ 'newPhysicalTable' smart constructor.
data PhysicalTable = PhysicalTable'
  { -- | A physical table type built from the results of the custom SQL query.
    customSql :: Prelude.Maybe CustomSql,
    -- | A physical table type for relational data sources.
    relationalTable :: Prelude.Maybe RelationalTable,
    -- | A physical table type for as S3 data source.
    s3Source :: Prelude.Maybe S3Source
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PhysicalTable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customSql', 'physicalTable_customSql' - A physical table type built from the results of the custom SQL query.
--
-- 'relationalTable', 'physicalTable_relationalTable' - A physical table type for relational data sources.
--
-- 's3Source', 'physicalTable_s3Source' - A physical table type for as S3 data source.
newPhysicalTable ::
  PhysicalTable
newPhysicalTable =
  PhysicalTable'
    { customSql = Prelude.Nothing,
      relationalTable = Prelude.Nothing,
      s3Source = Prelude.Nothing
    }

-- | A physical table type built from the results of the custom SQL query.
physicalTable_customSql :: Lens.Lens' PhysicalTable (Prelude.Maybe CustomSql)
physicalTable_customSql = Lens.lens (\PhysicalTable' {customSql} -> customSql) (\s@PhysicalTable' {} a -> s {customSql = a} :: PhysicalTable)

-- | A physical table type for relational data sources.
physicalTable_relationalTable :: Lens.Lens' PhysicalTable (Prelude.Maybe RelationalTable)
physicalTable_relationalTable = Lens.lens (\PhysicalTable' {relationalTable} -> relationalTable) (\s@PhysicalTable' {} a -> s {relationalTable = a} :: PhysicalTable)

-- | A physical table type for as S3 data source.
physicalTable_s3Source :: Lens.Lens' PhysicalTable (Prelude.Maybe S3Source)
physicalTable_s3Source = Lens.lens (\PhysicalTable' {s3Source} -> s3Source) (\s@PhysicalTable' {} a -> s {s3Source = a} :: PhysicalTable)

instance Data.FromJSON PhysicalTable where
  parseJSON =
    Data.withObject
      "PhysicalTable"
      ( \x ->
          PhysicalTable'
            Prelude.<$> (x Data..:? "CustomSql")
            Prelude.<*> (x Data..:? "RelationalTable")
            Prelude.<*> (x Data..:? "S3Source")
      )

instance Prelude.Hashable PhysicalTable where
  hashWithSalt _salt PhysicalTable' {..} =
    _salt `Prelude.hashWithSalt` customSql
      `Prelude.hashWithSalt` relationalTable
      `Prelude.hashWithSalt` s3Source

instance Prelude.NFData PhysicalTable where
  rnf PhysicalTable' {..} =
    Prelude.rnf customSql
      `Prelude.seq` Prelude.rnf relationalTable
      `Prelude.seq` Prelude.rnf s3Source

instance Data.ToJSON PhysicalTable where
  toJSON PhysicalTable' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CustomSql" Data..=) Prelude.<$> customSql,
            ("RelationalTable" Data..=)
              Prelude.<$> relationalTable,
            ("S3Source" Data..=) Prelude.<$> s3Source
          ]
      )
