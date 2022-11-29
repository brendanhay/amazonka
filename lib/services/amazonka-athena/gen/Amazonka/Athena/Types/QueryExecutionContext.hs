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
-- Module      : Amazonka.Athena.Types.QueryExecutionContext
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Athena.Types.QueryExecutionContext where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The database and data catalog context in which the query execution
-- occurs.
--
-- /See:/ 'newQueryExecutionContext' smart constructor.
data QueryExecutionContext = QueryExecutionContext'
  { -- | The name of the data catalog used in the query execution.
    catalog :: Prelude.Maybe Prelude.Text,
    -- | The name of the database used in the query execution. The database must
    -- exist in the catalog.
    database :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'QueryExecutionContext' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalog', 'queryExecutionContext_catalog' - The name of the data catalog used in the query execution.
--
-- 'database', 'queryExecutionContext_database' - The name of the database used in the query execution. The database must
-- exist in the catalog.
newQueryExecutionContext ::
  QueryExecutionContext
newQueryExecutionContext =
  QueryExecutionContext'
    { catalog = Prelude.Nothing,
      database = Prelude.Nothing
    }

-- | The name of the data catalog used in the query execution.
queryExecutionContext_catalog :: Lens.Lens' QueryExecutionContext (Prelude.Maybe Prelude.Text)
queryExecutionContext_catalog = Lens.lens (\QueryExecutionContext' {catalog} -> catalog) (\s@QueryExecutionContext' {} a -> s {catalog = a} :: QueryExecutionContext)

-- | The name of the database used in the query execution. The database must
-- exist in the catalog.
queryExecutionContext_database :: Lens.Lens' QueryExecutionContext (Prelude.Maybe Prelude.Text)
queryExecutionContext_database = Lens.lens (\QueryExecutionContext' {database} -> database) (\s@QueryExecutionContext' {} a -> s {database = a} :: QueryExecutionContext)

instance Core.FromJSON QueryExecutionContext where
  parseJSON =
    Core.withObject
      "QueryExecutionContext"
      ( \x ->
          QueryExecutionContext'
            Prelude.<$> (x Core..:? "Catalog")
            Prelude.<*> (x Core..:? "Database")
      )

instance Prelude.Hashable QueryExecutionContext where
  hashWithSalt _salt QueryExecutionContext' {..} =
    _salt `Prelude.hashWithSalt` catalog
      `Prelude.hashWithSalt` database

instance Prelude.NFData QueryExecutionContext where
  rnf QueryExecutionContext' {..} =
    Prelude.rnf catalog
      `Prelude.seq` Prelude.rnf database

instance Core.ToJSON QueryExecutionContext where
  toJSON QueryExecutionContext' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Catalog" Core..=) Prelude.<$> catalog,
            ("Database" Core..=) Prelude.<$> database
          ]
      )
