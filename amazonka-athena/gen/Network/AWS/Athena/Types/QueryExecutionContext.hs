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
-- Module      : Network.AWS.Athena.Types.QueryExecutionContext
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.QueryExecutionContext where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The database and data catalog context in which the query execution
-- occurs.
--
-- /See:/ 'newQueryExecutionContext' smart constructor.
data QueryExecutionContext = QueryExecutionContext'
  { -- | The name of the data catalog used in the query execution.
    catalog :: Core.Maybe Core.Text,
    -- | The name of the database used in the query execution.
    database :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'database', 'queryExecutionContext_database' - The name of the database used in the query execution.
newQueryExecutionContext ::
  QueryExecutionContext
newQueryExecutionContext =
  QueryExecutionContext'
    { catalog = Core.Nothing,
      database = Core.Nothing
    }

-- | The name of the data catalog used in the query execution.
queryExecutionContext_catalog :: Lens.Lens' QueryExecutionContext (Core.Maybe Core.Text)
queryExecutionContext_catalog = Lens.lens (\QueryExecutionContext' {catalog} -> catalog) (\s@QueryExecutionContext' {} a -> s {catalog = a} :: QueryExecutionContext)

-- | The name of the database used in the query execution.
queryExecutionContext_database :: Lens.Lens' QueryExecutionContext (Core.Maybe Core.Text)
queryExecutionContext_database = Lens.lens (\QueryExecutionContext' {database} -> database) (\s@QueryExecutionContext' {} a -> s {database = a} :: QueryExecutionContext)

instance Core.FromJSON QueryExecutionContext where
  parseJSON =
    Core.withObject
      "QueryExecutionContext"
      ( \x ->
          QueryExecutionContext'
            Core.<$> (x Core..:? "Catalog")
            Core.<*> (x Core..:? "Database")
      )

instance Core.Hashable QueryExecutionContext

instance Core.NFData QueryExecutionContext

instance Core.ToJSON QueryExecutionContext where
  toJSON QueryExecutionContext' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Catalog" Core..=) Core.<$> catalog,
            ("Database" Core..=) Core.<$> database
          ]
      )
