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
-- Module      : Network.AWS.IoTAnalytics.Types.SqlQueryDatasetAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.SqlQueryDatasetAction where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTAnalytics.Types.QueryFilter
import qualified Network.AWS.Lens as Lens

-- | The SQL query to modify the message.
--
-- /See:/ 'newSqlQueryDatasetAction' smart constructor.
data SqlQueryDatasetAction = SqlQueryDatasetAction'
  { -- | Prefilters applied to message data.
    filters :: Core.Maybe [QueryFilter],
    -- | A SQL query string.
    sqlQuery :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SqlQueryDatasetAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'sqlQueryDatasetAction_filters' - Prefilters applied to message data.
--
-- 'sqlQuery', 'sqlQueryDatasetAction_sqlQuery' - A SQL query string.
newSqlQueryDatasetAction ::
  -- | 'sqlQuery'
  Core.Text ->
  SqlQueryDatasetAction
newSqlQueryDatasetAction pSqlQuery_ =
  SqlQueryDatasetAction'
    { filters = Core.Nothing,
      sqlQuery = pSqlQuery_
    }

-- | Prefilters applied to message data.
sqlQueryDatasetAction_filters :: Lens.Lens' SqlQueryDatasetAction (Core.Maybe [QueryFilter])
sqlQueryDatasetAction_filters = Lens.lens (\SqlQueryDatasetAction' {filters} -> filters) (\s@SqlQueryDatasetAction' {} a -> s {filters = a} :: SqlQueryDatasetAction) Core.. Lens.mapping Lens._Coerce

-- | A SQL query string.
sqlQueryDatasetAction_sqlQuery :: Lens.Lens' SqlQueryDatasetAction Core.Text
sqlQueryDatasetAction_sqlQuery = Lens.lens (\SqlQueryDatasetAction' {sqlQuery} -> sqlQuery) (\s@SqlQueryDatasetAction' {} a -> s {sqlQuery = a} :: SqlQueryDatasetAction)

instance Core.FromJSON SqlQueryDatasetAction where
  parseJSON =
    Core.withObject
      "SqlQueryDatasetAction"
      ( \x ->
          SqlQueryDatasetAction'
            Core.<$> (x Core..:? "filters" Core..!= Core.mempty)
            Core.<*> (x Core..: "sqlQuery")
      )

instance Core.Hashable SqlQueryDatasetAction

instance Core.NFData SqlQueryDatasetAction

instance Core.ToJSON SqlQueryDatasetAction where
  toJSON SqlQueryDatasetAction' {..} =
    Core.object
      ( Core.catMaybes
          [ ("filters" Core..=) Core.<$> filters,
            Core.Just ("sqlQuery" Core..= sqlQuery)
          ]
      )
