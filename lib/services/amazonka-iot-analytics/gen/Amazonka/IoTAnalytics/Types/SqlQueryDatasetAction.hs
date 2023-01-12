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
-- Module      : Amazonka.IoTAnalytics.Types.SqlQueryDatasetAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTAnalytics.Types.SqlQueryDatasetAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTAnalytics.Types.QueryFilter
import qualified Amazonka.Prelude as Prelude

-- | The SQL query to modify the message.
--
-- /See:/ 'newSqlQueryDatasetAction' smart constructor.
data SqlQueryDatasetAction = SqlQueryDatasetAction'
  { -- | Prefilters applied to message data.
    filters :: Prelude.Maybe [QueryFilter],
    -- | A SQL query string.
    sqlQuery :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  SqlQueryDatasetAction
newSqlQueryDatasetAction pSqlQuery_ =
  SqlQueryDatasetAction'
    { filters = Prelude.Nothing,
      sqlQuery = pSqlQuery_
    }

-- | Prefilters applied to message data.
sqlQueryDatasetAction_filters :: Lens.Lens' SqlQueryDatasetAction (Prelude.Maybe [QueryFilter])
sqlQueryDatasetAction_filters = Lens.lens (\SqlQueryDatasetAction' {filters} -> filters) (\s@SqlQueryDatasetAction' {} a -> s {filters = a} :: SqlQueryDatasetAction) Prelude.. Lens.mapping Lens.coerced

-- | A SQL query string.
sqlQueryDatasetAction_sqlQuery :: Lens.Lens' SqlQueryDatasetAction Prelude.Text
sqlQueryDatasetAction_sqlQuery = Lens.lens (\SqlQueryDatasetAction' {sqlQuery} -> sqlQuery) (\s@SqlQueryDatasetAction' {} a -> s {sqlQuery = a} :: SqlQueryDatasetAction)

instance Data.FromJSON SqlQueryDatasetAction where
  parseJSON =
    Data.withObject
      "SqlQueryDatasetAction"
      ( \x ->
          SqlQueryDatasetAction'
            Prelude.<$> (x Data..:? "filters" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "sqlQuery")
      )

instance Prelude.Hashable SqlQueryDatasetAction where
  hashWithSalt _salt SqlQueryDatasetAction' {..} =
    _salt `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` sqlQuery

instance Prelude.NFData SqlQueryDatasetAction where
  rnf SqlQueryDatasetAction' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf sqlQuery

instance Data.ToJSON SqlQueryDatasetAction where
  toJSON SqlQueryDatasetAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("filters" Data..=) Prelude.<$> filters,
            Prelude.Just ("sqlQuery" Data..= sqlQuery)
          ]
      )
