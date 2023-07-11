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
-- Module      : Amazonka.Athena.Types.ResultSetMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Athena.Types.ResultSetMetadata where

import Amazonka.Athena.Types.ColumnInfo
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The metadata that describes the column structure and data types of a
-- table of query results. To return a @ResultSetMetadata@ object, use
-- GetQueryResults.
--
-- /See:/ 'newResultSetMetadata' smart constructor.
data ResultSetMetadata = ResultSetMetadata'
  { -- | Information about the columns returned in a query result metadata.
    columnInfo :: Prelude.Maybe [ColumnInfo]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResultSetMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'columnInfo', 'resultSetMetadata_columnInfo' - Information about the columns returned in a query result metadata.
newResultSetMetadata ::
  ResultSetMetadata
newResultSetMetadata =
  ResultSetMetadata' {columnInfo = Prelude.Nothing}

-- | Information about the columns returned in a query result metadata.
resultSetMetadata_columnInfo :: Lens.Lens' ResultSetMetadata (Prelude.Maybe [ColumnInfo])
resultSetMetadata_columnInfo = Lens.lens (\ResultSetMetadata' {columnInfo} -> columnInfo) (\s@ResultSetMetadata' {} a -> s {columnInfo = a} :: ResultSetMetadata) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ResultSetMetadata where
  parseJSON =
    Data.withObject
      "ResultSetMetadata"
      ( \x ->
          ResultSetMetadata'
            Prelude.<$> (x Data..:? "ColumnInfo" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ResultSetMetadata where
  hashWithSalt _salt ResultSetMetadata' {..} =
    _salt `Prelude.hashWithSalt` columnInfo

instance Prelude.NFData ResultSetMetadata where
  rnf ResultSetMetadata' {..} = Prelude.rnf columnInfo
