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
-- Module      : Amazonka.IotTwinMaker.Types.Row
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IotTwinMaker.Types.Row where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IotTwinMaker.Types.QueryResultValue
import qualified Amazonka.Prelude as Prelude

-- | Represents a single row in the query results.
--
-- /See:/ 'newRow' smart constructor.
data Row = Row'
  { -- | The data in a row of query results.
    rowData :: Prelude.Maybe [QueryResultValue]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Row' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rowData', 'row_rowData' - The data in a row of query results.
newRow ::
  Row
newRow = Row' {rowData = Prelude.Nothing}

-- | The data in a row of query results.
row_rowData :: Lens.Lens' Row (Prelude.Maybe [QueryResultValue])
row_rowData = Lens.lens (\Row' {rowData} -> rowData) (\s@Row' {} a -> s {rowData = a} :: Row) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON Row where
  parseJSON =
    Core.withObject
      "Row"
      ( \x ->
          Row'
            Prelude.<$> (x Core..:? "rowData" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable Row where
  hashWithSalt _salt Row' {..} =
    _salt `Prelude.hashWithSalt` rowData

instance Prelude.NFData Row where
  rnf Row' {..} = Prelude.rnf rowData
