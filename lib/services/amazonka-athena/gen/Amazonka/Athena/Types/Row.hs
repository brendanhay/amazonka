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
-- Module      : Amazonka.Athena.Types.Row
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Athena.Types.Row where

import Amazonka.Athena.Types.Datum
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The rows that make up a query result table.
--
-- /See:/ 'newRow' smart constructor.
data Row = Row'
  { -- | The data that populates a row in a query result table.
    data' :: Prelude.Maybe [Datum]
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
-- 'data'', 'row_data' - The data that populates a row in a query result table.
newRow ::
  Row
newRow = Row' {data' = Prelude.Nothing}

-- | The data that populates a row in a query result table.
row_data :: Lens.Lens' Row (Prelude.Maybe [Datum])
row_data = Lens.lens (\Row' {data'} -> data') (\s@Row' {} a -> s {data' = a} :: Row) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON Row where
  parseJSON =
    Data.withObject
      "Row"
      ( \x ->
          Row'
            Prelude.<$> (x Data..:? "Data" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable Row where
  hashWithSalt _salt Row' {..} =
    _salt `Prelude.hashWithSalt` data'

instance Prelude.NFData Row where
  rnf Row' {..} = Prelude.rnf data'
