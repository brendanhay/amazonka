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
-- Module      : Amazonka.TimeStreamQuery.Types.Row
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TimeStreamQuery.Types.Row where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import {-# SOURCE #-} Amazonka.TimeStreamQuery.Types.Datum

-- | Represents a single row in the query results.
--
-- /See:/ 'newRow' smart constructor.
data Row = Row'
  { -- | List of data points in a single row of the result set.
    data' :: [Datum]
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
-- 'data'', 'row_data' - List of data points in a single row of the result set.
newRow ::
  Row
newRow = Row' {data' = Prelude.mempty}

-- | List of data points in a single row of the result set.
row_data :: Lens.Lens' Row [Datum]
row_data = Lens.lens (\Row' {data'} -> data') (\s@Row' {} a -> s {data' = a} :: Row) Prelude.. Lens.coerced

instance Core.FromJSON Row where
  parseJSON =
    Core.withObject
      "Row"
      ( \x ->
          Row'
            Prelude.<$> (x Core..:? "Data" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable Row where
  hashWithSalt _salt Row' {..} =
    _salt `Prelude.hashWithSalt` data'

instance Prelude.NFData Row where
  rnf Row' {..} = Prelude.rnf data'
