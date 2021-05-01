{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Athena.Types.Row
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.Row where

import Network.AWS.Athena.Types.Datum
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The rows that comprise a query result table.
--
-- /See:/ 'newRow' smart constructor.
data Row = Row'
  { -- | The data that populates a row in a query result table.
    data' :: Prelude.Maybe [Datum]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
row_data = Lens.lens (\Row' {data'} -> data') (\s@Row' {} a -> s {data' = a} :: Row) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON Row where
  parseJSON =
    Prelude.withObject
      "Row"
      ( \x ->
          Row'
            Prelude.<$> (x Prelude..:? "Data" Prelude..!= Prelude.mempty)
      )

instance Prelude.Hashable Row

instance Prelude.NFData Row
