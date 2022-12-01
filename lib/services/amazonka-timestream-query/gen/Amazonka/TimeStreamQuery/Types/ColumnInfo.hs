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
-- Module      : Amazonka.TimeStreamQuery.Types.ColumnInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TimeStreamQuery.Types.ColumnInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import {-# SOURCE #-} Amazonka.TimeStreamQuery.Types.Type

-- | Contains the metadata for query results such as the column names, data
-- types, and other attributes.
--
-- /See:/ 'newColumnInfo' smart constructor.
data ColumnInfo = ColumnInfo'
  { -- | The name of the result set column. The name of the result set is
    -- available for columns of all data types except for arrays.
    name :: Prelude.Maybe Prelude.Text,
    -- | The data type of the result set column. The data type can be a scalar or
    -- complex. Scalar data types are integers, strings, doubles, Booleans, and
    -- others. Complex data types are types such as arrays, rows, and others.
    type' :: Type
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ColumnInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'columnInfo_name' - The name of the result set column. The name of the result set is
-- available for columns of all data types except for arrays.
--
-- 'type'', 'columnInfo_type' - The data type of the result set column. The data type can be a scalar or
-- complex. Scalar data types are integers, strings, doubles, Booleans, and
-- others. Complex data types are types such as arrays, rows, and others.
newColumnInfo ::
  -- | 'type''
  Type ->
  ColumnInfo
newColumnInfo pType_ =
  ColumnInfo' {name = Prelude.Nothing, type' = pType_}

-- | The name of the result set column. The name of the result set is
-- available for columns of all data types except for arrays.
columnInfo_name :: Lens.Lens' ColumnInfo (Prelude.Maybe Prelude.Text)
columnInfo_name = Lens.lens (\ColumnInfo' {name} -> name) (\s@ColumnInfo' {} a -> s {name = a} :: ColumnInfo)

-- | The data type of the result set column. The data type can be a scalar or
-- complex. Scalar data types are integers, strings, doubles, Booleans, and
-- others. Complex data types are types such as arrays, rows, and others.
columnInfo_type :: Lens.Lens' ColumnInfo Type
columnInfo_type = Lens.lens (\ColumnInfo' {type'} -> type') (\s@ColumnInfo' {} a -> s {type' = a} :: ColumnInfo)

instance Core.FromJSON ColumnInfo where
  parseJSON =
    Core.withObject
      "ColumnInfo"
      ( \x ->
          ColumnInfo'
            Prelude.<$> (x Core..:? "Name") Prelude.<*> (x Core..: "Type")
      )

instance Prelude.Hashable ColumnInfo where
  hashWithSalt _salt ColumnInfo' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'

instance Prelude.NFData ColumnInfo where
  rnf ColumnInfo' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf type'
