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
-- Module      : Amazonka.QuickSight.Types.ColumnGroupColumnSchema
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ColumnGroupColumnSchema where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure describing the name, data type, and geographic role of the
-- columns.
--
-- /See:/ 'newColumnGroupColumnSchema' smart constructor.
data ColumnGroupColumnSchema = ColumnGroupColumnSchema'
  { -- | The name of the column group\'s column schema.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ColumnGroupColumnSchema' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'columnGroupColumnSchema_name' - The name of the column group\'s column schema.
newColumnGroupColumnSchema ::
  ColumnGroupColumnSchema
newColumnGroupColumnSchema =
  ColumnGroupColumnSchema' {name = Prelude.Nothing}

-- | The name of the column group\'s column schema.
columnGroupColumnSchema_name :: Lens.Lens' ColumnGroupColumnSchema (Prelude.Maybe Prelude.Text)
columnGroupColumnSchema_name = Lens.lens (\ColumnGroupColumnSchema' {name} -> name) (\s@ColumnGroupColumnSchema' {} a -> s {name = a} :: ColumnGroupColumnSchema)

instance Data.FromJSON ColumnGroupColumnSchema where
  parseJSON =
    Data.withObject
      "ColumnGroupColumnSchema"
      ( \x ->
          ColumnGroupColumnSchema'
            Prelude.<$> (x Data..:? "Name")
      )

instance Prelude.Hashable ColumnGroupColumnSchema where
  hashWithSalt _salt ColumnGroupColumnSchema' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData ColumnGroupColumnSchema where
  rnf ColumnGroupColumnSchema' {..} = Prelude.rnf name

instance Data.ToJSON ColumnGroupColumnSchema where
  toJSON ColumnGroupColumnSchema' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Name" Data..=) Prelude.<$> name]
      )
