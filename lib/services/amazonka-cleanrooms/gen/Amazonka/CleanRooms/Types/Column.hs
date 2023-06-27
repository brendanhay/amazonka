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
-- Module      : Amazonka.CleanRooms.Types.Column
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CleanRooms.Types.Column where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A column within a schema relation, derived from the underlying AWS Glue
-- table.
--
-- /See:/ 'newColumn' smart constructor.
data Column = Column'
  { -- | The name of the column.
    name :: Prelude.Text,
    -- | The type of the column.
    type' :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Column' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'column_name' - The name of the column.
--
-- 'type'', 'column_type' - The type of the column.
newColumn ::
  -- | 'name'
  Prelude.Text ->
  -- | 'type''
  Prelude.Text ->
  Column
newColumn pName_ pType_ =
  Column' {name = pName_, type' = pType_}

-- | The name of the column.
column_name :: Lens.Lens' Column Prelude.Text
column_name = Lens.lens (\Column' {name} -> name) (\s@Column' {} a -> s {name = a} :: Column)

-- | The type of the column.
column_type :: Lens.Lens' Column Prelude.Text
column_type = Lens.lens (\Column' {type'} -> type') (\s@Column' {} a -> s {type' = a} :: Column)

instance Data.FromJSON Column where
  parseJSON =
    Data.withObject
      "Column"
      ( \x ->
          Column'
            Prelude.<$> (x Data..: "name")
            Prelude.<*> (x Data..: "type")
      )

instance Prelude.Hashable Column where
  hashWithSalt _salt Column' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'

instance Prelude.NFData Column where
  rnf Column' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf type'
