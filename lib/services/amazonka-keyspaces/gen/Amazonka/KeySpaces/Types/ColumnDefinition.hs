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
-- Module      : Amazonka.KeySpaces.Types.ColumnDefinition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KeySpaces.Types.ColumnDefinition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The names and data types of regular columns.
--
-- /See:/ 'newColumnDefinition' smart constructor.
data ColumnDefinition = ColumnDefinition'
  { -- | The name of the column.
    name :: Prelude.Text,
    -- | The data type of the column. For a list of available data types, see
    -- <https://docs.aws.amazon.com/keyspaces/latest/devguide/cql.elements.html#cql.data-types Data types>
    -- in the /Amazon Keyspaces Developer Guide/.
    type' :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ColumnDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'columnDefinition_name' - The name of the column.
--
-- 'type'', 'columnDefinition_type' - The data type of the column. For a list of available data types, see
-- <https://docs.aws.amazon.com/keyspaces/latest/devguide/cql.elements.html#cql.data-types Data types>
-- in the /Amazon Keyspaces Developer Guide/.
newColumnDefinition ::
  -- | 'name'
  Prelude.Text ->
  -- | 'type''
  Prelude.Text ->
  ColumnDefinition
newColumnDefinition pName_ pType_ =
  ColumnDefinition' {name = pName_, type' = pType_}

-- | The name of the column.
columnDefinition_name :: Lens.Lens' ColumnDefinition Prelude.Text
columnDefinition_name = Lens.lens (\ColumnDefinition' {name} -> name) (\s@ColumnDefinition' {} a -> s {name = a} :: ColumnDefinition)

-- | The data type of the column. For a list of available data types, see
-- <https://docs.aws.amazon.com/keyspaces/latest/devguide/cql.elements.html#cql.data-types Data types>
-- in the /Amazon Keyspaces Developer Guide/.
columnDefinition_type :: Lens.Lens' ColumnDefinition Prelude.Text
columnDefinition_type = Lens.lens (\ColumnDefinition' {type'} -> type') (\s@ColumnDefinition' {} a -> s {type' = a} :: ColumnDefinition)

instance Core.FromJSON ColumnDefinition where
  parseJSON =
    Core.withObject
      "ColumnDefinition"
      ( \x ->
          ColumnDefinition'
            Prelude.<$> (x Core..: "name") Prelude.<*> (x Core..: "type")
      )

instance Prelude.Hashable ColumnDefinition where
  hashWithSalt _salt ColumnDefinition' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'

instance Prelude.NFData ColumnDefinition where
  rnf ColumnDefinition' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf type'

instance Core.ToJSON ColumnDefinition where
  toJSON ColumnDefinition' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("name" Core..= name),
            Prelude.Just ("type" Core..= type')
          ]
      )
