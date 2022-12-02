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
-- Module      : Amazonka.Glue.Types.DropDuplicates
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.DropDuplicates where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies a transform that removes rows of repeating data from a data
-- set.
--
-- /See:/ 'newDropDuplicates' smart constructor.
data DropDuplicates = DropDuplicates'
  { -- | The name of the columns to be merged or removed if repeating.
    columns :: Prelude.Maybe [[Prelude.Text]],
    -- | The name of the transform node.
    name :: Prelude.Text,
    -- | The data inputs identified by their node names.
    inputs :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DropDuplicates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'columns', 'dropDuplicates_columns' - The name of the columns to be merged or removed if repeating.
--
-- 'name', 'dropDuplicates_name' - The name of the transform node.
--
-- 'inputs', 'dropDuplicates_inputs' - The data inputs identified by their node names.
newDropDuplicates ::
  -- | 'name'
  Prelude.Text ->
  -- | 'inputs'
  Prelude.NonEmpty Prelude.Text ->
  DropDuplicates
newDropDuplicates pName_ pInputs_ =
  DropDuplicates'
    { columns = Prelude.Nothing,
      name = pName_,
      inputs = Lens.coerced Lens.# pInputs_
    }

-- | The name of the columns to be merged or removed if repeating.
dropDuplicates_columns :: Lens.Lens' DropDuplicates (Prelude.Maybe [[Prelude.Text]])
dropDuplicates_columns = Lens.lens (\DropDuplicates' {columns} -> columns) (\s@DropDuplicates' {} a -> s {columns = a} :: DropDuplicates) Prelude.. Lens.mapping Lens.coerced

-- | The name of the transform node.
dropDuplicates_name :: Lens.Lens' DropDuplicates Prelude.Text
dropDuplicates_name = Lens.lens (\DropDuplicates' {name} -> name) (\s@DropDuplicates' {} a -> s {name = a} :: DropDuplicates)

-- | The data inputs identified by their node names.
dropDuplicates_inputs :: Lens.Lens' DropDuplicates (Prelude.NonEmpty Prelude.Text)
dropDuplicates_inputs = Lens.lens (\DropDuplicates' {inputs} -> inputs) (\s@DropDuplicates' {} a -> s {inputs = a} :: DropDuplicates) Prelude.. Lens.coerced

instance Data.FromJSON DropDuplicates where
  parseJSON =
    Data.withObject
      "DropDuplicates"
      ( \x ->
          DropDuplicates'
            Prelude.<$> (x Data..:? "Columns" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "Inputs")
      )

instance Prelude.Hashable DropDuplicates where
  hashWithSalt _salt DropDuplicates' {..} =
    _salt `Prelude.hashWithSalt` columns
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` inputs

instance Prelude.NFData DropDuplicates where
  rnf DropDuplicates' {..} =
    Prelude.rnf columns
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf inputs

instance Data.ToJSON DropDuplicates where
  toJSON DropDuplicates' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Columns" Data..=) Prelude.<$> columns,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Inputs" Data..= inputs)
          ]
      )
