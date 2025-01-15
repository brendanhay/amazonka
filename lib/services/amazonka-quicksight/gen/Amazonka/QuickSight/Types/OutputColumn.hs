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
-- Module      : Amazonka.QuickSight.Types.OutputColumn
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.OutputColumn where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ColumnDataType

-- | Output column.
--
-- /See:/ 'newOutputColumn' smart constructor.
data OutputColumn = OutputColumn'
  { -- | A description for a column.
    description :: Prelude.Maybe Prelude.Text,
    -- | A display name for the dataset.
    name :: Prelude.Maybe Prelude.Text,
    -- | Type.
    type' :: Prelude.Maybe ColumnDataType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OutputColumn' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'outputColumn_description' - A description for a column.
--
-- 'name', 'outputColumn_name' - A display name for the dataset.
--
-- 'type'', 'outputColumn_type' - Type.
newOutputColumn ::
  OutputColumn
newOutputColumn =
  OutputColumn'
    { description = Prelude.Nothing,
      name = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | A description for a column.
outputColumn_description :: Lens.Lens' OutputColumn (Prelude.Maybe Prelude.Text)
outputColumn_description = Lens.lens (\OutputColumn' {description} -> description) (\s@OutputColumn' {} a -> s {description = a} :: OutputColumn)

-- | A display name for the dataset.
outputColumn_name :: Lens.Lens' OutputColumn (Prelude.Maybe Prelude.Text)
outputColumn_name = Lens.lens (\OutputColumn' {name} -> name) (\s@OutputColumn' {} a -> s {name = a} :: OutputColumn)

-- | Type.
outputColumn_type :: Lens.Lens' OutputColumn (Prelude.Maybe ColumnDataType)
outputColumn_type = Lens.lens (\OutputColumn' {type'} -> type') (\s@OutputColumn' {} a -> s {type' = a} :: OutputColumn)

instance Data.FromJSON OutputColumn where
  parseJSON =
    Data.withObject
      "OutputColumn"
      ( \x ->
          OutputColumn'
            Prelude.<$> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable OutputColumn where
  hashWithSalt _salt OutputColumn' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'

instance Prelude.NFData OutputColumn where
  rnf OutputColumn' {..} =
    Prelude.rnf description `Prelude.seq`
      Prelude.rnf name `Prelude.seq`
        Prelude.rnf type'
