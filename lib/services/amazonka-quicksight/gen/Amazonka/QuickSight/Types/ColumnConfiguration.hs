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
-- Module      : Amazonka.QuickSight.Types.ColumnConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ColumnConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ColumnIdentifier
import Amazonka.QuickSight.Types.ColumnRole
import Amazonka.QuickSight.Types.FormatConfiguration

-- | The general configuration of a column.
--
-- /See:/ 'newColumnConfiguration' smart constructor.
data ColumnConfiguration = ColumnConfiguration'
  { -- | The format configuration of a column.
    formatConfiguration :: Prelude.Maybe FormatConfiguration,
    -- | The role of the column.
    role' :: Prelude.Maybe ColumnRole,
    -- | The column.
    column :: ColumnIdentifier
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ColumnConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'formatConfiguration', 'columnConfiguration_formatConfiguration' - The format configuration of a column.
--
-- 'role'', 'columnConfiguration_role' - The role of the column.
--
-- 'column', 'columnConfiguration_column' - The column.
newColumnConfiguration ::
  -- | 'column'
  ColumnIdentifier ->
  ColumnConfiguration
newColumnConfiguration pColumn_ =
  ColumnConfiguration'
    { formatConfiguration =
        Prelude.Nothing,
      role' = Prelude.Nothing,
      column = pColumn_
    }

-- | The format configuration of a column.
columnConfiguration_formatConfiguration :: Lens.Lens' ColumnConfiguration (Prelude.Maybe FormatConfiguration)
columnConfiguration_formatConfiguration = Lens.lens (\ColumnConfiguration' {formatConfiguration} -> formatConfiguration) (\s@ColumnConfiguration' {} a -> s {formatConfiguration = a} :: ColumnConfiguration)

-- | The role of the column.
columnConfiguration_role :: Lens.Lens' ColumnConfiguration (Prelude.Maybe ColumnRole)
columnConfiguration_role = Lens.lens (\ColumnConfiguration' {role'} -> role') (\s@ColumnConfiguration' {} a -> s {role' = a} :: ColumnConfiguration)

-- | The column.
columnConfiguration_column :: Lens.Lens' ColumnConfiguration ColumnIdentifier
columnConfiguration_column = Lens.lens (\ColumnConfiguration' {column} -> column) (\s@ColumnConfiguration' {} a -> s {column = a} :: ColumnConfiguration)

instance Data.FromJSON ColumnConfiguration where
  parseJSON =
    Data.withObject
      "ColumnConfiguration"
      ( \x ->
          ColumnConfiguration'
            Prelude.<$> (x Data..:? "FormatConfiguration")
            Prelude.<*> (x Data..:? "Role")
            Prelude.<*> (x Data..: "Column")
      )

instance Prelude.Hashable ColumnConfiguration where
  hashWithSalt _salt ColumnConfiguration' {..} =
    _salt `Prelude.hashWithSalt` formatConfiguration
      `Prelude.hashWithSalt` role'
      `Prelude.hashWithSalt` column

instance Prelude.NFData ColumnConfiguration where
  rnf ColumnConfiguration' {..} =
    Prelude.rnf formatConfiguration
      `Prelude.seq` Prelude.rnf role'
      `Prelude.seq` Prelude.rnf column

instance Data.ToJSON ColumnConfiguration where
  toJSON ColumnConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FormatConfiguration" Data..=)
              Prelude.<$> formatConfiguration,
            ("Role" Data..=) Prelude.<$> role',
            Prelude.Just ("Column" Data..= column)
          ]
      )
