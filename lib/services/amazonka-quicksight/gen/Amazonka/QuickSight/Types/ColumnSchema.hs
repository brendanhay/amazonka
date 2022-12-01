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
-- Module      : Amazonka.QuickSight.Types.ColumnSchema
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ColumnSchema where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The column schema.
--
-- /See:/ 'newColumnSchema' smart constructor.
data ColumnSchema = ColumnSchema'
  { -- | The name of the column schema.
    name :: Prelude.Maybe Prelude.Text,
    -- | The geographic role of the column schema.
    geographicRole :: Prelude.Maybe Prelude.Text,
    -- | The data type of the column schema.
    dataType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ColumnSchema' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'columnSchema_name' - The name of the column schema.
--
-- 'geographicRole', 'columnSchema_geographicRole' - The geographic role of the column schema.
--
-- 'dataType', 'columnSchema_dataType' - The data type of the column schema.
newColumnSchema ::
  ColumnSchema
newColumnSchema =
  ColumnSchema'
    { name = Prelude.Nothing,
      geographicRole = Prelude.Nothing,
      dataType = Prelude.Nothing
    }

-- | The name of the column schema.
columnSchema_name :: Lens.Lens' ColumnSchema (Prelude.Maybe Prelude.Text)
columnSchema_name = Lens.lens (\ColumnSchema' {name} -> name) (\s@ColumnSchema' {} a -> s {name = a} :: ColumnSchema)

-- | The geographic role of the column schema.
columnSchema_geographicRole :: Lens.Lens' ColumnSchema (Prelude.Maybe Prelude.Text)
columnSchema_geographicRole = Lens.lens (\ColumnSchema' {geographicRole} -> geographicRole) (\s@ColumnSchema' {} a -> s {geographicRole = a} :: ColumnSchema)

-- | The data type of the column schema.
columnSchema_dataType :: Lens.Lens' ColumnSchema (Prelude.Maybe Prelude.Text)
columnSchema_dataType = Lens.lens (\ColumnSchema' {dataType} -> dataType) (\s@ColumnSchema' {} a -> s {dataType = a} :: ColumnSchema)

instance Core.FromJSON ColumnSchema where
  parseJSON =
    Core.withObject
      "ColumnSchema"
      ( \x ->
          ColumnSchema'
            Prelude.<$> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "GeographicRole")
            Prelude.<*> (x Core..:? "DataType")
      )

instance Prelude.Hashable ColumnSchema where
  hashWithSalt _salt ColumnSchema' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` geographicRole
      `Prelude.hashWithSalt` dataType

instance Prelude.NFData ColumnSchema where
  rnf ColumnSchema' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf geographicRole
      `Prelude.seq` Prelude.rnf dataType
