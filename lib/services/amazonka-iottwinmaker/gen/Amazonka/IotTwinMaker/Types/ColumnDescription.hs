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
-- Module      : Amazonka.IotTwinMaker.Types.ColumnDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IotTwinMaker.Types.ColumnDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IotTwinMaker.Types.ColumnType
import qualified Amazonka.Prelude as Prelude

-- | A description of the column in the query results.
--
-- /See:/ 'newColumnDescription' smart constructor.
data ColumnDescription = ColumnDescription'
  { -- | The name of the column description.
    name :: Prelude.Maybe Prelude.Text,
    -- | The type of the column description.
    type' :: Prelude.Maybe ColumnType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ColumnDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'columnDescription_name' - The name of the column description.
--
-- 'type'', 'columnDescription_type' - The type of the column description.
newColumnDescription ::
  ColumnDescription
newColumnDescription =
  ColumnDescription'
    { name = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The name of the column description.
columnDescription_name :: Lens.Lens' ColumnDescription (Prelude.Maybe Prelude.Text)
columnDescription_name = Lens.lens (\ColumnDescription' {name} -> name) (\s@ColumnDescription' {} a -> s {name = a} :: ColumnDescription)

-- | The type of the column description.
columnDescription_type :: Lens.Lens' ColumnDescription (Prelude.Maybe ColumnType)
columnDescription_type = Lens.lens (\ColumnDescription' {type'} -> type') (\s@ColumnDescription' {} a -> s {type' = a} :: ColumnDescription)

instance Data.FromJSON ColumnDescription where
  parseJSON =
    Data.withObject
      "ColumnDescription"
      ( \x ->
          ColumnDescription'
            Prelude.<$> (x Data..:? "name")
            Prelude.<*> (x Data..:? "type")
      )

instance Prelude.Hashable ColumnDescription where
  hashWithSalt _salt ColumnDescription' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'

instance Prelude.NFData ColumnDescription where
  rnf ColumnDescription' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf type'
