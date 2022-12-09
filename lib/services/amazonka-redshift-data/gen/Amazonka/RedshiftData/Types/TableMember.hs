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
-- Module      : Amazonka.RedshiftData.Types.TableMember
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RedshiftData.Types.TableMember where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The properties of a table.
--
-- /See:/ 'newTableMember' smart constructor.
data TableMember = TableMember'
  { -- | The name of the table.
    name :: Prelude.Maybe Prelude.Text,
    -- | The schema containing the table.
    schema :: Prelude.Maybe Prelude.Text,
    -- | The type of the table. Possible values include TABLE, VIEW, SYSTEM
    -- TABLE, GLOBAL TEMPORARY, LOCAL TEMPORARY, ALIAS, and SYNONYM.
    type' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TableMember' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'tableMember_name' - The name of the table.
--
-- 'schema', 'tableMember_schema' - The schema containing the table.
--
-- 'type'', 'tableMember_type' - The type of the table. Possible values include TABLE, VIEW, SYSTEM
-- TABLE, GLOBAL TEMPORARY, LOCAL TEMPORARY, ALIAS, and SYNONYM.
newTableMember ::
  TableMember
newTableMember =
  TableMember'
    { name = Prelude.Nothing,
      schema = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The name of the table.
tableMember_name :: Lens.Lens' TableMember (Prelude.Maybe Prelude.Text)
tableMember_name = Lens.lens (\TableMember' {name} -> name) (\s@TableMember' {} a -> s {name = a} :: TableMember)

-- | The schema containing the table.
tableMember_schema :: Lens.Lens' TableMember (Prelude.Maybe Prelude.Text)
tableMember_schema = Lens.lens (\TableMember' {schema} -> schema) (\s@TableMember' {} a -> s {schema = a} :: TableMember)

-- | The type of the table. Possible values include TABLE, VIEW, SYSTEM
-- TABLE, GLOBAL TEMPORARY, LOCAL TEMPORARY, ALIAS, and SYNONYM.
tableMember_type :: Lens.Lens' TableMember (Prelude.Maybe Prelude.Text)
tableMember_type = Lens.lens (\TableMember' {type'} -> type') (\s@TableMember' {} a -> s {type' = a} :: TableMember)

instance Data.FromJSON TableMember where
  parseJSON =
    Data.withObject
      "TableMember"
      ( \x ->
          TableMember'
            Prelude.<$> (x Data..:? "name")
            Prelude.<*> (x Data..:? "schema")
            Prelude.<*> (x Data..:? "type")
      )

instance Prelude.Hashable TableMember where
  hashWithSalt _salt TableMember' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` schema
      `Prelude.hashWithSalt` type'

instance Prelude.NFData TableMember where
  rnf TableMember' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf schema
      `Prelude.seq` Prelude.rnf type'
