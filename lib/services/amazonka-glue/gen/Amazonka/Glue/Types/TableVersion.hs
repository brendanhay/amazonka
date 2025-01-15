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
-- Module      : Amazonka.Glue.Types.TableVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.TableVersion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.Table
import qualified Amazonka.Prelude as Prelude

-- | Specifies a version of a table.
--
-- /See:/ 'newTableVersion' smart constructor.
data TableVersion = TableVersion'
  { -- | The table in question.
    table :: Prelude.Maybe Table,
    -- | The ID value that identifies this table version. A @VersionId@ is a
    -- string representation of an integer. Each version is incremented by 1.
    versionId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TableVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'table', 'tableVersion_table' - The table in question.
--
-- 'versionId', 'tableVersion_versionId' - The ID value that identifies this table version. A @VersionId@ is a
-- string representation of an integer. Each version is incremented by 1.
newTableVersion ::
  TableVersion
newTableVersion =
  TableVersion'
    { table = Prelude.Nothing,
      versionId = Prelude.Nothing
    }

-- | The table in question.
tableVersion_table :: Lens.Lens' TableVersion (Prelude.Maybe Table)
tableVersion_table = Lens.lens (\TableVersion' {table} -> table) (\s@TableVersion' {} a -> s {table = a} :: TableVersion)

-- | The ID value that identifies this table version. A @VersionId@ is a
-- string representation of an integer. Each version is incremented by 1.
tableVersion_versionId :: Lens.Lens' TableVersion (Prelude.Maybe Prelude.Text)
tableVersion_versionId = Lens.lens (\TableVersion' {versionId} -> versionId) (\s@TableVersion' {} a -> s {versionId = a} :: TableVersion)

instance Data.FromJSON TableVersion where
  parseJSON =
    Data.withObject
      "TableVersion"
      ( \x ->
          TableVersion'
            Prelude.<$> (x Data..:? "Table")
            Prelude.<*> (x Data..:? "VersionId")
      )

instance Prelude.Hashable TableVersion where
  hashWithSalt _salt TableVersion' {..} =
    _salt
      `Prelude.hashWithSalt` table
      `Prelude.hashWithSalt` versionId

instance Prelude.NFData TableVersion where
  rnf TableVersion' {..} =
    Prelude.rnf table `Prelude.seq`
      Prelude.rnf versionId
