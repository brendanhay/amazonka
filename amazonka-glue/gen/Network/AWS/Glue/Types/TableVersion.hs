{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Glue.Types.TableVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.TableVersion where

import Network.AWS.Glue.Types.Table
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies a version of a table.
--
-- /See:/ 'newTableVersion' smart constructor.
data TableVersion = TableVersion'
  { -- | The ID value that identifies this table version. A @VersionId@ is a
    -- string representation of an integer. Each version is incremented by 1.
    versionId :: Prelude.Maybe Prelude.Text,
    -- | The table in question.
    table :: Prelude.Maybe Table
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TableVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'versionId', 'tableVersion_versionId' - The ID value that identifies this table version. A @VersionId@ is a
-- string representation of an integer. Each version is incremented by 1.
--
-- 'table', 'tableVersion_table' - The table in question.
newTableVersion ::
  TableVersion
newTableVersion =
  TableVersion'
    { versionId = Prelude.Nothing,
      table = Prelude.Nothing
    }

-- | The ID value that identifies this table version. A @VersionId@ is a
-- string representation of an integer. Each version is incremented by 1.
tableVersion_versionId :: Lens.Lens' TableVersion (Prelude.Maybe Prelude.Text)
tableVersion_versionId = Lens.lens (\TableVersion' {versionId} -> versionId) (\s@TableVersion' {} a -> s {versionId = a} :: TableVersion)

-- | The table in question.
tableVersion_table :: Lens.Lens' TableVersion (Prelude.Maybe Table)
tableVersion_table = Lens.lens (\TableVersion' {table} -> table) (\s@TableVersion' {} a -> s {table = a} :: TableVersion)

instance Prelude.FromJSON TableVersion where
  parseJSON =
    Prelude.withObject
      "TableVersion"
      ( \x ->
          TableVersion'
            Prelude.<$> (x Prelude..:? "VersionId")
            Prelude.<*> (x Prelude..:? "Table")
      )

instance Prelude.Hashable TableVersion

instance Prelude.NFData TableVersion
