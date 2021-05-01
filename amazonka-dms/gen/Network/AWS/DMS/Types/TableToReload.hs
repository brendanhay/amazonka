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
-- Module      : Network.AWS.DMS.Types.TableToReload
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.TableToReload where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides the name of the schema and table to be reloaded.
--
-- /See:/ 'newTableToReload' smart constructor.
data TableToReload = TableToReload'
  { -- | The schema name of the table to be reloaded.
    schemaName :: Prelude.Text,
    -- | The table name of the table to be reloaded.
    tableName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TableToReload' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schemaName', 'tableToReload_schemaName' - The schema name of the table to be reloaded.
--
-- 'tableName', 'tableToReload_tableName' - The table name of the table to be reloaded.
newTableToReload ::
  -- | 'schemaName'
  Prelude.Text ->
  -- | 'tableName'
  Prelude.Text ->
  TableToReload
newTableToReload pSchemaName_ pTableName_ =
  TableToReload'
    { schemaName = pSchemaName_,
      tableName = pTableName_
    }

-- | The schema name of the table to be reloaded.
tableToReload_schemaName :: Lens.Lens' TableToReload Prelude.Text
tableToReload_schemaName = Lens.lens (\TableToReload' {schemaName} -> schemaName) (\s@TableToReload' {} a -> s {schemaName = a} :: TableToReload)

-- | The table name of the table to be reloaded.
tableToReload_tableName :: Lens.Lens' TableToReload Prelude.Text
tableToReload_tableName = Lens.lens (\TableToReload' {tableName} -> tableName) (\s@TableToReload' {} a -> s {tableName = a} :: TableToReload)

instance Prelude.Hashable TableToReload

instance Prelude.NFData TableToReload

instance Prelude.ToJSON TableToReload where
  toJSON TableToReload' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("SchemaName" Prelude..= schemaName),
            Prelude.Just ("TableName" Prelude..= tableName)
          ]
      )
