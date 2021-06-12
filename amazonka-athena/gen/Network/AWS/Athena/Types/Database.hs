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
-- Module      : Network.AWS.Athena.Types.Database
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.Database where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains metadata information for a database in a data catalog.
--
-- /See:/ 'newDatabase' smart constructor.
data Database = Database'
  { -- | An optional description of the database.
    description :: Core.Maybe Core.Text,
    -- | A set of custom key\/value pairs.
    parameters :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The name of the database.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Database' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'database_description' - An optional description of the database.
--
-- 'parameters', 'database_parameters' - A set of custom key\/value pairs.
--
-- 'name', 'database_name' - The name of the database.
newDatabase ::
  -- | 'name'
  Core.Text ->
  Database
newDatabase pName_ =
  Database'
    { description = Core.Nothing,
      parameters = Core.Nothing,
      name = pName_
    }

-- | An optional description of the database.
database_description :: Lens.Lens' Database (Core.Maybe Core.Text)
database_description = Lens.lens (\Database' {description} -> description) (\s@Database' {} a -> s {description = a} :: Database)

-- | A set of custom key\/value pairs.
database_parameters :: Lens.Lens' Database (Core.Maybe (Core.HashMap Core.Text Core.Text))
database_parameters = Lens.lens (\Database' {parameters} -> parameters) (\s@Database' {} a -> s {parameters = a} :: Database) Core.. Lens.mapping Lens._Coerce

-- | The name of the database.
database_name :: Lens.Lens' Database Core.Text
database_name = Lens.lens (\Database' {name} -> name) (\s@Database' {} a -> s {name = a} :: Database)

instance Core.FromJSON Database where
  parseJSON =
    Core.withObject
      "Database"
      ( \x ->
          Database'
            Core.<$> (x Core..:? "Description")
            Core.<*> (x Core..:? "Parameters" Core..!= Core.mempty)
            Core.<*> (x Core..: "Name")
      )

instance Core.Hashable Database

instance Core.NFData Database
