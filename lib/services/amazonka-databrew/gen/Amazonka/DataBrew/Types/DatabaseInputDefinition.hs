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
-- Module      : Amazonka.DataBrew.Types.DatabaseInputDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataBrew.Types.DatabaseInputDefinition where

import qualified Amazonka.Core as Core
import Amazonka.DataBrew.Types.S3Location
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Connection information for dataset input files stored in a database.
--
-- /See:/ 'newDatabaseInputDefinition' smart constructor.
data DatabaseInputDefinition = DatabaseInputDefinition'
  { tempDirectory :: Prelude.Maybe S3Location,
    -- | The Glue Connection that stores the connection information for the
    -- target database.
    glueConnectionName :: Prelude.Text,
    -- | The table within the target database.
    databaseTableName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatabaseInputDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tempDirectory', 'databaseInputDefinition_tempDirectory' - Undocumented member.
--
-- 'glueConnectionName', 'databaseInputDefinition_glueConnectionName' - The Glue Connection that stores the connection information for the
-- target database.
--
-- 'databaseTableName', 'databaseInputDefinition_databaseTableName' - The table within the target database.
newDatabaseInputDefinition ::
  -- | 'glueConnectionName'
  Prelude.Text ->
  -- | 'databaseTableName'
  Prelude.Text ->
  DatabaseInputDefinition
newDatabaseInputDefinition
  pGlueConnectionName_
  pDatabaseTableName_ =
    DatabaseInputDefinition'
      { tempDirectory =
          Prelude.Nothing,
        glueConnectionName = pGlueConnectionName_,
        databaseTableName = pDatabaseTableName_
      }

-- | Undocumented member.
databaseInputDefinition_tempDirectory :: Lens.Lens' DatabaseInputDefinition (Prelude.Maybe S3Location)
databaseInputDefinition_tempDirectory = Lens.lens (\DatabaseInputDefinition' {tempDirectory} -> tempDirectory) (\s@DatabaseInputDefinition' {} a -> s {tempDirectory = a} :: DatabaseInputDefinition)

-- | The Glue Connection that stores the connection information for the
-- target database.
databaseInputDefinition_glueConnectionName :: Lens.Lens' DatabaseInputDefinition Prelude.Text
databaseInputDefinition_glueConnectionName = Lens.lens (\DatabaseInputDefinition' {glueConnectionName} -> glueConnectionName) (\s@DatabaseInputDefinition' {} a -> s {glueConnectionName = a} :: DatabaseInputDefinition)

-- | The table within the target database.
databaseInputDefinition_databaseTableName :: Lens.Lens' DatabaseInputDefinition Prelude.Text
databaseInputDefinition_databaseTableName = Lens.lens (\DatabaseInputDefinition' {databaseTableName} -> databaseTableName) (\s@DatabaseInputDefinition' {} a -> s {databaseTableName = a} :: DatabaseInputDefinition)

instance Core.FromJSON DatabaseInputDefinition where
  parseJSON =
    Core.withObject
      "DatabaseInputDefinition"
      ( \x ->
          DatabaseInputDefinition'
            Prelude.<$> (x Core..:? "TempDirectory")
            Prelude.<*> (x Core..: "GlueConnectionName")
            Prelude.<*> (x Core..: "DatabaseTableName")
      )

instance Prelude.Hashable DatabaseInputDefinition where
  hashWithSalt _salt DatabaseInputDefinition' {..} =
    _salt `Prelude.hashWithSalt` tempDirectory
      `Prelude.hashWithSalt` glueConnectionName
      `Prelude.hashWithSalt` databaseTableName

instance Prelude.NFData DatabaseInputDefinition where
  rnf DatabaseInputDefinition' {..} =
    Prelude.rnf tempDirectory
      `Prelude.seq` Prelude.rnf glueConnectionName
      `Prelude.seq` Prelude.rnf databaseTableName

instance Core.ToJSON DatabaseInputDefinition where
  toJSON DatabaseInputDefinition' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("TempDirectory" Core..=) Prelude.<$> tempDirectory,
            Prelude.Just
              ("GlueConnectionName" Core..= glueConnectionName),
            Prelude.Just
              ("DatabaseTableName" Core..= databaseTableName)
          ]
      )
