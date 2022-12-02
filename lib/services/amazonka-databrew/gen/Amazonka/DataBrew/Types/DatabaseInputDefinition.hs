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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataBrew.Types.DatabaseInputDefinition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataBrew.Types.S3Location
import qualified Amazonka.Prelude as Prelude

-- | Connection information for dataset input files stored in a database.
--
-- /See:/ 'newDatabaseInputDefinition' smart constructor.
data DatabaseInputDefinition = DatabaseInputDefinition'
  { tempDirectory :: Prelude.Maybe S3Location,
    -- | The table within the target database.
    databaseTableName :: Prelude.Maybe Prelude.Text,
    -- | Custom SQL to run against the provided Glue connection. This SQL will be
    -- used as the input for DataBrew projects and jobs.
    queryString :: Prelude.Maybe Prelude.Text,
    -- | The Glue Connection that stores the connection information for the
    -- target database.
    glueConnectionName :: Prelude.Text
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
-- 'databaseTableName', 'databaseInputDefinition_databaseTableName' - The table within the target database.
--
-- 'queryString', 'databaseInputDefinition_queryString' - Custom SQL to run against the provided Glue connection. This SQL will be
-- used as the input for DataBrew projects and jobs.
--
-- 'glueConnectionName', 'databaseInputDefinition_glueConnectionName' - The Glue Connection that stores the connection information for the
-- target database.
newDatabaseInputDefinition ::
  -- | 'glueConnectionName'
  Prelude.Text ->
  DatabaseInputDefinition
newDatabaseInputDefinition pGlueConnectionName_ =
  DatabaseInputDefinition'
    { tempDirectory =
        Prelude.Nothing,
      databaseTableName = Prelude.Nothing,
      queryString = Prelude.Nothing,
      glueConnectionName = pGlueConnectionName_
    }

-- | Undocumented member.
databaseInputDefinition_tempDirectory :: Lens.Lens' DatabaseInputDefinition (Prelude.Maybe S3Location)
databaseInputDefinition_tempDirectory = Lens.lens (\DatabaseInputDefinition' {tempDirectory} -> tempDirectory) (\s@DatabaseInputDefinition' {} a -> s {tempDirectory = a} :: DatabaseInputDefinition)

-- | The table within the target database.
databaseInputDefinition_databaseTableName :: Lens.Lens' DatabaseInputDefinition (Prelude.Maybe Prelude.Text)
databaseInputDefinition_databaseTableName = Lens.lens (\DatabaseInputDefinition' {databaseTableName} -> databaseTableName) (\s@DatabaseInputDefinition' {} a -> s {databaseTableName = a} :: DatabaseInputDefinition)

-- | Custom SQL to run against the provided Glue connection. This SQL will be
-- used as the input for DataBrew projects and jobs.
databaseInputDefinition_queryString :: Lens.Lens' DatabaseInputDefinition (Prelude.Maybe Prelude.Text)
databaseInputDefinition_queryString = Lens.lens (\DatabaseInputDefinition' {queryString} -> queryString) (\s@DatabaseInputDefinition' {} a -> s {queryString = a} :: DatabaseInputDefinition)

-- | The Glue Connection that stores the connection information for the
-- target database.
databaseInputDefinition_glueConnectionName :: Lens.Lens' DatabaseInputDefinition Prelude.Text
databaseInputDefinition_glueConnectionName = Lens.lens (\DatabaseInputDefinition' {glueConnectionName} -> glueConnectionName) (\s@DatabaseInputDefinition' {} a -> s {glueConnectionName = a} :: DatabaseInputDefinition)

instance Data.FromJSON DatabaseInputDefinition where
  parseJSON =
    Data.withObject
      "DatabaseInputDefinition"
      ( \x ->
          DatabaseInputDefinition'
            Prelude.<$> (x Data..:? "TempDirectory")
            Prelude.<*> (x Data..:? "DatabaseTableName")
            Prelude.<*> (x Data..:? "QueryString")
            Prelude.<*> (x Data..: "GlueConnectionName")
      )

instance Prelude.Hashable DatabaseInputDefinition where
  hashWithSalt _salt DatabaseInputDefinition' {..} =
    _salt `Prelude.hashWithSalt` tempDirectory
      `Prelude.hashWithSalt` databaseTableName
      `Prelude.hashWithSalt` queryString
      `Prelude.hashWithSalt` glueConnectionName

instance Prelude.NFData DatabaseInputDefinition where
  rnf DatabaseInputDefinition' {..} =
    Prelude.rnf tempDirectory
      `Prelude.seq` Prelude.rnf databaseTableName
      `Prelude.seq` Prelude.rnf queryString
      `Prelude.seq` Prelude.rnf glueConnectionName

instance Data.ToJSON DatabaseInputDefinition where
  toJSON DatabaseInputDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("TempDirectory" Data..=) Prelude.<$> tempDirectory,
            ("DatabaseTableName" Data..=)
              Prelude.<$> databaseTableName,
            ("QueryString" Data..=) Prelude.<$> queryString,
            Prelude.Just
              ("GlueConnectionName" Data..= glueConnectionName)
          ]
      )
