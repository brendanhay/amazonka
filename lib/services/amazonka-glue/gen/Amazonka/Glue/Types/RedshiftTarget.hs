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
-- Module      : Amazonka.Glue.Types.RedshiftTarget
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.RedshiftTarget where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.UpsertRedshiftTargetOptions
import qualified Amazonka.Prelude as Prelude

-- | Specifies a target that uses Amazon Redshift.
--
-- /See:/ 'newRedshiftTarget' smart constructor.
data RedshiftTarget = RedshiftTarget'
  { -- | The Amazon S3 path where temporary data can be staged when copying out
    -- of the database.
    redshiftTmpDir :: Prelude.Maybe Prelude.Text,
    -- | The IAM role with permissions.
    tmpDirIAMRole :: Prelude.Maybe Prelude.Text,
    -- | The set of options to configure an upsert operation when writing to a
    -- Redshift target.
    upsertRedshiftOptions :: Prelude.Maybe UpsertRedshiftTargetOptions,
    -- | The name of the data target.
    name :: Prelude.Text,
    -- | The nodes that are inputs to the data target.
    inputs :: Prelude.NonEmpty Prelude.Text,
    -- | The name of the database to write to.
    database :: Prelude.Text,
    -- | The name of the table in the database to write to.
    table :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RedshiftTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'redshiftTmpDir', 'redshiftTarget_redshiftTmpDir' - The Amazon S3 path where temporary data can be staged when copying out
-- of the database.
--
-- 'tmpDirIAMRole', 'redshiftTarget_tmpDirIAMRole' - The IAM role with permissions.
--
-- 'upsertRedshiftOptions', 'redshiftTarget_upsertRedshiftOptions' - The set of options to configure an upsert operation when writing to a
-- Redshift target.
--
-- 'name', 'redshiftTarget_name' - The name of the data target.
--
-- 'inputs', 'redshiftTarget_inputs' - The nodes that are inputs to the data target.
--
-- 'database', 'redshiftTarget_database' - The name of the database to write to.
--
-- 'table', 'redshiftTarget_table' - The name of the table in the database to write to.
newRedshiftTarget ::
  -- | 'name'
  Prelude.Text ->
  -- | 'inputs'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'database'
  Prelude.Text ->
  -- | 'table'
  Prelude.Text ->
  RedshiftTarget
newRedshiftTarget pName_ pInputs_ pDatabase_ pTable_ =
  RedshiftTarget'
    { redshiftTmpDir = Prelude.Nothing,
      tmpDirIAMRole = Prelude.Nothing,
      upsertRedshiftOptions = Prelude.Nothing,
      name = pName_,
      inputs = Lens.coerced Lens.# pInputs_,
      database = pDatabase_,
      table = pTable_
    }

-- | The Amazon S3 path where temporary data can be staged when copying out
-- of the database.
redshiftTarget_redshiftTmpDir :: Lens.Lens' RedshiftTarget (Prelude.Maybe Prelude.Text)
redshiftTarget_redshiftTmpDir = Lens.lens (\RedshiftTarget' {redshiftTmpDir} -> redshiftTmpDir) (\s@RedshiftTarget' {} a -> s {redshiftTmpDir = a} :: RedshiftTarget)

-- | The IAM role with permissions.
redshiftTarget_tmpDirIAMRole :: Lens.Lens' RedshiftTarget (Prelude.Maybe Prelude.Text)
redshiftTarget_tmpDirIAMRole = Lens.lens (\RedshiftTarget' {tmpDirIAMRole} -> tmpDirIAMRole) (\s@RedshiftTarget' {} a -> s {tmpDirIAMRole = a} :: RedshiftTarget)

-- | The set of options to configure an upsert operation when writing to a
-- Redshift target.
redshiftTarget_upsertRedshiftOptions :: Lens.Lens' RedshiftTarget (Prelude.Maybe UpsertRedshiftTargetOptions)
redshiftTarget_upsertRedshiftOptions = Lens.lens (\RedshiftTarget' {upsertRedshiftOptions} -> upsertRedshiftOptions) (\s@RedshiftTarget' {} a -> s {upsertRedshiftOptions = a} :: RedshiftTarget)

-- | The name of the data target.
redshiftTarget_name :: Lens.Lens' RedshiftTarget Prelude.Text
redshiftTarget_name = Lens.lens (\RedshiftTarget' {name} -> name) (\s@RedshiftTarget' {} a -> s {name = a} :: RedshiftTarget)

-- | The nodes that are inputs to the data target.
redshiftTarget_inputs :: Lens.Lens' RedshiftTarget (Prelude.NonEmpty Prelude.Text)
redshiftTarget_inputs = Lens.lens (\RedshiftTarget' {inputs} -> inputs) (\s@RedshiftTarget' {} a -> s {inputs = a} :: RedshiftTarget) Prelude.. Lens.coerced

-- | The name of the database to write to.
redshiftTarget_database :: Lens.Lens' RedshiftTarget Prelude.Text
redshiftTarget_database = Lens.lens (\RedshiftTarget' {database} -> database) (\s@RedshiftTarget' {} a -> s {database = a} :: RedshiftTarget)

-- | The name of the table in the database to write to.
redshiftTarget_table :: Lens.Lens' RedshiftTarget Prelude.Text
redshiftTarget_table = Lens.lens (\RedshiftTarget' {table} -> table) (\s@RedshiftTarget' {} a -> s {table = a} :: RedshiftTarget)

instance Data.FromJSON RedshiftTarget where
  parseJSON =
    Data.withObject
      "RedshiftTarget"
      ( \x ->
          RedshiftTarget'
            Prelude.<$> (x Data..:? "RedshiftTmpDir")
            Prelude.<*> (x Data..:? "TmpDirIAMRole")
            Prelude.<*> (x Data..:? "UpsertRedshiftOptions")
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "Inputs")
            Prelude.<*> (x Data..: "Database")
            Prelude.<*> (x Data..: "Table")
      )

instance Prelude.Hashable RedshiftTarget where
  hashWithSalt _salt RedshiftTarget' {..} =
    _salt
      `Prelude.hashWithSalt` redshiftTmpDir
      `Prelude.hashWithSalt` tmpDirIAMRole
      `Prelude.hashWithSalt` upsertRedshiftOptions
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` inputs
      `Prelude.hashWithSalt` database
      `Prelude.hashWithSalt` table

instance Prelude.NFData RedshiftTarget where
  rnf RedshiftTarget' {..} =
    Prelude.rnf redshiftTmpDir
      `Prelude.seq` Prelude.rnf tmpDirIAMRole
      `Prelude.seq` Prelude.rnf upsertRedshiftOptions
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf inputs
      `Prelude.seq` Prelude.rnf database
      `Prelude.seq` Prelude.rnf table

instance Data.ToJSON RedshiftTarget where
  toJSON RedshiftTarget' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RedshiftTmpDir" Data..=)
              Prelude.<$> redshiftTmpDir,
            ("TmpDirIAMRole" Data..=) Prelude.<$> tmpDirIAMRole,
            ("UpsertRedshiftOptions" Data..=)
              Prelude.<$> upsertRedshiftOptions,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Inputs" Data..= inputs),
            Prelude.Just ("Database" Data..= database),
            Prelude.Just ("Table" Data..= table)
          ]
      )
