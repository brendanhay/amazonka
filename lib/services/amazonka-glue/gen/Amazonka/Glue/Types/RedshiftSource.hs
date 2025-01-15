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
-- Module      : Amazonka.Glue.Types.RedshiftSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.RedshiftSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies an Amazon Redshift data store.
--
-- /See:/ 'newRedshiftSource' smart constructor.
data RedshiftSource = RedshiftSource'
  { -- | The Amazon S3 path where temporary data can be staged when copying out
    -- of the database.
    redshiftTmpDir :: Prelude.Maybe Prelude.Text,
    -- | The IAM role with permissions.
    tmpDirIAMRole :: Prelude.Maybe Prelude.Text,
    -- | The name of the Amazon Redshift data store.
    name :: Prelude.Text,
    -- | The database to read from.
    database :: Prelude.Text,
    -- | The database table to read from.
    table :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RedshiftSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'redshiftTmpDir', 'redshiftSource_redshiftTmpDir' - The Amazon S3 path where temporary data can be staged when copying out
-- of the database.
--
-- 'tmpDirIAMRole', 'redshiftSource_tmpDirIAMRole' - The IAM role with permissions.
--
-- 'name', 'redshiftSource_name' - The name of the Amazon Redshift data store.
--
-- 'database', 'redshiftSource_database' - The database to read from.
--
-- 'table', 'redshiftSource_table' - The database table to read from.
newRedshiftSource ::
  -- | 'name'
  Prelude.Text ->
  -- | 'database'
  Prelude.Text ->
  -- | 'table'
  Prelude.Text ->
  RedshiftSource
newRedshiftSource pName_ pDatabase_ pTable_ =
  RedshiftSource'
    { redshiftTmpDir = Prelude.Nothing,
      tmpDirIAMRole = Prelude.Nothing,
      name = pName_,
      database = pDatabase_,
      table = pTable_
    }

-- | The Amazon S3 path where temporary data can be staged when copying out
-- of the database.
redshiftSource_redshiftTmpDir :: Lens.Lens' RedshiftSource (Prelude.Maybe Prelude.Text)
redshiftSource_redshiftTmpDir = Lens.lens (\RedshiftSource' {redshiftTmpDir} -> redshiftTmpDir) (\s@RedshiftSource' {} a -> s {redshiftTmpDir = a} :: RedshiftSource)

-- | The IAM role with permissions.
redshiftSource_tmpDirIAMRole :: Lens.Lens' RedshiftSource (Prelude.Maybe Prelude.Text)
redshiftSource_tmpDirIAMRole = Lens.lens (\RedshiftSource' {tmpDirIAMRole} -> tmpDirIAMRole) (\s@RedshiftSource' {} a -> s {tmpDirIAMRole = a} :: RedshiftSource)

-- | The name of the Amazon Redshift data store.
redshiftSource_name :: Lens.Lens' RedshiftSource Prelude.Text
redshiftSource_name = Lens.lens (\RedshiftSource' {name} -> name) (\s@RedshiftSource' {} a -> s {name = a} :: RedshiftSource)

-- | The database to read from.
redshiftSource_database :: Lens.Lens' RedshiftSource Prelude.Text
redshiftSource_database = Lens.lens (\RedshiftSource' {database} -> database) (\s@RedshiftSource' {} a -> s {database = a} :: RedshiftSource)

-- | The database table to read from.
redshiftSource_table :: Lens.Lens' RedshiftSource Prelude.Text
redshiftSource_table = Lens.lens (\RedshiftSource' {table} -> table) (\s@RedshiftSource' {} a -> s {table = a} :: RedshiftSource)

instance Data.FromJSON RedshiftSource where
  parseJSON =
    Data.withObject
      "RedshiftSource"
      ( \x ->
          RedshiftSource'
            Prelude.<$> (x Data..:? "RedshiftTmpDir")
            Prelude.<*> (x Data..:? "TmpDirIAMRole")
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "Database")
            Prelude.<*> (x Data..: "Table")
      )

instance Prelude.Hashable RedshiftSource where
  hashWithSalt _salt RedshiftSource' {..} =
    _salt
      `Prelude.hashWithSalt` redshiftTmpDir
      `Prelude.hashWithSalt` tmpDirIAMRole
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` database
      `Prelude.hashWithSalt` table

instance Prelude.NFData RedshiftSource where
  rnf RedshiftSource' {..} =
    Prelude.rnf redshiftTmpDir `Prelude.seq`
      Prelude.rnf tmpDirIAMRole `Prelude.seq`
        Prelude.rnf name `Prelude.seq`
          Prelude.rnf database `Prelude.seq`
            Prelude.rnf table

instance Data.ToJSON RedshiftSource where
  toJSON RedshiftSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RedshiftTmpDir" Data..=)
              Prelude.<$> redshiftTmpDir,
            ("TmpDirIAMRole" Data..=) Prelude.<$> tmpDirIAMRole,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Database" Data..= database),
            Prelude.Just ("Table" Data..= table)
          ]
      )
