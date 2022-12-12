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
-- Module      : Amazonka.TimeStreamWrite.Types.Database
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TimeStreamWrite.Types.Database where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A top level container for a table. Databases and tables are the
-- fundamental management concepts in Amazon Timestream. All tables in a
-- database are encrypted with the same KMS key.
--
-- /See:/ 'newDatabase' smart constructor.
data Database = Database'
  { -- | The Amazon Resource Name that uniquely identifies this database.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time when the database was created, calculated from the Unix epoch
    -- time.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the Timestream database.
    databaseName :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the KMS key used to encrypt the data stored in the
    -- database.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The last time that this database was updated.
    lastUpdatedTime :: Prelude.Maybe Data.POSIX,
    -- | The total number of tables found within a Timestream database.
    tableCount :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Database' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'database_arn' - The Amazon Resource Name that uniquely identifies this database.
--
-- 'creationTime', 'database_creationTime' - The time when the database was created, calculated from the Unix epoch
-- time.
--
-- 'databaseName', 'database_databaseName' - The name of the Timestream database.
--
-- 'kmsKeyId', 'database_kmsKeyId' - The identifier of the KMS key used to encrypt the data stored in the
-- database.
--
-- 'lastUpdatedTime', 'database_lastUpdatedTime' - The last time that this database was updated.
--
-- 'tableCount', 'database_tableCount' - The total number of tables found within a Timestream database.
newDatabase ::
  Database
newDatabase =
  Database'
    { arn = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      databaseName = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      tableCount = Prelude.Nothing
    }

-- | The Amazon Resource Name that uniquely identifies this database.
database_arn :: Lens.Lens' Database (Prelude.Maybe Prelude.Text)
database_arn = Lens.lens (\Database' {arn} -> arn) (\s@Database' {} a -> s {arn = a} :: Database)

-- | The time when the database was created, calculated from the Unix epoch
-- time.
database_creationTime :: Lens.Lens' Database (Prelude.Maybe Prelude.UTCTime)
database_creationTime = Lens.lens (\Database' {creationTime} -> creationTime) (\s@Database' {} a -> s {creationTime = a} :: Database) Prelude.. Lens.mapping Data._Time

-- | The name of the Timestream database.
database_databaseName :: Lens.Lens' Database (Prelude.Maybe Prelude.Text)
database_databaseName = Lens.lens (\Database' {databaseName} -> databaseName) (\s@Database' {} a -> s {databaseName = a} :: Database)

-- | The identifier of the KMS key used to encrypt the data stored in the
-- database.
database_kmsKeyId :: Lens.Lens' Database (Prelude.Maybe Prelude.Text)
database_kmsKeyId = Lens.lens (\Database' {kmsKeyId} -> kmsKeyId) (\s@Database' {} a -> s {kmsKeyId = a} :: Database)

-- | The last time that this database was updated.
database_lastUpdatedTime :: Lens.Lens' Database (Prelude.Maybe Prelude.UTCTime)
database_lastUpdatedTime = Lens.lens (\Database' {lastUpdatedTime} -> lastUpdatedTime) (\s@Database' {} a -> s {lastUpdatedTime = a} :: Database) Prelude.. Lens.mapping Data._Time

-- | The total number of tables found within a Timestream database.
database_tableCount :: Lens.Lens' Database (Prelude.Maybe Prelude.Integer)
database_tableCount = Lens.lens (\Database' {tableCount} -> tableCount) (\s@Database' {} a -> s {tableCount = a} :: Database)

instance Data.FromJSON Database where
  parseJSON =
    Data.withObject
      "Database"
      ( \x ->
          Database'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "DatabaseName")
            Prelude.<*> (x Data..:? "KmsKeyId")
            Prelude.<*> (x Data..:? "LastUpdatedTime")
            Prelude.<*> (x Data..:? "TableCount")
      )

instance Prelude.Hashable Database where
  hashWithSalt _salt Database' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` tableCount

instance Prelude.NFData Database where
  rnf Database' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf tableCount
