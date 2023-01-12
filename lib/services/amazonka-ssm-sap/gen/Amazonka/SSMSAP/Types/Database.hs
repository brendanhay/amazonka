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
-- Module      : Amazonka.SSMSAP.Types.Database
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMSAP.Types.Database where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSMSAP.Types.ApplicationCredential
import Amazonka.SSMSAP.Types.DatabaseStatus
import Amazonka.SSMSAP.Types.DatabaseType

-- |
--
-- /See:/ 'newDatabase' smart constructor.
data Database = Database'
  { applicationId :: Prelude.Maybe Prelude.Text,
    arn :: Prelude.Maybe Prelude.Text,
    componentId :: Prelude.Maybe Prelude.Text,
    credentials :: Prelude.Maybe (Prelude.NonEmpty ApplicationCredential),
    databaseId :: Prelude.Maybe Prelude.Text,
    databaseName :: Prelude.Maybe Prelude.Text,
    databaseType :: Prelude.Maybe DatabaseType,
    lastUpdated :: Prelude.Maybe Data.POSIX,
    primaryHost :: Prelude.Maybe Prelude.Text,
    sQLPort :: Prelude.Maybe Prelude.Int,
    status :: Prelude.Maybe DatabaseStatus
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Database' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'database_applicationId' -
--
-- 'arn', 'database_arn' -
--
-- 'componentId', 'database_componentId' -
--
-- 'credentials', 'database_credentials' -
--
-- 'databaseId', 'database_databaseId' -
--
-- 'databaseName', 'database_databaseName' -
--
-- 'databaseType', 'database_databaseType' -
--
-- 'lastUpdated', 'database_lastUpdated' -
--
-- 'primaryHost', 'database_primaryHost' -
--
-- 'sQLPort', 'database_sQLPort' -
--
-- 'status', 'database_status' -
newDatabase ::
  Database
newDatabase =
  Database'
    { applicationId = Prelude.Nothing,
      arn = Prelude.Nothing,
      componentId = Prelude.Nothing,
      credentials = Prelude.Nothing,
      databaseId = Prelude.Nothing,
      databaseName = Prelude.Nothing,
      databaseType = Prelude.Nothing,
      lastUpdated = Prelude.Nothing,
      primaryHost = Prelude.Nothing,
      sQLPort = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- |
database_applicationId :: Lens.Lens' Database (Prelude.Maybe Prelude.Text)
database_applicationId = Lens.lens (\Database' {applicationId} -> applicationId) (\s@Database' {} a -> s {applicationId = a} :: Database)

-- |
database_arn :: Lens.Lens' Database (Prelude.Maybe Prelude.Text)
database_arn = Lens.lens (\Database' {arn} -> arn) (\s@Database' {} a -> s {arn = a} :: Database)

-- |
database_componentId :: Lens.Lens' Database (Prelude.Maybe Prelude.Text)
database_componentId = Lens.lens (\Database' {componentId} -> componentId) (\s@Database' {} a -> s {componentId = a} :: Database)

-- |
database_credentials :: Lens.Lens' Database (Prelude.Maybe (Prelude.NonEmpty ApplicationCredential))
database_credentials = Lens.lens (\Database' {credentials} -> credentials) (\s@Database' {} a -> s {credentials = a} :: Database) Prelude.. Lens.mapping Lens.coerced

-- |
database_databaseId :: Lens.Lens' Database (Prelude.Maybe Prelude.Text)
database_databaseId = Lens.lens (\Database' {databaseId} -> databaseId) (\s@Database' {} a -> s {databaseId = a} :: Database)

-- |
database_databaseName :: Lens.Lens' Database (Prelude.Maybe Prelude.Text)
database_databaseName = Lens.lens (\Database' {databaseName} -> databaseName) (\s@Database' {} a -> s {databaseName = a} :: Database)

-- |
database_databaseType :: Lens.Lens' Database (Prelude.Maybe DatabaseType)
database_databaseType = Lens.lens (\Database' {databaseType} -> databaseType) (\s@Database' {} a -> s {databaseType = a} :: Database)

-- |
database_lastUpdated :: Lens.Lens' Database (Prelude.Maybe Prelude.UTCTime)
database_lastUpdated = Lens.lens (\Database' {lastUpdated} -> lastUpdated) (\s@Database' {} a -> s {lastUpdated = a} :: Database) Prelude.. Lens.mapping Data._Time

-- |
database_primaryHost :: Lens.Lens' Database (Prelude.Maybe Prelude.Text)
database_primaryHost = Lens.lens (\Database' {primaryHost} -> primaryHost) (\s@Database' {} a -> s {primaryHost = a} :: Database)

-- |
database_sQLPort :: Lens.Lens' Database (Prelude.Maybe Prelude.Int)
database_sQLPort = Lens.lens (\Database' {sQLPort} -> sQLPort) (\s@Database' {} a -> s {sQLPort = a} :: Database)

-- |
database_status :: Lens.Lens' Database (Prelude.Maybe DatabaseStatus)
database_status = Lens.lens (\Database' {status} -> status) (\s@Database' {} a -> s {status = a} :: Database)

instance Data.FromJSON Database where
  parseJSON =
    Data.withObject
      "Database"
      ( \x ->
          Database'
            Prelude.<$> (x Data..:? "ApplicationId")
            Prelude.<*> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "ComponentId")
            Prelude.<*> (x Data..:? "Credentials")
            Prelude.<*> (x Data..:? "DatabaseId")
            Prelude.<*> (x Data..:? "DatabaseName")
            Prelude.<*> (x Data..:? "DatabaseType")
            Prelude.<*> (x Data..:? "LastUpdated")
            Prelude.<*> (x Data..:? "PrimaryHost")
            Prelude.<*> (x Data..:? "SQLPort")
            Prelude.<*> (x Data..:? "Status")
      )

instance Prelude.Hashable Database where
  hashWithSalt _salt Database' {..} =
    _salt `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` componentId
      `Prelude.hashWithSalt` credentials
      `Prelude.hashWithSalt` databaseId
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` databaseType
      `Prelude.hashWithSalt` lastUpdated
      `Prelude.hashWithSalt` primaryHost
      `Prelude.hashWithSalt` sQLPort
      `Prelude.hashWithSalt` status

instance Prelude.NFData Database where
  rnf Database' {..} =
    Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf componentId
      `Prelude.seq` Prelude.rnf credentials
      `Prelude.seq` Prelude.rnf databaseId
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf databaseType
      `Prelude.seq` Prelude.rnf lastUpdated
      `Prelude.seq` Prelude.rnf primaryHost
      `Prelude.seq` Prelude.rnf sQLPort
      `Prelude.seq` Prelude.rnf status
