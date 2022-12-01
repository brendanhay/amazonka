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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMSAP.Types.Database where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSMSAP.Types.ApplicationCredential
import Amazonka.SSMSAP.Types.DatabaseStatus
import Amazonka.SSMSAP.Types.DatabaseType

-- |
--
-- /See:/ 'newDatabase' smart constructor.
data Database = Database'
  { primaryHost :: Prelude.Maybe Prelude.Text,
    sQLPort :: Prelude.Maybe Prelude.Int,
    databaseName :: Prelude.Maybe Prelude.Text,
    arn :: Prelude.Maybe Prelude.Text,
    databaseType :: Prelude.Maybe DatabaseType,
    databaseId :: Prelude.Maybe Prelude.Text,
    status :: Prelude.Maybe DatabaseStatus,
    lastUpdated :: Prelude.Maybe Core.POSIX,
    credentials :: Prelude.Maybe (Prelude.NonEmpty ApplicationCredential),
    componentId :: Prelude.Maybe Prelude.Text,
    applicationId :: Prelude.Maybe Prelude.Text
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
-- 'primaryHost', 'database_primaryHost' -
--
-- 'sQLPort', 'database_sQLPort' -
--
-- 'databaseName', 'database_databaseName' -
--
-- 'arn', 'database_arn' -
--
-- 'databaseType', 'database_databaseType' -
--
-- 'databaseId', 'database_databaseId' -
--
-- 'status', 'database_status' -
--
-- 'lastUpdated', 'database_lastUpdated' -
--
-- 'credentials', 'database_credentials' -
--
-- 'componentId', 'database_componentId' -
--
-- 'applicationId', 'database_applicationId' -
newDatabase ::
  Database
newDatabase =
  Database'
    { primaryHost = Prelude.Nothing,
      sQLPort = Prelude.Nothing,
      databaseName = Prelude.Nothing,
      arn = Prelude.Nothing,
      databaseType = Prelude.Nothing,
      databaseId = Prelude.Nothing,
      status = Prelude.Nothing,
      lastUpdated = Prelude.Nothing,
      credentials = Prelude.Nothing,
      componentId = Prelude.Nothing,
      applicationId = Prelude.Nothing
    }

-- |
database_primaryHost :: Lens.Lens' Database (Prelude.Maybe Prelude.Text)
database_primaryHost = Lens.lens (\Database' {primaryHost} -> primaryHost) (\s@Database' {} a -> s {primaryHost = a} :: Database)

-- |
database_sQLPort :: Lens.Lens' Database (Prelude.Maybe Prelude.Int)
database_sQLPort = Lens.lens (\Database' {sQLPort} -> sQLPort) (\s@Database' {} a -> s {sQLPort = a} :: Database)

-- |
database_databaseName :: Lens.Lens' Database (Prelude.Maybe Prelude.Text)
database_databaseName = Lens.lens (\Database' {databaseName} -> databaseName) (\s@Database' {} a -> s {databaseName = a} :: Database)

-- |
database_arn :: Lens.Lens' Database (Prelude.Maybe Prelude.Text)
database_arn = Lens.lens (\Database' {arn} -> arn) (\s@Database' {} a -> s {arn = a} :: Database)

-- |
database_databaseType :: Lens.Lens' Database (Prelude.Maybe DatabaseType)
database_databaseType = Lens.lens (\Database' {databaseType} -> databaseType) (\s@Database' {} a -> s {databaseType = a} :: Database)

-- |
database_databaseId :: Lens.Lens' Database (Prelude.Maybe Prelude.Text)
database_databaseId = Lens.lens (\Database' {databaseId} -> databaseId) (\s@Database' {} a -> s {databaseId = a} :: Database)

-- |
database_status :: Lens.Lens' Database (Prelude.Maybe DatabaseStatus)
database_status = Lens.lens (\Database' {status} -> status) (\s@Database' {} a -> s {status = a} :: Database)

-- |
database_lastUpdated :: Lens.Lens' Database (Prelude.Maybe Prelude.UTCTime)
database_lastUpdated = Lens.lens (\Database' {lastUpdated} -> lastUpdated) (\s@Database' {} a -> s {lastUpdated = a} :: Database) Prelude.. Lens.mapping Core._Time

-- |
database_credentials :: Lens.Lens' Database (Prelude.Maybe (Prelude.NonEmpty ApplicationCredential))
database_credentials = Lens.lens (\Database' {credentials} -> credentials) (\s@Database' {} a -> s {credentials = a} :: Database) Prelude.. Lens.mapping Lens.coerced

-- |
database_componentId :: Lens.Lens' Database (Prelude.Maybe Prelude.Text)
database_componentId = Lens.lens (\Database' {componentId} -> componentId) (\s@Database' {} a -> s {componentId = a} :: Database)

-- |
database_applicationId :: Lens.Lens' Database (Prelude.Maybe Prelude.Text)
database_applicationId = Lens.lens (\Database' {applicationId} -> applicationId) (\s@Database' {} a -> s {applicationId = a} :: Database)

instance Core.FromJSON Database where
  parseJSON =
    Core.withObject
      "Database"
      ( \x ->
          Database'
            Prelude.<$> (x Core..:? "PrimaryHost")
            Prelude.<*> (x Core..:? "SQLPort")
            Prelude.<*> (x Core..:? "DatabaseName")
            Prelude.<*> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "DatabaseType")
            Prelude.<*> (x Core..:? "DatabaseId")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "LastUpdated")
            Prelude.<*> (x Core..:? "Credentials")
            Prelude.<*> (x Core..:? "ComponentId")
            Prelude.<*> (x Core..:? "ApplicationId")
      )

instance Prelude.Hashable Database where
  hashWithSalt _salt Database' {..} =
    _salt `Prelude.hashWithSalt` primaryHost
      `Prelude.hashWithSalt` sQLPort
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` databaseType
      `Prelude.hashWithSalt` databaseId
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` lastUpdated
      `Prelude.hashWithSalt` credentials
      `Prelude.hashWithSalt` componentId
      `Prelude.hashWithSalt` applicationId

instance Prelude.NFData Database where
  rnf Database' {..} =
    Prelude.rnf primaryHost
      `Prelude.seq` Prelude.rnf sQLPort
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf databaseType
      `Prelude.seq` Prelude.rnf databaseId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf lastUpdated
      `Prelude.seq` Prelude.rnf credentials
      `Prelude.seq` Prelude.rnf componentId
      `Prelude.seq` Prelude.rnf applicationId
