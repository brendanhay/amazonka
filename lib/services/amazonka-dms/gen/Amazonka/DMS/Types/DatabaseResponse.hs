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
-- Module      : Amazonka.DMS.Types.DatabaseResponse
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.DatabaseResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DMS.Types.CollectorShortInfoResponse
import Amazonka.DMS.Types.DatabaseInstanceSoftwareDetailsResponse
import Amazonka.DMS.Types.ServerShortInfoResponse
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a database in a Fleet Advisor collector inventory.
--
-- /See:/ 'newDatabaseResponse' smart constructor.
data DatabaseResponse = DatabaseResponse'
  { -- | The number of schemas in a Fleet Advisor collector inventory database.
    numberOfSchemas :: Prelude.Maybe Prelude.Integer,
    -- | The name of a database in a Fleet Advisor collector inventory.
    databaseName :: Prelude.Maybe Prelude.Text,
    -- | The ID of a database in a Fleet Advisor collector inventory.
    databaseId :: Prelude.Maybe Prelude.Text,
    -- | The server name of a database in a Fleet Advisor collector inventory.
    server :: Prelude.Maybe ServerShortInfoResponse,
    -- | A list of collectors associated with the database.
    collectors :: Prelude.Maybe [CollectorShortInfoResponse],
    -- | The IP address of a database in a Fleet Advisor collector inventory.
    ipAddress :: Prelude.Maybe Prelude.Text,
    -- | The software details of a database in a Fleet Advisor collector
    -- inventory, such as database engine and version.
    softwareDetails :: Prelude.Maybe DatabaseInstanceSoftwareDetailsResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatabaseResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'numberOfSchemas', 'databaseResponse_numberOfSchemas' - The number of schemas in a Fleet Advisor collector inventory database.
--
-- 'databaseName', 'databaseResponse_databaseName' - The name of a database in a Fleet Advisor collector inventory.
--
-- 'databaseId', 'databaseResponse_databaseId' - The ID of a database in a Fleet Advisor collector inventory.
--
-- 'server', 'databaseResponse_server' - The server name of a database in a Fleet Advisor collector inventory.
--
-- 'collectors', 'databaseResponse_collectors' - A list of collectors associated with the database.
--
-- 'ipAddress', 'databaseResponse_ipAddress' - The IP address of a database in a Fleet Advisor collector inventory.
--
-- 'softwareDetails', 'databaseResponse_softwareDetails' - The software details of a database in a Fleet Advisor collector
-- inventory, such as database engine and version.
newDatabaseResponse ::
  DatabaseResponse
newDatabaseResponse =
  DatabaseResponse'
    { numberOfSchemas =
        Prelude.Nothing,
      databaseName = Prelude.Nothing,
      databaseId = Prelude.Nothing,
      server = Prelude.Nothing,
      collectors = Prelude.Nothing,
      ipAddress = Prelude.Nothing,
      softwareDetails = Prelude.Nothing
    }

-- | The number of schemas in a Fleet Advisor collector inventory database.
databaseResponse_numberOfSchemas :: Lens.Lens' DatabaseResponse (Prelude.Maybe Prelude.Integer)
databaseResponse_numberOfSchemas = Lens.lens (\DatabaseResponse' {numberOfSchemas} -> numberOfSchemas) (\s@DatabaseResponse' {} a -> s {numberOfSchemas = a} :: DatabaseResponse)

-- | The name of a database in a Fleet Advisor collector inventory.
databaseResponse_databaseName :: Lens.Lens' DatabaseResponse (Prelude.Maybe Prelude.Text)
databaseResponse_databaseName = Lens.lens (\DatabaseResponse' {databaseName} -> databaseName) (\s@DatabaseResponse' {} a -> s {databaseName = a} :: DatabaseResponse)

-- | The ID of a database in a Fleet Advisor collector inventory.
databaseResponse_databaseId :: Lens.Lens' DatabaseResponse (Prelude.Maybe Prelude.Text)
databaseResponse_databaseId = Lens.lens (\DatabaseResponse' {databaseId} -> databaseId) (\s@DatabaseResponse' {} a -> s {databaseId = a} :: DatabaseResponse)

-- | The server name of a database in a Fleet Advisor collector inventory.
databaseResponse_server :: Lens.Lens' DatabaseResponse (Prelude.Maybe ServerShortInfoResponse)
databaseResponse_server = Lens.lens (\DatabaseResponse' {server} -> server) (\s@DatabaseResponse' {} a -> s {server = a} :: DatabaseResponse)

-- | A list of collectors associated with the database.
databaseResponse_collectors :: Lens.Lens' DatabaseResponse (Prelude.Maybe [CollectorShortInfoResponse])
databaseResponse_collectors = Lens.lens (\DatabaseResponse' {collectors} -> collectors) (\s@DatabaseResponse' {} a -> s {collectors = a} :: DatabaseResponse) Prelude.. Lens.mapping Lens.coerced

-- | The IP address of a database in a Fleet Advisor collector inventory.
databaseResponse_ipAddress :: Lens.Lens' DatabaseResponse (Prelude.Maybe Prelude.Text)
databaseResponse_ipAddress = Lens.lens (\DatabaseResponse' {ipAddress} -> ipAddress) (\s@DatabaseResponse' {} a -> s {ipAddress = a} :: DatabaseResponse)

-- | The software details of a database in a Fleet Advisor collector
-- inventory, such as database engine and version.
databaseResponse_softwareDetails :: Lens.Lens' DatabaseResponse (Prelude.Maybe DatabaseInstanceSoftwareDetailsResponse)
databaseResponse_softwareDetails = Lens.lens (\DatabaseResponse' {softwareDetails} -> softwareDetails) (\s@DatabaseResponse' {} a -> s {softwareDetails = a} :: DatabaseResponse)

instance Data.FromJSON DatabaseResponse where
  parseJSON =
    Data.withObject
      "DatabaseResponse"
      ( \x ->
          DatabaseResponse'
            Prelude.<$> (x Data..:? "NumberOfSchemas")
            Prelude.<*> (x Data..:? "DatabaseName")
            Prelude.<*> (x Data..:? "DatabaseId")
            Prelude.<*> (x Data..:? "Server")
            Prelude.<*> (x Data..:? "Collectors" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "IpAddress")
            Prelude.<*> (x Data..:? "SoftwareDetails")
      )

instance Prelude.Hashable DatabaseResponse where
  hashWithSalt _salt DatabaseResponse' {..} =
    _salt `Prelude.hashWithSalt` numberOfSchemas
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` databaseId
      `Prelude.hashWithSalt` server
      `Prelude.hashWithSalt` collectors
      `Prelude.hashWithSalt` ipAddress
      `Prelude.hashWithSalt` softwareDetails

instance Prelude.NFData DatabaseResponse where
  rnf DatabaseResponse' {..} =
    Prelude.rnf numberOfSchemas
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf databaseId
      `Prelude.seq` Prelude.rnf server
      `Prelude.seq` Prelude.rnf collectors
      `Prelude.seq` Prelude.rnf ipAddress
      `Prelude.seq` Prelude.rnf softwareDetails
