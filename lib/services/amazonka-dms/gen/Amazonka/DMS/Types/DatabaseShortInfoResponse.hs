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
-- Module      : Amazonka.DMS.Types.DatabaseShortInfoResponse
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.DatabaseShortInfoResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a database in a Fleet Advisor collector inventory.
--
-- /See:/ 'newDatabaseShortInfoResponse' smart constructor.
data DatabaseShortInfoResponse = DatabaseShortInfoResponse'
  { -- | The database engine of a database in a Fleet Advisor collector
    -- inventory, for example @PostgreSQL@.
    databaseEngine :: Prelude.Maybe Prelude.Text,
    -- | The name of a database in a Fleet Advisor collector inventory.
    databaseName :: Prelude.Maybe Prelude.Text,
    -- | The ID of a database in a Fleet Advisor collector inventory.
    databaseId :: Prelude.Maybe Prelude.Text,
    -- | The IP address of a database in a Fleet Advisor collector inventory.
    databaseIpAddress :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatabaseShortInfoResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'databaseEngine', 'databaseShortInfoResponse_databaseEngine' - The database engine of a database in a Fleet Advisor collector
-- inventory, for example @PostgreSQL@.
--
-- 'databaseName', 'databaseShortInfoResponse_databaseName' - The name of a database in a Fleet Advisor collector inventory.
--
-- 'databaseId', 'databaseShortInfoResponse_databaseId' - The ID of a database in a Fleet Advisor collector inventory.
--
-- 'databaseIpAddress', 'databaseShortInfoResponse_databaseIpAddress' - The IP address of a database in a Fleet Advisor collector inventory.
newDatabaseShortInfoResponse ::
  DatabaseShortInfoResponse
newDatabaseShortInfoResponse =
  DatabaseShortInfoResponse'
    { databaseEngine =
        Prelude.Nothing,
      databaseName = Prelude.Nothing,
      databaseId = Prelude.Nothing,
      databaseIpAddress = Prelude.Nothing
    }

-- | The database engine of a database in a Fleet Advisor collector
-- inventory, for example @PostgreSQL@.
databaseShortInfoResponse_databaseEngine :: Lens.Lens' DatabaseShortInfoResponse (Prelude.Maybe Prelude.Text)
databaseShortInfoResponse_databaseEngine = Lens.lens (\DatabaseShortInfoResponse' {databaseEngine} -> databaseEngine) (\s@DatabaseShortInfoResponse' {} a -> s {databaseEngine = a} :: DatabaseShortInfoResponse)

-- | The name of a database in a Fleet Advisor collector inventory.
databaseShortInfoResponse_databaseName :: Lens.Lens' DatabaseShortInfoResponse (Prelude.Maybe Prelude.Text)
databaseShortInfoResponse_databaseName = Lens.lens (\DatabaseShortInfoResponse' {databaseName} -> databaseName) (\s@DatabaseShortInfoResponse' {} a -> s {databaseName = a} :: DatabaseShortInfoResponse)

-- | The ID of a database in a Fleet Advisor collector inventory.
databaseShortInfoResponse_databaseId :: Lens.Lens' DatabaseShortInfoResponse (Prelude.Maybe Prelude.Text)
databaseShortInfoResponse_databaseId = Lens.lens (\DatabaseShortInfoResponse' {databaseId} -> databaseId) (\s@DatabaseShortInfoResponse' {} a -> s {databaseId = a} :: DatabaseShortInfoResponse)

-- | The IP address of a database in a Fleet Advisor collector inventory.
databaseShortInfoResponse_databaseIpAddress :: Lens.Lens' DatabaseShortInfoResponse (Prelude.Maybe Prelude.Text)
databaseShortInfoResponse_databaseIpAddress = Lens.lens (\DatabaseShortInfoResponse' {databaseIpAddress} -> databaseIpAddress) (\s@DatabaseShortInfoResponse' {} a -> s {databaseIpAddress = a} :: DatabaseShortInfoResponse)

instance Data.FromJSON DatabaseShortInfoResponse where
  parseJSON =
    Data.withObject
      "DatabaseShortInfoResponse"
      ( \x ->
          DatabaseShortInfoResponse'
            Prelude.<$> (x Data..:? "DatabaseEngine")
            Prelude.<*> (x Data..:? "DatabaseName")
            Prelude.<*> (x Data..:? "DatabaseId")
            Prelude.<*> (x Data..:? "DatabaseIpAddress")
      )

instance Prelude.Hashable DatabaseShortInfoResponse where
  hashWithSalt _salt DatabaseShortInfoResponse' {..} =
    _salt `Prelude.hashWithSalt` databaseEngine
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` databaseId
      `Prelude.hashWithSalt` databaseIpAddress

instance Prelude.NFData DatabaseShortInfoResponse where
  rnf DatabaseShortInfoResponse' {..} =
    Prelude.rnf databaseEngine
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf databaseId
      `Prelude.seq` Prelude.rnf databaseIpAddress
