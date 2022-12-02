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
-- Module      : Amazonka.DMS.Types.DatabaseInstanceSoftwareDetailsResponse
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.DatabaseInstanceSoftwareDetailsResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes an inventory database instance for a Fleet Advisor collector.
--
-- /See:/ 'newDatabaseInstanceSoftwareDetailsResponse' smart constructor.
data DatabaseInstanceSoftwareDetailsResponse = DatabaseInstanceSoftwareDetailsResponse'
  { -- | The database engine edition of a database in a Fleet Advisor collector
    -- inventory, for example @Express@.
    engineEdition :: Prelude.Maybe Prelude.Text,
    -- | The service pack level of the database.
    servicePack :: Prelude.Maybe Prelude.Text,
    -- | The database engine of a database in a Fleet Advisor collector
    -- inventory, for example @Microsoft SQL Server@.
    engine :: Prelude.Maybe Prelude.Text,
    -- | Information about the database engine software, for example
    -- @Mainstream support ends on November 14th, 2024@.
    tooltip :: Prelude.Maybe Prelude.Text,
    -- | The support level of the database, for example @Mainstream support@.
    supportLevel :: Prelude.Maybe Prelude.Text,
    -- | The database engine version of a database in a Fleet Advisor collector
    -- inventory, for example @2019@.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | The operating system architecture of the database.
    osArchitecture :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatabaseInstanceSoftwareDetailsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'engineEdition', 'databaseInstanceSoftwareDetailsResponse_engineEdition' - The database engine edition of a database in a Fleet Advisor collector
-- inventory, for example @Express@.
--
-- 'servicePack', 'databaseInstanceSoftwareDetailsResponse_servicePack' - The service pack level of the database.
--
-- 'engine', 'databaseInstanceSoftwareDetailsResponse_engine' - The database engine of a database in a Fleet Advisor collector
-- inventory, for example @Microsoft SQL Server@.
--
-- 'tooltip', 'databaseInstanceSoftwareDetailsResponse_tooltip' - Information about the database engine software, for example
-- @Mainstream support ends on November 14th, 2024@.
--
-- 'supportLevel', 'databaseInstanceSoftwareDetailsResponse_supportLevel' - The support level of the database, for example @Mainstream support@.
--
-- 'engineVersion', 'databaseInstanceSoftwareDetailsResponse_engineVersion' - The database engine version of a database in a Fleet Advisor collector
-- inventory, for example @2019@.
--
-- 'osArchitecture', 'databaseInstanceSoftwareDetailsResponse_osArchitecture' - The operating system architecture of the database.
newDatabaseInstanceSoftwareDetailsResponse ::
  DatabaseInstanceSoftwareDetailsResponse
newDatabaseInstanceSoftwareDetailsResponse =
  DatabaseInstanceSoftwareDetailsResponse'
    { engineEdition =
        Prelude.Nothing,
      servicePack = Prelude.Nothing,
      engine = Prelude.Nothing,
      tooltip = Prelude.Nothing,
      supportLevel = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      osArchitecture = Prelude.Nothing
    }

-- | The database engine edition of a database in a Fleet Advisor collector
-- inventory, for example @Express@.
databaseInstanceSoftwareDetailsResponse_engineEdition :: Lens.Lens' DatabaseInstanceSoftwareDetailsResponse (Prelude.Maybe Prelude.Text)
databaseInstanceSoftwareDetailsResponse_engineEdition = Lens.lens (\DatabaseInstanceSoftwareDetailsResponse' {engineEdition} -> engineEdition) (\s@DatabaseInstanceSoftwareDetailsResponse' {} a -> s {engineEdition = a} :: DatabaseInstanceSoftwareDetailsResponse)

-- | The service pack level of the database.
databaseInstanceSoftwareDetailsResponse_servicePack :: Lens.Lens' DatabaseInstanceSoftwareDetailsResponse (Prelude.Maybe Prelude.Text)
databaseInstanceSoftwareDetailsResponse_servicePack = Lens.lens (\DatabaseInstanceSoftwareDetailsResponse' {servicePack} -> servicePack) (\s@DatabaseInstanceSoftwareDetailsResponse' {} a -> s {servicePack = a} :: DatabaseInstanceSoftwareDetailsResponse)

-- | The database engine of a database in a Fleet Advisor collector
-- inventory, for example @Microsoft SQL Server@.
databaseInstanceSoftwareDetailsResponse_engine :: Lens.Lens' DatabaseInstanceSoftwareDetailsResponse (Prelude.Maybe Prelude.Text)
databaseInstanceSoftwareDetailsResponse_engine = Lens.lens (\DatabaseInstanceSoftwareDetailsResponse' {engine} -> engine) (\s@DatabaseInstanceSoftwareDetailsResponse' {} a -> s {engine = a} :: DatabaseInstanceSoftwareDetailsResponse)

-- | Information about the database engine software, for example
-- @Mainstream support ends on November 14th, 2024@.
databaseInstanceSoftwareDetailsResponse_tooltip :: Lens.Lens' DatabaseInstanceSoftwareDetailsResponse (Prelude.Maybe Prelude.Text)
databaseInstanceSoftwareDetailsResponse_tooltip = Lens.lens (\DatabaseInstanceSoftwareDetailsResponse' {tooltip} -> tooltip) (\s@DatabaseInstanceSoftwareDetailsResponse' {} a -> s {tooltip = a} :: DatabaseInstanceSoftwareDetailsResponse)

-- | The support level of the database, for example @Mainstream support@.
databaseInstanceSoftwareDetailsResponse_supportLevel :: Lens.Lens' DatabaseInstanceSoftwareDetailsResponse (Prelude.Maybe Prelude.Text)
databaseInstanceSoftwareDetailsResponse_supportLevel = Lens.lens (\DatabaseInstanceSoftwareDetailsResponse' {supportLevel} -> supportLevel) (\s@DatabaseInstanceSoftwareDetailsResponse' {} a -> s {supportLevel = a} :: DatabaseInstanceSoftwareDetailsResponse)

-- | The database engine version of a database in a Fleet Advisor collector
-- inventory, for example @2019@.
databaseInstanceSoftwareDetailsResponse_engineVersion :: Lens.Lens' DatabaseInstanceSoftwareDetailsResponse (Prelude.Maybe Prelude.Text)
databaseInstanceSoftwareDetailsResponse_engineVersion = Lens.lens (\DatabaseInstanceSoftwareDetailsResponse' {engineVersion} -> engineVersion) (\s@DatabaseInstanceSoftwareDetailsResponse' {} a -> s {engineVersion = a} :: DatabaseInstanceSoftwareDetailsResponse)

-- | The operating system architecture of the database.
databaseInstanceSoftwareDetailsResponse_osArchitecture :: Lens.Lens' DatabaseInstanceSoftwareDetailsResponse (Prelude.Maybe Prelude.Int)
databaseInstanceSoftwareDetailsResponse_osArchitecture = Lens.lens (\DatabaseInstanceSoftwareDetailsResponse' {osArchitecture} -> osArchitecture) (\s@DatabaseInstanceSoftwareDetailsResponse' {} a -> s {osArchitecture = a} :: DatabaseInstanceSoftwareDetailsResponse)

instance
  Data.FromJSON
    DatabaseInstanceSoftwareDetailsResponse
  where
  parseJSON =
    Data.withObject
      "DatabaseInstanceSoftwareDetailsResponse"
      ( \x ->
          DatabaseInstanceSoftwareDetailsResponse'
            Prelude.<$> (x Data..:? "EngineEdition")
            Prelude.<*> (x Data..:? "ServicePack")
            Prelude.<*> (x Data..:? "Engine")
            Prelude.<*> (x Data..:? "Tooltip")
            Prelude.<*> (x Data..:? "SupportLevel")
            Prelude.<*> (x Data..:? "EngineVersion")
            Prelude.<*> (x Data..:? "OsArchitecture")
      )

instance
  Prelude.Hashable
    DatabaseInstanceSoftwareDetailsResponse
  where
  hashWithSalt
    _salt
    DatabaseInstanceSoftwareDetailsResponse' {..} =
      _salt `Prelude.hashWithSalt` engineEdition
        `Prelude.hashWithSalt` servicePack
        `Prelude.hashWithSalt` engine
        `Prelude.hashWithSalt` tooltip
        `Prelude.hashWithSalt` supportLevel
        `Prelude.hashWithSalt` engineVersion
        `Prelude.hashWithSalt` osArchitecture

instance
  Prelude.NFData
    DatabaseInstanceSoftwareDetailsResponse
  where
  rnf DatabaseInstanceSoftwareDetailsResponse' {..} =
    Prelude.rnf engineEdition
      `Prelude.seq` Prelude.rnf servicePack
      `Prelude.seq` Prelude.rnf engine
      `Prelude.seq` Prelude.rnf tooltip
      `Prelude.seq` Prelude.rnf supportLevel
      `Prelude.seq` Prelude.rnf engineVersion
      `Prelude.seq` Prelude.rnf osArchitecture
