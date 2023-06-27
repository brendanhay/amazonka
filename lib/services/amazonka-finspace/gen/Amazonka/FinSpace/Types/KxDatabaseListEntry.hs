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
-- Module      : Amazonka.FinSpace.Types.KxDatabaseListEntry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FinSpace.Types.KxDatabaseListEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details about a FinSpace managed kdb database
--
-- /See:/ 'newKxDatabaseListEntry' smart constructor.
data KxDatabaseListEntry = KxDatabaseListEntry'
  { -- | The timestamp at which the database was created in FinSpace. The value
    -- is determined as epoch time in milliseconds. For example, the value for
    -- Monday, November 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
    createdTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The name of the kdb database.
    databaseName :: Prelude.Maybe Prelude.Text,
    -- | The last time that the database was modified. The value is determined as
    -- epoch time in milliseconds. For example, the value for Monday, November
    -- 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
    lastModifiedTimestamp :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KxDatabaseListEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdTimestamp', 'kxDatabaseListEntry_createdTimestamp' - The timestamp at which the database was created in FinSpace. The value
-- is determined as epoch time in milliseconds. For example, the value for
-- Monday, November 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
--
-- 'databaseName', 'kxDatabaseListEntry_databaseName' - The name of the kdb database.
--
-- 'lastModifiedTimestamp', 'kxDatabaseListEntry_lastModifiedTimestamp' - The last time that the database was modified. The value is determined as
-- epoch time in milliseconds. For example, the value for Monday, November
-- 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
newKxDatabaseListEntry ::
  KxDatabaseListEntry
newKxDatabaseListEntry =
  KxDatabaseListEntry'
    { createdTimestamp =
        Prelude.Nothing,
      databaseName = Prelude.Nothing,
      lastModifiedTimestamp = Prelude.Nothing
    }

-- | The timestamp at which the database was created in FinSpace. The value
-- is determined as epoch time in milliseconds. For example, the value for
-- Monday, November 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
kxDatabaseListEntry_createdTimestamp :: Lens.Lens' KxDatabaseListEntry (Prelude.Maybe Prelude.UTCTime)
kxDatabaseListEntry_createdTimestamp = Lens.lens (\KxDatabaseListEntry' {createdTimestamp} -> createdTimestamp) (\s@KxDatabaseListEntry' {} a -> s {createdTimestamp = a} :: KxDatabaseListEntry) Prelude.. Lens.mapping Data._Time

-- | The name of the kdb database.
kxDatabaseListEntry_databaseName :: Lens.Lens' KxDatabaseListEntry (Prelude.Maybe Prelude.Text)
kxDatabaseListEntry_databaseName = Lens.lens (\KxDatabaseListEntry' {databaseName} -> databaseName) (\s@KxDatabaseListEntry' {} a -> s {databaseName = a} :: KxDatabaseListEntry)

-- | The last time that the database was modified. The value is determined as
-- epoch time in milliseconds. For example, the value for Monday, November
-- 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
kxDatabaseListEntry_lastModifiedTimestamp :: Lens.Lens' KxDatabaseListEntry (Prelude.Maybe Prelude.UTCTime)
kxDatabaseListEntry_lastModifiedTimestamp = Lens.lens (\KxDatabaseListEntry' {lastModifiedTimestamp} -> lastModifiedTimestamp) (\s@KxDatabaseListEntry' {} a -> s {lastModifiedTimestamp = a} :: KxDatabaseListEntry) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON KxDatabaseListEntry where
  parseJSON =
    Data.withObject
      "KxDatabaseListEntry"
      ( \x ->
          KxDatabaseListEntry'
            Prelude.<$> (x Data..:? "createdTimestamp")
            Prelude.<*> (x Data..:? "databaseName")
            Prelude.<*> (x Data..:? "lastModifiedTimestamp")
      )

instance Prelude.Hashable KxDatabaseListEntry where
  hashWithSalt _salt KxDatabaseListEntry' {..} =
    _salt
      `Prelude.hashWithSalt` createdTimestamp
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` lastModifiedTimestamp

instance Prelude.NFData KxDatabaseListEntry where
  rnf KxDatabaseListEntry' {..} =
    Prelude.rnf createdTimestamp
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf lastModifiedTimestamp
