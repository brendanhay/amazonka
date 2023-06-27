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
-- Module      : Amazonka.FinSpace.Types.KxChangesetListEntry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FinSpace.Types.KxChangesetListEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FinSpace.Types.ChangesetStatus
import qualified Amazonka.Prelude as Prelude

-- | Details of changeset.
--
-- /See:/ 'newKxChangesetListEntry' smart constructor.
data KxChangesetListEntry = KxChangesetListEntry'
  { -- | Beginning time from which the changeset is active. The value is
    -- determined as epoch time in milliseconds. For example, the value for
    -- Monday, November 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
    activeFromTimestamp :: Prelude.Maybe Data.POSIX,
    -- | A unique identifier for the changeset.
    changesetId :: Prelude.Maybe Prelude.Text,
    -- | The timestamp at which the changeset was created in FinSpace. The value
    -- is determined as epoch time in milliseconds. For example, the value for
    -- Monday, November 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
    createdTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The timestamp at which the changeset was modified. The value is
    -- determined as epoch time in milliseconds. For example, the value for
    -- Monday, November 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
    lastModifiedTimestamp :: Prelude.Maybe Data.POSIX,
    -- | Status of the changeset.
    --
    -- -   Pending – Changeset creation is pending.
    --
    -- -   Processing – Changeset creation is running.
    --
    -- -   Failed – Changeset creation has failed.
    --
    -- -   Complete – Changeset creation has succeeded.
    status :: Prelude.Maybe ChangesetStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KxChangesetListEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activeFromTimestamp', 'kxChangesetListEntry_activeFromTimestamp' - Beginning time from which the changeset is active. The value is
-- determined as epoch time in milliseconds. For example, the value for
-- Monday, November 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
--
-- 'changesetId', 'kxChangesetListEntry_changesetId' - A unique identifier for the changeset.
--
-- 'createdTimestamp', 'kxChangesetListEntry_createdTimestamp' - The timestamp at which the changeset was created in FinSpace. The value
-- is determined as epoch time in milliseconds. For example, the value for
-- Monday, November 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
--
-- 'lastModifiedTimestamp', 'kxChangesetListEntry_lastModifiedTimestamp' - The timestamp at which the changeset was modified. The value is
-- determined as epoch time in milliseconds. For example, the value for
-- Monday, November 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
--
-- 'status', 'kxChangesetListEntry_status' - Status of the changeset.
--
-- -   Pending – Changeset creation is pending.
--
-- -   Processing – Changeset creation is running.
--
-- -   Failed – Changeset creation has failed.
--
-- -   Complete – Changeset creation has succeeded.
newKxChangesetListEntry ::
  KxChangesetListEntry
newKxChangesetListEntry =
  KxChangesetListEntry'
    { activeFromTimestamp =
        Prelude.Nothing,
      changesetId = Prelude.Nothing,
      createdTimestamp = Prelude.Nothing,
      lastModifiedTimestamp = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | Beginning time from which the changeset is active. The value is
-- determined as epoch time in milliseconds. For example, the value for
-- Monday, November 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
kxChangesetListEntry_activeFromTimestamp :: Lens.Lens' KxChangesetListEntry (Prelude.Maybe Prelude.UTCTime)
kxChangesetListEntry_activeFromTimestamp = Lens.lens (\KxChangesetListEntry' {activeFromTimestamp} -> activeFromTimestamp) (\s@KxChangesetListEntry' {} a -> s {activeFromTimestamp = a} :: KxChangesetListEntry) Prelude.. Lens.mapping Data._Time

-- | A unique identifier for the changeset.
kxChangesetListEntry_changesetId :: Lens.Lens' KxChangesetListEntry (Prelude.Maybe Prelude.Text)
kxChangesetListEntry_changesetId = Lens.lens (\KxChangesetListEntry' {changesetId} -> changesetId) (\s@KxChangesetListEntry' {} a -> s {changesetId = a} :: KxChangesetListEntry)

-- | The timestamp at which the changeset was created in FinSpace. The value
-- is determined as epoch time in milliseconds. For example, the value for
-- Monday, November 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
kxChangesetListEntry_createdTimestamp :: Lens.Lens' KxChangesetListEntry (Prelude.Maybe Prelude.UTCTime)
kxChangesetListEntry_createdTimestamp = Lens.lens (\KxChangesetListEntry' {createdTimestamp} -> createdTimestamp) (\s@KxChangesetListEntry' {} a -> s {createdTimestamp = a} :: KxChangesetListEntry) Prelude.. Lens.mapping Data._Time

-- | The timestamp at which the changeset was modified. The value is
-- determined as epoch time in milliseconds. For example, the value for
-- Monday, November 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
kxChangesetListEntry_lastModifiedTimestamp :: Lens.Lens' KxChangesetListEntry (Prelude.Maybe Prelude.UTCTime)
kxChangesetListEntry_lastModifiedTimestamp = Lens.lens (\KxChangesetListEntry' {lastModifiedTimestamp} -> lastModifiedTimestamp) (\s@KxChangesetListEntry' {} a -> s {lastModifiedTimestamp = a} :: KxChangesetListEntry) Prelude.. Lens.mapping Data._Time

-- | Status of the changeset.
--
-- -   Pending – Changeset creation is pending.
--
-- -   Processing – Changeset creation is running.
--
-- -   Failed – Changeset creation has failed.
--
-- -   Complete – Changeset creation has succeeded.
kxChangesetListEntry_status :: Lens.Lens' KxChangesetListEntry (Prelude.Maybe ChangesetStatus)
kxChangesetListEntry_status = Lens.lens (\KxChangesetListEntry' {status} -> status) (\s@KxChangesetListEntry' {} a -> s {status = a} :: KxChangesetListEntry)

instance Data.FromJSON KxChangesetListEntry where
  parseJSON =
    Data.withObject
      "KxChangesetListEntry"
      ( \x ->
          KxChangesetListEntry'
            Prelude.<$> (x Data..:? "activeFromTimestamp")
            Prelude.<*> (x Data..:? "changesetId")
            Prelude.<*> (x Data..:? "createdTimestamp")
            Prelude.<*> (x Data..:? "lastModifiedTimestamp")
            Prelude.<*> (x Data..:? "status")
      )

instance Prelude.Hashable KxChangesetListEntry where
  hashWithSalt _salt KxChangesetListEntry' {..} =
    _salt
      `Prelude.hashWithSalt` activeFromTimestamp
      `Prelude.hashWithSalt` changesetId
      `Prelude.hashWithSalt` createdTimestamp
      `Prelude.hashWithSalt` lastModifiedTimestamp
      `Prelude.hashWithSalt` status

instance Prelude.NFData KxChangesetListEntry where
  rnf KxChangesetListEntry' {..} =
    Prelude.rnf activeFromTimestamp
      `Prelude.seq` Prelude.rnf changesetId
      `Prelude.seq` Prelude.rnf createdTimestamp
      `Prelude.seq` Prelude.rnf lastModifiedTimestamp
      `Prelude.seq` Prelude.rnf status
