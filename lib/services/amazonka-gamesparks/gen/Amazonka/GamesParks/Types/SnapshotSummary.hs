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
-- Module      : Amazonka.GamesParks.Types.SnapshotSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GamesParks.Types.SnapshotSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The summary of the properties of a snapshot.
--
-- /See:/ 'newSnapshotSummary' smart constructor.
data SnapshotSummary = SnapshotSummary'
  { -- | The timestamp of when the snapshot was created.
    created :: Prelude.Maybe Data.ISO8601,
    -- | The description of the snapshot.
    description :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the snapshot.
    id :: Prelude.Maybe Prelude.Text,
    -- | Then timestamp of when the snapshot was last updated.
    lastUpdated :: Prelude.Maybe Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SnapshotSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'created', 'snapshotSummary_created' - The timestamp of when the snapshot was created.
--
-- 'description', 'snapshotSummary_description' - The description of the snapshot.
--
-- 'id', 'snapshotSummary_id' - The identifier of the snapshot.
--
-- 'lastUpdated', 'snapshotSummary_lastUpdated' - Then timestamp of when the snapshot was last updated.
newSnapshotSummary ::
  SnapshotSummary
newSnapshotSummary =
  SnapshotSummary'
    { created = Prelude.Nothing,
      description = Prelude.Nothing,
      id = Prelude.Nothing,
      lastUpdated = Prelude.Nothing
    }

-- | The timestamp of when the snapshot was created.
snapshotSummary_created :: Lens.Lens' SnapshotSummary (Prelude.Maybe Prelude.UTCTime)
snapshotSummary_created = Lens.lens (\SnapshotSummary' {created} -> created) (\s@SnapshotSummary' {} a -> s {created = a} :: SnapshotSummary) Prelude.. Lens.mapping Data._Time

-- | The description of the snapshot.
snapshotSummary_description :: Lens.Lens' SnapshotSummary (Prelude.Maybe Prelude.Text)
snapshotSummary_description = Lens.lens (\SnapshotSummary' {description} -> description) (\s@SnapshotSummary' {} a -> s {description = a} :: SnapshotSummary)

-- | The identifier of the snapshot.
snapshotSummary_id :: Lens.Lens' SnapshotSummary (Prelude.Maybe Prelude.Text)
snapshotSummary_id = Lens.lens (\SnapshotSummary' {id} -> id) (\s@SnapshotSummary' {} a -> s {id = a} :: SnapshotSummary)

-- | Then timestamp of when the snapshot was last updated.
snapshotSummary_lastUpdated :: Lens.Lens' SnapshotSummary (Prelude.Maybe Prelude.UTCTime)
snapshotSummary_lastUpdated = Lens.lens (\SnapshotSummary' {lastUpdated} -> lastUpdated) (\s@SnapshotSummary' {} a -> s {lastUpdated = a} :: SnapshotSummary) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON SnapshotSummary where
  parseJSON =
    Data.withObject
      "SnapshotSummary"
      ( \x ->
          SnapshotSummary'
            Prelude.<$> (x Data..:? "Created")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "LastUpdated")
      )

instance Prelude.Hashable SnapshotSummary where
  hashWithSalt _salt SnapshotSummary' {..} =
    _salt
      `Prelude.hashWithSalt` created
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` lastUpdated

instance Prelude.NFData SnapshotSummary where
  rnf SnapshotSummary' {..} =
    Prelude.rnf created `Prelude.seq`
      Prelude.rnf description `Prelude.seq`
        Prelude.rnf id `Prelude.seq`
          Prelude.rnf lastUpdated
