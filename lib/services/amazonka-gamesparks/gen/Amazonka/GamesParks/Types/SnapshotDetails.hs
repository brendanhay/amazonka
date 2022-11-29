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
-- Module      : Amazonka.GamesParks.Types.SnapshotDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GamesParks.Types.SnapshotDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GamesParks.Types.Section
import qualified Amazonka.Prelude as Prelude

-- | Properties that provide details of a snapshot.
--
-- /See:/ 'newSnapshotDetails' smart constructor.
data SnapshotDetails = SnapshotDetails'
  { -- | The timestamp of when the snapshot was created.
    created :: Prelude.Maybe Core.POSIX,
    -- | The sections in the snapshot.
    sections :: Prelude.Maybe (Prelude.HashMap Prelude.Text Section),
    -- | The identifier of the snapshot.
    id :: Prelude.Maybe Prelude.Text,
    -- | The description of the snapshot.
    description :: Prelude.Maybe Prelude.Text,
    -- | The timestamp of when the snapshot was last updated.
    lastUpdated :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SnapshotDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'created', 'snapshotDetails_created' - The timestamp of when the snapshot was created.
--
-- 'sections', 'snapshotDetails_sections' - The sections in the snapshot.
--
-- 'id', 'snapshotDetails_id' - The identifier of the snapshot.
--
-- 'description', 'snapshotDetails_description' - The description of the snapshot.
--
-- 'lastUpdated', 'snapshotDetails_lastUpdated' - The timestamp of when the snapshot was last updated.
newSnapshotDetails ::
  SnapshotDetails
newSnapshotDetails =
  SnapshotDetails'
    { created = Prelude.Nothing,
      sections = Prelude.Nothing,
      id = Prelude.Nothing,
      description = Prelude.Nothing,
      lastUpdated = Prelude.Nothing
    }

-- | The timestamp of when the snapshot was created.
snapshotDetails_created :: Lens.Lens' SnapshotDetails (Prelude.Maybe Prelude.UTCTime)
snapshotDetails_created = Lens.lens (\SnapshotDetails' {created} -> created) (\s@SnapshotDetails' {} a -> s {created = a} :: SnapshotDetails) Prelude.. Lens.mapping Core._Time

-- | The sections in the snapshot.
snapshotDetails_sections :: Lens.Lens' SnapshotDetails (Prelude.Maybe (Prelude.HashMap Prelude.Text Section))
snapshotDetails_sections = Lens.lens (\SnapshotDetails' {sections} -> sections) (\s@SnapshotDetails' {} a -> s {sections = a} :: SnapshotDetails) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the snapshot.
snapshotDetails_id :: Lens.Lens' SnapshotDetails (Prelude.Maybe Prelude.Text)
snapshotDetails_id = Lens.lens (\SnapshotDetails' {id} -> id) (\s@SnapshotDetails' {} a -> s {id = a} :: SnapshotDetails)

-- | The description of the snapshot.
snapshotDetails_description :: Lens.Lens' SnapshotDetails (Prelude.Maybe Prelude.Text)
snapshotDetails_description = Lens.lens (\SnapshotDetails' {description} -> description) (\s@SnapshotDetails' {} a -> s {description = a} :: SnapshotDetails)

-- | The timestamp of when the snapshot was last updated.
snapshotDetails_lastUpdated :: Lens.Lens' SnapshotDetails (Prelude.Maybe Prelude.UTCTime)
snapshotDetails_lastUpdated = Lens.lens (\SnapshotDetails' {lastUpdated} -> lastUpdated) (\s@SnapshotDetails' {} a -> s {lastUpdated = a} :: SnapshotDetails) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON SnapshotDetails where
  parseJSON =
    Core.withObject
      "SnapshotDetails"
      ( \x ->
          SnapshotDetails'
            Prelude.<$> (x Core..:? "Created")
            Prelude.<*> (x Core..:? "Sections" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "LastUpdated")
      )

instance Prelude.Hashable SnapshotDetails where
  hashWithSalt _salt SnapshotDetails' {..} =
    _salt `Prelude.hashWithSalt` created
      `Prelude.hashWithSalt` sections
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` lastUpdated

instance Prelude.NFData SnapshotDetails where
  rnf SnapshotDetails' {..} =
    Prelude.rnf created
      `Prelude.seq` Prelude.rnf sections
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf lastUpdated
