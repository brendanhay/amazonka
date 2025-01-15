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
-- Module      : Amazonka.Lightsail.Types.AutoSnapshotDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.AutoSnapshotDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types.AttachedDisk
import Amazonka.Lightsail.Types.AutoSnapshotStatus
import qualified Amazonka.Prelude as Prelude

-- | Describes an automatic snapshot.
--
-- /See:/ 'newAutoSnapshotDetails' smart constructor.
data AutoSnapshotDetails = AutoSnapshotDetails'
  { -- | The timestamp when the automatic snapshot was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The date of the automatic snapshot in @YYYY-MM-DD@ format.
    date :: Prelude.Maybe Prelude.Text,
    -- | An array of objects that describe the block storage disks attached to
    -- the instance when the automatic snapshot was created.
    fromAttachedDisks :: Prelude.Maybe [AttachedDisk],
    -- | The status of the automatic snapshot.
    status :: Prelude.Maybe AutoSnapshotStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutoSnapshotDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAt', 'autoSnapshotDetails_createdAt' - The timestamp when the automatic snapshot was created.
--
-- 'date', 'autoSnapshotDetails_date' - The date of the automatic snapshot in @YYYY-MM-DD@ format.
--
-- 'fromAttachedDisks', 'autoSnapshotDetails_fromAttachedDisks' - An array of objects that describe the block storage disks attached to
-- the instance when the automatic snapshot was created.
--
-- 'status', 'autoSnapshotDetails_status' - The status of the automatic snapshot.
newAutoSnapshotDetails ::
  AutoSnapshotDetails
newAutoSnapshotDetails =
  AutoSnapshotDetails'
    { createdAt = Prelude.Nothing,
      date = Prelude.Nothing,
      fromAttachedDisks = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The timestamp when the automatic snapshot was created.
autoSnapshotDetails_createdAt :: Lens.Lens' AutoSnapshotDetails (Prelude.Maybe Prelude.UTCTime)
autoSnapshotDetails_createdAt = Lens.lens (\AutoSnapshotDetails' {createdAt} -> createdAt) (\s@AutoSnapshotDetails' {} a -> s {createdAt = a} :: AutoSnapshotDetails) Prelude.. Lens.mapping Data._Time

-- | The date of the automatic snapshot in @YYYY-MM-DD@ format.
autoSnapshotDetails_date :: Lens.Lens' AutoSnapshotDetails (Prelude.Maybe Prelude.Text)
autoSnapshotDetails_date = Lens.lens (\AutoSnapshotDetails' {date} -> date) (\s@AutoSnapshotDetails' {} a -> s {date = a} :: AutoSnapshotDetails)

-- | An array of objects that describe the block storage disks attached to
-- the instance when the automatic snapshot was created.
autoSnapshotDetails_fromAttachedDisks :: Lens.Lens' AutoSnapshotDetails (Prelude.Maybe [AttachedDisk])
autoSnapshotDetails_fromAttachedDisks = Lens.lens (\AutoSnapshotDetails' {fromAttachedDisks} -> fromAttachedDisks) (\s@AutoSnapshotDetails' {} a -> s {fromAttachedDisks = a} :: AutoSnapshotDetails) Prelude.. Lens.mapping Lens.coerced

-- | The status of the automatic snapshot.
autoSnapshotDetails_status :: Lens.Lens' AutoSnapshotDetails (Prelude.Maybe AutoSnapshotStatus)
autoSnapshotDetails_status = Lens.lens (\AutoSnapshotDetails' {status} -> status) (\s@AutoSnapshotDetails' {} a -> s {status = a} :: AutoSnapshotDetails)

instance Data.FromJSON AutoSnapshotDetails where
  parseJSON =
    Data.withObject
      "AutoSnapshotDetails"
      ( \x ->
          AutoSnapshotDetails'
            Prelude.<$> (x Data..:? "createdAt")
            Prelude.<*> (x Data..:? "date")
            Prelude.<*> ( x
                            Data..:? "fromAttachedDisks"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "status")
      )

instance Prelude.Hashable AutoSnapshotDetails where
  hashWithSalt _salt AutoSnapshotDetails' {..} =
    _salt
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` date
      `Prelude.hashWithSalt` fromAttachedDisks
      `Prelude.hashWithSalt` status

instance Prelude.NFData AutoSnapshotDetails where
  rnf AutoSnapshotDetails' {..} =
    Prelude.rnf createdAt `Prelude.seq`
      Prelude.rnf date `Prelude.seq`
        Prelude.rnf fromAttachedDisks `Prelude.seq`
          Prelude.rnf status
