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
-- Module      : Network.AWS.Lightsail.Types.AutoSnapshotDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.AutoSnapshotDetails where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.AttachedDisk
import Network.AWS.Lightsail.Types.AutoSnapshotStatus

-- | Describes an automatic snapshot.
--
-- /See:/ 'newAutoSnapshotDetails' smart constructor.
data AutoSnapshotDetails = AutoSnapshotDetails'
  { -- | The status of the automatic snapshot.
    status :: Core.Maybe AutoSnapshotStatus,
    -- | The timestamp when the automatic snapshot was created.
    createdAt :: Core.Maybe Core.POSIX,
    -- | The date of the automatic snapshot in @YYYY-MM-DD@ format.
    date :: Core.Maybe Core.Text,
    -- | An array of objects that describe the block storage disks attached to
    -- the instance when the automatic snapshot was created.
    fromAttachedDisks :: Core.Maybe [AttachedDisk]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AutoSnapshotDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'autoSnapshotDetails_status' - The status of the automatic snapshot.
--
-- 'createdAt', 'autoSnapshotDetails_createdAt' - The timestamp when the automatic snapshot was created.
--
-- 'date', 'autoSnapshotDetails_date' - The date of the automatic snapshot in @YYYY-MM-DD@ format.
--
-- 'fromAttachedDisks', 'autoSnapshotDetails_fromAttachedDisks' - An array of objects that describe the block storage disks attached to
-- the instance when the automatic snapshot was created.
newAutoSnapshotDetails ::
  AutoSnapshotDetails
newAutoSnapshotDetails =
  AutoSnapshotDetails'
    { status = Core.Nothing,
      createdAt = Core.Nothing,
      date = Core.Nothing,
      fromAttachedDisks = Core.Nothing
    }

-- | The status of the automatic snapshot.
autoSnapshotDetails_status :: Lens.Lens' AutoSnapshotDetails (Core.Maybe AutoSnapshotStatus)
autoSnapshotDetails_status = Lens.lens (\AutoSnapshotDetails' {status} -> status) (\s@AutoSnapshotDetails' {} a -> s {status = a} :: AutoSnapshotDetails)

-- | The timestamp when the automatic snapshot was created.
autoSnapshotDetails_createdAt :: Lens.Lens' AutoSnapshotDetails (Core.Maybe Core.UTCTime)
autoSnapshotDetails_createdAt = Lens.lens (\AutoSnapshotDetails' {createdAt} -> createdAt) (\s@AutoSnapshotDetails' {} a -> s {createdAt = a} :: AutoSnapshotDetails) Core.. Lens.mapping Core._Time

-- | The date of the automatic snapshot in @YYYY-MM-DD@ format.
autoSnapshotDetails_date :: Lens.Lens' AutoSnapshotDetails (Core.Maybe Core.Text)
autoSnapshotDetails_date = Lens.lens (\AutoSnapshotDetails' {date} -> date) (\s@AutoSnapshotDetails' {} a -> s {date = a} :: AutoSnapshotDetails)

-- | An array of objects that describe the block storage disks attached to
-- the instance when the automatic snapshot was created.
autoSnapshotDetails_fromAttachedDisks :: Lens.Lens' AutoSnapshotDetails (Core.Maybe [AttachedDisk])
autoSnapshotDetails_fromAttachedDisks = Lens.lens (\AutoSnapshotDetails' {fromAttachedDisks} -> fromAttachedDisks) (\s@AutoSnapshotDetails' {} a -> s {fromAttachedDisks = a} :: AutoSnapshotDetails) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON AutoSnapshotDetails where
  parseJSON =
    Core.withObject
      "AutoSnapshotDetails"
      ( \x ->
          AutoSnapshotDetails'
            Core.<$> (x Core..:? "status")
            Core.<*> (x Core..:? "createdAt")
            Core.<*> (x Core..:? "date")
            Core.<*> ( x Core..:? "fromAttachedDisks"
                         Core..!= Core.mempty
                     )
      )

instance Core.Hashable AutoSnapshotDetails

instance Core.NFData AutoSnapshotDetails
