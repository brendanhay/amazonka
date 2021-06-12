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
-- Module      : Network.AWS.Lightsail.Types.AddOn
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.AddOn where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes an add-on that is enabled for an Amazon Lightsail resource.
--
-- /See:/ 'newAddOn' smart constructor.
data AddOn = AddOn'
  { -- | The daily time when an automatic snapshot is created.
    --
    -- The time shown is in @HH:00@ format, and in Coordinated Universal Time
    -- (UTC).
    --
    -- The snapshot is automatically created between the time shown and up to
    -- 45 minutes after.
    snapshotTimeOfDay :: Core.Maybe Core.Text,
    -- | The status of the add-on.
    status :: Core.Maybe Core.Text,
    -- | The name of the add-on.
    name :: Core.Maybe Core.Text,
    -- | The next daily time an automatic snapshot will be created.
    --
    -- The time shown is in @HH:00@ format, and in Coordinated Universal Time
    -- (UTC).
    --
    -- The snapshot is automatically created between the time shown and up to
    -- 45 minutes after.
    nextSnapshotTimeOfDay :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AddOn' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'snapshotTimeOfDay', 'addOn_snapshotTimeOfDay' - The daily time when an automatic snapshot is created.
--
-- The time shown is in @HH:00@ format, and in Coordinated Universal Time
-- (UTC).
--
-- The snapshot is automatically created between the time shown and up to
-- 45 minutes after.
--
-- 'status', 'addOn_status' - The status of the add-on.
--
-- 'name', 'addOn_name' - The name of the add-on.
--
-- 'nextSnapshotTimeOfDay', 'addOn_nextSnapshotTimeOfDay' - The next daily time an automatic snapshot will be created.
--
-- The time shown is in @HH:00@ format, and in Coordinated Universal Time
-- (UTC).
--
-- The snapshot is automatically created between the time shown and up to
-- 45 minutes after.
newAddOn ::
  AddOn
newAddOn =
  AddOn'
    { snapshotTimeOfDay = Core.Nothing,
      status = Core.Nothing,
      name = Core.Nothing,
      nextSnapshotTimeOfDay = Core.Nothing
    }

-- | The daily time when an automatic snapshot is created.
--
-- The time shown is in @HH:00@ format, and in Coordinated Universal Time
-- (UTC).
--
-- The snapshot is automatically created between the time shown and up to
-- 45 minutes after.
addOn_snapshotTimeOfDay :: Lens.Lens' AddOn (Core.Maybe Core.Text)
addOn_snapshotTimeOfDay = Lens.lens (\AddOn' {snapshotTimeOfDay} -> snapshotTimeOfDay) (\s@AddOn' {} a -> s {snapshotTimeOfDay = a} :: AddOn)

-- | The status of the add-on.
addOn_status :: Lens.Lens' AddOn (Core.Maybe Core.Text)
addOn_status = Lens.lens (\AddOn' {status} -> status) (\s@AddOn' {} a -> s {status = a} :: AddOn)

-- | The name of the add-on.
addOn_name :: Lens.Lens' AddOn (Core.Maybe Core.Text)
addOn_name = Lens.lens (\AddOn' {name} -> name) (\s@AddOn' {} a -> s {name = a} :: AddOn)

-- | The next daily time an automatic snapshot will be created.
--
-- The time shown is in @HH:00@ format, and in Coordinated Universal Time
-- (UTC).
--
-- The snapshot is automatically created between the time shown and up to
-- 45 minutes after.
addOn_nextSnapshotTimeOfDay :: Lens.Lens' AddOn (Core.Maybe Core.Text)
addOn_nextSnapshotTimeOfDay = Lens.lens (\AddOn' {nextSnapshotTimeOfDay} -> nextSnapshotTimeOfDay) (\s@AddOn' {} a -> s {nextSnapshotTimeOfDay = a} :: AddOn)

instance Core.FromJSON AddOn where
  parseJSON =
    Core.withObject
      "AddOn"
      ( \x ->
          AddOn'
            Core.<$> (x Core..:? "snapshotTimeOfDay")
            Core.<*> (x Core..:? "status")
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "nextSnapshotTimeOfDay")
      )

instance Core.Hashable AddOn

instance Core.NFData AddOn
