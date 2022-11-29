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
-- Module      : Amazonka.Lightsail.Types.AddOn
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.AddOn where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes an add-on that is enabled for an Amazon Lightsail resource.
--
-- /See:/ 'newAddOn' smart constructor.
data AddOn = AddOn'
  { -- | The name of the add-on.
    name :: Prelude.Maybe Prelude.Text,
    -- | The status of the add-on.
    status :: Prelude.Maybe Prelude.Text,
    -- | The daily time when an automatic snapshot is created.
    --
    -- The time shown is in @HH:00@ format, and in Coordinated Universal Time
    -- (UTC).
    --
    -- The snapshot is automatically created between the time shown and up to
    -- 45 minutes after.
    snapshotTimeOfDay :: Prelude.Maybe Prelude.Text,
    -- | The next daily time an automatic snapshot will be created.
    --
    -- The time shown is in @HH:00@ format, and in Coordinated Universal Time
    -- (UTC).
    --
    -- The snapshot is automatically created between the time shown and up to
    -- 45 minutes after.
    nextSnapshotTimeOfDay :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddOn' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'addOn_name' - The name of the add-on.
--
-- 'status', 'addOn_status' - The status of the add-on.
--
-- 'snapshotTimeOfDay', 'addOn_snapshotTimeOfDay' - The daily time when an automatic snapshot is created.
--
-- The time shown is in @HH:00@ format, and in Coordinated Universal Time
-- (UTC).
--
-- The snapshot is automatically created between the time shown and up to
-- 45 minutes after.
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
    { name = Prelude.Nothing,
      status = Prelude.Nothing,
      snapshotTimeOfDay = Prelude.Nothing,
      nextSnapshotTimeOfDay = Prelude.Nothing
    }

-- | The name of the add-on.
addOn_name :: Lens.Lens' AddOn (Prelude.Maybe Prelude.Text)
addOn_name = Lens.lens (\AddOn' {name} -> name) (\s@AddOn' {} a -> s {name = a} :: AddOn)

-- | The status of the add-on.
addOn_status :: Lens.Lens' AddOn (Prelude.Maybe Prelude.Text)
addOn_status = Lens.lens (\AddOn' {status} -> status) (\s@AddOn' {} a -> s {status = a} :: AddOn)

-- | The daily time when an automatic snapshot is created.
--
-- The time shown is in @HH:00@ format, and in Coordinated Universal Time
-- (UTC).
--
-- The snapshot is automatically created between the time shown and up to
-- 45 minutes after.
addOn_snapshotTimeOfDay :: Lens.Lens' AddOn (Prelude.Maybe Prelude.Text)
addOn_snapshotTimeOfDay = Lens.lens (\AddOn' {snapshotTimeOfDay} -> snapshotTimeOfDay) (\s@AddOn' {} a -> s {snapshotTimeOfDay = a} :: AddOn)

-- | The next daily time an automatic snapshot will be created.
--
-- The time shown is in @HH:00@ format, and in Coordinated Universal Time
-- (UTC).
--
-- The snapshot is automatically created between the time shown and up to
-- 45 minutes after.
addOn_nextSnapshotTimeOfDay :: Lens.Lens' AddOn (Prelude.Maybe Prelude.Text)
addOn_nextSnapshotTimeOfDay = Lens.lens (\AddOn' {nextSnapshotTimeOfDay} -> nextSnapshotTimeOfDay) (\s@AddOn' {} a -> s {nextSnapshotTimeOfDay = a} :: AddOn)

instance Core.FromJSON AddOn where
  parseJSON =
    Core.withObject
      "AddOn"
      ( \x ->
          AddOn'
            Prelude.<$> (x Core..:? "name")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "snapshotTimeOfDay")
            Prelude.<*> (x Core..:? "nextSnapshotTimeOfDay")
      )

instance Prelude.Hashable AddOn where
  hashWithSalt _salt AddOn' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` snapshotTimeOfDay
      `Prelude.hashWithSalt` nextSnapshotTimeOfDay

instance Prelude.NFData AddOn where
  rnf AddOn' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf snapshotTimeOfDay
      `Prelude.seq` Prelude.rnf nextSnapshotTimeOfDay
