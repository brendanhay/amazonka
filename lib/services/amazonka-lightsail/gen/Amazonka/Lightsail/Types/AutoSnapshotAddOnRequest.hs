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
-- Module      : Amazonka.Lightsail.Types.AutoSnapshotAddOnRequest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.AutoSnapshotAddOnRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes a request to enable or modify the automatic snapshot add-on
-- for an Amazon Lightsail instance or disk.
--
-- When you modify the automatic snapshot time for a resource, it is
-- typically effective immediately except under the following conditions:
--
-- -   If an automatic snapshot has been created for the current day, and
--     you change the snapshot time to a later time of day, then the new
--     snapshot time will be effective the following day. This ensures that
--     two snapshots are not created for the current day.
--
-- -   If an automatic snapshot has not yet been created for the current
--     day, and you change the snapshot time to an earlier time of day,
--     then the new snapshot time will be effective the following day and a
--     snapshot is automatically created at the previously set time for the
--     current day. This ensures that a snapshot is created for the current
--     day.
--
-- -   If an automatic snapshot has not yet been created for the current
--     day, and you change the snapshot time to a time that is within 30
--     minutes from your current time, then the new snapshot time will be
--     effective the following day and a snapshot is automatically created
--     at the previously set time for the current day. This ensures that a
--     snapshot is created for the current day, because 30 minutes is
--     required between your current time and the new snapshot time that
--     you specify.
--
-- -   If an automatic snapshot is scheduled to be created within 30
--     minutes from your current time and you change the snapshot time,
--     then the new snapshot time will be effective the following day and a
--     snapshot is automatically created at the previously set time for the
--     current day. This ensures that a snapshot is created for the current
--     day, because 30 minutes is required between your current time and
--     the new snapshot time that you specify.
--
-- /See:/ 'newAutoSnapshotAddOnRequest' smart constructor.
data AutoSnapshotAddOnRequest = AutoSnapshotAddOnRequest'
  { -- | The daily time when an automatic snapshot will be created.
    --
    -- Constraints:
    --
    -- -   Must be in @HH:00@ format, and in an hourly increment.
    --
    -- -   Specified in Coordinated Universal Time (UTC).
    --
    -- -   The snapshot will be automatically created between the time
    --     specified and up to 45 minutes after.
    snapshotTimeOfDay :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutoSnapshotAddOnRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'snapshotTimeOfDay', 'autoSnapshotAddOnRequest_snapshotTimeOfDay' - The daily time when an automatic snapshot will be created.
--
-- Constraints:
--
-- -   Must be in @HH:00@ format, and in an hourly increment.
--
-- -   Specified in Coordinated Universal Time (UTC).
--
-- -   The snapshot will be automatically created between the time
--     specified and up to 45 minutes after.
newAutoSnapshotAddOnRequest ::
  AutoSnapshotAddOnRequest
newAutoSnapshotAddOnRequest =
  AutoSnapshotAddOnRequest'
    { snapshotTimeOfDay =
        Prelude.Nothing
    }

-- | The daily time when an automatic snapshot will be created.
--
-- Constraints:
--
-- -   Must be in @HH:00@ format, and in an hourly increment.
--
-- -   Specified in Coordinated Universal Time (UTC).
--
-- -   The snapshot will be automatically created between the time
--     specified and up to 45 minutes after.
autoSnapshotAddOnRequest_snapshotTimeOfDay :: Lens.Lens' AutoSnapshotAddOnRequest (Prelude.Maybe Prelude.Text)
autoSnapshotAddOnRequest_snapshotTimeOfDay = Lens.lens (\AutoSnapshotAddOnRequest' {snapshotTimeOfDay} -> snapshotTimeOfDay) (\s@AutoSnapshotAddOnRequest' {} a -> s {snapshotTimeOfDay = a} :: AutoSnapshotAddOnRequest)

instance Prelude.Hashable AutoSnapshotAddOnRequest where
  hashWithSalt _salt AutoSnapshotAddOnRequest' {..} =
    _salt `Prelude.hashWithSalt` snapshotTimeOfDay

instance Prelude.NFData AutoSnapshotAddOnRequest where
  rnf AutoSnapshotAddOnRequest' {..} =
    Prelude.rnf snapshotTimeOfDay

instance Core.ToJSON AutoSnapshotAddOnRequest where
  toJSON AutoSnapshotAddOnRequest' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("snapshotTimeOfDay" Core..=)
              Prelude.<$> snapshotTimeOfDay
          ]
      )
