-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.AutoSnapshotAddOnRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.AutoSnapshotAddOnRequest
  ( AutoSnapshotAddOnRequest (..),

    -- * Smart constructor
    mkAutoSnapshotAddOnRequest,

    -- * Lenses
    asaorSnapshotTimeOfDay,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a request to enable or modify the automatic snapshot add-on for an Amazon Lightsail instance or disk.
--
-- When you modify the automatic snapshot time for a resource, it is typically effective immediately except under the following conditions:
--
--     * If an automatic snapshot has been created for the current day, and you change the snapshot time to a later time of day, then the new snapshot time will be effective the following day. This ensures that two snapshots are not created for the current day.
--
--
--     * If an automatic snapshot has not yet been created for the current day, and you change the snapshot time to an earlier time of day, then the new snapshot time will be effective the following day and a snapshot is automatically created at the previously set time for the current day. This ensures that a snapshot is created for the current day.
--
--
--     * If an automatic snapshot has not yet been created for the current day, and you change the snapshot time to a time that is within 30 minutes from your current time, then the new snapshot time will be effective the following day and a snapshot is automatically created at the previously set time for the current day. This ensures that a snapshot is created for the current day, because 30 minutes is required between your current time and the new snapshot time that you specify.
--
--
--     * If an automatic snapshot is scheduled to be created within 30 minutes from your current time and you change the snapshot time, then the new snapshot time will be effective the following day and a snapshot is automatically created at the previously set time for the current day. This ensures that a snapshot is created for the current day, because 30 minutes is required between your current time and the new snapshot time that you specify.
--
--
--
-- /See:/ 'mkAutoSnapshotAddOnRequest' smart constructor.
newtype AutoSnapshotAddOnRequest = AutoSnapshotAddOnRequest'
  { snapshotTimeOfDay ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AutoSnapshotAddOnRequest' with the minimum fields required to make a request.
--
-- * 'snapshotTimeOfDay' - The daily time when an automatic snapshot will be created.
--
-- Constraints:
--
--     * Must be in @HH:00@ format, and in an hourly increment.
--
--
--     * Specified in Coordinated Universal Time (UTC).
--
--
--     * The snapshot will be automatically created between the time specified and up to 45 minutes after.
mkAutoSnapshotAddOnRequest ::
  AutoSnapshotAddOnRequest
mkAutoSnapshotAddOnRequest =
  AutoSnapshotAddOnRequest' {snapshotTimeOfDay = Lude.Nothing}

-- | The daily time when an automatic snapshot will be created.
--
-- Constraints:
--
--     * Must be in @HH:00@ format, and in an hourly increment.
--
--
--     * Specified in Coordinated Universal Time (UTC).
--
--
--     * The snapshot will be automatically created between the time specified and up to 45 minutes after.
--
--
--
-- /Note:/ Consider using 'snapshotTimeOfDay' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asaorSnapshotTimeOfDay :: Lens.Lens' AutoSnapshotAddOnRequest (Lude.Maybe Lude.Text)
asaorSnapshotTimeOfDay = Lens.lens (snapshotTimeOfDay :: AutoSnapshotAddOnRequest -> Lude.Maybe Lude.Text) (\s a -> s {snapshotTimeOfDay = a} :: AutoSnapshotAddOnRequest)
{-# DEPRECATED asaorSnapshotTimeOfDay "Use generic-lens or generic-optics with 'snapshotTimeOfDay' instead." #-}

instance Lude.ToJSON AutoSnapshotAddOnRequest where
  toJSON AutoSnapshotAddOnRequest' {..} =
    Lude.object
      ( Lude.catMaybes
          [("snapshotTimeOfDay" Lude..=) Lude.<$> snapshotTimeOfDay]
      )
