-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.AddOn
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.AddOn
  ( AddOn (..),

    -- * Smart constructor
    mkAddOn,

    -- * Lenses
    aoStatus,
    aoNextSnapshotTimeOfDay,
    aoSnapshotTimeOfDay,
    aoName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an add-on that is enabled for an Amazon Lightsail resource.
--
-- /See:/ 'mkAddOn' smart constructor.
data AddOn = AddOn'
  { status :: Lude.Maybe Lude.Text,
    nextSnapshotTimeOfDay :: Lude.Maybe Lude.Text,
    snapshotTimeOfDay :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddOn' with the minimum fields required to make a request.
--
-- * 'name' - The name of the add-on.
-- * 'nextSnapshotTimeOfDay' - The next daily time an automatic snapshot will be created.
--
-- The time shown is in @HH:00@ format, and in Coordinated Universal Time (UTC).
-- The snapshot is automatically created between the time shown and up to 45 minutes after.
-- * 'snapshotTimeOfDay' - The daily time when an automatic snapshot is created.
--
-- The time shown is in @HH:00@ format, and in Coordinated Universal Time (UTC).
-- The snapshot is automatically created between the time shown and up to 45 minutes after.
-- * 'status' - The status of the add-on.
mkAddOn ::
  AddOn
mkAddOn =
  AddOn'
    { status = Lude.Nothing,
      nextSnapshotTimeOfDay = Lude.Nothing,
      snapshotTimeOfDay = Lude.Nothing,
      name = Lude.Nothing
    }

-- | The status of the add-on.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aoStatus :: Lens.Lens' AddOn (Lude.Maybe Lude.Text)
aoStatus = Lens.lens (status :: AddOn -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: AddOn)
{-# DEPRECATED aoStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The next daily time an automatic snapshot will be created.
--
-- The time shown is in @HH:00@ format, and in Coordinated Universal Time (UTC).
-- The snapshot is automatically created between the time shown and up to 45 minutes after.
--
-- /Note:/ Consider using 'nextSnapshotTimeOfDay' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aoNextSnapshotTimeOfDay :: Lens.Lens' AddOn (Lude.Maybe Lude.Text)
aoNextSnapshotTimeOfDay = Lens.lens (nextSnapshotTimeOfDay :: AddOn -> Lude.Maybe Lude.Text) (\s a -> s {nextSnapshotTimeOfDay = a} :: AddOn)
{-# DEPRECATED aoNextSnapshotTimeOfDay "Use generic-lens or generic-optics with 'nextSnapshotTimeOfDay' instead." #-}

-- | The daily time when an automatic snapshot is created.
--
-- The time shown is in @HH:00@ format, and in Coordinated Universal Time (UTC).
-- The snapshot is automatically created between the time shown and up to 45 minutes after.
--
-- /Note:/ Consider using 'snapshotTimeOfDay' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aoSnapshotTimeOfDay :: Lens.Lens' AddOn (Lude.Maybe Lude.Text)
aoSnapshotTimeOfDay = Lens.lens (snapshotTimeOfDay :: AddOn -> Lude.Maybe Lude.Text) (\s a -> s {snapshotTimeOfDay = a} :: AddOn)
{-# DEPRECATED aoSnapshotTimeOfDay "Use generic-lens or generic-optics with 'snapshotTimeOfDay' instead." #-}

-- | The name of the add-on.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aoName :: Lens.Lens' AddOn (Lude.Maybe Lude.Text)
aoName = Lens.lens (name :: AddOn -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: AddOn)
{-# DEPRECATED aoName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON AddOn where
  parseJSON =
    Lude.withObject
      "AddOn"
      ( \x ->
          AddOn'
            Lude.<$> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "nextSnapshotTimeOfDay")
            Lude.<*> (x Lude..:? "snapshotTimeOfDay")
            Lude.<*> (x Lude..:? "name")
      )
