-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.MountPoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.MountPoint
  ( MountPoint (..),

    -- * Smart constructor
    mkMountPoint,

    -- * Lenses
    mpContainerPath,
    mpSourceVolume,
    mpReadOnly,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Details on a volume mount point that is used in a container definition.
--
-- /See:/ 'mkMountPoint' smart constructor.
data MountPoint = MountPoint'
  { containerPath ::
      Lude.Maybe Lude.Text,
    sourceVolume :: Lude.Maybe Lude.Text,
    readOnly :: Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MountPoint' with the minimum fields required to make a request.
--
-- * 'containerPath' - The path on the container to mount the host volume at.
-- * 'readOnly' - If this value is @true@ , the container has read-only access to the volume. If this value is @false@ , then the container can write to the volume. The default value is @false@ .
-- * 'sourceVolume' - The name of the volume to mount. Must be a volume name referenced in the @name@ parameter of task definition @volume@ .
mkMountPoint ::
  MountPoint
mkMountPoint =
  MountPoint'
    { containerPath = Lude.Nothing,
      sourceVolume = Lude.Nothing,
      readOnly = Lude.Nothing
    }

-- | The path on the container to mount the host volume at.
--
-- /Note:/ Consider using 'containerPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpContainerPath :: Lens.Lens' MountPoint (Lude.Maybe Lude.Text)
mpContainerPath = Lens.lens (containerPath :: MountPoint -> Lude.Maybe Lude.Text) (\s a -> s {containerPath = a} :: MountPoint)
{-# DEPRECATED mpContainerPath "Use generic-lens or generic-optics with 'containerPath' instead." #-}

-- | The name of the volume to mount. Must be a volume name referenced in the @name@ parameter of task definition @volume@ .
--
-- /Note:/ Consider using 'sourceVolume' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpSourceVolume :: Lens.Lens' MountPoint (Lude.Maybe Lude.Text)
mpSourceVolume = Lens.lens (sourceVolume :: MountPoint -> Lude.Maybe Lude.Text) (\s a -> s {sourceVolume = a} :: MountPoint)
{-# DEPRECATED mpSourceVolume "Use generic-lens or generic-optics with 'sourceVolume' instead." #-}

-- | If this value is @true@ , the container has read-only access to the volume. If this value is @false@ , then the container can write to the volume. The default value is @false@ .
--
-- /Note:/ Consider using 'readOnly' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpReadOnly :: Lens.Lens' MountPoint (Lude.Maybe Lude.Bool)
mpReadOnly = Lens.lens (readOnly :: MountPoint -> Lude.Maybe Lude.Bool) (\s a -> s {readOnly = a} :: MountPoint)
{-# DEPRECATED mpReadOnly "Use generic-lens or generic-optics with 'readOnly' instead." #-}

instance Lude.FromJSON MountPoint where
  parseJSON =
    Lude.withObject
      "MountPoint"
      ( \x ->
          MountPoint'
            Lude.<$> (x Lude..:? "containerPath")
            Lude.<*> (x Lude..:? "sourceVolume")
            Lude.<*> (x Lude..:? "readOnly")
      )

instance Lude.ToJSON MountPoint where
  toJSON MountPoint' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("containerPath" Lude..=) Lude.<$> containerPath,
            ("sourceVolume" Lude..=) Lude.<$> sourceVolume,
            ("readOnly" Lude..=) Lude.<$> readOnly
          ]
      )
