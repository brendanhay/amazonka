-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.VolumeFrom
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.VolumeFrom
  ( VolumeFrom (..),

    -- * Smart constructor
    mkVolumeFrom,

    -- * Lenses
    vfSourceContainer,
    vfReadOnly,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Details on a data volume from another container in the same task definition.
--
-- /See:/ 'mkVolumeFrom' smart constructor.
data VolumeFrom = VolumeFrom'
  { sourceContainer ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'VolumeFrom' with the minimum fields required to make a request.
--
-- * 'readOnly' - If this value is @true@ , the container has read-only access to the volume. If this value is @false@ , then the container can write to the volume. The default value is @false@ .
-- * 'sourceContainer' - The name of another container within the same task definition from which to mount volumes.
mkVolumeFrom ::
  VolumeFrom
mkVolumeFrom =
  VolumeFrom'
    { sourceContainer = Lude.Nothing,
      readOnly = Lude.Nothing
    }

-- | The name of another container within the same task definition from which to mount volumes.
--
-- /Note:/ Consider using 'sourceContainer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vfSourceContainer :: Lens.Lens' VolumeFrom (Lude.Maybe Lude.Text)
vfSourceContainer = Lens.lens (sourceContainer :: VolumeFrom -> Lude.Maybe Lude.Text) (\s a -> s {sourceContainer = a} :: VolumeFrom)
{-# DEPRECATED vfSourceContainer "Use generic-lens or generic-optics with 'sourceContainer' instead." #-}

-- | If this value is @true@ , the container has read-only access to the volume. If this value is @false@ , then the container can write to the volume. The default value is @false@ .
--
-- /Note:/ Consider using 'readOnly' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vfReadOnly :: Lens.Lens' VolumeFrom (Lude.Maybe Lude.Bool)
vfReadOnly = Lens.lens (readOnly :: VolumeFrom -> Lude.Maybe Lude.Bool) (\s a -> s {readOnly = a} :: VolumeFrom)
{-# DEPRECATED vfReadOnly "Use generic-lens or generic-optics with 'readOnly' instead." #-}

instance Lude.FromJSON VolumeFrom where
  parseJSON =
    Lude.withObject
      "VolumeFrom"
      ( \x ->
          VolumeFrom'
            Lude.<$> (x Lude..:? "sourceContainer") Lude.<*> (x Lude..:? "readOnly")
      )

instance Lude.ToJSON VolumeFrom where
  toJSON VolumeFrom' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("sourceContainer" Lude..=) Lude.<$> sourceContainer,
            ("readOnly" Lude..=) Lude.<$> readOnly
          ]
      )
