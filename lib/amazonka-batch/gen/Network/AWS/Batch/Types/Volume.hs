{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.Volume
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.Volume
  ( Volume (..),

    -- * Smart constructor
    mkVolume,

    -- * Lenses
    vName,
    vHost,
  )
where

import Network.AWS.Batch.Types.Host
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A data volume used in a job's container properties.
--
-- /See:/ 'mkVolume' smart constructor.
data Volume = Volume'
  { -- | The name of the volume. Up to 255 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed. This name is referenced in the @sourceVolume@ parameter of container definition @mountPoints@ .
    name :: Lude.Maybe Lude.Text,
    -- | The contents of the @host@ parameter determine whether your data volume persists on the host container instance and where it is stored. If the host parameter is empty, then the Docker daemon assigns a host path for your data volume. However, the data is not guaranteed to persist after the containers associated with it stop running.
    host :: Lude.Maybe Host
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Volume' with the minimum fields required to make a request.
--
-- * 'name' - The name of the volume. Up to 255 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed. This name is referenced in the @sourceVolume@ parameter of container definition @mountPoints@ .
-- * 'host' - The contents of the @host@ parameter determine whether your data volume persists on the host container instance and where it is stored. If the host parameter is empty, then the Docker daemon assigns a host path for your data volume. However, the data is not guaranteed to persist after the containers associated with it stop running.
mkVolume ::
  Volume
mkVolume = Volume' {name = Lude.Nothing, host = Lude.Nothing}

-- | The name of the volume. Up to 255 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed. This name is referenced in the @sourceVolume@ parameter of container definition @mountPoints@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vName :: Lens.Lens' Volume (Lude.Maybe Lude.Text)
vName = Lens.lens (name :: Volume -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Volume)
{-# DEPRECATED vName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The contents of the @host@ parameter determine whether your data volume persists on the host container instance and where it is stored. If the host parameter is empty, then the Docker daemon assigns a host path for your data volume. However, the data is not guaranteed to persist after the containers associated with it stop running.
--
-- /Note:/ Consider using 'host' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vHost :: Lens.Lens' Volume (Lude.Maybe Host)
vHost = Lens.lens (host :: Volume -> Lude.Maybe Host) (\s a -> s {host = a} :: Volume)
{-# DEPRECATED vHost "Use generic-lens or generic-optics with 'host' instead." #-}

instance Lude.FromJSON Volume where
  parseJSON =
    Lude.withObject
      "Volume"
      ( \x ->
          Volume' Lude.<$> (x Lude..:? "name") Lude.<*> (x Lude..:? "host")
      )

instance Lude.ToJSON Volume where
  toJSON Volume' {..} =
    Lude.object
      ( Lude.catMaybes
          [("name" Lude..=) Lude.<$> name, ("host" Lude..=) Lude.<$> host]
      )
