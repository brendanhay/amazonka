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
    vHost,
    vName,
  )
where

import qualified Network.AWS.Batch.Types.Host as Types
import qualified Network.AWS.Batch.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A data volume used in a job's container properties.
--
-- /See:/ 'mkVolume' smart constructor.
data Volume = Volume'
  { -- | The contents of the @host@ parameter determine whether your data volume persists on the host container instance and where it is stored. If the host parameter is empty, then the Docker daemon assigns a host path for your data volume. However, the data is not guaranteed to persist after the containers associated with it stop running.
    host :: Core.Maybe Types.Host,
    -- | The name of the volume. Up to 255 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed. This name is referenced in the @sourceVolume@ parameter of container definition @mountPoints@ .
    name :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Volume' value with any optional fields omitted.
mkVolume ::
  Volume
mkVolume = Volume' {host = Core.Nothing, name = Core.Nothing}

-- | The contents of the @host@ parameter determine whether your data volume persists on the host container instance and where it is stored. If the host parameter is empty, then the Docker daemon assigns a host path for your data volume. However, the data is not guaranteed to persist after the containers associated with it stop running.
--
-- /Note:/ Consider using 'host' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vHost :: Lens.Lens' Volume (Core.Maybe Types.Host)
vHost = Lens.field @"host"
{-# DEPRECATED vHost "Use generic-lens or generic-optics with 'host' instead." #-}

-- | The name of the volume. Up to 255 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed. This name is referenced in the @sourceVolume@ parameter of container definition @mountPoints@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vName :: Lens.Lens' Volume (Core.Maybe Types.String)
vName = Lens.field @"name"
{-# DEPRECATED vName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON Volume where
  toJSON Volume {..} =
    Core.object
      ( Core.catMaybes
          [("host" Core..=) Core.<$> host, ("name" Core..=) Core.<$> name]
      )

instance Core.FromJSON Volume where
  parseJSON =
    Core.withObject "Volume" Core.$
      \x ->
        Volume' Core.<$> (x Core..:? "host") Core.<*> (x Core..:? "name")
