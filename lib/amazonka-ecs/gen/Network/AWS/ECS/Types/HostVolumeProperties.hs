{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.HostVolumeProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.HostVolumeProperties
  ( HostVolumeProperties (..),

    -- * Smart constructor
    mkHostVolumeProperties,

    -- * Lenses
    hvpSourcePath,
  )
where

import qualified Network.AWS.ECS.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Details on a container instance bind mount host volume.
--
-- /See:/ 'mkHostVolumeProperties' smart constructor.
newtype HostVolumeProperties = HostVolumeProperties'
  { -- | When the @host@ parameter is used, specify a @sourcePath@ to declare the path on the host container instance that is presented to the container. If this parameter is empty, then the Docker daemon has assigned a host path for you. If the @host@ parameter contains a @sourcePath@ file location, then the data volume persists at the specified location on the host container instance until you delete it manually. If the @sourcePath@ value does not exist on the host container instance, the Docker daemon creates it. If the location does exist, the contents of the source path folder are exported.
    --
    -- If you are using the Fargate launch type, the @sourcePath@ parameter is not supported.
    sourcePath :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'HostVolumeProperties' value with any optional fields omitted.
mkHostVolumeProperties ::
  HostVolumeProperties
mkHostVolumeProperties =
  HostVolumeProperties' {sourcePath = Core.Nothing}

-- | When the @host@ parameter is used, specify a @sourcePath@ to declare the path on the host container instance that is presented to the container. If this parameter is empty, then the Docker daemon has assigned a host path for you. If the @host@ parameter contains a @sourcePath@ file location, then the data volume persists at the specified location on the host container instance until you delete it manually. If the @sourcePath@ value does not exist on the host container instance, the Docker daemon creates it. If the location does exist, the contents of the source path folder are exported.
--
-- If you are using the Fargate launch type, the @sourcePath@ parameter is not supported.
--
-- /Note:/ Consider using 'sourcePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hvpSourcePath :: Lens.Lens' HostVolumeProperties (Core.Maybe Types.String)
hvpSourcePath = Lens.field @"sourcePath"
{-# DEPRECATED hvpSourcePath "Use generic-lens or generic-optics with 'sourcePath' instead." #-}

instance Core.FromJSON HostVolumeProperties where
  toJSON HostVolumeProperties {..} =
    Core.object
      (Core.catMaybes [("sourcePath" Core..=) Core.<$> sourcePath])

instance Core.FromJSON HostVolumeProperties where
  parseJSON =
    Core.withObject "HostVolumeProperties" Core.$
      \x -> HostVolumeProperties' Core.<$> (x Core..:? "sourcePath")
