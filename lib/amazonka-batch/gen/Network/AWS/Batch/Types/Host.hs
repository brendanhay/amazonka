{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.Host
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.Host
  ( Host (..),

    -- * Smart constructor
    mkHost,

    -- * Lenses
    hSourcePath,
  )
where

import qualified Network.AWS.Batch.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Determine whether your data volume persists on the host container instance and where it is stored. If this parameter is empty, then the Docker daemon assigns a host path for your data volume, but the data is not guaranteed to persist after the containers associated with it stop running.
--
-- /See:/ 'mkHost' smart constructor.
newtype Host = Host'
  { -- | The path on the host container instance that is presented to the container. If this parameter is empty, then the Docker daemon has assigned a host path for you. If this parameter contains a file location, then the data volume persists at the specified location on the host container instance until you delete it manually. If the source path location does not exist on the host container instance, the Docker daemon creates it. If the location does exist, the contents of the source path folder are exported.
    sourcePath :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'Host' value with any optional fields omitted.
mkHost ::
  Host
mkHost = Host' {sourcePath = Core.Nothing}

-- | The path on the host container instance that is presented to the container. If this parameter is empty, then the Docker daemon has assigned a host path for you. If this parameter contains a file location, then the data volume persists at the specified location on the host container instance until you delete it manually. If the source path location does not exist on the host container instance, the Docker daemon creates it. If the location does exist, the contents of the source path folder are exported.
--
-- /Note:/ Consider using 'sourcePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hSourcePath :: Lens.Lens' Host (Core.Maybe Types.String)
hSourcePath = Lens.field @"sourcePath"
{-# DEPRECATED hSourcePath "Use generic-lens or generic-optics with 'sourcePath' instead." #-}

instance Core.FromJSON Host where
  toJSON Host {..} =
    Core.object
      (Core.catMaybes [("sourcePath" Core..=) Core.<$> sourcePath])

instance Core.FromJSON Host where
  parseJSON =
    Core.withObject "Host" Core.$
      \x -> Host' Core.<$> (x Core..:? "sourcePath")
