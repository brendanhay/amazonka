{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.EnvironmentImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.EnvironmentImage
  ( EnvironmentImage (..),

    -- * Smart constructor
    mkEnvironmentImage,

    -- * Lenses
    eiDescription,
    eiName,
    eiVersions,
  )
where

import qualified Network.AWS.CodeBuild.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a Docker image that is managed by AWS CodeBuild.
--
-- /See:/ 'mkEnvironmentImage' smart constructor.
data EnvironmentImage = EnvironmentImage'
  { -- | The description of the Docker image.
    description :: Core.Maybe Types.String,
    -- | The name of the Docker image.
    name :: Core.Maybe Types.String,
    -- | A list of environment image versions.
    versions :: Core.Maybe [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EnvironmentImage' value with any optional fields omitted.
mkEnvironmentImage ::
  EnvironmentImage
mkEnvironmentImage =
  EnvironmentImage'
    { description = Core.Nothing,
      name = Core.Nothing,
      versions = Core.Nothing
    }

-- | The description of the Docker image.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiDescription :: Lens.Lens' EnvironmentImage (Core.Maybe Types.String)
eiDescription = Lens.field @"description"
{-# DEPRECATED eiDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The name of the Docker image.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiName :: Lens.Lens' EnvironmentImage (Core.Maybe Types.String)
eiName = Lens.field @"name"
{-# DEPRECATED eiName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A list of environment image versions.
--
-- /Note:/ Consider using 'versions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiVersions :: Lens.Lens' EnvironmentImage (Core.Maybe [Types.String])
eiVersions = Lens.field @"versions"
{-# DEPRECATED eiVersions "Use generic-lens or generic-optics with 'versions' instead." #-}

instance Core.FromJSON EnvironmentImage where
  parseJSON =
    Core.withObject "EnvironmentImage" Core.$
      \x ->
        EnvironmentImage'
          Core.<$> (x Core..:? "description")
          Core.<*> (x Core..:? "name")
          Core.<*> (x Core..:? "versions")
