{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.EnvironmentImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeBuild.Types.EnvironmentImage
  ( EnvironmentImage (..)
  -- * Smart constructor
  , mkEnvironmentImage
  -- * Lenses
  , eiDescription
  , eiName
  , eiVersions
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a Docker image that is managed by AWS CodeBuild.
--
-- /See:/ 'mkEnvironmentImage' smart constructor.
data EnvironmentImage = EnvironmentImage'
  { description :: Core.Maybe Core.Text
    -- ^ The description of the Docker image.
  , name :: Core.Maybe Core.Text
    -- ^ The name of the Docker image.
  , versions :: Core.Maybe [Core.Text]
    -- ^ A list of environment image versions.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EnvironmentImage' value with any optional fields omitted.
mkEnvironmentImage
    :: EnvironmentImage
mkEnvironmentImage
  = EnvironmentImage'{description = Core.Nothing,
                      name = Core.Nothing, versions = Core.Nothing}

-- | The description of the Docker image.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiDescription :: Lens.Lens' EnvironmentImage (Core.Maybe Core.Text)
eiDescription = Lens.field @"description"
{-# INLINEABLE eiDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The name of the Docker image.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiName :: Lens.Lens' EnvironmentImage (Core.Maybe Core.Text)
eiName = Lens.field @"name"
{-# INLINEABLE eiName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | A list of environment image versions.
--
-- /Note:/ Consider using 'versions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiVersions :: Lens.Lens' EnvironmentImage (Core.Maybe [Core.Text])
eiVersions = Lens.field @"versions"
{-# INLINEABLE eiVersions #-}
{-# DEPRECATED versions "Use generic-lens or generic-optics with 'versions' instead"  #-}

instance Core.FromJSON EnvironmentImage where
        parseJSON
          = Core.withObject "EnvironmentImage" Core.$
              \ x ->
                EnvironmentImage' Core.<$>
                  (x Core..:? "description") Core.<*> x Core..:? "name" Core.<*>
                    x Core..:? "versions"
