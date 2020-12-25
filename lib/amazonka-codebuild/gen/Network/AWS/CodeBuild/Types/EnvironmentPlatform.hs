{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.EnvironmentPlatform
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.EnvironmentPlatform
  ( EnvironmentPlatform (..),

    -- * Smart constructor
    mkEnvironmentPlatform,

    -- * Lenses
    epLanguages,
    epPlatform,
  )
where

import qualified Network.AWS.CodeBuild.Types.EnvironmentLanguage as Types
import qualified Network.AWS.CodeBuild.Types.PlatformType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A set of Docker images that are related by platform and are managed by AWS CodeBuild.
--
-- /See:/ 'mkEnvironmentPlatform' smart constructor.
data EnvironmentPlatform = EnvironmentPlatform'
  { -- | The list of programming languages that are available for the specified platform.
    languages :: Core.Maybe [Types.EnvironmentLanguage],
    -- | The platform's name.
    platform :: Core.Maybe Types.PlatformType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EnvironmentPlatform' value with any optional fields omitted.
mkEnvironmentPlatform ::
  EnvironmentPlatform
mkEnvironmentPlatform =
  EnvironmentPlatform'
    { languages = Core.Nothing,
      platform = Core.Nothing
    }

-- | The list of programming languages that are available for the specified platform.
--
-- /Note:/ Consider using 'languages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epLanguages :: Lens.Lens' EnvironmentPlatform (Core.Maybe [Types.EnvironmentLanguage])
epLanguages = Lens.field @"languages"
{-# DEPRECATED epLanguages "Use generic-lens or generic-optics with 'languages' instead." #-}

-- | The platform's name.
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epPlatform :: Lens.Lens' EnvironmentPlatform (Core.Maybe Types.PlatformType)
epPlatform = Lens.field @"platform"
{-# DEPRECATED epPlatform "Use generic-lens or generic-optics with 'platform' instead." #-}

instance Core.FromJSON EnvironmentPlatform where
  parseJSON =
    Core.withObject "EnvironmentPlatform" Core.$
      \x ->
        EnvironmentPlatform'
          Core.<$> (x Core..:? "languages") Core.<*> (x Core..:? "platform")
