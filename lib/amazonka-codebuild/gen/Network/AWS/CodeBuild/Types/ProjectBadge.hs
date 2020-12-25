{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.ProjectBadge
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.ProjectBadge
  ( ProjectBadge (..),

    -- * Smart constructor
    mkProjectBadge,

    -- * Lenses
    pbBadgeEnabled,
    pbBadgeRequestUrl,
  )
where

import qualified Network.AWS.CodeBuild.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the build badge for the build project.
--
-- /See:/ 'mkProjectBadge' smart constructor.
data ProjectBadge = ProjectBadge'
  { -- | Set this to true to generate a publicly accessible URL for your project's build badge.
    badgeEnabled :: Core.Maybe Core.Bool,
    -- | The publicly-accessible URL through which you can access the build badge for your project.
    --
    -- The publicly accessible URL through which you can access the build badge for your project.
    badgeRequestUrl :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ProjectBadge' value with any optional fields omitted.
mkProjectBadge ::
  ProjectBadge
mkProjectBadge =
  ProjectBadge'
    { badgeEnabled = Core.Nothing,
      badgeRequestUrl = Core.Nothing
    }

-- | Set this to true to generate a publicly accessible URL for your project's build badge.
--
-- /Note:/ Consider using 'badgeEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbBadgeEnabled :: Lens.Lens' ProjectBadge (Core.Maybe Core.Bool)
pbBadgeEnabled = Lens.field @"badgeEnabled"
{-# DEPRECATED pbBadgeEnabled "Use generic-lens or generic-optics with 'badgeEnabled' instead." #-}

-- | The publicly-accessible URL through which you can access the build badge for your project.
--
-- The publicly accessible URL through which you can access the build badge for your project.
--
-- /Note:/ Consider using 'badgeRequestUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbBadgeRequestUrl :: Lens.Lens' ProjectBadge (Core.Maybe Types.String)
pbBadgeRequestUrl = Lens.field @"badgeRequestUrl"
{-# DEPRECATED pbBadgeRequestUrl "Use generic-lens or generic-optics with 'badgeRequestUrl' instead." #-}

instance Core.FromJSON ProjectBadge where
  parseJSON =
    Core.withObject "ProjectBadge" Core.$
      \x ->
        ProjectBadge'
          Core.<$> (x Core..:? "badgeEnabled") Core.<*> (x Core..:? "badgeRequestUrl")
