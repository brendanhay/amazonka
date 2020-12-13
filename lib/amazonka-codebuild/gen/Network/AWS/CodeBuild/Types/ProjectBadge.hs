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
    pbBadgeRequestURL,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the build badge for the build project.
--
-- /See:/ 'mkProjectBadge' smart constructor.
data ProjectBadge = ProjectBadge'
  { -- | Set this to true to generate a publicly accessible URL for your project's build badge.
    badgeEnabled :: Lude.Maybe Lude.Bool,
    -- | The publicly-accessible URL through which you can access the build badge for your project.
    --
    -- The publicly accessible URL through which you can access the build badge for your project.
    badgeRequestURL :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProjectBadge' with the minimum fields required to make a request.
--
-- * 'badgeEnabled' - Set this to true to generate a publicly accessible URL for your project's build badge.
-- * 'badgeRequestURL' - The publicly-accessible URL through which you can access the build badge for your project.
--
-- The publicly accessible URL through which you can access the build badge for your project.
mkProjectBadge ::
  ProjectBadge
mkProjectBadge =
  ProjectBadge'
    { badgeEnabled = Lude.Nothing,
      badgeRequestURL = Lude.Nothing
    }

-- | Set this to true to generate a publicly accessible URL for your project's build badge.
--
-- /Note:/ Consider using 'badgeEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbBadgeEnabled :: Lens.Lens' ProjectBadge (Lude.Maybe Lude.Bool)
pbBadgeEnabled = Lens.lens (badgeEnabled :: ProjectBadge -> Lude.Maybe Lude.Bool) (\s a -> s {badgeEnabled = a} :: ProjectBadge)
{-# DEPRECATED pbBadgeEnabled "Use generic-lens or generic-optics with 'badgeEnabled' instead." #-}

-- | The publicly-accessible URL through which you can access the build badge for your project.
--
-- The publicly accessible URL through which you can access the build badge for your project.
--
-- /Note:/ Consider using 'badgeRequestURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbBadgeRequestURL :: Lens.Lens' ProjectBadge (Lude.Maybe Lude.Text)
pbBadgeRequestURL = Lens.lens (badgeRequestURL :: ProjectBadge -> Lude.Maybe Lude.Text) (\s a -> s {badgeRequestURL = a} :: ProjectBadge)
{-# DEPRECATED pbBadgeRequestURL "Use generic-lens or generic-optics with 'badgeRequestURL' instead." #-}

instance Lude.FromJSON ProjectBadge where
  parseJSON =
    Lude.withObject
      "ProjectBadge"
      ( \x ->
          ProjectBadge'
            Lude.<$> (x Lude..:? "badgeEnabled")
            Lude.<*> (x Lude..:? "badgeRequestUrl")
      )
