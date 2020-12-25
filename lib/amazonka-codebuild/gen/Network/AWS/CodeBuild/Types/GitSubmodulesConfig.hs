{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.GitSubmodulesConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.GitSubmodulesConfig
  ( GitSubmodulesConfig (..),

    -- * Smart constructor
    mkGitSubmodulesConfig,

    -- * Lenses
    gscFetchSubmodules,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the Git submodules configuration for an AWS CodeBuild build project.
--
-- /See:/ 'mkGitSubmodulesConfig' smart constructor.
newtype GitSubmodulesConfig = GitSubmodulesConfig'
  { -- | Set to true to fetch Git submodules for your AWS CodeBuild build project.
    fetchSubmodules :: Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GitSubmodulesConfig' value with any optional fields omitted.
mkGitSubmodulesConfig ::
  -- | 'fetchSubmodules'
  Core.Bool ->
  GitSubmodulesConfig
mkGitSubmodulesConfig fetchSubmodules =
  GitSubmodulesConfig' {fetchSubmodules}

-- | Set to true to fetch Git submodules for your AWS CodeBuild build project.
--
-- /Note:/ Consider using 'fetchSubmodules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gscFetchSubmodules :: Lens.Lens' GitSubmodulesConfig Core.Bool
gscFetchSubmodules = Lens.field @"fetchSubmodules"
{-# DEPRECATED gscFetchSubmodules "Use generic-lens or generic-optics with 'fetchSubmodules' instead." #-}

instance Core.FromJSON GitSubmodulesConfig where
  toJSON GitSubmodulesConfig {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("fetchSubmodules" Core..= fetchSubmodules)]
      )

instance Core.FromJSON GitSubmodulesConfig where
  parseJSON =
    Core.withObject "GitSubmodulesConfig" Core.$
      \x -> GitSubmodulesConfig' Core.<$> (x Core..: "fetchSubmodules")
