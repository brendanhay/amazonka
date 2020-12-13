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
import qualified Network.AWS.Prelude as Lude

-- | Information about the Git submodules configuration for an AWS CodeBuild build project.
--
-- /See:/ 'mkGitSubmodulesConfig' smart constructor.
newtype GitSubmodulesConfig = GitSubmodulesConfig'
  { -- | Set to true to fetch Git submodules for your AWS CodeBuild build project.
    fetchSubmodules :: Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GitSubmodulesConfig' with the minimum fields required to make a request.
--
-- * 'fetchSubmodules' - Set to true to fetch Git submodules for your AWS CodeBuild build project.
mkGitSubmodulesConfig ::
  -- | 'fetchSubmodules'
  Lude.Bool ->
  GitSubmodulesConfig
mkGitSubmodulesConfig pFetchSubmodules_ =
  GitSubmodulesConfig' {fetchSubmodules = pFetchSubmodules_}

-- | Set to true to fetch Git submodules for your AWS CodeBuild build project.
--
-- /Note:/ Consider using 'fetchSubmodules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gscFetchSubmodules :: Lens.Lens' GitSubmodulesConfig Lude.Bool
gscFetchSubmodules = Lens.lens (fetchSubmodules :: GitSubmodulesConfig -> Lude.Bool) (\s a -> s {fetchSubmodules = a} :: GitSubmodulesConfig)
{-# DEPRECATED gscFetchSubmodules "Use generic-lens or generic-optics with 'fetchSubmodules' instead." #-}

instance Lude.FromJSON GitSubmodulesConfig where
  parseJSON =
    Lude.withObject
      "GitSubmodulesConfig"
      ( \x ->
          GitSubmodulesConfig' Lude.<$> (x Lude..: "fetchSubmodules")
      )

instance Lude.ToJSON GitSubmodulesConfig where
  toJSON GitSubmodulesConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("fetchSubmodules" Lude..= fetchSubmodules)]
      )
