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
    epPlatform,
    epLanguages,
  )
where

import Network.AWS.CodeBuild.Types.EnvironmentLanguage
import Network.AWS.CodeBuild.Types.PlatformType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A set of Docker images that are related by platform and are managed by AWS CodeBuild.
--
-- /See:/ 'mkEnvironmentPlatform' smart constructor.
data EnvironmentPlatform = EnvironmentPlatform'
  { platform ::
      Lude.Maybe PlatformType,
    languages :: Lude.Maybe [EnvironmentLanguage]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnvironmentPlatform' with the minimum fields required to make a request.
--
-- * 'languages' - The list of programming languages that are available for the specified platform.
-- * 'platform' - The platform's name.
mkEnvironmentPlatform ::
  EnvironmentPlatform
mkEnvironmentPlatform =
  EnvironmentPlatform'
    { platform = Lude.Nothing,
      languages = Lude.Nothing
    }

-- | The platform's name.
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epPlatform :: Lens.Lens' EnvironmentPlatform (Lude.Maybe PlatformType)
epPlatform = Lens.lens (platform :: EnvironmentPlatform -> Lude.Maybe PlatformType) (\s a -> s {platform = a} :: EnvironmentPlatform)
{-# DEPRECATED epPlatform "Use generic-lens or generic-optics with 'platform' instead." #-}

-- | The list of programming languages that are available for the specified platform.
--
-- /Note:/ Consider using 'languages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epLanguages :: Lens.Lens' EnvironmentPlatform (Lude.Maybe [EnvironmentLanguage])
epLanguages = Lens.lens (languages :: EnvironmentPlatform -> Lude.Maybe [EnvironmentLanguage]) (\s a -> s {languages = a} :: EnvironmentPlatform)
{-# DEPRECATED epLanguages "Use generic-lens or generic-optics with 'languages' instead." #-}

instance Lude.FromJSON EnvironmentPlatform where
  parseJSON =
    Lude.withObject
      "EnvironmentPlatform"
      ( \x ->
          EnvironmentPlatform'
            Lude.<$> (x Lude..:? "platform")
            Lude.<*> (x Lude..:? "languages" Lude..!= Lude.mempty)
      )
