{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.EnvironmentLanguage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.EnvironmentLanguage
  ( EnvironmentLanguage (..),

    -- * Smart constructor
    mkEnvironmentLanguage,

    -- * Lenses
    elImages,
    elLanguage,
  )
where

import Network.AWS.CodeBuild.Types.EnvironmentImage
import Network.AWS.CodeBuild.Types.LanguageType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A set of Docker images that are related by programming language and are managed by AWS CodeBuild.
--
-- /See:/ 'mkEnvironmentLanguage' smart constructor.
data EnvironmentLanguage = EnvironmentLanguage'
  { images ::
      Lude.Maybe [EnvironmentImage],
    language :: Lude.Maybe LanguageType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnvironmentLanguage' with the minimum fields required to make a request.
--
-- * 'images' - The list of Docker images that are related by the specified programming language.
-- * 'language' - The programming language for the Docker images.
mkEnvironmentLanguage ::
  EnvironmentLanguage
mkEnvironmentLanguage =
  EnvironmentLanguage'
    { images = Lude.Nothing,
      language = Lude.Nothing
    }

-- | The list of Docker images that are related by the specified programming language.
--
-- /Note:/ Consider using 'images' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
elImages :: Lens.Lens' EnvironmentLanguage (Lude.Maybe [EnvironmentImage])
elImages = Lens.lens (images :: EnvironmentLanguage -> Lude.Maybe [EnvironmentImage]) (\s a -> s {images = a} :: EnvironmentLanguage)
{-# DEPRECATED elImages "Use generic-lens or generic-optics with 'images' instead." #-}

-- | The programming language for the Docker images.
--
-- /Note:/ Consider using 'language' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
elLanguage :: Lens.Lens' EnvironmentLanguage (Lude.Maybe LanguageType)
elLanguage = Lens.lens (language :: EnvironmentLanguage -> Lude.Maybe LanguageType) (\s a -> s {language = a} :: EnvironmentLanguage)
{-# DEPRECATED elLanguage "Use generic-lens or generic-optics with 'language' instead." #-}

instance Lude.FromJSON EnvironmentLanguage where
  parseJSON =
    Lude.withObject
      "EnvironmentLanguage"
      ( \x ->
          EnvironmentLanguage'
            Lude.<$> (x Lude..:? "images" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "language")
      )
