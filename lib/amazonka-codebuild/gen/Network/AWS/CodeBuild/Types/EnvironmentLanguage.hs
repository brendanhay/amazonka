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

import qualified Network.AWS.CodeBuild.Types.EnvironmentImage as Types
import qualified Network.AWS.CodeBuild.Types.LanguageType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A set of Docker images that are related by programming language and are managed by AWS CodeBuild.
--
-- /See:/ 'mkEnvironmentLanguage' smart constructor.
data EnvironmentLanguage = EnvironmentLanguage'
  { -- | The list of Docker images that are related by the specified programming language.
    images :: Core.Maybe [Types.EnvironmentImage],
    -- | The programming language for the Docker images.
    language :: Core.Maybe Types.LanguageType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EnvironmentLanguage' value with any optional fields omitted.
mkEnvironmentLanguage ::
  EnvironmentLanguage
mkEnvironmentLanguage =
  EnvironmentLanguage'
    { images = Core.Nothing,
      language = Core.Nothing
    }

-- | The list of Docker images that are related by the specified programming language.
--
-- /Note:/ Consider using 'images' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
elImages :: Lens.Lens' EnvironmentLanguage (Core.Maybe [Types.EnvironmentImage])
elImages = Lens.field @"images"
{-# DEPRECATED elImages "Use generic-lens or generic-optics with 'images' instead." #-}

-- | The programming language for the Docker images.
--
-- /Note:/ Consider using 'language' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
elLanguage :: Lens.Lens' EnvironmentLanguage (Core.Maybe Types.LanguageType)
elLanguage = Lens.field @"language"
{-# DEPRECATED elLanguage "Use generic-lens or generic-optics with 'language' instead." #-}

instance Core.FromJSON EnvironmentLanguage where
  parseJSON =
    Core.withObject "EnvironmentLanguage" Core.$
      \x ->
        EnvironmentLanguage'
          Core.<$> (x Core..:? "images") Core.<*> (x Core..:? "language")
