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
    eiVersions,
    eiName,
    eiDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a Docker image that is managed by AWS CodeBuild.
--
-- /See:/ 'mkEnvironmentImage' smart constructor.
data EnvironmentImage = EnvironmentImage'
  { versions ::
      Lude.Maybe [Lude.Text],
    name :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnvironmentImage' with the minimum fields required to make a request.
--
-- * 'description' - The description of the Docker image.
-- * 'name' - The name of the Docker image.
-- * 'versions' - A list of environment image versions.
mkEnvironmentImage ::
  EnvironmentImage
mkEnvironmentImage =
  EnvironmentImage'
    { versions = Lude.Nothing,
      name = Lude.Nothing,
      description = Lude.Nothing
    }

-- | A list of environment image versions.
--
-- /Note:/ Consider using 'versions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiVersions :: Lens.Lens' EnvironmentImage (Lude.Maybe [Lude.Text])
eiVersions = Lens.lens (versions :: EnvironmentImage -> Lude.Maybe [Lude.Text]) (\s a -> s {versions = a} :: EnvironmentImage)
{-# DEPRECATED eiVersions "Use generic-lens or generic-optics with 'versions' instead." #-}

-- | The name of the Docker image.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiName :: Lens.Lens' EnvironmentImage (Lude.Maybe Lude.Text)
eiName = Lens.lens (name :: EnvironmentImage -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: EnvironmentImage)
{-# DEPRECATED eiName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The description of the Docker image.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiDescription :: Lens.Lens' EnvironmentImage (Lude.Maybe Lude.Text)
eiDescription = Lens.lens (description :: EnvironmentImage -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: EnvironmentImage)
{-# DEPRECATED eiDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON EnvironmentImage where
  parseJSON =
    Lude.withObject
      "EnvironmentImage"
      ( \x ->
          EnvironmentImage'
            Lude.<$> (x Lude..:? "versions" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "description")
      )
