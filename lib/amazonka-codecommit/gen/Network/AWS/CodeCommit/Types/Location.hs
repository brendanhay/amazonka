{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.Location
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.Location
  ( Location (..),

    -- * Smart constructor
    mkLocation,

    -- * Lenses
    lRelativeFileVersion,
    lFilePath,
    lFilePosition,
  )
where

import Network.AWS.CodeCommit.Types.RelativeFileVersionEnum
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Returns information about the location of a change or comment in the comparison between two commits or a pull request.
--
-- /See:/ 'mkLocation' smart constructor.
data Location = Location'
  { -- | In a comparison of commits or a pull request, whether the change is in the before or after of that comparison.
    relativeFileVersion :: Lude.Maybe RelativeFileVersionEnum,
    -- | The name of the file being compared, including its extension and subdirectory, if any.
    filePath :: Lude.Maybe Lude.Text,
    -- | The position of a change in a compared file, in line number format.
    filePosition :: Lude.Maybe Lude.Integer
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Location' with the minimum fields required to make a request.
--
-- * 'relativeFileVersion' - In a comparison of commits or a pull request, whether the change is in the before or after of that comparison.
-- * 'filePath' - The name of the file being compared, including its extension and subdirectory, if any.
-- * 'filePosition' - The position of a change in a compared file, in line number format.
mkLocation ::
  Location
mkLocation =
  Location'
    { relativeFileVersion = Lude.Nothing,
      filePath = Lude.Nothing,
      filePosition = Lude.Nothing
    }

-- | In a comparison of commits or a pull request, whether the change is in the before or after of that comparison.
--
-- /Note:/ Consider using 'relativeFileVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lRelativeFileVersion :: Lens.Lens' Location (Lude.Maybe RelativeFileVersionEnum)
lRelativeFileVersion = Lens.lens (relativeFileVersion :: Location -> Lude.Maybe RelativeFileVersionEnum) (\s a -> s {relativeFileVersion = a} :: Location)
{-# DEPRECATED lRelativeFileVersion "Use generic-lens or generic-optics with 'relativeFileVersion' instead." #-}

-- | The name of the file being compared, including its extension and subdirectory, if any.
--
-- /Note:/ Consider using 'filePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lFilePath :: Lens.Lens' Location (Lude.Maybe Lude.Text)
lFilePath = Lens.lens (filePath :: Location -> Lude.Maybe Lude.Text) (\s a -> s {filePath = a} :: Location)
{-# DEPRECATED lFilePath "Use generic-lens or generic-optics with 'filePath' instead." #-}

-- | The position of a change in a compared file, in line number format.
--
-- /Note:/ Consider using 'filePosition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lFilePosition :: Lens.Lens' Location (Lude.Maybe Lude.Integer)
lFilePosition = Lens.lens (filePosition :: Location -> Lude.Maybe Lude.Integer) (\s a -> s {filePosition = a} :: Location)
{-# DEPRECATED lFilePosition "Use generic-lens or generic-optics with 'filePosition' instead." #-}

instance Lude.FromJSON Location where
  parseJSON =
    Lude.withObject
      "Location"
      ( \x ->
          Location'
            Lude.<$> (x Lude..:? "relativeFileVersion")
            Lude.<*> (x Lude..:? "filePath")
            Lude.<*> (x Lude..:? "filePosition")
      )

instance Lude.ToJSON Location where
  toJSON Location' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("relativeFileVersion" Lude..=) Lude.<$> relativeFileVersion,
            ("filePath" Lude..=) Lude.<$> filePath,
            ("filePosition" Lude..=) Lude.<$> filePosition
          ]
      )
