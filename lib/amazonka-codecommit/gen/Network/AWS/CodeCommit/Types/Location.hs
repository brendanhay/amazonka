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
    lFilePath,
    lFilePosition,
    lRelativeFileVersion,
  )
where

import qualified Network.AWS.CodeCommit.Types.FilePath as Types
import qualified Network.AWS.CodeCommit.Types.RelativeFileVersionEnum as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Returns information about the location of a change or comment in the comparison between two commits or a pull request.
--
-- /See:/ 'mkLocation' smart constructor.
data Location = Location'
  { -- | The name of the file being compared, including its extension and subdirectory, if any.
    filePath :: Core.Maybe Types.FilePath,
    -- | The position of a change in a compared file, in line number format.
    filePosition :: Core.Maybe Core.Integer,
    -- | In a comparison of commits or a pull request, whether the change is in the before or after of that comparison.
    relativeFileVersion :: Core.Maybe Types.RelativeFileVersionEnum
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Location' value with any optional fields omitted.
mkLocation ::
  Location
mkLocation =
  Location'
    { filePath = Core.Nothing,
      filePosition = Core.Nothing,
      relativeFileVersion = Core.Nothing
    }

-- | The name of the file being compared, including its extension and subdirectory, if any.
--
-- /Note:/ Consider using 'filePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lFilePath :: Lens.Lens' Location (Core.Maybe Types.FilePath)
lFilePath = Lens.field @"filePath"
{-# DEPRECATED lFilePath "Use generic-lens or generic-optics with 'filePath' instead." #-}

-- | The position of a change in a compared file, in line number format.
--
-- /Note:/ Consider using 'filePosition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lFilePosition :: Lens.Lens' Location (Core.Maybe Core.Integer)
lFilePosition = Lens.field @"filePosition"
{-# DEPRECATED lFilePosition "Use generic-lens or generic-optics with 'filePosition' instead." #-}

-- | In a comparison of commits or a pull request, whether the change is in the before or after of that comparison.
--
-- /Note:/ Consider using 'relativeFileVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lRelativeFileVersion :: Lens.Lens' Location (Core.Maybe Types.RelativeFileVersionEnum)
lRelativeFileVersion = Lens.field @"relativeFileVersion"
{-# DEPRECATED lRelativeFileVersion "Use generic-lens or generic-optics with 'relativeFileVersion' instead." #-}

instance Core.FromJSON Location where
  toJSON Location {..} =
    Core.object
      ( Core.catMaybes
          [ ("filePath" Core..=) Core.<$> filePath,
            ("filePosition" Core..=) Core.<$> filePosition,
            ("relativeFileVersion" Core..=) Core.<$> relativeFileVersion
          ]
      )

instance Core.FromJSON Location where
  parseJSON =
    Core.withObject "Location" Core.$
      \x ->
        Location'
          Core.<$> (x Core..:? "filePath")
          Core.<*> (x Core..:? "filePosition")
          Core.<*> (x Core..:? "relativeFileVersion")
