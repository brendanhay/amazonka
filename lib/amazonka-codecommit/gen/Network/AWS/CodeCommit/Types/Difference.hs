{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.Difference
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.Difference
  ( Difference (..),

    -- * Smart constructor
    mkDifference,

    -- * Lenses
    dAfterBlob,
    dBeforeBlob,
    dChangeType,
  )
where

import qualified Network.AWS.CodeCommit.Types.BlobMetadata as Types
import qualified Network.AWS.CodeCommit.Types.ChangeTypeEnum as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Returns information about a set of differences for a commit specifier.
--
-- /See:/ 'mkDifference' smart constructor.
data Difference = Difference'
  { -- | Information about an @afterBlob@ data type object, including the ID, the file mode permission code, and the path.
    afterBlob :: Core.Maybe Types.BlobMetadata,
    -- | Information about a @beforeBlob@ data type object, including the ID, the file mode permission code, and the path.
    beforeBlob :: Core.Maybe Types.BlobMetadata,
    -- | Whether the change type of the difference is an addition (A), deletion (D), or modification (M).
    changeType :: Core.Maybe Types.ChangeTypeEnum
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Difference' value with any optional fields omitted.
mkDifference ::
  Difference
mkDifference =
  Difference'
    { afterBlob = Core.Nothing,
      beforeBlob = Core.Nothing,
      changeType = Core.Nothing
    }

-- | Information about an @afterBlob@ data type object, including the ID, the file mode permission code, and the path.
--
-- /Note:/ Consider using 'afterBlob' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dAfterBlob :: Lens.Lens' Difference (Core.Maybe Types.BlobMetadata)
dAfterBlob = Lens.field @"afterBlob"
{-# DEPRECATED dAfterBlob "Use generic-lens or generic-optics with 'afterBlob' instead." #-}

-- | Information about a @beforeBlob@ data type object, including the ID, the file mode permission code, and the path.
--
-- /Note:/ Consider using 'beforeBlob' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dBeforeBlob :: Lens.Lens' Difference (Core.Maybe Types.BlobMetadata)
dBeforeBlob = Lens.field @"beforeBlob"
{-# DEPRECATED dBeforeBlob "Use generic-lens or generic-optics with 'beforeBlob' instead." #-}

-- | Whether the change type of the difference is an addition (A), deletion (D), or modification (M).
--
-- /Note:/ Consider using 'changeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dChangeType :: Lens.Lens' Difference (Core.Maybe Types.ChangeTypeEnum)
dChangeType = Lens.field @"changeType"
{-# DEPRECATED dChangeType "Use generic-lens or generic-optics with 'changeType' instead." #-}

instance Core.FromJSON Difference where
  parseJSON =
    Core.withObject "Difference" Core.$
      \x ->
        Difference'
          Core.<$> (x Core..:? "afterBlob")
          Core.<*> (x Core..:? "beforeBlob")
          Core.<*> (x Core..:? "changeType")
