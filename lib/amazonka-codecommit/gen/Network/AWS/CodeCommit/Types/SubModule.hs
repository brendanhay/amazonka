{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.SubModule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.SubModule
  ( SubModule (..),

    -- * Smart constructor
    mkSubModule,

    -- * Lenses
    smAbsolutePath,
    smCommitId,
    smRelativePath,
  )
where

import qualified Network.AWS.CodeCommit.Types.ObjectId as Types
import qualified Network.AWS.CodeCommit.Types.Path as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Returns information about a submodule reference in a repository folder.
--
-- /See:/ 'mkSubModule' smart constructor.
data SubModule = SubModule'
  { -- | The fully qualified path to the folder that contains the reference to the submodule.
    absolutePath :: Core.Maybe Types.Path,
    -- | The commit ID that contains the reference to the submodule.
    commitId :: Core.Maybe Types.ObjectId,
    -- | The relative path of the submodule from the folder where the query originated.
    relativePath :: Core.Maybe Types.Path
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SubModule' value with any optional fields omitted.
mkSubModule ::
  SubModule
mkSubModule =
  SubModule'
    { absolutePath = Core.Nothing,
      commitId = Core.Nothing,
      relativePath = Core.Nothing
    }

-- | The fully qualified path to the folder that contains the reference to the submodule.
--
-- /Note:/ Consider using 'absolutePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smAbsolutePath :: Lens.Lens' SubModule (Core.Maybe Types.Path)
smAbsolutePath = Lens.field @"absolutePath"
{-# DEPRECATED smAbsolutePath "Use generic-lens or generic-optics with 'absolutePath' instead." #-}

-- | The commit ID that contains the reference to the submodule.
--
-- /Note:/ Consider using 'commitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smCommitId :: Lens.Lens' SubModule (Core.Maybe Types.ObjectId)
smCommitId = Lens.field @"commitId"
{-# DEPRECATED smCommitId "Use generic-lens or generic-optics with 'commitId' instead." #-}

-- | The relative path of the submodule from the folder where the query originated.
--
-- /Note:/ Consider using 'relativePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smRelativePath :: Lens.Lens' SubModule (Core.Maybe Types.Path)
smRelativePath = Lens.field @"relativePath"
{-# DEPRECATED smRelativePath "Use generic-lens or generic-optics with 'relativePath' instead." #-}

instance Core.FromJSON SubModule where
  parseJSON =
    Core.withObject "SubModule" Core.$
      \x ->
        SubModule'
          Core.<$> (x Core..:? "absolutePath")
          Core.<*> (x Core..:? "commitId")
          Core.<*> (x Core..:? "relativePath")
