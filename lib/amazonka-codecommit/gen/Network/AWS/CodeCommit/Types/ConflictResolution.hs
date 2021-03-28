{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.ConflictResolution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeCommit.Types.ConflictResolution
  ( ConflictResolution (..)
  -- * Smart constructor
  , mkConflictResolution
  -- * Lenses
  , crDeleteFiles
  , crReplaceContents
  , crSetFileModes
  ) where

import qualified Network.AWS.CodeCommit.Types.DeleteFileEntry as Types
import qualified Network.AWS.CodeCommit.Types.ReplaceContentEntry as Types
import qualified Network.AWS.CodeCommit.Types.SetFileModeEntry as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | If AUTOMERGE is the conflict resolution strategy, a list of inputs to use when resolving conflicts during a merge.
--
-- /See:/ 'mkConflictResolution' smart constructor.
data ConflictResolution = ConflictResolution'
  { deleteFiles :: Core.Maybe [Types.DeleteFileEntry]
    -- ^ Files to be deleted as part of the merge conflict resolution.
  , replaceContents :: Core.Maybe [Types.ReplaceContentEntry]
    -- ^ Files to have content replaced as part of the merge conflict resolution.
  , setFileModes :: Core.Maybe [Types.SetFileModeEntry]
    -- ^ File modes that are set as part of the merge conflict resolution.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ConflictResolution' value with any optional fields omitted.
mkConflictResolution
    :: ConflictResolution
mkConflictResolution
  = ConflictResolution'{deleteFiles = Core.Nothing,
                        replaceContents = Core.Nothing, setFileModes = Core.Nothing}

-- | Files to be deleted as part of the merge conflict resolution.
--
-- /Note:/ Consider using 'deleteFiles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crDeleteFiles :: Lens.Lens' ConflictResolution (Core.Maybe [Types.DeleteFileEntry])
crDeleteFiles = Lens.field @"deleteFiles"
{-# INLINEABLE crDeleteFiles #-}
{-# DEPRECATED deleteFiles "Use generic-lens or generic-optics with 'deleteFiles' instead"  #-}

-- | Files to have content replaced as part of the merge conflict resolution.
--
-- /Note:/ Consider using 'replaceContents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crReplaceContents :: Lens.Lens' ConflictResolution (Core.Maybe [Types.ReplaceContentEntry])
crReplaceContents = Lens.field @"replaceContents"
{-# INLINEABLE crReplaceContents #-}
{-# DEPRECATED replaceContents "Use generic-lens or generic-optics with 'replaceContents' instead"  #-}

-- | File modes that are set as part of the merge conflict resolution.
--
-- /Note:/ Consider using 'setFileModes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crSetFileModes :: Lens.Lens' ConflictResolution (Core.Maybe [Types.SetFileModeEntry])
crSetFileModes = Lens.field @"setFileModes"
{-# INLINEABLE crSetFileModes #-}
{-# DEPRECATED setFileModes "Use generic-lens or generic-optics with 'setFileModes' instead"  #-}

instance Core.FromJSON ConflictResolution where
        toJSON ConflictResolution{..}
          = Core.object
              (Core.catMaybes
                 [("deleteFiles" Core..=) Core.<$> deleteFiles,
                  ("replaceContents" Core..=) Core.<$> replaceContents,
                  ("setFileModes" Core..=) Core.<$> setFileModes])
