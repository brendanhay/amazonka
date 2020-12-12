{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.ConflictResolution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.ConflictResolution
  ( ConflictResolution (..),

    -- * Smart constructor
    mkConflictResolution,

    -- * Lenses
    crSetFileModes,
    crDeleteFiles,
    crReplaceContents,
  )
where

import Network.AWS.CodeCommit.Types.DeleteFileEntry
import Network.AWS.CodeCommit.Types.ReplaceContentEntry
import Network.AWS.CodeCommit.Types.SetFileModeEntry
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | If AUTOMERGE is the conflict resolution strategy, a list of inputs to use when resolving conflicts during a merge.
--
-- /See:/ 'mkConflictResolution' smart constructor.
data ConflictResolution = ConflictResolution'
  { setFileModes ::
      Lude.Maybe [SetFileModeEntry],
    deleteFiles :: Lude.Maybe [DeleteFileEntry],
    replaceContents :: Lude.Maybe [ReplaceContentEntry]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConflictResolution' with the minimum fields required to make a request.
--
-- * 'deleteFiles' - Files to be deleted as part of the merge conflict resolution.
-- * 'replaceContents' - Files to have content replaced as part of the merge conflict resolution.
-- * 'setFileModes' - File modes that are set as part of the merge conflict resolution.
mkConflictResolution ::
  ConflictResolution
mkConflictResolution =
  ConflictResolution'
    { setFileModes = Lude.Nothing,
      deleteFiles = Lude.Nothing,
      replaceContents = Lude.Nothing
    }

-- | File modes that are set as part of the merge conflict resolution.
--
-- /Note:/ Consider using 'setFileModes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crSetFileModes :: Lens.Lens' ConflictResolution (Lude.Maybe [SetFileModeEntry])
crSetFileModes = Lens.lens (setFileModes :: ConflictResolution -> Lude.Maybe [SetFileModeEntry]) (\s a -> s {setFileModes = a} :: ConflictResolution)
{-# DEPRECATED crSetFileModes "Use generic-lens or generic-optics with 'setFileModes' instead." #-}

-- | Files to be deleted as part of the merge conflict resolution.
--
-- /Note:/ Consider using 'deleteFiles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crDeleteFiles :: Lens.Lens' ConflictResolution (Lude.Maybe [DeleteFileEntry])
crDeleteFiles = Lens.lens (deleteFiles :: ConflictResolution -> Lude.Maybe [DeleteFileEntry]) (\s a -> s {deleteFiles = a} :: ConflictResolution)
{-# DEPRECATED crDeleteFiles "Use generic-lens or generic-optics with 'deleteFiles' instead." #-}

-- | Files to have content replaced as part of the merge conflict resolution.
--
-- /Note:/ Consider using 'replaceContents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crReplaceContents :: Lens.Lens' ConflictResolution (Lude.Maybe [ReplaceContentEntry])
crReplaceContents = Lens.lens (replaceContents :: ConflictResolution -> Lude.Maybe [ReplaceContentEntry]) (\s a -> s {replaceContents = a} :: ConflictResolution)
{-# DEPRECATED crReplaceContents "Use generic-lens or generic-optics with 'replaceContents' instead." #-}

instance Lude.ToJSON ConflictResolution where
  toJSON ConflictResolution' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("setFileModes" Lude..=) Lude.<$> setFileModes,
            ("deleteFiles" Lude..=) Lude.<$> deleteFiles,
            ("replaceContents" Lude..=) Lude.<$> replaceContents
          ]
      )
