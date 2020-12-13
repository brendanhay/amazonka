{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.BatchDescribeMergeConflictsError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.BatchDescribeMergeConflictsError
  ( BatchDescribeMergeConflictsError (..),

    -- * Smart constructor
    mkBatchDescribeMergeConflictsError,

    -- * Lenses
    bdmceFilePath,
    bdmceExceptionName,
    bdmceMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Returns information about errors in a BatchDescribeMergeConflicts operation.
--
-- /See:/ 'mkBatchDescribeMergeConflictsError' smart constructor.
data BatchDescribeMergeConflictsError = BatchDescribeMergeConflictsError'
  { -- | The path to the file.
    filePath :: Lude.Text,
    -- | The name of the exception.
    exceptionName :: Lude.Text,
    -- | The message provided by the exception.
    message :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchDescribeMergeConflictsError' with the minimum fields required to make a request.
--
-- * 'filePath' - The path to the file.
-- * 'exceptionName' - The name of the exception.
-- * 'message' - The message provided by the exception.
mkBatchDescribeMergeConflictsError ::
  -- | 'filePath'
  Lude.Text ->
  -- | 'exceptionName'
  Lude.Text ->
  -- | 'message'
  Lude.Text ->
  BatchDescribeMergeConflictsError
mkBatchDescribeMergeConflictsError
  pFilePath_
  pExceptionName_
  pMessage_ =
    BatchDescribeMergeConflictsError'
      { filePath = pFilePath_,
        exceptionName = pExceptionName_,
        message = pMessage_
      }

-- | The path to the file.
--
-- /Note:/ Consider using 'filePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdmceFilePath :: Lens.Lens' BatchDescribeMergeConflictsError Lude.Text
bdmceFilePath = Lens.lens (filePath :: BatchDescribeMergeConflictsError -> Lude.Text) (\s a -> s {filePath = a} :: BatchDescribeMergeConflictsError)
{-# DEPRECATED bdmceFilePath "Use generic-lens or generic-optics with 'filePath' instead." #-}

-- | The name of the exception.
--
-- /Note:/ Consider using 'exceptionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdmceExceptionName :: Lens.Lens' BatchDescribeMergeConflictsError Lude.Text
bdmceExceptionName = Lens.lens (exceptionName :: BatchDescribeMergeConflictsError -> Lude.Text) (\s a -> s {exceptionName = a} :: BatchDescribeMergeConflictsError)
{-# DEPRECATED bdmceExceptionName "Use generic-lens or generic-optics with 'exceptionName' instead." #-}

-- | The message provided by the exception.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdmceMessage :: Lens.Lens' BatchDescribeMergeConflictsError Lude.Text
bdmceMessage = Lens.lens (message :: BatchDescribeMergeConflictsError -> Lude.Text) (\s a -> s {message = a} :: BatchDescribeMergeConflictsError)
{-# DEPRECATED bdmceMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromJSON BatchDescribeMergeConflictsError where
  parseJSON =
    Lude.withObject
      "BatchDescribeMergeConflictsError"
      ( \x ->
          BatchDescribeMergeConflictsError'
            Lude.<$> (x Lude..: "filePath")
            Lude.<*> (x Lude..: "exceptionName")
            Lude.<*> (x Lude..: "message")
      )
