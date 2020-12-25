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

import qualified Network.AWS.CodeCommit.Types.ExceptionName as Types
import qualified Network.AWS.CodeCommit.Types.Message as Types
import qualified Network.AWS.CodeCommit.Types.Path as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Returns information about errors in a BatchDescribeMergeConflicts operation.
--
-- /See:/ 'mkBatchDescribeMergeConflictsError' smart constructor.
data BatchDescribeMergeConflictsError = BatchDescribeMergeConflictsError'
  { -- | The path to the file.
    filePath :: Types.Path,
    -- | The name of the exception.
    exceptionName :: Types.ExceptionName,
    -- | The message provided by the exception.
    message :: Types.Message
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchDescribeMergeConflictsError' value with any optional fields omitted.
mkBatchDescribeMergeConflictsError ::
  -- | 'filePath'
  Types.Path ->
  -- | 'exceptionName'
  Types.ExceptionName ->
  -- | 'message'
  Types.Message ->
  BatchDescribeMergeConflictsError
mkBatchDescribeMergeConflictsError filePath exceptionName message =
  BatchDescribeMergeConflictsError'
    { filePath,
      exceptionName,
      message
    }

-- | The path to the file.
--
-- /Note:/ Consider using 'filePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdmceFilePath :: Lens.Lens' BatchDescribeMergeConflictsError Types.Path
bdmceFilePath = Lens.field @"filePath"
{-# DEPRECATED bdmceFilePath "Use generic-lens or generic-optics with 'filePath' instead." #-}

-- | The name of the exception.
--
-- /Note:/ Consider using 'exceptionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdmceExceptionName :: Lens.Lens' BatchDescribeMergeConflictsError Types.ExceptionName
bdmceExceptionName = Lens.field @"exceptionName"
{-# DEPRECATED bdmceExceptionName "Use generic-lens or generic-optics with 'exceptionName' instead." #-}

-- | The message provided by the exception.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdmceMessage :: Lens.Lens' BatchDescribeMergeConflictsError Types.Message
bdmceMessage = Lens.field @"message"
{-# DEPRECATED bdmceMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Core.FromJSON BatchDescribeMergeConflictsError where
  parseJSON =
    Core.withObject "BatchDescribeMergeConflictsError" Core.$
      \x ->
        BatchDescribeMergeConflictsError'
          Core.<$> (x Core..: "filePath")
          Core.<*> (x Core..: "exceptionName")
          Core.<*> (x Core..: "message")
