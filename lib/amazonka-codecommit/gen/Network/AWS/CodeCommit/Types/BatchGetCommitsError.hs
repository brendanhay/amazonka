{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.BatchGetCommitsError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeCommit.Types.BatchGetCommitsError
  ( BatchGetCommitsError (..)
  -- * Smart constructor
  , mkBatchGetCommitsError
  -- * Lenses
  , bgceCommitId
  , bgceErrorCode
  , bgceErrorMessage
  ) where

import qualified Network.AWS.CodeCommit.Types.CommitId as Types
import qualified Network.AWS.CodeCommit.Types.ErrorCode as Types
import qualified Network.AWS.CodeCommit.Types.ErrorMessage as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Returns information about errors in a BatchGetCommits operation.
--
-- /See:/ 'mkBatchGetCommitsError' smart constructor.
data BatchGetCommitsError = BatchGetCommitsError'
  { commitId :: Core.Maybe Types.CommitId
    -- ^ A commit ID that either could not be found or was not in a valid format.
  , errorCode :: Core.Maybe Types.ErrorCode
    -- ^ An error code that specifies whether the commit ID was not valid or not found.
  , errorMessage :: Core.Maybe Types.ErrorMessage
    -- ^ An error message that provides detail about why the commit ID either was not found or was not valid.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchGetCommitsError' value with any optional fields omitted.
mkBatchGetCommitsError
    :: BatchGetCommitsError
mkBatchGetCommitsError
  = BatchGetCommitsError'{commitId = Core.Nothing,
                          errorCode = Core.Nothing, errorMessage = Core.Nothing}

-- | A commit ID that either could not be found or was not in a valid format.
--
-- /Note:/ Consider using 'commitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgceCommitId :: Lens.Lens' BatchGetCommitsError (Core.Maybe Types.CommitId)
bgceCommitId = Lens.field @"commitId"
{-# INLINEABLE bgceCommitId #-}
{-# DEPRECATED commitId "Use generic-lens or generic-optics with 'commitId' instead"  #-}

-- | An error code that specifies whether the commit ID was not valid or not found.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgceErrorCode :: Lens.Lens' BatchGetCommitsError (Core.Maybe Types.ErrorCode)
bgceErrorCode = Lens.field @"errorCode"
{-# INLINEABLE bgceErrorCode #-}
{-# DEPRECATED errorCode "Use generic-lens or generic-optics with 'errorCode' instead"  #-}

-- | An error message that provides detail about why the commit ID either was not found or was not valid.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgceErrorMessage :: Lens.Lens' BatchGetCommitsError (Core.Maybe Types.ErrorMessage)
bgceErrorMessage = Lens.field @"errorMessage"
{-# INLINEABLE bgceErrorMessage #-}
{-# DEPRECATED errorMessage "Use generic-lens or generic-optics with 'errorMessage' instead"  #-}

instance Core.FromJSON BatchGetCommitsError where
        parseJSON
          = Core.withObject "BatchGetCommitsError" Core.$
              \ x ->
                BatchGetCommitsError' Core.<$>
                  (x Core..:? "commitId") Core.<*> x Core..:? "errorCode" Core.<*>
                    x Core..:? "errorMessage"
