{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.CommentsForComparedCommit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeCommit.Types.CommentsForComparedCommit
  ( CommentsForComparedCommit (..)
  -- * Smart constructor
  , mkCommentsForComparedCommit
  -- * Lenses
  , cfccAfterBlobId
  , cfccAfterCommitId
  , cfccBeforeBlobId
  , cfccBeforeCommitId
  , cfccComments
  , cfccLocation
  , cfccRepositoryName
  ) where

import qualified Network.AWS.CodeCommit.Types.Comment as Types
import qualified Network.AWS.CodeCommit.Types.CommitId as Types
import qualified Network.AWS.CodeCommit.Types.Location as Types
import qualified Network.AWS.CodeCommit.Types.ObjectId as Types
import qualified Network.AWS.CodeCommit.Types.RepositoryName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Returns information about comments on the comparison between two commits.
--
-- /See:/ 'mkCommentsForComparedCommit' smart constructor.
data CommentsForComparedCommit = CommentsForComparedCommit'
  { afterBlobId :: Core.Maybe Types.ObjectId
    -- ^ The full blob ID of the commit used to establish the after of the comparison.
  , afterCommitId :: Core.Maybe Types.CommitId
    -- ^ The full commit ID of the commit used to establish the after of the comparison.
  , beforeBlobId :: Core.Maybe Types.ObjectId
    -- ^ The full blob ID of the commit used to establish the before of the comparison.
  , beforeCommitId :: Core.Maybe Types.CommitId
    -- ^ The full commit ID of the commit used to establish the before of the comparison.
  , comments :: Core.Maybe [Types.Comment]
    -- ^ An array of comment objects. Each comment object contains information about a comment on the comparison between commits.
  , location :: Core.Maybe Types.Location
    -- ^ Location information about the comment on the comparison, including the file name, line number, and whether the version of the file where the comment was made is BEFORE or AFTER.
  , repositoryName :: Core.Maybe Types.RepositoryName
    -- ^ The name of the repository that contains the compared commits.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CommentsForComparedCommit' value with any optional fields omitted.
mkCommentsForComparedCommit
    :: CommentsForComparedCommit
mkCommentsForComparedCommit
  = CommentsForComparedCommit'{afterBlobId = Core.Nothing,
                               afterCommitId = Core.Nothing, beforeBlobId = Core.Nothing,
                               beforeCommitId = Core.Nothing, comments = Core.Nothing,
                               location = Core.Nothing, repositoryName = Core.Nothing}

-- | The full blob ID of the commit used to establish the after of the comparison.
--
-- /Note:/ Consider using 'afterBlobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfccAfterBlobId :: Lens.Lens' CommentsForComparedCommit (Core.Maybe Types.ObjectId)
cfccAfterBlobId = Lens.field @"afterBlobId"
{-# INLINEABLE cfccAfterBlobId #-}
{-# DEPRECATED afterBlobId "Use generic-lens or generic-optics with 'afterBlobId' instead"  #-}

-- | The full commit ID of the commit used to establish the after of the comparison.
--
-- /Note:/ Consider using 'afterCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfccAfterCommitId :: Lens.Lens' CommentsForComparedCommit (Core.Maybe Types.CommitId)
cfccAfterCommitId = Lens.field @"afterCommitId"
{-# INLINEABLE cfccAfterCommitId #-}
{-# DEPRECATED afterCommitId "Use generic-lens or generic-optics with 'afterCommitId' instead"  #-}

-- | The full blob ID of the commit used to establish the before of the comparison.
--
-- /Note:/ Consider using 'beforeBlobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfccBeforeBlobId :: Lens.Lens' CommentsForComparedCommit (Core.Maybe Types.ObjectId)
cfccBeforeBlobId = Lens.field @"beforeBlobId"
{-# INLINEABLE cfccBeforeBlobId #-}
{-# DEPRECATED beforeBlobId "Use generic-lens or generic-optics with 'beforeBlobId' instead"  #-}

-- | The full commit ID of the commit used to establish the before of the comparison.
--
-- /Note:/ Consider using 'beforeCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfccBeforeCommitId :: Lens.Lens' CommentsForComparedCommit (Core.Maybe Types.CommitId)
cfccBeforeCommitId = Lens.field @"beforeCommitId"
{-# INLINEABLE cfccBeforeCommitId #-}
{-# DEPRECATED beforeCommitId "Use generic-lens or generic-optics with 'beforeCommitId' instead"  #-}

-- | An array of comment objects. Each comment object contains information about a comment on the comparison between commits.
--
-- /Note:/ Consider using 'comments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfccComments :: Lens.Lens' CommentsForComparedCommit (Core.Maybe [Types.Comment])
cfccComments = Lens.field @"comments"
{-# INLINEABLE cfccComments #-}
{-# DEPRECATED comments "Use generic-lens or generic-optics with 'comments' instead"  #-}

-- | Location information about the comment on the comparison, including the file name, line number, and whether the version of the file where the comment was made is BEFORE or AFTER.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfccLocation :: Lens.Lens' CommentsForComparedCommit (Core.Maybe Types.Location)
cfccLocation = Lens.field @"location"
{-# INLINEABLE cfccLocation #-}
{-# DEPRECATED location "Use generic-lens or generic-optics with 'location' instead"  #-}

-- | The name of the repository that contains the compared commits.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfccRepositoryName :: Lens.Lens' CommentsForComparedCommit (Core.Maybe Types.RepositoryName)
cfccRepositoryName = Lens.field @"repositoryName"
{-# INLINEABLE cfccRepositoryName #-}
{-# DEPRECATED repositoryName "Use generic-lens or generic-optics with 'repositoryName' instead"  #-}

instance Core.FromJSON CommentsForComparedCommit where
        parseJSON
          = Core.withObject "CommentsForComparedCommit" Core.$
              \ x ->
                CommentsForComparedCommit' Core.<$>
                  (x Core..:? "afterBlobId") Core.<*> x Core..:? "afterCommitId"
                    Core.<*> x Core..:? "beforeBlobId"
                    Core.<*> x Core..:? "beforeCommitId"
                    Core.<*> x Core..:? "comments"
                    Core.<*> x Core..:? "location"
                    Core.<*> x Core..:? "repositoryName"
