{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.GetCommit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a commit, including commit message and committer information.
module Network.AWS.CodeCommit.GetCommit
  ( -- * Creating a request
    GetCommit (..),
    mkGetCommit,

    -- ** Request lenses
    gcRepositoryName,
    gcCommitId,

    -- * Destructuring the response
    GetCommitResponse (..),
    mkGetCommitResponse,

    -- ** Response lenses
    gcrrsCommit,
    gcrrsResponseStatus,
  )
where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a get commit operation.
--
-- /See:/ 'mkGetCommit' smart constructor.
data GetCommit = GetCommit'
  { -- | The name of the repository to which the commit was made.
    repositoryName :: Types.RepositoryName,
    -- | The commit ID. Commit IDs are the full SHA ID of the commit.
    commitId :: Types.ObjectId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetCommit' value with any optional fields omitted.
mkGetCommit ::
  -- | 'repositoryName'
  Types.RepositoryName ->
  -- | 'commitId'
  Types.ObjectId ->
  GetCommit
mkGetCommit repositoryName commitId =
  GetCommit' {repositoryName, commitId}

-- | The name of the repository to which the commit was made.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcRepositoryName :: Lens.Lens' GetCommit Types.RepositoryName
gcRepositoryName = Lens.field @"repositoryName"
{-# DEPRECATED gcRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The commit ID. Commit IDs are the full SHA ID of the commit.
--
-- /Note:/ Consider using 'commitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcCommitId :: Lens.Lens' GetCommit Types.ObjectId
gcCommitId = Lens.field @"commitId"
{-# DEPRECATED gcCommitId "Use generic-lens or generic-optics with 'commitId' instead." #-}

instance Core.FromJSON GetCommit where
  toJSON GetCommit {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("repositoryName" Core..= repositoryName),
            Core.Just ("commitId" Core..= commitId)
          ]
      )

instance Core.AWSRequest GetCommit where
  type Rs GetCommit = GetCommitResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "CodeCommit_20150413.GetCommit")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCommitResponse'
            Core.<$> (x Core..: "commit") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the output of a get commit operation.
--
-- /See:/ 'mkGetCommitResponse' smart constructor.
data GetCommitResponse = GetCommitResponse'
  { -- | A commit data type object that contains information about the specified commit.
    commit :: Types.Commit,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetCommitResponse' value with any optional fields omitted.
mkGetCommitResponse ::
  -- | 'commit'
  Types.Commit ->
  -- | 'responseStatus'
  Core.Int ->
  GetCommitResponse
mkGetCommitResponse commit responseStatus =
  GetCommitResponse' {commit, responseStatus}

-- | A commit data type object that contains information about the specified commit.
--
-- /Note:/ Consider using 'commit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrrsCommit :: Lens.Lens' GetCommitResponse Types.Commit
gcrrsCommit = Lens.field @"commit"
{-# DEPRECATED gcrrsCommit "Use generic-lens or generic-optics with 'commit' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrrsResponseStatus :: Lens.Lens' GetCommitResponse Core.Int
gcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
