{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.CreateBranch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a branch in a repository and points the branch to a commit.
module Network.AWS.CodeCommit.CreateBranch
  ( -- * Creating a request
    CreateBranch (..),
    mkCreateBranch,

    -- ** Request lenses
    cbRepositoryName,
    cbBranchName,
    cbCommitId,

    -- * Destructuring the response
    CreateBranchResponse (..),
    mkCreateBranchResponse,
  )
where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a create branch operation.
--
-- /See:/ 'mkCreateBranch' smart constructor.
data CreateBranch = CreateBranch'
  { -- | The name of the repository in which you want to create the new branch.
    repositoryName :: Types.RepositoryName,
    -- | The name of the new branch to create.
    branchName :: Types.BranchName,
    -- | The ID of the commit to point the new branch to.
    commitId :: Types.CommitId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateBranch' value with any optional fields omitted.
mkCreateBranch ::
  -- | 'repositoryName'
  Types.RepositoryName ->
  -- | 'branchName'
  Types.BranchName ->
  -- | 'commitId'
  Types.CommitId ->
  CreateBranch
mkCreateBranch repositoryName branchName commitId =
  CreateBranch' {repositoryName, branchName, commitId}

-- | The name of the repository in which you want to create the new branch.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbRepositoryName :: Lens.Lens' CreateBranch Types.RepositoryName
cbRepositoryName = Lens.field @"repositoryName"
{-# DEPRECATED cbRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The name of the new branch to create.
--
-- /Note:/ Consider using 'branchName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbBranchName :: Lens.Lens' CreateBranch Types.BranchName
cbBranchName = Lens.field @"branchName"
{-# DEPRECATED cbBranchName "Use generic-lens or generic-optics with 'branchName' instead." #-}

-- | The ID of the commit to point the new branch to.
--
-- /Note:/ Consider using 'commitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbCommitId :: Lens.Lens' CreateBranch Types.CommitId
cbCommitId = Lens.field @"commitId"
{-# DEPRECATED cbCommitId "Use generic-lens or generic-optics with 'commitId' instead." #-}

instance Core.FromJSON CreateBranch where
  toJSON CreateBranch {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("repositoryName" Core..= repositoryName),
            Core.Just ("branchName" Core..= branchName),
            Core.Just ("commitId" Core..= commitId)
          ]
      )

instance Core.AWSRequest CreateBranch where
  type Rs CreateBranch = CreateBranchResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "CodeCommit_20150413.CreateBranch")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull CreateBranchResponse'

-- | /See:/ 'mkCreateBranchResponse' smart constructor.
data CreateBranchResponse = CreateBranchResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateBranchResponse' value with any optional fields omitted.
mkCreateBranchResponse ::
  CreateBranchResponse
mkCreateBranchResponse = CreateBranchResponse'
