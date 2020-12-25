{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.GetRepository
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a repository.
module Network.AWS.CodeCommit.GetRepository
  ( -- * Creating a request
    GetRepository (..),
    mkGetRepository,

    -- ** Request lenses
    grRepositoryName,

    -- * Destructuring the response
    GetRepositoryResponse (..),
    mkGetRepositoryResponse,

    -- ** Response lenses
    grrrsRepositoryMetadata,
    grrrsResponseStatus,
  )
where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a get repository operation.
--
-- /See:/ 'mkGetRepository' smart constructor.
newtype GetRepository = GetRepository'
  { -- | The name of the repository to get information about.
    repositoryName :: Types.RepositoryName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetRepository' value with any optional fields omitted.
mkGetRepository ::
  -- | 'repositoryName'
  Types.RepositoryName ->
  GetRepository
mkGetRepository repositoryName = GetRepository' {repositoryName}

-- | The name of the repository to get information about.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grRepositoryName :: Lens.Lens' GetRepository Types.RepositoryName
grRepositoryName = Lens.field @"repositoryName"
{-# DEPRECATED grRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

instance Core.FromJSON GetRepository where
  toJSON GetRepository {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("repositoryName" Core..= repositoryName)]
      )

instance Core.AWSRequest GetRepository where
  type Rs GetRepository = GetRepositoryResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "CodeCommit_20150413.GetRepository")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRepositoryResponse'
            Core.<$> (x Core..:? "repositoryMetadata")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the output of a get repository operation.
--
-- /See:/ 'mkGetRepositoryResponse' smart constructor.
data GetRepositoryResponse = GetRepositoryResponse'
  { -- | Information about the repository.
    repositoryMetadata :: Core.Maybe Types.RepositoryMetadata,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetRepositoryResponse' value with any optional fields omitted.
mkGetRepositoryResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetRepositoryResponse
mkGetRepositoryResponse responseStatus =
  GetRepositoryResponse'
    { repositoryMetadata = Core.Nothing,
      responseStatus
    }

-- | Information about the repository.
--
-- /Note:/ Consider using 'repositoryMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrrsRepositoryMetadata :: Lens.Lens' GetRepositoryResponse (Core.Maybe Types.RepositoryMetadata)
grrrsRepositoryMetadata = Lens.field @"repositoryMetadata"
{-# DEPRECATED grrrsRepositoryMetadata "Use generic-lens or generic-optics with 'repositoryMetadata' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrrsResponseStatus :: Lens.Lens' GetRepositoryResponse Core.Int
grrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED grrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
