{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.CreateRepository
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new, empty repository.
module Network.AWS.CodeCommit.CreateRepository
  ( -- * Creating a request
    CreateRepository (..),
    mkCreateRepository,

    -- ** Request lenses
    crRepositoryName,
    crRepositoryDescription,
    crTags,

    -- * Destructuring the response
    CreateRepositoryResponse (..),
    mkCreateRepositoryResponse,

    -- ** Response lenses
    crrrsRepositoryMetadata,
    crrrsResponseStatus,
  )
where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a create repository operation.
--
-- /See:/ 'mkCreateRepository' smart constructor.
data CreateRepository = CreateRepository'
  { -- | The name of the new repository to be created.
    repositoryName :: Types.RepositoryName,
    -- | A comment or description about the new repository.
    repositoryDescription :: Core.Maybe Types.RepositoryDescription,
    -- | One or more tag key-value pairs to use when tagging this repository.
    tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateRepository' value with any optional fields omitted.
mkCreateRepository ::
  -- | 'repositoryName'
  Types.RepositoryName ->
  CreateRepository
mkCreateRepository repositoryName =
  CreateRepository'
    { repositoryName,
      repositoryDescription = Core.Nothing,
      tags = Core.Nothing
    }

-- | The name of the new repository to be created.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crRepositoryName :: Lens.Lens' CreateRepository Types.RepositoryName
crRepositoryName = Lens.field @"repositoryName"
{-# DEPRECATED crRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | A comment or description about the new repository.
--
-- /Note:/ Consider using 'repositoryDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crRepositoryDescription :: Lens.Lens' CreateRepository (Core.Maybe Types.RepositoryDescription)
crRepositoryDescription = Lens.field @"repositoryDescription"
{-# DEPRECATED crRepositoryDescription "Use generic-lens or generic-optics with 'repositoryDescription' instead." #-}

-- | One or more tag key-value pairs to use when tagging this repository.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crTags :: Lens.Lens' CreateRepository (Core.Maybe (Core.HashMap Types.TagKey Types.TagValue))
crTags = Lens.field @"tags"
{-# DEPRECATED crTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreateRepository where
  toJSON CreateRepository {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("repositoryName" Core..= repositoryName),
            ("repositoryDescription" Core..=) Core.<$> repositoryDescription,
            ("tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreateRepository where
  type Rs CreateRepository = CreateRepositoryResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "CodeCommit_20150413.CreateRepository")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateRepositoryResponse'
            Core.<$> (x Core..:? "repositoryMetadata")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the output of a create repository operation.
--
-- /See:/ 'mkCreateRepositoryResponse' smart constructor.
data CreateRepositoryResponse = CreateRepositoryResponse'
  { -- | Information about the newly created repository.
    repositoryMetadata :: Core.Maybe Types.RepositoryMetadata,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateRepositoryResponse' value with any optional fields omitted.
mkCreateRepositoryResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateRepositoryResponse
mkCreateRepositoryResponse responseStatus =
  CreateRepositoryResponse'
    { repositoryMetadata = Core.Nothing,
      responseStatus
    }

-- | Information about the newly created repository.
--
-- /Note:/ Consider using 'repositoryMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrrsRepositoryMetadata :: Lens.Lens' CreateRepositoryResponse (Core.Maybe Types.RepositoryMetadata)
crrrsRepositoryMetadata = Lens.field @"repositoryMetadata"
{-# DEPRECATED crrrsRepositoryMetadata "Use generic-lens or generic-optics with 'repositoryMetadata' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrrsResponseStatus :: Lens.Lens' CreateRepositoryResponse Core.Int
crrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED crrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
