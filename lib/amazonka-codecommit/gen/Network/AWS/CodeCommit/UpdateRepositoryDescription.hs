{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.UpdateRepositoryDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets or changes the comment or description for a repository.
module Network.AWS.CodeCommit.UpdateRepositoryDescription
  ( -- * Creating a request
    UpdateRepositoryDescription (..),
    mkUpdateRepositoryDescription,

    -- ** Request lenses
    urdRepositoryName,
    urdRepositoryDescription,

    -- * Destructuring the response
    UpdateRepositoryDescriptionResponse (..),
    mkUpdateRepositoryDescriptionResponse,
  )
where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of an update repository description operation.
--
-- /See:/ 'mkUpdateRepositoryDescription' smart constructor.
data UpdateRepositoryDescription = UpdateRepositoryDescription'
  { -- | The name of the repository to set or change the comment or description for.
    repositoryName :: Types.RepositoryName,
    -- | The new comment or description for the specified repository. Repository descriptions are limited to 1,000 characters.
    repositoryDescription :: Core.Maybe Types.RepositoryDescription
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateRepositoryDescription' value with any optional fields omitted.
mkUpdateRepositoryDescription ::
  -- | 'repositoryName'
  Types.RepositoryName ->
  UpdateRepositoryDescription
mkUpdateRepositoryDescription repositoryName =
  UpdateRepositoryDescription'
    { repositoryName,
      repositoryDescription = Core.Nothing
    }

-- | The name of the repository to set or change the comment or description for.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urdRepositoryName :: Lens.Lens' UpdateRepositoryDescription Types.RepositoryName
urdRepositoryName = Lens.field @"repositoryName"
{-# DEPRECATED urdRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The new comment or description for the specified repository. Repository descriptions are limited to 1,000 characters.
--
-- /Note:/ Consider using 'repositoryDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urdRepositoryDescription :: Lens.Lens' UpdateRepositoryDescription (Core.Maybe Types.RepositoryDescription)
urdRepositoryDescription = Lens.field @"repositoryDescription"
{-# DEPRECATED urdRepositoryDescription "Use generic-lens or generic-optics with 'repositoryDescription' instead." #-}

instance Core.FromJSON UpdateRepositoryDescription where
  toJSON UpdateRepositoryDescription {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("repositoryName" Core..= repositoryName),
            ("repositoryDescription" Core..=) Core.<$> repositoryDescription
          ]
      )

instance Core.AWSRequest UpdateRepositoryDescription where
  type
    Rs UpdateRepositoryDescription =
      UpdateRepositoryDescriptionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "CodeCommit_20150413.UpdateRepositoryDescription")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveNull UpdateRepositoryDescriptionResponse'

-- | /See:/ 'mkUpdateRepositoryDescriptionResponse' smart constructor.
data UpdateRepositoryDescriptionResponse = UpdateRepositoryDescriptionResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateRepositoryDescriptionResponse' value with any optional fields omitted.
mkUpdateRepositoryDescriptionResponse ::
  UpdateRepositoryDescriptionResponse
mkUpdateRepositoryDescriptionResponse =
  UpdateRepositoryDescriptionResponse'
