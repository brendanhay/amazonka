{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.UpdateRepositoryName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Renames a repository. The repository name must be unique across the calling AWS account. Repository names are limited to 100 alphanumeric, dash, and underscore characters, and cannot include certain characters. The suffix .git is prohibited. For more information about the limits on repository names, see <https://docs.aws.amazon.com/codecommit/latest/userguide/limits.html Limits> in the AWS CodeCommit User Guide.
module Network.AWS.CodeCommit.UpdateRepositoryName
  ( -- * Creating a request
    UpdateRepositoryName (..),
    mkUpdateRepositoryName,

    -- ** Request lenses
    urnOldName,
    urnNewName,

    -- * Destructuring the response
    UpdateRepositoryNameResponse (..),
    mkUpdateRepositoryNameResponse,
  )
where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of an update repository description operation.
--
-- /See:/ 'mkUpdateRepositoryName' smart constructor.
data UpdateRepositoryName = UpdateRepositoryName'
  { -- | The current name of the repository.
    oldName :: Types.OldName,
    -- | The new name for the repository.
    newName :: Types.NewName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateRepositoryName' value with any optional fields omitted.
mkUpdateRepositoryName ::
  -- | 'oldName'
  Types.OldName ->
  -- | 'newName'
  Types.NewName ->
  UpdateRepositoryName
mkUpdateRepositoryName oldName newName =
  UpdateRepositoryName' {oldName, newName}

-- | The current name of the repository.
--
-- /Note:/ Consider using 'oldName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urnOldName :: Lens.Lens' UpdateRepositoryName Types.OldName
urnOldName = Lens.field @"oldName"
{-# DEPRECATED urnOldName "Use generic-lens or generic-optics with 'oldName' instead." #-}

-- | The new name for the repository.
--
-- /Note:/ Consider using 'newName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urnNewName :: Lens.Lens' UpdateRepositoryName Types.NewName
urnNewName = Lens.field @"newName"
{-# DEPRECATED urnNewName "Use generic-lens or generic-optics with 'newName' instead." #-}

instance Core.FromJSON UpdateRepositoryName where
  toJSON UpdateRepositoryName {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("oldName" Core..= oldName),
            Core.Just ("newName" Core..= newName)
          ]
      )

instance Core.AWSRequest UpdateRepositoryName where
  type Rs UpdateRepositoryName = UpdateRepositoryNameResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "CodeCommit_20150413.UpdateRepositoryName")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull UpdateRepositoryNameResponse'

-- | /See:/ 'mkUpdateRepositoryNameResponse' smart constructor.
data UpdateRepositoryNameResponse = UpdateRepositoryNameResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateRepositoryNameResponse' value with any optional fields omitted.
mkUpdateRepositoryNameResponse ::
  UpdateRepositoryNameResponse
mkUpdateRepositoryNameResponse = UpdateRepositoryNameResponse'
