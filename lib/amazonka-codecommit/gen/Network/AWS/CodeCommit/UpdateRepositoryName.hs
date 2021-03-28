{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      UpdateRepositoryName (..)
    , mkUpdateRepositoryName
    -- ** Request lenses
    , urnOldName
    , urnNewName

    -- * Destructuring the response
    , UpdateRepositoryNameResponse (..)
    , mkUpdateRepositoryNameResponse
    ) where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of an update repository description operation.
--
-- /See:/ 'mkUpdateRepositoryName' smart constructor.
data UpdateRepositoryName = UpdateRepositoryName'
  { oldName :: Types.OldName
    -- ^ The current name of the repository.
  , newName :: Types.NewName
    -- ^ The new name for the repository.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateRepositoryName' value with any optional fields omitted.
mkUpdateRepositoryName
    :: Types.OldName -- ^ 'oldName'
    -> Types.NewName -- ^ 'newName'
    -> UpdateRepositoryName
mkUpdateRepositoryName oldName newName
  = UpdateRepositoryName'{oldName, newName}

-- | The current name of the repository.
--
-- /Note:/ Consider using 'oldName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urnOldName :: Lens.Lens' UpdateRepositoryName Types.OldName
urnOldName = Lens.field @"oldName"
{-# INLINEABLE urnOldName #-}
{-# DEPRECATED oldName "Use generic-lens or generic-optics with 'oldName' instead"  #-}

-- | The new name for the repository.
--
-- /Note:/ Consider using 'newName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urnNewName :: Lens.Lens' UpdateRepositoryName Types.NewName
urnNewName = Lens.field @"newName"
{-# INLINEABLE urnNewName #-}
{-# DEPRECATED newName "Use generic-lens or generic-optics with 'newName' instead"  #-}

instance Core.ToQuery UpdateRepositoryName where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateRepositoryName where
        toHeaders UpdateRepositoryName{..}
          = Core.pure
              ("X-Amz-Target", "CodeCommit_20150413.UpdateRepositoryName")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateRepositoryName where
        toJSON UpdateRepositoryName{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("oldName" Core..= oldName),
                  Core.Just ("newName" Core..= newName)])

instance Core.AWSRequest UpdateRepositoryName where
        type Rs UpdateRepositoryName = UpdateRepositoryNameResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull UpdateRepositoryNameResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateRepositoryNameResponse' smart constructor.
data UpdateRepositoryNameResponse = UpdateRepositoryNameResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateRepositoryNameResponse' value with any optional fields omitted.
mkUpdateRepositoryNameResponse
    :: UpdateRepositoryNameResponse
mkUpdateRepositoryNameResponse = UpdateRepositoryNameResponse'
