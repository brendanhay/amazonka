{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.DeleteRepository
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a repository. If a specified repository was already deleted, a null repository ID is returned.
--
-- /Important:/ Deleting a repository also deletes all associated objects and metadata. After a repository is deleted, all future push calls to the deleted repository fail.
module Network.AWS.CodeCommit.DeleteRepository
    (
    -- * Creating a request
      DeleteRepository (..)
    , mkDeleteRepository
    -- ** Request lenses
    , drRepositoryName

    -- * Destructuring the response
    , DeleteRepositoryResponse (..)
    , mkDeleteRepositoryResponse
    -- ** Response lenses
    , drrrsRepositoryId
    , drrrsResponseStatus
    ) where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a delete repository operation.
--
-- /See:/ 'mkDeleteRepository' smart constructor.
newtype DeleteRepository = DeleteRepository'
  { repositoryName :: Types.RepositoryName
    -- ^ The name of the repository to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteRepository' value with any optional fields omitted.
mkDeleteRepository
    :: Types.RepositoryName -- ^ 'repositoryName'
    -> DeleteRepository
mkDeleteRepository repositoryName
  = DeleteRepository'{repositoryName}

-- | The name of the repository to delete.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drRepositoryName :: Lens.Lens' DeleteRepository Types.RepositoryName
drRepositoryName = Lens.field @"repositoryName"
{-# INLINEABLE drRepositoryName #-}
{-# DEPRECATED repositoryName "Use generic-lens or generic-optics with 'repositoryName' instead"  #-}

instance Core.ToQuery DeleteRepository where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteRepository where
        toHeaders DeleteRepository{..}
          = Core.pure
              ("X-Amz-Target", "CodeCommit_20150413.DeleteRepository")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteRepository where
        toJSON DeleteRepository{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("repositoryName" Core..= repositoryName)])

instance Core.AWSRequest DeleteRepository where
        type Rs DeleteRepository = DeleteRepositoryResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteRepositoryResponse' Core.<$>
                   (x Core..:? "repositoryId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the output of a delete repository operation.
--
-- /See:/ 'mkDeleteRepositoryResponse' smart constructor.
data DeleteRepositoryResponse = DeleteRepositoryResponse'
  { repositoryId :: Core.Maybe Types.RepositoryId
    -- ^ The ID of the repository that was deleted.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteRepositoryResponse' value with any optional fields omitted.
mkDeleteRepositoryResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteRepositoryResponse
mkDeleteRepositoryResponse responseStatus
  = DeleteRepositoryResponse'{repositoryId = Core.Nothing,
                              responseStatus}

-- | The ID of the repository that was deleted.
--
-- /Note:/ Consider using 'repositoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsRepositoryId :: Lens.Lens' DeleteRepositoryResponse (Core.Maybe Types.RepositoryId)
drrrsRepositoryId = Lens.field @"repositoryId"
{-# INLINEABLE drrrsRepositoryId #-}
{-# DEPRECATED repositoryId "Use generic-lens or generic-optics with 'repositoryId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsResponseStatus :: Lens.Lens' DeleteRepositoryResponse Core.Int
drrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
