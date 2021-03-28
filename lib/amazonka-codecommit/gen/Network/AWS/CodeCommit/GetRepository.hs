{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetRepository (..)
    , mkGetRepository
    -- ** Request lenses
    , grRepositoryName

    -- * Destructuring the response
    , GetRepositoryResponse (..)
    , mkGetRepositoryResponse
    -- ** Response lenses
    , grrrsRepositoryMetadata
    , grrrsResponseStatus
    ) where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a get repository operation.
--
-- /See:/ 'mkGetRepository' smart constructor.
newtype GetRepository = GetRepository'
  { repositoryName :: Types.RepositoryName
    -- ^ The name of the repository to get information about.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetRepository' value with any optional fields omitted.
mkGetRepository
    :: Types.RepositoryName -- ^ 'repositoryName'
    -> GetRepository
mkGetRepository repositoryName = GetRepository'{repositoryName}

-- | The name of the repository to get information about.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grRepositoryName :: Lens.Lens' GetRepository Types.RepositoryName
grRepositoryName = Lens.field @"repositoryName"
{-# INLINEABLE grRepositoryName #-}
{-# DEPRECATED repositoryName "Use generic-lens or generic-optics with 'repositoryName' instead"  #-}

instance Core.ToQuery GetRepository where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetRepository where
        toHeaders GetRepository{..}
          = Core.pure ("X-Amz-Target", "CodeCommit_20150413.GetRepository")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetRepository where
        toJSON GetRepository{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("repositoryName" Core..= repositoryName)])

instance Core.AWSRequest GetRepository where
        type Rs GetRepository = GetRepositoryResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetRepositoryResponse' Core.<$>
                   (x Core..:? "repositoryMetadata") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the output of a get repository operation.
--
-- /See:/ 'mkGetRepositoryResponse' smart constructor.
data GetRepositoryResponse = GetRepositoryResponse'
  { repositoryMetadata :: Core.Maybe Types.RepositoryMetadata
    -- ^ Information about the repository.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetRepositoryResponse' value with any optional fields omitted.
mkGetRepositoryResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetRepositoryResponse
mkGetRepositoryResponse responseStatus
  = GetRepositoryResponse'{repositoryMetadata = Core.Nothing,
                           responseStatus}

-- | Information about the repository.
--
-- /Note:/ Consider using 'repositoryMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrrsRepositoryMetadata :: Lens.Lens' GetRepositoryResponse (Core.Maybe Types.RepositoryMetadata)
grrrsRepositoryMetadata = Lens.field @"repositoryMetadata"
{-# INLINEABLE grrrsRepositoryMetadata #-}
{-# DEPRECATED repositoryMetadata "Use generic-lens or generic-optics with 'repositoryMetadata' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrrsResponseStatus :: Lens.Lens' GetRepositoryResponse Core.Int
grrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE grrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
