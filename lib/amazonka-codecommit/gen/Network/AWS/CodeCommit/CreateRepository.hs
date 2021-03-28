{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateRepository (..)
    , mkCreateRepository
    -- ** Request lenses
    , crRepositoryName
    , crRepositoryDescription
    , crTags

    -- * Destructuring the response
    , CreateRepositoryResponse (..)
    , mkCreateRepositoryResponse
    -- ** Response lenses
    , crrrsRepositoryMetadata
    , crrrsResponseStatus
    ) where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a create repository operation.
--
-- /See:/ 'mkCreateRepository' smart constructor.
data CreateRepository = CreateRepository'
  { repositoryName :: Types.RepositoryName
    -- ^ The name of the new repository to be created.
  , repositoryDescription :: Core.Maybe Types.RepositoryDescription
    -- ^ A comment or description about the new repository.
  , tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue)
    -- ^ One or more tag key-value pairs to use when tagging this repository.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateRepository' value with any optional fields omitted.
mkCreateRepository
    :: Types.RepositoryName -- ^ 'repositoryName'
    -> CreateRepository
mkCreateRepository repositoryName
  = CreateRepository'{repositoryName,
                      repositoryDescription = Core.Nothing, tags = Core.Nothing}

-- | The name of the new repository to be created.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crRepositoryName :: Lens.Lens' CreateRepository Types.RepositoryName
crRepositoryName = Lens.field @"repositoryName"
{-# INLINEABLE crRepositoryName #-}
{-# DEPRECATED repositoryName "Use generic-lens or generic-optics with 'repositoryName' instead"  #-}

-- | A comment or description about the new repository.
--
-- /Note:/ Consider using 'repositoryDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crRepositoryDescription :: Lens.Lens' CreateRepository (Core.Maybe Types.RepositoryDescription)
crRepositoryDescription = Lens.field @"repositoryDescription"
{-# INLINEABLE crRepositoryDescription #-}
{-# DEPRECATED repositoryDescription "Use generic-lens or generic-optics with 'repositoryDescription' instead"  #-}

-- | One or more tag key-value pairs to use when tagging this repository.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crTags :: Lens.Lens' CreateRepository (Core.Maybe (Core.HashMap Types.TagKey Types.TagValue))
crTags = Lens.field @"tags"
{-# INLINEABLE crTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateRepository where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateRepository where
        toHeaders CreateRepository{..}
          = Core.pure
              ("X-Amz-Target", "CodeCommit_20150413.CreateRepository")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateRepository where
        toJSON CreateRepository{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("repositoryName" Core..= repositoryName),
                  ("repositoryDescription" Core..=) Core.<$> repositoryDescription,
                  ("tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreateRepository where
        type Rs CreateRepository = CreateRepositoryResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateRepositoryResponse' Core.<$>
                   (x Core..:? "repositoryMetadata") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the output of a create repository operation.
--
-- /See:/ 'mkCreateRepositoryResponse' smart constructor.
data CreateRepositoryResponse = CreateRepositoryResponse'
  { repositoryMetadata :: Core.Maybe Types.RepositoryMetadata
    -- ^ Information about the newly created repository.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateRepositoryResponse' value with any optional fields omitted.
mkCreateRepositoryResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateRepositoryResponse
mkCreateRepositoryResponse responseStatus
  = CreateRepositoryResponse'{repositoryMetadata = Core.Nothing,
                              responseStatus}

-- | Information about the newly created repository.
--
-- /Note:/ Consider using 'repositoryMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrrsRepositoryMetadata :: Lens.Lens' CreateRepositoryResponse (Core.Maybe Types.RepositoryMetadata)
crrrsRepositoryMetadata = Lens.field @"repositoryMetadata"
{-# INLINEABLE crrrsRepositoryMetadata #-}
{-# DEPRECATED repositoryMetadata "Use generic-lens or generic-optics with 'repositoryMetadata' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrrsResponseStatus :: Lens.Lens' CreateRepositoryResponse Core.Int
crrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE crrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
