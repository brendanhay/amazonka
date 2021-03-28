{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.UntagProject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes tags from a project.
module Network.AWS.CodeStar.UntagProject
    (
    -- * Creating a request
      UntagProject (..)
    , mkUntagProject
    -- ** Request lenses
    , uId
    , uTags

    -- * Destructuring the response
    , UntagProjectResponse (..)
    , mkUntagProjectResponse
    -- ** Response lenses
    , ursResponseStatus
    ) where

import qualified Network.AWS.CodeStar.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUntagProject' smart constructor.
data UntagProject = UntagProject'
  { id :: Types.Id
    -- ^ The ID of the project to remove tags from.
  , tags :: [Types.TagKey]
    -- ^ The tags to remove from the project.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UntagProject' value with any optional fields omitted.
mkUntagProject
    :: Types.Id -- ^ 'id'
    -> UntagProject
mkUntagProject id = UntagProject'{id, tags = Core.mempty}

-- | The ID of the project to remove tags from.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uId :: Lens.Lens' UntagProject Types.Id
uId = Lens.field @"id"
{-# INLINEABLE uId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The tags to remove from the project.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uTags :: Lens.Lens' UntagProject [Types.TagKey]
uTags = Lens.field @"tags"
{-# INLINEABLE uTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery UntagProject where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UntagProject where
        toHeaders UntagProject{..}
          = Core.pure ("X-Amz-Target", "CodeStar_20170419.UntagProject")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UntagProject where
        toJSON UntagProject{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("id" Core..= id), Core.Just ("tags" Core..= tags)])

instance Core.AWSRequest UntagProject where
        type Rs UntagProject = UntagProjectResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 UntagProjectResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUntagProjectResponse' smart constructor.
newtype UntagProjectResponse = UntagProjectResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UntagProjectResponse' value with any optional fields omitted.
mkUntagProjectResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UntagProjectResponse
mkUntagProjectResponse responseStatus
  = UntagProjectResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ursResponseStatus :: Lens.Lens' UntagProjectResponse Core.Int
ursResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ursResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
