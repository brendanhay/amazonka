{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DeleteTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified tags.
module Network.AWS.AutoScaling.DeleteTags
    (
    -- * Creating a request
      DeleteTags (..)
    , mkDeleteTags
    -- ** Request lenses
    , dtTags

    -- * Destructuring the response
    , DeleteTagsResponse (..)
    , mkDeleteTagsResponse
    ) where

import qualified Network.AWS.AutoScaling.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteTags' smart constructor.
newtype DeleteTags = DeleteTags'
  { tags :: [Types.Tag]
    -- ^ One or more tags.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTags' value with any optional fields omitted.
mkDeleteTags
    :: DeleteTags
mkDeleteTags = DeleteTags'{tags = Core.mempty}

-- | One or more tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtTags :: Lens.Lens' DeleteTags [Types.Tag]
dtTags = Lens.field @"tags"
{-# INLINEABLE dtTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery DeleteTags where
        toQuery DeleteTags{..}
          = Core.toQueryPair "Action" ("DeleteTags" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2011-01-01" :: Core.Text)
              Core.<> Core.toQueryPair "Tags" (Core.toQueryList "member" tags)

instance Core.ToHeaders DeleteTags where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteTags where
        type Rs DeleteTags = DeleteTagsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeleteTagsResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteTagsResponse' smart constructor.
data DeleteTagsResponse = DeleteTagsResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTagsResponse' value with any optional fields omitted.
mkDeleteTagsResponse
    :: DeleteTagsResponse
mkDeleteTagsResponse = DeleteTagsResponse'
