{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.DeleteTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a tag from a resource.
module Network.AWS.MQ.DeleteTags
    (
    -- * Creating a request
      DeleteTags (..)
    , mkDeleteTags
    -- ** Request lenses
    , dtTagKeys
    , dtResourceArn

    -- * Destructuring the response
    , DeleteTagsResponse (..)
    , mkDeleteTagsResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MQ.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteTags' smart constructor.
data DeleteTags = DeleteTags'
  { tagKeys :: [Core.Text]
    -- ^ An array of tag keys to delete
  , resourceArn :: Core.Text
    -- ^ The Amazon Resource Name (ARN) of the resource tag.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTags' value with any optional fields omitted.
mkDeleteTags
    :: Core.Text -- ^ 'resourceArn'
    -> DeleteTags
mkDeleteTags resourceArn
  = DeleteTags'{tagKeys = Core.mempty, resourceArn}

-- | An array of tag keys to delete
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtTagKeys :: Lens.Lens' DeleteTags [Core.Text]
dtTagKeys = Lens.field @"tagKeys"
{-# INLINEABLE dtTagKeys #-}
{-# DEPRECATED tagKeys "Use generic-lens or generic-optics with 'tagKeys' instead"  #-}

-- | The Amazon Resource Name (ARN) of the resource tag.
--
-- /Note:/ Consider using 'resourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtResourceArn :: Lens.Lens' DeleteTags Core.Text
dtResourceArn = Lens.field @"resourceArn"
{-# INLINEABLE dtResourceArn #-}
{-# DEPRECATED resourceArn "Use generic-lens or generic-optics with 'resourceArn' instead"  #-}

instance Core.ToQuery DeleteTags where
        toQuery DeleteTags{..}
          = Core.toQueryPair "tagKeys" (Core.toQueryList "member" tagKeys)

instance Core.ToHeaders DeleteTags where
        toHeaders DeleteTags{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DeleteTags where
        type Rs DeleteTags = DeleteTagsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath = "/v1/tags/" Core.<> Core.toText resourceArn,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
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
