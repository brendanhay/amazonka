{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.CreateTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Add a tag to a resource.
module Network.AWS.MQ.CreateTags
    (
    -- * Creating a request
      CreateTags (..)
    , mkCreateTags
    -- ** Request lenses
    , ctResourceArn
    , ctTags

    -- * Destructuring the response
    , CreateTagsResponse (..)
    , mkCreateTagsResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MQ.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A map of the key-value pairs for the resource tag.
--
-- /See:/ 'mkCreateTags' smart constructor.
data CreateTags = CreateTags'
  { resourceArn :: Core.Text
    -- ^ The Amazon Resource Name (ARN) of the resource tag.
  , tags :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ The key-value pair for the resource tag.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTags' value with any optional fields omitted.
mkCreateTags
    :: Core.Text -- ^ 'resourceArn'
    -> CreateTags
mkCreateTags resourceArn
  = CreateTags'{resourceArn, tags = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the resource tag.
--
-- /Note:/ Consider using 'resourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctResourceArn :: Lens.Lens' CreateTags Core.Text
ctResourceArn = Lens.field @"resourceArn"
{-# INLINEABLE ctResourceArn #-}
{-# DEPRECATED resourceArn "Use generic-lens or generic-optics with 'resourceArn' instead"  #-}

-- | The key-value pair for the resource tag.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctTags :: Lens.Lens' CreateTags (Core.Maybe (Core.HashMap Core.Text Core.Text))
ctTags = Lens.field @"tags"
{-# INLINEABLE ctTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateTags where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateTags where
        toHeaders CreateTags{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateTags where
        toJSON CreateTags{..}
          = Core.object (Core.catMaybes [("tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreateTags where
        type Rs CreateTags = CreateTagsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/v1/tags/" Core.<> Core.toText resourceArn,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull CreateTagsResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateTagsResponse' smart constructor.
data CreateTagsResponse = CreateTagsResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTagsResponse' value with any optional fields omitted.
mkCreateTagsResponse
    :: CreateTagsResponse
mkCreateTagsResponse = CreateTagsResponse'
