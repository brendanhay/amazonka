{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.Untag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes tags from a specified resource group.
module Network.AWS.ResourceGroups.Untag
    (
    -- * Creating a request
      Untag (..)
    , mkUntag
    -- ** Request lenses
    , uArn
    , uKeys

    -- * Destructuring the response
    , UntagResponse (..)
    , mkUntagResponse
    -- ** Response lenses
    , urrsArn
    , urrsKeys
    , urrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.ResourceGroups.Types as Types
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUntag' smart constructor.
data Untag = Untag'
  { arn :: Types.Arn
    -- ^ The ARN of the resource group from which to remove tags. The command removed both the specified keys and any values associated with those keys.
  , keys :: [Types.TagKey]
    -- ^ The keys of the tags to be removed.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Untag' value with any optional fields omitted.
mkUntag
    :: Types.Arn -- ^ 'arn'
    -> Untag
mkUntag arn = Untag'{arn, keys = Core.mempty}

-- | The ARN of the resource group from which to remove tags. The command removed both the specified keys and any values associated with those keys.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uArn :: Lens.Lens' Untag Types.Arn
uArn = Lens.field @"arn"
{-# INLINEABLE uArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The keys of the tags to be removed.
--
-- /Note:/ Consider using 'keys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uKeys :: Lens.Lens' Untag [Types.TagKey]
uKeys = Lens.field @"keys"
{-# INLINEABLE uKeys #-}
{-# DEPRECATED keys "Use generic-lens or generic-optics with 'keys' instead"  #-}

instance Core.ToQuery Untag where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders Untag where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON Untag where
        toJSON Untag{..}
          = Core.object (Core.catMaybes [Core.Just ("Keys" Core..= keys)])

instance Core.AWSRequest Untag where
        type Rs Untag = UntagResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PATCH,
                         Core._rqPath =
                           "/resources/" Core.<> Core.toText arn Core.<> "/tags",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UntagResponse' Core.<$>
                   (x Core..:? "Arn") Core.<*> x Core..:? "Keys" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUntagResponse' smart constructor.
data UntagResponse = UntagResponse'
  { arn :: Core.Maybe Types.Arn
    -- ^ The ARN of the resource group from which tags have been removed.
  , keys :: Core.Maybe [Types.TagKey]
    -- ^ The keys of the tags that were removed.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UntagResponse' value with any optional fields omitted.
mkUntagResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UntagResponse
mkUntagResponse responseStatus
  = UntagResponse'{arn = Core.Nothing, keys = Core.Nothing,
                   responseStatus}

-- | The ARN of the resource group from which tags have been removed.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrsArn :: Lens.Lens' UntagResponse (Core.Maybe Types.Arn)
urrsArn = Lens.field @"arn"
{-# INLINEABLE urrsArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The keys of the tags that were removed.
--
-- /Note:/ Consider using 'keys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrsKeys :: Lens.Lens' UntagResponse (Core.Maybe [Types.TagKey])
urrsKeys = Lens.field @"keys"
{-# INLINEABLE urrsKeys #-}
{-# DEPRECATED keys "Use generic-lens or generic-optics with 'keys' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrsResponseStatus :: Lens.Lens' UntagResponse Core.Int
urrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE urrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
