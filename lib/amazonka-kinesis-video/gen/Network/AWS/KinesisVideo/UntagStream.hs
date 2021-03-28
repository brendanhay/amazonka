{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideo.UntagStream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes one or more tags from a stream. In the request, specify only a tag key or keys; don't specify the value. If you specify a tag key that does not exist, it's ignored.
--
-- In the request, you must provide the @StreamName@ or @StreamARN@ .
module Network.AWS.KinesisVideo.UntagStream
    (
    -- * Creating a request
      UntagStream (..)
    , mkUntagStream
    -- ** Request lenses
    , usTagKeyList
    , usStreamARN
    , usStreamName

    -- * Destructuring the response
    , UntagStreamResponse (..)
    , mkUntagStreamResponse
    -- ** Response lenses
    , usrrsResponseStatus
    ) where

import qualified Network.AWS.KinesisVideo.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUntagStream' smart constructor.
data UntagStream = UntagStream'
  { tagKeyList :: Core.NonEmpty Types.TagKey
    -- ^ A list of the keys of the tags that you want to remove.
  , streamARN :: Core.Maybe Types.StreamARN
    -- ^ The Amazon Resource Name (ARN) of the stream that you want to remove tags from.
  , streamName :: Core.Maybe Types.StreamName
    -- ^ The name of the stream that you want to remove tags from.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UntagStream' value with any optional fields omitted.
mkUntagStream
    :: Core.NonEmpty Types.TagKey -- ^ 'tagKeyList'
    -> UntagStream
mkUntagStream tagKeyList
  = UntagStream'{tagKeyList, streamARN = Core.Nothing,
                 streamName = Core.Nothing}

-- | A list of the keys of the tags that you want to remove.
--
-- /Note:/ Consider using 'tagKeyList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usTagKeyList :: Lens.Lens' UntagStream (Core.NonEmpty Types.TagKey)
usTagKeyList = Lens.field @"tagKeyList"
{-# INLINEABLE usTagKeyList #-}
{-# DEPRECATED tagKeyList "Use generic-lens or generic-optics with 'tagKeyList' instead"  #-}

-- | The Amazon Resource Name (ARN) of the stream that you want to remove tags from.
--
-- /Note:/ Consider using 'streamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usStreamARN :: Lens.Lens' UntagStream (Core.Maybe Types.StreamARN)
usStreamARN = Lens.field @"streamARN"
{-# INLINEABLE usStreamARN #-}
{-# DEPRECATED streamARN "Use generic-lens or generic-optics with 'streamARN' instead"  #-}

-- | The name of the stream that you want to remove tags from.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usStreamName :: Lens.Lens' UntagStream (Core.Maybe Types.StreamName)
usStreamName = Lens.field @"streamName"
{-# INLINEABLE usStreamName #-}
{-# DEPRECATED streamName "Use generic-lens or generic-optics with 'streamName' instead"  #-}

instance Core.ToQuery UntagStream where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UntagStream where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON UntagStream where
        toJSON UntagStream{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("TagKeyList" Core..= tagKeyList),
                  ("StreamARN" Core..=) Core.<$> streamARN,
                  ("StreamName" Core..=) Core.<$> streamName])

instance Core.AWSRequest UntagStream where
        type Rs UntagStream = UntagStreamResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/untagStream",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 UntagStreamResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUntagStreamResponse' smart constructor.
newtype UntagStreamResponse = UntagStreamResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UntagStreamResponse' value with any optional fields omitted.
mkUntagStreamResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UntagStreamResponse
mkUntagStreamResponse responseStatus
  = UntagStreamResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usrrsResponseStatus :: Lens.Lens' UntagStreamResponse Core.Int
usrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE usrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
