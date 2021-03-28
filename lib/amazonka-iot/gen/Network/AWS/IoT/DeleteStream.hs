{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DeleteStream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a stream.
module Network.AWS.IoT.DeleteStream
    (
    -- * Creating a request
      DeleteStream (..)
    , mkDeleteStream
    -- ** Request lenses
    , dsStreamId

    -- * Destructuring the response
    , DeleteStreamResponse (..)
    , mkDeleteStreamResponse
    -- ** Response lenses
    , dsrrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteStream' smart constructor.
newtype DeleteStream = DeleteStream'
  { streamId :: Types.StreamId
    -- ^ The stream ID.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteStream' value with any optional fields omitted.
mkDeleteStream
    :: Types.StreamId -- ^ 'streamId'
    -> DeleteStream
mkDeleteStream streamId = DeleteStream'{streamId}

-- | The stream ID.
--
-- /Note:/ Consider using 'streamId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsStreamId :: Lens.Lens' DeleteStream Types.StreamId
dsStreamId = Lens.field @"streamId"
{-# INLINEABLE dsStreamId #-}
{-# DEPRECATED streamId "Use generic-lens or generic-optics with 'streamId' instead"  #-}

instance Core.ToQuery DeleteStream where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteStream where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteStream where
        type Rs DeleteStream = DeleteStreamResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath = "/streams/" Core.<> Core.toText streamId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteStreamResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteStreamResponse' smart constructor.
newtype DeleteStreamResponse = DeleteStreamResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteStreamResponse' value with any optional fields omitted.
mkDeleteStreamResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteStreamResponse
mkDeleteStreamResponse responseStatus
  = DeleteStreamResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsResponseStatus :: Lens.Lens' DeleteStreamResponse Core.Int
dsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
