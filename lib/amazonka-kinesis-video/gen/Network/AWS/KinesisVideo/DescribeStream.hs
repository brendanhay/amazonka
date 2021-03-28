{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideo.DescribeStream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the most current information about the specified stream. You must specify either the @StreamName@ or the @StreamARN@ . 
module Network.AWS.KinesisVideo.DescribeStream
    (
    -- * Creating a request
      DescribeStream (..)
    , mkDescribeStream
    -- ** Request lenses
    , dStreamARN
    , dStreamName

    -- * Destructuring the response
    , DescribeStreamResponse (..)
    , mkDescribeStreamResponse
    -- ** Response lenses
    , dsrfrsStreamInfo
    , dsrfrsResponseStatus
    ) where

import qualified Network.AWS.KinesisVideo.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeStream' smart constructor.
data DescribeStream = DescribeStream'
  { streamARN :: Core.Maybe Types.ResourceARN
    -- ^ The Amazon Resource Name (ARN) of the stream.
  , streamName :: Core.Maybe Types.StreamName
    -- ^ The name of the stream.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeStream' value with any optional fields omitted.
mkDescribeStream
    :: DescribeStream
mkDescribeStream
  = DescribeStream'{streamARN = Core.Nothing,
                    streamName = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the stream.
--
-- /Note:/ Consider using 'streamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dStreamARN :: Lens.Lens' DescribeStream (Core.Maybe Types.ResourceARN)
dStreamARN = Lens.field @"streamARN"
{-# INLINEABLE dStreamARN #-}
{-# DEPRECATED streamARN "Use generic-lens or generic-optics with 'streamARN' instead"  #-}

-- | The name of the stream.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dStreamName :: Lens.Lens' DescribeStream (Core.Maybe Types.StreamName)
dStreamName = Lens.field @"streamName"
{-# INLINEABLE dStreamName #-}
{-# DEPRECATED streamName "Use generic-lens or generic-optics with 'streamName' instead"  #-}

instance Core.ToQuery DescribeStream where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeStream where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON DescribeStream where
        toJSON DescribeStream{..}
          = Core.object
              (Core.catMaybes
                 [("StreamARN" Core..=) Core.<$> streamARN,
                  ("StreamName" Core..=) Core.<$> streamName])

instance Core.AWSRequest DescribeStream where
        type Rs DescribeStream = DescribeStreamResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/describeStream",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeStreamResponse' Core.<$>
                   (x Core..:? "StreamInfo") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeStreamResponse' smart constructor.
data DescribeStreamResponse = DescribeStreamResponse'
  { streamInfo :: Core.Maybe Types.StreamInfo
    -- ^ An object that describes the stream.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeStreamResponse' value with any optional fields omitted.
mkDescribeStreamResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeStreamResponse
mkDescribeStreamResponse responseStatus
  = DescribeStreamResponse'{streamInfo = Core.Nothing,
                            responseStatus}

-- | An object that describes the stream.
--
-- /Note:/ Consider using 'streamInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrfrsStreamInfo :: Lens.Lens' DescribeStreamResponse (Core.Maybe Types.StreamInfo)
dsrfrsStreamInfo = Lens.field @"streamInfo"
{-# INLINEABLE dsrfrsStreamInfo #-}
{-# DEPRECATED streamInfo "Use generic-lens or generic-optics with 'streamInfo' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrfrsResponseStatus :: Lens.Lens' DescribeStreamResponse Core.Int
dsrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dsrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
