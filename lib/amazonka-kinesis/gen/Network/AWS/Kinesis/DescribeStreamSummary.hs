{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.DescribeStreamSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a summarized description of the specified Kinesis data stream without the shard list.
--
-- The information returned includes the stream name, Amazon Resource Name (ARN), status, record retention period, approximate creation time, monitoring, encryption details, and open shard count. 
-- 'DescribeStreamSummary' has a limit of 20 transactions per second per account.
module Network.AWS.Kinesis.DescribeStreamSummary
    (
    -- * Creating a request
      DescribeStreamSummary (..)
    , mkDescribeStreamSummary
    -- ** Request lenses
    , dssStreamName

    -- * Destructuring the response
    , DescribeStreamSummaryResponse (..)
    , mkDescribeStreamSummaryResponse
    -- ** Response lenses
    , dssrrsStreamDescriptionSummary
    , dssrrsResponseStatus
    ) where

import qualified Network.AWS.Kinesis.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeStreamSummary' smart constructor.
newtype DescribeStreamSummary = DescribeStreamSummary'
  { streamName :: Types.StreamName
    -- ^ The name of the stream to describe.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeStreamSummary' value with any optional fields omitted.
mkDescribeStreamSummary
    :: Types.StreamName -- ^ 'streamName'
    -> DescribeStreamSummary
mkDescribeStreamSummary streamName
  = DescribeStreamSummary'{streamName}

-- | The name of the stream to describe.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssStreamName :: Lens.Lens' DescribeStreamSummary Types.StreamName
dssStreamName = Lens.field @"streamName"
{-# INLINEABLE dssStreamName #-}
{-# DEPRECATED streamName "Use generic-lens or generic-optics with 'streamName' instead"  #-}

instance Core.ToQuery DescribeStreamSummary where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeStreamSummary where
        toHeaders DescribeStreamSummary{..}
          = Core.pure
              ("X-Amz-Target", "Kinesis_20131202.DescribeStreamSummary")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeStreamSummary where
        toJSON DescribeStreamSummary{..}
          = Core.object
              (Core.catMaybes [Core.Just ("StreamName" Core..= streamName)])

instance Core.AWSRequest DescribeStreamSummary where
        type Rs DescribeStreamSummary = DescribeStreamSummaryResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeStreamSummaryResponse' Core.<$>
                   (x Core..: "StreamDescriptionSummary") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeStreamSummaryResponse' smart constructor.
data DescribeStreamSummaryResponse = DescribeStreamSummaryResponse'
  { streamDescriptionSummary :: Types.StreamDescriptionSummary
    -- ^ A 'StreamDescriptionSummary' containing information about the stream.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeStreamSummaryResponse' value with any optional fields omitted.
mkDescribeStreamSummaryResponse
    :: Types.StreamDescriptionSummary -- ^ 'streamDescriptionSummary'
    -> Core.Int -- ^ 'responseStatus'
    -> DescribeStreamSummaryResponse
mkDescribeStreamSummaryResponse streamDescriptionSummary
  responseStatus
  = DescribeStreamSummaryResponse'{streamDescriptionSummary,
                                   responseStatus}

-- | A 'StreamDescriptionSummary' containing information about the stream.
--
-- /Note:/ Consider using 'streamDescriptionSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssrrsStreamDescriptionSummary :: Lens.Lens' DescribeStreamSummaryResponse Types.StreamDescriptionSummary
dssrrsStreamDescriptionSummary = Lens.field @"streamDescriptionSummary"
{-# INLINEABLE dssrrsStreamDescriptionSummary #-}
{-# DEPRECATED streamDescriptionSummary "Use generic-lens or generic-optics with 'streamDescriptionSummary' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssrrsResponseStatus :: Lens.Lens' DescribeStreamSummaryResponse Core.Int
dssrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dssrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
