{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.PutTraceSegments
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Uploads segment documents to AWS X-Ray. The <https://docs.aws.amazon.com/xray/index.html X-Ray SDK> generates segment documents and sends them to the X-Ray daemon, which uploads them in batches. A segment document can be a completed segment, an in-progress segment, or an array of subsegments.
--
-- Segments must include the following fields. For the full segment document schema, see <https://docs.aws.amazon.com/xray/latest/devguide/xray-api-segmentdocuments.html AWS X-Ray Segment Documents> in the /AWS X-Ray Developer Guide/ .
-- __Required segment document fields__ 
--
--     * @name@ - The name of the service that handled the request.
--
--
--     * @id@ - A 64-bit identifier for the segment, unique among segments in the same trace, in 16 hexadecimal digits.
--
--
--     * @trace_id@ - A unique identifier that connects all segments and subsegments originating from a single client request.
--
--
--     * @start_time@ - Time the segment or subsegment was created, in floating point seconds in epoch time, accurate to milliseconds. For example, @1480615200.010@ or @1.480615200010E9@ .
--
--
--     * @end_time@ - Time the segment or subsegment was closed. For example, @1480615200.090@ or @1.480615200090E9@ . Specify either an @end_time@ or @in_progress@ .
--
--
--     * @in_progress@ - Set to @true@ instead of specifying an @end_time@ to record that a segment has been started, but is not complete. Send an in-progress segment when your application receives a request that will take a long time to serve, to trace that the request was received. When the response is sent, send the complete segment to overwrite the in-progress segment.
--
--
-- A @trace_id@ consists of three numbers separated by hyphens. For example, 1-58406520-a006649127e371903a2de979. This includes:
-- __Trace ID Format__ 
--
--     * The version number, for instance, @1@ .
--
--
--     * The time of the original request, in Unix epoch time, in 8 hexadecimal digits. For example, 10:00AM December 2nd, 2016 PST in epoch time is @1480615200@ seconds, or @58406520@ in hexadecimal.
--
--
--     * A 96-bit identifier for the trace, globally unique, in 24 hexadecimal digits.
--
--
module Network.AWS.XRay.PutTraceSegments
    (
    -- * Creating a request
      PutTraceSegments (..)
    , mkPutTraceSegments
    -- ** Request lenses
    , ptsTraceSegmentDocuments

    -- * Destructuring the response
    , PutTraceSegmentsResponse (..)
    , mkPutTraceSegmentsResponse
    -- ** Response lenses
    , ptsrrsUnprocessedTraceSegments
    , ptsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.XRay.Types as Types

-- | /See:/ 'mkPutTraceSegments' smart constructor.
newtype PutTraceSegments = PutTraceSegments'
  { traceSegmentDocuments :: [Types.TraceSegmentDocument]
    -- ^ A string containing a JSON document defining one or more segments or subsegments.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PutTraceSegments' value with any optional fields omitted.
mkPutTraceSegments
    :: PutTraceSegments
mkPutTraceSegments
  = PutTraceSegments'{traceSegmentDocuments = Core.mempty}

-- | A string containing a JSON document defining one or more segments or subsegments.
--
-- /Note:/ Consider using 'traceSegmentDocuments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptsTraceSegmentDocuments :: Lens.Lens' PutTraceSegments [Types.TraceSegmentDocument]
ptsTraceSegmentDocuments = Lens.field @"traceSegmentDocuments"
{-# INLINEABLE ptsTraceSegmentDocuments #-}
{-# DEPRECATED traceSegmentDocuments "Use generic-lens or generic-optics with 'traceSegmentDocuments' instead"  #-}

instance Core.ToQuery PutTraceSegments where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders PutTraceSegments where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON PutTraceSegments where
        toJSON PutTraceSegments{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("TraceSegmentDocuments" Core..= traceSegmentDocuments)])

instance Core.AWSRequest PutTraceSegments where
        type Rs PutTraceSegments = PutTraceSegmentsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/TraceSegments",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 PutTraceSegmentsResponse' Core.<$>
                   (x Core..:? "UnprocessedTraceSegments") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPutTraceSegmentsResponse' smart constructor.
data PutTraceSegmentsResponse = PutTraceSegmentsResponse'
  { unprocessedTraceSegments :: Core.Maybe [Types.UnprocessedTraceSegment]
    -- ^ Segments that failed processing.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutTraceSegmentsResponse' value with any optional fields omitted.
mkPutTraceSegmentsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> PutTraceSegmentsResponse
mkPutTraceSegmentsResponse responseStatus
  = PutTraceSegmentsResponse'{unprocessedTraceSegments =
                                Core.Nothing,
                              responseStatus}

-- | Segments that failed processing.
--
-- /Note:/ Consider using 'unprocessedTraceSegments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptsrrsUnprocessedTraceSegments :: Lens.Lens' PutTraceSegmentsResponse (Core.Maybe [Types.UnprocessedTraceSegment])
ptsrrsUnprocessedTraceSegments = Lens.field @"unprocessedTraceSegments"
{-# INLINEABLE ptsrrsUnprocessedTraceSegments #-}
{-# DEPRECATED unprocessedTraceSegments "Use generic-lens or generic-optics with 'unprocessedTraceSegments' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptsrrsResponseStatus :: Lens.Lens' PutTraceSegmentsResponse Core.Int
ptsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ptsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
