{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
module Network.AWS.XRay.PutTraceSegments
  ( -- * Creating a request
    PutTraceSegments (..),
    mkPutTraceSegments,

    -- ** Request lenses
    ptsTraceSegmentDocuments,

    -- * Destructuring the response
    PutTraceSegmentsResponse (..),
    mkPutTraceSegmentsResponse,

    -- ** Response lenses
    ptsrsUnprocessedTraceSegments,
    ptsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.XRay.Types

-- | /See:/ 'mkPutTraceSegments' smart constructor.
newtype PutTraceSegments = PutTraceSegments'
  { -- | A string containing a JSON document defining one or more segments or subsegments.
    traceSegmentDocuments :: [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutTraceSegments' with the minimum fields required to make a request.
--
-- * 'traceSegmentDocuments' - A string containing a JSON document defining one or more segments or subsegments.
mkPutTraceSegments ::
  PutTraceSegments
mkPutTraceSegments =
  PutTraceSegments' {traceSegmentDocuments = Lude.mempty}

-- | A string containing a JSON document defining one or more segments or subsegments.
--
-- /Note:/ Consider using 'traceSegmentDocuments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptsTraceSegmentDocuments :: Lens.Lens' PutTraceSegments [Lude.Text]
ptsTraceSegmentDocuments = Lens.lens (traceSegmentDocuments :: PutTraceSegments -> [Lude.Text]) (\s a -> s {traceSegmentDocuments = a} :: PutTraceSegments)
{-# DEPRECATED ptsTraceSegmentDocuments "Use generic-lens or generic-optics with 'traceSegmentDocuments' instead." #-}

instance Lude.AWSRequest PutTraceSegments where
  type Rs PutTraceSegments = PutTraceSegmentsResponse
  request = Req.postJSON xRayService
  response =
    Res.receiveJSON
      ( \s h x ->
          PutTraceSegmentsResponse'
            Lude.<$> (x Lude..?> "UnprocessedTraceSegments" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutTraceSegments where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON PutTraceSegments where
  toJSON PutTraceSegments' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("TraceSegmentDocuments" Lude..= traceSegmentDocuments)
          ]
      )

instance Lude.ToPath PutTraceSegments where
  toPath = Lude.const "/TraceSegments"

instance Lude.ToQuery PutTraceSegments where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutTraceSegmentsResponse' smart constructor.
data PutTraceSegmentsResponse = PutTraceSegmentsResponse'
  { -- | Segments that failed processing.
    unprocessedTraceSegments :: Lude.Maybe [UnprocessedTraceSegment],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutTraceSegmentsResponse' with the minimum fields required to make a request.
--
-- * 'unprocessedTraceSegments' - Segments that failed processing.
-- * 'responseStatus' - The response status code.
mkPutTraceSegmentsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutTraceSegmentsResponse
mkPutTraceSegmentsResponse pResponseStatus_ =
  PutTraceSegmentsResponse'
    { unprocessedTraceSegments =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Segments that failed processing.
--
-- /Note:/ Consider using 'unprocessedTraceSegments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptsrsUnprocessedTraceSegments :: Lens.Lens' PutTraceSegmentsResponse (Lude.Maybe [UnprocessedTraceSegment])
ptsrsUnprocessedTraceSegments = Lens.lens (unprocessedTraceSegments :: PutTraceSegmentsResponse -> Lude.Maybe [UnprocessedTraceSegment]) (\s a -> s {unprocessedTraceSegments = a} :: PutTraceSegmentsResponse)
{-# DEPRECATED ptsrsUnprocessedTraceSegments "Use generic-lens or generic-optics with 'unprocessedTraceSegments' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptsrsResponseStatus :: Lens.Lens' PutTraceSegmentsResponse Lude.Int
ptsrsResponseStatus = Lens.lens (responseStatus :: PutTraceSegmentsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutTraceSegmentsResponse)
{-# DEPRECATED ptsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
