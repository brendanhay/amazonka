{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.PutTraceSegments
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Uploads segment documents to AWS X-Ray. The X-Ray SDK generates segment documents and sends them to the X-Ray daemon, which uploads them in batches. A segment document can be a completed segment, an in-progress segment, or an array of subsegments.
--
--
-- Segments must include the following fields. For the full segment document schema, see <https://docs.aws.amazon.com/xray/latest/devguide/xray-api-segmentdocuments.html AWS X-Ray Segment Documents> in the /AWS X-Ray Developer Guide/ .
--
-- __Required Segment Document Fields__
--
--     * @name@ - The name of the service that handled the request.
--
--     * @id@ - A 64-bit identifier for the segment, unique among segments in the same trace, in 16 hexadecimal digits.
--
--     * @trace_id@ - A unique identifier that connects all segments and subsegments originating from a single client request.
--
--     * @start_time@ - Time the segment or subsegment was created, in floating point seconds in epoch time, accurate to milliseconds. For example, @1480615200.010@ or @1.480615200010E9@ .
--
--     * @end_time@ - Time the segment or subsegment was closed. For example, @1480615200.090@ or @1.480615200090E9@ . Specify either an @end_time@ or @in_progress@ .
--
--     * @in_progress@ - Set to @true@ instead of specifying an @end_time@ to record that a segment has been started, but is not complete. Send an in progress segment when your application receives a request that will take a long time to serve, to trace the fact that the request was received. When the response is sent, send the complete segment to overwrite the in-progress segment.
--
--
--
-- A @trace_id@ consists of three numbers separated by hyphens. For example, 1-58406520-a006649127e371903a2de979. This includes:
--
-- __Trace ID Format__
--
--     * The version number, i.e. @1@ .
--
--     * The time of the original request, in Unix epoch time, in 8 hexadecimal digits. For example, 10:00AM December 2nd, 2016 PST in epoch time is @1480615200@ seconds, or @58406520@ in hexadecimal.
--
--     * A 96-bit identifier for the trace, globally unique, in 24 hexadecimal digits.
--
--
--
module Network.AWS.XRay.PutTraceSegments
    (
    -- * Creating a Request
      putTraceSegments
    , PutTraceSegments
    -- * Request Lenses
    , ptsTraceSegmentDocuments

    -- * Destructuring the Response
    , putTraceSegmentsResponse
    , PutTraceSegmentsResponse
    -- * Response Lenses
    , ptsrsUnprocessedTraceSegments
    , ptsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.XRay.Types
import Network.AWS.XRay.Types.Product

-- | /See:/ 'putTraceSegments' smart constructor.
newtype PutTraceSegments = PutTraceSegments'
  { _ptsTraceSegmentDocuments :: [Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutTraceSegments' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ptsTraceSegmentDocuments' - A string containing a JSON document defining one or more segments or subsegments.
putTraceSegments
    :: PutTraceSegments
putTraceSegments = PutTraceSegments' {_ptsTraceSegmentDocuments = mempty}


-- | A string containing a JSON document defining one or more segments or subsegments.
ptsTraceSegmentDocuments :: Lens' PutTraceSegments [Text]
ptsTraceSegmentDocuments = lens _ptsTraceSegmentDocuments (\ s a -> s{_ptsTraceSegmentDocuments = a}) . _Coerce

instance AWSRequest PutTraceSegments where
        type Rs PutTraceSegments = PutTraceSegmentsResponse
        request = postJSON xRay
        response
          = receiveJSON
              (\ s h x ->
                 PutTraceSegmentsResponse' <$>
                   (x .?> "UnprocessedTraceSegments" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable PutTraceSegments where

instance NFData PutTraceSegments where

instance ToHeaders PutTraceSegments where
        toHeaders = const mempty

instance ToJSON PutTraceSegments where
        toJSON PutTraceSegments'{..}
          = object
              (catMaybes
                 [Just
                    ("TraceSegmentDocuments" .=
                       _ptsTraceSegmentDocuments)])

instance ToPath PutTraceSegments where
        toPath = const "/TraceSegments"

instance ToQuery PutTraceSegments where
        toQuery = const mempty

-- | /See:/ 'putTraceSegmentsResponse' smart constructor.
data PutTraceSegmentsResponse = PutTraceSegmentsResponse'
  { _ptsrsUnprocessedTraceSegments :: !(Maybe [UnprocessedTraceSegment])
  , _ptsrsResponseStatus           :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutTraceSegmentsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ptsrsUnprocessedTraceSegments' - Segments that failed processing.
--
-- * 'ptsrsResponseStatus' - -- | The response status code.
putTraceSegmentsResponse
    :: Int -- ^ 'ptsrsResponseStatus'
    -> PutTraceSegmentsResponse
putTraceSegmentsResponse pResponseStatus_ =
  PutTraceSegmentsResponse'
    { _ptsrsUnprocessedTraceSegments = Nothing
    , _ptsrsResponseStatus = pResponseStatus_
    }


-- | Segments that failed processing.
ptsrsUnprocessedTraceSegments :: Lens' PutTraceSegmentsResponse [UnprocessedTraceSegment]
ptsrsUnprocessedTraceSegments = lens _ptsrsUnprocessedTraceSegments (\ s a -> s{_ptsrsUnprocessedTraceSegments = a}) . _Default . _Coerce

-- | -- | The response status code.
ptsrsResponseStatus :: Lens' PutTraceSegmentsResponse Int
ptsrsResponseStatus = lens _ptsrsResponseStatus (\ s a -> s{_ptsrsResponseStatus = a})

instance NFData PutTraceSegmentsResponse where
