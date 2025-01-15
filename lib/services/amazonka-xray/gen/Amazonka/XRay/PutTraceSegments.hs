{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.XRay.PutTraceSegments
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Uploads segment documents to Amazon Web Services X-Ray. The
-- <https://docs.aws.amazon.com/xray/index.html X-Ray SDK> generates
-- segment documents and sends them to the X-Ray daemon, which uploads them
-- in batches. A segment document can be a completed segment, an
-- in-progress segment, or an array of subsegments.
--
-- Segments must include the following fields. For the full segment
-- document schema, see
-- <https://docs.aws.amazon.com/xray/latest/devguide/xray-api-segmentdocuments.html Amazon Web Services X-Ray Segment Documents>
-- in the /Amazon Web Services X-Ray Developer Guide/.
--
-- __Required segment document fields__
--
-- -   @name@ - The name of the service that handled the request.
--
-- -   @id@ - A 64-bit identifier for the segment, unique among segments in
--     the same trace, in 16 hexadecimal digits.
--
-- -   @trace_id@ - A unique identifier that connects all segments and
--     subsegments originating from a single client request.
--
-- -   @start_time@ - Time the segment or subsegment was created, in
--     floating point seconds in epoch time, accurate to milliseconds. For
--     example, @1480615200.010@ or @1.480615200010E9@.
--
-- -   @end_time@ - Time the segment or subsegment was closed. For example,
--     @1480615200.090@ or @1.480615200090E9@. Specify either an @end_time@
--     or @in_progress@.
--
-- -   @in_progress@ - Set to @true@ instead of specifying an @end_time@ to
--     record that a segment has been started, but is not complete. Send an
--     in-progress segment when your application receives a request that
--     will take a long time to serve, to trace that the request was
--     received. When the response is sent, send the complete segment to
--     overwrite the in-progress segment.
--
-- A @trace_id@ consists of three numbers separated by hyphens. For
-- example, 1-58406520-a006649127e371903a2de979. This includes:
--
-- __Trace ID Format__
--
-- -   The version number, for instance, @1@.
--
-- -   The time of the original request, in Unix epoch time, in 8
--     hexadecimal digits. For example, 10:00AM December 2nd, 2016 PST in
--     epoch time is @1480615200@ seconds, or @58406520@ in hexadecimal.
--
-- -   A 96-bit identifier for the trace, globally unique, in 24
--     hexadecimal digits.
module Amazonka.XRay.PutTraceSegments
  ( -- * Creating a Request
    PutTraceSegments (..),
    newPutTraceSegments,

    -- * Request Lenses
    putTraceSegments_traceSegmentDocuments,

    -- * Destructuring the Response
    PutTraceSegmentsResponse (..),
    newPutTraceSegmentsResponse,

    -- * Response Lenses
    putTraceSegmentsResponse_unprocessedTraceSegments,
    putTraceSegmentsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.XRay.Types

-- | /See:/ 'newPutTraceSegments' smart constructor.
data PutTraceSegments = PutTraceSegments'
  { -- | A string containing a JSON document defining one or more segments or
    -- subsegments.
    traceSegmentDocuments :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutTraceSegments' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'traceSegmentDocuments', 'putTraceSegments_traceSegmentDocuments' - A string containing a JSON document defining one or more segments or
-- subsegments.
newPutTraceSegments ::
  PutTraceSegments
newPutTraceSegments =
  PutTraceSegments'
    { traceSegmentDocuments =
        Prelude.mempty
    }

-- | A string containing a JSON document defining one or more segments or
-- subsegments.
putTraceSegments_traceSegmentDocuments :: Lens.Lens' PutTraceSegments [Prelude.Text]
putTraceSegments_traceSegmentDocuments = Lens.lens (\PutTraceSegments' {traceSegmentDocuments} -> traceSegmentDocuments) (\s@PutTraceSegments' {} a -> s {traceSegmentDocuments = a} :: PutTraceSegments) Prelude.. Lens.coerced

instance Core.AWSRequest PutTraceSegments where
  type
    AWSResponse PutTraceSegments =
      PutTraceSegmentsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutTraceSegmentsResponse'
            Prelude.<$> ( x
                            Data..?> "UnprocessedTraceSegments"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutTraceSegments where
  hashWithSalt _salt PutTraceSegments' {..} =
    _salt `Prelude.hashWithSalt` traceSegmentDocuments

instance Prelude.NFData PutTraceSegments where
  rnf PutTraceSegments' {..} =
    Prelude.rnf traceSegmentDocuments

instance Data.ToHeaders PutTraceSegments where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON PutTraceSegments where
  toJSON PutTraceSegments' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "TraceSegmentDocuments"
                  Data..= traceSegmentDocuments
              )
          ]
      )

instance Data.ToPath PutTraceSegments where
  toPath = Prelude.const "/TraceSegments"

instance Data.ToQuery PutTraceSegments where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutTraceSegmentsResponse' smart constructor.
data PutTraceSegmentsResponse = PutTraceSegmentsResponse'
  { -- | Segments that failed processing.
    unprocessedTraceSegments :: Prelude.Maybe [UnprocessedTraceSegment],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutTraceSegmentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'unprocessedTraceSegments', 'putTraceSegmentsResponse_unprocessedTraceSegments' - Segments that failed processing.
--
-- 'httpStatus', 'putTraceSegmentsResponse_httpStatus' - The response's http status code.
newPutTraceSegmentsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutTraceSegmentsResponse
newPutTraceSegmentsResponse pHttpStatus_ =
  PutTraceSegmentsResponse'
    { unprocessedTraceSegments =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Segments that failed processing.
putTraceSegmentsResponse_unprocessedTraceSegments :: Lens.Lens' PutTraceSegmentsResponse (Prelude.Maybe [UnprocessedTraceSegment])
putTraceSegmentsResponse_unprocessedTraceSegments = Lens.lens (\PutTraceSegmentsResponse' {unprocessedTraceSegments} -> unprocessedTraceSegments) (\s@PutTraceSegmentsResponse' {} a -> s {unprocessedTraceSegments = a} :: PutTraceSegmentsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
putTraceSegmentsResponse_httpStatus :: Lens.Lens' PutTraceSegmentsResponse Prelude.Int
putTraceSegmentsResponse_httpStatus = Lens.lens (\PutTraceSegmentsResponse' {httpStatus} -> httpStatus) (\s@PutTraceSegmentsResponse' {} a -> s {httpStatus = a} :: PutTraceSegmentsResponse)

instance Prelude.NFData PutTraceSegmentsResponse where
  rnf PutTraceSegmentsResponse' {..} =
    Prelude.rnf unprocessedTraceSegments `Prelude.seq`
      Prelude.rnf httpStatus
