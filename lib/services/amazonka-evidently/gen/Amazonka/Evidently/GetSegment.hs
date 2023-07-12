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
-- Module      : Amazonka.Evidently.GetSegment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified segment. Specify the segment you
-- want to view by specifying its ARN.
module Amazonka.Evidently.GetSegment
  ( -- * Creating a Request
    GetSegment (..),
    newGetSegment,

    -- * Request Lenses
    getSegment_segment,

    -- * Destructuring the Response
    GetSegmentResponse (..),
    newGetSegmentResponse,

    -- * Response Lenses
    getSegmentResponse_httpStatus,
    getSegmentResponse_segment,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Evidently.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetSegment' smart constructor.
data GetSegment = GetSegment'
  { -- | The ARN of the segment to return information for.
    segment :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSegment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'segment', 'getSegment_segment' - The ARN of the segment to return information for.
newGetSegment ::
  -- | 'segment'
  Prelude.Text ->
  GetSegment
newGetSegment pSegment_ =
  GetSegment' {segment = pSegment_}

-- | The ARN of the segment to return information for.
getSegment_segment :: Lens.Lens' GetSegment Prelude.Text
getSegment_segment = Lens.lens (\GetSegment' {segment} -> segment) (\s@GetSegment' {} a -> s {segment = a} :: GetSegment)

instance Core.AWSRequest GetSegment where
  type AWSResponse GetSegment = GetSegmentResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSegmentResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "segment")
      )

instance Prelude.Hashable GetSegment where
  hashWithSalt _salt GetSegment' {..} =
    _salt `Prelude.hashWithSalt` segment

instance Prelude.NFData GetSegment where
  rnf GetSegment' {..} = Prelude.rnf segment

instance Data.ToHeaders GetSegment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetSegment where
  toPath GetSegment' {..} =
    Prelude.mconcat ["/segments/", Data.toBS segment]

instance Data.ToQuery GetSegment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSegmentResponse' smart constructor.
data GetSegmentResponse = GetSegmentResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A structure that contains the complete information about the segment.
    segment :: Segment
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSegmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getSegmentResponse_httpStatus' - The response's http status code.
--
-- 'segment', 'getSegmentResponse_segment' - A structure that contains the complete information about the segment.
newGetSegmentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'segment'
  Segment ->
  GetSegmentResponse
newGetSegmentResponse pHttpStatus_ pSegment_ =
  GetSegmentResponse'
    { httpStatus = pHttpStatus_,
      segment = pSegment_
    }

-- | The response's http status code.
getSegmentResponse_httpStatus :: Lens.Lens' GetSegmentResponse Prelude.Int
getSegmentResponse_httpStatus = Lens.lens (\GetSegmentResponse' {httpStatus} -> httpStatus) (\s@GetSegmentResponse' {} a -> s {httpStatus = a} :: GetSegmentResponse)

-- | A structure that contains the complete information about the segment.
getSegmentResponse_segment :: Lens.Lens' GetSegmentResponse Segment
getSegmentResponse_segment = Lens.lens (\GetSegmentResponse' {segment} -> segment) (\s@GetSegmentResponse' {} a -> s {segment = a} :: GetSegmentResponse)

instance Prelude.NFData GetSegmentResponse where
  rnf GetSegmentResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf segment
