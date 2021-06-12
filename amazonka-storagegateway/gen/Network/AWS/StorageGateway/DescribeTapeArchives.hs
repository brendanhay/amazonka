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
-- Module      : Network.AWS.StorageGateway.DescribeTapeArchives
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a description of specified virtual tapes in the virtual tape
-- shelf (VTS). This operation is only supported in the tape gateway type.
--
-- If a specific @TapeARN@ is not specified, AWS Storage Gateway returns a
-- description of all virtual tapes found in the VTS associated with your
-- account.
--
-- This operation returns paginated results.
module Network.AWS.StorageGateway.DescribeTapeArchives
  ( -- * Creating a Request
    DescribeTapeArchives (..),
    newDescribeTapeArchives,

    -- * Request Lenses
    describeTapeArchives_tapeARNs,
    describeTapeArchives_limit,
    describeTapeArchives_marker,

    -- * Destructuring the Response
    DescribeTapeArchivesResponse (..),
    newDescribeTapeArchivesResponse,

    -- * Response Lenses
    describeTapeArchivesResponse_tapeArchives,
    describeTapeArchivesResponse_marker,
    describeTapeArchivesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | DescribeTapeArchivesInput
--
-- /See:/ 'newDescribeTapeArchives' smart constructor.
data DescribeTapeArchives = DescribeTapeArchives'
  { -- | Specifies one or more unique Amazon Resource Names (ARNs) that represent
    -- the virtual tapes you want to describe.
    tapeARNs :: Core.Maybe [Core.Text],
    -- | Specifies that the number of virtual tapes described be limited to the
    -- specified number.
    limit :: Core.Maybe Core.Natural,
    -- | An opaque string that indicates the position at which to begin
    -- describing virtual tapes.
    marker :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeTapeArchives' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tapeARNs', 'describeTapeArchives_tapeARNs' - Specifies one or more unique Amazon Resource Names (ARNs) that represent
-- the virtual tapes you want to describe.
--
-- 'limit', 'describeTapeArchives_limit' - Specifies that the number of virtual tapes described be limited to the
-- specified number.
--
-- 'marker', 'describeTapeArchives_marker' - An opaque string that indicates the position at which to begin
-- describing virtual tapes.
newDescribeTapeArchives ::
  DescribeTapeArchives
newDescribeTapeArchives =
  DescribeTapeArchives'
    { tapeARNs = Core.Nothing,
      limit = Core.Nothing,
      marker = Core.Nothing
    }

-- | Specifies one or more unique Amazon Resource Names (ARNs) that represent
-- the virtual tapes you want to describe.
describeTapeArchives_tapeARNs :: Lens.Lens' DescribeTapeArchives (Core.Maybe [Core.Text])
describeTapeArchives_tapeARNs = Lens.lens (\DescribeTapeArchives' {tapeARNs} -> tapeARNs) (\s@DescribeTapeArchives' {} a -> s {tapeARNs = a} :: DescribeTapeArchives) Core.. Lens.mapping Lens._Coerce

-- | Specifies that the number of virtual tapes described be limited to the
-- specified number.
describeTapeArchives_limit :: Lens.Lens' DescribeTapeArchives (Core.Maybe Core.Natural)
describeTapeArchives_limit = Lens.lens (\DescribeTapeArchives' {limit} -> limit) (\s@DescribeTapeArchives' {} a -> s {limit = a} :: DescribeTapeArchives)

-- | An opaque string that indicates the position at which to begin
-- describing virtual tapes.
describeTapeArchives_marker :: Lens.Lens' DescribeTapeArchives (Core.Maybe Core.Text)
describeTapeArchives_marker = Lens.lens (\DescribeTapeArchives' {marker} -> marker) (\s@DescribeTapeArchives' {} a -> s {marker = a} :: DescribeTapeArchives)

instance Core.AWSPager DescribeTapeArchives where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeTapeArchivesResponse_marker
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeTapeArchivesResponse_tapeArchives
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeTapeArchives_marker
          Lens..~ rs
          Lens.^? describeTapeArchivesResponse_marker Core.. Lens._Just

instance Core.AWSRequest DescribeTapeArchives where
  type
    AWSResponse DescribeTapeArchives =
      DescribeTapeArchivesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTapeArchivesResponse'
            Core.<$> (x Core..?> "TapeArchives" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeTapeArchives

instance Core.NFData DescribeTapeArchives

instance Core.ToHeaders DescribeTapeArchives where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.DescribeTapeArchives" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeTapeArchives where
  toJSON DescribeTapeArchives' {..} =
    Core.object
      ( Core.catMaybes
          [ ("TapeARNs" Core..=) Core.<$> tapeARNs,
            ("Limit" Core..=) Core.<$> limit,
            ("Marker" Core..=) Core.<$> marker
          ]
      )

instance Core.ToPath DescribeTapeArchives where
  toPath = Core.const "/"

instance Core.ToQuery DescribeTapeArchives where
  toQuery = Core.const Core.mempty

-- | DescribeTapeArchivesOutput
--
-- /See:/ 'newDescribeTapeArchivesResponse' smart constructor.
data DescribeTapeArchivesResponse = DescribeTapeArchivesResponse'
  { -- | An array of virtual tape objects in the virtual tape shelf (VTS). The
    -- description includes of the Amazon Resource Name (ARN) of the virtual
    -- tapes. The information returned includes the Amazon Resource Names
    -- (ARNs) of the tapes, size of the tapes, status of the tapes, progress of
    -- the description, and tape barcode.
    tapeArchives :: Core.Maybe [TapeArchive],
    -- | An opaque string that indicates the position at which the virtual tapes
    -- that were fetched for description ended. Use this marker in your next
    -- request to fetch the next set of virtual tapes in the virtual tape shelf
    -- (VTS). If there are no more virtual tapes to describe, this field does
    -- not appear in the response.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeTapeArchivesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tapeArchives', 'describeTapeArchivesResponse_tapeArchives' - An array of virtual tape objects in the virtual tape shelf (VTS). The
-- description includes of the Amazon Resource Name (ARN) of the virtual
-- tapes. The information returned includes the Amazon Resource Names
-- (ARNs) of the tapes, size of the tapes, status of the tapes, progress of
-- the description, and tape barcode.
--
-- 'marker', 'describeTapeArchivesResponse_marker' - An opaque string that indicates the position at which the virtual tapes
-- that were fetched for description ended. Use this marker in your next
-- request to fetch the next set of virtual tapes in the virtual tape shelf
-- (VTS). If there are no more virtual tapes to describe, this field does
-- not appear in the response.
--
-- 'httpStatus', 'describeTapeArchivesResponse_httpStatus' - The response's http status code.
newDescribeTapeArchivesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeTapeArchivesResponse
newDescribeTapeArchivesResponse pHttpStatus_ =
  DescribeTapeArchivesResponse'
    { tapeArchives =
        Core.Nothing,
      marker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of virtual tape objects in the virtual tape shelf (VTS). The
-- description includes of the Amazon Resource Name (ARN) of the virtual
-- tapes. The information returned includes the Amazon Resource Names
-- (ARNs) of the tapes, size of the tapes, status of the tapes, progress of
-- the description, and tape barcode.
describeTapeArchivesResponse_tapeArchives :: Lens.Lens' DescribeTapeArchivesResponse (Core.Maybe [TapeArchive])
describeTapeArchivesResponse_tapeArchives = Lens.lens (\DescribeTapeArchivesResponse' {tapeArchives} -> tapeArchives) (\s@DescribeTapeArchivesResponse' {} a -> s {tapeArchives = a} :: DescribeTapeArchivesResponse) Core.. Lens.mapping Lens._Coerce

-- | An opaque string that indicates the position at which the virtual tapes
-- that were fetched for description ended. Use this marker in your next
-- request to fetch the next set of virtual tapes in the virtual tape shelf
-- (VTS). If there are no more virtual tapes to describe, this field does
-- not appear in the response.
describeTapeArchivesResponse_marker :: Lens.Lens' DescribeTapeArchivesResponse (Core.Maybe Core.Text)
describeTapeArchivesResponse_marker = Lens.lens (\DescribeTapeArchivesResponse' {marker} -> marker) (\s@DescribeTapeArchivesResponse' {} a -> s {marker = a} :: DescribeTapeArchivesResponse)

-- | The response's http status code.
describeTapeArchivesResponse_httpStatus :: Lens.Lens' DescribeTapeArchivesResponse Core.Int
describeTapeArchivesResponse_httpStatus = Lens.lens (\DescribeTapeArchivesResponse' {httpStatus} -> httpStatus) (\s@DescribeTapeArchivesResponse' {} a -> s {httpStatus = a} :: DescribeTapeArchivesResponse)

instance Core.NFData DescribeTapeArchivesResponse
