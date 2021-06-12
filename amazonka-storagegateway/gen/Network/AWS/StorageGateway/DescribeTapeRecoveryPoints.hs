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
-- Module      : Network.AWS.StorageGateway.DescribeTapeRecoveryPoints
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of virtual tape recovery points that are available for
-- the specified tape gateway.
--
-- A recovery point is a point-in-time view of a virtual tape at which all
-- the data on the virtual tape is consistent. If your gateway crashes,
-- virtual tapes that have recovery points can be recovered to a new
-- gateway. This operation is only supported in the tape gateway type.
--
-- This operation returns paginated results.
module Network.AWS.StorageGateway.DescribeTapeRecoveryPoints
  ( -- * Creating a Request
    DescribeTapeRecoveryPoints (..),
    newDescribeTapeRecoveryPoints,

    -- * Request Lenses
    describeTapeRecoveryPoints_limit,
    describeTapeRecoveryPoints_marker,
    describeTapeRecoveryPoints_gatewayARN,

    -- * Destructuring the Response
    DescribeTapeRecoveryPointsResponse (..),
    newDescribeTapeRecoveryPointsResponse,

    -- * Response Lenses
    describeTapeRecoveryPointsResponse_tapeRecoveryPointInfos,
    describeTapeRecoveryPointsResponse_gatewayARN,
    describeTapeRecoveryPointsResponse_marker,
    describeTapeRecoveryPointsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | DescribeTapeRecoveryPointsInput
--
-- /See:/ 'newDescribeTapeRecoveryPoints' smart constructor.
data DescribeTapeRecoveryPoints = DescribeTapeRecoveryPoints'
  { -- | Specifies that the number of virtual tape recovery points that are
    -- described be limited to the specified number.
    limit :: Core.Maybe Core.Natural,
    -- | An opaque string that indicates the position at which to begin
    -- describing the virtual tape recovery points.
    marker :: Core.Maybe Core.Text,
    gatewayARN :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeTapeRecoveryPoints' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limit', 'describeTapeRecoveryPoints_limit' - Specifies that the number of virtual tape recovery points that are
-- described be limited to the specified number.
--
-- 'marker', 'describeTapeRecoveryPoints_marker' - An opaque string that indicates the position at which to begin
-- describing the virtual tape recovery points.
--
-- 'gatewayARN', 'describeTapeRecoveryPoints_gatewayARN' - Undocumented member.
newDescribeTapeRecoveryPoints ::
  -- | 'gatewayARN'
  Core.Text ->
  DescribeTapeRecoveryPoints
newDescribeTapeRecoveryPoints pGatewayARN_ =
  DescribeTapeRecoveryPoints'
    { limit = Core.Nothing,
      marker = Core.Nothing,
      gatewayARN = pGatewayARN_
    }

-- | Specifies that the number of virtual tape recovery points that are
-- described be limited to the specified number.
describeTapeRecoveryPoints_limit :: Lens.Lens' DescribeTapeRecoveryPoints (Core.Maybe Core.Natural)
describeTapeRecoveryPoints_limit = Lens.lens (\DescribeTapeRecoveryPoints' {limit} -> limit) (\s@DescribeTapeRecoveryPoints' {} a -> s {limit = a} :: DescribeTapeRecoveryPoints)

-- | An opaque string that indicates the position at which to begin
-- describing the virtual tape recovery points.
describeTapeRecoveryPoints_marker :: Lens.Lens' DescribeTapeRecoveryPoints (Core.Maybe Core.Text)
describeTapeRecoveryPoints_marker = Lens.lens (\DescribeTapeRecoveryPoints' {marker} -> marker) (\s@DescribeTapeRecoveryPoints' {} a -> s {marker = a} :: DescribeTapeRecoveryPoints)

-- | Undocumented member.
describeTapeRecoveryPoints_gatewayARN :: Lens.Lens' DescribeTapeRecoveryPoints Core.Text
describeTapeRecoveryPoints_gatewayARN = Lens.lens (\DescribeTapeRecoveryPoints' {gatewayARN} -> gatewayARN) (\s@DescribeTapeRecoveryPoints' {} a -> s {gatewayARN = a} :: DescribeTapeRecoveryPoints)

instance Core.AWSPager DescribeTapeRecoveryPoints where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeTapeRecoveryPointsResponse_marker
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeTapeRecoveryPointsResponse_tapeRecoveryPointInfos
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeTapeRecoveryPoints_marker
          Lens..~ rs
          Lens.^? describeTapeRecoveryPointsResponse_marker
            Core.. Lens._Just

instance Core.AWSRequest DescribeTapeRecoveryPoints where
  type
    AWSResponse DescribeTapeRecoveryPoints =
      DescribeTapeRecoveryPointsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTapeRecoveryPointsResponse'
            Core.<$> ( x Core..?> "TapeRecoveryPointInfos"
                         Core..!@ Core.mempty
                     )
            Core.<*> (x Core..?> "GatewayARN")
            Core.<*> (x Core..?> "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeTapeRecoveryPoints

instance Core.NFData DescribeTapeRecoveryPoints

instance Core.ToHeaders DescribeTapeRecoveryPoints where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.DescribeTapeRecoveryPoints" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeTapeRecoveryPoints where
  toJSON DescribeTapeRecoveryPoints' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Limit" Core..=) Core.<$> limit,
            ("Marker" Core..=) Core.<$> marker,
            Core.Just ("GatewayARN" Core..= gatewayARN)
          ]
      )

instance Core.ToPath DescribeTapeRecoveryPoints where
  toPath = Core.const "/"

instance Core.ToQuery DescribeTapeRecoveryPoints where
  toQuery = Core.const Core.mempty

-- | DescribeTapeRecoveryPointsOutput
--
-- /See:/ 'newDescribeTapeRecoveryPointsResponse' smart constructor.
data DescribeTapeRecoveryPointsResponse = DescribeTapeRecoveryPointsResponse'
  { -- | An array of TapeRecoveryPointInfos that are available for the specified
    -- gateway.
    tapeRecoveryPointInfos :: Core.Maybe [TapeRecoveryPointInfo],
    gatewayARN :: Core.Maybe Core.Text,
    -- | An opaque string that indicates the position at which the virtual tape
    -- recovery points that were listed for description ended.
    --
    -- Use this marker in your next request to list the next set of virtual
    -- tape recovery points in the list. If there are no more recovery points
    -- to describe, this field does not appear in the response.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeTapeRecoveryPointsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tapeRecoveryPointInfos', 'describeTapeRecoveryPointsResponse_tapeRecoveryPointInfos' - An array of TapeRecoveryPointInfos that are available for the specified
-- gateway.
--
-- 'gatewayARN', 'describeTapeRecoveryPointsResponse_gatewayARN' - Undocumented member.
--
-- 'marker', 'describeTapeRecoveryPointsResponse_marker' - An opaque string that indicates the position at which the virtual tape
-- recovery points that were listed for description ended.
--
-- Use this marker in your next request to list the next set of virtual
-- tape recovery points in the list. If there are no more recovery points
-- to describe, this field does not appear in the response.
--
-- 'httpStatus', 'describeTapeRecoveryPointsResponse_httpStatus' - The response's http status code.
newDescribeTapeRecoveryPointsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeTapeRecoveryPointsResponse
newDescribeTapeRecoveryPointsResponse pHttpStatus_ =
  DescribeTapeRecoveryPointsResponse'
    { tapeRecoveryPointInfos =
        Core.Nothing,
      gatewayARN = Core.Nothing,
      marker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of TapeRecoveryPointInfos that are available for the specified
-- gateway.
describeTapeRecoveryPointsResponse_tapeRecoveryPointInfos :: Lens.Lens' DescribeTapeRecoveryPointsResponse (Core.Maybe [TapeRecoveryPointInfo])
describeTapeRecoveryPointsResponse_tapeRecoveryPointInfos = Lens.lens (\DescribeTapeRecoveryPointsResponse' {tapeRecoveryPointInfos} -> tapeRecoveryPointInfos) (\s@DescribeTapeRecoveryPointsResponse' {} a -> s {tapeRecoveryPointInfos = a} :: DescribeTapeRecoveryPointsResponse) Core.. Lens.mapping Lens._Coerce

-- | Undocumented member.
describeTapeRecoveryPointsResponse_gatewayARN :: Lens.Lens' DescribeTapeRecoveryPointsResponse (Core.Maybe Core.Text)
describeTapeRecoveryPointsResponse_gatewayARN = Lens.lens (\DescribeTapeRecoveryPointsResponse' {gatewayARN} -> gatewayARN) (\s@DescribeTapeRecoveryPointsResponse' {} a -> s {gatewayARN = a} :: DescribeTapeRecoveryPointsResponse)

-- | An opaque string that indicates the position at which the virtual tape
-- recovery points that were listed for description ended.
--
-- Use this marker in your next request to list the next set of virtual
-- tape recovery points in the list. If there are no more recovery points
-- to describe, this field does not appear in the response.
describeTapeRecoveryPointsResponse_marker :: Lens.Lens' DescribeTapeRecoveryPointsResponse (Core.Maybe Core.Text)
describeTapeRecoveryPointsResponse_marker = Lens.lens (\DescribeTapeRecoveryPointsResponse' {marker} -> marker) (\s@DescribeTapeRecoveryPointsResponse' {} a -> s {marker = a} :: DescribeTapeRecoveryPointsResponse)

-- | The response's http status code.
describeTapeRecoveryPointsResponse_httpStatus :: Lens.Lens' DescribeTapeRecoveryPointsResponse Core.Int
describeTapeRecoveryPointsResponse_httpStatus = Lens.lens (\DescribeTapeRecoveryPointsResponse' {httpStatus} -> httpStatus) (\s@DescribeTapeRecoveryPointsResponse' {} a -> s {httpStatus = a} :: DescribeTapeRecoveryPointsResponse)

instance
  Core.NFData
    DescribeTapeRecoveryPointsResponse
