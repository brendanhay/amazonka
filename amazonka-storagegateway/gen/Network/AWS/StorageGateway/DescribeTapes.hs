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
-- Module      : Network.AWS.StorageGateway.DescribeTapes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a description of the specified Amazon Resource Name (ARN) of
-- virtual tapes. If a @TapeARN@ is not specified, returns a description of
-- all virtual tapes associated with the specified gateway. This operation
-- is only supported in the tape gateway type.
--
-- This operation returns paginated results.
module Network.AWS.StorageGateway.DescribeTapes
  ( -- * Creating a Request
    DescribeTapes (..),
    newDescribeTapes,

    -- * Request Lenses
    describeTapes_tapeARNs,
    describeTapes_limit,
    describeTapes_marker,
    describeTapes_gatewayARN,

    -- * Destructuring the Response
    DescribeTapesResponse (..),
    newDescribeTapesResponse,

    -- * Response Lenses
    describeTapesResponse_tapes,
    describeTapesResponse_marker,
    describeTapesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | DescribeTapesInput
--
-- /See:/ 'newDescribeTapes' smart constructor.
data DescribeTapes = DescribeTapes'
  { -- | Specifies one or more unique Amazon Resource Names (ARNs) that represent
    -- the virtual tapes you want to describe. If this parameter is not
    -- specified, Tape gateway returns a description of all virtual tapes
    -- associated with the specified gateway.
    tapeARNs :: Core.Maybe [Core.Text],
    -- | Specifies that the number of virtual tapes described be limited to the
    -- specified number.
    --
    -- Amazon Web Services may impose its own limit, if this field is not set.
    limit :: Core.Maybe Core.Natural,
    -- | A marker value, obtained in a previous call to @DescribeTapes@. This
    -- marker indicates which page of results to retrieve.
    --
    -- If not specified, the first page of results is retrieved.
    marker :: Core.Maybe Core.Text,
    gatewayARN :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeTapes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tapeARNs', 'describeTapes_tapeARNs' - Specifies one or more unique Amazon Resource Names (ARNs) that represent
-- the virtual tapes you want to describe. If this parameter is not
-- specified, Tape gateway returns a description of all virtual tapes
-- associated with the specified gateway.
--
-- 'limit', 'describeTapes_limit' - Specifies that the number of virtual tapes described be limited to the
-- specified number.
--
-- Amazon Web Services may impose its own limit, if this field is not set.
--
-- 'marker', 'describeTapes_marker' - A marker value, obtained in a previous call to @DescribeTapes@. This
-- marker indicates which page of results to retrieve.
--
-- If not specified, the first page of results is retrieved.
--
-- 'gatewayARN', 'describeTapes_gatewayARN' - Undocumented member.
newDescribeTapes ::
  -- | 'gatewayARN'
  Core.Text ->
  DescribeTapes
newDescribeTapes pGatewayARN_ =
  DescribeTapes'
    { tapeARNs = Core.Nothing,
      limit = Core.Nothing,
      marker = Core.Nothing,
      gatewayARN = pGatewayARN_
    }

-- | Specifies one or more unique Amazon Resource Names (ARNs) that represent
-- the virtual tapes you want to describe. If this parameter is not
-- specified, Tape gateway returns a description of all virtual tapes
-- associated with the specified gateway.
describeTapes_tapeARNs :: Lens.Lens' DescribeTapes (Core.Maybe [Core.Text])
describeTapes_tapeARNs = Lens.lens (\DescribeTapes' {tapeARNs} -> tapeARNs) (\s@DescribeTapes' {} a -> s {tapeARNs = a} :: DescribeTapes) Core.. Lens.mapping Lens._Coerce

-- | Specifies that the number of virtual tapes described be limited to the
-- specified number.
--
-- Amazon Web Services may impose its own limit, if this field is not set.
describeTapes_limit :: Lens.Lens' DescribeTapes (Core.Maybe Core.Natural)
describeTapes_limit = Lens.lens (\DescribeTapes' {limit} -> limit) (\s@DescribeTapes' {} a -> s {limit = a} :: DescribeTapes)

-- | A marker value, obtained in a previous call to @DescribeTapes@. This
-- marker indicates which page of results to retrieve.
--
-- If not specified, the first page of results is retrieved.
describeTapes_marker :: Lens.Lens' DescribeTapes (Core.Maybe Core.Text)
describeTapes_marker = Lens.lens (\DescribeTapes' {marker} -> marker) (\s@DescribeTapes' {} a -> s {marker = a} :: DescribeTapes)

-- | Undocumented member.
describeTapes_gatewayARN :: Lens.Lens' DescribeTapes Core.Text
describeTapes_gatewayARN = Lens.lens (\DescribeTapes' {gatewayARN} -> gatewayARN) (\s@DescribeTapes' {} a -> s {gatewayARN = a} :: DescribeTapes)

instance Core.AWSPager DescribeTapes where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeTapesResponse_marker Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeTapesResponse_tapes Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeTapes_marker
          Lens..~ rs
          Lens.^? describeTapesResponse_marker Core.. Lens._Just

instance Core.AWSRequest DescribeTapes where
  type
    AWSResponse DescribeTapes =
      DescribeTapesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTapesResponse'
            Core.<$> (x Core..?> "Tapes" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeTapes

instance Core.NFData DescribeTapes

instance Core.ToHeaders DescribeTapes where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.DescribeTapes" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeTapes where
  toJSON DescribeTapes' {..} =
    Core.object
      ( Core.catMaybes
          [ ("TapeARNs" Core..=) Core.<$> tapeARNs,
            ("Limit" Core..=) Core.<$> limit,
            ("Marker" Core..=) Core.<$> marker,
            Core.Just ("GatewayARN" Core..= gatewayARN)
          ]
      )

instance Core.ToPath DescribeTapes where
  toPath = Core.const "/"

instance Core.ToQuery DescribeTapes where
  toQuery = Core.const Core.mempty

-- | DescribeTapesOutput
--
-- /See:/ 'newDescribeTapesResponse' smart constructor.
data DescribeTapesResponse = DescribeTapesResponse'
  { -- | An array of virtual tape descriptions.
    tapes :: Core.Maybe [Tape],
    -- | An opaque string which can be used as part of a subsequent DescribeTapes
    -- call to retrieve the next page of results.
    --
    -- If a response does not contain a marker, then there are no more results
    -- to be retrieved.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeTapesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tapes', 'describeTapesResponse_tapes' - An array of virtual tape descriptions.
--
-- 'marker', 'describeTapesResponse_marker' - An opaque string which can be used as part of a subsequent DescribeTapes
-- call to retrieve the next page of results.
--
-- If a response does not contain a marker, then there are no more results
-- to be retrieved.
--
-- 'httpStatus', 'describeTapesResponse_httpStatus' - The response's http status code.
newDescribeTapesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeTapesResponse
newDescribeTapesResponse pHttpStatus_ =
  DescribeTapesResponse'
    { tapes = Core.Nothing,
      marker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of virtual tape descriptions.
describeTapesResponse_tapes :: Lens.Lens' DescribeTapesResponse (Core.Maybe [Tape])
describeTapesResponse_tapes = Lens.lens (\DescribeTapesResponse' {tapes} -> tapes) (\s@DescribeTapesResponse' {} a -> s {tapes = a} :: DescribeTapesResponse) Core.. Lens.mapping Lens._Coerce

-- | An opaque string which can be used as part of a subsequent DescribeTapes
-- call to retrieve the next page of results.
--
-- If a response does not contain a marker, then there are no more results
-- to be retrieved.
describeTapesResponse_marker :: Lens.Lens' DescribeTapesResponse (Core.Maybe Core.Text)
describeTapesResponse_marker = Lens.lens (\DescribeTapesResponse' {marker} -> marker) (\s@DescribeTapesResponse' {} a -> s {marker = a} :: DescribeTapesResponse)

-- | The response's http status code.
describeTapesResponse_httpStatus :: Lens.Lens' DescribeTapesResponse Core.Int
describeTapesResponse_httpStatus = Lens.lens (\DescribeTapesResponse' {httpStatus} -> httpStatus) (\s@DescribeTapesResponse' {} a -> s {httpStatus = a} :: DescribeTapesResponse)

instance Core.NFData DescribeTapesResponse
