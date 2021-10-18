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
    describeTapes_marker,
    describeTapes_limit,
    describeTapes_tapeARNs,
    describeTapes_gatewayARN,

    -- * Destructuring the Response
    DescribeTapesResponse (..),
    newDescribeTapesResponse,

    -- * Response Lenses
    describeTapesResponse_marker,
    describeTapesResponse_tapes,
    describeTapesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | DescribeTapesInput
--
-- /See:/ 'newDescribeTapes' smart constructor.
data DescribeTapes = DescribeTapes'
  { -- | A marker value, obtained in a previous call to @DescribeTapes@. This
    -- marker indicates which page of results to retrieve.
    --
    -- If not specified, the first page of results is retrieved.
    marker :: Prelude.Maybe Prelude.Text,
    -- | Specifies that the number of virtual tapes described be limited to the
    -- specified number.
    --
    -- Amazon Web Services may impose its own limit, if this field is not set.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | Specifies one or more unique Amazon Resource Names (ARNs) that represent
    -- the virtual tapes you want to describe. If this parameter is not
    -- specified, Tape gateway returns a description of all virtual tapes
    -- associated with the specified gateway.
    tapeARNs :: Prelude.Maybe [Prelude.Text],
    gatewayARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTapes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'describeTapes_marker' - A marker value, obtained in a previous call to @DescribeTapes@. This
-- marker indicates which page of results to retrieve.
--
-- If not specified, the first page of results is retrieved.
--
-- 'limit', 'describeTapes_limit' - Specifies that the number of virtual tapes described be limited to the
-- specified number.
--
-- Amazon Web Services may impose its own limit, if this field is not set.
--
-- 'tapeARNs', 'describeTapes_tapeARNs' - Specifies one or more unique Amazon Resource Names (ARNs) that represent
-- the virtual tapes you want to describe. If this parameter is not
-- specified, Tape gateway returns a description of all virtual tapes
-- associated with the specified gateway.
--
-- 'gatewayARN', 'describeTapes_gatewayARN' - Undocumented member.
newDescribeTapes ::
  -- | 'gatewayARN'
  Prelude.Text ->
  DescribeTapes
newDescribeTapes pGatewayARN_ =
  DescribeTapes'
    { marker = Prelude.Nothing,
      limit = Prelude.Nothing,
      tapeARNs = Prelude.Nothing,
      gatewayARN = pGatewayARN_
    }

-- | A marker value, obtained in a previous call to @DescribeTapes@. This
-- marker indicates which page of results to retrieve.
--
-- If not specified, the first page of results is retrieved.
describeTapes_marker :: Lens.Lens' DescribeTapes (Prelude.Maybe Prelude.Text)
describeTapes_marker = Lens.lens (\DescribeTapes' {marker} -> marker) (\s@DescribeTapes' {} a -> s {marker = a} :: DescribeTapes)

-- | Specifies that the number of virtual tapes described be limited to the
-- specified number.
--
-- Amazon Web Services may impose its own limit, if this field is not set.
describeTapes_limit :: Lens.Lens' DescribeTapes (Prelude.Maybe Prelude.Natural)
describeTapes_limit = Lens.lens (\DescribeTapes' {limit} -> limit) (\s@DescribeTapes' {} a -> s {limit = a} :: DescribeTapes)

-- | Specifies one or more unique Amazon Resource Names (ARNs) that represent
-- the virtual tapes you want to describe. If this parameter is not
-- specified, Tape gateway returns a description of all virtual tapes
-- associated with the specified gateway.
describeTapes_tapeARNs :: Lens.Lens' DescribeTapes (Prelude.Maybe [Prelude.Text])
describeTapes_tapeARNs = Lens.lens (\DescribeTapes' {tapeARNs} -> tapeARNs) (\s@DescribeTapes' {} a -> s {tapeARNs = a} :: DescribeTapes) Prelude.. Lens.mapping Lens._Coerce

-- | Undocumented member.
describeTapes_gatewayARN :: Lens.Lens' DescribeTapes Prelude.Text
describeTapes_gatewayARN = Lens.lens (\DescribeTapes' {gatewayARN} -> gatewayARN) (\s@DescribeTapes' {} a -> s {gatewayARN = a} :: DescribeTapes)

instance Core.AWSPager DescribeTapes where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeTapesResponse_marker Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeTapesResponse_tapes Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeTapes_marker
          Lens..~ rs
          Lens.^? describeTapesResponse_marker Prelude.. Lens._Just

instance Core.AWSRequest DescribeTapes where
  type
    AWSResponse DescribeTapes =
      DescribeTapesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTapesResponse'
            Prelude.<$> (x Core..?> "Marker")
            Prelude.<*> (x Core..?> "Tapes" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeTapes

instance Prelude.NFData DescribeTapes

instance Core.ToHeaders DescribeTapes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.DescribeTapes" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeTapes where
  toJSON DescribeTapes' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Marker" Core..=) Prelude.<$> marker,
            ("Limit" Core..=) Prelude.<$> limit,
            ("TapeARNs" Core..=) Prelude.<$> tapeARNs,
            Prelude.Just ("GatewayARN" Core..= gatewayARN)
          ]
      )

instance Core.ToPath DescribeTapes where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeTapes where
  toQuery = Prelude.const Prelude.mempty

-- | DescribeTapesOutput
--
-- /See:/ 'newDescribeTapesResponse' smart constructor.
data DescribeTapesResponse = DescribeTapesResponse'
  { -- | An opaque string that can be used as part of a subsequent
    -- @DescribeTapes@ call to retrieve the next page of results.
    --
    -- If a response does not contain a marker, then there are no more results
    -- to be retrieved.
    marker :: Prelude.Maybe Prelude.Text,
    -- | An array of virtual tape descriptions.
    tapes :: Prelude.Maybe [Tape],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTapesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'describeTapesResponse_marker' - An opaque string that can be used as part of a subsequent
-- @DescribeTapes@ call to retrieve the next page of results.
--
-- If a response does not contain a marker, then there are no more results
-- to be retrieved.
--
-- 'tapes', 'describeTapesResponse_tapes' - An array of virtual tape descriptions.
--
-- 'httpStatus', 'describeTapesResponse_httpStatus' - The response's http status code.
newDescribeTapesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeTapesResponse
newDescribeTapesResponse pHttpStatus_ =
  DescribeTapesResponse'
    { marker = Prelude.Nothing,
      tapes = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An opaque string that can be used as part of a subsequent
-- @DescribeTapes@ call to retrieve the next page of results.
--
-- If a response does not contain a marker, then there are no more results
-- to be retrieved.
describeTapesResponse_marker :: Lens.Lens' DescribeTapesResponse (Prelude.Maybe Prelude.Text)
describeTapesResponse_marker = Lens.lens (\DescribeTapesResponse' {marker} -> marker) (\s@DescribeTapesResponse' {} a -> s {marker = a} :: DescribeTapesResponse)

-- | An array of virtual tape descriptions.
describeTapesResponse_tapes :: Lens.Lens' DescribeTapesResponse (Prelude.Maybe [Tape])
describeTapesResponse_tapes = Lens.lens (\DescribeTapesResponse' {tapes} -> tapes) (\s@DescribeTapesResponse' {} a -> s {tapes = a} :: DescribeTapesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeTapesResponse_httpStatus :: Lens.Lens' DescribeTapesResponse Prelude.Int
describeTapesResponse_httpStatus = Lens.lens (\DescribeTapesResponse' {httpStatus} -> httpStatus) (\s@DescribeTapesResponse' {} a -> s {httpStatus = a} :: DescribeTapesResponse)

instance Prelude.NFData DescribeTapesResponse
