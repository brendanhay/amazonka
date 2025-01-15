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
-- Module      : Amazonka.StorageGateway.DescribeTapes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a description of the specified Amazon Resource Name (ARN) of
-- virtual tapes. If a @TapeARN@ is not specified, returns a description of
-- all virtual tapes associated with the specified gateway. This operation
-- is only supported in the tape gateway type.
--
-- This operation returns paginated results.
module Amazonka.StorageGateway.DescribeTapes
  ( -- * Creating a Request
    DescribeTapes (..),
    newDescribeTapes,

    -- * Request Lenses
    describeTapes_limit,
    describeTapes_marker,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StorageGateway.Types

-- | DescribeTapesInput
--
-- /See:/ 'newDescribeTapes' smart constructor.
data DescribeTapes = DescribeTapes'
  { -- | Specifies that the number of virtual tapes described be limited to the
    -- specified number.
    --
    -- Amazon Web Services may impose its own limit, if this field is not set.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | A marker value, obtained in a previous call to @DescribeTapes@. This
    -- marker indicates which page of results to retrieve.
    --
    -- If not specified, the first page of results is retrieved.
    marker :: Prelude.Maybe Prelude.Text,
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
    { limit = Prelude.Nothing,
      marker = Prelude.Nothing,
      tapeARNs = Prelude.Nothing,
      gatewayARN = pGatewayARN_
    }

-- | Specifies that the number of virtual tapes described be limited to the
-- specified number.
--
-- Amazon Web Services may impose its own limit, if this field is not set.
describeTapes_limit :: Lens.Lens' DescribeTapes (Prelude.Maybe Prelude.Natural)
describeTapes_limit = Lens.lens (\DescribeTapes' {limit} -> limit) (\s@DescribeTapes' {} a -> s {limit = a} :: DescribeTapes)

-- | A marker value, obtained in a previous call to @DescribeTapes@. This
-- marker indicates which page of results to retrieve.
--
-- If not specified, the first page of results is retrieved.
describeTapes_marker :: Lens.Lens' DescribeTapes (Prelude.Maybe Prelude.Text)
describeTapes_marker = Lens.lens (\DescribeTapes' {marker} -> marker) (\s@DescribeTapes' {} a -> s {marker = a} :: DescribeTapes)

-- | Specifies one or more unique Amazon Resource Names (ARNs) that represent
-- the virtual tapes you want to describe. If this parameter is not
-- specified, Tape gateway returns a description of all virtual tapes
-- associated with the specified gateway.
describeTapes_tapeARNs :: Lens.Lens' DescribeTapes (Prelude.Maybe [Prelude.Text])
describeTapes_tapeARNs = Lens.lens (\DescribeTapes' {tapeARNs} -> tapeARNs) (\s@DescribeTapes' {} a -> s {tapeARNs = a} :: DescribeTapes) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
describeTapes_gatewayARN :: Lens.Lens' DescribeTapes Prelude.Text
describeTapes_gatewayARN = Lens.lens (\DescribeTapes' {gatewayARN} -> gatewayARN) (\s@DescribeTapes' {} a -> s {gatewayARN = a} :: DescribeTapes)

instance Core.AWSPager DescribeTapes where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeTapesResponse_marker
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeTapesResponse_tapes
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& describeTapes_marker
              Lens..~ rs
              Lens.^? describeTapesResponse_marker
              Prelude.. Lens._Just

instance Core.AWSRequest DescribeTapes where
  type
    AWSResponse DescribeTapes =
      DescribeTapesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTapesResponse'
            Prelude.<$> (x Data..?> "Marker")
            Prelude.<*> (x Data..?> "Tapes" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeTapes where
  hashWithSalt _salt DescribeTapes' {..} =
    _salt
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` tapeARNs
      `Prelude.hashWithSalt` gatewayARN

instance Prelude.NFData DescribeTapes where
  rnf DescribeTapes' {..} =
    Prelude.rnf limit `Prelude.seq`
      Prelude.rnf marker `Prelude.seq`
        Prelude.rnf tapeARNs `Prelude.seq`
          Prelude.rnf gatewayARN

instance Data.ToHeaders DescribeTapes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StorageGateway_20130630.DescribeTapes" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeTapes where
  toJSON DescribeTapes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Limit" Data..=) Prelude.<$> limit,
            ("Marker" Data..=) Prelude.<$> marker,
            ("TapeARNs" Data..=) Prelude.<$> tapeARNs,
            Prelude.Just ("GatewayARN" Data..= gatewayARN)
          ]
      )

instance Data.ToPath DescribeTapes where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeTapes where
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
describeTapesResponse_tapes = Lens.lens (\DescribeTapesResponse' {tapes} -> tapes) (\s@DescribeTapesResponse' {} a -> s {tapes = a} :: DescribeTapesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeTapesResponse_httpStatus :: Lens.Lens' DescribeTapesResponse Prelude.Int
describeTapesResponse_httpStatus = Lens.lens (\DescribeTapesResponse' {httpStatus} -> httpStatus) (\s@DescribeTapesResponse' {} a -> s {httpStatus = a} :: DescribeTapesResponse)

instance Prelude.NFData DescribeTapesResponse where
  rnf DescribeTapesResponse' {..} =
    Prelude.rnf marker `Prelude.seq`
      Prelude.rnf tapes `Prelude.seq`
        Prelude.rnf httpStatus
