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
-- Module      : Amazonka.StorageGateway.ListTapes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists virtual tapes in your virtual tape library (VTL) and your virtual
-- tape shelf (VTS). You specify the tapes to list by specifying one or
-- more tape Amazon Resource Names (ARNs). If you don\'t specify a tape
-- ARN, the operation lists all virtual tapes in both your VTL and VTS.
--
-- This operation supports pagination. By default, the operation returns a
-- maximum of up to 100 tapes. You can optionally specify the @Limit@
-- parameter in the body to limit the number of tapes in the response. If
-- the number of tapes returned in the response is truncated, the response
-- includes a @Marker@ element that you can use in your subsequent request
-- to retrieve the next set of tapes. This operation is only supported in
-- the tape gateway type.
--
-- This operation returns paginated results.
module Amazonka.StorageGateway.ListTapes
  ( -- * Creating a Request
    ListTapes (..),
    newListTapes,

    -- * Request Lenses
    listTapes_tapeARNs,
    listTapes_marker,
    listTapes_limit,

    -- * Destructuring the Response
    ListTapesResponse (..),
    newListTapesResponse,

    -- * Response Lenses
    listTapesResponse_marker,
    listTapesResponse_tapeInfos,
    listTapesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StorageGateway.Types

-- | A JSON object that contains one or more of the following fields:
--
-- -   ListTapesInput$Limit
--
-- -   ListTapesInput$Marker
--
-- -   ListTapesInput$TapeARNs
--
-- /See:/ 'newListTapes' smart constructor.
data ListTapes = ListTapes'
  { tapeARNs :: Prelude.Maybe [Prelude.Text],
    -- | A string that indicates the position at which to begin the returned list
    -- of tapes.
    marker :: Prelude.Maybe Prelude.Text,
    -- | An optional number limit for the tapes in the list returned by this
    -- call.
    limit :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTapes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tapeARNs', 'listTapes_tapeARNs' - Undocumented member.
--
-- 'marker', 'listTapes_marker' - A string that indicates the position at which to begin the returned list
-- of tapes.
--
-- 'limit', 'listTapes_limit' - An optional number limit for the tapes in the list returned by this
-- call.
newListTapes ::
  ListTapes
newListTapes =
  ListTapes'
    { tapeARNs = Prelude.Nothing,
      marker = Prelude.Nothing,
      limit = Prelude.Nothing
    }

-- | Undocumented member.
listTapes_tapeARNs :: Lens.Lens' ListTapes (Prelude.Maybe [Prelude.Text])
listTapes_tapeARNs = Lens.lens (\ListTapes' {tapeARNs} -> tapeARNs) (\s@ListTapes' {} a -> s {tapeARNs = a} :: ListTapes) Prelude.. Lens.mapping Lens.coerced

-- | A string that indicates the position at which to begin the returned list
-- of tapes.
listTapes_marker :: Lens.Lens' ListTapes (Prelude.Maybe Prelude.Text)
listTapes_marker = Lens.lens (\ListTapes' {marker} -> marker) (\s@ListTapes' {} a -> s {marker = a} :: ListTapes)

-- | An optional number limit for the tapes in the list returned by this
-- call.
listTapes_limit :: Lens.Lens' ListTapes (Prelude.Maybe Prelude.Natural)
listTapes_limit = Lens.lens (\ListTapes' {limit} -> limit) (\s@ListTapes' {} a -> s {limit = a} :: ListTapes)

instance Core.AWSPager ListTapes where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTapesResponse_marker Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listTapesResponse_tapeInfos Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listTapes_marker
          Lens..~ rs
          Lens.^? listTapesResponse_marker Prelude.. Lens._Just

instance Core.AWSRequest ListTapes where
  type AWSResponse ListTapes = ListTapesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTapesResponse'
            Prelude.<$> (x Core..?> "Marker")
            Prelude.<*> (x Core..?> "TapeInfos" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTapes where
  hashWithSalt _salt ListTapes' {..} =
    _salt `Prelude.hashWithSalt` tapeARNs
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` limit

instance Prelude.NFData ListTapes where
  rnf ListTapes' {..} =
    Prelude.rnf tapeARNs
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf limit

instance Core.ToHeaders ListTapes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.ListTapes" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListTapes where
  toJSON ListTapes' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("TapeARNs" Core..=) Prelude.<$> tapeARNs,
            ("Marker" Core..=) Prelude.<$> marker,
            ("Limit" Core..=) Prelude.<$> limit
          ]
      )

instance Core.ToPath ListTapes where
  toPath = Prelude.const "/"

instance Core.ToQuery ListTapes where
  toQuery = Prelude.const Prelude.mempty

-- | A JSON object containing the following fields:
--
-- -   ListTapesOutput$Marker
--
-- -   ListTapesOutput$VolumeInfos
--
-- /See:/ 'newListTapesResponse' smart constructor.
data ListTapesResponse = ListTapesResponse'
  { -- | A string that indicates the position at which to begin returning the
    -- next list of tapes. Use the marker in your next request to continue
    -- pagination of tapes. If there are no more tapes to list, this element
    -- does not appear in the response body.
    marker :: Prelude.Maybe Prelude.Text,
    tapeInfos :: Prelude.Maybe [TapeInfo],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTapesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listTapesResponse_marker' - A string that indicates the position at which to begin returning the
-- next list of tapes. Use the marker in your next request to continue
-- pagination of tapes. If there are no more tapes to list, this element
-- does not appear in the response body.
--
-- 'tapeInfos', 'listTapesResponse_tapeInfos' - Undocumented member.
--
-- 'httpStatus', 'listTapesResponse_httpStatus' - The response's http status code.
newListTapesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTapesResponse
newListTapesResponse pHttpStatus_ =
  ListTapesResponse'
    { marker = Prelude.Nothing,
      tapeInfos = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A string that indicates the position at which to begin returning the
-- next list of tapes. Use the marker in your next request to continue
-- pagination of tapes. If there are no more tapes to list, this element
-- does not appear in the response body.
listTapesResponse_marker :: Lens.Lens' ListTapesResponse (Prelude.Maybe Prelude.Text)
listTapesResponse_marker = Lens.lens (\ListTapesResponse' {marker} -> marker) (\s@ListTapesResponse' {} a -> s {marker = a} :: ListTapesResponse)

-- | Undocumented member.
listTapesResponse_tapeInfos :: Lens.Lens' ListTapesResponse (Prelude.Maybe [TapeInfo])
listTapesResponse_tapeInfos = Lens.lens (\ListTapesResponse' {tapeInfos} -> tapeInfos) (\s@ListTapesResponse' {} a -> s {tapeInfos = a} :: ListTapesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listTapesResponse_httpStatus :: Lens.Lens' ListTapesResponse Prelude.Int
listTapesResponse_httpStatus = Lens.lens (\ListTapesResponse' {httpStatus} -> httpStatus) (\s@ListTapesResponse' {} a -> s {httpStatus = a} :: ListTapesResponse)

instance Prelude.NFData ListTapesResponse where
  rnf ListTapesResponse' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf tapeInfos
      `Prelude.seq` Prelude.rnf httpStatus
