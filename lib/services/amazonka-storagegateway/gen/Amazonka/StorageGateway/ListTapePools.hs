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
-- Module      : Amazonka.StorageGateway.ListTapePools
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists custom tape pools. You specify custom tape pools to list by
-- specifying one or more custom tape pool Amazon Resource Names (ARNs). If
-- you don\'t specify a custom tape pool ARN, the operation lists all
-- custom tape pools.
--
-- This operation supports pagination. You can optionally specify the
-- @Limit@ parameter in the body to limit the number of tape pools in the
-- response. If the number of tape pools returned in the response is
-- truncated, the response includes a @Marker@ element that you can use in
-- your subsequent request to retrieve the next set of tape pools.
--
-- This operation returns paginated results.
module Amazonka.StorageGateway.ListTapePools
  ( -- * Creating a Request
    ListTapePools (..),
    newListTapePools,

    -- * Request Lenses
    listTapePools_limit,
    listTapePools_marker,
    listTapePools_poolARNs,

    -- * Destructuring the Response
    ListTapePoolsResponse (..),
    newListTapePoolsResponse,

    -- * Response Lenses
    listTapePoolsResponse_marker,
    listTapePoolsResponse_poolInfos,
    listTapePoolsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StorageGateway.Types

-- | /See:/ 'newListTapePools' smart constructor.
data ListTapePools = ListTapePools'
  { -- | An optional number limit for the tape pools in the list returned by this
    -- call.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | A string that indicates the position at which to begin the returned list
    -- of tape pools.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of each of the custom tape pools you want
    -- to list. If you don\'t specify a custom tape pool ARN, the response
    -- lists all custom tape pools.
    poolARNs :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTapePools' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limit', 'listTapePools_limit' - An optional number limit for the tape pools in the list returned by this
-- call.
--
-- 'marker', 'listTapePools_marker' - A string that indicates the position at which to begin the returned list
-- of tape pools.
--
-- 'poolARNs', 'listTapePools_poolARNs' - The Amazon Resource Name (ARN) of each of the custom tape pools you want
-- to list. If you don\'t specify a custom tape pool ARN, the response
-- lists all custom tape pools.
newListTapePools ::
  ListTapePools
newListTapePools =
  ListTapePools'
    { limit = Prelude.Nothing,
      marker = Prelude.Nothing,
      poolARNs = Prelude.Nothing
    }

-- | An optional number limit for the tape pools in the list returned by this
-- call.
listTapePools_limit :: Lens.Lens' ListTapePools (Prelude.Maybe Prelude.Natural)
listTapePools_limit = Lens.lens (\ListTapePools' {limit} -> limit) (\s@ListTapePools' {} a -> s {limit = a} :: ListTapePools)

-- | A string that indicates the position at which to begin the returned list
-- of tape pools.
listTapePools_marker :: Lens.Lens' ListTapePools (Prelude.Maybe Prelude.Text)
listTapePools_marker = Lens.lens (\ListTapePools' {marker} -> marker) (\s@ListTapePools' {} a -> s {marker = a} :: ListTapePools)

-- | The Amazon Resource Name (ARN) of each of the custom tape pools you want
-- to list. If you don\'t specify a custom tape pool ARN, the response
-- lists all custom tape pools.
listTapePools_poolARNs :: Lens.Lens' ListTapePools (Prelude.Maybe [Prelude.Text])
listTapePools_poolARNs = Lens.lens (\ListTapePools' {poolARNs} -> poolARNs) (\s@ListTapePools' {} a -> s {poolARNs = a} :: ListTapePools) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSPager ListTapePools where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTapePoolsResponse_marker
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listTapePoolsResponse_poolInfos
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listTapePools_marker
          Lens..~ rs
          Lens.^? listTapePoolsResponse_marker
          Prelude.. Lens._Just

instance Core.AWSRequest ListTapePools where
  type
    AWSResponse ListTapePools =
      ListTapePoolsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTapePoolsResponse'
            Prelude.<$> (x Data..?> "Marker")
            Prelude.<*> (x Data..?> "PoolInfos" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTapePools where
  hashWithSalt _salt ListTapePools' {..} =
    _salt
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` poolARNs

instance Prelude.NFData ListTapePools where
  rnf ListTapePools' {..} =
    Prelude.rnf limit
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf poolARNs

instance Data.ToHeaders ListTapePools where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StorageGateway_20130630.ListTapePools" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListTapePools where
  toJSON ListTapePools' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Limit" Data..=) Prelude.<$> limit,
            ("Marker" Data..=) Prelude.<$> marker,
            ("PoolARNs" Data..=) Prelude.<$> poolARNs
          ]
      )

instance Data.ToPath ListTapePools where
  toPath = Prelude.const "/"

instance Data.ToQuery ListTapePools where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListTapePoolsResponse' smart constructor.
data ListTapePoolsResponse = ListTapePoolsResponse'
  { -- | A string that indicates the position at which to begin the returned list
    -- of tape pools. Use the marker in your next request to continue
    -- pagination of tape pools. If there are no more tape pools to list, this
    -- element does not appear in the response body.
    marker :: Prelude.Maybe Prelude.Text,
    -- | An array of @PoolInfo@ objects, where each object describes a single
    -- custom tape pool. If there are no custom tape pools, the @PoolInfos@ is
    -- an empty array.
    poolInfos :: Prelude.Maybe [PoolInfo],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTapePoolsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listTapePoolsResponse_marker' - A string that indicates the position at which to begin the returned list
-- of tape pools. Use the marker in your next request to continue
-- pagination of tape pools. If there are no more tape pools to list, this
-- element does not appear in the response body.
--
-- 'poolInfos', 'listTapePoolsResponse_poolInfos' - An array of @PoolInfo@ objects, where each object describes a single
-- custom tape pool. If there are no custom tape pools, the @PoolInfos@ is
-- an empty array.
--
-- 'httpStatus', 'listTapePoolsResponse_httpStatus' - The response's http status code.
newListTapePoolsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTapePoolsResponse
newListTapePoolsResponse pHttpStatus_ =
  ListTapePoolsResponse'
    { marker = Prelude.Nothing,
      poolInfos = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A string that indicates the position at which to begin the returned list
-- of tape pools. Use the marker in your next request to continue
-- pagination of tape pools. If there are no more tape pools to list, this
-- element does not appear in the response body.
listTapePoolsResponse_marker :: Lens.Lens' ListTapePoolsResponse (Prelude.Maybe Prelude.Text)
listTapePoolsResponse_marker = Lens.lens (\ListTapePoolsResponse' {marker} -> marker) (\s@ListTapePoolsResponse' {} a -> s {marker = a} :: ListTapePoolsResponse)

-- | An array of @PoolInfo@ objects, where each object describes a single
-- custom tape pool. If there are no custom tape pools, the @PoolInfos@ is
-- an empty array.
listTapePoolsResponse_poolInfos :: Lens.Lens' ListTapePoolsResponse (Prelude.Maybe [PoolInfo])
listTapePoolsResponse_poolInfos = Lens.lens (\ListTapePoolsResponse' {poolInfos} -> poolInfos) (\s@ListTapePoolsResponse' {} a -> s {poolInfos = a} :: ListTapePoolsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listTapePoolsResponse_httpStatus :: Lens.Lens' ListTapePoolsResponse Prelude.Int
listTapePoolsResponse_httpStatus = Lens.lens (\ListTapePoolsResponse' {httpStatus} -> httpStatus) (\s@ListTapePoolsResponse' {} a -> s {httpStatus = a} :: ListTapePoolsResponse)

instance Prelude.NFData ListTapePoolsResponse where
  rnf ListTapePoolsResponse' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf poolInfos
      `Prelude.seq` Prelude.rnf httpStatus
