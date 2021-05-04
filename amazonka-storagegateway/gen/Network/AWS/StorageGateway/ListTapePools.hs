{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.StorageGateway.ListTapePools
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.StorageGateway.ListTapePools
  ( -- * Creating a Request
    ListTapePools (..),
    newListTapePools,

    -- * Request Lenses
    listTapePools_limit,
    listTapePools_poolARNs,
    listTapePools_marker,

    -- * Destructuring the Response
    ListTapePoolsResponse (..),
    newListTapePoolsResponse,

    -- * Response Lenses
    listTapePoolsResponse_poolInfos,
    listTapePoolsResponse_marker,
    listTapePoolsResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | /See:/ 'newListTapePools' smart constructor.
data ListTapePools = ListTapePools'
  { -- | An optional number limit for the tape pools in the list returned by this
    -- call.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name (ARN) of each of the custom tape pools you want
    -- to list. If you don\'t specify a custom tape pool ARN, the response
    -- lists all custom tape pools.
    poolARNs :: Prelude.Maybe [Prelude.Text],
    -- | A string that indicates the position at which to begin the returned list
    -- of tape pools.
    marker :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'poolARNs', 'listTapePools_poolARNs' - The Amazon Resource Name (ARN) of each of the custom tape pools you want
-- to list. If you don\'t specify a custom tape pool ARN, the response
-- lists all custom tape pools.
--
-- 'marker', 'listTapePools_marker' - A string that indicates the position at which to begin the returned list
-- of tape pools.
newListTapePools ::
  ListTapePools
newListTapePools =
  ListTapePools'
    { limit = Prelude.Nothing,
      poolARNs = Prelude.Nothing,
      marker = Prelude.Nothing
    }

-- | An optional number limit for the tape pools in the list returned by this
-- call.
listTapePools_limit :: Lens.Lens' ListTapePools (Prelude.Maybe Prelude.Natural)
listTapePools_limit = Lens.lens (\ListTapePools' {limit} -> limit) (\s@ListTapePools' {} a -> s {limit = a} :: ListTapePools)

-- | The Amazon Resource Name (ARN) of each of the custom tape pools you want
-- to list. If you don\'t specify a custom tape pool ARN, the response
-- lists all custom tape pools.
listTapePools_poolARNs :: Lens.Lens' ListTapePools (Prelude.Maybe [Prelude.Text])
listTapePools_poolARNs = Lens.lens (\ListTapePools' {poolARNs} -> poolARNs) (\s@ListTapePools' {} a -> s {poolARNs = a} :: ListTapePools) Prelude.. Lens.mapping Prelude._Coerce

-- | A string that indicates the position at which to begin the returned list
-- of tape pools.
listTapePools_marker :: Lens.Lens' ListTapePools (Prelude.Maybe Prelude.Text)
listTapePools_marker = Lens.lens (\ListTapePools' {marker} -> marker) (\s@ListTapePools' {} a -> s {marker = a} :: ListTapePools)

instance Pager.AWSPager ListTapePools where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listTapePoolsResponse_marker Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listTapePoolsResponse_poolInfos Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listTapePools_marker
          Lens..~ rs
          Lens.^? listTapePoolsResponse_marker Prelude.. Lens._Just

instance Prelude.AWSRequest ListTapePools where
  type Rs ListTapePools = ListTapePoolsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTapePoolsResponse'
            Prelude.<$> ( x Prelude..?> "PoolInfos"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (x Prelude..?> "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTapePools

instance Prelude.NFData ListTapePools

instance Prelude.ToHeaders ListTapePools where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "StorageGateway_20130630.ListTapePools" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ListTapePools where
  toJSON ListTapePools' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Limit" Prelude..=) Prelude.<$> limit,
            ("PoolARNs" Prelude..=) Prelude.<$> poolARNs,
            ("Marker" Prelude..=) Prelude.<$> marker
          ]
      )

instance Prelude.ToPath ListTapePools where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListTapePools where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListTapePoolsResponse' smart constructor.
data ListTapePoolsResponse = ListTapePoolsResponse'
  { -- | An array of @PoolInfo@ objects, where each object describes a single
    -- custom tape pool. If there are no custom tape pools, the @PoolInfos@ is
    -- an empty array.
    poolInfos :: Prelude.Maybe [PoolInfo],
    -- | A string that indicates the position at which to begin the returned list
    -- of tape pools. Use the marker in your next request to continue
    -- pagination of tape pools. If there are no more tape pools to list, this
    -- element does not appear in the response body.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListTapePoolsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'poolInfos', 'listTapePoolsResponse_poolInfos' - An array of @PoolInfo@ objects, where each object describes a single
-- custom tape pool. If there are no custom tape pools, the @PoolInfos@ is
-- an empty array.
--
-- 'marker', 'listTapePoolsResponse_marker' - A string that indicates the position at which to begin the returned list
-- of tape pools. Use the marker in your next request to continue
-- pagination of tape pools. If there are no more tape pools to list, this
-- element does not appear in the response body.
--
-- 'httpStatus', 'listTapePoolsResponse_httpStatus' - The response's http status code.
newListTapePoolsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTapePoolsResponse
newListTapePoolsResponse pHttpStatus_ =
  ListTapePoolsResponse'
    { poolInfos = Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of @PoolInfo@ objects, where each object describes a single
-- custom tape pool. If there are no custom tape pools, the @PoolInfos@ is
-- an empty array.
listTapePoolsResponse_poolInfos :: Lens.Lens' ListTapePoolsResponse (Prelude.Maybe [PoolInfo])
listTapePoolsResponse_poolInfos = Lens.lens (\ListTapePoolsResponse' {poolInfos} -> poolInfos) (\s@ListTapePoolsResponse' {} a -> s {poolInfos = a} :: ListTapePoolsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | A string that indicates the position at which to begin the returned list
-- of tape pools. Use the marker in your next request to continue
-- pagination of tape pools. If there are no more tape pools to list, this
-- element does not appear in the response body.
listTapePoolsResponse_marker :: Lens.Lens' ListTapePoolsResponse (Prelude.Maybe Prelude.Text)
listTapePoolsResponse_marker = Lens.lens (\ListTapePoolsResponse' {marker} -> marker) (\s@ListTapePoolsResponse' {} a -> s {marker = a} :: ListTapePoolsResponse)

-- | The response's http status code.
listTapePoolsResponse_httpStatus :: Lens.Lens' ListTapePoolsResponse Prelude.Int
listTapePoolsResponse_httpStatus = Lens.lens (\ListTapePoolsResponse' {httpStatus} -> httpStatus) (\s@ListTapePoolsResponse' {} a -> s {httpStatus = a} :: ListTapePoolsResponse)

instance Prelude.NFData ListTapePoolsResponse
