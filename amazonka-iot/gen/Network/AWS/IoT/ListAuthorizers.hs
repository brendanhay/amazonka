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
-- Module      : Network.AWS.IoT.ListAuthorizers
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the authorizers registered in your account.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListAuthorizers
  ( -- * Creating a Request
    ListAuthorizers (..),
    newListAuthorizers,

    -- * Request Lenses
    listAuthorizers_status,
    listAuthorizers_pageSize,
    listAuthorizers_ascendingOrder,
    listAuthorizers_marker,

    -- * Destructuring the Response
    ListAuthorizersResponse (..),
    newListAuthorizersResponse,

    -- * Response Lenses
    listAuthorizersResponse_nextMarker,
    listAuthorizersResponse_authorizers,
    listAuthorizersResponse_httpStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListAuthorizers' smart constructor.
data ListAuthorizers = ListAuthorizers'
  { -- | The status of the list authorizers request.
    status :: Prelude.Maybe AuthorizerStatus,
    -- | The maximum number of results to return at one time.
    pageSize :: Prelude.Maybe Prelude.Natural,
    -- | Return the list of authorizers in ascending alphabetical order.
    ascendingOrder :: Prelude.Maybe Prelude.Bool,
    -- | A marker used to get the next set of results.
    marker :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListAuthorizers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'listAuthorizers_status' - The status of the list authorizers request.
--
-- 'pageSize', 'listAuthorizers_pageSize' - The maximum number of results to return at one time.
--
-- 'ascendingOrder', 'listAuthorizers_ascendingOrder' - Return the list of authorizers in ascending alphabetical order.
--
-- 'marker', 'listAuthorizers_marker' - A marker used to get the next set of results.
newListAuthorizers ::
  ListAuthorizers
newListAuthorizers =
  ListAuthorizers'
    { status = Prelude.Nothing,
      pageSize = Prelude.Nothing,
      ascendingOrder = Prelude.Nothing,
      marker = Prelude.Nothing
    }

-- | The status of the list authorizers request.
listAuthorizers_status :: Lens.Lens' ListAuthorizers (Prelude.Maybe AuthorizerStatus)
listAuthorizers_status = Lens.lens (\ListAuthorizers' {status} -> status) (\s@ListAuthorizers' {} a -> s {status = a} :: ListAuthorizers)

-- | The maximum number of results to return at one time.
listAuthorizers_pageSize :: Lens.Lens' ListAuthorizers (Prelude.Maybe Prelude.Natural)
listAuthorizers_pageSize = Lens.lens (\ListAuthorizers' {pageSize} -> pageSize) (\s@ListAuthorizers' {} a -> s {pageSize = a} :: ListAuthorizers)

-- | Return the list of authorizers in ascending alphabetical order.
listAuthorizers_ascendingOrder :: Lens.Lens' ListAuthorizers (Prelude.Maybe Prelude.Bool)
listAuthorizers_ascendingOrder = Lens.lens (\ListAuthorizers' {ascendingOrder} -> ascendingOrder) (\s@ListAuthorizers' {} a -> s {ascendingOrder = a} :: ListAuthorizers)

-- | A marker used to get the next set of results.
listAuthorizers_marker :: Lens.Lens' ListAuthorizers (Prelude.Maybe Prelude.Text)
listAuthorizers_marker = Lens.lens (\ListAuthorizers' {marker} -> marker) (\s@ListAuthorizers' {} a -> s {marker = a} :: ListAuthorizers)

instance Pager.AWSPager ListAuthorizers where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listAuthorizersResponse_nextMarker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listAuthorizersResponse_authorizers
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listAuthorizers_marker
          Lens..~ rs
          Lens.^? listAuthorizersResponse_nextMarker
            Prelude.. Lens._Just

instance Prelude.AWSRequest ListAuthorizers where
  type Rs ListAuthorizers = ListAuthorizersResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAuthorizersResponse'
            Prelude.<$> (x Prelude..?> "nextMarker")
            Prelude.<*> ( x Prelude..?> "authorizers"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAuthorizers

instance Prelude.NFData ListAuthorizers

instance Prelude.ToHeaders ListAuthorizers where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath ListAuthorizers where
  toPath = Prelude.const "/authorizers/"

instance Prelude.ToQuery ListAuthorizers where
  toQuery ListAuthorizers' {..} =
    Prelude.mconcat
      [ "status" Prelude.=: status,
        "pageSize" Prelude.=: pageSize,
        "isAscendingOrder" Prelude.=: ascendingOrder,
        "marker" Prelude.=: marker
      ]

-- | /See:/ 'newListAuthorizersResponse' smart constructor.
data ListAuthorizersResponse = ListAuthorizersResponse'
  { -- | A marker used to get the next set of results.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | The authorizers.
    authorizers :: Prelude.Maybe [AuthorizerSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListAuthorizersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextMarker', 'listAuthorizersResponse_nextMarker' - A marker used to get the next set of results.
--
-- 'authorizers', 'listAuthorizersResponse_authorizers' - The authorizers.
--
-- 'httpStatus', 'listAuthorizersResponse_httpStatus' - The response's http status code.
newListAuthorizersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAuthorizersResponse
newListAuthorizersResponse pHttpStatus_ =
  ListAuthorizersResponse'
    { nextMarker =
        Prelude.Nothing,
      authorizers = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A marker used to get the next set of results.
listAuthorizersResponse_nextMarker :: Lens.Lens' ListAuthorizersResponse (Prelude.Maybe Prelude.Text)
listAuthorizersResponse_nextMarker = Lens.lens (\ListAuthorizersResponse' {nextMarker} -> nextMarker) (\s@ListAuthorizersResponse' {} a -> s {nextMarker = a} :: ListAuthorizersResponse)

-- | The authorizers.
listAuthorizersResponse_authorizers :: Lens.Lens' ListAuthorizersResponse (Prelude.Maybe [AuthorizerSummary])
listAuthorizersResponse_authorizers = Lens.lens (\ListAuthorizersResponse' {authorizers} -> authorizers) (\s@ListAuthorizersResponse' {} a -> s {authorizers = a} :: ListAuthorizersResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listAuthorizersResponse_httpStatus :: Lens.Lens' ListAuthorizersResponse Prelude.Int
listAuthorizersResponse_httpStatus = Lens.lens (\ListAuthorizersResponse' {httpStatus} -> httpStatus) (\s@ListAuthorizersResponse' {} a -> s {httpStatus = a} :: ListAuthorizersResponse)

instance Prelude.NFData ListAuthorizersResponse
