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
-- Module      : Amazonka.DirectoryService.ListIpRoutes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the address blocks that you have added to a directory.
--
-- This operation returns paginated results.
module Amazonka.DirectoryService.ListIpRoutes
  ( -- * Creating a Request
    ListIpRoutes (..),
    newListIpRoutes,

    -- * Request Lenses
    listIpRoutes_limit,
    listIpRoutes_nextToken,
    listIpRoutes_directoryId,

    -- * Destructuring the Response
    ListIpRoutesResponse (..),
    newListIpRoutesResponse,

    -- * Response Lenses
    listIpRoutesResponse_ipRoutesInfo,
    listIpRoutesResponse_nextToken,
    listIpRoutesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectoryService.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListIpRoutes' smart constructor.
data ListIpRoutes = ListIpRoutes'
  { -- | Maximum number of items to return. If this value is zero, the maximum
    -- number of items is specified by the limitations of the operation.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The /ListIpRoutes.NextToken/ value from a previous call to ListIpRoutes.
    -- Pass null if this is the first call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Identifier (ID) of the directory for which you want to retrieve the IP
    -- addresses.
    directoryId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListIpRoutes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limit', 'listIpRoutes_limit' - Maximum number of items to return. If this value is zero, the maximum
-- number of items is specified by the limitations of the operation.
--
-- 'nextToken', 'listIpRoutes_nextToken' - The /ListIpRoutes.NextToken/ value from a previous call to ListIpRoutes.
-- Pass null if this is the first call.
--
-- 'directoryId', 'listIpRoutes_directoryId' - Identifier (ID) of the directory for which you want to retrieve the IP
-- addresses.
newListIpRoutes ::
  -- | 'directoryId'
  Prelude.Text ->
  ListIpRoutes
newListIpRoutes pDirectoryId_ =
  ListIpRoutes'
    { limit = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      directoryId = pDirectoryId_
    }

-- | Maximum number of items to return. If this value is zero, the maximum
-- number of items is specified by the limitations of the operation.
listIpRoutes_limit :: Lens.Lens' ListIpRoutes (Prelude.Maybe Prelude.Natural)
listIpRoutes_limit = Lens.lens (\ListIpRoutes' {limit} -> limit) (\s@ListIpRoutes' {} a -> s {limit = a} :: ListIpRoutes)

-- | The /ListIpRoutes.NextToken/ value from a previous call to ListIpRoutes.
-- Pass null if this is the first call.
listIpRoutes_nextToken :: Lens.Lens' ListIpRoutes (Prelude.Maybe Prelude.Text)
listIpRoutes_nextToken = Lens.lens (\ListIpRoutes' {nextToken} -> nextToken) (\s@ListIpRoutes' {} a -> s {nextToken = a} :: ListIpRoutes)

-- | Identifier (ID) of the directory for which you want to retrieve the IP
-- addresses.
listIpRoutes_directoryId :: Lens.Lens' ListIpRoutes Prelude.Text
listIpRoutes_directoryId = Lens.lens (\ListIpRoutes' {directoryId} -> directoryId) (\s@ListIpRoutes' {} a -> s {directoryId = a} :: ListIpRoutes)

instance Core.AWSPager ListIpRoutes where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listIpRoutesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listIpRoutesResponse_ipRoutesInfo
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listIpRoutes_nextToken
          Lens..~ rs
          Lens.^? listIpRoutesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListIpRoutes where
  type AWSResponse ListIpRoutes = ListIpRoutesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListIpRoutesResponse'
            Prelude.<$> (x Data..?> "IpRoutesInfo" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListIpRoutes where
  hashWithSalt _salt ListIpRoutes' {..} =
    _salt
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` directoryId

instance Prelude.NFData ListIpRoutes where
  rnf ListIpRoutes' {..} =
    Prelude.rnf limit
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf directoryId

instance Data.ToHeaders ListIpRoutes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DirectoryService_20150416.ListIpRoutes" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListIpRoutes where
  toJSON ListIpRoutes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Limit" Data..=) Prelude.<$> limit,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("DirectoryId" Data..= directoryId)
          ]
      )

instance Data.ToPath ListIpRoutes where
  toPath = Prelude.const "/"

instance Data.ToQuery ListIpRoutes where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListIpRoutesResponse' smart constructor.
data ListIpRoutesResponse = ListIpRoutesResponse'
  { -- | A list of IpRoutes.
    ipRoutesInfo :: Prelude.Maybe [IpRouteInfo],
    -- | If not null, more results are available. Pass this value for the
    -- /NextToken/ parameter in a subsequent call to ListIpRoutes to retrieve
    -- the next set of items.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListIpRoutesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipRoutesInfo', 'listIpRoutesResponse_ipRoutesInfo' - A list of IpRoutes.
--
-- 'nextToken', 'listIpRoutesResponse_nextToken' - If not null, more results are available. Pass this value for the
-- /NextToken/ parameter in a subsequent call to ListIpRoutes to retrieve
-- the next set of items.
--
-- 'httpStatus', 'listIpRoutesResponse_httpStatus' - The response's http status code.
newListIpRoutesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListIpRoutesResponse
newListIpRoutesResponse pHttpStatus_ =
  ListIpRoutesResponse'
    { ipRoutesInfo =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of IpRoutes.
listIpRoutesResponse_ipRoutesInfo :: Lens.Lens' ListIpRoutesResponse (Prelude.Maybe [IpRouteInfo])
listIpRoutesResponse_ipRoutesInfo = Lens.lens (\ListIpRoutesResponse' {ipRoutesInfo} -> ipRoutesInfo) (\s@ListIpRoutesResponse' {} a -> s {ipRoutesInfo = a} :: ListIpRoutesResponse) Prelude.. Lens.mapping Lens.coerced

-- | If not null, more results are available. Pass this value for the
-- /NextToken/ parameter in a subsequent call to ListIpRoutes to retrieve
-- the next set of items.
listIpRoutesResponse_nextToken :: Lens.Lens' ListIpRoutesResponse (Prelude.Maybe Prelude.Text)
listIpRoutesResponse_nextToken = Lens.lens (\ListIpRoutesResponse' {nextToken} -> nextToken) (\s@ListIpRoutesResponse' {} a -> s {nextToken = a} :: ListIpRoutesResponse)

-- | The response's http status code.
listIpRoutesResponse_httpStatus :: Lens.Lens' ListIpRoutesResponse Prelude.Int
listIpRoutesResponse_httpStatus = Lens.lens (\ListIpRoutesResponse' {httpStatus} -> httpStatus) (\s@ListIpRoutesResponse' {} a -> s {httpStatus = a} :: ListIpRoutesResponse)

instance Prelude.NFData ListIpRoutesResponse where
  rnf ListIpRoutesResponse' {..} =
    Prelude.rnf ipRoutesInfo
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
