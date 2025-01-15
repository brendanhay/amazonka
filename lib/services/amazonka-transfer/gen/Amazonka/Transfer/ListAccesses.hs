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
-- Module      : Amazonka.Transfer.ListAccesses
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the details for all the accesses you have on your server.
--
-- This operation returns paginated results.
module Amazonka.Transfer.ListAccesses
  ( -- * Creating a Request
    ListAccesses (..),
    newListAccesses,

    -- * Request Lenses
    listAccesses_maxResults,
    listAccesses_nextToken,
    listAccesses_serverId,

    -- * Destructuring the Response
    ListAccessesResponse (..),
    newListAccessesResponse,

    -- * Response Lenses
    listAccessesResponse_nextToken,
    listAccessesResponse_httpStatus,
    listAccessesResponse_serverId,
    listAccessesResponse_accesses,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transfer.Types

-- | /See:/ 'newListAccesses' smart constructor.
data ListAccesses = ListAccesses'
  { -- | Specifies the maximum number of access SIDs to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | When you can get additional results from the @ListAccesses@ call, a
    -- @NextToken@ parameter is returned in the output. You can then pass in a
    -- subsequent command to the @NextToken@ parameter to continue listing
    -- additional accesses.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A system-assigned unique identifier for a server that has users assigned
    -- to it.
    serverId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAccesses' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listAccesses_maxResults' - Specifies the maximum number of access SIDs to return.
--
-- 'nextToken', 'listAccesses_nextToken' - When you can get additional results from the @ListAccesses@ call, a
-- @NextToken@ parameter is returned in the output. You can then pass in a
-- subsequent command to the @NextToken@ parameter to continue listing
-- additional accesses.
--
-- 'serverId', 'listAccesses_serverId' - A system-assigned unique identifier for a server that has users assigned
-- to it.
newListAccesses ::
  -- | 'serverId'
  Prelude.Text ->
  ListAccesses
newListAccesses pServerId_ =
  ListAccesses'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      serverId = pServerId_
    }

-- | Specifies the maximum number of access SIDs to return.
listAccesses_maxResults :: Lens.Lens' ListAccesses (Prelude.Maybe Prelude.Natural)
listAccesses_maxResults = Lens.lens (\ListAccesses' {maxResults} -> maxResults) (\s@ListAccesses' {} a -> s {maxResults = a} :: ListAccesses)

-- | When you can get additional results from the @ListAccesses@ call, a
-- @NextToken@ parameter is returned in the output. You can then pass in a
-- subsequent command to the @NextToken@ parameter to continue listing
-- additional accesses.
listAccesses_nextToken :: Lens.Lens' ListAccesses (Prelude.Maybe Prelude.Text)
listAccesses_nextToken = Lens.lens (\ListAccesses' {nextToken} -> nextToken) (\s@ListAccesses' {} a -> s {nextToken = a} :: ListAccesses)

-- | A system-assigned unique identifier for a server that has users assigned
-- to it.
listAccesses_serverId :: Lens.Lens' ListAccesses Prelude.Text
listAccesses_serverId = Lens.lens (\ListAccesses' {serverId} -> serverId) (\s@ListAccesses' {} a -> s {serverId = a} :: ListAccesses)

instance Core.AWSPager ListAccesses where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAccessesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        (rs Lens.^. listAccessesResponse_accesses) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listAccesses_nextToken
              Lens..~ rs
              Lens.^? listAccessesResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListAccesses where
  type AWSResponse ListAccesses = ListAccessesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAccessesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "ServerId")
            Prelude.<*> (x Data..?> "Accesses" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListAccesses where
  hashWithSalt _salt ListAccesses' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` serverId

instance Prelude.NFData ListAccesses where
  rnf ListAccesses' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf serverId

instance Data.ToHeaders ListAccesses where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "TransferService.ListAccesses" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListAccesses where
  toJSON ListAccesses' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("ServerId" Data..= serverId)
          ]
      )

instance Data.ToPath ListAccesses where
  toPath = Prelude.const "/"

instance Data.ToQuery ListAccesses where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAccessesResponse' smart constructor.
data ListAccessesResponse = ListAccessesResponse'
  { -- | When you can get additional results from the @ListAccesses@ call, a
    -- @NextToken@ parameter is returned in the output. You can then pass in a
    -- subsequent command to the @NextToken@ parameter to continue listing
    -- additional accesses.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A system-assigned unique identifier for a server that has users assigned
    -- to it.
    serverId :: Prelude.Text,
    -- | Returns the accesses and their properties for the @ServerId@ value that
    -- you specify.
    accesses :: [ListedAccess]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAccessesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAccessesResponse_nextToken' - When you can get additional results from the @ListAccesses@ call, a
-- @NextToken@ parameter is returned in the output. You can then pass in a
-- subsequent command to the @NextToken@ parameter to continue listing
-- additional accesses.
--
-- 'httpStatus', 'listAccessesResponse_httpStatus' - The response's http status code.
--
-- 'serverId', 'listAccessesResponse_serverId' - A system-assigned unique identifier for a server that has users assigned
-- to it.
--
-- 'accesses', 'listAccessesResponse_accesses' - Returns the accesses and their properties for the @ServerId@ value that
-- you specify.
newListAccessesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'serverId'
  Prelude.Text ->
  ListAccessesResponse
newListAccessesResponse pHttpStatus_ pServerId_ =
  ListAccessesResponse'
    { nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      serverId = pServerId_,
      accesses = Prelude.mempty
    }

-- | When you can get additional results from the @ListAccesses@ call, a
-- @NextToken@ parameter is returned in the output. You can then pass in a
-- subsequent command to the @NextToken@ parameter to continue listing
-- additional accesses.
listAccessesResponse_nextToken :: Lens.Lens' ListAccessesResponse (Prelude.Maybe Prelude.Text)
listAccessesResponse_nextToken = Lens.lens (\ListAccessesResponse' {nextToken} -> nextToken) (\s@ListAccessesResponse' {} a -> s {nextToken = a} :: ListAccessesResponse)

-- | The response's http status code.
listAccessesResponse_httpStatus :: Lens.Lens' ListAccessesResponse Prelude.Int
listAccessesResponse_httpStatus = Lens.lens (\ListAccessesResponse' {httpStatus} -> httpStatus) (\s@ListAccessesResponse' {} a -> s {httpStatus = a} :: ListAccessesResponse)

-- | A system-assigned unique identifier for a server that has users assigned
-- to it.
listAccessesResponse_serverId :: Lens.Lens' ListAccessesResponse Prelude.Text
listAccessesResponse_serverId = Lens.lens (\ListAccessesResponse' {serverId} -> serverId) (\s@ListAccessesResponse' {} a -> s {serverId = a} :: ListAccessesResponse)

-- | Returns the accesses and their properties for the @ServerId@ value that
-- you specify.
listAccessesResponse_accesses :: Lens.Lens' ListAccessesResponse [ListedAccess]
listAccessesResponse_accesses = Lens.lens (\ListAccessesResponse' {accesses} -> accesses) (\s@ListAccessesResponse' {} a -> s {accesses = a} :: ListAccessesResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListAccessesResponse where
  rnf ListAccessesResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf httpStatus `Prelude.seq`
        Prelude.rnf serverId `Prelude.seq`
          Prelude.rnf accesses
