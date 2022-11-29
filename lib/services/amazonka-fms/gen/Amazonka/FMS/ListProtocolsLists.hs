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
-- Module      : Amazonka.FMS.ListProtocolsLists
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of @ProtocolsListDataSummary@ objects.
--
-- This operation returns paginated results.
module Amazonka.FMS.ListProtocolsLists
  ( -- * Creating a Request
    ListProtocolsLists (..),
    newListProtocolsLists,

    -- * Request Lenses
    listProtocolsLists_defaultLists,
    listProtocolsLists_nextToken,
    listProtocolsLists_maxResults,

    -- * Destructuring the Response
    ListProtocolsListsResponse (..),
    newListProtocolsListsResponse,

    -- * Response Lenses
    listProtocolsListsResponse_nextToken,
    listProtocolsListsResponse_protocolsLists,
    listProtocolsListsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.FMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListProtocolsLists' smart constructor.
data ListProtocolsLists = ListProtocolsLists'
  { -- | Specifies whether the lists to retrieve are default lists owned by
    -- Firewall Manager.
    defaultLists :: Prelude.Maybe Prelude.Bool,
    -- | If you specify a value for @MaxResults@ in your list request, and you
    -- have more objects than the maximum, Firewall Manager returns this token
    -- in the response. For all but the first request, you provide the token
    -- returned by the prior request in the request parameters, to retrieve the
    -- next batch of objects.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of objects that you want Firewall Manager to return
    -- for this request. If more objects are available, in the response,
    -- Firewall Manager provides a @NextToken@ value that you can use in a
    -- subsequent call to get the next batch of objects.
    --
    -- If you don\'t specify this, Firewall Manager returns all available
    -- objects.
    maxResults :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListProtocolsLists' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultLists', 'listProtocolsLists_defaultLists' - Specifies whether the lists to retrieve are default lists owned by
-- Firewall Manager.
--
-- 'nextToken', 'listProtocolsLists_nextToken' - If you specify a value for @MaxResults@ in your list request, and you
-- have more objects than the maximum, Firewall Manager returns this token
-- in the response. For all but the first request, you provide the token
-- returned by the prior request in the request parameters, to retrieve the
-- next batch of objects.
--
-- 'maxResults', 'listProtocolsLists_maxResults' - The maximum number of objects that you want Firewall Manager to return
-- for this request. If more objects are available, in the response,
-- Firewall Manager provides a @NextToken@ value that you can use in a
-- subsequent call to get the next batch of objects.
--
-- If you don\'t specify this, Firewall Manager returns all available
-- objects.
newListProtocolsLists ::
  -- | 'maxResults'
  Prelude.Natural ->
  ListProtocolsLists
newListProtocolsLists pMaxResults_ =
  ListProtocolsLists'
    { defaultLists = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = pMaxResults_
    }

-- | Specifies whether the lists to retrieve are default lists owned by
-- Firewall Manager.
listProtocolsLists_defaultLists :: Lens.Lens' ListProtocolsLists (Prelude.Maybe Prelude.Bool)
listProtocolsLists_defaultLists = Lens.lens (\ListProtocolsLists' {defaultLists} -> defaultLists) (\s@ListProtocolsLists' {} a -> s {defaultLists = a} :: ListProtocolsLists)

-- | If you specify a value for @MaxResults@ in your list request, and you
-- have more objects than the maximum, Firewall Manager returns this token
-- in the response. For all but the first request, you provide the token
-- returned by the prior request in the request parameters, to retrieve the
-- next batch of objects.
listProtocolsLists_nextToken :: Lens.Lens' ListProtocolsLists (Prelude.Maybe Prelude.Text)
listProtocolsLists_nextToken = Lens.lens (\ListProtocolsLists' {nextToken} -> nextToken) (\s@ListProtocolsLists' {} a -> s {nextToken = a} :: ListProtocolsLists)

-- | The maximum number of objects that you want Firewall Manager to return
-- for this request. If more objects are available, in the response,
-- Firewall Manager provides a @NextToken@ value that you can use in a
-- subsequent call to get the next batch of objects.
--
-- If you don\'t specify this, Firewall Manager returns all available
-- objects.
listProtocolsLists_maxResults :: Lens.Lens' ListProtocolsLists Prelude.Natural
listProtocolsLists_maxResults = Lens.lens (\ListProtocolsLists' {maxResults} -> maxResults) (\s@ListProtocolsLists' {} a -> s {maxResults = a} :: ListProtocolsLists)

instance Core.AWSPager ListProtocolsLists where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listProtocolsListsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listProtocolsListsResponse_protocolsLists
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listProtocolsLists_nextToken
          Lens..~ rs
          Lens.^? listProtocolsListsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListProtocolsLists where
  type
    AWSResponse ListProtocolsLists =
      ListProtocolsListsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListProtocolsListsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "ProtocolsLists" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListProtocolsLists where
  hashWithSalt _salt ListProtocolsLists' {..} =
    _salt `Prelude.hashWithSalt` defaultLists
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListProtocolsLists where
  rnf ListProtocolsLists' {..} =
    Prelude.rnf defaultLists
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListProtocolsLists where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSFMS_20180101.ListProtocolsLists" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListProtocolsLists where
  toJSON ListProtocolsLists' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DefaultLists" Core..=) Prelude.<$> defaultLists,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            Prelude.Just ("MaxResults" Core..= maxResults)
          ]
      )

instance Core.ToPath ListProtocolsLists where
  toPath = Prelude.const "/"

instance Core.ToQuery ListProtocolsLists where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListProtocolsListsResponse' smart constructor.
data ListProtocolsListsResponse = ListProtocolsListsResponse'
  { -- | If you specify a value for @MaxResults@ in your list request, and you
    -- have more objects than the maximum, Firewall Manager returns this token
    -- in the response. You can use this token in subsequent requests to
    -- retrieve the next batch of objects.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of @ProtocolsListDataSummary@ objects.
    protocolsLists :: Prelude.Maybe [ProtocolsListDataSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListProtocolsListsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listProtocolsListsResponse_nextToken' - If you specify a value for @MaxResults@ in your list request, and you
-- have more objects than the maximum, Firewall Manager returns this token
-- in the response. You can use this token in subsequent requests to
-- retrieve the next batch of objects.
--
-- 'protocolsLists', 'listProtocolsListsResponse_protocolsLists' - An array of @ProtocolsListDataSummary@ objects.
--
-- 'httpStatus', 'listProtocolsListsResponse_httpStatus' - The response's http status code.
newListProtocolsListsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListProtocolsListsResponse
newListProtocolsListsResponse pHttpStatus_ =
  ListProtocolsListsResponse'
    { nextToken =
        Prelude.Nothing,
      protocolsLists = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If you specify a value for @MaxResults@ in your list request, and you
-- have more objects than the maximum, Firewall Manager returns this token
-- in the response. You can use this token in subsequent requests to
-- retrieve the next batch of objects.
listProtocolsListsResponse_nextToken :: Lens.Lens' ListProtocolsListsResponse (Prelude.Maybe Prelude.Text)
listProtocolsListsResponse_nextToken = Lens.lens (\ListProtocolsListsResponse' {nextToken} -> nextToken) (\s@ListProtocolsListsResponse' {} a -> s {nextToken = a} :: ListProtocolsListsResponse)

-- | An array of @ProtocolsListDataSummary@ objects.
listProtocolsListsResponse_protocolsLists :: Lens.Lens' ListProtocolsListsResponse (Prelude.Maybe [ProtocolsListDataSummary])
listProtocolsListsResponse_protocolsLists = Lens.lens (\ListProtocolsListsResponse' {protocolsLists} -> protocolsLists) (\s@ListProtocolsListsResponse' {} a -> s {protocolsLists = a} :: ListProtocolsListsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listProtocolsListsResponse_httpStatus :: Lens.Lens' ListProtocolsListsResponse Prelude.Int
listProtocolsListsResponse_httpStatus = Lens.lens (\ListProtocolsListsResponse' {httpStatus} -> httpStatus) (\s@ListProtocolsListsResponse' {} a -> s {httpStatus = a} :: ListProtocolsListsResponse)

instance Prelude.NFData ListProtocolsListsResponse where
  rnf ListProtocolsListsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf protocolsLists
      `Prelude.seq` Prelude.rnf httpStatus
