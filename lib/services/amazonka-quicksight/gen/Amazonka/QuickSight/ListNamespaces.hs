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
-- Module      : Amazonka.QuickSight.ListNamespaces
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the namespaces for the specified Amazon Web Services account. This
-- operation doesn\'t list deleted namespaces.
--
-- This operation returns paginated results.
module Amazonka.QuickSight.ListNamespaces
  ( -- * Creating a Request
    ListNamespaces (..),
    newListNamespaces,

    -- * Request Lenses
    listNamespaces_maxResults,
    listNamespaces_nextToken,
    listNamespaces_awsAccountId,

    -- * Destructuring the Response
    ListNamespacesResponse (..),
    newListNamespacesResponse,

    -- * Response Lenses
    listNamespacesResponse_namespaces,
    listNamespacesResponse_nextToken,
    listNamespacesResponse_requestId,
    listNamespacesResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListNamespaces' smart constructor.
data ListNamespaces = ListNamespaces'
  { -- | The maximum number of results to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A unique pagination token that can be used in a subsequent request. You
    -- will receive a pagination token in the response body of a previous
    -- @ListNameSpaces@ API call if there is more data that can be returned. To
    -- receive the data, make another @ListNamespaces@ API call with the
    -- returned token to retrieve the next page of data. Each token is valid
    -- for 24 hours. If you try to make a @ListNamespaces@ API call with an
    -- expired token, you will receive a @HTTP 400 InvalidNextTokenException@
    -- error.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID for the Amazon Web Services account that contains the Amazon
    -- QuickSight namespaces that you want to list.
    awsAccountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListNamespaces' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listNamespaces_maxResults' - The maximum number of results to return.
--
-- 'nextToken', 'listNamespaces_nextToken' - A unique pagination token that can be used in a subsequent request. You
-- will receive a pagination token in the response body of a previous
-- @ListNameSpaces@ API call if there is more data that can be returned. To
-- receive the data, make another @ListNamespaces@ API call with the
-- returned token to retrieve the next page of data. Each token is valid
-- for 24 hours. If you try to make a @ListNamespaces@ API call with an
-- expired token, you will receive a @HTTP 400 InvalidNextTokenException@
-- error.
--
-- 'awsAccountId', 'listNamespaces_awsAccountId' - The ID for the Amazon Web Services account that contains the Amazon
-- QuickSight namespaces that you want to list.
newListNamespaces ::
  -- | 'awsAccountId'
  Prelude.Text ->
  ListNamespaces
newListNamespaces pAwsAccountId_ =
  ListNamespaces'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      awsAccountId = pAwsAccountId_
    }

-- | The maximum number of results to return.
listNamespaces_maxResults :: Lens.Lens' ListNamespaces (Prelude.Maybe Prelude.Natural)
listNamespaces_maxResults = Lens.lens (\ListNamespaces' {maxResults} -> maxResults) (\s@ListNamespaces' {} a -> s {maxResults = a} :: ListNamespaces)

-- | A unique pagination token that can be used in a subsequent request. You
-- will receive a pagination token in the response body of a previous
-- @ListNameSpaces@ API call if there is more data that can be returned. To
-- receive the data, make another @ListNamespaces@ API call with the
-- returned token to retrieve the next page of data. Each token is valid
-- for 24 hours. If you try to make a @ListNamespaces@ API call with an
-- expired token, you will receive a @HTTP 400 InvalidNextTokenException@
-- error.
listNamespaces_nextToken :: Lens.Lens' ListNamespaces (Prelude.Maybe Prelude.Text)
listNamespaces_nextToken = Lens.lens (\ListNamespaces' {nextToken} -> nextToken) (\s@ListNamespaces' {} a -> s {nextToken = a} :: ListNamespaces)

-- | The ID for the Amazon Web Services account that contains the Amazon
-- QuickSight namespaces that you want to list.
listNamespaces_awsAccountId :: Lens.Lens' ListNamespaces Prelude.Text
listNamespaces_awsAccountId = Lens.lens (\ListNamespaces' {awsAccountId} -> awsAccountId) (\s@ListNamespaces' {} a -> s {awsAccountId = a} :: ListNamespaces)

instance Core.AWSPager ListNamespaces where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listNamespacesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listNamespacesResponse_namespaces
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listNamespaces_nextToken
          Lens..~ rs
          Lens.^? listNamespacesResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListNamespaces where
  type
    AWSResponse ListNamespaces =
      ListNamespacesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListNamespacesResponse'
            Prelude.<$> (x Data..?> "Namespaces" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListNamespaces where
  hashWithSalt _salt ListNamespaces' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` awsAccountId

instance Prelude.NFData ListNamespaces where
  rnf ListNamespaces' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf awsAccountId

instance Data.ToHeaders ListNamespaces where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListNamespaces where
  toPath ListNamespaces' {..} =
    Prelude.mconcat
      ["/accounts/", Data.toBS awsAccountId, "/namespaces"]

instance Data.ToQuery ListNamespaces where
  toQuery ListNamespaces' {..} =
    Prelude.mconcat
      [ "max-results" Data.=: maxResults,
        "next-token" Data.=: nextToken
      ]

-- | /See:/ 'newListNamespacesResponse' smart constructor.
data ListNamespacesResponse = ListNamespacesResponse'
  { -- | The information about the namespaces in this Amazon Web Services
    -- account. The response includes the namespace ARN, name, Amazon Web
    -- Services Region, notification email address, creation status, and
    -- identity store.
    namespaces :: Prelude.Maybe [NamespaceInfoV2],
    -- | A unique pagination token that can be used in a subsequent request.
    -- Receiving @NextToken@ in your response inticates that there is more data
    -- that can be returned. To receive the data, make another @ListNamespaces@
    -- API call with the returned token to retrieve the next page of data. Each
    -- token is valid for 24 hours. If you try to make a @ListNamespaces@ API
    -- call with an expired token, you will receive a
    -- @HTTP 400 InvalidNextTokenException@ error.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListNamespacesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'namespaces', 'listNamespacesResponse_namespaces' - The information about the namespaces in this Amazon Web Services
-- account. The response includes the namespace ARN, name, Amazon Web
-- Services Region, notification email address, creation status, and
-- identity store.
--
-- 'nextToken', 'listNamespacesResponse_nextToken' - A unique pagination token that can be used in a subsequent request.
-- Receiving @NextToken@ in your response inticates that there is more data
-- that can be returned. To receive the data, make another @ListNamespaces@
-- API call with the returned token to retrieve the next page of data. Each
-- token is valid for 24 hours. If you try to make a @ListNamespaces@ API
-- call with an expired token, you will receive a
-- @HTTP 400 InvalidNextTokenException@ error.
--
-- 'requestId', 'listNamespacesResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'status', 'listNamespacesResponse_status' - The HTTP status of the request.
newListNamespacesResponse ::
  -- | 'status'
  Prelude.Int ->
  ListNamespacesResponse
newListNamespacesResponse pStatus_ =
  ListNamespacesResponse'
    { namespaces =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      requestId = Prelude.Nothing,
      status = pStatus_
    }

-- | The information about the namespaces in this Amazon Web Services
-- account. The response includes the namespace ARN, name, Amazon Web
-- Services Region, notification email address, creation status, and
-- identity store.
listNamespacesResponse_namespaces :: Lens.Lens' ListNamespacesResponse (Prelude.Maybe [NamespaceInfoV2])
listNamespacesResponse_namespaces = Lens.lens (\ListNamespacesResponse' {namespaces} -> namespaces) (\s@ListNamespacesResponse' {} a -> s {namespaces = a} :: ListNamespacesResponse) Prelude.. Lens.mapping Lens.coerced

-- | A unique pagination token that can be used in a subsequent request.
-- Receiving @NextToken@ in your response inticates that there is more data
-- that can be returned. To receive the data, make another @ListNamespaces@
-- API call with the returned token to retrieve the next page of data. Each
-- token is valid for 24 hours. If you try to make a @ListNamespaces@ API
-- call with an expired token, you will receive a
-- @HTTP 400 InvalidNextTokenException@ error.
listNamespacesResponse_nextToken :: Lens.Lens' ListNamespacesResponse (Prelude.Maybe Prelude.Text)
listNamespacesResponse_nextToken = Lens.lens (\ListNamespacesResponse' {nextToken} -> nextToken) (\s@ListNamespacesResponse' {} a -> s {nextToken = a} :: ListNamespacesResponse)

-- | The Amazon Web Services request ID for this operation.
listNamespacesResponse_requestId :: Lens.Lens' ListNamespacesResponse (Prelude.Maybe Prelude.Text)
listNamespacesResponse_requestId = Lens.lens (\ListNamespacesResponse' {requestId} -> requestId) (\s@ListNamespacesResponse' {} a -> s {requestId = a} :: ListNamespacesResponse)

-- | The HTTP status of the request.
listNamespacesResponse_status :: Lens.Lens' ListNamespacesResponse Prelude.Int
listNamespacesResponse_status = Lens.lens (\ListNamespacesResponse' {status} -> status) (\s@ListNamespacesResponse' {} a -> s {status = a} :: ListNamespacesResponse)

instance Prelude.NFData ListNamespacesResponse where
  rnf ListNamespacesResponse' {..} =
    Prelude.rnf namespaces
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf status
