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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the namespaces for the specified Amazon Web Services account.
--
-- This operation returns paginated results.
module Amazonka.QuickSight.ListNamespaces
  ( -- * Creating a Request
    ListNamespaces (..),
    newListNamespaces,

    -- * Request Lenses
    listNamespaces_nextToken,
    listNamespaces_maxResults,
    listNamespaces_awsAccountId,

    -- * Destructuring the Response
    ListNamespacesResponse (..),
    newListNamespacesResponse,

    -- * Response Lenses
    listNamespacesResponse_requestId,
    listNamespacesResponse_namespaces,
    listNamespacesResponse_nextToken,
    listNamespacesResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListNamespaces' smart constructor.
data ListNamespaces = ListNamespaces'
  { -- | A pagination token that can be used in a subsequent request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
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
-- 'nextToken', 'listNamespaces_nextToken' - A pagination token that can be used in a subsequent request.
--
-- 'maxResults', 'listNamespaces_maxResults' - The maximum number of results to return.
--
-- 'awsAccountId', 'listNamespaces_awsAccountId' - The ID for the Amazon Web Services account that contains the Amazon
-- QuickSight namespaces that you want to list.
newListNamespaces ::
  -- | 'awsAccountId'
  Prelude.Text ->
  ListNamespaces
newListNamespaces pAwsAccountId_ =
  ListNamespaces'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      awsAccountId = pAwsAccountId_
    }

-- | A pagination token that can be used in a subsequent request.
listNamespaces_nextToken :: Lens.Lens' ListNamespaces (Prelude.Maybe Prelude.Text)
listNamespaces_nextToken = Lens.lens (\ListNamespaces' {nextToken} -> nextToken) (\s@ListNamespaces' {} a -> s {nextToken = a} :: ListNamespaces)

-- | The maximum number of results to return.
listNamespaces_maxResults :: Lens.Lens' ListNamespaces (Prelude.Maybe Prelude.Natural)
listNamespaces_maxResults = Lens.lens (\ListNamespaces' {maxResults} -> maxResults) (\s@ListNamespaces' {} a -> s {maxResults = a} :: ListNamespaces)

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
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListNamespacesResponse'
            Prelude.<$> (x Core..?> "RequestId")
            Prelude.<*> (x Core..?> "Namespaces" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListNamespaces where
  hashWithSalt salt' ListNamespaces' {..} =
    salt' `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListNamespaces where
  rnf ListNamespaces' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListNamespaces where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListNamespaces where
  toPath ListNamespaces' {..} =
    Prelude.mconcat
      ["/accounts/", Core.toBS awsAccountId, "/namespaces"]

instance Core.ToQuery ListNamespaces where
  toQuery ListNamespaces' {..} =
    Prelude.mconcat
      [ "next-token" Core.=: nextToken,
        "max-results" Core.=: maxResults
      ]

-- | /See:/ 'newListNamespacesResponse' smart constructor.
data ListNamespacesResponse = ListNamespacesResponse'
  { -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The information about the namespaces in this Amazon Web Services
    -- account. The response includes the namespace ARN, name, Amazon Web
    -- Services Region, notification email address, creation status, and
    -- identity store.
    namespaces :: Prelude.Maybe [NamespaceInfoV2],
    -- | A pagination token that can be used in a subsequent request.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'requestId', 'listNamespacesResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'namespaces', 'listNamespacesResponse_namespaces' - The information about the namespaces in this Amazon Web Services
-- account. The response includes the namespace ARN, name, Amazon Web
-- Services Region, notification email address, creation status, and
-- identity store.
--
-- 'nextToken', 'listNamespacesResponse_nextToken' - A pagination token that can be used in a subsequent request.
--
-- 'status', 'listNamespacesResponse_status' - The HTTP status of the request.
newListNamespacesResponse ::
  -- | 'status'
  Prelude.Int ->
  ListNamespacesResponse
newListNamespacesResponse pStatus_ =
  ListNamespacesResponse'
    { requestId =
        Prelude.Nothing,
      namespaces = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      status = pStatus_
    }

-- | The Amazon Web Services request ID for this operation.
listNamespacesResponse_requestId :: Lens.Lens' ListNamespacesResponse (Prelude.Maybe Prelude.Text)
listNamespacesResponse_requestId = Lens.lens (\ListNamespacesResponse' {requestId} -> requestId) (\s@ListNamespacesResponse' {} a -> s {requestId = a} :: ListNamespacesResponse)

-- | The information about the namespaces in this Amazon Web Services
-- account. The response includes the namespace ARN, name, Amazon Web
-- Services Region, notification email address, creation status, and
-- identity store.
listNamespacesResponse_namespaces :: Lens.Lens' ListNamespacesResponse (Prelude.Maybe [NamespaceInfoV2])
listNamespacesResponse_namespaces = Lens.lens (\ListNamespacesResponse' {namespaces} -> namespaces) (\s@ListNamespacesResponse' {} a -> s {namespaces = a} :: ListNamespacesResponse) Prelude.. Lens.mapping Lens.coerced

-- | A pagination token that can be used in a subsequent request.
listNamespacesResponse_nextToken :: Lens.Lens' ListNamespacesResponse (Prelude.Maybe Prelude.Text)
listNamespacesResponse_nextToken = Lens.lens (\ListNamespacesResponse' {nextToken} -> nextToken) (\s@ListNamespacesResponse' {} a -> s {nextToken = a} :: ListNamespacesResponse)

-- | The HTTP status of the request.
listNamespacesResponse_status :: Lens.Lens' ListNamespacesResponse Prelude.Int
listNamespacesResponse_status = Lens.lens (\ListNamespacesResponse' {status} -> status) (\s@ListNamespacesResponse' {} a -> s {status = a} :: ListNamespacesResponse)

instance Prelude.NFData ListNamespacesResponse where
  rnf ListNamespacesResponse' {..} =
    Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf namespaces
