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
-- Module      : Amazonka.M2.ListDeployments
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all deployments of a specific application. A
-- deployment is a combination of a specific application and a specific
-- version of that application. Each deployment is mapped to a particular
-- application version.
--
-- This operation returns paginated results.
module Amazonka.M2.ListDeployments
  ( -- * Creating a Request
    ListDeployments (..),
    newListDeployments,

    -- * Request Lenses
    listDeployments_maxResults,
    listDeployments_nextToken,
    listDeployments_applicationId,

    -- * Destructuring the Response
    ListDeploymentsResponse (..),
    newListDeploymentsResponse,

    -- * Response Lenses
    listDeploymentsResponse_nextToken,
    listDeploymentsResponse_httpStatus,
    listDeploymentsResponse_deployments,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.M2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDeployments' smart constructor.
data ListDeployments = ListDeployments'
  { -- | The maximum number of objects to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A pagination token returned from a previous call to this operation. This
    -- specifies the next item to return. To return to the beginning of the
    -- list, exclude this parameter.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The application identifier.
    applicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDeployments' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listDeployments_maxResults' - The maximum number of objects to return.
--
-- 'nextToken', 'listDeployments_nextToken' - A pagination token returned from a previous call to this operation. This
-- specifies the next item to return. To return to the beginning of the
-- list, exclude this parameter.
--
-- 'applicationId', 'listDeployments_applicationId' - The application identifier.
newListDeployments ::
  -- | 'applicationId'
  Prelude.Text ->
  ListDeployments
newListDeployments pApplicationId_ =
  ListDeployments'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      applicationId = pApplicationId_
    }

-- | The maximum number of objects to return.
listDeployments_maxResults :: Lens.Lens' ListDeployments (Prelude.Maybe Prelude.Natural)
listDeployments_maxResults = Lens.lens (\ListDeployments' {maxResults} -> maxResults) (\s@ListDeployments' {} a -> s {maxResults = a} :: ListDeployments)

-- | A pagination token returned from a previous call to this operation. This
-- specifies the next item to return. To return to the beginning of the
-- list, exclude this parameter.
listDeployments_nextToken :: Lens.Lens' ListDeployments (Prelude.Maybe Prelude.Text)
listDeployments_nextToken = Lens.lens (\ListDeployments' {nextToken} -> nextToken) (\s@ListDeployments' {} a -> s {nextToken = a} :: ListDeployments)

-- | The application identifier.
listDeployments_applicationId :: Lens.Lens' ListDeployments Prelude.Text
listDeployments_applicationId = Lens.lens (\ListDeployments' {applicationId} -> applicationId) (\s@ListDeployments' {} a -> s {applicationId = a} :: ListDeployments)

instance Core.AWSPager ListDeployments where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDeploymentsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        (rs Lens.^. listDeploymentsResponse_deployments) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listDeployments_nextToken
          Lens..~ rs
          Lens.^? listDeploymentsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListDeployments where
  type
    AWSResponse ListDeployments =
      ListDeploymentsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDeploymentsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "deployments" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListDeployments where
  hashWithSalt _salt ListDeployments' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` applicationId

instance Prelude.NFData ListDeployments where
  rnf ListDeployments' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf applicationId

instance Data.ToHeaders ListDeployments where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListDeployments where
  toPath ListDeployments' {..} =
    Prelude.mconcat
      [ "/applications/",
        Data.toBS applicationId,
        "/deployments"
      ]

instance Data.ToQuery ListDeployments where
  toQuery ListDeployments' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListDeploymentsResponse' smart constructor.
data ListDeploymentsResponse = ListDeploymentsResponse'
  { -- | If there are more items to return, this contains a token that is passed
    -- to a subsequent call to this operation to retrieve the next set of
    -- items.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The list of deployments that is returned.
    deployments :: [DeploymentSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDeploymentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDeploymentsResponse_nextToken' - If there are more items to return, this contains a token that is passed
-- to a subsequent call to this operation to retrieve the next set of
-- items.
--
-- 'httpStatus', 'listDeploymentsResponse_httpStatus' - The response's http status code.
--
-- 'deployments', 'listDeploymentsResponse_deployments' - The list of deployments that is returned.
newListDeploymentsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDeploymentsResponse
newListDeploymentsResponse pHttpStatus_ =
  ListDeploymentsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      deployments = Prelude.mempty
    }

-- | If there are more items to return, this contains a token that is passed
-- to a subsequent call to this operation to retrieve the next set of
-- items.
listDeploymentsResponse_nextToken :: Lens.Lens' ListDeploymentsResponse (Prelude.Maybe Prelude.Text)
listDeploymentsResponse_nextToken = Lens.lens (\ListDeploymentsResponse' {nextToken} -> nextToken) (\s@ListDeploymentsResponse' {} a -> s {nextToken = a} :: ListDeploymentsResponse)

-- | The response's http status code.
listDeploymentsResponse_httpStatus :: Lens.Lens' ListDeploymentsResponse Prelude.Int
listDeploymentsResponse_httpStatus = Lens.lens (\ListDeploymentsResponse' {httpStatus} -> httpStatus) (\s@ListDeploymentsResponse' {} a -> s {httpStatus = a} :: ListDeploymentsResponse)

-- | The list of deployments that is returned.
listDeploymentsResponse_deployments :: Lens.Lens' ListDeploymentsResponse [DeploymentSummary]
listDeploymentsResponse_deployments = Lens.lens (\ListDeploymentsResponse' {deployments} -> deployments) (\s@ListDeploymentsResponse' {} a -> s {deployments = a} :: ListDeploymentsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListDeploymentsResponse where
  rnf ListDeploymentsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf deployments
