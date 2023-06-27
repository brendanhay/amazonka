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
-- Module      : Amazonka.Connect.ListUseCases
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the use cases for the integration association.
--
-- This operation returns paginated results.
module Amazonka.Connect.ListUseCases
  ( -- * Creating a Request
    ListUseCases (..),
    newListUseCases,

    -- * Request Lenses
    listUseCases_maxResults,
    listUseCases_nextToken,
    listUseCases_instanceId,
    listUseCases_integrationAssociationId,

    -- * Destructuring the Response
    ListUseCasesResponse (..),
    newListUseCasesResponse,

    -- * Response Lenses
    listUseCasesResponse_nextToken,
    listUseCasesResponse_useCaseSummaryList,
    listUseCasesResponse_httpStatus,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Provides summary information about the use cases for the specified
-- integration association.
--
-- /See:/ 'newListUseCases' smart constructor.
data ListUseCases = ListUseCases'
  { -- | The maximum number of results to return per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the Amazon Connect instance. You can
    -- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
    -- in the Amazon Resource Name (ARN) of the instance.
    instanceId :: Prelude.Text,
    -- | The identifier for the integration association.
    integrationAssociationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListUseCases' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listUseCases_maxResults' - The maximum number of results to return per page.
--
-- 'nextToken', 'listUseCases_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'instanceId', 'listUseCases_instanceId' - The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
--
-- 'integrationAssociationId', 'listUseCases_integrationAssociationId' - The identifier for the integration association.
newListUseCases ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'integrationAssociationId'
  Prelude.Text ->
  ListUseCases
newListUseCases
  pInstanceId_
  pIntegrationAssociationId_ =
    ListUseCases'
      { maxResults = Prelude.Nothing,
        nextToken = Prelude.Nothing,
        instanceId = pInstanceId_,
        integrationAssociationId =
          pIntegrationAssociationId_
      }

-- | The maximum number of results to return per page.
listUseCases_maxResults :: Lens.Lens' ListUseCases (Prelude.Maybe Prelude.Natural)
listUseCases_maxResults = Lens.lens (\ListUseCases' {maxResults} -> maxResults) (\s@ListUseCases' {} a -> s {maxResults = a} :: ListUseCases)

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
listUseCases_nextToken :: Lens.Lens' ListUseCases (Prelude.Maybe Prelude.Text)
listUseCases_nextToken = Lens.lens (\ListUseCases' {nextToken} -> nextToken) (\s@ListUseCases' {} a -> s {nextToken = a} :: ListUseCases)

-- | The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
listUseCases_instanceId :: Lens.Lens' ListUseCases Prelude.Text
listUseCases_instanceId = Lens.lens (\ListUseCases' {instanceId} -> instanceId) (\s@ListUseCases' {} a -> s {instanceId = a} :: ListUseCases)

-- | The identifier for the integration association.
listUseCases_integrationAssociationId :: Lens.Lens' ListUseCases Prelude.Text
listUseCases_integrationAssociationId = Lens.lens (\ListUseCases' {integrationAssociationId} -> integrationAssociationId) (\s@ListUseCases' {} a -> s {integrationAssociationId = a} :: ListUseCases)

instance Core.AWSPager ListUseCases where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listUseCasesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listUseCasesResponse_useCaseSummaryList
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listUseCases_nextToken
          Lens..~ rs
          Lens.^? listUseCasesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListUseCases where
  type AWSResponse ListUseCases = ListUseCasesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListUseCasesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x
                            Data..?> "UseCaseSummaryList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListUseCases where
  hashWithSalt _salt ListUseCases' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` integrationAssociationId

instance Prelude.NFData ListUseCases where
  rnf ListUseCases' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf integrationAssociationId

instance Data.ToHeaders ListUseCases where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListUseCases where
  toPath ListUseCases' {..} =
    Prelude.mconcat
      [ "/instance/",
        Data.toBS instanceId,
        "/integration-associations/",
        Data.toBS integrationAssociationId,
        "/use-cases"
      ]

instance Data.ToQuery ListUseCases where
  toQuery ListUseCases' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListUseCasesResponse' smart constructor.
data ListUseCasesResponse = ListUseCasesResponse'
  { -- | If there are additional results, this is the token for the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The use cases.
    useCaseSummaryList :: Prelude.Maybe [UseCase],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListUseCasesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listUseCasesResponse_nextToken' - If there are additional results, this is the token for the next set of
-- results.
--
-- 'useCaseSummaryList', 'listUseCasesResponse_useCaseSummaryList' - The use cases.
--
-- 'httpStatus', 'listUseCasesResponse_httpStatus' - The response's http status code.
newListUseCasesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListUseCasesResponse
newListUseCasesResponse pHttpStatus_ =
  ListUseCasesResponse'
    { nextToken = Prelude.Nothing,
      useCaseSummaryList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If there are additional results, this is the token for the next set of
-- results.
listUseCasesResponse_nextToken :: Lens.Lens' ListUseCasesResponse (Prelude.Maybe Prelude.Text)
listUseCasesResponse_nextToken = Lens.lens (\ListUseCasesResponse' {nextToken} -> nextToken) (\s@ListUseCasesResponse' {} a -> s {nextToken = a} :: ListUseCasesResponse)

-- | The use cases.
listUseCasesResponse_useCaseSummaryList :: Lens.Lens' ListUseCasesResponse (Prelude.Maybe [UseCase])
listUseCasesResponse_useCaseSummaryList = Lens.lens (\ListUseCasesResponse' {useCaseSummaryList} -> useCaseSummaryList) (\s@ListUseCasesResponse' {} a -> s {useCaseSummaryList = a} :: ListUseCasesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listUseCasesResponse_httpStatus :: Lens.Lens' ListUseCasesResponse Prelude.Int
listUseCasesResponse_httpStatus = Lens.lens (\ListUseCasesResponse' {httpStatus} -> httpStatus) (\s@ListUseCasesResponse' {} a -> s {httpStatus = a} :: ListUseCasesResponse)

instance Prelude.NFData ListUseCasesResponse where
  rnf ListUseCasesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf useCaseSummaryList
      `Prelude.seq` Prelude.rnf httpStatus
