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
-- Module      : Network.AWS.Connect.ListIntegrationAssociations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Provides summary information about the AppIntegration associations for
-- the specified Amazon Connect instance.
--
-- This operation returns paginated results.
module Network.AWS.Connect.ListIntegrationAssociations
  ( -- * Creating a Request
    ListIntegrationAssociations (..),
    newListIntegrationAssociations,

    -- * Request Lenses
    listIntegrationAssociations_nextToken,
    listIntegrationAssociations_maxResults,
    listIntegrationAssociations_instanceId,

    -- * Destructuring the Response
    ListIntegrationAssociationsResponse (..),
    newListIntegrationAssociationsResponse,

    -- * Response Lenses
    listIntegrationAssociationsResponse_nextToken,
    listIntegrationAssociationsResponse_integrationAssociationSummaryList,
    listIntegrationAssociationsResponse_httpStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListIntegrationAssociations' smart constructor.
data ListIntegrationAssociations = ListIntegrationAssociations'
  { -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The identifier of the Amazon Connect instance.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListIntegrationAssociations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listIntegrationAssociations_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'maxResults', 'listIntegrationAssociations_maxResults' - The maximum number of results to return per page.
--
-- 'instanceId', 'listIntegrationAssociations_instanceId' - The identifier of the Amazon Connect instance.
newListIntegrationAssociations ::
  -- | 'instanceId'
  Prelude.Text ->
  ListIntegrationAssociations
newListIntegrationAssociations pInstanceId_ =
  ListIntegrationAssociations'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      instanceId = pInstanceId_
    }

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
listIntegrationAssociations_nextToken :: Lens.Lens' ListIntegrationAssociations (Prelude.Maybe Prelude.Text)
listIntegrationAssociations_nextToken = Lens.lens (\ListIntegrationAssociations' {nextToken} -> nextToken) (\s@ListIntegrationAssociations' {} a -> s {nextToken = a} :: ListIntegrationAssociations)

-- | The maximum number of results to return per page.
listIntegrationAssociations_maxResults :: Lens.Lens' ListIntegrationAssociations (Prelude.Maybe Prelude.Natural)
listIntegrationAssociations_maxResults = Lens.lens (\ListIntegrationAssociations' {maxResults} -> maxResults) (\s@ListIntegrationAssociations' {} a -> s {maxResults = a} :: ListIntegrationAssociations)

-- | The identifier of the Amazon Connect instance.
listIntegrationAssociations_instanceId :: Lens.Lens' ListIntegrationAssociations Prelude.Text
listIntegrationAssociations_instanceId = Lens.lens (\ListIntegrationAssociations' {instanceId} -> instanceId) (\s@ListIntegrationAssociations' {} a -> s {instanceId = a} :: ListIntegrationAssociations)

instance Core.AWSPager ListIntegrationAssociations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listIntegrationAssociationsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listIntegrationAssociationsResponse_integrationAssociationSummaryList
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listIntegrationAssociations_nextToken
          Lens..~ rs
          Lens.^? listIntegrationAssociationsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListIntegrationAssociations where
  type
    AWSResponse ListIntegrationAssociations =
      ListIntegrationAssociationsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListIntegrationAssociationsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "IntegrationAssociationSummaryList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListIntegrationAssociations

instance Prelude.NFData ListIntegrationAssociations

instance Core.ToHeaders ListIntegrationAssociations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListIntegrationAssociations where
  toPath ListIntegrationAssociations' {..} =
    Prelude.mconcat
      [ "/instance/",
        Core.toBS instanceId,
        "/integration-associations"
      ]

instance Core.ToQuery ListIntegrationAssociations where
  toQuery ListIntegrationAssociations' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListIntegrationAssociationsResponse' smart constructor.
data ListIntegrationAssociationsResponse = ListIntegrationAssociationsResponse'
  { -- | If there are additional results, this is the token for the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The AppIntegration associations.
    integrationAssociationSummaryList :: Prelude.Maybe [IntegrationAssociationSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListIntegrationAssociationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listIntegrationAssociationsResponse_nextToken' - If there are additional results, this is the token for the next set of
-- results.
--
-- 'integrationAssociationSummaryList', 'listIntegrationAssociationsResponse_integrationAssociationSummaryList' - The AppIntegration associations.
--
-- 'httpStatus', 'listIntegrationAssociationsResponse_httpStatus' - The response's http status code.
newListIntegrationAssociationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListIntegrationAssociationsResponse
newListIntegrationAssociationsResponse pHttpStatus_ =
  ListIntegrationAssociationsResponse'
    { nextToken =
        Prelude.Nothing,
      integrationAssociationSummaryList =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If there are additional results, this is the token for the next set of
-- results.
listIntegrationAssociationsResponse_nextToken :: Lens.Lens' ListIntegrationAssociationsResponse (Prelude.Maybe Prelude.Text)
listIntegrationAssociationsResponse_nextToken = Lens.lens (\ListIntegrationAssociationsResponse' {nextToken} -> nextToken) (\s@ListIntegrationAssociationsResponse' {} a -> s {nextToken = a} :: ListIntegrationAssociationsResponse)

-- | The AppIntegration associations.
listIntegrationAssociationsResponse_integrationAssociationSummaryList :: Lens.Lens' ListIntegrationAssociationsResponse (Prelude.Maybe [IntegrationAssociationSummary])
listIntegrationAssociationsResponse_integrationAssociationSummaryList = Lens.lens (\ListIntegrationAssociationsResponse' {integrationAssociationSummaryList} -> integrationAssociationSummaryList) (\s@ListIntegrationAssociationsResponse' {} a -> s {integrationAssociationSummaryList = a} :: ListIntegrationAssociationsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listIntegrationAssociationsResponse_httpStatus :: Lens.Lens' ListIntegrationAssociationsResponse Prelude.Int
listIntegrationAssociationsResponse_httpStatus = Lens.lens (\ListIntegrationAssociationsResponse' {httpStatus} -> httpStatus) (\s@ListIntegrationAssociationsResponse' {} a -> s {httpStatus = a} :: ListIntegrationAssociationsResponse)

instance
  Prelude.NFData
    ListIntegrationAssociationsResponse
