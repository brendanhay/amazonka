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
-- Module      : Amazonka.AppIntegrationS.ListDataIntegrations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a paginated list of DataIntegrations in the account.
--
-- You cannot create a DataIntegration association for a DataIntegration
-- that has been previously associated. Use a different DataIntegration, or
-- recreate the DataIntegration using the
-- <https://docs.aws.amazon.com/appintegrations/latest/APIReference/API_CreateDataIntegration.html CreateDataIntegration>
-- API.
module Amazonka.AppIntegrationS.ListDataIntegrations
  ( -- * Creating a Request
    ListDataIntegrations (..),
    newListDataIntegrations,

    -- * Request Lenses
    listDataIntegrations_maxResults,
    listDataIntegrations_nextToken,

    -- * Destructuring the Response
    ListDataIntegrationsResponse (..),
    newListDataIntegrationsResponse,

    -- * Response Lenses
    listDataIntegrationsResponse_dataIntegrations,
    listDataIntegrationsResponse_nextToken,
    listDataIntegrationsResponse_httpStatus,
  )
where

import Amazonka.AppIntegrationS.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDataIntegrations' smart constructor.
data ListDataIntegrations = ListDataIntegrations'
  { -- | The maximum number of results to return per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDataIntegrations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listDataIntegrations_maxResults' - The maximum number of results to return per page.
--
-- 'nextToken', 'listDataIntegrations_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
newListDataIntegrations ::
  ListDataIntegrations
newListDataIntegrations =
  ListDataIntegrations'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of results to return per page.
listDataIntegrations_maxResults :: Lens.Lens' ListDataIntegrations (Prelude.Maybe Prelude.Natural)
listDataIntegrations_maxResults = Lens.lens (\ListDataIntegrations' {maxResults} -> maxResults) (\s@ListDataIntegrations' {} a -> s {maxResults = a} :: ListDataIntegrations)

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
listDataIntegrations_nextToken :: Lens.Lens' ListDataIntegrations (Prelude.Maybe Prelude.Text)
listDataIntegrations_nextToken = Lens.lens (\ListDataIntegrations' {nextToken} -> nextToken) (\s@ListDataIntegrations' {} a -> s {nextToken = a} :: ListDataIntegrations)

instance Core.AWSRequest ListDataIntegrations where
  type
    AWSResponse ListDataIntegrations =
      ListDataIntegrationsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDataIntegrationsResponse'
            Prelude.<$> (x Data..?> "DataIntegrations")
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDataIntegrations where
  hashWithSalt _salt ListDataIntegrations' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListDataIntegrations where
  rnf ListDataIntegrations' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListDataIntegrations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListDataIntegrations where
  toPath = Prelude.const "/dataIntegrations"

instance Data.ToQuery ListDataIntegrations where
  toQuery ListDataIntegrations' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListDataIntegrationsResponse' smart constructor.
data ListDataIntegrationsResponse = ListDataIntegrationsResponse'
  { -- | The DataIntegrations associated with this account.
    dataIntegrations :: Prelude.Maybe (Prelude.NonEmpty DataIntegrationSummary),
    -- | If there are additional results, this is the token for the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDataIntegrationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataIntegrations', 'listDataIntegrationsResponse_dataIntegrations' - The DataIntegrations associated with this account.
--
-- 'nextToken', 'listDataIntegrationsResponse_nextToken' - If there are additional results, this is the token for the next set of
-- results.
--
-- 'httpStatus', 'listDataIntegrationsResponse_httpStatus' - The response's http status code.
newListDataIntegrationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDataIntegrationsResponse
newListDataIntegrationsResponse pHttpStatus_ =
  ListDataIntegrationsResponse'
    { dataIntegrations =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The DataIntegrations associated with this account.
listDataIntegrationsResponse_dataIntegrations :: Lens.Lens' ListDataIntegrationsResponse (Prelude.Maybe (Prelude.NonEmpty DataIntegrationSummary))
listDataIntegrationsResponse_dataIntegrations = Lens.lens (\ListDataIntegrationsResponse' {dataIntegrations} -> dataIntegrations) (\s@ListDataIntegrationsResponse' {} a -> s {dataIntegrations = a} :: ListDataIntegrationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | If there are additional results, this is the token for the next set of
-- results.
listDataIntegrationsResponse_nextToken :: Lens.Lens' ListDataIntegrationsResponse (Prelude.Maybe Prelude.Text)
listDataIntegrationsResponse_nextToken = Lens.lens (\ListDataIntegrationsResponse' {nextToken} -> nextToken) (\s@ListDataIntegrationsResponse' {} a -> s {nextToken = a} :: ListDataIntegrationsResponse)

-- | The response's http status code.
listDataIntegrationsResponse_httpStatus :: Lens.Lens' ListDataIntegrationsResponse Prelude.Int
listDataIntegrationsResponse_httpStatus = Lens.lens (\ListDataIntegrationsResponse' {httpStatus} -> httpStatus) (\s@ListDataIntegrationsResponse' {} a -> s {httpStatus = a} :: ListDataIntegrationsResponse)

instance Prelude.NFData ListDataIntegrationsResponse where
  rnf ListDataIntegrationsResponse' {..} =
    Prelude.rnf dataIntegrations
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
