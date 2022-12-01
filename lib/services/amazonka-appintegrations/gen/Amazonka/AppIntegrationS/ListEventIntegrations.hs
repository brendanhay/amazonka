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
-- Module      : Amazonka.AppIntegrationS.ListEventIntegrations
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a paginated list of event integrations in the account.
module Amazonka.AppIntegrationS.ListEventIntegrations
  ( -- * Creating a Request
    ListEventIntegrations (..),
    newListEventIntegrations,

    -- * Request Lenses
    listEventIntegrations_nextToken,
    listEventIntegrations_maxResults,

    -- * Destructuring the Response
    ListEventIntegrationsResponse (..),
    newListEventIntegrationsResponse,

    -- * Response Lenses
    listEventIntegrationsResponse_nextToken,
    listEventIntegrationsResponse_eventIntegrations,
    listEventIntegrationsResponse_httpStatus,
  )
where

import Amazonka.AppIntegrationS.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListEventIntegrations' smart constructor.
data ListEventIntegrations = ListEventIntegrations'
  { -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return per page.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEventIntegrations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listEventIntegrations_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'maxResults', 'listEventIntegrations_maxResults' - The maximum number of results to return per page.
newListEventIntegrations ::
  ListEventIntegrations
newListEventIntegrations =
  ListEventIntegrations'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
listEventIntegrations_nextToken :: Lens.Lens' ListEventIntegrations (Prelude.Maybe Prelude.Text)
listEventIntegrations_nextToken = Lens.lens (\ListEventIntegrations' {nextToken} -> nextToken) (\s@ListEventIntegrations' {} a -> s {nextToken = a} :: ListEventIntegrations)

-- | The maximum number of results to return per page.
listEventIntegrations_maxResults :: Lens.Lens' ListEventIntegrations (Prelude.Maybe Prelude.Natural)
listEventIntegrations_maxResults = Lens.lens (\ListEventIntegrations' {maxResults} -> maxResults) (\s@ListEventIntegrations' {} a -> s {maxResults = a} :: ListEventIntegrations)

instance Core.AWSRequest ListEventIntegrations where
  type
    AWSResponse ListEventIntegrations =
      ListEventIntegrationsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListEventIntegrationsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "EventIntegrations")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListEventIntegrations where
  hashWithSalt _salt ListEventIntegrations' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListEventIntegrations where
  rnf ListEventIntegrations' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListEventIntegrations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListEventIntegrations where
  toPath = Prelude.const "/eventIntegrations"

instance Core.ToQuery ListEventIntegrations where
  toQuery ListEventIntegrations' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListEventIntegrationsResponse' smart constructor.
data ListEventIntegrationsResponse = ListEventIntegrationsResponse'
  { -- | If there are additional results, this is the token for the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The event integrations.
    eventIntegrations :: Prelude.Maybe (Prelude.NonEmpty EventIntegration),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEventIntegrationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listEventIntegrationsResponse_nextToken' - If there are additional results, this is the token for the next set of
-- results.
--
-- 'eventIntegrations', 'listEventIntegrationsResponse_eventIntegrations' - The event integrations.
--
-- 'httpStatus', 'listEventIntegrationsResponse_httpStatus' - The response's http status code.
newListEventIntegrationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListEventIntegrationsResponse
newListEventIntegrationsResponse pHttpStatus_ =
  ListEventIntegrationsResponse'
    { nextToken =
        Prelude.Nothing,
      eventIntegrations = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If there are additional results, this is the token for the next set of
-- results.
listEventIntegrationsResponse_nextToken :: Lens.Lens' ListEventIntegrationsResponse (Prelude.Maybe Prelude.Text)
listEventIntegrationsResponse_nextToken = Lens.lens (\ListEventIntegrationsResponse' {nextToken} -> nextToken) (\s@ListEventIntegrationsResponse' {} a -> s {nextToken = a} :: ListEventIntegrationsResponse)

-- | The event integrations.
listEventIntegrationsResponse_eventIntegrations :: Lens.Lens' ListEventIntegrationsResponse (Prelude.Maybe (Prelude.NonEmpty EventIntegration))
listEventIntegrationsResponse_eventIntegrations = Lens.lens (\ListEventIntegrationsResponse' {eventIntegrations} -> eventIntegrations) (\s@ListEventIntegrationsResponse' {} a -> s {eventIntegrations = a} :: ListEventIntegrationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listEventIntegrationsResponse_httpStatus :: Lens.Lens' ListEventIntegrationsResponse Prelude.Int
listEventIntegrationsResponse_httpStatus = Lens.lens (\ListEventIntegrationsResponse' {httpStatus} -> httpStatus) (\s@ListEventIntegrationsResponse' {} a -> s {httpStatus = a} :: ListEventIntegrationsResponse)

instance Prelude.NFData ListEventIntegrationsResponse where
  rnf ListEventIntegrationsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf eventIntegrations
      `Prelude.seq` Prelude.rnf httpStatus
