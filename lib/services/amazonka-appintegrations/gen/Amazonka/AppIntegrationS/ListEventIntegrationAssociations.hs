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
-- Module      : Amazonka.AppIntegrationS.ListEventIntegrationAssociations
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a paginated list of event integration associations in the
-- account.
module Amazonka.AppIntegrationS.ListEventIntegrationAssociations
  ( -- * Creating a Request
    ListEventIntegrationAssociations (..),
    newListEventIntegrationAssociations,

    -- * Request Lenses
    listEventIntegrationAssociations_maxResults,
    listEventIntegrationAssociations_nextToken,
    listEventIntegrationAssociations_eventIntegrationName,

    -- * Destructuring the Response
    ListEventIntegrationAssociationsResponse (..),
    newListEventIntegrationAssociationsResponse,

    -- * Response Lenses
    listEventIntegrationAssociationsResponse_eventIntegrationAssociations,
    listEventIntegrationAssociationsResponse_nextToken,
    listEventIntegrationAssociationsResponse_httpStatus,
  )
where

import Amazonka.AppIntegrationS.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListEventIntegrationAssociations' smart constructor.
data ListEventIntegrationAssociations = ListEventIntegrationAssociations'
  { -- | The maximum number of results to return per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the event integration.
    eventIntegrationName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEventIntegrationAssociations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listEventIntegrationAssociations_maxResults' - The maximum number of results to return per page.
--
-- 'nextToken', 'listEventIntegrationAssociations_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'eventIntegrationName', 'listEventIntegrationAssociations_eventIntegrationName' - The name of the event integration.
newListEventIntegrationAssociations ::
  -- | 'eventIntegrationName'
  Prelude.Text ->
  ListEventIntegrationAssociations
newListEventIntegrationAssociations
  pEventIntegrationName_ =
    ListEventIntegrationAssociations'
      { maxResults =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        eventIntegrationName =
          pEventIntegrationName_
      }

-- | The maximum number of results to return per page.
listEventIntegrationAssociations_maxResults :: Lens.Lens' ListEventIntegrationAssociations (Prelude.Maybe Prelude.Natural)
listEventIntegrationAssociations_maxResults = Lens.lens (\ListEventIntegrationAssociations' {maxResults} -> maxResults) (\s@ListEventIntegrationAssociations' {} a -> s {maxResults = a} :: ListEventIntegrationAssociations)

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
listEventIntegrationAssociations_nextToken :: Lens.Lens' ListEventIntegrationAssociations (Prelude.Maybe Prelude.Text)
listEventIntegrationAssociations_nextToken = Lens.lens (\ListEventIntegrationAssociations' {nextToken} -> nextToken) (\s@ListEventIntegrationAssociations' {} a -> s {nextToken = a} :: ListEventIntegrationAssociations)

-- | The name of the event integration.
listEventIntegrationAssociations_eventIntegrationName :: Lens.Lens' ListEventIntegrationAssociations Prelude.Text
listEventIntegrationAssociations_eventIntegrationName = Lens.lens (\ListEventIntegrationAssociations' {eventIntegrationName} -> eventIntegrationName) (\s@ListEventIntegrationAssociations' {} a -> s {eventIntegrationName = a} :: ListEventIntegrationAssociations)

instance
  Core.AWSRequest
    ListEventIntegrationAssociations
  where
  type
    AWSResponse ListEventIntegrationAssociations =
      ListEventIntegrationAssociationsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListEventIntegrationAssociationsResponse'
            Prelude.<$> (x Data..?> "EventIntegrationAssociations")
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListEventIntegrationAssociations
  where
  hashWithSalt
    _salt
    ListEventIntegrationAssociations' {..} =
      _salt `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` eventIntegrationName

instance
  Prelude.NFData
    ListEventIntegrationAssociations
  where
  rnf ListEventIntegrationAssociations' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf eventIntegrationName

instance
  Data.ToHeaders
    ListEventIntegrationAssociations
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListEventIntegrationAssociations where
  toPath ListEventIntegrationAssociations' {..} =
    Prelude.mconcat
      [ "/eventIntegrations/",
        Data.toBS eventIntegrationName,
        "/associations"
      ]

instance
  Data.ToQuery
    ListEventIntegrationAssociations
  where
  toQuery ListEventIntegrationAssociations' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListEventIntegrationAssociationsResponse' smart constructor.
data ListEventIntegrationAssociationsResponse = ListEventIntegrationAssociationsResponse'
  { -- | The event integration associations.
    eventIntegrationAssociations :: Prelude.Maybe (Prelude.NonEmpty EventIntegrationAssociation),
    -- | If there are additional results, this is the token for the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEventIntegrationAssociationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventIntegrationAssociations', 'listEventIntegrationAssociationsResponse_eventIntegrationAssociations' - The event integration associations.
--
-- 'nextToken', 'listEventIntegrationAssociationsResponse_nextToken' - If there are additional results, this is the token for the next set of
-- results.
--
-- 'httpStatus', 'listEventIntegrationAssociationsResponse_httpStatus' - The response's http status code.
newListEventIntegrationAssociationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListEventIntegrationAssociationsResponse
newListEventIntegrationAssociationsResponse
  pHttpStatus_ =
    ListEventIntegrationAssociationsResponse'
      { eventIntegrationAssociations =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The event integration associations.
listEventIntegrationAssociationsResponse_eventIntegrationAssociations :: Lens.Lens' ListEventIntegrationAssociationsResponse (Prelude.Maybe (Prelude.NonEmpty EventIntegrationAssociation))
listEventIntegrationAssociationsResponse_eventIntegrationAssociations = Lens.lens (\ListEventIntegrationAssociationsResponse' {eventIntegrationAssociations} -> eventIntegrationAssociations) (\s@ListEventIntegrationAssociationsResponse' {} a -> s {eventIntegrationAssociations = a} :: ListEventIntegrationAssociationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | If there are additional results, this is the token for the next set of
-- results.
listEventIntegrationAssociationsResponse_nextToken :: Lens.Lens' ListEventIntegrationAssociationsResponse (Prelude.Maybe Prelude.Text)
listEventIntegrationAssociationsResponse_nextToken = Lens.lens (\ListEventIntegrationAssociationsResponse' {nextToken} -> nextToken) (\s@ListEventIntegrationAssociationsResponse' {} a -> s {nextToken = a} :: ListEventIntegrationAssociationsResponse)

-- | The response's http status code.
listEventIntegrationAssociationsResponse_httpStatus :: Lens.Lens' ListEventIntegrationAssociationsResponse Prelude.Int
listEventIntegrationAssociationsResponse_httpStatus = Lens.lens (\ListEventIntegrationAssociationsResponse' {httpStatus} -> httpStatus) (\s@ListEventIntegrationAssociationsResponse' {} a -> s {httpStatus = a} :: ListEventIntegrationAssociationsResponse)

instance
  Prelude.NFData
    ListEventIntegrationAssociationsResponse
  where
  rnf ListEventIntegrationAssociationsResponse' {..} =
    Prelude.rnf eventIntegrationAssociations
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
