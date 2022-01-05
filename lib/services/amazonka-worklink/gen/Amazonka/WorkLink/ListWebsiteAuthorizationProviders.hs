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
-- Module      : Amazonka.WorkLink.ListWebsiteAuthorizationProviders
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of website authorization providers associated with a
-- specified fleet.
module Amazonka.WorkLink.ListWebsiteAuthorizationProviders
  ( -- * Creating a Request
    ListWebsiteAuthorizationProviders (..),
    newListWebsiteAuthorizationProviders,

    -- * Request Lenses
    listWebsiteAuthorizationProviders_nextToken,
    listWebsiteAuthorizationProviders_maxResults,
    listWebsiteAuthorizationProviders_fleetArn,

    -- * Destructuring the Response
    ListWebsiteAuthorizationProvidersResponse (..),
    newListWebsiteAuthorizationProvidersResponse,

    -- * Response Lenses
    listWebsiteAuthorizationProvidersResponse_websiteAuthorizationProviders,
    listWebsiteAuthorizationProvidersResponse_nextToken,
    listWebsiteAuthorizationProvidersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkLink.Types

-- | /See:/ 'newListWebsiteAuthorizationProviders' smart constructor.
data ListWebsiteAuthorizationProviders = ListWebsiteAuthorizationProviders'
  { -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If this value is null, it retrieves the first page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to be included in the next page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The ARN of the fleet.
    fleetArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListWebsiteAuthorizationProviders' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listWebsiteAuthorizationProviders_nextToken' - The pagination token to use to retrieve the next page of results for
-- this operation. If this value is null, it retrieves the first page.
--
-- 'maxResults', 'listWebsiteAuthorizationProviders_maxResults' - The maximum number of results to be included in the next page.
--
-- 'fleetArn', 'listWebsiteAuthorizationProviders_fleetArn' - The ARN of the fleet.
newListWebsiteAuthorizationProviders ::
  -- | 'fleetArn'
  Prelude.Text ->
  ListWebsiteAuthorizationProviders
newListWebsiteAuthorizationProviders pFleetArn_ =
  ListWebsiteAuthorizationProviders'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      fleetArn = pFleetArn_
    }

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If this value is null, it retrieves the first page.
listWebsiteAuthorizationProviders_nextToken :: Lens.Lens' ListWebsiteAuthorizationProviders (Prelude.Maybe Prelude.Text)
listWebsiteAuthorizationProviders_nextToken = Lens.lens (\ListWebsiteAuthorizationProviders' {nextToken} -> nextToken) (\s@ListWebsiteAuthorizationProviders' {} a -> s {nextToken = a} :: ListWebsiteAuthorizationProviders)

-- | The maximum number of results to be included in the next page.
listWebsiteAuthorizationProviders_maxResults :: Lens.Lens' ListWebsiteAuthorizationProviders (Prelude.Maybe Prelude.Natural)
listWebsiteAuthorizationProviders_maxResults = Lens.lens (\ListWebsiteAuthorizationProviders' {maxResults} -> maxResults) (\s@ListWebsiteAuthorizationProviders' {} a -> s {maxResults = a} :: ListWebsiteAuthorizationProviders)

-- | The ARN of the fleet.
listWebsiteAuthorizationProviders_fleetArn :: Lens.Lens' ListWebsiteAuthorizationProviders Prelude.Text
listWebsiteAuthorizationProviders_fleetArn = Lens.lens (\ListWebsiteAuthorizationProviders' {fleetArn} -> fleetArn) (\s@ListWebsiteAuthorizationProviders' {} a -> s {fleetArn = a} :: ListWebsiteAuthorizationProviders)

instance
  Core.AWSRequest
    ListWebsiteAuthorizationProviders
  where
  type
    AWSResponse ListWebsiteAuthorizationProviders =
      ListWebsiteAuthorizationProvidersResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListWebsiteAuthorizationProvidersResponse'
            Prelude.<$> ( x Core..?> "WebsiteAuthorizationProviders"
                            Core..!@ Prelude.mempty
                        )
              Prelude.<*> (x Core..?> "NextToken")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListWebsiteAuthorizationProviders
  where
  hashWithSalt
    _salt
    ListWebsiteAuthorizationProviders' {..} =
      _salt `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` fleetArn

instance
  Prelude.NFData
    ListWebsiteAuthorizationProviders
  where
  rnf ListWebsiteAuthorizationProviders' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf fleetArn

instance
  Core.ToHeaders
    ListWebsiteAuthorizationProviders
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    ListWebsiteAuthorizationProviders
  where
  toJSON ListWebsiteAuthorizationProviders' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just ("FleetArn" Core..= fleetArn)
          ]
      )

instance
  Core.ToPath
    ListWebsiteAuthorizationProviders
  where
  toPath =
    Prelude.const "/listWebsiteAuthorizationProviders"

instance
  Core.ToQuery
    ListWebsiteAuthorizationProviders
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListWebsiteAuthorizationProvidersResponse' smart constructor.
data ListWebsiteAuthorizationProvidersResponse = ListWebsiteAuthorizationProvidersResponse'
  { -- | The website authorization providers.
    websiteAuthorizationProviders :: Prelude.Maybe [WebsiteAuthorizationProviderSummary],
    -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If this value is null, it retrieves the first page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListWebsiteAuthorizationProvidersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'websiteAuthorizationProviders', 'listWebsiteAuthorizationProvidersResponse_websiteAuthorizationProviders' - The website authorization providers.
--
-- 'nextToken', 'listWebsiteAuthorizationProvidersResponse_nextToken' - The pagination token to use to retrieve the next page of results for
-- this operation. If this value is null, it retrieves the first page.
--
-- 'httpStatus', 'listWebsiteAuthorizationProvidersResponse_httpStatus' - The response's http status code.
newListWebsiteAuthorizationProvidersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListWebsiteAuthorizationProvidersResponse
newListWebsiteAuthorizationProvidersResponse
  pHttpStatus_ =
    ListWebsiteAuthorizationProvidersResponse'
      { websiteAuthorizationProviders =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The website authorization providers.
listWebsiteAuthorizationProvidersResponse_websiteAuthorizationProviders :: Lens.Lens' ListWebsiteAuthorizationProvidersResponse (Prelude.Maybe [WebsiteAuthorizationProviderSummary])
listWebsiteAuthorizationProvidersResponse_websiteAuthorizationProviders = Lens.lens (\ListWebsiteAuthorizationProvidersResponse' {websiteAuthorizationProviders} -> websiteAuthorizationProviders) (\s@ListWebsiteAuthorizationProvidersResponse' {} a -> s {websiteAuthorizationProviders = a} :: ListWebsiteAuthorizationProvidersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If this value is null, it retrieves the first page.
listWebsiteAuthorizationProvidersResponse_nextToken :: Lens.Lens' ListWebsiteAuthorizationProvidersResponse (Prelude.Maybe Prelude.Text)
listWebsiteAuthorizationProvidersResponse_nextToken = Lens.lens (\ListWebsiteAuthorizationProvidersResponse' {nextToken} -> nextToken) (\s@ListWebsiteAuthorizationProvidersResponse' {} a -> s {nextToken = a} :: ListWebsiteAuthorizationProvidersResponse)

-- | The response's http status code.
listWebsiteAuthorizationProvidersResponse_httpStatus :: Lens.Lens' ListWebsiteAuthorizationProvidersResponse Prelude.Int
listWebsiteAuthorizationProvidersResponse_httpStatus = Lens.lens (\ListWebsiteAuthorizationProvidersResponse' {httpStatus} -> httpStatus) (\s@ListWebsiteAuthorizationProvidersResponse' {} a -> s {httpStatus = a} :: ListWebsiteAuthorizationProvidersResponse)

instance
  Prelude.NFData
    ListWebsiteAuthorizationProvidersResponse
  where
  rnf ListWebsiteAuthorizationProvidersResponse' {..} =
    Prelude.rnf websiteAuthorizationProviders
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
