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
-- Module      : Network.AWS.WorkLink.ListWebsiteCertificateAuthorities
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of certificate authorities added for the current
-- account and Region.
module Network.AWS.WorkLink.ListWebsiteCertificateAuthorities
  ( -- * Creating a Request
    ListWebsiteCertificateAuthorities (..),
    newListWebsiteCertificateAuthorities,

    -- * Request Lenses
    listWebsiteCertificateAuthorities_nextToken,
    listWebsiteCertificateAuthorities_maxResults,
    listWebsiteCertificateAuthorities_fleetArn,

    -- * Destructuring the Response
    ListWebsiteCertificateAuthoritiesResponse (..),
    newListWebsiteCertificateAuthoritiesResponse,

    -- * Response Lenses
    listWebsiteCertificateAuthoritiesResponse_websiteCertificateAuthorities,
    listWebsiteCertificateAuthoritiesResponse_nextToken,
    listWebsiteCertificateAuthoritiesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkLink.Types

-- | /See:/ 'newListWebsiteCertificateAuthorities' smart constructor.
data ListWebsiteCertificateAuthorities = ListWebsiteCertificateAuthorities'
  { -- | The pagination token used to retrieve the next page of results for this
    -- operation. If this value is null, it retrieves the first page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to be included in the next page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The ARN of the fleet.
    fleetArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListWebsiteCertificateAuthorities' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listWebsiteCertificateAuthorities_nextToken' - The pagination token used to retrieve the next page of results for this
-- operation. If this value is null, it retrieves the first page.
--
-- 'maxResults', 'listWebsiteCertificateAuthorities_maxResults' - The maximum number of results to be included in the next page.
--
-- 'fleetArn', 'listWebsiteCertificateAuthorities_fleetArn' - The ARN of the fleet.
newListWebsiteCertificateAuthorities ::
  -- | 'fleetArn'
  Prelude.Text ->
  ListWebsiteCertificateAuthorities
newListWebsiteCertificateAuthorities pFleetArn_ =
  ListWebsiteCertificateAuthorities'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      fleetArn = pFleetArn_
    }

-- | The pagination token used to retrieve the next page of results for this
-- operation. If this value is null, it retrieves the first page.
listWebsiteCertificateAuthorities_nextToken :: Lens.Lens' ListWebsiteCertificateAuthorities (Prelude.Maybe Prelude.Text)
listWebsiteCertificateAuthorities_nextToken = Lens.lens (\ListWebsiteCertificateAuthorities' {nextToken} -> nextToken) (\s@ListWebsiteCertificateAuthorities' {} a -> s {nextToken = a} :: ListWebsiteCertificateAuthorities)

-- | The maximum number of results to be included in the next page.
listWebsiteCertificateAuthorities_maxResults :: Lens.Lens' ListWebsiteCertificateAuthorities (Prelude.Maybe Prelude.Natural)
listWebsiteCertificateAuthorities_maxResults = Lens.lens (\ListWebsiteCertificateAuthorities' {maxResults} -> maxResults) (\s@ListWebsiteCertificateAuthorities' {} a -> s {maxResults = a} :: ListWebsiteCertificateAuthorities)

-- | The ARN of the fleet.
listWebsiteCertificateAuthorities_fleetArn :: Lens.Lens' ListWebsiteCertificateAuthorities Prelude.Text
listWebsiteCertificateAuthorities_fleetArn = Lens.lens (\ListWebsiteCertificateAuthorities' {fleetArn} -> fleetArn) (\s@ListWebsiteCertificateAuthorities' {} a -> s {fleetArn = a} :: ListWebsiteCertificateAuthorities)

instance
  Core.AWSRequest
    ListWebsiteCertificateAuthorities
  where
  type
    AWSResponse ListWebsiteCertificateAuthorities =
      ListWebsiteCertificateAuthoritiesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListWebsiteCertificateAuthoritiesResponse'
            Prelude.<$> ( x Core..?> "WebsiteCertificateAuthorities"
                            Core..!@ Prelude.mempty
                        )
              Prelude.<*> (x Core..?> "NextToken")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListWebsiteCertificateAuthorities

instance
  Prelude.NFData
    ListWebsiteCertificateAuthorities

instance
  Core.ToHeaders
    ListWebsiteCertificateAuthorities
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
    ListWebsiteCertificateAuthorities
  where
  toJSON ListWebsiteCertificateAuthorities' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just ("FleetArn" Core..= fleetArn)
          ]
      )

instance
  Core.ToPath
    ListWebsiteCertificateAuthorities
  where
  toPath =
    Prelude.const "/listWebsiteCertificateAuthorities"

instance
  Core.ToQuery
    ListWebsiteCertificateAuthorities
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListWebsiteCertificateAuthoritiesResponse' smart constructor.
data ListWebsiteCertificateAuthoritiesResponse = ListWebsiteCertificateAuthoritiesResponse'
  { -- | Information about the certificates.
    websiteCertificateAuthorities :: Prelude.Maybe [WebsiteCaSummary],
    -- | The pagination token used to retrieve the next page of results for this
    -- operation. If there are no more pages, this value is null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListWebsiteCertificateAuthoritiesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'websiteCertificateAuthorities', 'listWebsiteCertificateAuthoritiesResponse_websiteCertificateAuthorities' - Information about the certificates.
--
-- 'nextToken', 'listWebsiteCertificateAuthoritiesResponse_nextToken' - The pagination token used to retrieve the next page of results for this
-- operation. If there are no more pages, this value is null.
--
-- 'httpStatus', 'listWebsiteCertificateAuthoritiesResponse_httpStatus' - The response's http status code.
newListWebsiteCertificateAuthoritiesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListWebsiteCertificateAuthoritiesResponse
newListWebsiteCertificateAuthoritiesResponse
  pHttpStatus_ =
    ListWebsiteCertificateAuthoritiesResponse'
      { websiteCertificateAuthorities =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the certificates.
listWebsiteCertificateAuthoritiesResponse_websiteCertificateAuthorities :: Lens.Lens' ListWebsiteCertificateAuthoritiesResponse (Prelude.Maybe [WebsiteCaSummary])
listWebsiteCertificateAuthoritiesResponse_websiteCertificateAuthorities = Lens.lens (\ListWebsiteCertificateAuthoritiesResponse' {websiteCertificateAuthorities} -> websiteCertificateAuthorities) (\s@ListWebsiteCertificateAuthoritiesResponse' {} a -> s {websiteCertificateAuthorities = a} :: ListWebsiteCertificateAuthoritiesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token used to retrieve the next page of results for this
-- operation. If there are no more pages, this value is null.
listWebsiteCertificateAuthoritiesResponse_nextToken :: Lens.Lens' ListWebsiteCertificateAuthoritiesResponse (Prelude.Maybe Prelude.Text)
listWebsiteCertificateAuthoritiesResponse_nextToken = Lens.lens (\ListWebsiteCertificateAuthoritiesResponse' {nextToken} -> nextToken) (\s@ListWebsiteCertificateAuthoritiesResponse' {} a -> s {nextToken = a} :: ListWebsiteCertificateAuthoritiesResponse)

-- | The response's http status code.
listWebsiteCertificateAuthoritiesResponse_httpStatus :: Lens.Lens' ListWebsiteCertificateAuthoritiesResponse Prelude.Int
listWebsiteCertificateAuthoritiesResponse_httpStatus = Lens.lens (\ListWebsiteCertificateAuthoritiesResponse' {httpStatus} -> httpStatus) (\s@ListWebsiteCertificateAuthoritiesResponse' {} a -> s {httpStatus = a} :: ListWebsiteCertificateAuthoritiesResponse)

instance
  Prelude.NFData
    ListWebsiteCertificateAuthoritiesResponse
