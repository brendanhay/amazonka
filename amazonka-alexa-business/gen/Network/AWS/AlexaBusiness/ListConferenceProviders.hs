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
-- Module      : Network.AWS.AlexaBusiness.ListConferenceProviders
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists conference providers under a specific AWS account.
--
-- This operation returns paginated results.
module Network.AWS.AlexaBusiness.ListConferenceProviders
  ( -- * Creating a Request
    ListConferenceProviders (..),
    newListConferenceProviders,

    -- * Request Lenses
    listConferenceProviders_nextToken,
    listConferenceProviders_maxResults,

    -- * Destructuring the Response
    ListConferenceProvidersResponse (..),
    newListConferenceProvidersResponse,

    -- * Response Lenses
    listConferenceProvidersResponse_nextToken,
    listConferenceProvidersResponse_conferenceProviders,
    listConferenceProvidersResponse_httpStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListConferenceProviders' smart constructor.
data ListConferenceProviders = ListConferenceProviders'
  { -- | The tokens used for pagination.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of conference providers to be returned, per paginated
    -- calls.
    maxResults :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListConferenceProviders' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listConferenceProviders_nextToken' - The tokens used for pagination.
--
-- 'maxResults', 'listConferenceProviders_maxResults' - The maximum number of conference providers to be returned, per paginated
-- calls.
newListConferenceProviders ::
  ListConferenceProviders
newListConferenceProviders =
  ListConferenceProviders'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing
    }

-- | The tokens used for pagination.
listConferenceProviders_nextToken :: Lens.Lens' ListConferenceProviders (Core.Maybe Core.Text)
listConferenceProviders_nextToken = Lens.lens (\ListConferenceProviders' {nextToken} -> nextToken) (\s@ListConferenceProviders' {} a -> s {nextToken = a} :: ListConferenceProviders)

-- | The maximum number of conference providers to be returned, per paginated
-- calls.
listConferenceProviders_maxResults :: Lens.Lens' ListConferenceProviders (Core.Maybe Core.Natural)
listConferenceProviders_maxResults = Lens.lens (\ListConferenceProviders' {maxResults} -> maxResults) (\s@ListConferenceProviders' {} a -> s {maxResults = a} :: ListConferenceProviders)

instance Core.AWSPager ListConferenceProviders where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listConferenceProvidersResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listConferenceProvidersResponse_conferenceProviders
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listConferenceProviders_nextToken
          Lens..~ rs
          Lens.^? listConferenceProvidersResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListConferenceProviders where
  type
    AWSResponse ListConferenceProviders =
      ListConferenceProvidersResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListConferenceProvidersResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> ( x Core..?> "ConferenceProviders"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListConferenceProviders

instance Core.NFData ListConferenceProviders

instance Core.ToHeaders ListConferenceProviders where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.ListConferenceProviders" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListConferenceProviders where
  toJSON ListConferenceProviders' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults
          ]
      )

instance Core.ToPath ListConferenceProviders where
  toPath = Core.const "/"

instance Core.ToQuery ListConferenceProviders where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListConferenceProvidersResponse' smart constructor.
data ListConferenceProvidersResponse = ListConferenceProvidersResponse'
  { -- | The tokens used for pagination.
    nextToken :: Core.Maybe Core.Text,
    -- | The conference providers.
    conferenceProviders :: Core.Maybe [ConferenceProvider],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListConferenceProvidersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listConferenceProvidersResponse_nextToken' - The tokens used for pagination.
--
-- 'conferenceProviders', 'listConferenceProvidersResponse_conferenceProviders' - The conference providers.
--
-- 'httpStatus', 'listConferenceProvidersResponse_httpStatus' - The response's http status code.
newListConferenceProvidersResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListConferenceProvidersResponse
newListConferenceProvidersResponse pHttpStatus_ =
  ListConferenceProvidersResponse'
    { nextToken =
        Core.Nothing,
      conferenceProviders = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The tokens used for pagination.
listConferenceProvidersResponse_nextToken :: Lens.Lens' ListConferenceProvidersResponse (Core.Maybe Core.Text)
listConferenceProvidersResponse_nextToken = Lens.lens (\ListConferenceProvidersResponse' {nextToken} -> nextToken) (\s@ListConferenceProvidersResponse' {} a -> s {nextToken = a} :: ListConferenceProvidersResponse)

-- | The conference providers.
listConferenceProvidersResponse_conferenceProviders :: Lens.Lens' ListConferenceProvidersResponse (Core.Maybe [ConferenceProvider])
listConferenceProvidersResponse_conferenceProviders = Lens.lens (\ListConferenceProvidersResponse' {conferenceProviders} -> conferenceProviders) (\s@ListConferenceProvidersResponse' {} a -> s {conferenceProviders = a} :: ListConferenceProvidersResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listConferenceProvidersResponse_httpStatus :: Lens.Lens' ListConferenceProvidersResponse Core.Int
listConferenceProvidersResponse_httpStatus = Lens.lens (\ListConferenceProvidersResponse' {httpStatus} -> httpStatus) (\s@ListConferenceProvidersResponse' {} a -> s {httpStatus = a} :: ListConferenceProvidersResponse)

instance Core.NFData ListConferenceProvidersResponse
