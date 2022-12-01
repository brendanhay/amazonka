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
-- Module      : Amazonka.AlexaBusiness.ListConferenceProviders
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists conference providers under a specific AWS account.
--
-- This operation returns paginated results.
module Amazonka.AlexaBusiness.ListConferenceProviders
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

import Amazonka.AlexaBusiness.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListConferenceProviders' smart constructor.
data ListConferenceProviders = ListConferenceProviders'
  { -- | The tokens used for pagination.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of conference providers to be returned, per paginated
    -- calls.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The tokens used for pagination.
listConferenceProviders_nextToken :: Lens.Lens' ListConferenceProviders (Prelude.Maybe Prelude.Text)
listConferenceProviders_nextToken = Lens.lens (\ListConferenceProviders' {nextToken} -> nextToken) (\s@ListConferenceProviders' {} a -> s {nextToken = a} :: ListConferenceProviders)

-- | The maximum number of conference providers to be returned, per paginated
-- calls.
listConferenceProviders_maxResults :: Lens.Lens' ListConferenceProviders (Prelude.Maybe Prelude.Natural)
listConferenceProviders_maxResults = Lens.lens (\ListConferenceProviders' {maxResults} -> maxResults) (\s@ListConferenceProviders' {} a -> s {maxResults = a} :: ListConferenceProviders)

instance Core.AWSPager ListConferenceProviders where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listConferenceProvidersResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listConferenceProvidersResponse_conferenceProviders
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listConferenceProviders_nextToken
          Lens..~ rs
          Lens.^? listConferenceProvidersResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListConferenceProviders where
  type
    AWSResponse ListConferenceProviders =
      ListConferenceProvidersResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListConferenceProvidersResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "ConferenceProviders"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListConferenceProviders where
  hashWithSalt _salt ListConferenceProviders' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListConferenceProviders where
  rnf ListConferenceProviders' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListConferenceProviders where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.ListConferenceProviders" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListConferenceProviders where
  toJSON ListConferenceProviders' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListConferenceProviders where
  toPath = Prelude.const "/"

instance Core.ToQuery ListConferenceProviders where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListConferenceProvidersResponse' smart constructor.
data ListConferenceProvidersResponse = ListConferenceProvidersResponse'
  { -- | The tokens used for pagination.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The conference providers.
    conferenceProviders :: Prelude.Maybe [ConferenceProvider],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListConferenceProvidersResponse
newListConferenceProvidersResponse pHttpStatus_ =
  ListConferenceProvidersResponse'
    { nextToken =
        Prelude.Nothing,
      conferenceProviders = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The tokens used for pagination.
listConferenceProvidersResponse_nextToken :: Lens.Lens' ListConferenceProvidersResponse (Prelude.Maybe Prelude.Text)
listConferenceProvidersResponse_nextToken = Lens.lens (\ListConferenceProvidersResponse' {nextToken} -> nextToken) (\s@ListConferenceProvidersResponse' {} a -> s {nextToken = a} :: ListConferenceProvidersResponse)

-- | The conference providers.
listConferenceProvidersResponse_conferenceProviders :: Lens.Lens' ListConferenceProvidersResponse (Prelude.Maybe [ConferenceProvider])
listConferenceProvidersResponse_conferenceProviders = Lens.lens (\ListConferenceProvidersResponse' {conferenceProviders} -> conferenceProviders) (\s@ListConferenceProvidersResponse' {} a -> s {conferenceProviders = a} :: ListConferenceProvidersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listConferenceProvidersResponse_httpStatus :: Lens.Lens' ListConferenceProvidersResponse Prelude.Int
listConferenceProvidersResponse_httpStatus = Lens.lens (\ListConferenceProvidersResponse' {httpStatus} -> httpStatus) (\s@ListConferenceProvidersResponse' {} a -> s {httpStatus = a} :: ListConferenceProvidersResponse)

instance
  Prelude.NFData
    ListConferenceProvidersResponse
  where
  rnf ListConferenceProvidersResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf conferenceProviders
      `Prelude.seq` Prelude.rnf httpStatus
