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
-- Module      : Amazonka.VoiceId.ListSpeakers
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all speakers in a specified domain.
--
-- This operation returns paginated results.
module Amazonka.VoiceId.ListSpeakers
  ( -- * Creating a Request
    ListSpeakers (..),
    newListSpeakers,

    -- * Request Lenses
    listSpeakers_nextToken,
    listSpeakers_maxResults,
    listSpeakers_domainId,

    -- * Destructuring the Response
    ListSpeakersResponse (..),
    newListSpeakersResponse,

    -- * Response Lenses
    listSpeakersResponse_nextToken,
    listSpeakersResponse_speakerSummaries,
    listSpeakersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VoiceId.Types

-- | /See:/ 'newListSpeakers' smart constructor.
data ListSpeakers = ListSpeakers'
  { -- | If @NextToken@ is returned, there are more results available. The value
    -- of @NextToken@ is a unique pagination token for each page. Make the call
    -- again using the returned token to retrieve the next page. Keep all other
    -- arguments unchanged. Each pagination token expires after 24 hours.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results that are returned per call. You can use
    -- @NextToken@ to obtain further pages of results. The default is 100; the
    -- maximum allowed page size is also 100.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The identifier of the domain.
    domainId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSpeakers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSpeakers_nextToken' - If @NextToken@ is returned, there are more results available. The value
-- of @NextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged. Each pagination token expires after 24 hours.
--
-- 'maxResults', 'listSpeakers_maxResults' - The maximum number of results that are returned per call. You can use
-- @NextToken@ to obtain further pages of results. The default is 100; the
-- maximum allowed page size is also 100.
--
-- 'domainId', 'listSpeakers_domainId' - The identifier of the domain.
newListSpeakers ::
  -- | 'domainId'
  Prelude.Text ->
  ListSpeakers
newListSpeakers pDomainId_ =
  ListSpeakers'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      domainId = pDomainId_
    }

-- | If @NextToken@ is returned, there are more results available. The value
-- of @NextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged. Each pagination token expires after 24 hours.
listSpeakers_nextToken :: Lens.Lens' ListSpeakers (Prelude.Maybe Prelude.Text)
listSpeakers_nextToken = Lens.lens (\ListSpeakers' {nextToken} -> nextToken) (\s@ListSpeakers' {} a -> s {nextToken = a} :: ListSpeakers)

-- | The maximum number of results that are returned per call. You can use
-- @NextToken@ to obtain further pages of results. The default is 100; the
-- maximum allowed page size is also 100.
listSpeakers_maxResults :: Lens.Lens' ListSpeakers (Prelude.Maybe Prelude.Natural)
listSpeakers_maxResults = Lens.lens (\ListSpeakers' {maxResults} -> maxResults) (\s@ListSpeakers' {} a -> s {maxResults = a} :: ListSpeakers)

-- | The identifier of the domain.
listSpeakers_domainId :: Lens.Lens' ListSpeakers Prelude.Text
listSpeakers_domainId = Lens.lens (\ListSpeakers' {domainId} -> domainId) (\s@ListSpeakers' {} a -> s {domainId = a} :: ListSpeakers)

instance Core.AWSPager ListSpeakers where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSpeakersResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listSpeakersResponse_speakerSummaries
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listSpeakers_nextToken
          Lens..~ rs
          Lens.^? listSpeakersResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListSpeakers where
  type AWSResponse ListSpeakers = ListSpeakersResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSpeakersResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "SpeakerSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSpeakers where
  hashWithSalt _salt ListSpeakers' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` domainId

instance Prelude.NFData ListSpeakers where
  rnf ListSpeakers' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf domainId

instance Core.ToHeaders ListSpeakers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("VoiceID.ListSpeakers" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListSpeakers where
  toJSON ListSpeakers' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just ("DomainId" Core..= domainId)
          ]
      )

instance Core.ToPath ListSpeakers where
  toPath = Prelude.const "/"

instance Core.ToQuery ListSpeakers where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListSpeakersResponse' smart constructor.
data ListSpeakersResponse = ListSpeakersResponse'
  { -- | If @NextToken@ is returned, there are more results available. The value
    -- of @NextToken@ is a unique pagination token for each page. Make the call
    -- again using the returned token to retrieve the next page. Keep all other
    -- arguments unchanged. Each pagination token expires after 24 hours.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list containing details about each speaker in the Amazon Web Services
    -- account.
    speakerSummaries :: Prelude.Maybe [SpeakerSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSpeakersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSpeakersResponse_nextToken' - If @NextToken@ is returned, there are more results available. The value
-- of @NextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged. Each pagination token expires after 24 hours.
--
-- 'speakerSummaries', 'listSpeakersResponse_speakerSummaries' - A list containing details about each speaker in the Amazon Web Services
-- account.
--
-- 'httpStatus', 'listSpeakersResponse_httpStatus' - The response's http status code.
newListSpeakersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSpeakersResponse
newListSpeakersResponse pHttpStatus_ =
  ListSpeakersResponse'
    { nextToken = Prelude.Nothing,
      speakerSummaries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If @NextToken@ is returned, there are more results available. The value
-- of @NextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged. Each pagination token expires after 24 hours.
listSpeakersResponse_nextToken :: Lens.Lens' ListSpeakersResponse (Prelude.Maybe Prelude.Text)
listSpeakersResponse_nextToken = Lens.lens (\ListSpeakersResponse' {nextToken} -> nextToken) (\s@ListSpeakersResponse' {} a -> s {nextToken = a} :: ListSpeakersResponse)

-- | A list containing details about each speaker in the Amazon Web Services
-- account.
listSpeakersResponse_speakerSummaries :: Lens.Lens' ListSpeakersResponse (Prelude.Maybe [SpeakerSummary])
listSpeakersResponse_speakerSummaries = Lens.lens (\ListSpeakersResponse' {speakerSummaries} -> speakerSummaries) (\s@ListSpeakersResponse' {} a -> s {speakerSummaries = a} :: ListSpeakersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listSpeakersResponse_httpStatus :: Lens.Lens' ListSpeakersResponse Prelude.Int
listSpeakersResponse_httpStatus = Lens.lens (\ListSpeakersResponse' {httpStatus} -> httpStatus) (\s@ListSpeakersResponse' {} a -> s {httpStatus = a} :: ListSpeakersResponse)

instance Prelude.NFData ListSpeakersResponse where
  rnf ListSpeakersResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf speakerSummaries
      `Prelude.seq` Prelude.rnf httpStatus
