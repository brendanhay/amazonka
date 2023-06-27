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
-- Module      : Amazonka.ChimeSdkVoice.ListVoiceProfileDomains
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the specified voice profile domains in the administrator\'s AWS
-- account.
module Amazonka.ChimeSdkVoice.ListVoiceProfileDomains
  ( -- * Creating a Request
    ListVoiceProfileDomains (..),
    newListVoiceProfileDomains,

    -- * Request Lenses
    listVoiceProfileDomains_maxResults,
    listVoiceProfileDomains_nextToken,

    -- * Destructuring the Response
    ListVoiceProfileDomainsResponse (..),
    newListVoiceProfileDomainsResponse,

    -- * Response Lenses
    listVoiceProfileDomainsResponse_nextToken,
    listVoiceProfileDomainsResponse_voiceProfileDomains,
    listVoiceProfileDomainsResponse_httpStatus,
  )
where

import Amazonka.ChimeSdkVoice.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListVoiceProfileDomains' smart constructor.
data ListVoiceProfileDomains = ListVoiceProfileDomains'
  { -- | The maximum number of results to return in a single call.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token used to return the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListVoiceProfileDomains' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listVoiceProfileDomains_maxResults' - The maximum number of results to return in a single call.
--
-- 'nextToken', 'listVoiceProfileDomains_nextToken' - The token used to return the next page of results.
newListVoiceProfileDomains ::
  ListVoiceProfileDomains
newListVoiceProfileDomains =
  ListVoiceProfileDomains'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of results to return in a single call.
listVoiceProfileDomains_maxResults :: Lens.Lens' ListVoiceProfileDomains (Prelude.Maybe Prelude.Natural)
listVoiceProfileDomains_maxResults = Lens.lens (\ListVoiceProfileDomains' {maxResults} -> maxResults) (\s@ListVoiceProfileDomains' {} a -> s {maxResults = a} :: ListVoiceProfileDomains)

-- | The token used to return the next page of results.
listVoiceProfileDomains_nextToken :: Lens.Lens' ListVoiceProfileDomains (Prelude.Maybe Prelude.Text)
listVoiceProfileDomains_nextToken = Lens.lens (\ListVoiceProfileDomains' {nextToken} -> nextToken) (\s@ListVoiceProfileDomains' {} a -> s {nextToken = a} :: ListVoiceProfileDomains)

instance Core.AWSRequest ListVoiceProfileDomains where
  type
    AWSResponse ListVoiceProfileDomains =
      ListVoiceProfileDomainsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListVoiceProfileDomainsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x
                            Data..?> "VoiceProfileDomains"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListVoiceProfileDomains where
  hashWithSalt _salt ListVoiceProfileDomains' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListVoiceProfileDomains where
  rnf ListVoiceProfileDomains' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListVoiceProfileDomains where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListVoiceProfileDomains where
  toPath = Prelude.const "/voice-profile-domains"

instance Data.ToQuery ListVoiceProfileDomains where
  toQuery ListVoiceProfileDomains' {..} =
    Prelude.mconcat
      [ "max-results" Data.=: maxResults,
        "next-token" Data.=: nextToken
      ]

-- | /See:/ 'newListVoiceProfileDomainsResponse' smart constructor.
data ListVoiceProfileDomainsResponse = ListVoiceProfileDomainsResponse'
  { -- | The token used to return the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of voice profile domains.
    voiceProfileDomains :: Prelude.Maybe [VoiceProfileDomainSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListVoiceProfileDomainsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listVoiceProfileDomainsResponse_nextToken' - The token used to return the next page of results.
--
-- 'voiceProfileDomains', 'listVoiceProfileDomainsResponse_voiceProfileDomains' - The list of voice profile domains.
--
-- 'httpStatus', 'listVoiceProfileDomainsResponse_httpStatus' - The response's http status code.
newListVoiceProfileDomainsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListVoiceProfileDomainsResponse
newListVoiceProfileDomainsResponse pHttpStatus_ =
  ListVoiceProfileDomainsResponse'
    { nextToken =
        Prelude.Nothing,
      voiceProfileDomains = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token used to return the next page of results.
listVoiceProfileDomainsResponse_nextToken :: Lens.Lens' ListVoiceProfileDomainsResponse (Prelude.Maybe Prelude.Text)
listVoiceProfileDomainsResponse_nextToken = Lens.lens (\ListVoiceProfileDomainsResponse' {nextToken} -> nextToken) (\s@ListVoiceProfileDomainsResponse' {} a -> s {nextToken = a} :: ListVoiceProfileDomainsResponse)

-- | The list of voice profile domains.
listVoiceProfileDomainsResponse_voiceProfileDomains :: Lens.Lens' ListVoiceProfileDomainsResponse (Prelude.Maybe [VoiceProfileDomainSummary])
listVoiceProfileDomainsResponse_voiceProfileDomains = Lens.lens (\ListVoiceProfileDomainsResponse' {voiceProfileDomains} -> voiceProfileDomains) (\s@ListVoiceProfileDomainsResponse' {} a -> s {voiceProfileDomains = a} :: ListVoiceProfileDomainsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listVoiceProfileDomainsResponse_httpStatus :: Lens.Lens' ListVoiceProfileDomainsResponse Prelude.Int
listVoiceProfileDomainsResponse_httpStatus = Lens.lens (\ListVoiceProfileDomainsResponse' {httpStatus} -> httpStatus) (\s@ListVoiceProfileDomainsResponse' {} a -> s {httpStatus = a} :: ListVoiceProfileDomainsResponse)

instance
  Prelude.NFData
    ListVoiceProfileDomainsResponse
  where
  rnf ListVoiceProfileDomainsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf voiceProfileDomains
      `Prelude.seq` Prelude.rnf httpStatus
