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
-- Module      : Amazonka.ChimeSdkVoice.ListVoiceProfiles
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the voice profiles in a voice profile domain.
module Amazonka.ChimeSdkVoice.ListVoiceProfiles
  ( -- * Creating a Request
    ListVoiceProfiles (..),
    newListVoiceProfiles,

    -- * Request Lenses
    listVoiceProfiles_maxResults,
    listVoiceProfiles_nextToken,
    listVoiceProfiles_voiceProfileDomainId,

    -- * Destructuring the Response
    ListVoiceProfilesResponse (..),
    newListVoiceProfilesResponse,

    -- * Response Lenses
    listVoiceProfilesResponse_nextToken,
    listVoiceProfilesResponse_voiceProfiles,
    listVoiceProfilesResponse_httpStatus,
  )
where

import Amazonka.ChimeSdkVoice.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListVoiceProfiles' smart constructor.
data ListVoiceProfiles = ListVoiceProfiles'
  { -- | The maximum number of results in the request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token used to retrieve the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the voice profile domain.
    voiceProfileDomainId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListVoiceProfiles' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listVoiceProfiles_maxResults' - The maximum number of results in the request.
--
-- 'nextToken', 'listVoiceProfiles_nextToken' - The token used to retrieve the next page of results.
--
-- 'voiceProfileDomainId', 'listVoiceProfiles_voiceProfileDomainId' - The ID of the voice profile domain.
newListVoiceProfiles ::
  -- | 'voiceProfileDomainId'
  Prelude.Text ->
  ListVoiceProfiles
newListVoiceProfiles pVoiceProfileDomainId_ =
  ListVoiceProfiles'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      voiceProfileDomainId = pVoiceProfileDomainId_
    }

-- | The maximum number of results in the request.
listVoiceProfiles_maxResults :: Lens.Lens' ListVoiceProfiles (Prelude.Maybe Prelude.Natural)
listVoiceProfiles_maxResults = Lens.lens (\ListVoiceProfiles' {maxResults} -> maxResults) (\s@ListVoiceProfiles' {} a -> s {maxResults = a} :: ListVoiceProfiles)

-- | The token used to retrieve the next page of results.
listVoiceProfiles_nextToken :: Lens.Lens' ListVoiceProfiles (Prelude.Maybe Prelude.Text)
listVoiceProfiles_nextToken = Lens.lens (\ListVoiceProfiles' {nextToken} -> nextToken) (\s@ListVoiceProfiles' {} a -> s {nextToken = a} :: ListVoiceProfiles)

-- | The ID of the voice profile domain.
listVoiceProfiles_voiceProfileDomainId :: Lens.Lens' ListVoiceProfiles Prelude.Text
listVoiceProfiles_voiceProfileDomainId = Lens.lens (\ListVoiceProfiles' {voiceProfileDomainId} -> voiceProfileDomainId) (\s@ListVoiceProfiles' {} a -> s {voiceProfileDomainId = a} :: ListVoiceProfiles)

instance Core.AWSRequest ListVoiceProfiles where
  type
    AWSResponse ListVoiceProfiles =
      ListVoiceProfilesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListVoiceProfilesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "VoiceProfiles" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListVoiceProfiles where
  hashWithSalt _salt ListVoiceProfiles' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` voiceProfileDomainId

instance Prelude.NFData ListVoiceProfiles where
  rnf ListVoiceProfiles' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf voiceProfileDomainId

instance Data.ToHeaders ListVoiceProfiles where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListVoiceProfiles where
  toPath = Prelude.const "/voice-profiles"

instance Data.ToQuery ListVoiceProfiles where
  toQuery ListVoiceProfiles' {..} =
    Prelude.mconcat
      [ "max-results" Data.=: maxResults,
        "next-token" Data.=: nextToken,
        "voice-profile-domain-id"
          Data.=: voiceProfileDomainId
      ]

-- | /See:/ 'newListVoiceProfilesResponse' smart constructor.
data ListVoiceProfilesResponse = ListVoiceProfilesResponse'
  { -- | The token used to retrieve the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of voice profiles.
    voiceProfiles :: Prelude.Maybe [VoiceProfileSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListVoiceProfilesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listVoiceProfilesResponse_nextToken' - The token used to retrieve the next page of results.
--
-- 'voiceProfiles', 'listVoiceProfilesResponse_voiceProfiles' - The list of voice profiles.
--
-- 'httpStatus', 'listVoiceProfilesResponse_httpStatus' - The response's http status code.
newListVoiceProfilesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListVoiceProfilesResponse
newListVoiceProfilesResponse pHttpStatus_ =
  ListVoiceProfilesResponse'
    { nextToken =
        Prelude.Nothing,
      voiceProfiles = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token used to retrieve the next page of results.
listVoiceProfilesResponse_nextToken :: Lens.Lens' ListVoiceProfilesResponse (Prelude.Maybe Prelude.Text)
listVoiceProfilesResponse_nextToken = Lens.lens (\ListVoiceProfilesResponse' {nextToken} -> nextToken) (\s@ListVoiceProfilesResponse' {} a -> s {nextToken = a} :: ListVoiceProfilesResponse)

-- | The list of voice profiles.
listVoiceProfilesResponse_voiceProfiles :: Lens.Lens' ListVoiceProfilesResponse (Prelude.Maybe [VoiceProfileSummary])
listVoiceProfilesResponse_voiceProfiles = Lens.lens (\ListVoiceProfilesResponse' {voiceProfiles} -> voiceProfiles) (\s@ListVoiceProfilesResponse' {} a -> s {voiceProfiles = a} :: ListVoiceProfilesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listVoiceProfilesResponse_httpStatus :: Lens.Lens' ListVoiceProfilesResponse Prelude.Int
listVoiceProfilesResponse_httpStatus = Lens.lens (\ListVoiceProfilesResponse' {httpStatus} -> httpStatus) (\s@ListVoiceProfilesResponse' {} a -> s {httpStatus = a} :: ListVoiceProfilesResponse)

instance Prelude.NFData ListVoiceProfilesResponse where
  rnf ListVoiceProfilesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf voiceProfiles
      `Prelude.seq` Prelude.rnf httpStatus
