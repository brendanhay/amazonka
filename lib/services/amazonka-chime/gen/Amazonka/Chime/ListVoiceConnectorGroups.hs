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
-- Module      : Amazonka.Chime.ListVoiceConnectorGroups
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the Amazon Chime Voice Connector groups for the administrator\'s
-- AWS account.
module Amazonka.Chime.ListVoiceConnectorGroups
  ( -- * Creating a Request
    ListVoiceConnectorGroups (..),
    newListVoiceConnectorGroups,

    -- * Request Lenses
    listVoiceConnectorGroups_nextToken,
    listVoiceConnectorGroups_maxResults,

    -- * Destructuring the Response
    ListVoiceConnectorGroupsResponse (..),
    newListVoiceConnectorGroupsResponse,

    -- * Response Lenses
    listVoiceConnectorGroupsResponse_voiceConnectorGroups,
    listVoiceConnectorGroupsResponse_nextToken,
    listVoiceConnectorGroupsResponse_httpStatus,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListVoiceConnectorGroups' smart constructor.
data ListVoiceConnectorGroups = ListVoiceConnectorGroups'
  { -- | The token to use to retrieve the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return in a single call.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListVoiceConnectorGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listVoiceConnectorGroups_nextToken' - The token to use to retrieve the next page of results.
--
-- 'maxResults', 'listVoiceConnectorGroups_maxResults' - The maximum number of results to return in a single call.
newListVoiceConnectorGroups ::
  ListVoiceConnectorGroups
newListVoiceConnectorGroups =
  ListVoiceConnectorGroups'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The token to use to retrieve the next page of results.
listVoiceConnectorGroups_nextToken :: Lens.Lens' ListVoiceConnectorGroups (Prelude.Maybe Prelude.Text)
listVoiceConnectorGroups_nextToken = Lens.lens (\ListVoiceConnectorGroups' {nextToken} -> nextToken) (\s@ListVoiceConnectorGroups' {} a -> s {nextToken = a} :: ListVoiceConnectorGroups)

-- | The maximum number of results to return in a single call.
listVoiceConnectorGroups_maxResults :: Lens.Lens' ListVoiceConnectorGroups (Prelude.Maybe Prelude.Natural)
listVoiceConnectorGroups_maxResults = Lens.lens (\ListVoiceConnectorGroups' {maxResults} -> maxResults) (\s@ListVoiceConnectorGroups' {} a -> s {maxResults = a} :: ListVoiceConnectorGroups)

instance Core.AWSRequest ListVoiceConnectorGroups where
  type
    AWSResponse ListVoiceConnectorGroups =
      ListVoiceConnectorGroupsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListVoiceConnectorGroupsResponse'
            Prelude.<$> ( x Core..?> "VoiceConnectorGroups"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListVoiceConnectorGroups

instance Prelude.NFData ListVoiceConnectorGroups

instance Core.ToHeaders ListVoiceConnectorGroups where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListVoiceConnectorGroups where
  toPath = Prelude.const "/voice-connector-groups"

instance Core.ToQuery ListVoiceConnectorGroups where
  toQuery ListVoiceConnectorGroups' {..} =
    Prelude.mconcat
      [ "next-token" Core.=: nextToken,
        "max-results" Core.=: maxResults
      ]

-- | /See:/ 'newListVoiceConnectorGroupsResponse' smart constructor.
data ListVoiceConnectorGroupsResponse = ListVoiceConnectorGroupsResponse'
  { -- | The details of the Amazon Chime Voice Connector groups.
    voiceConnectorGroups :: Prelude.Maybe [VoiceConnectorGroup],
    -- | The token to use to retrieve the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListVoiceConnectorGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'voiceConnectorGroups', 'listVoiceConnectorGroupsResponse_voiceConnectorGroups' - The details of the Amazon Chime Voice Connector groups.
--
-- 'nextToken', 'listVoiceConnectorGroupsResponse_nextToken' - The token to use to retrieve the next page of results.
--
-- 'httpStatus', 'listVoiceConnectorGroupsResponse_httpStatus' - The response's http status code.
newListVoiceConnectorGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListVoiceConnectorGroupsResponse
newListVoiceConnectorGroupsResponse pHttpStatus_ =
  ListVoiceConnectorGroupsResponse'
    { voiceConnectorGroups =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The details of the Amazon Chime Voice Connector groups.
listVoiceConnectorGroupsResponse_voiceConnectorGroups :: Lens.Lens' ListVoiceConnectorGroupsResponse (Prelude.Maybe [VoiceConnectorGroup])
listVoiceConnectorGroupsResponse_voiceConnectorGroups = Lens.lens (\ListVoiceConnectorGroupsResponse' {voiceConnectorGroups} -> voiceConnectorGroups) (\s@ListVoiceConnectorGroupsResponse' {} a -> s {voiceConnectorGroups = a} :: ListVoiceConnectorGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to retrieve the next page of results.
listVoiceConnectorGroupsResponse_nextToken :: Lens.Lens' ListVoiceConnectorGroupsResponse (Prelude.Maybe Prelude.Text)
listVoiceConnectorGroupsResponse_nextToken = Lens.lens (\ListVoiceConnectorGroupsResponse' {nextToken} -> nextToken) (\s@ListVoiceConnectorGroupsResponse' {} a -> s {nextToken = a} :: ListVoiceConnectorGroupsResponse)

-- | The response's http status code.
listVoiceConnectorGroupsResponse_httpStatus :: Lens.Lens' ListVoiceConnectorGroupsResponse Prelude.Int
listVoiceConnectorGroupsResponse_httpStatus = Lens.lens (\ListVoiceConnectorGroupsResponse' {httpStatus} -> httpStatus) (\s@ListVoiceConnectorGroupsResponse' {} a -> s {httpStatus = a} :: ListVoiceConnectorGroupsResponse)

instance
  Prelude.NFData
    ListVoiceConnectorGroupsResponse
