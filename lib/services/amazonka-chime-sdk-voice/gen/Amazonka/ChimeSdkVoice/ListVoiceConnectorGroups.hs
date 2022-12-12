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
-- Module      : Amazonka.ChimeSdkVoice.ListVoiceConnectorGroups
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- -- | Undocumented operation.
module Amazonka.ChimeSdkVoice.ListVoiceConnectorGroups
  ( -- * Creating a Request
    ListVoiceConnectorGroups (..),
    newListVoiceConnectorGroups,

    -- * Request Lenses
    listVoiceConnectorGroups_maxResults,
    listVoiceConnectorGroups_nextToken,

    -- * Destructuring the Response
    ListVoiceConnectorGroupsResponse (..),
    newListVoiceConnectorGroupsResponse,

    -- * Response Lenses
    listVoiceConnectorGroupsResponse_nextToken,
    listVoiceConnectorGroupsResponse_voiceConnectorGroups,
    listVoiceConnectorGroupsResponse_httpStatus,
  )
where

import Amazonka.ChimeSdkVoice.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListVoiceConnectorGroups' smart constructor.
data ListVoiceConnectorGroups = ListVoiceConnectorGroups'
  { maxResults :: Prelude.Maybe Prelude.Natural,
    nextToken :: Prelude.Maybe Prelude.Text
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
-- 'maxResults', 'listVoiceConnectorGroups_maxResults' - Undocumented member.
--
-- 'nextToken', 'listVoiceConnectorGroups_nextToken' - Undocumented member.
newListVoiceConnectorGroups ::
  ListVoiceConnectorGroups
newListVoiceConnectorGroups =
  ListVoiceConnectorGroups'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Undocumented member.
listVoiceConnectorGroups_maxResults :: Lens.Lens' ListVoiceConnectorGroups (Prelude.Maybe Prelude.Natural)
listVoiceConnectorGroups_maxResults = Lens.lens (\ListVoiceConnectorGroups' {maxResults} -> maxResults) (\s@ListVoiceConnectorGroups' {} a -> s {maxResults = a} :: ListVoiceConnectorGroups)

-- | Undocumented member.
listVoiceConnectorGroups_nextToken :: Lens.Lens' ListVoiceConnectorGroups (Prelude.Maybe Prelude.Text)
listVoiceConnectorGroups_nextToken = Lens.lens (\ListVoiceConnectorGroups' {nextToken} -> nextToken) (\s@ListVoiceConnectorGroups' {} a -> s {nextToken = a} :: ListVoiceConnectorGroups)

instance Core.AWSRequest ListVoiceConnectorGroups where
  type
    AWSResponse ListVoiceConnectorGroups =
      ListVoiceConnectorGroupsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListVoiceConnectorGroupsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x Data..?> "VoiceConnectorGroups"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListVoiceConnectorGroups where
  hashWithSalt _salt ListVoiceConnectorGroups' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListVoiceConnectorGroups where
  rnf ListVoiceConnectorGroups' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListVoiceConnectorGroups where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListVoiceConnectorGroups where
  toPath = Prelude.const "/voice-connector-groups"

instance Data.ToQuery ListVoiceConnectorGroups where
  toQuery ListVoiceConnectorGroups' {..} =
    Prelude.mconcat
      [ "max-results" Data.=: maxResults,
        "next-token" Data.=: nextToken
      ]

-- | /See:/ 'newListVoiceConnectorGroupsResponse' smart constructor.
data ListVoiceConnectorGroupsResponse = ListVoiceConnectorGroupsResponse'
  { nextToken :: Prelude.Maybe Prelude.Text,
    voiceConnectorGroups :: Prelude.Maybe [VoiceConnectorGroup],
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
-- 'nextToken', 'listVoiceConnectorGroupsResponse_nextToken' - Undocumented member.
--
-- 'voiceConnectorGroups', 'listVoiceConnectorGroupsResponse_voiceConnectorGroups' - Undocumented member.
--
-- 'httpStatus', 'listVoiceConnectorGroupsResponse_httpStatus' - The response's http status code.
newListVoiceConnectorGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListVoiceConnectorGroupsResponse
newListVoiceConnectorGroupsResponse pHttpStatus_ =
  ListVoiceConnectorGroupsResponse'
    { nextToken =
        Prelude.Nothing,
      voiceConnectorGroups = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
listVoiceConnectorGroupsResponse_nextToken :: Lens.Lens' ListVoiceConnectorGroupsResponse (Prelude.Maybe Prelude.Text)
listVoiceConnectorGroupsResponse_nextToken = Lens.lens (\ListVoiceConnectorGroupsResponse' {nextToken} -> nextToken) (\s@ListVoiceConnectorGroupsResponse' {} a -> s {nextToken = a} :: ListVoiceConnectorGroupsResponse)

-- | Undocumented member.
listVoiceConnectorGroupsResponse_voiceConnectorGroups :: Lens.Lens' ListVoiceConnectorGroupsResponse (Prelude.Maybe [VoiceConnectorGroup])
listVoiceConnectorGroupsResponse_voiceConnectorGroups = Lens.lens (\ListVoiceConnectorGroupsResponse' {voiceConnectorGroups} -> voiceConnectorGroups) (\s@ListVoiceConnectorGroupsResponse' {} a -> s {voiceConnectorGroups = a} :: ListVoiceConnectorGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listVoiceConnectorGroupsResponse_httpStatus :: Lens.Lens' ListVoiceConnectorGroupsResponse Prelude.Int
listVoiceConnectorGroupsResponse_httpStatus = Lens.lens (\ListVoiceConnectorGroupsResponse' {httpStatus} -> httpStatus) (\s@ListVoiceConnectorGroupsResponse' {} a -> s {httpStatus = a} :: ListVoiceConnectorGroupsResponse)

instance
  Prelude.NFData
    ListVoiceConnectorGroupsResponse
  where
  rnf ListVoiceConnectorGroupsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf voiceConnectorGroups
      `Prelude.seq` Prelude.rnf httpStatus
