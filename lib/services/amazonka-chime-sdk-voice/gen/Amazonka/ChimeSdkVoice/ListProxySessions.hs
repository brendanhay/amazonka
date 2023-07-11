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
-- Module      : Amazonka.ChimeSdkVoice.ListProxySessions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- -- | Undocumented operation.
module Amazonka.ChimeSdkVoice.ListProxySessions
  ( -- * Creating a Request
    ListProxySessions (..),
    newListProxySessions,

    -- * Request Lenses
    listProxySessions_maxResults,
    listProxySessions_nextToken,
    listProxySessions_status,
    listProxySessions_voiceConnectorId,

    -- * Destructuring the Response
    ListProxySessionsResponse (..),
    newListProxySessionsResponse,

    -- * Response Lenses
    listProxySessionsResponse_nextToken,
    listProxySessionsResponse_proxySessions,
    listProxySessionsResponse_httpStatus,
  )
where

import Amazonka.ChimeSdkVoice.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListProxySessions' smart constructor.
data ListProxySessions = ListProxySessions'
  { maxResults :: Prelude.Maybe Prelude.Natural,
    nextToken :: Prelude.Maybe Prelude.Text,
    status :: Prelude.Maybe ProxySessionStatus,
    voiceConnectorId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListProxySessions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listProxySessions_maxResults' - Undocumented member.
--
-- 'nextToken', 'listProxySessions_nextToken' - Undocumented member.
--
-- 'status', 'listProxySessions_status' - Undocumented member.
--
-- 'voiceConnectorId', 'listProxySessions_voiceConnectorId' - Undocumented member.
newListProxySessions ::
  -- | 'voiceConnectorId'
  Prelude.Text ->
  ListProxySessions
newListProxySessions pVoiceConnectorId_ =
  ListProxySessions'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      status = Prelude.Nothing,
      voiceConnectorId = pVoiceConnectorId_
    }

-- | Undocumented member.
listProxySessions_maxResults :: Lens.Lens' ListProxySessions (Prelude.Maybe Prelude.Natural)
listProxySessions_maxResults = Lens.lens (\ListProxySessions' {maxResults} -> maxResults) (\s@ListProxySessions' {} a -> s {maxResults = a} :: ListProxySessions)

-- | Undocumented member.
listProxySessions_nextToken :: Lens.Lens' ListProxySessions (Prelude.Maybe Prelude.Text)
listProxySessions_nextToken = Lens.lens (\ListProxySessions' {nextToken} -> nextToken) (\s@ListProxySessions' {} a -> s {nextToken = a} :: ListProxySessions)

-- | Undocumented member.
listProxySessions_status :: Lens.Lens' ListProxySessions (Prelude.Maybe ProxySessionStatus)
listProxySessions_status = Lens.lens (\ListProxySessions' {status} -> status) (\s@ListProxySessions' {} a -> s {status = a} :: ListProxySessions)

-- | Undocumented member.
listProxySessions_voiceConnectorId :: Lens.Lens' ListProxySessions Prelude.Text
listProxySessions_voiceConnectorId = Lens.lens (\ListProxySessions' {voiceConnectorId} -> voiceConnectorId) (\s@ListProxySessions' {} a -> s {voiceConnectorId = a} :: ListProxySessions)

instance Core.AWSRequest ListProxySessions where
  type
    AWSResponse ListProxySessions =
      ListProxySessionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListProxySessionsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "ProxySessions" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListProxySessions where
  hashWithSalt _salt ListProxySessions' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` voiceConnectorId

instance Prelude.NFData ListProxySessions where
  rnf ListProxySessions' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf voiceConnectorId

instance Data.ToHeaders ListProxySessions where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListProxySessions where
  toPath ListProxySessions' {..} =
    Prelude.mconcat
      [ "/voice-connectors/",
        Data.toBS voiceConnectorId,
        "/proxy-sessions"
      ]

instance Data.ToQuery ListProxySessions where
  toQuery ListProxySessions' {..} =
    Prelude.mconcat
      [ "max-results" Data.=: maxResults,
        "next-token" Data.=: nextToken,
        "status" Data.=: status
      ]

-- | /See:/ 'newListProxySessionsResponse' smart constructor.
data ListProxySessionsResponse = ListProxySessionsResponse'
  { nextToken :: Prelude.Maybe Prelude.Text,
    proxySessions :: Prelude.Maybe [ProxySession],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListProxySessionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listProxySessionsResponse_nextToken' - Undocumented member.
--
-- 'proxySessions', 'listProxySessionsResponse_proxySessions' - Undocumented member.
--
-- 'httpStatus', 'listProxySessionsResponse_httpStatus' - The response's http status code.
newListProxySessionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListProxySessionsResponse
newListProxySessionsResponse pHttpStatus_ =
  ListProxySessionsResponse'
    { nextToken =
        Prelude.Nothing,
      proxySessions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
listProxySessionsResponse_nextToken :: Lens.Lens' ListProxySessionsResponse (Prelude.Maybe Prelude.Text)
listProxySessionsResponse_nextToken = Lens.lens (\ListProxySessionsResponse' {nextToken} -> nextToken) (\s@ListProxySessionsResponse' {} a -> s {nextToken = a} :: ListProxySessionsResponse)

-- | Undocumented member.
listProxySessionsResponse_proxySessions :: Lens.Lens' ListProxySessionsResponse (Prelude.Maybe [ProxySession])
listProxySessionsResponse_proxySessions = Lens.lens (\ListProxySessionsResponse' {proxySessions} -> proxySessions) (\s@ListProxySessionsResponse' {} a -> s {proxySessions = a} :: ListProxySessionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listProxySessionsResponse_httpStatus :: Lens.Lens' ListProxySessionsResponse Prelude.Int
listProxySessionsResponse_httpStatus = Lens.lens (\ListProxySessionsResponse' {httpStatus} -> httpStatus) (\s@ListProxySessionsResponse' {} a -> s {httpStatus = a} :: ListProxySessionsResponse)

instance Prelude.NFData ListProxySessionsResponse where
  rnf ListProxySessionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf proxySessions
      `Prelude.seq` Prelude.rnf httpStatus
