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
-- Module      : Amazonka.ChimeSdkVoice.GetProxySession
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- -- | Undocumented operation.
module Amazonka.ChimeSdkVoice.GetProxySession
  ( -- * Creating a Request
    GetProxySession (..),
    newGetProxySession,

    -- * Request Lenses
    getProxySession_voiceConnectorId,
    getProxySession_proxySessionId,

    -- * Destructuring the Response
    GetProxySessionResponse (..),
    newGetProxySessionResponse,

    -- * Response Lenses
    getProxySessionResponse_proxySession,
    getProxySessionResponse_httpStatus,
  )
where

import Amazonka.ChimeSdkVoice.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetProxySession' smart constructor.
data GetProxySession = GetProxySession'
  { voiceConnectorId :: Prelude.Text,
    proxySessionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetProxySession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'voiceConnectorId', 'getProxySession_voiceConnectorId' - Undocumented member.
--
-- 'proxySessionId', 'getProxySession_proxySessionId' - Undocumented member.
newGetProxySession ::
  -- | 'voiceConnectorId'
  Prelude.Text ->
  -- | 'proxySessionId'
  Prelude.Text ->
  GetProxySession
newGetProxySession
  pVoiceConnectorId_
  pProxySessionId_ =
    GetProxySession'
      { voiceConnectorId =
          pVoiceConnectorId_,
        proxySessionId = pProxySessionId_
      }

-- | Undocumented member.
getProxySession_voiceConnectorId :: Lens.Lens' GetProxySession Prelude.Text
getProxySession_voiceConnectorId = Lens.lens (\GetProxySession' {voiceConnectorId} -> voiceConnectorId) (\s@GetProxySession' {} a -> s {voiceConnectorId = a} :: GetProxySession)

-- | Undocumented member.
getProxySession_proxySessionId :: Lens.Lens' GetProxySession Prelude.Text
getProxySession_proxySessionId = Lens.lens (\GetProxySession' {proxySessionId} -> proxySessionId) (\s@GetProxySession' {} a -> s {proxySessionId = a} :: GetProxySession)

instance Core.AWSRequest GetProxySession where
  type
    AWSResponse GetProxySession =
      GetProxySessionResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetProxySessionResponse'
            Prelude.<$> (x Data..?> "ProxySession")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetProxySession where
  hashWithSalt _salt GetProxySession' {..} =
    _salt
      `Prelude.hashWithSalt` voiceConnectorId
      `Prelude.hashWithSalt` proxySessionId

instance Prelude.NFData GetProxySession where
  rnf GetProxySession' {..} =
    Prelude.rnf voiceConnectorId
      `Prelude.seq` Prelude.rnf proxySessionId

instance Data.ToHeaders GetProxySession where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetProxySession where
  toPath GetProxySession' {..} =
    Prelude.mconcat
      [ "/voice-connectors/",
        Data.toBS voiceConnectorId,
        "/proxy-sessions/",
        Data.toBS proxySessionId
      ]

instance Data.ToQuery GetProxySession where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetProxySessionResponse' smart constructor.
data GetProxySessionResponse = GetProxySessionResponse'
  { proxySession :: Prelude.Maybe ProxySession,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetProxySessionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'proxySession', 'getProxySessionResponse_proxySession' - Undocumented member.
--
-- 'httpStatus', 'getProxySessionResponse_httpStatus' - The response's http status code.
newGetProxySessionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetProxySessionResponse
newGetProxySessionResponse pHttpStatus_ =
  GetProxySessionResponse'
    { proxySession =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
getProxySessionResponse_proxySession :: Lens.Lens' GetProxySessionResponse (Prelude.Maybe ProxySession)
getProxySessionResponse_proxySession = Lens.lens (\GetProxySessionResponse' {proxySession} -> proxySession) (\s@GetProxySessionResponse' {} a -> s {proxySession = a} :: GetProxySessionResponse)

-- | The response's http status code.
getProxySessionResponse_httpStatus :: Lens.Lens' GetProxySessionResponse Prelude.Int
getProxySessionResponse_httpStatus = Lens.lens (\GetProxySessionResponse' {httpStatus} -> httpStatus) (\s@GetProxySessionResponse' {} a -> s {httpStatus = a} :: GetProxySessionResponse)

instance Prelude.NFData GetProxySessionResponse where
  rnf GetProxySessionResponse' {..} =
    Prelude.rnf proxySession
      `Prelude.seq` Prelude.rnf httpStatus
