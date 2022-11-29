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
-- Module      : Amazonka.Chime.UpdateProxySession
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified proxy session details, such as voice or SMS
-- capabilities.
module Amazonka.Chime.UpdateProxySession
  ( -- * Creating a Request
    UpdateProxySession (..),
    newUpdateProxySession,

    -- * Request Lenses
    updateProxySession_expiryMinutes,
    updateProxySession_capabilities,
    updateProxySession_voiceConnectorId,
    updateProxySession_proxySessionId,

    -- * Destructuring the Response
    UpdateProxySessionResponse (..),
    newUpdateProxySessionResponse,

    -- * Response Lenses
    updateProxySessionResponse_proxySession,
    updateProxySessionResponse_httpStatus,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateProxySession' smart constructor.
data UpdateProxySession = UpdateProxySession'
  { -- | The number of minutes allowed for the proxy session.
    expiryMinutes :: Prelude.Maybe Prelude.Natural,
    -- | The proxy session capabilities.
    capabilities :: [Capability],
    -- | The Amazon Chime voice connector ID.
    voiceConnectorId :: Prelude.Text,
    -- | The proxy session ID.
    proxySessionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateProxySession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expiryMinutes', 'updateProxySession_expiryMinutes' - The number of minutes allowed for the proxy session.
--
-- 'capabilities', 'updateProxySession_capabilities' - The proxy session capabilities.
--
-- 'voiceConnectorId', 'updateProxySession_voiceConnectorId' - The Amazon Chime voice connector ID.
--
-- 'proxySessionId', 'updateProxySession_proxySessionId' - The proxy session ID.
newUpdateProxySession ::
  -- | 'voiceConnectorId'
  Prelude.Text ->
  -- | 'proxySessionId'
  Prelude.Text ->
  UpdateProxySession
newUpdateProxySession
  pVoiceConnectorId_
  pProxySessionId_ =
    UpdateProxySession'
      { expiryMinutes =
          Prelude.Nothing,
        capabilities = Prelude.mempty,
        voiceConnectorId = pVoiceConnectorId_,
        proxySessionId = pProxySessionId_
      }

-- | The number of minutes allowed for the proxy session.
updateProxySession_expiryMinutes :: Lens.Lens' UpdateProxySession (Prelude.Maybe Prelude.Natural)
updateProxySession_expiryMinutes = Lens.lens (\UpdateProxySession' {expiryMinutes} -> expiryMinutes) (\s@UpdateProxySession' {} a -> s {expiryMinutes = a} :: UpdateProxySession)

-- | The proxy session capabilities.
updateProxySession_capabilities :: Lens.Lens' UpdateProxySession [Capability]
updateProxySession_capabilities = Lens.lens (\UpdateProxySession' {capabilities} -> capabilities) (\s@UpdateProxySession' {} a -> s {capabilities = a} :: UpdateProxySession) Prelude.. Lens.coerced

-- | The Amazon Chime voice connector ID.
updateProxySession_voiceConnectorId :: Lens.Lens' UpdateProxySession Prelude.Text
updateProxySession_voiceConnectorId = Lens.lens (\UpdateProxySession' {voiceConnectorId} -> voiceConnectorId) (\s@UpdateProxySession' {} a -> s {voiceConnectorId = a} :: UpdateProxySession)

-- | The proxy session ID.
updateProxySession_proxySessionId :: Lens.Lens' UpdateProxySession Prelude.Text
updateProxySession_proxySessionId = Lens.lens (\UpdateProxySession' {proxySessionId} -> proxySessionId) (\s@UpdateProxySession' {} a -> s {proxySessionId = a} :: UpdateProxySession)

instance Core.AWSRequest UpdateProxySession where
  type
    AWSResponse UpdateProxySession =
      UpdateProxySessionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateProxySessionResponse'
            Prelude.<$> (x Core..?> "ProxySession")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateProxySession where
  hashWithSalt _salt UpdateProxySession' {..} =
    _salt `Prelude.hashWithSalt` expiryMinutes
      `Prelude.hashWithSalt` capabilities
      `Prelude.hashWithSalt` voiceConnectorId
      `Prelude.hashWithSalt` proxySessionId

instance Prelude.NFData UpdateProxySession where
  rnf UpdateProxySession' {..} =
    Prelude.rnf expiryMinutes
      `Prelude.seq` Prelude.rnf capabilities
      `Prelude.seq` Prelude.rnf voiceConnectorId
      `Prelude.seq` Prelude.rnf proxySessionId

instance Core.ToHeaders UpdateProxySession where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON UpdateProxySession where
  toJSON UpdateProxySession' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ExpiryMinutes" Core..=) Prelude.<$> expiryMinutes,
            Prelude.Just ("Capabilities" Core..= capabilities)
          ]
      )

instance Core.ToPath UpdateProxySession where
  toPath UpdateProxySession' {..} =
    Prelude.mconcat
      [ "/voice-connectors/",
        Core.toBS voiceConnectorId,
        "/proxy-sessions/",
        Core.toBS proxySessionId
      ]

instance Core.ToQuery UpdateProxySession where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateProxySessionResponse' smart constructor.
data UpdateProxySessionResponse = UpdateProxySessionResponse'
  { -- | The proxy session details.
    proxySession :: Prelude.Maybe ProxySession,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateProxySessionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'proxySession', 'updateProxySessionResponse_proxySession' - The proxy session details.
--
-- 'httpStatus', 'updateProxySessionResponse_httpStatus' - The response's http status code.
newUpdateProxySessionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateProxySessionResponse
newUpdateProxySessionResponse pHttpStatus_ =
  UpdateProxySessionResponse'
    { proxySession =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The proxy session details.
updateProxySessionResponse_proxySession :: Lens.Lens' UpdateProxySessionResponse (Prelude.Maybe ProxySession)
updateProxySessionResponse_proxySession = Lens.lens (\UpdateProxySessionResponse' {proxySession} -> proxySession) (\s@UpdateProxySessionResponse' {} a -> s {proxySession = a} :: UpdateProxySessionResponse)

-- | The response's http status code.
updateProxySessionResponse_httpStatus :: Lens.Lens' UpdateProxySessionResponse Prelude.Int
updateProxySessionResponse_httpStatus = Lens.lens (\UpdateProxySessionResponse' {httpStatus} -> httpStatus) (\s@UpdateProxySessionResponse' {} a -> s {httpStatus = a} :: UpdateProxySessionResponse)

instance Prelude.NFData UpdateProxySessionResponse where
  rnf UpdateProxySessionResponse' {..} =
    Prelude.rnf proxySession
      `Prelude.seq` Prelude.rnf httpStatus
