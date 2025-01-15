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
-- Module      : Amazonka.ChimeSdkVoice.GetVoiceConnectorProxy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- -- | Undocumented operation.
module Amazonka.ChimeSdkVoice.GetVoiceConnectorProxy
  ( -- * Creating a Request
    GetVoiceConnectorProxy (..),
    newGetVoiceConnectorProxy,

    -- * Request Lenses
    getVoiceConnectorProxy_voiceConnectorId,

    -- * Destructuring the Response
    GetVoiceConnectorProxyResponse (..),
    newGetVoiceConnectorProxyResponse,

    -- * Response Lenses
    getVoiceConnectorProxyResponse_proxy,
    getVoiceConnectorProxyResponse_httpStatus,
  )
where

import Amazonka.ChimeSdkVoice.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetVoiceConnectorProxy' smart constructor.
data GetVoiceConnectorProxy = GetVoiceConnectorProxy'
  { voiceConnectorId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetVoiceConnectorProxy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'voiceConnectorId', 'getVoiceConnectorProxy_voiceConnectorId' - Undocumented member.
newGetVoiceConnectorProxy ::
  -- | 'voiceConnectorId'
  Prelude.Text ->
  GetVoiceConnectorProxy
newGetVoiceConnectorProxy pVoiceConnectorId_ =
  GetVoiceConnectorProxy'
    { voiceConnectorId =
        pVoiceConnectorId_
    }

-- | Undocumented member.
getVoiceConnectorProxy_voiceConnectorId :: Lens.Lens' GetVoiceConnectorProxy Prelude.Text
getVoiceConnectorProxy_voiceConnectorId = Lens.lens (\GetVoiceConnectorProxy' {voiceConnectorId} -> voiceConnectorId) (\s@GetVoiceConnectorProxy' {} a -> s {voiceConnectorId = a} :: GetVoiceConnectorProxy)

instance Core.AWSRequest GetVoiceConnectorProxy where
  type
    AWSResponse GetVoiceConnectorProxy =
      GetVoiceConnectorProxyResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetVoiceConnectorProxyResponse'
            Prelude.<$> (x Data..?> "Proxy")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetVoiceConnectorProxy where
  hashWithSalt _salt GetVoiceConnectorProxy' {..} =
    _salt `Prelude.hashWithSalt` voiceConnectorId

instance Prelude.NFData GetVoiceConnectorProxy where
  rnf GetVoiceConnectorProxy' {..} =
    Prelude.rnf voiceConnectorId

instance Data.ToHeaders GetVoiceConnectorProxy where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetVoiceConnectorProxy where
  toPath GetVoiceConnectorProxy' {..} =
    Prelude.mconcat
      [ "/voice-connectors/",
        Data.toBS voiceConnectorId,
        "/programmable-numbers/proxy"
      ]

instance Data.ToQuery GetVoiceConnectorProxy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetVoiceConnectorProxyResponse' smart constructor.
data GetVoiceConnectorProxyResponse = GetVoiceConnectorProxyResponse'
  { proxy :: Prelude.Maybe Proxy,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetVoiceConnectorProxyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'proxy', 'getVoiceConnectorProxyResponse_proxy' - Undocumented member.
--
-- 'httpStatus', 'getVoiceConnectorProxyResponse_httpStatus' - The response's http status code.
newGetVoiceConnectorProxyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetVoiceConnectorProxyResponse
newGetVoiceConnectorProxyResponse pHttpStatus_ =
  GetVoiceConnectorProxyResponse'
    { proxy =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
getVoiceConnectorProxyResponse_proxy :: Lens.Lens' GetVoiceConnectorProxyResponse (Prelude.Maybe Proxy)
getVoiceConnectorProxyResponse_proxy = Lens.lens (\GetVoiceConnectorProxyResponse' {proxy} -> proxy) (\s@GetVoiceConnectorProxyResponse' {} a -> s {proxy = a} :: GetVoiceConnectorProxyResponse)

-- | The response's http status code.
getVoiceConnectorProxyResponse_httpStatus :: Lens.Lens' GetVoiceConnectorProxyResponse Prelude.Int
getVoiceConnectorProxyResponse_httpStatus = Lens.lens (\GetVoiceConnectorProxyResponse' {httpStatus} -> httpStatus) (\s@GetVoiceConnectorProxyResponse' {} a -> s {httpStatus = a} :: GetVoiceConnectorProxyResponse)

instance
  Prelude.NFData
    GetVoiceConnectorProxyResponse
  where
  rnf GetVoiceConnectorProxyResponse' {..} =
    Prelude.rnf proxy `Prelude.seq`
      Prelude.rnf httpStatus
