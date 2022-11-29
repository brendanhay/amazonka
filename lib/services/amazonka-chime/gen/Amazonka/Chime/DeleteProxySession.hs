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
-- Module      : Amazonka.Chime.DeleteProxySession
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified proxy session from the specified Amazon Chime
-- Voice Connector.
module Amazonka.Chime.DeleteProxySession
  ( -- * Creating a Request
    DeleteProxySession (..),
    newDeleteProxySession,

    -- * Request Lenses
    deleteProxySession_voiceConnectorId,
    deleteProxySession_proxySessionId,

    -- * Destructuring the Response
    DeleteProxySessionResponse (..),
    newDeleteProxySessionResponse,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteProxySession' smart constructor.
data DeleteProxySession = DeleteProxySession'
  { -- | The Amazon Chime voice connector ID.
    voiceConnectorId :: Prelude.Text,
    -- | The proxy session ID.
    proxySessionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteProxySession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'voiceConnectorId', 'deleteProxySession_voiceConnectorId' - The Amazon Chime voice connector ID.
--
-- 'proxySessionId', 'deleteProxySession_proxySessionId' - The proxy session ID.
newDeleteProxySession ::
  -- | 'voiceConnectorId'
  Prelude.Text ->
  -- | 'proxySessionId'
  Prelude.Text ->
  DeleteProxySession
newDeleteProxySession
  pVoiceConnectorId_
  pProxySessionId_ =
    DeleteProxySession'
      { voiceConnectorId =
          pVoiceConnectorId_,
        proxySessionId = pProxySessionId_
      }

-- | The Amazon Chime voice connector ID.
deleteProxySession_voiceConnectorId :: Lens.Lens' DeleteProxySession Prelude.Text
deleteProxySession_voiceConnectorId = Lens.lens (\DeleteProxySession' {voiceConnectorId} -> voiceConnectorId) (\s@DeleteProxySession' {} a -> s {voiceConnectorId = a} :: DeleteProxySession)

-- | The proxy session ID.
deleteProxySession_proxySessionId :: Lens.Lens' DeleteProxySession Prelude.Text
deleteProxySession_proxySessionId = Lens.lens (\DeleteProxySession' {proxySessionId} -> proxySessionId) (\s@DeleteProxySession' {} a -> s {proxySessionId = a} :: DeleteProxySession)

instance Core.AWSRequest DeleteProxySession where
  type
    AWSResponse DeleteProxySession =
      DeleteProxySessionResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteProxySessionResponse'

instance Prelude.Hashable DeleteProxySession where
  hashWithSalt _salt DeleteProxySession' {..} =
    _salt `Prelude.hashWithSalt` voiceConnectorId
      `Prelude.hashWithSalt` proxySessionId

instance Prelude.NFData DeleteProxySession where
  rnf DeleteProxySession' {..} =
    Prelude.rnf voiceConnectorId
      `Prelude.seq` Prelude.rnf proxySessionId

instance Core.ToHeaders DeleteProxySession where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DeleteProxySession where
  toPath DeleteProxySession' {..} =
    Prelude.mconcat
      [ "/voice-connectors/",
        Core.toBS voiceConnectorId,
        "/proxy-sessions/",
        Core.toBS proxySessionId
      ]

instance Core.ToQuery DeleteProxySession where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteProxySessionResponse' smart constructor.
data DeleteProxySessionResponse = DeleteProxySessionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteProxySessionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteProxySessionResponse ::
  DeleteProxySessionResponse
newDeleteProxySessionResponse =
  DeleteProxySessionResponse'

instance Prelude.NFData DeleteProxySessionResponse where
  rnf _ = ()
