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
-- Module      : Amazonka.ChimeSdkVoice.DeleteProxySession
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- -- | Undocumented operation.
module Amazonka.ChimeSdkVoice.DeleteProxySession
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

import Amazonka.ChimeSdkVoice.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteProxySession' smart constructor.
data DeleteProxySession = DeleteProxySession'
  { voiceConnectorId :: Prelude.Text,
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
-- 'voiceConnectorId', 'deleteProxySession_voiceConnectorId' - Undocumented member.
--
-- 'proxySessionId', 'deleteProxySession_proxySessionId' - Undocumented member.
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

-- | Undocumented member.
deleteProxySession_voiceConnectorId :: Lens.Lens' DeleteProxySession Prelude.Text
deleteProxySession_voiceConnectorId = Lens.lens (\DeleteProxySession' {voiceConnectorId} -> voiceConnectorId) (\s@DeleteProxySession' {} a -> s {voiceConnectorId = a} :: DeleteProxySession)

-- | Undocumented member.
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

instance Data.ToHeaders DeleteProxySession where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteProxySession where
  toPath DeleteProxySession' {..} =
    Prelude.mconcat
      [ "/voice-connectors/",
        Data.toBS voiceConnectorId,
        "/proxy-sessions/",
        Data.toBS proxySessionId
      ]

instance Data.ToQuery DeleteProxySession where
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
