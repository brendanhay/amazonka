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
-- Module      : Amazonka.ChimeSdkVoice.DeleteVoiceConnectorProxy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- -- | Undocumented operation.
module Amazonka.ChimeSdkVoice.DeleteVoiceConnectorProxy
  ( -- * Creating a Request
    DeleteVoiceConnectorProxy (..),
    newDeleteVoiceConnectorProxy,

    -- * Request Lenses
    deleteVoiceConnectorProxy_voiceConnectorId,

    -- * Destructuring the Response
    DeleteVoiceConnectorProxyResponse (..),
    newDeleteVoiceConnectorProxyResponse,
  )
where

import Amazonka.ChimeSdkVoice.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteVoiceConnectorProxy' smart constructor.
data DeleteVoiceConnectorProxy = DeleteVoiceConnectorProxy'
  { voiceConnectorId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVoiceConnectorProxy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'voiceConnectorId', 'deleteVoiceConnectorProxy_voiceConnectorId' - Undocumented member.
newDeleteVoiceConnectorProxy ::
  -- | 'voiceConnectorId'
  Prelude.Text ->
  DeleteVoiceConnectorProxy
newDeleteVoiceConnectorProxy pVoiceConnectorId_ =
  DeleteVoiceConnectorProxy'
    { voiceConnectorId =
        pVoiceConnectorId_
    }

-- | Undocumented member.
deleteVoiceConnectorProxy_voiceConnectorId :: Lens.Lens' DeleteVoiceConnectorProxy Prelude.Text
deleteVoiceConnectorProxy_voiceConnectorId = Lens.lens (\DeleteVoiceConnectorProxy' {voiceConnectorId} -> voiceConnectorId) (\s@DeleteVoiceConnectorProxy' {} a -> s {voiceConnectorId = a} :: DeleteVoiceConnectorProxy)

instance Core.AWSRequest DeleteVoiceConnectorProxy where
  type
    AWSResponse DeleteVoiceConnectorProxy =
      DeleteVoiceConnectorProxyResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull
      DeleteVoiceConnectorProxyResponse'

instance Prelude.Hashable DeleteVoiceConnectorProxy where
  hashWithSalt _salt DeleteVoiceConnectorProxy' {..} =
    _salt `Prelude.hashWithSalt` voiceConnectorId

instance Prelude.NFData DeleteVoiceConnectorProxy where
  rnf DeleteVoiceConnectorProxy' {..} =
    Prelude.rnf voiceConnectorId

instance Data.ToHeaders DeleteVoiceConnectorProxy where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteVoiceConnectorProxy where
  toPath DeleteVoiceConnectorProxy' {..} =
    Prelude.mconcat
      [ "/voice-connectors/",
        Data.toBS voiceConnectorId,
        "/programmable-numbers/proxy"
      ]

instance Data.ToQuery DeleteVoiceConnectorProxy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteVoiceConnectorProxyResponse' smart constructor.
data DeleteVoiceConnectorProxyResponse = DeleteVoiceConnectorProxyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVoiceConnectorProxyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteVoiceConnectorProxyResponse ::
  DeleteVoiceConnectorProxyResponse
newDeleteVoiceConnectorProxyResponse =
  DeleteVoiceConnectorProxyResponse'

instance
  Prelude.NFData
    DeleteVoiceConnectorProxyResponse
  where
  rnf _ = ()
