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
-- Module      : Amazonka.ChimeSdkVoice.DeleteVoiceConnector
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- -- | Undocumented operation.
module Amazonka.ChimeSdkVoice.DeleteVoiceConnector
  ( -- * Creating a Request
    DeleteVoiceConnector (..),
    newDeleteVoiceConnector,

    -- * Request Lenses
    deleteVoiceConnector_voiceConnectorId,

    -- * Destructuring the Response
    DeleteVoiceConnectorResponse (..),
    newDeleteVoiceConnectorResponse,
  )
where

import Amazonka.ChimeSdkVoice.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteVoiceConnector' smart constructor.
data DeleteVoiceConnector = DeleteVoiceConnector'
  { voiceConnectorId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVoiceConnector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'voiceConnectorId', 'deleteVoiceConnector_voiceConnectorId' - Undocumented member.
newDeleteVoiceConnector ::
  -- | 'voiceConnectorId'
  Prelude.Text ->
  DeleteVoiceConnector
newDeleteVoiceConnector pVoiceConnectorId_ =
  DeleteVoiceConnector'
    { voiceConnectorId =
        pVoiceConnectorId_
    }

-- | Undocumented member.
deleteVoiceConnector_voiceConnectorId :: Lens.Lens' DeleteVoiceConnector Prelude.Text
deleteVoiceConnector_voiceConnectorId = Lens.lens (\DeleteVoiceConnector' {voiceConnectorId} -> voiceConnectorId) (\s@DeleteVoiceConnector' {} a -> s {voiceConnectorId = a} :: DeleteVoiceConnector)

instance Core.AWSRequest DeleteVoiceConnector where
  type
    AWSResponse DeleteVoiceConnector =
      DeleteVoiceConnectorResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteVoiceConnectorResponse'

instance Prelude.Hashable DeleteVoiceConnector where
  hashWithSalt _salt DeleteVoiceConnector' {..} =
    _salt `Prelude.hashWithSalt` voiceConnectorId

instance Prelude.NFData DeleteVoiceConnector where
  rnf DeleteVoiceConnector' {..} =
    Prelude.rnf voiceConnectorId

instance Data.ToHeaders DeleteVoiceConnector where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteVoiceConnector where
  toPath DeleteVoiceConnector' {..} =
    Prelude.mconcat
      ["/voice-connectors/", Data.toBS voiceConnectorId]

instance Data.ToQuery DeleteVoiceConnector where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteVoiceConnectorResponse' smart constructor.
data DeleteVoiceConnectorResponse = DeleteVoiceConnectorResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVoiceConnectorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteVoiceConnectorResponse ::
  DeleteVoiceConnectorResponse
newDeleteVoiceConnectorResponse =
  DeleteVoiceConnectorResponse'

instance Prelude.NFData DeleteVoiceConnectorResponse where
  rnf _ = ()
