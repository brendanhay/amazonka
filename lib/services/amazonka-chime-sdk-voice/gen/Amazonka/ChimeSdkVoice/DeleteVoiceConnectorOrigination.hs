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
-- Module      : Amazonka.ChimeSdkVoice.DeleteVoiceConnectorOrigination
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the origination settings for the specified Amazon Chime SDK
-- Voice Connector.
--
-- If emergency calling is configured for the Voice Connector, it must be
-- deleted prior to deleting the origination settings.
module Amazonka.ChimeSdkVoice.DeleteVoiceConnectorOrigination
  ( -- * Creating a Request
    DeleteVoiceConnectorOrigination (..),
    newDeleteVoiceConnectorOrigination,

    -- * Request Lenses
    deleteVoiceConnectorOrigination_voiceConnectorId,

    -- * Destructuring the Response
    DeleteVoiceConnectorOriginationResponse (..),
    newDeleteVoiceConnectorOriginationResponse,
  )
where

import Amazonka.ChimeSdkVoice.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteVoiceConnectorOrigination' smart constructor.
data DeleteVoiceConnectorOrigination = DeleteVoiceConnectorOrigination'
  { -- | The Voice Connector ID.
    voiceConnectorId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVoiceConnectorOrigination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'voiceConnectorId', 'deleteVoiceConnectorOrigination_voiceConnectorId' - The Voice Connector ID.
newDeleteVoiceConnectorOrigination ::
  -- | 'voiceConnectorId'
  Prelude.Text ->
  DeleteVoiceConnectorOrigination
newDeleteVoiceConnectorOrigination pVoiceConnectorId_ =
  DeleteVoiceConnectorOrigination'
    { voiceConnectorId =
        pVoiceConnectorId_
    }

-- | The Voice Connector ID.
deleteVoiceConnectorOrigination_voiceConnectorId :: Lens.Lens' DeleteVoiceConnectorOrigination Prelude.Text
deleteVoiceConnectorOrigination_voiceConnectorId = Lens.lens (\DeleteVoiceConnectorOrigination' {voiceConnectorId} -> voiceConnectorId) (\s@DeleteVoiceConnectorOrigination' {} a -> s {voiceConnectorId = a} :: DeleteVoiceConnectorOrigination)

instance
  Core.AWSRequest
    DeleteVoiceConnectorOrigination
  where
  type
    AWSResponse DeleteVoiceConnectorOrigination =
      DeleteVoiceConnectorOriginationResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull
      DeleteVoiceConnectorOriginationResponse'

instance
  Prelude.Hashable
    DeleteVoiceConnectorOrigination
  where
  hashWithSalt
    _salt
    DeleteVoiceConnectorOrigination' {..} =
      _salt `Prelude.hashWithSalt` voiceConnectorId

instance
  Prelude.NFData
    DeleteVoiceConnectorOrigination
  where
  rnf DeleteVoiceConnectorOrigination' {..} =
    Prelude.rnf voiceConnectorId

instance
  Data.ToHeaders
    DeleteVoiceConnectorOrigination
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteVoiceConnectorOrigination where
  toPath DeleteVoiceConnectorOrigination' {..} =
    Prelude.mconcat
      [ "/voice-connectors/",
        Data.toBS voiceConnectorId,
        "/origination"
      ]

instance Data.ToQuery DeleteVoiceConnectorOrigination where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteVoiceConnectorOriginationResponse' smart constructor.
data DeleteVoiceConnectorOriginationResponse = DeleteVoiceConnectorOriginationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVoiceConnectorOriginationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteVoiceConnectorOriginationResponse ::
  DeleteVoiceConnectorOriginationResponse
newDeleteVoiceConnectorOriginationResponse =
  DeleteVoiceConnectorOriginationResponse'

instance
  Prelude.NFData
    DeleteVoiceConnectorOriginationResponse
  where
  rnf _ = ()
