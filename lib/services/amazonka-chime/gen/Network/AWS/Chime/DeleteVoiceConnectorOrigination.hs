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
-- Module      : Network.AWS.Chime.DeleteVoiceConnectorOrigination
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the origination settings for the specified Amazon Chime Voice
-- Connector.
--
-- If emergency calling is configured for the Amazon Chime Voice Connector,
-- it must be deleted prior to deleting the origination settings.
module Network.AWS.Chime.DeleteVoiceConnectorOrigination
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

import Network.AWS.Chime.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteVoiceConnectorOrigination' smart constructor.
data DeleteVoiceConnectorOrigination = DeleteVoiceConnectorOrigination'
  { -- | The Amazon Chime Voice Connector ID.
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
-- 'voiceConnectorId', 'deleteVoiceConnectorOrigination_voiceConnectorId' - The Amazon Chime Voice Connector ID.
newDeleteVoiceConnectorOrigination ::
  -- | 'voiceConnectorId'
  Prelude.Text ->
  DeleteVoiceConnectorOrigination
newDeleteVoiceConnectorOrigination pVoiceConnectorId_ =
  DeleteVoiceConnectorOrigination'
    { voiceConnectorId =
        pVoiceConnectorId_
    }

-- | The Amazon Chime Voice Connector ID.
deleteVoiceConnectorOrigination_voiceConnectorId :: Lens.Lens' DeleteVoiceConnectorOrigination Prelude.Text
deleteVoiceConnectorOrigination_voiceConnectorId = Lens.lens (\DeleteVoiceConnectorOrigination' {voiceConnectorId} -> voiceConnectorId) (\s@DeleteVoiceConnectorOrigination' {} a -> s {voiceConnectorId = a} :: DeleteVoiceConnectorOrigination)

instance
  Core.AWSRequest
    DeleteVoiceConnectorOrigination
  where
  type
    AWSResponse DeleteVoiceConnectorOrigination =
      DeleteVoiceConnectorOriginationResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull
      DeleteVoiceConnectorOriginationResponse'

instance
  Prelude.Hashable
    DeleteVoiceConnectorOrigination

instance
  Prelude.NFData
    DeleteVoiceConnectorOrigination

instance
  Core.ToHeaders
    DeleteVoiceConnectorOrigination
  where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DeleteVoiceConnectorOrigination where
  toPath DeleteVoiceConnectorOrigination' {..} =
    Prelude.mconcat
      [ "/voice-connectors/",
        Core.toBS voiceConnectorId,
        "/origination"
      ]

instance Core.ToQuery DeleteVoiceConnectorOrigination where
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
