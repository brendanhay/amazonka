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
-- Module      : Network.AWS.Chime.DeleteVoiceConnector
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Amazon Chime Voice Connector. Any phone numbers
-- associated with the Amazon Chime Voice Connector must be disassociated
-- from it before it can be deleted.
module Network.AWS.Chime.DeleteVoiceConnector
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

import Network.AWS.Chime.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteVoiceConnector' smart constructor.
data DeleteVoiceConnector = DeleteVoiceConnector'
  { -- | The Amazon Chime Voice Connector ID.
    voiceConnectorId :: Prelude.Text
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
-- 'voiceConnectorId', 'deleteVoiceConnector_voiceConnectorId' - The Amazon Chime Voice Connector ID.
newDeleteVoiceConnector ::
  -- | 'voiceConnectorId'
  Prelude.Text ->
  DeleteVoiceConnector
newDeleteVoiceConnector pVoiceConnectorId_ =
  DeleteVoiceConnector'
    { voiceConnectorId =
        pVoiceConnectorId_
    }

-- | The Amazon Chime Voice Connector ID.
deleteVoiceConnector_voiceConnectorId :: Lens.Lens' DeleteVoiceConnector Prelude.Text
deleteVoiceConnector_voiceConnectorId = Lens.lens (\DeleteVoiceConnector' {voiceConnectorId} -> voiceConnectorId) (\s@DeleteVoiceConnector' {} a -> s {voiceConnectorId = a} :: DeleteVoiceConnector)

instance Core.AWSRequest DeleteVoiceConnector where
  type
    AWSResponse DeleteVoiceConnector =
      DeleteVoiceConnectorResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull DeleteVoiceConnectorResponse'

instance Prelude.Hashable DeleteVoiceConnector

instance Prelude.NFData DeleteVoiceConnector

instance Core.ToHeaders DeleteVoiceConnector where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DeleteVoiceConnector where
  toPath DeleteVoiceConnector' {..} =
    Prelude.mconcat
      ["/voice-connectors/", Core.toBS voiceConnectorId]

instance Core.ToQuery DeleteVoiceConnector where
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

instance Prelude.NFData DeleteVoiceConnectorResponse
