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
-- Module      : Amazonka.Chime.DeleteVoiceConnectorTermination
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the termination settings for the specified Amazon Chime Voice
-- Connector.
--
-- If emergency calling is configured for the Amazon Chime Voice Connector,
-- it must be deleted prior to deleting the termination settings.
module Amazonka.Chime.DeleteVoiceConnectorTermination
  ( -- * Creating a Request
    DeleteVoiceConnectorTermination (..),
    newDeleteVoiceConnectorTermination,

    -- * Request Lenses
    deleteVoiceConnectorTermination_voiceConnectorId,

    -- * Destructuring the Response
    DeleteVoiceConnectorTerminationResponse (..),
    newDeleteVoiceConnectorTerminationResponse,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteVoiceConnectorTermination' smart constructor.
data DeleteVoiceConnectorTermination = DeleteVoiceConnectorTermination'
  { -- | The Amazon Chime Voice Connector ID.
    voiceConnectorId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVoiceConnectorTermination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'voiceConnectorId', 'deleteVoiceConnectorTermination_voiceConnectorId' - The Amazon Chime Voice Connector ID.
newDeleteVoiceConnectorTermination ::
  -- | 'voiceConnectorId'
  Prelude.Text ->
  DeleteVoiceConnectorTermination
newDeleteVoiceConnectorTermination pVoiceConnectorId_ =
  DeleteVoiceConnectorTermination'
    { voiceConnectorId =
        pVoiceConnectorId_
    }

-- | The Amazon Chime Voice Connector ID.
deleteVoiceConnectorTermination_voiceConnectorId :: Lens.Lens' DeleteVoiceConnectorTermination Prelude.Text
deleteVoiceConnectorTermination_voiceConnectorId = Lens.lens (\DeleteVoiceConnectorTermination' {voiceConnectorId} -> voiceConnectorId) (\s@DeleteVoiceConnectorTermination' {} a -> s {voiceConnectorId = a} :: DeleteVoiceConnectorTermination)

instance
  Core.AWSRequest
    DeleteVoiceConnectorTermination
  where
  type
    AWSResponse DeleteVoiceConnectorTermination =
      DeleteVoiceConnectorTerminationResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull
      DeleteVoiceConnectorTerminationResponse'

instance
  Prelude.Hashable
    DeleteVoiceConnectorTermination
  where
  hashWithSalt
    _salt
    DeleteVoiceConnectorTermination' {..} =
      _salt `Prelude.hashWithSalt` voiceConnectorId

instance
  Prelude.NFData
    DeleteVoiceConnectorTermination
  where
  rnf DeleteVoiceConnectorTermination' {..} =
    Prelude.rnf voiceConnectorId

instance
  Core.ToHeaders
    DeleteVoiceConnectorTermination
  where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DeleteVoiceConnectorTermination where
  toPath DeleteVoiceConnectorTermination' {..} =
    Prelude.mconcat
      [ "/voice-connectors/",
        Core.toBS voiceConnectorId,
        "/termination"
      ]

instance Core.ToQuery DeleteVoiceConnectorTermination where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteVoiceConnectorTerminationResponse' smart constructor.
data DeleteVoiceConnectorTerminationResponse = DeleteVoiceConnectorTerminationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVoiceConnectorTerminationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteVoiceConnectorTerminationResponse ::
  DeleteVoiceConnectorTerminationResponse
newDeleteVoiceConnectorTerminationResponse =
  DeleteVoiceConnectorTerminationResponse'

instance
  Prelude.NFData
    DeleteVoiceConnectorTerminationResponse
  where
  rnf _ = ()
