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
-- Module      : Amazonka.Chime.DeleteVoiceConnectorEmergencyCallingConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the emergency calling configuration details from the specified
-- Amazon Chime Voice Connector.
module Amazonka.Chime.DeleteVoiceConnectorEmergencyCallingConfiguration
  ( -- * Creating a Request
    DeleteVoiceConnectorEmergencyCallingConfiguration (..),
    newDeleteVoiceConnectorEmergencyCallingConfiguration,

    -- * Request Lenses
    deleteVoiceConnectorEmergencyCallingConfiguration_voiceConnectorId,

    -- * Destructuring the Response
    DeleteVoiceConnectorEmergencyCallingConfigurationResponse (..),
    newDeleteVoiceConnectorEmergencyCallingConfigurationResponse,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteVoiceConnectorEmergencyCallingConfiguration' smart constructor.
data DeleteVoiceConnectorEmergencyCallingConfiguration = DeleteVoiceConnectorEmergencyCallingConfiguration'
  { -- | The Amazon Chime Voice Connector ID.
    voiceConnectorId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVoiceConnectorEmergencyCallingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'voiceConnectorId', 'deleteVoiceConnectorEmergencyCallingConfiguration_voiceConnectorId' - The Amazon Chime Voice Connector ID.
newDeleteVoiceConnectorEmergencyCallingConfiguration ::
  -- | 'voiceConnectorId'
  Prelude.Text ->
  DeleteVoiceConnectorEmergencyCallingConfiguration
newDeleteVoiceConnectorEmergencyCallingConfiguration
  pVoiceConnectorId_ =
    DeleteVoiceConnectorEmergencyCallingConfiguration'
      { voiceConnectorId =
          pVoiceConnectorId_
      }

-- | The Amazon Chime Voice Connector ID.
deleteVoiceConnectorEmergencyCallingConfiguration_voiceConnectorId :: Lens.Lens' DeleteVoiceConnectorEmergencyCallingConfiguration Prelude.Text
deleteVoiceConnectorEmergencyCallingConfiguration_voiceConnectorId = Lens.lens (\DeleteVoiceConnectorEmergencyCallingConfiguration' {voiceConnectorId} -> voiceConnectorId) (\s@DeleteVoiceConnectorEmergencyCallingConfiguration' {} a -> s {voiceConnectorId = a} :: DeleteVoiceConnectorEmergencyCallingConfiguration)

instance
  Core.AWSRequest
    DeleteVoiceConnectorEmergencyCallingConfiguration
  where
  type
    AWSResponse
      DeleteVoiceConnectorEmergencyCallingConfiguration =
      DeleteVoiceConnectorEmergencyCallingConfigurationResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull
      DeleteVoiceConnectorEmergencyCallingConfigurationResponse'

instance
  Prelude.Hashable
    DeleteVoiceConnectorEmergencyCallingConfiguration
  where
  hashWithSalt
    _salt
    DeleteVoiceConnectorEmergencyCallingConfiguration' {..} =
      _salt `Prelude.hashWithSalt` voiceConnectorId

instance
  Prelude.NFData
    DeleteVoiceConnectorEmergencyCallingConfiguration
  where
  rnf
    DeleteVoiceConnectorEmergencyCallingConfiguration' {..} =
      Prelude.rnf voiceConnectorId

instance
  Data.ToHeaders
    DeleteVoiceConnectorEmergencyCallingConfiguration
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    DeleteVoiceConnectorEmergencyCallingConfiguration
  where
  toPath
    DeleteVoiceConnectorEmergencyCallingConfiguration' {..} =
      Prelude.mconcat
        [ "/voice-connectors/",
          Data.toBS voiceConnectorId,
          "/emergency-calling-configuration"
        ]

instance
  Data.ToQuery
    DeleteVoiceConnectorEmergencyCallingConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteVoiceConnectorEmergencyCallingConfigurationResponse' smart constructor.
data DeleteVoiceConnectorEmergencyCallingConfigurationResponse = DeleteVoiceConnectorEmergencyCallingConfigurationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVoiceConnectorEmergencyCallingConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteVoiceConnectorEmergencyCallingConfigurationResponse ::
  DeleteVoiceConnectorEmergencyCallingConfigurationResponse
newDeleteVoiceConnectorEmergencyCallingConfigurationResponse =
  DeleteVoiceConnectorEmergencyCallingConfigurationResponse'

instance
  Prelude.NFData
    DeleteVoiceConnectorEmergencyCallingConfigurationResponse
  where
  rnf _ = ()
