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
-- Module      : Network.AWS.Chime.DeleteVoiceConnectorStreamingConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the streaming configuration for the specified Amazon Chime Voice
-- Connector.
module Network.AWS.Chime.DeleteVoiceConnectorStreamingConfiguration
  ( -- * Creating a Request
    DeleteVoiceConnectorStreamingConfiguration (..),
    newDeleteVoiceConnectorStreamingConfiguration,

    -- * Request Lenses
    deleteVoiceConnectorStreamingConfiguration_voiceConnectorId,

    -- * Destructuring the Response
    DeleteVoiceConnectorStreamingConfigurationResponse (..),
    newDeleteVoiceConnectorStreamingConfigurationResponse,
  )
where

import Network.AWS.Chime.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteVoiceConnectorStreamingConfiguration' smart constructor.
data DeleteVoiceConnectorStreamingConfiguration = DeleteVoiceConnectorStreamingConfiguration'
  { -- | The Amazon Chime Voice Connector ID.
    voiceConnectorId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVoiceConnectorStreamingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'voiceConnectorId', 'deleteVoiceConnectorStreamingConfiguration_voiceConnectorId' - The Amazon Chime Voice Connector ID.
newDeleteVoiceConnectorStreamingConfiguration ::
  -- | 'voiceConnectorId'
  Prelude.Text ->
  DeleteVoiceConnectorStreamingConfiguration
newDeleteVoiceConnectorStreamingConfiguration
  pVoiceConnectorId_ =
    DeleteVoiceConnectorStreamingConfiguration'
      { voiceConnectorId =
          pVoiceConnectorId_
      }

-- | The Amazon Chime Voice Connector ID.
deleteVoiceConnectorStreamingConfiguration_voiceConnectorId :: Lens.Lens' DeleteVoiceConnectorStreamingConfiguration Prelude.Text
deleteVoiceConnectorStreamingConfiguration_voiceConnectorId = Lens.lens (\DeleteVoiceConnectorStreamingConfiguration' {voiceConnectorId} -> voiceConnectorId) (\s@DeleteVoiceConnectorStreamingConfiguration' {} a -> s {voiceConnectorId = a} :: DeleteVoiceConnectorStreamingConfiguration)

instance
  Core.AWSRequest
    DeleteVoiceConnectorStreamingConfiguration
  where
  type
    AWSResponse
      DeleteVoiceConnectorStreamingConfiguration =
      DeleteVoiceConnectorStreamingConfigurationResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull
      DeleteVoiceConnectorStreamingConfigurationResponse'

instance
  Prelude.Hashable
    DeleteVoiceConnectorStreamingConfiguration

instance
  Prelude.NFData
    DeleteVoiceConnectorStreamingConfiguration

instance
  Core.ToHeaders
    DeleteVoiceConnectorStreamingConfiguration
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    DeleteVoiceConnectorStreamingConfiguration
  where
  toPath
    DeleteVoiceConnectorStreamingConfiguration' {..} =
      Prelude.mconcat
        [ "/voice-connectors/",
          Core.toBS voiceConnectorId,
          "/streaming-configuration"
        ]

instance
  Core.ToQuery
    DeleteVoiceConnectorStreamingConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteVoiceConnectorStreamingConfigurationResponse' smart constructor.
data DeleteVoiceConnectorStreamingConfigurationResponse = DeleteVoiceConnectorStreamingConfigurationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVoiceConnectorStreamingConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteVoiceConnectorStreamingConfigurationResponse ::
  DeleteVoiceConnectorStreamingConfigurationResponse
newDeleteVoiceConnectorStreamingConfigurationResponse =
  DeleteVoiceConnectorStreamingConfigurationResponse'

instance
  Prelude.NFData
    DeleteVoiceConnectorStreamingConfigurationResponse
