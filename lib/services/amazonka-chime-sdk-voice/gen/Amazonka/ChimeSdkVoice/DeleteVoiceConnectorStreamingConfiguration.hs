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
-- Module      : Amazonka.ChimeSdkVoice.DeleteVoiceConnectorStreamingConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- -- | Undocumented operation.
module Amazonka.ChimeSdkVoice.DeleteVoiceConnectorStreamingConfiguration
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

import Amazonka.ChimeSdkVoice.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteVoiceConnectorStreamingConfiguration' smart constructor.
data DeleteVoiceConnectorStreamingConfiguration = DeleteVoiceConnectorStreamingConfiguration'
  { voiceConnectorId :: Prelude.Text
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
-- 'voiceConnectorId', 'deleteVoiceConnectorStreamingConfiguration_voiceConnectorId' - Undocumented member.
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

-- | Undocumented member.
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
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull
      DeleteVoiceConnectorStreamingConfigurationResponse'

instance
  Prelude.Hashable
    DeleteVoiceConnectorStreamingConfiguration
  where
  hashWithSalt
    _salt
    DeleteVoiceConnectorStreamingConfiguration' {..} =
      _salt `Prelude.hashWithSalt` voiceConnectorId

instance
  Prelude.NFData
    DeleteVoiceConnectorStreamingConfiguration
  where
  rnf DeleteVoiceConnectorStreamingConfiguration' {..} =
    Prelude.rnf voiceConnectorId

instance
  Data.ToHeaders
    DeleteVoiceConnectorStreamingConfiguration
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    DeleteVoiceConnectorStreamingConfiguration
  where
  toPath
    DeleteVoiceConnectorStreamingConfiguration' {..} =
      Prelude.mconcat
        [ "/voice-connectors/",
          Data.toBS voiceConnectorId,
          "/streaming-configuration"
        ]

instance
  Data.ToQuery
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
  where
  rnf _ = ()
