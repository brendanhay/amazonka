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
-- Module      : Amazonka.ChimeSdkVoice.UpdateVoiceConnector
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the details for the specified Amazon Chime SDK Voice Connector.
module Amazonka.ChimeSdkVoice.UpdateVoiceConnector
  ( -- * Creating a Request
    UpdateVoiceConnector (..),
    newUpdateVoiceConnector,

    -- * Request Lenses
    updateVoiceConnector_voiceConnectorId,
    updateVoiceConnector_name,
    updateVoiceConnector_requireEncryption,

    -- * Destructuring the Response
    UpdateVoiceConnectorResponse (..),
    newUpdateVoiceConnectorResponse,

    -- * Response Lenses
    updateVoiceConnectorResponse_voiceConnector,
    updateVoiceConnectorResponse_httpStatus,
  )
where

import Amazonka.ChimeSdkVoice.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateVoiceConnector' smart constructor.
data UpdateVoiceConnector = UpdateVoiceConnector'
  { -- | The Voice Connector ID.
    voiceConnectorId :: Prelude.Text,
    -- | The name of the Voice Connector.
    name :: Prelude.Text,
    -- | When enabled, requires encryption for the Voice Connector.
    requireEncryption :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateVoiceConnector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'voiceConnectorId', 'updateVoiceConnector_voiceConnectorId' - The Voice Connector ID.
--
-- 'name', 'updateVoiceConnector_name' - The name of the Voice Connector.
--
-- 'requireEncryption', 'updateVoiceConnector_requireEncryption' - When enabled, requires encryption for the Voice Connector.
newUpdateVoiceConnector ::
  -- | 'voiceConnectorId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'requireEncryption'
  Prelude.Bool ->
  UpdateVoiceConnector
newUpdateVoiceConnector
  pVoiceConnectorId_
  pName_
  pRequireEncryption_ =
    UpdateVoiceConnector'
      { voiceConnectorId =
          pVoiceConnectorId_,
        name = pName_,
        requireEncryption = pRequireEncryption_
      }

-- | The Voice Connector ID.
updateVoiceConnector_voiceConnectorId :: Lens.Lens' UpdateVoiceConnector Prelude.Text
updateVoiceConnector_voiceConnectorId = Lens.lens (\UpdateVoiceConnector' {voiceConnectorId} -> voiceConnectorId) (\s@UpdateVoiceConnector' {} a -> s {voiceConnectorId = a} :: UpdateVoiceConnector)

-- | The name of the Voice Connector.
updateVoiceConnector_name :: Lens.Lens' UpdateVoiceConnector Prelude.Text
updateVoiceConnector_name = Lens.lens (\UpdateVoiceConnector' {name} -> name) (\s@UpdateVoiceConnector' {} a -> s {name = a} :: UpdateVoiceConnector)

-- | When enabled, requires encryption for the Voice Connector.
updateVoiceConnector_requireEncryption :: Lens.Lens' UpdateVoiceConnector Prelude.Bool
updateVoiceConnector_requireEncryption = Lens.lens (\UpdateVoiceConnector' {requireEncryption} -> requireEncryption) (\s@UpdateVoiceConnector' {} a -> s {requireEncryption = a} :: UpdateVoiceConnector)

instance Core.AWSRequest UpdateVoiceConnector where
  type
    AWSResponse UpdateVoiceConnector =
      UpdateVoiceConnectorResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateVoiceConnectorResponse'
            Prelude.<$> (x Data..?> "VoiceConnector")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateVoiceConnector where
  hashWithSalt _salt UpdateVoiceConnector' {..} =
    _salt
      `Prelude.hashWithSalt` voiceConnectorId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` requireEncryption

instance Prelude.NFData UpdateVoiceConnector where
  rnf UpdateVoiceConnector' {..} =
    Prelude.rnf voiceConnectorId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf requireEncryption

instance Data.ToHeaders UpdateVoiceConnector where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON UpdateVoiceConnector where
  toJSON UpdateVoiceConnector' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just
              ("RequireEncryption" Data..= requireEncryption)
          ]
      )

instance Data.ToPath UpdateVoiceConnector where
  toPath UpdateVoiceConnector' {..} =
    Prelude.mconcat
      ["/voice-connectors/", Data.toBS voiceConnectorId]

instance Data.ToQuery UpdateVoiceConnector where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateVoiceConnectorResponse' smart constructor.
data UpdateVoiceConnectorResponse = UpdateVoiceConnectorResponse'
  { -- | The updated Voice Connector details.
    voiceConnector :: Prelude.Maybe VoiceConnector,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateVoiceConnectorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'voiceConnector', 'updateVoiceConnectorResponse_voiceConnector' - The updated Voice Connector details.
--
-- 'httpStatus', 'updateVoiceConnectorResponse_httpStatus' - The response's http status code.
newUpdateVoiceConnectorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateVoiceConnectorResponse
newUpdateVoiceConnectorResponse pHttpStatus_ =
  UpdateVoiceConnectorResponse'
    { voiceConnector =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The updated Voice Connector details.
updateVoiceConnectorResponse_voiceConnector :: Lens.Lens' UpdateVoiceConnectorResponse (Prelude.Maybe VoiceConnector)
updateVoiceConnectorResponse_voiceConnector = Lens.lens (\UpdateVoiceConnectorResponse' {voiceConnector} -> voiceConnector) (\s@UpdateVoiceConnectorResponse' {} a -> s {voiceConnector = a} :: UpdateVoiceConnectorResponse)

-- | The response's http status code.
updateVoiceConnectorResponse_httpStatus :: Lens.Lens' UpdateVoiceConnectorResponse Prelude.Int
updateVoiceConnectorResponse_httpStatus = Lens.lens (\UpdateVoiceConnectorResponse' {httpStatus} -> httpStatus) (\s@UpdateVoiceConnectorResponse' {} a -> s {httpStatus = a} :: UpdateVoiceConnectorResponse)

instance Prelude.NFData UpdateVoiceConnectorResponse where
  rnf UpdateVoiceConnectorResponse' {..} =
    Prelude.rnf voiceConnector
      `Prelude.seq` Prelude.rnf httpStatus
