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
-- Module      : Amazonka.ConnectCampaigns.UpdateCampaignOutboundCallConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the outbound call config of a campaign. This API is idempotent.
module Amazonka.ConnectCampaigns.UpdateCampaignOutboundCallConfig
  ( -- * Creating a Request
    UpdateCampaignOutboundCallConfig (..),
    newUpdateCampaignOutboundCallConfig,

    -- * Request Lenses
    updateCampaignOutboundCallConfig_answerMachineDetectionConfig,
    updateCampaignOutboundCallConfig_connectContactFlowId,
    updateCampaignOutboundCallConfig_connectSourcePhoneNumber,
    updateCampaignOutboundCallConfig_id,

    -- * Destructuring the Response
    UpdateCampaignOutboundCallConfigResponse (..),
    newUpdateCampaignOutboundCallConfigResponse,
  )
where

import Amazonka.ConnectCampaigns.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | UpdateCampaignOutboundCallConfigRequest
--
-- /See:/ 'newUpdateCampaignOutboundCallConfig' smart constructor.
data UpdateCampaignOutboundCallConfig = UpdateCampaignOutboundCallConfig'
  { answerMachineDetectionConfig :: Prelude.Maybe AnswerMachineDetectionConfig,
    connectContactFlowId :: Prelude.Maybe Prelude.Text,
    connectSourcePhoneNumber :: Prelude.Maybe Prelude.Text,
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateCampaignOutboundCallConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'answerMachineDetectionConfig', 'updateCampaignOutboundCallConfig_answerMachineDetectionConfig' - Undocumented member.
--
-- 'connectContactFlowId', 'updateCampaignOutboundCallConfig_connectContactFlowId' - Undocumented member.
--
-- 'connectSourcePhoneNumber', 'updateCampaignOutboundCallConfig_connectSourcePhoneNumber' - Undocumented member.
--
-- 'id', 'updateCampaignOutboundCallConfig_id' - Undocumented member.
newUpdateCampaignOutboundCallConfig ::
  -- | 'id'
  Prelude.Text ->
  UpdateCampaignOutboundCallConfig
newUpdateCampaignOutboundCallConfig pId_ =
  UpdateCampaignOutboundCallConfig'
    { answerMachineDetectionConfig =
        Prelude.Nothing,
      connectContactFlowId = Prelude.Nothing,
      connectSourcePhoneNumber =
        Prelude.Nothing,
      id = pId_
    }

-- | Undocumented member.
updateCampaignOutboundCallConfig_answerMachineDetectionConfig :: Lens.Lens' UpdateCampaignOutboundCallConfig (Prelude.Maybe AnswerMachineDetectionConfig)
updateCampaignOutboundCallConfig_answerMachineDetectionConfig = Lens.lens (\UpdateCampaignOutboundCallConfig' {answerMachineDetectionConfig} -> answerMachineDetectionConfig) (\s@UpdateCampaignOutboundCallConfig' {} a -> s {answerMachineDetectionConfig = a} :: UpdateCampaignOutboundCallConfig)

-- | Undocumented member.
updateCampaignOutboundCallConfig_connectContactFlowId :: Lens.Lens' UpdateCampaignOutboundCallConfig (Prelude.Maybe Prelude.Text)
updateCampaignOutboundCallConfig_connectContactFlowId = Lens.lens (\UpdateCampaignOutboundCallConfig' {connectContactFlowId} -> connectContactFlowId) (\s@UpdateCampaignOutboundCallConfig' {} a -> s {connectContactFlowId = a} :: UpdateCampaignOutboundCallConfig)

-- | Undocumented member.
updateCampaignOutboundCallConfig_connectSourcePhoneNumber :: Lens.Lens' UpdateCampaignOutboundCallConfig (Prelude.Maybe Prelude.Text)
updateCampaignOutboundCallConfig_connectSourcePhoneNumber = Lens.lens (\UpdateCampaignOutboundCallConfig' {connectSourcePhoneNumber} -> connectSourcePhoneNumber) (\s@UpdateCampaignOutboundCallConfig' {} a -> s {connectSourcePhoneNumber = a} :: UpdateCampaignOutboundCallConfig)

-- | Undocumented member.
updateCampaignOutboundCallConfig_id :: Lens.Lens' UpdateCampaignOutboundCallConfig Prelude.Text
updateCampaignOutboundCallConfig_id = Lens.lens (\UpdateCampaignOutboundCallConfig' {id} -> id) (\s@UpdateCampaignOutboundCallConfig' {} a -> s {id = a} :: UpdateCampaignOutboundCallConfig)

instance
  Core.AWSRequest
    UpdateCampaignOutboundCallConfig
  where
  type
    AWSResponse UpdateCampaignOutboundCallConfig =
      UpdateCampaignOutboundCallConfigResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      UpdateCampaignOutboundCallConfigResponse'

instance
  Prelude.Hashable
    UpdateCampaignOutboundCallConfig
  where
  hashWithSalt
    _salt
    UpdateCampaignOutboundCallConfig' {..} =
      _salt
        `Prelude.hashWithSalt` answerMachineDetectionConfig
        `Prelude.hashWithSalt` connectContactFlowId
        `Prelude.hashWithSalt` connectSourcePhoneNumber
        `Prelude.hashWithSalt` id

instance
  Prelude.NFData
    UpdateCampaignOutboundCallConfig
  where
  rnf UpdateCampaignOutboundCallConfig' {..} =
    Prelude.rnf answerMachineDetectionConfig
      `Prelude.seq` Prelude.rnf connectContactFlowId
      `Prelude.seq` Prelude.rnf connectSourcePhoneNumber
      `Prelude.seq` Prelude.rnf id

instance
  Data.ToHeaders
    UpdateCampaignOutboundCallConfig
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateCampaignOutboundCallConfig where
  toJSON UpdateCampaignOutboundCallConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("answerMachineDetectionConfig" Data..=)
              Prelude.<$> answerMachineDetectionConfig,
            ("connectContactFlowId" Data..=)
              Prelude.<$> connectContactFlowId,
            ("connectSourcePhoneNumber" Data..=)
              Prelude.<$> connectSourcePhoneNumber
          ]
      )

instance Data.ToPath UpdateCampaignOutboundCallConfig where
  toPath UpdateCampaignOutboundCallConfig' {..} =
    Prelude.mconcat
      [ "/campaigns/",
        Data.toBS id,
        "/outbound-call-config"
      ]

instance
  Data.ToQuery
    UpdateCampaignOutboundCallConfig
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateCampaignOutboundCallConfigResponse' smart constructor.
data UpdateCampaignOutboundCallConfigResponse = UpdateCampaignOutboundCallConfigResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateCampaignOutboundCallConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateCampaignOutboundCallConfigResponse ::
  UpdateCampaignOutboundCallConfigResponse
newUpdateCampaignOutboundCallConfigResponse =
  UpdateCampaignOutboundCallConfigResponse'

instance
  Prelude.NFData
    UpdateCampaignOutboundCallConfigResponse
  where
  rnf _ = ()
