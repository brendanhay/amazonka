{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ConnectCampaigns.Types.OutboundCallConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectCampaigns.Types.OutboundCallConfig where

import Amazonka.ConnectCampaigns.Types.AnswerMachineDetectionConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The configuration used for outbound calls.
--
-- /See:/ 'newOutboundCallConfig' smart constructor.
data OutboundCallConfig = OutboundCallConfig'
  { answerMachineDetectionConfig :: Prelude.Maybe AnswerMachineDetectionConfig,
    connectSourcePhoneNumber :: Prelude.Maybe Prelude.Text,
    connectContactFlowId :: Prelude.Text,
    connectQueueId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OutboundCallConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'answerMachineDetectionConfig', 'outboundCallConfig_answerMachineDetectionConfig' - Undocumented member.
--
-- 'connectSourcePhoneNumber', 'outboundCallConfig_connectSourcePhoneNumber' - Undocumented member.
--
-- 'connectContactFlowId', 'outboundCallConfig_connectContactFlowId' - Undocumented member.
--
-- 'connectQueueId', 'outboundCallConfig_connectQueueId' - Undocumented member.
newOutboundCallConfig ::
  -- | 'connectContactFlowId'
  Prelude.Text ->
  -- | 'connectQueueId'
  Prelude.Text ->
  OutboundCallConfig
newOutboundCallConfig
  pConnectContactFlowId_
  pConnectQueueId_ =
    OutboundCallConfig'
      { answerMachineDetectionConfig =
          Prelude.Nothing,
        connectSourcePhoneNumber = Prelude.Nothing,
        connectContactFlowId = pConnectContactFlowId_,
        connectQueueId = pConnectQueueId_
      }

-- | Undocumented member.
outboundCallConfig_answerMachineDetectionConfig :: Lens.Lens' OutboundCallConfig (Prelude.Maybe AnswerMachineDetectionConfig)
outboundCallConfig_answerMachineDetectionConfig = Lens.lens (\OutboundCallConfig' {answerMachineDetectionConfig} -> answerMachineDetectionConfig) (\s@OutboundCallConfig' {} a -> s {answerMachineDetectionConfig = a} :: OutboundCallConfig)

-- | Undocumented member.
outboundCallConfig_connectSourcePhoneNumber :: Lens.Lens' OutboundCallConfig (Prelude.Maybe Prelude.Text)
outboundCallConfig_connectSourcePhoneNumber = Lens.lens (\OutboundCallConfig' {connectSourcePhoneNumber} -> connectSourcePhoneNumber) (\s@OutboundCallConfig' {} a -> s {connectSourcePhoneNumber = a} :: OutboundCallConfig)

-- | Undocumented member.
outboundCallConfig_connectContactFlowId :: Lens.Lens' OutboundCallConfig Prelude.Text
outboundCallConfig_connectContactFlowId = Lens.lens (\OutboundCallConfig' {connectContactFlowId} -> connectContactFlowId) (\s@OutboundCallConfig' {} a -> s {connectContactFlowId = a} :: OutboundCallConfig)

-- | Undocumented member.
outboundCallConfig_connectQueueId :: Lens.Lens' OutboundCallConfig Prelude.Text
outboundCallConfig_connectQueueId = Lens.lens (\OutboundCallConfig' {connectQueueId} -> connectQueueId) (\s@OutboundCallConfig' {} a -> s {connectQueueId = a} :: OutboundCallConfig)

instance Core.FromJSON OutboundCallConfig where
  parseJSON =
    Core.withObject
      "OutboundCallConfig"
      ( \x ->
          OutboundCallConfig'
            Prelude.<$> (x Core..:? "answerMachineDetectionConfig")
            Prelude.<*> (x Core..:? "connectSourcePhoneNumber")
            Prelude.<*> (x Core..: "connectContactFlowId")
            Prelude.<*> (x Core..: "connectQueueId")
      )

instance Prelude.Hashable OutboundCallConfig where
  hashWithSalt _salt OutboundCallConfig' {..} =
    _salt
      `Prelude.hashWithSalt` answerMachineDetectionConfig
      `Prelude.hashWithSalt` connectSourcePhoneNumber
      `Prelude.hashWithSalt` connectContactFlowId
      `Prelude.hashWithSalt` connectQueueId

instance Prelude.NFData OutboundCallConfig where
  rnf OutboundCallConfig' {..} =
    Prelude.rnf answerMachineDetectionConfig
      `Prelude.seq` Prelude.rnf connectSourcePhoneNumber
      `Prelude.seq` Prelude.rnf connectContactFlowId
      `Prelude.seq` Prelude.rnf connectQueueId

instance Core.ToJSON OutboundCallConfig where
  toJSON OutboundCallConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("answerMachineDetectionConfig" Core..=)
              Prelude.<$> answerMachineDetectionConfig,
            ("connectSourcePhoneNumber" Core..=)
              Prelude.<$> connectSourcePhoneNumber,
            Prelude.Just
              ( "connectContactFlowId"
                  Core..= connectContactFlowId
              ),
            Prelude.Just
              ("connectQueueId" Core..= connectQueueId)
          ]
      )
