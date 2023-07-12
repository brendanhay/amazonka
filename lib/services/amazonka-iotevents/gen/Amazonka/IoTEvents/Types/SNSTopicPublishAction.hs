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
-- Module      : Amazonka.IoTEvents.Types.SNSTopicPublishAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEvents.Types.SNSTopicPublishAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTEvents.Types.Payload
import qualified Amazonka.Prelude as Prelude

-- | Information required to publish the Amazon SNS message.
--
-- /See:/ 'newSNSTopicPublishAction' smart constructor.
data SNSTopicPublishAction = SNSTopicPublishAction'
  { -- | You can configure the action payload when you send a message as an
    -- Amazon SNS push notification.
    payload :: Prelude.Maybe Payload,
    -- | The ARN of the Amazon SNS target where the message is sent.
    targetArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SNSTopicPublishAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'payload', 'sNSTopicPublishAction_payload' - You can configure the action payload when you send a message as an
-- Amazon SNS push notification.
--
-- 'targetArn', 'sNSTopicPublishAction_targetArn' - The ARN of the Amazon SNS target where the message is sent.
newSNSTopicPublishAction ::
  -- | 'targetArn'
  Prelude.Text ->
  SNSTopicPublishAction
newSNSTopicPublishAction pTargetArn_ =
  SNSTopicPublishAction'
    { payload = Prelude.Nothing,
      targetArn = pTargetArn_
    }

-- | You can configure the action payload when you send a message as an
-- Amazon SNS push notification.
sNSTopicPublishAction_payload :: Lens.Lens' SNSTopicPublishAction (Prelude.Maybe Payload)
sNSTopicPublishAction_payload = Lens.lens (\SNSTopicPublishAction' {payload} -> payload) (\s@SNSTopicPublishAction' {} a -> s {payload = a} :: SNSTopicPublishAction)

-- | The ARN of the Amazon SNS target where the message is sent.
sNSTopicPublishAction_targetArn :: Lens.Lens' SNSTopicPublishAction Prelude.Text
sNSTopicPublishAction_targetArn = Lens.lens (\SNSTopicPublishAction' {targetArn} -> targetArn) (\s@SNSTopicPublishAction' {} a -> s {targetArn = a} :: SNSTopicPublishAction)

instance Data.FromJSON SNSTopicPublishAction where
  parseJSON =
    Data.withObject
      "SNSTopicPublishAction"
      ( \x ->
          SNSTopicPublishAction'
            Prelude.<$> (x Data..:? "payload")
            Prelude.<*> (x Data..: "targetArn")
      )

instance Prelude.Hashable SNSTopicPublishAction where
  hashWithSalt _salt SNSTopicPublishAction' {..} =
    _salt
      `Prelude.hashWithSalt` payload
      `Prelude.hashWithSalt` targetArn

instance Prelude.NFData SNSTopicPublishAction where
  rnf SNSTopicPublishAction' {..} =
    Prelude.rnf payload
      `Prelude.seq` Prelude.rnf targetArn

instance Data.ToJSON SNSTopicPublishAction where
  toJSON SNSTopicPublishAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("payload" Data..=) Prelude.<$> payload,
            Prelude.Just ("targetArn" Data..= targetArn)
          ]
      )
