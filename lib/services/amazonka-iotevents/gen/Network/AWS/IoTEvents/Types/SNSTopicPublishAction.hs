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
-- Module      : Network.AWS.IoTEvents.Types.SNSTopicPublishAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTEvents.Types.SNSTopicPublishAction where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTEvents.Types.Payload
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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

instance Core.FromJSON SNSTopicPublishAction where
  parseJSON =
    Core.withObject
      "SNSTopicPublishAction"
      ( \x ->
          SNSTopicPublishAction'
            Prelude.<$> (x Core..:? "payload")
            Prelude.<*> (x Core..: "targetArn")
      )

instance Prelude.Hashable SNSTopicPublishAction

instance Prelude.NFData SNSTopicPublishAction

instance Core.ToJSON SNSTopicPublishAction where
  toJSON SNSTopicPublishAction' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("payload" Core..=) Prelude.<$> payload,
            Prelude.Just ("targetArn" Core..= targetArn)
          ]
      )
