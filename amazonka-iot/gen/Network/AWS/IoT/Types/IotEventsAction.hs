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
-- Module      : Network.AWS.IoT.Types.IotEventsAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.IotEventsAction where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Sends an input to an AWS IoT Events detector.
--
-- /See:/ 'newIotEventsAction' smart constructor.
data IotEventsAction = IotEventsAction'
  { -- | Whether to process the event actions as a batch. The default value is
    -- @false@.
    --
    -- When @batchMode@ is @true@, you can\'t specify a @messageId@.
    --
    -- When @batchMode@ is @true@ and the rule SQL statement evaluates to an
    -- Array, each Array element is treated as a separate message when it\'s
    -- sent to AWS IoT Events by calling
    -- <https://docs.aws.amazon.com/iotevents/latest/apireference/API_iotevents-data_BatchPutMessage.html BatchPutMessage>
    -- . The resulting array can\'t have more than 10 messages.
    batchMode :: Core.Maybe Core.Bool,
    -- | The ID of the message. The default @messageId@ is a new UUID value.
    --
    -- When @batchMode@ is @true@, you can\'t specify a @messageId@--a new UUID
    -- value will be assigned.
    --
    -- Assign a value to this property to ensure that only one input (message)
    -- with a given @messageId@ will be processed by an AWS IoT Events
    -- detector.
    messageId :: Core.Maybe Core.Text,
    -- | The name of the AWS IoT Events input.
    inputName :: Core.Text,
    -- | The ARN of the role that grants AWS IoT permission to send an input to
    -- an AWS IoT Events detector. (\"Action\":\"iotevents:BatchPutMessage\").
    roleArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'IotEventsAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'batchMode', 'iotEventsAction_batchMode' - Whether to process the event actions as a batch. The default value is
-- @false@.
--
-- When @batchMode@ is @true@, you can\'t specify a @messageId@.
--
-- When @batchMode@ is @true@ and the rule SQL statement evaluates to an
-- Array, each Array element is treated as a separate message when it\'s
-- sent to AWS IoT Events by calling
-- <https://docs.aws.amazon.com/iotevents/latest/apireference/API_iotevents-data_BatchPutMessage.html BatchPutMessage>
-- . The resulting array can\'t have more than 10 messages.
--
-- 'messageId', 'iotEventsAction_messageId' - The ID of the message. The default @messageId@ is a new UUID value.
--
-- When @batchMode@ is @true@, you can\'t specify a @messageId@--a new UUID
-- value will be assigned.
--
-- Assign a value to this property to ensure that only one input (message)
-- with a given @messageId@ will be processed by an AWS IoT Events
-- detector.
--
-- 'inputName', 'iotEventsAction_inputName' - The name of the AWS IoT Events input.
--
-- 'roleArn', 'iotEventsAction_roleArn' - The ARN of the role that grants AWS IoT permission to send an input to
-- an AWS IoT Events detector. (\"Action\":\"iotevents:BatchPutMessage\").
newIotEventsAction ::
  -- | 'inputName'
  Core.Text ->
  -- | 'roleArn'
  Core.Text ->
  IotEventsAction
newIotEventsAction pInputName_ pRoleArn_ =
  IotEventsAction'
    { batchMode = Core.Nothing,
      messageId = Core.Nothing,
      inputName = pInputName_,
      roleArn = pRoleArn_
    }

-- | Whether to process the event actions as a batch. The default value is
-- @false@.
--
-- When @batchMode@ is @true@, you can\'t specify a @messageId@.
--
-- When @batchMode@ is @true@ and the rule SQL statement evaluates to an
-- Array, each Array element is treated as a separate message when it\'s
-- sent to AWS IoT Events by calling
-- <https://docs.aws.amazon.com/iotevents/latest/apireference/API_iotevents-data_BatchPutMessage.html BatchPutMessage>
-- . The resulting array can\'t have more than 10 messages.
iotEventsAction_batchMode :: Lens.Lens' IotEventsAction (Core.Maybe Core.Bool)
iotEventsAction_batchMode = Lens.lens (\IotEventsAction' {batchMode} -> batchMode) (\s@IotEventsAction' {} a -> s {batchMode = a} :: IotEventsAction)

-- | The ID of the message. The default @messageId@ is a new UUID value.
--
-- When @batchMode@ is @true@, you can\'t specify a @messageId@--a new UUID
-- value will be assigned.
--
-- Assign a value to this property to ensure that only one input (message)
-- with a given @messageId@ will be processed by an AWS IoT Events
-- detector.
iotEventsAction_messageId :: Lens.Lens' IotEventsAction (Core.Maybe Core.Text)
iotEventsAction_messageId = Lens.lens (\IotEventsAction' {messageId} -> messageId) (\s@IotEventsAction' {} a -> s {messageId = a} :: IotEventsAction)

-- | The name of the AWS IoT Events input.
iotEventsAction_inputName :: Lens.Lens' IotEventsAction Core.Text
iotEventsAction_inputName = Lens.lens (\IotEventsAction' {inputName} -> inputName) (\s@IotEventsAction' {} a -> s {inputName = a} :: IotEventsAction)

-- | The ARN of the role that grants AWS IoT permission to send an input to
-- an AWS IoT Events detector. (\"Action\":\"iotevents:BatchPutMessage\").
iotEventsAction_roleArn :: Lens.Lens' IotEventsAction Core.Text
iotEventsAction_roleArn = Lens.lens (\IotEventsAction' {roleArn} -> roleArn) (\s@IotEventsAction' {} a -> s {roleArn = a} :: IotEventsAction)

instance Core.FromJSON IotEventsAction where
  parseJSON =
    Core.withObject
      "IotEventsAction"
      ( \x ->
          IotEventsAction'
            Core.<$> (x Core..:? "batchMode")
            Core.<*> (x Core..:? "messageId")
            Core.<*> (x Core..: "inputName")
            Core.<*> (x Core..: "roleArn")
      )

instance Core.Hashable IotEventsAction

instance Core.NFData IotEventsAction

instance Core.ToJSON IotEventsAction where
  toJSON IotEventsAction' {..} =
    Core.object
      ( Core.catMaybes
          [ ("batchMode" Core..=) Core.<$> batchMode,
            ("messageId" Core..=) Core.<$> messageId,
            Core.Just ("inputName" Core..= inputName),
            Core.Just ("roleArn" Core..= roleArn)
          ]
      )
