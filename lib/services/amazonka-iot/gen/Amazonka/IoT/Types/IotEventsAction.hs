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
-- Module      : Amazonka.IoT.Types.IotEventsAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.IotEventsAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Sends an input to an IoT Events detector.
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
    -- sent to IoT Events by calling
    -- <https://docs.aws.amazon.com/iotevents/latest/apireference/API_iotevents-data_BatchPutMessage.html BatchPutMessage>
    -- . The resulting array can\'t have more than 10 messages.
    batchMode :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the message. The default @messageId@ is a new UUID value.
    --
    -- When @batchMode@ is @true@, you can\'t specify a @messageId@--a new UUID
    -- value will be assigned.
    --
    -- Assign a value to this property to ensure that only one input (message)
    -- with a given @messageId@ will be processed by an IoT Events detector.
    messageId :: Prelude.Maybe Prelude.Text,
    -- | The name of the IoT Events input.
    inputName :: Prelude.Text,
    -- | The ARN of the role that grants IoT permission to send an input to an
    -- IoT Events detector. (\"Action\":\"iotevents:BatchPutMessage\").
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- sent to IoT Events by calling
-- <https://docs.aws.amazon.com/iotevents/latest/apireference/API_iotevents-data_BatchPutMessage.html BatchPutMessage>
-- . The resulting array can\'t have more than 10 messages.
--
-- 'messageId', 'iotEventsAction_messageId' - The ID of the message. The default @messageId@ is a new UUID value.
--
-- When @batchMode@ is @true@, you can\'t specify a @messageId@--a new UUID
-- value will be assigned.
--
-- Assign a value to this property to ensure that only one input (message)
-- with a given @messageId@ will be processed by an IoT Events detector.
--
-- 'inputName', 'iotEventsAction_inputName' - The name of the IoT Events input.
--
-- 'roleArn', 'iotEventsAction_roleArn' - The ARN of the role that grants IoT permission to send an input to an
-- IoT Events detector. (\"Action\":\"iotevents:BatchPutMessage\").
newIotEventsAction ::
  -- | 'inputName'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  IotEventsAction
newIotEventsAction pInputName_ pRoleArn_ =
  IotEventsAction'
    { batchMode = Prelude.Nothing,
      messageId = Prelude.Nothing,
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
-- sent to IoT Events by calling
-- <https://docs.aws.amazon.com/iotevents/latest/apireference/API_iotevents-data_BatchPutMessage.html BatchPutMessage>
-- . The resulting array can\'t have more than 10 messages.
iotEventsAction_batchMode :: Lens.Lens' IotEventsAction (Prelude.Maybe Prelude.Bool)
iotEventsAction_batchMode = Lens.lens (\IotEventsAction' {batchMode} -> batchMode) (\s@IotEventsAction' {} a -> s {batchMode = a} :: IotEventsAction)

-- | The ID of the message. The default @messageId@ is a new UUID value.
--
-- When @batchMode@ is @true@, you can\'t specify a @messageId@--a new UUID
-- value will be assigned.
--
-- Assign a value to this property to ensure that only one input (message)
-- with a given @messageId@ will be processed by an IoT Events detector.
iotEventsAction_messageId :: Lens.Lens' IotEventsAction (Prelude.Maybe Prelude.Text)
iotEventsAction_messageId = Lens.lens (\IotEventsAction' {messageId} -> messageId) (\s@IotEventsAction' {} a -> s {messageId = a} :: IotEventsAction)

-- | The name of the IoT Events input.
iotEventsAction_inputName :: Lens.Lens' IotEventsAction Prelude.Text
iotEventsAction_inputName = Lens.lens (\IotEventsAction' {inputName} -> inputName) (\s@IotEventsAction' {} a -> s {inputName = a} :: IotEventsAction)

-- | The ARN of the role that grants IoT permission to send an input to an
-- IoT Events detector. (\"Action\":\"iotevents:BatchPutMessage\").
iotEventsAction_roleArn :: Lens.Lens' IotEventsAction Prelude.Text
iotEventsAction_roleArn = Lens.lens (\IotEventsAction' {roleArn} -> roleArn) (\s@IotEventsAction' {} a -> s {roleArn = a} :: IotEventsAction)

instance Data.FromJSON IotEventsAction where
  parseJSON =
    Data.withObject
      "IotEventsAction"
      ( \x ->
          IotEventsAction'
            Prelude.<$> (x Data..:? "batchMode")
            Prelude.<*> (x Data..:? "messageId")
            Prelude.<*> (x Data..: "inputName")
            Prelude.<*> (x Data..: "roleArn")
      )

instance Prelude.Hashable IotEventsAction where
  hashWithSalt _salt IotEventsAction' {..} =
    _salt
      `Prelude.hashWithSalt` batchMode
      `Prelude.hashWithSalt` messageId
      `Prelude.hashWithSalt` inputName
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData IotEventsAction where
  rnf IotEventsAction' {..} =
    Prelude.rnf batchMode
      `Prelude.seq` Prelude.rnf messageId
      `Prelude.seq` Prelude.rnf inputName
      `Prelude.seq` Prelude.rnf roleArn

instance Data.ToJSON IotEventsAction where
  toJSON IotEventsAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("batchMode" Data..=) Prelude.<$> batchMode,
            ("messageId" Data..=) Prelude.<$> messageId,
            Prelude.Just ("inputName" Data..= inputName),
            Prelude.Just ("roleArn" Data..= roleArn)
          ]
      )
