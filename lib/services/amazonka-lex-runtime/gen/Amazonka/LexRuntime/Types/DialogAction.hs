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
-- Module      : Amazonka.LexRuntime.Types.DialogAction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexRuntime.Types.DialogAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LexRuntime.Types.DialogActionType
import Amazonka.LexRuntime.Types.FulfillmentState
import Amazonka.LexRuntime.Types.MessageFormatType
import qualified Amazonka.Prelude as Prelude

-- | Describes the next action that the bot should take in its interaction
-- with the user and provides information about the context in which the
-- action takes place. Use the @DialogAction@ data type to set the
-- interaction to a specific state, or to return the interaction to a
-- previous state.
--
-- /See:/ 'newDialogAction' smart constructor.
data DialogAction = DialogAction'
  { -- | The fulfillment state of the intent. The possible values are:
    --
    -- -   @Failed@ - The Lambda function associated with the intent failed to
    --     fulfill the intent.
    --
    -- -   @Fulfilled@ - The intent has fulfilled by the Lambda function
    --     associated with the intent.
    --
    -- -   @ReadyForFulfillment@ - All of the information necessary for the
    --     intent is present and the intent ready to be fulfilled by the client
    --     application.
    fulfillmentState :: Prelude.Maybe FulfillmentState,
    -- | The message that should be shown to the user. If you don\'t specify a
    -- message, Amazon Lex will use the message configured for the intent.
    message :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The name of the slot that should be elicited from the user.
    slotToElicit :: Prelude.Maybe Prelude.Text,
    -- | -   @PlainText@ - The message contains plain UTF-8 text.
    --
    -- -   @CustomPayload@ - The message is a custom format for the client.
    --
    -- -   @SSML@ - The message contains text formatted for voice output.
    --
    -- -   @Composite@ - The message contains an escaped JSON object containing
    --     one or more messages. For more information, see
    --     <https://docs.aws.amazon.com/lex/latest/dg/howitworks-manage-prompts.html Message Groups>.
    messageFormat :: Prelude.Maybe MessageFormatType,
    -- | The name of the intent.
    intentName :: Prelude.Maybe Prelude.Text,
    -- | Map of the slots that have been gathered and their values.
    slots :: Prelude.Maybe (Core.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | The next action that the bot should take in its interaction with the
    -- user. The possible values are:
    --
    -- -   @ConfirmIntent@ - The next action is asking the user if the intent
    --     is complete and ready to be fulfilled. This is a yes\/no question
    --     such as \"Place the order?\"
    --
    -- -   @Close@ - Indicates that the there will not be a response from the
    --     user. For example, the statement \"Your order has been placed\" does
    --     not require a response.
    --
    -- -   @Delegate@ - The next action is determined by Amazon Lex.
    --
    -- -   @ElicitIntent@ - The next action is to determine the intent that the
    --     user wants to fulfill.
    --
    -- -   @ElicitSlot@ - The next action is to elicit a slot value from the
    --     user.
    type' :: DialogActionType
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DialogAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fulfillmentState', 'dialogAction_fulfillmentState' - The fulfillment state of the intent. The possible values are:
--
-- -   @Failed@ - The Lambda function associated with the intent failed to
--     fulfill the intent.
--
-- -   @Fulfilled@ - The intent has fulfilled by the Lambda function
--     associated with the intent.
--
-- -   @ReadyForFulfillment@ - All of the information necessary for the
--     intent is present and the intent ready to be fulfilled by the client
--     application.
--
-- 'message', 'dialogAction_message' - The message that should be shown to the user. If you don\'t specify a
-- message, Amazon Lex will use the message configured for the intent.
--
-- 'slotToElicit', 'dialogAction_slotToElicit' - The name of the slot that should be elicited from the user.
--
-- 'messageFormat', 'dialogAction_messageFormat' - -   @PlainText@ - The message contains plain UTF-8 text.
--
-- -   @CustomPayload@ - The message is a custom format for the client.
--
-- -   @SSML@ - The message contains text formatted for voice output.
--
-- -   @Composite@ - The message contains an escaped JSON object containing
--     one or more messages. For more information, see
--     <https://docs.aws.amazon.com/lex/latest/dg/howitworks-manage-prompts.html Message Groups>.
--
-- 'intentName', 'dialogAction_intentName' - The name of the intent.
--
-- 'slots', 'dialogAction_slots' - Map of the slots that have been gathered and their values.
--
-- 'type'', 'dialogAction_type' - The next action that the bot should take in its interaction with the
-- user. The possible values are:
--
-- -   @ConfirmIntent@ - The next action is asking the user if the intent
--     is complete and ready to be fulfilled. This is a yes\/no question
--     such as \"Place the order?\"
--
-- -   @Close@ - Indicates that the there will not be a response from the
--     user. For example, the statement \"Your order has been placed\" does
--     not require a response.
--
-- -   @Delegate@ - The next action is determined by Amazon Lex.
--
-- -   @ElicitIntent@ - The next action is to determine the intent that the
--     user wants to fulfill.
--
-- -   @ElicitSlot@ - The next action is to elicit a slot value from the
--     user.
newDialogAction ::
  -- | 'type''
  DialogActionType ->
  DialogAction
newDialogAction pType_ =
  DialogAction'
    { fulfillmentState = Prelude.Nothing,
      message = Prelude.Nothing,
      slotToElicit = Prelude.Nothing,
      messageFormat = Prelude.Nothing,
      intentName = Prelude.Nothing,
      slots = Prelude.Nothing,
      type' = pType_
    }

-- | The fulfillment state of the intent. The possible values are:
--
-- -   @Failed@ - The Lambda function associated with the intent failed to
--     fulfill the intent.
--
-- -   @Fulfilled@ - The intent has fulfilled by the Lambda function
--     associated with the intent.
--
-- -   @ReadyForFulfillment@ - All of the information necessary for the
--     intent is present and the intent ready to be fulfilled by the client
--     application.
dialogAction_fulfillmentState :: Lens.Lens' DialogAction (Prelude.Maybe FulfillmentState)
dialogAction_fulfillmentState = Lens.lens (\DialogAction' {fulfillmentState} -> fulfillmentState) (\s@DialogAction' {} a -> s {fulfillmentState = a} :: DialogAction)

-- | The message that should be shown to the user. If you don\'t specify a
-- message, Amazon Lex will use the message configured for the intent.
dialogAction_message :: Lens.Lens' DialogAction (Prelude.Maybe Prelude.Text)
dialogAction_message = Lens.lens (\DialogAction' {message} -> message) (\s@DialogAction' {} a -> s {message = a} :: DialogAction) Prelude.. Lens.mapping Core._Sensitive

-- | The name of the slot that should be elicited from the user.
dialogAction_slotToElicit :: Lens.Lens' DialogAction (Prelude.Maybe Prelude.Text)
dialogAction_slotToElicit = Lens.lens (\DialogAction' {slotToElicit} -> slotToElicit) (\s@DialogAction' {} a -> s {slotToElicit = a} :: DialogAction)

-- | -   @PlainText@ - The message contains plain UTF-8 text.
--
-- -   @CustomPayload@ - The message is a custom format for the client.
--
-- -   @SSML@ - The message contains text formatted for voice output.
--
-- -   @Composite@ - The message contains an escaped JSON object containing
--     one or more messages. For more information, see
--     <https://docs.aws.amazon.com/lex/latest/dg/howitworks-manage-prompts.html Message Groups>.
dialogAction_messageFormat :: Lens.Lens' DialogAction (Prelude.Maybe MessageFormatType)
dialogAction_messageFormat = Lens.lens (\DialogAction' {messageFormat} -> messageFormat) (\s@DialogAction' {} a -> s {messageFormat = a} :: DialogAction)

-- | The name of the intent.
dialogAction_intentName :: Lens.Lens' DialogAction (Prelude.Maybe Prelude.Text)
dialogAction_intentName = Lens.lens (\DialogAction' {intentName} -> intentName) (\s@DialogAction' {} a -> s {intentName = a} :: DialogAction)

-- | Map of the slots that have been gathered and their values.
dialogAction_slots :: Lens.Lens' DialogAction (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
dialogAction_slots = Lens.lens (\DialogAction' {slots} -> slots) (\s@DialogAction' {} a -> s {slots = a} :: DialogAction) Prelude.. Lens.mapping (Core._Sensitive Prelude.. Lens.coerced)

-- | The next action that the bot should take in its interaction with the
-- user. The possible values are:
--
-- -   @ConfirmIntent@ - The next action is asking the user if the intent
--     is complete and ready to be fulfilled. This is a yes\/no question
--     such as \"Place the order?\"
--
-- -   @Close@ - Indicates that the there will not be a response from the
--     user. For example, the statement \"Your order has been placed\" does
--     not require a response.
--
-- -   @Delegate@ - The next action is determined by Amazon Lex.
--
-- -   @ElicitIntent@ - The next action is to determine the intent that the
--     user wants to fulfill.
--
-- -   @ElicitSlot@ - The next action is to elicit a slot value from the
--     user.
dialogAction_type :: Lens.Lens' DialogAction DialogActionType
dialogAction_type = Lens.lens (\DialogAction' {type'} -> type') (\s@DialogAction' {} a -> s {type' = a} :: DialogAction)

instance Core.FromJSON DialogAction where
  parseJSON =
    Core.withObject
      "DialogAction"
      ( \x ->
          DialogAction'
            Prelude.<$> (x Core..:? "fulfillmentState")
            Prelude.<*> (x Core..:? "message")
            Prelude.<*> (x Core..:? "slotToElicit")
            Prelude.<*> (x Core..:? "messageFormat")
            Prelude.<*> (x Core..:? "intentName")
            Prelude.<*> (x Core..:? "slots" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..: "type")
      )

instance Prelude.Hashable DialogAction where
  hashWithSalt _salt DialogAction' {..} =
    _salt `Prelude.hashWithSalt` fulfillmentState
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` slotToElicit
      `Prelude.hashWithSalt` messageFormat
      `Prelude.hashWithSalt` intentName
      `Prelude.hashWithSalt` slots
      `Prelude.hashWithSalt` type'

instance Prelude.NFData DialogAction where
  rnf DialogAction' {..} =
    Prelude.rnf fulfillmentState
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf slotToElicit
      `Prelude.seq` Prelude.rnf messageFormat
      `Prelude.seq` Prelude.rnf intentName
      `Prelude.seq` Prelude.rnf slots
      `Prelude.seq` Prelude.rnf type'

instance Core.ToJSON DialogAction where
  toJSON DialogAction' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("fulfillmentState" Core..=)
              Prelude.<$> fulfillmentState,
            ("message" Core..=) Prelude.<$> message,
            ("slotToElicit" Core..=) Prelude.<$> slotToElicit,
            ("messageFormat" Core..=) Prelude.<$> messageFormat,
            ("intentName" Core..=) Prelude.<$> intentName,
            ("slots" Core..=) Prelude.<$> slots,
            Prelude.Just ("type" Core..= type')
          ]
      )
