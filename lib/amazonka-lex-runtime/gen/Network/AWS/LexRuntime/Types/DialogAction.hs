{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexRuntime.Types.DialogAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexRuntime.Types.DialogAction
  ( DialogAction (..),

    -- * Smart constructor
    mkDialogAction,

    -- * Lenses
    daSlots,
    daIntentName,
    daFulfillmentState,
    daMessageFormat,
    daMessage,
    daSlotToElicit,
    daType,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexRuntime.Types.DialogActionType
import Network.AWS.LexRuntime.Types.FulfillmentState
import Network.AWS.LexRuntime.Types.MessageFormatType
import qualified Network.AWS.Prelude as Lude

-- | Describes the next action that the bot should take in its interaction with the user and provides information about the context in which the action takes place. Use the @DialogAction@ data type to set the interaction to a specific state, or to return the interaction to a previous state.
--
-- /See:/ 'mkDialogAction' smart constructor.
data DialogAction = DialogAction'
  { slots ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    intentName :: Lude.Maybe Lude.Text,
    fulfillmentState :: Lude.Maybe FulfillmentState,
    messageFormat :: Lude.Maybe MessageFormatType,
    message :: Lude.Maybe (Lude.Sensitive Lude.Text),
    slotToElicit :: Lude.Maybe Lude.Text,
    type' :: DialogActionType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DialogAction' with the minimum fields required to make a request.
--
-- * 'fulfillmentState' - The fulfillment state of the intent. The possible values are:
--
--
--     * @Failed@ - The Lambda function associated with the intent failed to fulfill the intent.
--
--
--     * @Fulfilled@ - The intent has fulfilled by the Lambda function associated with the intent.
--
--
--     * @ReadyForFulfillment@ - All of the information necessary for the intent is present and the intent ready to be fulfilled by the client application.
--
--
-- * 'intentName' - The name of the intent.
-- * 'message' - The message that should be shown to the user. If you don't specify a message, Amazon Lex will use the message configured for the intent.
-- * 'messageFormat' -
--
--     * @PlainText@ - The message contains plain UTF-8 text.
--
--
--     * @CustomPayload@ - The message is a custom format for the client.
--
--
--     * @SSML@ - The message contains text formatted for voice output.
--
--
--     * @Composite@ - The message contains an escaped JSON object containing one or more messages. For more information, see <https://docs.aws.amazon.com/lex/latest/dg/howitworks-manage-prompts.html Message Groups> .
--
--
-- * 'slotToElicit' - The name of the slot that should be elicited from the user.
-- * 'slots' - Map of the slots that have been gathered and their values.
-- * 'type'' - The next action that the bot should take in its interaction with the user. The possible values are:
--
--
--     * @ConfirmIntent@ - The next action is asking the user if the intent is complete and ready to be fulfilled. This is a yes/no question such as "Place the order?"
--
--
--     * @Close@ - Indicates that the there will not be a response from the user. For example, the statement "Your order has been placed" does not require a response.
--
--
--     * @Delegate@ - The next action is determined by Amazon Lex.
--
--
--     * @ElicitIntent@ - The next action is to determine the intent that the user wants to fulfill.
--
--
--     * @ElicitSlot@ - The next action is to elicit a slot value from the user.
mkDialogAction ::
  -- | 'type''
  DialogActionType ->
  DialogAction
mkDialogAction pType_ =
  DialogAction'
    { slots = Lude.Nothing,
      intentName = Lude.Nothing,
      fulfillmentState = Lude.Nothing,
      messageFormat = Lude.Nothing,
      message = Lude.Nothing,
      slotToElicit = Lude.Nothing,
      type' = pType_
    }

-- | Map of the slots that have been gathered and their values.
--
-- /Note:/ Consider using 'slots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daSlots :: Lens.Lens' DialogAction (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
daSlots = Lens.lens (slots :: DialogAction -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {slots = a} :: DialogAction)
{-# DEPRECATED daSlots "Use generic-lens or generic-optics with 'slots' instead." #-}

-- | The name of the intent.
--
-- /Note:/ Consider using 'intentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daIntentName :: Lens.Lens' DialogAction (Lude.Maybe Lude.Text)
daIntentName = Lens.lens (intentName :: DialogAction -> Lude.Maybe Lude.Text) (\s a -> s {intentName = a} :: DialogAction)
{-# DEPRECATED daIntentName "Use generic-lens or generic-optics with 'intentName' instead." #-}

-- | The fulfillment state of the intent. The possible values are:
--
--
--     * @Failed@ - The Lambda function associated with the intent failed to fulfill the intent.
--
--
--     * @Fulfilled@ - The intent has fulfilled by the Lambda function associated with the intent.
--
--
--     * @ReadyForFulfillment@ - All of the information necessary for the intent is present and the intent ready to be fulfilled by the client application.
--
--
--
-- /Note:/ Consider using 'fulfillmentState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daFulfillmentState :: Lens.Lens' DialogAction (Lude.Maybe FulfillmentState)
daFulfillmentState = Lens.lens (fulfillmentState :: DialogAction -> Lude.Maybe FulfillmentState) (\s a -> s {fulfillmentState = a} :: DialogAction)
{-# DEPRECATED daFulfillmentState "Use generic-lens or generic-optics with 'fulfillmentState' instead." #-}

-- |
--
--     * @PlainText@ - The message contains plain UTF-8 text.
--
--
--     * @CustomPayload@ - The message is a custom format for the client.
--
--
--     * @SSML@ - The message contains text formatted for voice output.
--
--
--     * @Composite@ - The message contains an escaped JSON object containing one or more messages. For more information, see <https://docs.aws.amazon.com/lex/latest/dg/howitworks-manage-prompts.html Message Groups> .
--
--
--
-- /Note:/ Consider using 'messageFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daMessageFormat :: Lens.Lens' DialogAction (Lude.Maybe MessageFormatType)
daMessageFormat = Lens.lens (messageFormat :: DialogAction -> Lude.Maybe MessageFormatType) (\s a -> s {messageFormat = a} :: DialogAction)
{-# DEPRECATED daMessageFormat "Use generic-lens or generic-optics with 'messageFormat' instead." #-}

-- | The message that should be shown to the user. If you don't specify a message, Amazon Lex will use the message configured for the intent.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daMessage :: Lens.Lens' DialogAction (Lude.Maybe (Lude.Sensitive Lude.Text))
daMessage = Lens.lens (message :: DialogAction -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {message = a} :: DialogAction)
{-# DEPRECATED daMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The name of the slot that should be elicited from the user.
--
-- /Note:/ Consider using 'slotToElicit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daSlotToElicit :: Lens.Lens' DialogAction (Lude.Maybe Lude.Text)
daSlotToElicit = Lens.lens (slotToElicit :: DialogAction -> Lude.Maybe Lude.Text) (\s a -> s {slotToElicit = a} :: DialogAction)
{-# DEPRECATED daSlotToElicit "Use generic-lens or generic-optics with 'slotToElicit' instead." #-}

-- | The next action that the bot should take in its interaction with the user. The possible values are:
--
--
--     * @ConfirmIntent@ - The next action is asking the user if the intent is complete and ready to be fulfilled. This is a yes/no question such as "Place the order?"
--
--
--     * @Close@ - Indicates that the there will not be a response from the user. For example, the statement "Your order has been placed" does not require a response.
--
--
--     * @Delegate@ - The next action is determined by Amazon Lex.
--
--
--     * @ElicitIntent@ - The next action is to determine the intent that the user wants to fulfill.
--
--
--     * @ElicitSlot@ - The next action is to elicit a slot value from the user.
--
--
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daType :: Lens.Lens' DialogAction DialogActionType
daType = Lens.lens (type' :: DialogAction -> DialogActionType) (\s a -> s {type' = a} :: DialogAction)
{-# DEPRECATED daType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON DialogAction where
  parseJSON =
    Lude.withObject
      "DialogAction"
      ( \x ->
          DialogAction'
            Lude.<$> (x Lude..:? "slots" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "intentName")
            Lude.<*> (x Lude..:? "fulfillmentState")
            Lude.<*> (x Lude..:? "messageFormat")
            Lude.<*> (x Lude..:? "message")
            Lude.<*> (x Lude..:? "slotToElicit")
            Lude.<*> (x Lude..: "type")
      )

instance Lude.ToJSON DialogAction where
  toJSON DialogAction' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("slots" Lude..=) Lude.<$> slots,
            ("intentName" Lude..=) Lude.<$> intentName,
            ("fulfillmentState" Lude..=) Lude.<$> fulfillmentState,
            ("messageFormat" Lude..=) Lude.<$> messageFormat,
            ("message" Lude..=) Lude.<$> message,
            ("slotToElicit" Lude..=) Lude.<$> slotToElicit,
            Lude.Just ("type" Lude..= type')
          ]
      )
