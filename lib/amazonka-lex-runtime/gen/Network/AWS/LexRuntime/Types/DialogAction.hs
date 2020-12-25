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
    daType,
    daFulfillmentState,
    daIntentName,
    daMessage,
    daMessageFormat,
    daSlotToElicit,
    daSlots,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexRuntime.Types.DialogActionType as Types
import qualified Network.AWS.LexRuntime.Types.FulfillmentState as Types
import qualified Network.AWS.LexRuntime.Types.IntentName as Types
import qualified Network.AWS.LexRuntime.Types.MessageFormatType as Types
import qualified Network.AWS.LexRuntime.Types.String as Types
import qualified Network.AWS.LexRuntime.Types.Text as Types
import qualified Network.AWS.Prelude as Core

-- | Describes the next action that the bot should take in its interaction with the user and provides information about the context in which the action takes place. Use the @DialogAction@ data type to set the interaction to a specific state, or to return the interaction to a previous state.
--
-- /See:/ 'mkDialogAction' smart constructor.
data DialogAction = DialogAction'
  { -- | The next action that the bot should take in its interaction with the user. The possible values are:
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
    type' :: Types.DialogActionType,
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
    fulfillmentState :: Core.Maybe Types.FulfillmentState,
    -- | The name of the intent.
    intentName :: Core.Maybe Types.IntentName,
    -- | The message that should be shown to the user. If you don't specify a message, Amazon Lex will use the message configured for the intent.
    message :: Core.Maybe Types.Text,
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
    messageFormat :: Core.Maybe Types.MessageFormatType,
    -- | The name of the slot that should be elicited from the user.
    slotToElicit :: Core.Maybe Types.String,
    -- | Map of the slots that have been gathered and their values.
    slots :: Core.Maybe (Core.HashMap Types.String Types.String)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DialogAction' value with any optional fields omitted.
mkDialogAction ::
  -- | 'type\''
  Types.DialogActionType ->
  DialogAction
mkDialogAction type' =
  DialogAction'
    { type',
      fulfillmentState = Core.Nothing,
      intentName = Core.Nothing,
      message = Core.Nothing,
      messageFormat = Core.Nothing,
      slotToElicit = Core.Nothing,
      slots = Core.Nothing
    }

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
daType :: Lens.Lens' DialogAction Types.DialogActionType
daType = Lens.field @"type'"
{-# DEPRECATED daType "Use generic-lens or generic-optics with 'type'' instead." #-}

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
daFulfillmentState :: Lens.Lens' DialogAction (Core.Maybe Types.FulfillmentState)
daFulfillmentState = Lens.field @"fulfillmentState"
{-# DEPRECATED daFulfillmentState "Use generic-lens or generic-optics with 'fulfillmentState' instead." #-}

-- | The name of the intent.
--
-- /Note:/ Consider using 'intentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daIntentName :: Lens.Lens' DialogAction (Core.Maybe Types.IntentName)
daIntentName = Lens.field @"intentName"
{-# DEPRECATED daIntentName "Use generic-lens or generic-optics with 'intentName' instead." #-}

-- | The message that should be shown to the user. If you don't specify a message, Amazon Lex will use the message configured for the intent.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daMessage :: Lens.Lens' DialogAction (Core.Maybe Types.Text)
daMessage = Lens.field @"message"
{-# DEPRECATED daMessage "Use generic-lens or generic-optics with 'message' instead." #-}

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
daMessageFormat :: Lens.Lens' DialogAction (Core.Maybe Types.MessageFormatType)
daMessageFormat = Lens.field @"messageFormat"
{-# DEPRECATED daMessageFormat "Use generic-lens or generic-optics with 'messageFormat' instead." #-}

-- | The name of the slot that should be elicited from the user.
--
-- /Note:/ Consider using 'slotToElicit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daSlotToElicit :: Lens.Lens' DialogAction (Core.Maybe Types.String)
daSlotToElicit = Lens.field @"slotToElicit"
{-# DEPRECATED daSlotToElicit "Use generic-lens or generic-optics with 'slotToElicit' instead." #-}

-- | Map of the slots that have been gathered and their values.
--
-- /Note:/ Consider using 'slots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daSlots :: Lens.Lens' DialogAction (Core.Maybe (Core.HashMap Types.String Types.String))
daSlots = Lens.field @"slots"
{-# DEPRECATED daSlots "Use generic-lens or generic-optics with 'slots' instead." #-}

instance Core.FromJSON DialogAction where
  toJSON DialogAction {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("type" Core..= type'),
            ("fulfillmentState" Core..=) Core.<$> fulfillmentState,
            ("intentName" Core..=) Core.<$> intentName,
            ("message" Core..=) Core.<$> message,
            ("messageFormat" Core..=) Core.<$> messageFormat,
            ("slotToElicit" Core..=) Core.<$> slotToElicit,
            ("slots" Core..=) Core.<$> slots
          ]
      )

instance Core.FromJSON DialogAction where
  parseJSON =
    Core.withObject "DialogAction" Core.$
      \x ->
        DialogAction'
          Core.<$> (x Core..: "type")
          Core.<*> (x Core..:? "fulfillmentState")
          Core.<*> (x Core..:? "intentName")
          Core.<*> (x Core..:? "message")
          Core.<*> (x Core..:? "messageFormat")
          Core.<*> (x Core..:? "slotToElicit")
          Core.<*> (x Core..:? "slots")
