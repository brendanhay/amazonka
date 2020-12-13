{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexRuntime.Types.IntentSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexRuntime.Types.IntentSummary
  ( IntentSummary (..),

    -- * Smart constructor
    mkIntentSummary,

    -- * Lenses
    isCheckpointLabel,
    isSlots,
    isIntentName,
    isDialogActionType,
    isFulfillmentState,
    isConfirmationStatus,
    isSlotToElicit,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexRuntime.Types.ConfirmationStatus
import Network.AWS.LexRuntime.Types.DialogActionType
import Network.AWS.LexRuntime.Types.FulfillmentState
import qualified Network.AWS.Prelude as Lude

-- | Provides information about the state of an intent. You can use this information to get the current state of an intent so that you can process the intent, or so that you can return the intent to its previous state.
--
-- /See:/ 'mkIntentSummary' smart constructor.
data IntentSummary = IntentSummary'
  { -- | A user-defined label that identifies a particular intent. You can use this label to return to a previous intent.
    --
    -- Use the @checkpointLabelFilter@ parameter of the @GetSessionRequest@ operation to filter the intents returned by the operation to those with only the specified label.
    checkpointLabel :: Lude.Maybe Lude.Text,
    -- | Map of the slots that have been gathered and their values.
    slots :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The name of the intent.
    intentName :: Lude.Maybe Lude.Text,
    -- | The next action that the bot should take in its interaction with the user. The possible values are:
    --
    --
    --     * @ConfirmIntent@ - The next action is asking the user if the intent is complete and ready to be fulfilled. This is a yes/no question such as "Place the order?"
    --
    --
    --     * @Close@ - Indicates that the there will not be a response from the user. For example, the statement "Your order has been placed" does not require a response.
    --
    --
    --     * @ElicitIntent@ - The next action is to determine the intent that the user wants to fulfill.
    --
    --
    --     * @ElicitSlot@ - The next action is to elicit a slot value from the user.
    dialogActionType :: DialogActionType,
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
    fulfillmentState :: Lude.Maybe FulfillmentState,
    -- | The status of the intent after the user responds to the confirmation prompt. If the user confirms the intent, Amazon Lex sets this field to @Confirmed@ . If the user denies the intent, Amazon Lex sets this value to @Denied@ . The possible values are:
    --
    --
    --     * @Confirmed@ - The user has responded "Yes" to the confirmation prompt, confirming that the intent is complete and that it is ready to be fulfilled.
    --
    --
    --     * @Denied@ - The user has responded "No" to the confirmation prompt.
    --
    --
    --     * @None@ - The user has never been prompted for confirmation; or, the user was prompted but did not confirm or deny the prompt.
    confirmationStatus :: Lude.Maybe ConfirmationStatus,
    -- | The next slot to elicit from the user. If there is not slot to elicit, the field is blank.
    slotToElicit :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'IntentSummary' with the minimum fields required to make a request.
--
-- * 'checkpointLabel' - A user-defined label that identifies a particular intent. You can use this label to return to a previous intent.
--
-- Use the @checkpointLabelFilter@ parameter of the @GetSessionRequest@ operation to filter the intents returned by the operation to those with only the specified label.
-- * 'slots' - Map of the slots that have been gathered and their values.
-- * 'intentName' - The name of the intent.
-- * 'dialogActionType' - The next action that the bot should take in its interaction with the user. The possible values are:
--
--
--     * @ConfirmIntent@ - The next action is asking the user if the intent is complete and ready to be fulfilled. This is a yes/no question such as "Place the order?"
--
--
--     * @Close@ - Indicates that the there will not be a response from the user. For example, the statement "Your order has been placed" does not require a response.
--
--
--     * @ElicitIntent@ - The next action is to determine the intent that the user wants to fulfill.
--
--
--     * @ElicitSlot@ - The next action is to elicit a slot value from the user.
--
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
-- * 'confirmationStatus' - The status of the intent after the user responds to the confirmation prompt. If the user confirms the intent, Amazon Lex sets this field to @Confirmed@ . If the user denies the intent, Amazon Lex sets this value to @Denied@ . The possible values are:
--
--
--     * @Confirmed@ - The user has responded "Yes" to the confirmation prompt, confirming that the intent is complete and that it is ready to be fulfilled.
--
--
--     * @Denied@ - The user has responded "No" to the confirmation prompt.
--
--
--     * @None@ - The user has never been prompted for confirmation; or, the user was prompted but did not confirm or deny the prompt.
--
--
-- * 'slotToElicit' - The next slot to elicit from the user. If there is not slot to elicit, the field is blank.
mkIntentSummary ::
  -- | 'dialogActionType'
  DialogActionType ->
  IntentSummary
mkIntentSummary pDialogActionType_ =
  IntentSummary'
    { checkpointLabel = Lude.Nothing,
      slots = Lude.Nothing,
      intentName = Lude.Nothing,
      dialogActionType = pDialogActionType_,
      fulfillmentState = Lude.Nothing,
      confirmationStatus = Lude.Nothing,
      slotToElicit = Lude.Nothing
    }

-- | A user-defined label that identifies a particular intent. You can use this label to return to a previous intent.
--
-- Use the @checkpointLabelFilter@ parameter of the @GetSessionRequest@ operation to filter the intents returned by the operation to those with only the specified label.
--
-- /Note:/ Consider using 'checkpointLabel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isCheckpointLabel :: Lens.Lens' IntentSummary (Lude.Maybe Lude.Text)
isCheckpointLabel = Lens.lens (checkpointLabel :: IntentSummary -> Lude.Maybe Lude.Text) (\s a -> s {checkpointLabel = a} :: IntentSummary)
{-# DEPRECATED isCheckpointLabel "Use generic-lens or generic-optics with 'checkpointLabel' instead." #-}

-- | Map of the slots that have been gathered and their values.
--
-- /Note:/ Consider using 'slots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isSlots :: Lens.Lens' IntentSummary (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
isSlots = Lens.lens (slots :: IntentSummary -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {slots = a} :: IntentSummary)
{-# DEPRECATED isSlots "Use generic-lens or generic-optics with 'slots' instead." #-}

-- | The name of the intent.
--
-- /Note:/ Consider using 'intentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isIntentName :: Lens.Lens' IntentSummary (Lude.Maybe Lude.Text)
isIntentName = Lens.lens (intentName :: IntentSummary -> Lude.Maybe Lude.Text) (\s a -> s {intentName = a} :: IntentSummary)
{-# DEPRECATED isIntentName "Use generic-lens or generic-optics with 'intentName' instead." #-}

-- | The next action that the bot should take in its interaction with the user. The possible values are:
--
--
--     * @ConfirmIntent@ - The next action is asking the user if the intent is complete and ready to be fulfilled. This is a yes/no question such as "Place the order?"
--
--
--     * @Close@ - Indicates that the there will not be a response from the user. For example, the statement "Your order has been placed" does not require a response.
--
--
--     * @ElicitIntent@ - The next action is to determine the intent that the user wants to fulfill.
--
--
--     * @ElicitSlot@ - The next action is to elicit a slot value from the user.
--
--
--
-- /Note:/ Consider using 'dialogActionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isDialogActionType :: Lens.Lens' IntentSummary DialogActionType
isDialogActionType = Lens.lens (dialogActionType :: IntentSummary -> DialogActionType) (\s a -> s {dialogActionType = a} :: IntentSummary)
{-# DEPRECATED isDialogActionType "Use generic-lens or generic-optics with 'dialogActionType' instead." #-}

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
isFulfillmentState :: Lens.Lens' IntentSummary (Lude.Maybe FulfillmentState)
isFulfillmentState = Lens.lens (fulfillmentState :: IntentSummary -> Lude.Maybe FulfillmentState) (\s a -> s {fulfillmentState = a} :: IntentSummary)
{-# DEPRECATED isFulfillmentState "Use generic-lens or generic-optics with 'fulfillmentState' instead." #-}

-- | The status of the intent after the user responds to the confirmation prompt. If the user confirms the intent, Amazon Lex sets this field to @Confirmed@ . If the user denies the intent, Amazon Lex sets this value to @Denied@ . The possible values are:
--
--
--     * @Confirmed@ - The user has responded "Yes" to the confirmation prompt, confirming that the intent is complete and that it is ready to be fulfilled.
--
--
--     * @Denied@ - The user has responded "No" to the confirmation prompt.
--
--
--     * @None@ - The user has never been prompted for confirmation; or, the user was prompted but did not confirm or deny the prompt.
--
--
--
-- /Note:/ Consider using 'confirmationStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isConfirmationStatus :: Lens.Lens' IntentSummary (Lude.Maybe ConfirmationStatus)
isConfirmationStatus = Lens.lens (confirmationStatus :: IntentSummary -> Lude.Maybe ConfirmationStatus) (\s a -> s {confirmationStatus = a} :: IntentSummary)
{-# DEPRECATED isConfirmationStatus "Use generic-lens or generic-optics with 'confirmationStatus' instead." #-}

-- | The next slot to elicit from the user. If there is not slot to elicit, the field is blank.
--
-- /Note:/ Consider using 'slotToElicit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isSlotToElicit :: Lens.Lens' IntentSummary (Lude.Maybe Lude.Text)
isSlotToElicit = Lens.lens (slotToElicit :: IntentSummary -> Lude.Maybe Lude.Text) (\s a -> s {slotToElicit = a} :: IntentSummary)
{-# DEPRECATED isSlotToElicit "Use generic-lens or generic-optics with 'slotToElicit' instead." #-}

instance Lude.FromJSON IntentSummary where
  parseJSON =
    Lude.withObject
      "IntentSummary"
      ( \x ->
          IntentSummary'
            Lude.<$> (x Lude..:? "checkpointLabel")
            Lude.<*> (x Lude..:? "slots" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "intentName")
            Lude.<*> (x Lude..: "dialogActionType")
            Lude.<*> (x Lude..:? "fulfillmentState")
            Lude.<*> (x Lude..:? "confirmationStatus")
            Lude.<*> (x Lude..:? "slotToElicit")
      )

instance Lude.ToJSON IntentSummary where
  toJSON IntentSummary' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("checkpointLabel" Lude..=) Lude.<$> checkpointLabel,
            ("slots" Lude..=) Lude.<$> slots,
            ("intentName" Lude..=) Lude.<$> intentName,
            Lude.Just ("dialogActionType" Lude..= dialogActionType),
            ("fulfillmentState" Lude..=) Lude.<$> fulfillmentState,
            ("confirmationStatus" Lude..=) Lude.<$> confirmationStatus,
            ("slotToElicit" Lude..=) Lude.<$> slotToElicit
          ]
      )
