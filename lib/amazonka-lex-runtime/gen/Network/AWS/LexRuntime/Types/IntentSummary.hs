{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexRuntime.Types.IntentSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.LexRuntime.Types.IntentSummary
  ( IntentSummary (..)
  -- * Smart constructor
  , mkIntentSummary
  -- * Lenses
  , isDialogActionType
  , isCheckpointLabel
  , isConfirmationStatus
  , isFulfillmentState
  , isIntentName
  , isSlotToElicit
  , isSlots
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexRuntime.Types.ConfirmationStatus as Types
import qualified Network.AWS.LexRuntime.Types.DialogActionType as Types
import qualified Network.AWS.LexRuntime.Types.FulfillmentState as Types
import qualified Network.AWS.LexRuntime.Types.IntentName as Types
import qualified Network.AWS.LexRuntime.Types.IntentSummaryCheckpointLabel as Types
import qualified Network.AWS.Prelude as Core

-- | Provides information about the state of an intent. You can use this information to get the current state of an intent so that you can process the intent, or so that you can return the intent to its previous state.
--
-- /See:/ 'mkIntentSummary' smart constructor.
data IntentSummary = IntentSummary'
  { dialogActionType :: Types.DialogActionType
    -- ^ The next action that the bot should take in its interaction with the user. The possible values are:
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
  , checkpointLabel :: Core.Maybe Types.IntentSummaryCheckpointLabel
    -- ^ A user-defined label that identifies a particular intent. You can use this label to return to a previous intent. 
--
-- Use the @checkpointLabelFilter@ parameter of the @GetSessionRequest@ operation to filter the intents returned by the operation to those with only the specified label.
  , confirmationStatus :: Core.Maybe Types.ConfirmationStatus
    -- ^ The status of the intent after the user responds to the confirmation prompt. If the user confirms the intent, Amazon Lex sets this field to @Confirmed@ . If the user denies the intent, Amazon Lex sets this value to @Denied@ . The possible values are:
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
  , fulfillmentState :: Core.Maybe Types.FulfillmentState
    -- ^ The fulfillment state of the intent. The possible values are:
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
  , intentName :: Core.Maybe Types.IntentName
    -- ^ The name of the intent.
  , slotToElicit :: Core.Maybe Core.Text
    -- ^ The next slot to elicit from the user. If there is not slot to elicit, the field is blank.
  , slots :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ Map of the slots that have been gathered and their values. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'IntentSummary' value with any optional fields omitted.
mkIntentSummary
    :: Types.DialogActionType -- ^ 'dialogActionType'
    -> IntentSummary
mkIntentSummary dialogActionType
  = IntentSummary'{dialogActionType, checkpointLabel = Core.Nothing,
                   confirmationStatus = Core.Nothing, fulfillmentState = Core.Nothing,
                   intentName = Core.Nothing, slotToElicit = Core.Nothing,
                   slots = Core.Nothing}

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
isDialogActionType :: Lens.Lens' IntentSummary Types.DialogActionType
isDialogActionType = Lens.field @"dialogActionType"
{-# INLINEABLE isDialogActionType #-}
{-# DEPRECATED dialogActionType "Use generic-lens or generic-optics with 'dialogActionType' instead"  #-}

-- | A user-defined label that identifies a particular intent. You can use this label to return to a previous intent. 
--
-- Use the @checkpointLabelFilter@ parameter of the @GetSessionRequest@ operation to filter the intents returned by the operation to those with only the specified label.
--
-- /Note:/ Consider using 'checkpointLabel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isCheckpointLabel :: Lens.Lens' IntentSummary (Core.Maybe Types.IntentSummaryCheckpointLabel)
isCheckpointLabel = Lens.field @"checkpointLabel"
{-# INLINEABLE isCheckpointLabel #-}
{-# DEPRECATED checkpointLabel "Use generic-lens or generic-optics with 'checkpointLabel' instead"  #-}

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
isConfirmationStatus :: Lens.Lens' IntentSummary (Core.Maybe Types.ConfirmationStatus)
isConfirmationStatus = Lens.field @"confirmationStatus"
{-# INLINEABLE isConfirmationStatus #-}
{-# DEPRECATED confirmationStatus "Use generic-lens or generic-optics with 'confirmationStatus' instead"  #-}

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
isFulfillmentState :: Lens.Lens' IntentSummary (Core.Maybe Types.FulfillmentState)
isFulfillmentState = Lens.field @"fulfillmentState"
{-# INLINEABLE isFulfillmentState #-}
{-# DEPRECATED fulfillmentState "Use generic-lens or generic-optics with 'fulfillmentState' instead"  #-}

-- | The name of the intent.
--
-- /Note:/ Consider using 'intentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isIntentName :: Lens.Lens' IntentSummary (Core.Maybe Types.IntentName)
isIntentName = Lens.field @"intentName"
{-# INLINEABLE isIntentName #-}
{-# DEPRECATED intentName "Use generic-lens or generic-optics with 'intentName' instead"  #-}

-- | The next slot to elicit from the user. If there is not slot to elicit, the field is blank.
--
-- /Note:/ Consider using 'slotToElicit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isSlotToElicit :: Lens.Lens' IntentSummary (Core.Maybe Core.Text)
isSlotToElicit = Lens.field @"slotToElicit"
{-# INLINEABLE isSlotToElicit #-}
{-# DEPRECATED slotToElicit "Use generic-lens or generic-optics with 'slotToElicit' instead"  #-}

-- | Map of the slots that have been gathered and their values. 
--
-- /Note:/ Consider using 'slots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isSlots :: Lens.Lens' IntentSummary (Core.Maybe (Core.HashMap Core.Text Core.Text))
isSlots = Lens.field @"slots"
{-# INLINEABLE isSlots #-}
{-# DEPRECATED slots "Use generic-lens or generic-optics with 'slots' instead"  #-}

instance Core.FromJSON IntentSummary where
        toJSON IntentSummary{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("dialogActionType" Core..= dialogActionType),
                  ("checkpointLabel" Core..=) Core.<$> checkpointLabel,
                  ("confirmationStatus" Core..=) Core.<$> confirmationStatus,
                  ("fulfillmentState" Core..=) Core.<$> fulfillmentState,
                  ("intentName" Core..=) Core.<$> intentName,
                  ("slotToElicit" Core..=) Core.<$> slotToElicit,
                  ("slots" Core..=) Core.<$> slots])

instance Core.FromJSON IntentSummary where
        parseJSON
          = Core.withObject "IntentSummary" Core.$
              \ x ->
                IntentSummary' Core.<$>
                  (x Core..: "dialogActionType") Core.<*>
                    x Core..:? "checkpointLabel"
                    Core.<*> x Core..:? "confirmationStatus"
                    Core.<*> x Core..:? "fulfillmentState"
                    Core.<*> x Core..:? "intentName"
                    Core.<*> x Core..:? "slotToElicit"
                    Core.<*> x Core..:? "slots"
