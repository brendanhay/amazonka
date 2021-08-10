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
-- Module      : Network.AWS.LexRuntime.Types.IntentSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexRuntime.Types.IntentSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexRuntime.Types.ConfirmationStatus
import Network.AWS.LexRuntime.Types.DialogActionType
import Network.AWS.LexRuntime.Types.FulfillmentState
import qualified Network.AWS.Prelude as Prelude

-- | Provides information about the state of an intent. You can use this
-- information to get the current state of an intent so that you can
-- process the intent, or so that you can return the intent to its previous
-- state.
--
-- /See:/ 'newIntentSummary' smart constructor.
data IntentSummary = IntentSummary'
  { -- | The name of the intent.
    intentName :: Prelude.Maybe Prelude.Text,
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
    fulfillmentState :: Prelude.Maybe FulfillmentState,
    -- | Map of the slots that have been gathered and their values.
    slots :: Prelude.Maybe (Core.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | A user-defined label that identifies a particular intent. You can use
    -- this label to return to a previous intent.
    --
    -- Use the @checkpointLabelFilter@ parameter of the @GetSessionRequest@
    -- operation to filter the intents returned by the operation to those with
    -- only the specified label.
    checkpointLabel :: Prelude.Maybe Prelude.Text,
    -- | The next slot to elicit from the user. If there is not slot to elicit,
    -- the field is blank.
    slotToElicit :: Prelude.Maybe Prelude.Text,
    -- | The status of the intent after the user responds to the confirmation
    -- prompt. If the user confirms the intent, Amazon Lex sets this field to
    -- @Confirmed@. If the user denies the intent, Amazon Lex sets this value
    -- to @Denied@. The possible values are:
    --
    -- -   @Confirmed@ - The user has responded \"Yes\" to the confirmation
    --     prompt, confirming that the intent is complete and that it is ready
    --     to be fulfilled.
    --
    -- -   @Denied@ - The user has responded \"No\" to the confirmation prompt.
    --
    -- -   @None@ - The user has never been prompted for confirmation; or, the
    --     user was prompted but did not confirm or deny the prompt.
    confirmationStatus :: Prelude.Maybe ConfirmationStatus,
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
    -- -   @ElicitIntent@ - The next action is to determine the intent that the
    --     user wants to fulfill.
    --
    -- -   @ElicitSlot@ - The next action is to elicit a slot value from the
    --     user.
    dialogActionType :: DialogActionType
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IntentSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'intentName', 'intentSummary_intentName' - The name of the intent.
--
-- 'fulfillmentState', 'intentSummary_fulfillmentState' - The fulfillment state of the intent. The possible values are:
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
-- 'slots', 'intentSummary_slots' - Map of the slots that have been gathered and their values.
--
-- 'checkpointLabel', 'intentSummary_checkpointLabel' - A user-defined label that identifies a particular intent. You can use
-- this label to return to a previous intent.
--
-- Use the @checkpointLabelFilter@ parameter of the @GetSessionRequest@
-- operation to filter the intents returned by the operation to those with
-- only the specified label.
--
-- 'slotToElicit', 'intentSummary_slotToElicit' - The next slot to elicit from the user. If there is not slot to elicit,
-- the field is blank.
--
-- 'confirmationStatus', 'intentSummary_confirmationStatus' - The status of the intent after the user responds to the confirmation
-- prompt. If the user confirms the intent, Amazon Lex sets this field to
-- @Confirmed@. If the user denies the intent, Amazon Lex sets this value
-- to @Denied@. The possible values are:
--
-- -   @Confirmed@ - The user has responded \"Yes\" to the confirmation
--     prompt, confirming that the intent is complete and that it is ready
--     to be fulfilled.
--
-- -   @Denied@ - The user has responded \"No\" to the confirmation prompt.
--
-- -   @None@ - The user has never been prompted for confirmation; or, the
--     user was prompted but did not confirm or deny the prompt.
--
-- 'dialogActionType', 'intentSummary_dialogActionType' - The next action that the bot should take in its interaction with the
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
-- -   @ElicitIntent@ - The next action is to determine the intent that the
--     user wants to fulfill.
--
-- -   @ElicitSlot@ - The next action is to elicit a slot value from the
--     user.
newIntentSummary ::
  -- | 'dialogActionType'
  DialogActionType ->
  IntentSummary
newIntentSummary pDialogActionType_ =
  IntentSummary'
    { intentName = Prelude.Nothing,
      fulfillmentState = Prelude.Nothing,
      slots = Prelude.Nothing,
      checkpointLabel = Prelude.Nothing,
      slotToElicit = Prelude.Nothing,
      confirmationStatus = Prelude.Nothing,
      dialogActionType = pDialogActionType_
    }

-- | The name of the intent.
intentSummary_intentName :: Lens.Lens' IntentSummary (Prelude.Maybe Prelude.Text)
intentSummary_intentName = Lens.lens (\IntentSummary' {intentName} -> intentName) (\s@IntentSummary' {} a -> s {intentName = a} :: IntentSummary)

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
intentSummary_fulfillmentState :: Lens.Lens' IntentSummary (Prelude.Maybe FulfillmentState)
intentSummary_fulfillmentState = Lens.lens (\IntentSummary' {fulfillmentState} -> fulfillmentState) (\s@IntentSummary' {} a -> s {fulfillmentState = a} :: IntentSummary)

-- | Map of the slots that have been gathered and their values.
intentSummary_slots :: Lens.Lens' IntentSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
intentSummary_slots = Lens.lens (\IntentSummary' {slots} -> slots) (\s@IntentSummary' {} a -> s {slots = a} :: IntentSummary) Prelude.. Lens.mapping (Core._Sensitive Prelude.. Lens._Coerce)

-- | A user-defined label that identifies a particular intent. You can use
-- this label to return to a previous intent.
--
-- Use the @checkpointLabelFilter@ parameter of the @GetSessionRequest@
-- operation to filter the intents returned by the operation to those with
-- only the specified label.
intentSummary_checkpointLabel :: Lens.Lens' IntentSummary (Prelude.Maybe Prelude.Text)
intentSummary_checkpointLabel = Lens.lens (\IntentSummary' {checkpointLabel} -> checkpointLabel) (\s@IntentSummary' {} a -> s {checkpointLabel = a} :: IntentSummary)

-- | The next slot to elicit from the user. If there is not slot to elicit,
-- the field is blank.
intentSummary_slotToElicit :: Lens.Lens' IntentSummary (Prelude.Maybe Prelude.Text)
intentSummary_slotToElicit = Lens.lens (\IntentSummary' {slotToElicit} -> slotToElicit) (\s@IntentSummary' {} a -> s {slotToElicit = a} :: IntentSummary)

-- | The status of the intent after the user responds to the confirmation
-- prompt. If the user confirms the intent, Amazon Lex sets this field to
-- @Confirmed@. If the user denies the intent, Amazon Lex sets this value
-- to @Denied@. The possible values are:
--
-- -   @Confirmed@ - The user has responded \"Yes\" to the confirmation
--     prompt, confirming that the intent is complete and that it is ready
--     to be fulfilled.
--
-- -   @Denied@ - The user has responded \"No\" to the confirmation prompt.
--
-- -   @None@ - The user has never been prompted for confirmation; or, the
--     user was prompted but did not confirm or deny the prompt.
intentSummary_confirmationStatus :: Lens.Lens' IntentSummary (Prelude.Maybe ConfirmationStatus)
intentSummary_confirmationStatus = Lens.lens (\IntentSummary' {confirmationStatus} -> confirmationStatus) (\s@IntentSummary' {} a -> s {confirmationStatus = a} :: IntentSummary)

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
-- -   @ElicitIntent@ - The next action is to determine the intent that the
--     user wants to fulfill.
--
-- -   @ElicitSlot@ - The next action is to elicit a slot value from the
--     user.
intentSummary_dialogActionType :: Lens.Lens' IntentSummary DialogActionType
intentSummary_dialogActionType = Lens.lens (\IntentSummary' {dialogActionType} -> dialogActionType) (\s@IntentSummary' {} a -> s {dialogActionType = a} :: IntentSummary)

instance Core.FromJSON IntentSummary where
  parseJSON =
    Core.withObject
      "IntentSummary"
      ( \x ->
          IntentSummary'
            Prelude.<$> (x Core..:? "intentName")
            Prelude.<*> (x Core..:? "fulfillmentState")
            Prelude.<*> (x Core..:? "slots" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "checkpointLabel")
            Prelude.<*> (x Core..:? "slotToElicit")
            Prelude.<*> (x Core..:? "confirmationStatus")
            Prelude.<*> (x Core..: "dialogActionType")
      )

instance Prelude.Hashable IntentSummary

instance Prelude.NFData IntentSummary

instance Core.ToJSON IntentSummary where
  toJSON IntentSummary' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("intentName" Core..=) Prelude.<$> intentName,
            ("fulfillmentState" Core..=)
              Prelude.<$> fulfillmentState,
            ("slots" Core..=) Prelude.<$> slots,
            ("checkpointLabel" Core..=)
              Prelude.<$> checkpointLabel,
            ("slotToElicit" Core..=) Prelude.<$> slotToElicit,
            ("confirmationStatus" Core..=)
              Prelude.<$> confirmationStatus,
            Prelude.Just
              ("dialogActionType" Core..= dialogActionType)
          ]
      )
