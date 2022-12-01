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
-- Module      : Amazonka.LexV2Models.Types.IntentConfirmationSetting
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.IntentConfirmationSetting where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LexV2Models.Types.ConditionalSpecification
import Amazonka.LexV2Models.Types.DialogCodeHookInvocationSetting
import Amazonka.LexV2Models.Types.DialogState
import Amazonka.LexV2Models.Types.ElicitationCodeHookInvocationSetting
import Amazonka.LexV2Models.Types.PromptSpecification
import Amazonka.LexV2Models.Types.ResponseSpecification
import qualified Amazonka.Prelude as Prelude

-- | Provides a prompt for making sure that the user is ready for the intent
-- to be fulfilled.
--
-- /See:/ 'newIntentConfirmationSetting' smart constructor.
data IntentConfirmationSetting = IntentConfirmationSetting'
  { -- | Specifies whether the intent\'s confirmation is sent to the user. When
    -- this field is false, confirmation and declination responses aren\'t
    -- sent. If the @active@ field isn\'t specified, the default is true.
    active :: Prelude.Maybe Prelude.Bool,
    -- | The next step to take in the conversation if the confirmation step
    -- fails.
    failureNextStep :: Prelude.Maybe DialogState,
    -- | The @DialogCodeHookInvocationSetting@ object associated with intent\'s
    -- confirmation step. The dialog code hook is triggered based on these
    -- invocation settings when the confirmation next step or declination next
    -- step or failure next step is @InvokeDialogCodeHook@.
    codeHook :: Prelude.Maybe DialogCodeHookInvocationSetting,
    -- | The @DialogCodeHookInvocationSetting@ used when the code hook is invoked
    -- during confirmation prompt retries.
    elicitationCodeHook :: Prelude.Maybe ElicitationCodeHookInvocationSetting,
    -- | When the user answers \"no\" to the question defined in
    -- @promptSpecification@, Amazon Lex responds with this response to
    -- acknowledge that the intent was canceled.
    declinationResponse :: Prelude.Maybe ResponseSpecification,
    -- | A list of conditional branches to evaluate after the intent is declined.
    declinationConditional :: Prelude.Maybe ConditionalSpecification,
    failureConditional :: Prelude.Maybe ConditionalSpecification,
    confirmationResponse :: Prelude.Maybe ResponseSpecification,
    -- | Specifies the next step that the bot executes when the customer confirms
    -- the intent.
    confirmationNextStep :: Prelude.Maybe DialogState,
    failureResponse :: Prelude.Maybe ResponseSpecification,
    -- | A list of conditional branches to evaluate after the intent is closed.
    confirmationConditional :: Prelude.Maybe ConditionalSpecification,
    -- | Specifies the next step that the bot executes when the customer declines
    -- the intent.
    declinationNextStep :: Prelude.Maybe DialogState,
    -- | Prompts the user to confirm the intent. This question should have a yes
    -- or no answer.
    --
    -- Amazon Lex uses this prompt to ensure that the user acknowledges that
    -- the intent is ready for fulfillment. For example, with the @OrderPizza@
    -- intent, you might want to confirm that the order is correct before
    -- placing it. For other intents, such as intents that simply respond to
    -- user questions, you might not need to ask the user for confirmation
    -- before providing the information.
    promptSpecification :: PromptSpecification
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IntentConfirmationSetting' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'active', 'intentConfirmationSetting_active' - Specifies whether the intent\'s confirmation is sent to the user. When
-- this field is false, confirmation and declination responses aren\'t
-- sent. If the @active@ field isn\'t specified, the default is true.
--
-- 'failureNextStep', 'intentConfirmationSetting_failureNextStep' - The next step to take in the conversation if the confirmation step
-- fails.
--
-- 'codeHook', 'intentConfirmationSetting_codeHook' - The @DialogCodeHookInvocationSetting@ object associated with intent\'s
-- confirmation step. The dialog code hook is triggered based on these
-- invocation settings when the confirmation next step or declination next
-- step or failure next step is @InvokeDialogCodeHook@.
--
-- 'elicitationCodeHook', 'intentConfirmationSetting_elicitationCodeHook' - The @DialogCodeHookInvocationSetting@ used when the code hook is invoked
-- during confirmation prompt retries.
--
-- 'declinationResponse', 'intentConfirmationSetting_declinationResponse' - When the user answers \"no\" to the question defined in
-- @promptSpecification@, Amazon Lex responds with this response to
-- acknowledge that the intent was canceled.
--
-- 'declinationConditional', 'intentConfirmationSetting_declinationConditional' - A list of conditional branches to evaluate after the intent is declined.
--
-- 'failureConditional', 'intentConfirmationSetting_failureConditional' - Undocumented member.
--
-- 'confirmationResponse', 'intentConfirmationSetting_confirmationResponse' - Undocumented member.
--
-- 'confirmationNextStep', 'intentConfirmationSetting_confirmationNextStep' - Specifies the next step that the bot executes when the customer confirms
-- the intent.
--
-- 'failureResponse', 'intentConfirmationSetting_failureResponse' - Undocumented member.
--
-- 'confirmationConditional', 'intentConfirmationSetting_confirmationConditional' - A list of conditional branches to evaluate after the intent is closed.
--
-- 'declinationNextStep', 'intentConfirmationSetting_declinationNextStep' - Specifies the next step that the bot executes when the customer declines
-- the intent.
--
-- 'promptSpecification', 'intentConfirmationSetting_promptSpecification' - Prompts the user to confirm the intent. This question should have a yes
-- or no answer.
--
-- Amazon Lex uses this prompt to ensure that the user acknowledges that
-- the intent is ready for fulfillment. For example, with the @OrderPizza@
-- intent, you might want to confirm that the order is correct before
-- placing it. For other intents, such as intents that simply respond to
-- user questions, you might not need to ask the user for confirmation
-- before providing the information.
newIntentConfirmationSetting ::
  -- | 'promptSpecification'
  PromptSpecification ->
  IntentConfirmationSetting
newIntentConfirmationSetting pPromptSpecification_ =
  IntentConfirmationSetting'
    { active =
        Prelude.Nothing,
      failureNextStep = Prelude.Nothing,
      codeHook = Prelude.Nothing,
      elicitationCodeHook = Prelude.Nothing,
      declinationResponse = Prelude.Nothing,
      declinationConditional = Prelude.Nothing,
      failureConditional = Prelude.Nothing,
      confirmationResponse = Prelude.Nothing,
      confirmationNextStep = Prelude.Nothing,
      failureResponse = Prelude.Nothing,
      confirmationConditional = Prelude.Nothing,
      declinationNextStep = Prelude.Nothing,
      promptSpecification = pPromptSpecification_
    }

-- | Specifies whether the intent\'s confirmation is sent to the user. When
-- this field is false, confirmation and declination responses aren\'t
-- sent. If the @active@ field isn\'t specified, the default is true.
intentConfirmationSetting_active :: Lens.Lens' IntentConfirmationSetting (Prelude.Maybe Prelude.Bool)
intentConfirmationSetting_active = Lens.lens (\IntentConfirmationSetting' {active} -> active) (\s@IntentConfirmationSetting' {} a -> s {active = a} :: IntentConfirmationSetting)

-- | The next step to take in the conversation if the confirmation step
-- fails.
intentConfirmationSetting_failureNextStep :: Lens.Lens' IntentConfirmationSetting (Prelude.Maybe DialogState)
intentConfirmationSetting_failureNextStep = Lens.lens (\IntentConfirmationSetting' {failureNextStep} -> failureNextStep) (\s@IntentConfirmationSetting' {} a -> s {failureNextStep = a} :: IntentConfirmationSetting)

-- | The @DialogCodeHookInvocationSetting@ object associated with intent\'s
-- confirmation step. The dialog code hook is triggered based on these
-- invocation settings when the confirmation next step or declination next
-- step or failure next step is @InvokeDialogCodeHook@.
intentConfirmationSetting_codeHook :: Lens.Lens' IntentConfirmationSetting (Prelude.Maybe DialogCodeHookInvocationSetting)
intentConfirmationSetting_codeHook = Lens.lens (\IntentConfirmationSetting' {codeHook} -> codeHook) (\s@IntentConfirmationSetting' {} a -> s {codeHook = a} :: IntentConfirmationSetting)

-- | The @DialogCodeHookInvocationSetting@ used when the code hook is invoked
-- during confirmation prompt retries.
intentConfirmationSetting_elicitationCodeHook :: Lens.Lens' IntentConfirmationSetting (Prelude.Maybe ElicitationCodeHookInvocationSetting)
intentConfirmationSetting_elicitationCodeHook = Lens.lens (\IntentConfirmationSetting' {elicitationCodeHook} -> elicitationCodeHook) (\s@IntentConfirmationSetting' {} a -> s {elicitationCodeHook = a} :: IntentConfirmationSetting)

-- | When the user answers \"no\" to the question defined in
-- @promptSpecification@, Amazon Lex responds with this response to
-- acknowledge that the intent was canceled.
intentConfirmationSetting_declinationResponse :: Lens.Lens' IntentConfirmationSetting (Prelude.Maybe ResponseSpecification)
intentConfirmationSetting_declinationResponse = Lens.lens (\IntentConfirmationSetting' {declinationResponse} -> declinationResponse) (\s@IntentConfirmationSetting' {} a -> s {declinationResponse = a} :: IntentConfirmationSetting)

-- | A list of conditional branches to evaluate after the intent is declined.
intentConfirmationSetting_declinationConditional :: Lens.Lens' IntentConfirmationSetting (Prelude.Maybe ConditionalSpecification)
intentConfirmationSetting_declinationConditional = Lens.lens (\IntentConfirmationSetting' {declinationConditional} -> declinationConditional) (\s@IntentConfirmationSetting' {} a -> s {declinationConditional = a} :: IntentConfirmationSetting)

-- | Undocumented member.
intentConfirmationSetting_failureConditional :: Lens.Lens' IntentConfirmationSetting (Prelude.Maybe ConditionalSpecification)
intentConfirmationSetting_failureConditional = Lens.lens (\IntentConfirmationSetting' {failureConditional} -> failureConditional) (\s@IntentConfirmationSetting' {} a -> s {failureConditional = a} :: IntentConfirmationSetting)

-- | Undocumented member.
intentConfirmationSetting_confirmationResponse :: Lens.Lens' IntentConfirmationSetting (Prelude.Maybe ResponseSpecification)
intentConfirmationSetting_confirmationResponse = Lens.lens (\IntentConfirmationSetting' {confirmationResponse} -> confirmationResponse) (\s@IntentConfirmationSetting' {} a -> s {confirmationResponse = a} :: IntentConfirmationSetting)

-- | Specifies the next step that the bot executes when the customer confirms
-- the intent.
intentConfirmationSetting_confirmationNextStep :: Lens.Lens' IntentConfirmationSetting (Prelude.Maybe DialogState)
intentConfirmationSetting_confirmationNextStep = Lens.lens (\IntentConfirmationSetting' {confirmationNextStep} -> confirmationNextStep) (\s@IntentConfirmationSetting' {} a -> s {confirmationNextStep = a} :: IntentConfirmationSetting)

-- | Undocumented member.
intentConfirmationSetting_failureResponse :: Lens.Lens' IntentConfirmationSetting (Prelude.Maybe ResponseSpecification)
intentConfirmationSetting_failureResponse = Lens.lens (\IntentConfirmationSetting' {failureResponse} -> failureResponse) (\s@IntentConfirmationSetting' {} a -> s {failureResponse = a} :: IntentConfirmationSetting)

-- | A list of conditional branches to evaluate after the intent is closed.
intentConfirmationSetting_confirmationConditional :: Lens.Lens' IntentConfirmationSetting (Prelude.Maybe ConditionalSpecification)
intentConfirmationSetting_confirmationConditional = Lens.lens (\IntentConfirmationSetting' {confirmationConditional} -> confirmationConditional) (\s@IntentConfirmationSetting' {} a -> s {confirmationConditional = a} :: IntentConfirmationSetting)

-- | Specifies the next step that the bot executes when the customer declines
-- the intent.
intentConfirmationSetting_declinationNextStep :: Lens.Lens' IntentConfirmationSetting (Prelude.Maybe DialogState)
intentConfirmationSetting_declinationNextStep = Lens.lens (\IntentConfirmationSetting' {declinationNextStep} -> declinationNextStep) (\s@IntentConfirmationSetting' {} a -> s {declinationNextStep = a} :: IntentConfirmationSetting)

-- | Prompts the user to confirm the intent. This question should have a yes
-- or no answer.
--
-- Amazon Lex uses this prompt to ensure that the user acknowledges that
-- the intent is ready for fulfillment. For example, with the @OrderPizza@
-- intent, you might want to confirm that the order is correct before
-- placing it. For other intents, such as intents that simply respond to
-- user questions, you might not need to ask the user for confirmation
-- before providing the information.
intentConfirmationSetting_promptSpecification :: Lens.Lens' IntentConfirmationSetting PromptSpecification
intentConfirmationSetting_promptSpecification = Lens.lens (\IntentConfirmationSetting' {promptSpecification} -> promptSpecification) (\s@IntentConfirmationSetting' {} a -> s {promptSpecification = a} :: IntentConfirmationSetting)

instance Core.FromJSON IntentConfirmationSetting where
  parseJSON =
    Core.withObject
      "IntentConfirmationSetting"
      ( \x ->
          IntentConfirmationSetting'
            Prelude.<$> (x Core..:? "active")
            Prelude.<*> (x Core..:? "failureNextStep")
            Prelude.<*> (x Core..:? "codeHook")
            Prelude.<*> (x Core..:? "elicitationCodeHook")
            Prelude.<*> (x Core..:? "declinationResponse")
            Prelude.<*> (x Core..:? "declinationConditional")
            Prelude.<*> (x Core..:? "failureConditional")
            Prelude.<*> (x Core..:? "confirmationResponse")
            Prelude.<*> (x Core..:? "confirmationNextStep")
            Prelude.<*> (x Core..:? "failureResponse")
            Prelude.<*> (x Core..:? "confirmationConditional")
            Prelude.<*> (x Core..:? "declinationNextStep")
            Prelude.<*> (x Core..: "promptSpecification")
      )

instance Prelude.Hashable IntentConfirmationSetting where
  hashWithSalt _salt IntentConfirmationSetting' {..} =
    _salt `Prelude.hashWithSalt` active
      `Prelude.hashWithSalt` failureNextStep
      `Prelude.hashWithSalt` codeHook
      `Prelude.hashWithSalt` elicitationCodeHook
      `Prelude.hashWithSalt` declinationResponse
      `Prelude.hashWithSalt` declinationConditional
      `Prelude.hashWithSalt` failureConditional
      `Prelude.hashWithSalt` confirmationResponse
      `Prelude.hashWithSalt` confirmationNextStep
      `Prelude.hashWithSalt` failureResponse
      `Prelude.hashWithSalt` confirmationConditional
      `Prelude.hashWithSalt` declinationNextStep
      `Prelude.hashWithSalt` promptSpecification

instance Prelude.NFData IntentConfirmationSetting where
  rnf IntentConfirmationSetting' {..} =
    Prelude.rnf active
      `Prelude.seq` Prelude.rnf failureNextStep
      `Prelude.seq` Prelude.rnf codeHook
      `Prelude.seq` Prelude.rnf elicitationCodeHook
      `Prelude.seq` Prelude.rnf declinationResponse
      `Prelude.seq` Prelude.rnf declinationConditional
      `Prelude.seq` Prelude.rnf failureConditional
      `Prelude.seq` Prelude.rnf confirmationResponse
      `Prelude.seq` Prelude.rnf confirmationNextStep
      `Prelude.seq` Prelude.rnf failureResponse
      `Prelude.seq` Prelude.rnf confirmationConditional
      `Prelude.seq` Prelude.rnf declinationNextStep
      `Prelude.seq` Prelude.rnf promptSpecification

instance Core.ToJSON IntentConfirmationSetting where
  toJSON IntentConfirmationSetting' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("active" Core..=) Prelude.<$> active,
            ("failureNextStep" Core..=)
              Prelude.<$> failureNextStep,
            ("codeHook" Core..=) Prelude.<$> codeHook,
            ("elicitationCodeHook" Core..=)
              Prelude.<$> elicitationCodeHook,
            ("declinationResponse" Core..=)
              Prelude.<$> declinationResponse,
            ("declinationConditional" Core..=)
              Prelude.<$> declinationConditional,
            ("failureConditional" Core..=)
              Prelude.<$> failureConditional,
            ("confirmationResponse" Core..=)
              Prelude.<$> confirmationResponse,
            ("confirmationNextStep" Core..=)
              Prelude.<$> confirmationNextStep,
            ("failureResponse" Core..=)
              Prelude.<$> failureResponse,
            ("confirmationConditional" Core..=)
              Prelude.<$> confirmationConditional,
            ("declinationNextStep" Core..=)
              Prelude.<$> declinationNextStep,
            Prelude.Just
              ("promptSpecification" Core..= promptSpecification)
          ]
      )
