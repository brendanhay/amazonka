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
-- Module      : Network.AWS.LexV2Models.Types.IntentConfirmationSetting
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexV2Models.Types.IntentConfirmationSetting where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexV2Models.Types.PromptSpecification
import Network.AWS.LexV2Models.Types.ResponseSpecification
import qualified Network.AWS.Prelude as Prelude

-- | Provides a prompt for making sure that the user is ready for the intent
-- to be fulfilled.
--
-- /See:/ 'newIntentConfirmationSetting' smart constructor.
data IntentConfirmationSetting = IntentConfirmationSetting'
  { -- | Specifies whether the intent\'s confirmation is sent to the user. When
    -- this field is false, confirmation and declination responses aren\'t
    -- sent. If the @active@ field isn\'t specified, the default is true.
    active :: Prelude.Maybe Prelude.Bool,
    -- | Prompts the user to confirm the intent. This question should have a yes
    -- or no answer.
    --
    -- Amazon Lex uses this prompt to ensure that the user acknowledges that
    -- the intent is ready for fulfillment. For example, with the @OrderPizza@
    -- intent, you might want to confirm that the order is correct before
    -- placing it. For other intents, such as intents that simply respond to
    -- user questions, you might not need to ask the user for confirmation
    -- before providing the information.
    promptSpecification :: PromptSpecification,
    -- | When the user answers \"no\" to the question defined in
    -- @promptSpecification@, Amazon Lex responds with this response to
    -- acknowledge that the intent was canceled.
    declinationResponse :: ResponseSpecification
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
-- 'promptSpecification', 'intentConfirmationSetting_promptSpecification' - Prompts the user to confirm the intent. This question should have a yes
-- or no answer.
--
-- Amazon Lex uses this prompt to ensure that the user acknowledges that
-- the intent is ready for fulfillment. For example, with the @OrderPizza@
-- intent, you might want to confirm that the order is correct before
-- placing it. For other intents, such as intents that simply respond to
-- user questions, you might not need to ask the user for confirmation
-- before providing the information.
--
-- 'declinationResponse', 'intentConfirmationSetting_declinationResponse' - When the user answers \"no\" to the question defined in
-- @promptSpecification@, Amazon Lex responds with this response to
-- acknowledge that the intent was canceled.
newIntentConfirmationSetting ::
  -- | 'promptSpecification'
  PromptSpecification ->
  -- | 'declinationResponse'
  ResponseSpecification ->
  IntentConfirmationSetting
newIntentConfirmationSetting
  pPromptSpecification_
  pDeclinationResponse_ =
    IntentConfirmationSetting'
      { active =
          Prelude.Nothing,
        promptSpecification = pPromptSpecification_,
        declinationResponse = pDeclinationResponse_
      }

-- | Specifies whether the intent\'s confirmation is sent to the user. When
-- this field is false, confirmation and declination responses aren\'t
-- sent. If the @active@ field isn\'t specified, the default is true.
intentConfirmationSetting_active :: Lens.Lens' IntentConfirmationSetting (Prelude.Maybe Prelude.Bool)
intentConfirmationSetting_active = Lens.lens (\IntentConfirmationSetting' {active} -> active) (\s@IntentConfirmationSetting' {} a -> s {active = a} :: IntentConfirmationSetting)

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

-- | When the user answers \"no\" to the question defined in
-- @promptSpecification@, Amazon Lex responds with this response to
-- acknowledge that the intent was canceled.
intentConfirmationSetting_declinationResponse :: Lens.Lens' IntentConfirmationSetting ResponseSpecification
intentConfirmationSetting_declinationResponse = Lens.lens (\IntentConfirmationSetting' {declinationResponse} -> declinationResponse) (\s@IntentConfirmationSetting' {} a -> s {declinationResponse = a} :: IntentConfirmationSetting)

instance Core.FromJSON IntentConfirmationSetting where
  parseJSON =
    Core.withObject
      "IntentConfirmationSetting"
      ( \x ->
          IntentConfirmationSetting'
            Prelude.<$> (x Core..:? "active")
            Prelude.<*> (x Core..: "promptSpecification")
            Prelude.<*> (x Core..: "declinationResponse")
      )

instance Prelude.Hashable IntentConfirmationSetting

instance Prelude.NFData IntentConfirmationSetting

instance Core.ToJSON IntentConfirmationSetting where
  toJSON IntentConfirmationSetting' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("active" Core..=) Prelude.<$> active,
            Prelude.Just
              ("promptSpecification" Core..= promptSpecification),
            Prelude.Just
              ("declinationResponse" Core..= declinationResponse)
          ]
      )
