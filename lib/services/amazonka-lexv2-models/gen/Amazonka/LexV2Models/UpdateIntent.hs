{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.LexV2Models.UpdateIntent
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the settings for an intent.
module Amazonka.LexV2Models.UpdateIntent
  ( -- * Creating a Request
    UpdateIntent (..),
    newUpdateIntent,

    -- * Request Lenses
    updateIntent_intentClosingSetting,
    updateIntent_sampleUtterances,
    updateIntent_kendraConfiguration,
    updateIntent_dialogCodeHook,
    updateIntent_outputContexts,
    updateIntent_intentConfirmationSetting,
    updateIntent_parentIntentSignature,
    updateIntent_description,
    updateIntent_slotPriorities,
    updateIntent_fulfillmentCodeHook,
    updateIntent_initialResponseSetting,
    updateIntent_inputContexts,
    updateIntent_intentId,
    updateIntent_intentName,
    updateIntent_botId,
    updateIntent_botVersion,
    updateIntent_localeId,

    -- * Destructuring the Response
    UpdateIntentResponse (..),
    newUpdateIntentResponse,

    -- * Response Lenses
    updateIntentResponse_intentClosingSetting,
    updateIntentResponse_sampleUtterances,
    updateIntentResponse_kendraConfiguration,
    updateIntentResponse_botVersion,
    updateIntentResponse_creationDateTime,
    updateIntentResponse_dialogCodeHook,
    updateIntentResponse_localeId,
    updateIntentResponse_outputContexts,
    updateIntentResponse_intentConfirmationSetting,
    updateIntentResponse_parentIntentSignature,
    updateIntentResponse_description,
    updateIntentResponse_botId,
    updateIntentResponse_slotPriorities,
    updateIntentResponse_intentId,
    updateIntentResponse_intentName,
    updateIntentResponse_fulfillmentCodeHook,
    updateIntentResponse_initialResponseSetting,
    updateIntentResponse_inputContexts,
    updateIntentResponse_lastUpdatedDateTime,
    updateIntentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LexV2Models.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateIntent' smart constructor.
data UpdateIntent = UpdateIntent'
  { -- | The new response that Amazon Lex sends the user when the intent is
    -- closed.
    intentClosingSetting :: Prelude.Maybe IntentClosingSetting,
    -- | New utterances used to invoke the intent.
    sampleUtterances :: Prelude.Maybe [SampleUtterance],
    -- | New configuration settings for connecting to an Amazon Kendra index.
    kendraConfiguration :: Prelude.Maybe KendraConfiguration,
    -- | The new Lambda function to use between each turn of the conversation
    -- with the bot.
    dialogCodeHook :: Prelude.Maybe DialogCodeHookSettings,
    -- | A new list of contexts that Amazon Lex activates when the intent is
    -- fulfilled.
    outputContexts :: Prelude.Maybe [OutputContext],
    -- | New prompts that Amazon Lex sends to the user to confirm the completion
    -- of an intent.
    intentConfirmationSetting :: Prelude.Maybe IntentConfirmationSetting,
    -- | The signature of the new built-in intent to use as the parent of this
    -- intent.
    parentIntentSignature :: Prelude.Maybe Prelude.Text,
    -- | The new description of the intent.
    description :: Prelude.Maybe Prelude.Text,
    -- | A new list of slots and their priorities that are contained by the
    -- intent.
    slotPriorities :: Prelude.Maybe [SlotPriority],
    -- | The new Lambda function to call when all of the intents required slots
    -- are provided and the intent is ready for fulfillment.
    fulfillmentCodeHook :: Prelude.Maybe FulfillmentCodeHookSettings,
    initialResponseSetting :: Prelude.Maybe InitialResponseSetting,
    -- | A new list of contexts that must be active in order for Amazon Lex to
    -- consider the intent.
    inputContexts :: Prelude.Maybe [InputContext],
    -- | The unique identifier of the intent to update.
    intentId :: Prelude.Text,
    -- | The new name for the intent.
    intentName :: Prelude.Text,
    -- | The identifier of the bot that contains the intent.
    botId :: Prelude.Text,
    -- | The version of the bot that contains the intent. Must be @DRAFT@.
    botVersion :: Prelude.Text,
    -- | The identifier of the language and locale where this intent is used. The
    -- string must match one of the supported locales. For more information,
    -- see
    -- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>.
    localeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateIntent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'intentClosingSetting', 'updateIntent_intentClosingSetting' - The new response that Amazon Lex sends the user when the intent is
-- closed.
--
-- 'sampleUtterances', 'updateIntent_sampleUtterances' - New utterances used to invoke the intent.
--
-- 'kendraConfiguration', 'updateIntent_kendraConfiguration' - New configuration settings for connecting to an Amazon Kendra index.
--
-- 'dialogCodeHook', 'updateIntent_dialogCodeHook' - The new Lambda function to use between each turn of the conversation
-- with the bot.
--
-- 'outputContexts', 'updateIntent_outputContexts' - A new list of contexts that Amazon Lex activates when the intent is
-- fulfilled.
--
-- 'intentConfirmationSetting', 'updateIntent_intentConfirmationSetting' - New prompts that Amazon Lex sends to the user to confirm the completion
-- of an intent.
--
-- 'parentIntentSignature', 'updateIntent_parentIntentSignature' - The signature of the new built-in intent to use as the parent of this
-- intent.
--
-- 'description', 'updateIntent_description' - The new description of the intent.
--
-- 'slotPriorities', 'updateIntent_slotPriorities' - A new list of slots and their priorities that are contained by the
-- intent.
--
-- 'fulfillmentCodeHook', 'updateIntent_fulfillmentCodeHook' - The new Lambda function to call when all of the intents required slots
-- are provided and the intent is ready for fulfillment.
--
-- 'initialResponseSetting', 'updateIntent_initialResponseSetting' -
--
-- 'inputContexts', 'updateIntent_inputContexts' - A new list of contexts that must be active in order for Amazon Lex to
-- consider the intent.
--
-- 'intentId', 'updateIntent_intentId' - The unique identifier of the intent to update.
--
-- 'intentName', 'updateIntent_intentName' - The new name for the intent.
--
-- 'botId', 'updateIntent_botId' - The identifier of the bot that contains the intent.
--
-- 'botVersion', 'updateIntent_botVersion' - The version of the bot that contains the intent. Must be @DRAFT@.
--
-- 'localeId', 'updateIntent_localeId' - The identifier of the language and locale where this intent is used. The
-- string must match one of the supported locales. For more information,
-- see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>.
newUpdateIntent ::
  -- | 'intentId'
  Prelude.Text ->
  -- | 'intentName'
  Prelude.Text ->
  -- | 'botId'
  Prelude.Text ->
  -- | 'botVersion'
  Prelude.Text ->
  -- | 'localeId'
  Prelude.Text ->
  UpdateIntent
newUpdateIntent
  pIntentId_
  pIntentName_
  pBotId_
  pBotVersion_
  pLocaleId_ =
    UpdateIntent'
      { intentClosingSetting =
          Prelude.Nothing,
        sampleUtterances = Prelude.Nothing,
        kendraConfiguration = Prelude.Nothing,
        dialogCodeHook = Prelude.Nothing,
        outputContexts = Prelude.Nothing,
        intentConfirmationSetting = Prelude.Nothing,
        parentIntentSignature = Prelude.Nothing,
        description = Prelude.Nothing,
        slotPriorities = Prelude.Nothing,
        fulfillmentCodeHook = Prelude.Nothing,
        initialResponseSetting = Prelude.Nothing,
        inputContexts = Prelude.Nothing,
        intentId = pIntentId_,
        intentName = pIntentName_,
        botId = pBotId_,
        botVersion = pBotVersion_,
        localeId = pLocaleId_
      }

-- | The new response that Amazon Lex sends the user when the intent is
-- closed.
updateIntent_intentClosingSetting :: Lens.Lens' UpdateIntent (Prelude.Maybe IntentClosingSetting)
updateIntent_intentClosingSetting = Lens.lens (\UpdateIntent' {intentClosingSetting} -> intentClosingSetting) (\s@UpdateIntent' {} a -> s {intentClosingSetting = a} :: UpdateIntent)

-- | New utterances used to invoke the intent.
updateIntent_sampleUtterances :: Lens.Lens' UpdateIntent (Prelude.Maybe [SampleUtterance])
updateIntent_sampleUtterances = Lens.lens (\UpdateIntent' {sampleUtterances} -> sampleUtterances) (\s@UpdateIntent' {} a -> s {sampleUtterances = a} :: UpdateIntent) Prelude.. Lens.mapping Lens.coerced

-- | New configuration settings for connecting to an Amazon Kendra index.
updateIntent_kendraConfiguration :: Lens.Lens' UpdateIntent (Prelude.Maybe KendraConfiguration)
updateIntent_kendraConfiguration = Lens.lens (\UpdateIntent' {kendraConfiguration} -> kendraConfiguration) (\s@UpdateIntent' {} a -> s {kendraConfiguration = a} :: UpdateIntent)

-- | The new Lambda function to use between each turn of the conversation
-- with the bot.
updateIntent_dialogCodeHook :: Lens.Lens' UpdateIntent (Prelude.Maybe DialogCodeHookSettings)
updateIntent_dialogCodeHook = Lens.lens (\UpdateIntent' {dialogCodeHook} -> dialogCodeHook) (\s@UpdateIntent' {} a -> s {dialogCodeHook = a} :: UpdateIntent)

-- | A new list of contexts that Amazon Lex activates when the intent is
-- fulfilled.
updateIntent_outputContexts :: Lens.Lens' UpdateIntent (Prelude.Maybe [OutputContext])
updateIntent_outputContexts = Lens.lens (\UpdateIntent' {outputContexts} -> outputContexts) (\s@UpdateIntent' {} a -> s {outputContexts = a} :: UpdateIntent) Prelude.. Lens.mapping Lens.coerced

-- | New prompts that Amazon Lex sends to the user to confirm the completion
-- of an intent.
updateIntent_intentConfirmationSetting :: Lens.Lens' UpdateIntent (Prelude.Maybe IntentConfirmationSetting)
updateIntent_intentConfirmationSetting = Lens.lens (\UpdateIntent' {intentConfirmationSetting} -> intentConfirmationSetting) (\s@UpdateIntent' {} a -> s {intentConfirmationSetting = a} :: UpdateIntent)

-- | The signature of the new built-in intent to use as the parent of this
-- intent.
updateIntent_parentIntentSignature :: Lens.Lens' UpdateIntent (Prelude.Maybe Prelude.Text)
updateIntent_parentIntentSignature = Lens.lens (\UpdateIntent' {parentIntentSignature} -> parentIntentSignature) (\s@UpdateIntent' {} a -> s {parentIntentSignature = a} :: UpdateIntent)

-- | The new description of the intent.
updateIntent_description :: Lens.Lens' UpdateIntent (Prelude.Maybe Prelude.Text)
updateIntent_description = Lens.lens (\UpdateIntent' {description} -> description) (\s@UpdateIntent' {} a -> s {description = a} :: UpdateIntent)

-- | A new list of slots and their priorities that are contained by the
-- intent.
updateIntent_slotPriorities :: Lens.Lens' UpdateIntent (Prelude.Maybe [SlotPriority])
updateIntent_slotPriorities = Lens.lens (\UpdateIntent' {slotPriorities} -> slotPriorities) (\s@UpdateIntent' {} a -> s {slotPriorities = a} :: UpdateIntent) Prelude.. Lens.mapping Lens.coerced

-- | The new Lambda function to call when all of the intents required slots
-- are provided and the intent is ready for fulfillment.
updateIntent_fulfillmentCodeHook :: Lens.Lens' UpdateIntent (Prelude.Maybe FulfillmentCodeHookSettings)
updateIntent_fulfillmentCodeHook = Lens.lens (\UpdateIntent' {fulfillmentCodeHook} -> fulfillmentCodeHook) (\s@UpdateIntent' {} a -> s {fulfillmentCodeHook = a} :: UpdateIntent)

-- |
updateIntent_initialResponseSetting :: Lens.Lens' UpdateIntent (Prelude.Maybe InitialResponseSetting)
updateIntent_initialResponseSetting = Lens.lens (\UpdateIntent' {initialResponseSetting} -> initialResponseSetting) (\s@UpdateIntent' {} a -> s {initialResponseSetting = a} :: UpdateIntent)

-- | A new list of contexts that must be active in order for Amazon Lex to
-- consider the intent.
updateIntent_inputContexts :: Lens.Lens' UpdateIntent (Prelude.Maybe [InputContext])
updateIntent_inputContexts = Lens.lens (\UpdateIntent' {inputContexts} -> inputContexts) (\s@UpdateIntent' {} a -> s {inputContexts = a} :: UpdateIntent) Prelude.. Lens.mapping Lens.coerced

-- | The unique identifier of the intent to update.
updateIntent_intentId :: Lens.Lens' UpdateIntent Prelude.Text
updateIntent_intentId = Lens.lens (\UpdateIntent' {intentId} -> intentId) (\s@UpdateIntent' {} a -> s {intentId = a} :: UpdateIntent)

-- | The new name for the intent.
updateIntent_intentName :: Lens.Lens' UpdateIntent Prelude.Text
updateIntent_intentName = Lens.lens (\UpdateIntent' {intentName} -> intentName) (\s@UpdateIntent' {} a -> s {intentName = a} :: UpdateIntent)

-- | The identifier of the bot that contains the intent.
updateIntent_botId :: Lens.Lens' UpdateIntent Prelude.Text
updateIntent_botId = Lens.lens (\UpdateIntent' {botId} -> botId) (\s@UpdateIntent' {} a -> s {botId = a} :: UpdateIntent)

-- | The version of the bot that contains the intent. Must be @DRAFT@.
updateIntent_botVersion :: Lens.Lens' UpdateIntent Prelude.Text
updateIntent_botVersion = Lens.lens (\UpdateIntent' {botVersion} -> botVersion) (\s@UpdateIntent' {} a -> s {botVersion = a} :: UpdateIntent)

-- | The identifier of the language and locale where this intent is used. The
-- string must match one of the supported locales. For more information,
-- see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>.
updateIntent_localeId :: Lens.Lens' UpdateIntent Prelude.Text
updateIntent_localeId = Lens.lens (\UpdateIntent' {localeId} -> localeId) (\s@UpdateIntent' {} a -> s {localeId = a} :: UpdateIntent)

instance Core.AWSRequest UpdateIntent where
  type AWSResponse UpdateIntent = UpdateIntentResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateIntentResponse'
            Prelude.<$> (x Core..?> "intentClosingSetting")
            Prelude.<*> ( x Core..?> "sampleUtterances"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "kendraConfiguration")
            Prelude.<*> (x Core..?> "botVersion")
            Prelude.<*> (x Core..?> "creationDateTime")
            Prelude.<*> (x Core..?> "dialogCodeHook")
            Prelude.<*> (x Core..?> "localeId")
            Prelude.<*> (x Core..?> "outputContexts" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "intentConfirmationSetting")
            Prelude.<*> (x Core..?> "parentIntentSignature")
            Prelude.<*> (x Core..?> "description")
            Prelude.<*> (x Core..?> "botId")
            Prelude.<*> (x Core..?> "slotPriorities" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "intentId")
            Prelude.<*> (x Core..?> "intentName")
            Prelude.<*> (x Core..?> "fulfillmentCodeHook")
            Prelude.<*> (x Core..?> "initialResponseSetting")
            Prelude.<*> (x Core..?> "inputContexts" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "lastUpdatedDateTime")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateIntent where
  hashWithSalt _salt UpdateIntent' {..} =
    _salt `Prelude.hashWithSalt` intentClosingSetting
      `Prelude.hashWithSalt` sampleUtterances
      `Prelude.hashWithSalt` kendraConfiguration
      `Prelude.hashWithSalt` dialogCodeHook
      `Prelude.hashWithSalt` outputContexts
      `Prelude.hashWithSalt` intentConfirmationSetting
      `Prelude.hashWithSalt` parentIntentSignature
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` slotPriorities
      `Prelude.hashWithSalt` fulfillmentCodeHook
      `Prelude.hashWithSalt` initialResponseSetting
      `Prelude.hashWithSalt` inputContexts
      `Prelude.hashWithSalt` intentId
      `Prelude.hashWithSalt` intentName
      `Prelude.hashWithSalt` botId
      `Prelude.hashWithSalt` botVersion
      `Prelude.hashWithSalt` localeId

instance Prelude.NFData UpdateIntent where
  rnf UpdateIntent' {..} =
    Prelude.rnf intentClosingSetting
      `Prelude.seq` Prelude.rnf sampleUtterances
      `Prelude.seq` Prelude.rnf kendraConfiguration
      `Prelude.seq` Prelude.rnf dialogCodeHook
      `Prelude.seq` Prelude.rnf outputContexts
      `Prelude.seq` Prelude.rnf intentConfirmationSetting
      `Prelude.seq` Prelude.rnf parentIntentSignature
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf slotPriorities
      `Prelude.seq` Prelude.rnf fulfillmentCodeHook
      `Prelude.seq` Prelude.rnf initialResponseSetting
      `Prelude.seq` Prelude.rnf inputContexts
      `Prelude.seq` Prelude.rnf intentId
      `Prelude.seq` Prelude.rnf intentName
      `Prelude.seq` Prelude.rnf botId
      `Prelude.seq` Prelude.rnf botVersion
      `Prelude.seq` Prelude.rnf localeId

instance Core.ToHeaders UpdateIntent where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateIntent where
  toJSON UpdateIntent' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("intentClosingSetting" Core..=)
              Prelude.<$> intentClosingSetting,
            ("sampleUtterances" Core..=)
              Prelude.<$> sampleUtterances,
            ("kendraConfiguration" Core..=)
              Prelude.<$> kendraConfiguration,
            ("dialogCodeHook" Core..=)
              Prelude.<$> dialogCodeHook,
            ("outputContexts" Core..=)
              Prelude.<$> outputContexts,
            ("intentConfirmationSetting" Core..=)
              Prelude.<$> intentConfirmationSetting,
            ("parentIntentSignature" Core..=)
              Prelude.<$> parentIntentSignature,
            ("description" Core..=) Prelude.<$> description,
            ("slotPriorities" Core..=)
              Prelude.<$> slotPriorities,
            ("fulfillmentCodeHook" Core..=)
              Prelude.<$> fulfillmentCodeHook,
            ("initialResponseSetting" Core..=)
              Prelude.<$> initialResponseSetting,
            ("inputContexts" Core..=) Prelude.<$> inputContexts,
            Prelude.Just ("intentName" Core..= intentName)
          ]
      )

instance Core.ToPath UpdateIntent where
  toPath UpdateIntent' {..} =
    Prelude.mconcat
      [ "/bots/",
        Core.toBS botId,
        "/botversions/",
        Core.toBS botVersion,
        "/botlocales/",
        Core.toBS localeId,
        "/intents/",
        Core.toBS intentId,
        "/"
      ]

instance Core.ToQuery UpdateIntent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateIntentResponse' smart constructor.
data UpdateIntentResponse = UpdateIntentResponse'
  { -- | The updated response that Amazon Lex sends the user when the intent is
    -- closed.
    intentClosingSetting :: Prelude.Maybe IntentClosingSetting,
    -- | The updated list of sample utterances for the intent.
    sampleUtterances :: Prelude.Maybe [SampleUtterance],
    -- | The updated configuration for connecting to an Amazon Kendra index with
    -- the @AMAZON.KendraSearchIntent@ intent.
    kendraConfiguration :: Prelude.Maybe KendraConfiguration,
    -- | The version of the bot that contains the intent. Will always be @DRAFT@.
    botVersion :: Prelude.Maybe Prelude.Text,
    -- | A timestamp of when the intent was created.
    creationDateTime :: Prelude.Maybe Core.POSIX,
    -- | The updated Lambda function called during each turn of the conversation
    -- with the user.
    dialogCodeHook :: Prelude.Maybe DialogCodeHookSettings,
    -- | The updated language and locale of the intent.
    localeId :: Prelude.Maybe Prelude.Text,
    -- | The updated list of contexts that Amazon Lex activates when the intent
    -- is fulfilled.
    outputContexts :: Prelude.Maybe [OutputContext],
    -- | The updated prompts that Amazon Lex sends to the user to confirm the
    -- completion of an intent.
    intentConfirmationSetting :: Prelude.Maybe IntentConfirmationSetting,
    -- | The updated built-in intent that is the parent of this intent.
    parentIntentSignature :: Prelude.Maybe Prelude.Text,
    -- | The updated description of the intent.
    description :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the bot that contains the intent.
    botId :: Prelude.Maybe Prelude.Text,
    -- | The updated list of slots and their priorities that are elicited from
    -- the user for the intent.
    slotPriorities :: Prelude.Maybe [SlotPriority],
    -- | The identifier of the intent that was updated.
    intentId :: Prelude.Maybe Prelude.Text,
    -- | The updated name of the intent.
    intentName :: Prelude.Maybe Prelude.Text,
    -- | The updated Lambda function called when the intent is ready for
    -- fulfillment.
    fulfillmentCodeHook :: Prelude.Maybe FulfillmentCodeHookSettings,
    initialResponseSetting :: Prelude.Maybe InitialResponseSetting,
    -- | The updated list of contexts that must be active for the intent to be
    -- considered by Amazon Lex.
    inputContexts :: Prelude.Maybe [InputContext],
    -- | A timestamp of the last time that the intent was modified.
    lastUpdatedDateTime :: Prelude.Maybe Core.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateIntentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'intentClosingSetting', 'updateIntentResponse_intentClosingSetting' - The updated response that Amazon Lex sends the user when the intent is
-- closed.
--
-- 'sampleUtterances', 'updateIntentResponse_sampleUtterances' - The updated list of sample utterances for the intent.
--
-- 'kendraConfiguration', 'updateIntentResponse_kendraConfiguration' - The updated configuration for connecting to an Amazon Kendra index with
-- the @AMAZON.KendraSearchIntent@ intent.
--
-- 'botVersion', 'updateIntentResponse_botVersion' - The version of the bot that contains the intent. Will always be @DRAFT@.
--
-- 'creationDateTime', 'updateIntentResponse_creationDateTime' - A timestamp of when the intent was created.
--
-- 'dialogCodeHook', 'updateIntentResponse_dialogCodeHook' - The updated Lambda function called during each turn of the conversation
-- with the user.
--
-- 'localeId', 'updateIntentResponse_localeId' - The updated language and locale of the intent.
--
-- 'outputContexts', 'updateIntentResponse_outputContexts' - The updated list of contexts that Amazon Lex activates when the intent
-- is fulfilled.
--
-- 'intentConfirmationSetting', 'updateIntentResponse_intentConfirmationSetting' - The updated prompts that Amazon Lex sends to the user to confirm the
-- completion of an intent.
--
-- 'parentIntentSignature', 'updateIntentResponse_parentIntentSignature' - The updated built-in intent that is the parent of this intent.
--
-- 'description', 'updateIntentResponse_description' - The updated description of the intent.
--
-- 'botId', 'updateIntentResponse_botId' - The identifier of the bot that contains the intent.
--
-- 'slotPriorities', 'updateIntentResponse_slotPriorities' - The updated list of slots and their priorities that are elicited from
-- the user for the intent.
--
-- 'intentId', 'updateIntentResponse_intentId' - The identifier of the intent that was updated.
--
-- 'intentName', 'updateIntentResponse_intentName' - The updated name of the intent.
--
-- 'fulfillmentCodeHook', 'updateIntentResponse_fulfillmentCodeHook' - The updated Lambda function called when the intent is ready for
-- fulfillment.
--
-- 'initialResponseSetting', 'updateIntentResponse_initialResponseSetting' -
--
-- 'inputContexts', 'updateIntentResponse_inputContexts' - The updated list of contexts that must be active for the intent to be
-- considered by Amazon Lex.
--
-- 'lastUpdatedDateTime', 'updateIntentResponse_lastUpdatedDateTime' - A timestamp of the last time that the intent was modified.
--
-- 'httpStatus', 'updateIntentResponse_httpStatus' - The response's http status code.
newUpdateIntentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateIntentResponse
newUpdateIntentResponse pHttpStatus_ =
  UpdateIntentResponse'
    { intentClosingSetting =
        Prelude.Nothing,
      sampleUtterances = Prelude.Nothing,
      kendraConfiguration = Prelude.Nothing,
      botVersion = Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      dialogCodeHook = Prelude.Nothing,
      localeId = Prelude.Nothing,
      outputContexts = Prelude.Nothing,
      intentConfirmationSetting = Prelude.Nothing,
      parentIntentSignature = Prelude.Nothing,
      description = Prelude.Nothing,
      botId = Prelude.Nothing,
      slotPriorities = Prelude.Nothing,
      intentId = Prelude.Nothing,
      intentName = Prelude.Nothing,
      fulfillmentCodeHook = Prelude.Nothing,
      initialResponseSetting = Prelude.Nothing,
      inputContexts = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The updated response that Amazon Lex sends the user when the intent is
-- closed.
updateIntentResponse_intentClosingSetting :: Lens.Lens' UpdateIntentResponse (Prelude.Maybe IntentClosingSetting)
updateIntentResponse_intentClosingSetting = Lens.lens (\UpdateIntentResponse' {intentClosingSetting} -> intentClosingSetting) (\s@UpdateIntentResponse' {} a -> s {intentClosingSetting = a} :: UpdateIntentResponse)

-- | The updated list of sample utterances for the intent.
updateIntentResponse_sampleUtterances :: Lens.Lens' UpdateIntentResponse (Prelude.Maybe [SampleUtterance])
updateIntentResponse_sampleUtterances = Lens.lens (\UpdateIntentResponse' {sampleUtterances} -> sampleUtterances) (\s@UpdateIntentResponse' {} a -> s {sampleUtterances = a} :: UpdateIntentResponse) Prelude.. Lens.mapping Lens.coerced

-- | The updated configuration for connecting to an Amazon Kendra index with
-- the @AMAZON.KendraSearchIntent@ intent.
updateIntentResponse_kendraConfiguration :: Lens.Lens' UpdateIntentResponse (Prelude.Maybe KendraConfiguration)
updateIntentResponse_kendraConfiguration = Lens.lens (\UpdateIntentResponse' {kendraConfiguration} -> kendraConfiguration) (\s@UpdateIntentResponse' {} a -> s {kendraConfiguration = a} :: UpdateIntentResponse)

-- | The version of the bot that contains the intent. Will always be @DRAFT@.
updateIntentResponse_botVersion :: Lens.Lens' UpdateIntentResponse (Prelude.Maybe Prelude.Text)
updateIntentResponse_botVersion = Lens.lens (\UpdateIntentResponse' {botVersion} -> botVersion) (\s@UpdateIntentResponse' {} a -> s {botVersion = a} :: UpdateIntentResponse)

-- | A timestamp of when the intent was created.
updateIntentResponse_creationDateTime :: Lens.Lens' UpdateIntentResponse (Prelude.Maybe Prelude.UTCTime)
updateIntentResponse_creationDateTime = Lens.lens (\UpdateIntentResponse' {creationDateTime} -> creationDateTime) (\s@UpdateIntentResponse' {} a -> s {creationDateTime = a} :: UpdateIntentResponse) Prelude.. Lens.mapping Core._Time

-- | The updated Lambda function called during each turn of the conversation
-- with the user.
updateIntentResponse_dialogCodeHook :: Lens.Lens' UpdateIntentResponse (Prelude.Maybe DialogCodeHookSettings)
updateIntentResponse_dialogCodeHook = Lens.lens (\UpdateIntentResponse' {dialogCodeHook} -> dialogCodeHook) (\s@UpdateIntentResponse' {} a -> s {dialogCodeHook = a} :: UpdateIntentResponse)

-- | The updated language and locale of the intent.
updateIntentResponse_localeId :: Lens.Lens' UpdateIntentResponse (Prelude.Maybe Prelude.Text)
updateIntentResponse_localeId = Lens.lens (\UpdateIntentResponse' {localeId} -> localeId) (\s@UpdateIntentResponse' {} a -> s {localeId = a} :: UpdateIntentResponse)

-- | The updated list of contexts that Amazon Lex activates when the intent
-- is fulfilled.
updateIntentResponse_outputContexts :: Lens.Lens' UpdateIntentResponse (Prelude.Maybe [OutputContext])
updateIntentResponse_outputContexts = Lens.lens (\UpdateIntentResponse' {outputContexts} -> outputContexts) (\s@UpdateIntentResponse' {} a -> s {outputContexts = a} :: UpdateIntentResponse) Prelude.. Lens.mapping Lens.coerced

-- | The updated prompts that Amazon Lex sends to the user to confirm the
-- completion of an intent.
updateIntentResponse_intentConfirmationSetting :: Lens.Lens' UpdateIntentResponse (Prelude.Maybe IntentConfirmationSetting)
updateIntentResponse_intentConfirmationSetting = Lens.lens (\UpdateIntentResponse' {intentConfirmationSetting} -> intentConfirmationSetting) (\s@UpdateIntentResponse' {} a -> s {intentConfirmationSetting = a} :: UpdateIntentResponse)

-- | The updated built-in intent that is the parent of this intent.
updateIntentResponse_parentIntentSignature :: Lens.Lens' UpdateIntentResponse (Prelude.Maybe Prelude.Text)
updateIntentResponse_parentIntentSignature = Lens.lens (\UpdateIntentResponse' {parentIntentSignature} -> parentIntentSignature) (\s@UpdateIntentResponse' {} a -> s {parentIntentSignature = a} :: UpdateIntentResponse)

-- | The updated description of the intent.
updateIntentResponse_description :: Lens.Lens' UpdateIntentResponse (Prelude.Maybe Prelude.Text)
updateIntentResponse_description = Lens.lens (\UpdateIntentResponse' {description} -> description) (\s@UpdateIntentResponse' {} a -> s {description = a} :: UpdateIntentResponse)

-- | The identifier of the bot that contains the intent.
updateIntentResponse_botId :: Lens.Lens' UpdateIntentResponse (Prelude.Maybe Prelude.Text)
updateIntentResponse_botId = Lens.lens (\UpdateIntentResponse' {botId} -> botId) (\s@UpdateIntentResponse' {} a -> s {botId = a} :: UpdateIntentResponse)

-- | The updated list of slots and their priorities that are elicited from
-- the user for the intent.
updateIntentResponse_slotPriorities :: Lens.Lens' UpdateIntentResponse (Prelude.Maybe [SlotPriority])
updateIntentResponse_slotPriorities = Lens.lens (\UpdateIntentResponse' {slotPriorities} -> slotPriorities) (\s@UpdateIntentResponse' {} a -> s {slotPriorities = a} :: UpdateIntentResponse) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the intent that was updated.
updateIntentResponse_intentId :: Lens.Lens' UpdateIntentResponse (Prelude.Maybe Prelude.Text)
updateIntentResponse_intentId = Lens.lens (\UpdateIntentResponse' {intentId} -> intentId) (\s@UpdateIntentResponse' {} a -> s {intentId = a} :: UpdateIntentResponse)

-- | The updated name of the intent.
updateIntentResponse_intentName :: Lens.Lens' UpdateIntentResponse (Prelude.Maybe Prelude.Text)
updateIntentResponse_intentName = Lens.lens (\UpdateIntentResponse' {intentName} -> intentName) (\s@UpdateIntentResponse' {} a -> s {intentName = a} :: UpdateIntentResponse)

-- | The updated Lambda function called when the intent is ready for
-- fulfillment.
updateIntentResponse_fulfillmentCodeHook :: Lens.Lens' UpdateIntentResponse (Prelude.Maybe FulfillmentCodeHookSettings)
updateIntentResponse_fulfillmentCodeHook = Lens.lens (\UpdateIntentResponse' {fulfillmentCodeHook} -> fulfillmentCodeHook) (\s@UpdateIntentResponse' {} a -> s {fulfillmentCodeHook = a} :: UpdateIntentResponse)

-- |
updateIntentResponse_initialResponseSetting :: Lens.Lens' UpdateIntentResponse (Prelude.Maybe InitialResponseSetting)
updateIntentResponse_initialResponseSetting = Lens.lens (\UpdateIntentResponse' {initialResponseSetting} -> initialResponseSetting) (\s@UpdateIntentResponse' {} a -> s {initialResponseSetting = a} :: UpdateIntentResponse)

-- | The updated list of contexts that must be active for the intent to be
-- considered by Amazon Lex.
updateIntentResponse_inputContexts :: Lens.Lens' UpdateIntentResponse (Prelude.Maybe [InputContext])
updateIntentResponse_inputContexts = Lens.lens (\UpdateIntentResponse' {inputContexts} -> inputContexts) (\s@UpdateIntentResponse' {} a -> s {inputContexts = a} :: UpdateIntentResponse) Prelude.. Lens.mapping Lens.coerced

-- | A timestamp of the last time that the intent was modified.
updateIntentResponse_lastUpdatedDateTime :: Lens.Lens' UpdateIntentResponse (Prelude.Maybe Prelude.UTCTime)
updateIntentResponse_lastUpdatedDateTime = Lens.lens (\UpdateIntentResponse' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@UpdateIntentResponse' {} a -> s {lastUpdatedDateTime = a} :: UpdateIntentResponse) Prelude.. Lens.mapping Core._Time

-- | The response's http status code.
updateIntentResponse_httpStatus :: Lens.Lens' UpdateIntentResponse Prelude.Int
updateIntentResponse_httpStatus = Lens.lens (\UpdateIntentResponse' {httpStatus} -> httpStatus) (\s@UpdateIntentResponse' {} a -> s {httpStatus = a} :: UpdateIntentResponse)

instance Prelude.NFData UpdateIntentResponse where
  rnf UpdateIntentResponse' {..} =
    Prelude.rnf intentClosingSetting
      `Prelude.seq` Prelude.rnf sampleUtterances
      `Prelude.seq` Prelude.rnf kendraConfiguration
      `Prelude.seq` Prelude.rnf botVersion
      `Prelude.seq` Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf dialogCodeHook
      `Prelude.seq` Prelude.rnf localeId
      `Prelude.seq` Prelude.rnf outputContexts
      `Prelude.seq` Prelude.rnf intentConfirmationSetting
      `Prelude.seq` Prelude.rnf parentIntentSignature
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf botId
      `Prelude.seq` Prelude.rnf slotPriorities
      `Prelude.seq` Prelude.rnf intentId
      `Prelude.seq` Prelude.rnf intentName
      `Prelude.seq` Prelude.rnf fulfillmentCodeHook
      `Prelude.seq` Prelude.rnf initialResponseSetting
      `Prelude.seq` Prelude.rnf inputContexts
      `Prelude.seq` Prelude.rnf lastUpdatedDateTime
      `Prelude.seq` Prelude.rnf httpStatus
