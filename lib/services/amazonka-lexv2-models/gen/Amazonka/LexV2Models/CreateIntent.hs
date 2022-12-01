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
-- Module      : Amazonka.LexV2Models.CreateIntent
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an intent.
--
-- To define the interaction between the user and your bot, you define one
-- or more intents. For example, for a pizza ordering bot you would create
-- an @OrderPizza@ intent.
--
-- When you create an intent, you must provide a name. You can optionally
-- provide the following:
--
-- -   Sample utterances. For example, \"I want to order a pizza\" and
--     \"Can I order a pizza.\" You can\'t provide utterances for built-in
--     intents.
--
-- -   Information to be gathered. You specify slots for the information
--     that you bot requests from the user. You can specify standard slot
--     types, such as date and time, or custom slot types for your
--     application.
--
-- -   How the intent is fulfilled. You can provide a Lambda function or
--     configure the intent to return the intent information to your client
--     application. If you use a Lambda function, Amazon Lex invokes the
--     function when all of the intent information is available.
--
-- -   A confirmation prompt to send to the user to confirm an intent. For
--     example, \"Shall I order your pizza?\"
--
-- -   A conclusion statement to send to the user after the intent is
--     fulfilled. For example, \"I ordered your pizza.\"
--
-- -   A follow-up prompt that asks the user for additional activity. For
--     example, \"Do you want a drink with your pizza?\"
module Amazonka.LexV2Models.CreateIntent
  ( -- * Creating a Request
    CreateIntent (..),
    newCreateIntent,

    -- * Request Lenses
    createIntent_intentClosingSetting,
    createIntent_sampleUtterances,
    createIntent_kendraConfiguration,
    createIntent_dialogCodeHook,
    createIntent_outputContexts,
    createIntent_intentConfirmationSetting,
    createIntent_parentIntentSignature,
    createIntent_description,
    createIntent_fulfillmentCodeHook,
    createIntent_initialResponseSetting,
    createIntent_inputContexts,
    createIntent_intentName,
    createIntent_botId,
    createIntent_botVersion,
    createIntent_localeId,

    -- * Destructuring the Response
    CreateIntentResponse (..),
    newCreateIntentResponse,

    -- * Response Lenses
    createIntentResponse_intentClosingSetting,
    createIntentResponse_sampleUtterances,
    createIntentResponse_kendraConfiguration,
    createIntentResponse_botVersion,
    createIntentResponse_creationDateTime,
    createIntentResponse_dialogCodeHook,
    createIntentResponse_localeId,
    createIntentResponse_outputContexts,
    createIntentResponse_intentConfirmationSetting,
    createIntentResponse_parentIntentSignature,
    createIntentResponse_description,
    createIntentResponse_botId,
    createIntentResponse_intentId,
    createIntentResponse_intentName,
    createIntentResponse_fulfillmentCodeHook,
    createIntentResponse_initialResponseSetting,
    createIntentResponse_inputContexts,
    createIntentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LexV2Models.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateIntent' smart constructor.
data CreateIntent = CreateIntent'
  { -- | Sets the response that Amazon Lex sends to the user when the intent is
    -- closed.
    intentClosingSetting :: Prelude.Maybe IntentClosingSetting,
    -- | An array of strings that a user might say to signal the intent. For
    -- example, \"I want a pizza\", or \"I want a {PizzaSize} pizza\".
    --
    -- In an utterance, slot names are enclosed in curly braces (\"{\", \"}\")
    -- to indicate where they should be displayed in the utterance shown to the
    -- user..
    sampleUtterances :: Prelude.Maybe [SampleUtterance],
    -- | Configuration information required to use the
    -- @AMAZON.KendraSearchIntent@ intent to connect to an Amazon Kendra index.
    -- The @AMAZON.KendraSearchIntent@ intent is called when Amazon Lex can\'t
    -- determine another intent to invoke.
    kendraConfiguration :: Prelude.Maybe KendraConfiguration,
    -- | Specifies that Amazon Lex invokes the alias Lambda function for each
    -- user input. You can invoke this Lambda function to personalize user
    -- interaction.
    --
    -- For example, suppose that your bot determines that the user\'s name is
    -- John. You Lambda function might retrieve John\'s information from a
    -- backend database and prepopulate some of the values. For example, if you
    -- find that John is gluten intolerant, you might set the corresponding
    -- intent slot, @glutenIntolerant@ to @true@. You might find John\'s phone
    -- number and set the corresponding session attribute.
    dialogCodeHook :: Prelude.Maybe DialogCodeHookSettings,
    -- | A lists of contexts that the intent activates when it is fulfilled.
    --
    -- You can use an output context to indicate the intents that Amazon Lex
    -- should consider for the next turn of the conversation with a customer.
    --
    -- When you use the @outputContextsList@ property, all of the contexts
    -- specified in the list are activated when the intent is fulfilled. You
    -- can set up to 10 output contexts. You can also set the number of
    -- conversation turns that the context should be active, or the length of
    -- time that the context should be active.
    outputContexts :: Prelude.Maybe [OutputContext],
    -- | Provides prompts that Amazon Lex sends to the user to confirm the
    -- completion of an intent. If the user answers \"no,\" the settings
    -- contain a statement that is sent to the user to end the intent.
    intentConfirmationSetting :: Prelude.Maybe IntentConfirmationSetting,
    -- | A unique identifier for the built-in intent to base this intent on.
    parentIntentSignature :: Prelude.Maybe Prelude.Text,
    -- | A description of the intent. Use the description to help identify the
    -- intent in lists.
    description :: Prelude.Maybe Prelude.Text,
    -- | Specifies that Amazon Lex invokes the alias Lambda function when the
    -- intent is ready for fulfillment. You can invoke this function to
    -- complete the bot\'s transaction with the user.
    --
    -- For example, in a pizza ordering bot, the Lambda function can look up
    -- the closest pizza restaurant to the customer\'s location and then place
    -- an order on the customer\'s behalf.
    fulfillmentCodeHook :: Prelude.Maybe FulfillmentCodeHookSettings,
    -- | Configuration settings for the response that is sent to the user at the
    -- beginning of a conversation, before eliciting slot values.
    initialResponseSetting :: Prelude.Maybe InitialResponseSetting,
    -- | A list of contexts that must be active for this intent to be considered
    -- by Amazon Lex.
    --
    -- When an intent has an input context list, Amazon Lex only considers
    -- using the intent in an interaction with the user when the specified
    -- contexts are included in the active context list for the session. If the
    -- contexts are not active, then Amazon Lex will not use the intent.
    --
    -- A context can be automatically activated using the @outputContexts@
    -- property or it can be set at runtime.
    --
    -- For example, if there are two intents with different input contexts that
    -- respond to the same utterances, only the intent with the active context
    -- will respond.
    --
    -- An intent may have up to 5 input contexts. If an intent has multiple
    -- input contexts, all of the contexts must be active to consider the
    -- intent.
    inputContexts :: Prelude.Maybe [InputContext],
    -- | The name of the intent. Intent names must be unique in the locale that
    -- contains the intent and cannot match the name of any built-in intent.
    intentName :: Prelude.Text,
    -- | The identifier of the bot associated with this intent.
    botId :: Prelude.Text,
    -- | The identifier of the version of the bot associated with this intent.
    botVersion :: Prelude.Text,
    -- | The identifier of the language and locale where this intent is used. All
    -- of the bots, slot types, and slots used by the intent must have the same
    -- locale. For more information, see
    -- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>.
    localeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateIntent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'intentClosingSetting', 'createIntent_intentClosingSetting' - Sets the response that Amazon Lex sends to the user when the intent is
-- closed.
--
-- 'sampleUtterances', 'createIntent_sampleUtterances' - An array of strings that a user might say to signal the intent. For
-- example, \"I want a pizza\", or \"I want a {PizzaSize} pizza\".
--
-- In an utterance, slot names are enclosed in curly braces (\"{\", \"}\")
-- to indicate where they should be displayed in the utterance shown to the
-- user..
--
-- 'kendraConfiguration', 'createIntent_kendraConfiguration' - Configuration information required to use the
-- @AMAZON.KendraSearchIntent@ intent to connect to an Amazon Kendra index.
-- The @AMAZON.KendraSearchIntent@ intent is called when Amazon Lex can\'t
-- determine another intent to invoke.
--
-- 'dialogCodeHook', 'createIntent_dialogCodeHook' - Specifies that Amazon Lex invokes the alias Lambda function for each
-- user input. You can invoke this Lambda function to personalize user
-- interaction.
--
-- For example, suppose that your bot determines that the user\'s name is
-- John. You Lambda function might retrieve John\'s information from a
-- backend database and prepopulate some of the values. For example, if you
-- find that John is gluten intolerant, you might set the corresponding
-- intent slot, @glutenIntolerant@ to @true@. You might find John\'s phone
-- number and set the corresponding session attribute.
--
-- 'outputContexts', 'createIntent_outputContexts' - A lists of contexts that the intent activates when it is fulfilled.
--
-- You can use an output context to indicate the intents that Amazon Lex
-- should consider for the next turn of the conversation with a customer.
--
-- When you use the @outputContextsList@ property, all of the contexts
-- specified in the list are activated when the intent is fulfilled. You
-- can set up to 10 output contexts. You can also set the number of
-- conversation turns that the context should be active, or the length of
-- time that the context should be active.
--
-- 'intentConfirmationSetting', 'createIntent_intentConfirmationSetting' - Provides prompts that Amazon Lex sends to the user to confirm the
-- completion of an intent. If the user answers \"no,\" the settings
-- contain a statement that is sent to the user to end the intent.
--
-- 'parentIntentSignature', 'createIntent_parentIntentSignature' - A unique identifier for the built-in intent to base this intent on.
--
-- 'description', 'createIntent_description' - A description of the intent. Use the description to help identify the
-- intent in lists.
--
-- 'fulfillmentCodeHook', 'createIntent_fulfillmentCodeHook' - Specifies that Amazon Lex invokes the alias Lambda function when the
-- intent is ready for fulfillment. You can invoke this function to
-- complete the bot\'s transaction with the user.
--
-- For example, in a pizza ordering bot, the Lambda function can look up
-- the closest pizza restaurant to the customer\'s location and then place
-- an order on the customer\'s behalf.
--
-- 'initialResponseSetting', 'createIntent_initialResponseSetting' - Configuration settings for the response that is sent to the user at the
-- beginning of a conversation, before eliciting slot values.
--
-- 'inputContexts', 'createIntent_inputContexts' - A list of contexts that must be active for this intent to be considered
-- by Amazon Lex.
--
-- When an intent has an input context list, Amazon Lex only considers
-- using the intent in an interaction with the user when the specified
-- contexts are included in the active context list for the session. If the
-- contexts are not active, then Amazon Lex will not use the intent.
--
-- A context can be automatically activated using the @outputContexts@
-- property or it can be set at runtime.
--
-- For example, if there are two intents with different input contexts that
-- respond to the same utterances, only the intent with the active context
-- will respond.
--
-- An intent may have up to 5 input contexts. If an intent has multiple
-- input contexts, all of the contexts must be active to consider the
-- intent.
--
-- 'intentName', 'createIntent_intentName' - The name of the intent. Intent names must be unique in the locale that
-- contains the intent and cannot match the name of any built-in intent.
--
-- 'botId', 'createIntent_botId' - The identifier of the bot associated with this intent.
--
-- 'botVersion', 'createIntent_botVersion' - The identifier of the version of the bot associated with this intent.
--
-- 'localeId', 'createIntent_localeId' - The identifier of the language and locale where this intent is used. All
-- of the bots, slot types, and slots used by the intent must have the same
-- locale. For more information, see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>.
newCreateIntent ::
  -- | 'intentName'
  Prelude.Text ->
  -- | 'botId'
  Prelude.Text ->
  -- | 'botVersion'
  Prelude.Text ->
  -- | 'localeId'
  Prelude.Text ->
  CreateIntent
newCreateIntent
  pIntentName_
  pBotId_
  pBotVersion_
  pLocaleId_ =
    CreateIntent'
      { intentClosingSetting =
          Prelude.Nothing,
        sampleUtterances = Prelude.Nothing,
        kendraConfiguration = Prelude.Nothing,
        dialogCodeHook = Prelude.Nothing,
        outputContexts = Prelude.Nothing,
        intentConfirmationSetting = Prelude.Nothing,
        parentIntentSignature = Prelude.Nothing,
        description = Prelude.Nothing,
        fulfillmentCodeHook = Prelude.Nothing,
        initialResponseSetting = Prelude.Nothing,
        inputContexts = Prelude.Nothing,
        intentName = pIntentName_,
        botId = pBotId_,
        botVersion = pBotVersion_,
        localeId = pLocaleId_
      }

-- | Sets the response that Amazon Lex sends to the user when the intent is
-- closed.
createIntent_intentClosingSetting :: Lens.Lens' CreateIntent (Prelude.Maybe IntentClosingSetting)
createIntent_intentClosingSetting = Lens.lens (\CreateIntent' {intentClosingSetting} -> intentClosingSetting) (\s@CreateIntent' {} a -> s {intentClosingSetting = a} :: CreateIntent)

-- | An array of strings that a user might say to signal the intent. For
-- example, \"I want a pizza\", or \"I want a {PizzaSize} pizza\".
--
-- In an utterance, slot names are enclosed in curly braces (\"{\", \"}\")
-- to indicate where they should be displayed in the utterance shown to the
-- user..
createIntent_sampleUtterances :: Lens.Lens' CreateIntent (Prelude.Maybe [SampleUtterance])
createIntent_sampleUtterances = Lens.lens (\CreateIntent' {sampleUtterances} -> sampleUtterances) (\s@CreateIntent' {} a -> s {sampleUtterances = a} :: CreateIntent) Prelude.. Lens.mapping Lens.coerced

-- | Configuration information required to use the
-- @AMAZON.KendraSearchIntent@ intent to connect to an Amazon Kendra index.
-- The @AMAZON.KendraSearchIntent@ intent is called when Amazon Lex can\'t
-- determine another intent to invoke.
createIntent_kendraConfiguration :: Lens.Lens' CreateIntent (Prelude.Maybe KendraConfiguration)
createIntent_kendraConfiguration = Lens.lens (\CreateIntent' {kendraConfiguration} -> kendraConfiguration) (\s@CreateIntent' {} a -> s {kendraConfiguration = a} :: CreateIntent)

-- | Specifies that Amazon Lex invokes the alias Lambda function for each
-- user input. You can invoke this Lambda function to personalize user
-- interaction.
--
-- For example, suppose that your bot determines that the user\'s name is
-- John. You Lambda function might retrieve John\'s information from a
-- backend database and prepopulate some of the values. For example, if you
-- find that John is gluten intolerant, you might set the corresponding
-- intent slot, @glutenIntolerant@ to @true@. You might find John\'s phone
-- number and set the corresponding session attribute.
createIntent_dialogCodeHook :: Lens.Lens' CreateIntent (Prelude.Maybe DialogCodeHookSettings)
createIntent_dialogCodeHook = Lens.lens (\CreateIntent' {dialogCodeHook} -> dialogCodeHook) (\s@CreateIntent' {} a -> s {dialogCodeHook = a} :: CreateIntent)

-- | A lists of contexts that the intent activates when it is fulfilled.
--
-- You can use an output context to indicate the intents that Amazon Lex
-- should consider for the next turn of the conversation with a customer.
--
-- When you use the @outputContextsList@ property, all of the contexts
-- specified in the list are activated when the intent is fulfilled. You
-- can set up to 10 output contexts. You can also set the number of
-- conversation turns that the context should be active, or the length of
-- time that the context should be active.
createIntent_outputContexts :: Lens.Lens' CreateIntent (Prelude.Maybe [OutputContext])
createIntent_outputContexts = Lens.lens (\CreateIntent' {outputContexts} -> outputContexts) (\s@CreateIntent' {} a -> s {outputContexts = a} :: CreateIntent) Prelude.. Lens.mapping Lens.coerced

-- | Provides prompts that Amazon Lex sends to the user to confirm the
-- completion of an intent. If the user answers \"no,\" the settings
-- contain a statement that is sent to the user to end the intent.
createIntent_intentConfirmationSetting :: Lens.Lens' CreateIntent (Prelude.Maybe IntentConfirmationSetting)
createIntent_intentConfirmationSetting = Lens.lens (\CreateIntent' {intentConfirmationSetting} -> intentConfirmationSetting) (\s@CreateIntent' {} a -> s {intentConfirmationSetting = a} :: CreateIntent)

-- | A unique identifier for the built-in intent to base this intent on.
createIntent_parentIntentSignature :: Lens.Lens' CreateIntent (Prelude.Maybe Prelude.Text)
createIntent_parentIntentSignature = Lens.lens (\CreateIntent' {parentIntentSignature} -> parentIntentSignature) (\s@CreateIntent' {} a -> s {parentIntentSignature = a} :: CreateIntent)

-- | A description of the intent. Use the description to help identify the
-- intent in lists.
createIntent_description :: Lens.Lens' CreateIntent (Prelude.Maybe Prelude.Text)
createIntent_description = Lens.lens (\CreateIntent' {description} -> description) (\s@CreateIntent' {} a -> s {description = a} :: CreateIntent)

-- | Specifies that Amazon Lex invokes the alias Lambda function when the
-- intent is ready for fulfillment. You can invoke this function to
-- complete the bot\'s transaction with the user.
--
-- For example, in a pizza ordering bot, the Lambda function can look up
-- the closest pizza restaurant to the customer\'s location and then place
-- an order on the customer\'s behalf.
createIntent_fulfillmentCodeHook :: Lens.Lens' CreateIntent (Prelude.Maybe FulfillmentCodeHookSettings)
createIntent_fulfillmentCodeHook = Lens.lens (\CreateIntent' {fulfillmentCodeHook} -> fulfillmentCodeHook) (\s@CreateIntent' {} a -> s {fulfillmentCodeHook = a} :: CreateIntent)

-- | Configuration settings for the response that is sent to the user at the
-- beginning of a conversation, before eliciting slot values.
createIntent_initialResponseSetting :: Lens.Lens' CreateIntent (Prelude.Maybe InitialResponseSetting)
createIntent_initialResponseSetting = Lens.lens (\CreateIntent' {initialResponseSetting} -> initialResponseSetting) (\s@CreateIntent' {} a -> s {initialResponseSetting = a} :: CreateIntent)

-- | A list of contexts that must be active for this intent to be considered
-- by Amazon Lex.
--
-- When an intent has an input context list, Amazon Lex only considers
-- using the intent in an interaction with the user when the specified
-- contexts are included in the active context list for the session. If the
-- contexts are not active, then Amazon Lex will not use the intent.
--
-- A context can be automatically activated using the @outputContexts@
-- property or it can be set at runtime.
--
-- For example, if there are two intents with different input contexts that
-- respond to the same utterances, only the intent with the active context
-- will respond.
--
-- An intent may have up to 5 input contexts. If an intent has multiple
-- input contexts, all of the contexts must be active to consider the
-- intent.
createIntent_inputContexts :: Lens.Lens' CreateIntent (Prelude.Maybe [InputContext])
createIntent_inputContexts = Lens.lens (\CreateIntent' {inputContexts} -> inputContexts) (\s@CreateIntent' {} a -> s {inputContexts = a} :: CreateIntent) Prelude.. Lens.mapping Lens.coerced

-- | The name of the intent. Intent names must be unique in the locale that
-- contains the intent and cannot match the name of any built-in intent.
createIntent_intentName :: Lens.Lens' CreateIntent Prelude.Text
createIntent_intentName = Lens.lens (\CreateIntent' {intentName} -> intentName) (\s@CreateIntent' {} a -> s {intentName = a} :: CreateIntent)

-- | The identifier of the bot associated with this intent.
createIntent_botId :: Lens.Lens' CreateIntent Prelude.Text
createIntent_botId = Lens.lens (\CreateIntent' {botId} -> botId) (\s@CreateIntent' {} a -> s {botId = a} :: CreateIntent)

-- | The identifier of the version of the bot associated with this intent.
createIntent_botVersion :: Lens.Lens' CreateIntent Prelude.Text
createIntent_botVersion = Lens.lens (\CreateIntent' {botVersion} -> botVersion) (\s@CreateIntent' {} a -> s {botVersion = a} :: CreateIntent)

-- | The identifier of the language and locale where this intent is used. All
-- of the bots, slot types, and slots used by the intent must have the same
-- locale. For more information, see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>.
createIntent_localeId :: Lens.Lens' CreateIntent Prelude.Text
createIntent_localeId = Lens.lens (\CreateIntent' {localeId} -> localeId) (\s@CreateIntent' {} a -> s {localeId = a} :: CreateIntent)

instance Core.AWSRequest CreateIntent where
  type AWSResponse CreateIntent = CreateIntentResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateIntentResponse'
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
            Prelude.<*> (x Core..?> "intentId")
            Prelude.<*> (x Core..?> "intentName")
            Prelude.<*> (x Core..?> "fulfillmentCodeHook")
            Prelude.<*> (x Core..?> "initialResponseSetting")
            Prelude.<*> (x Core..?> "inputContexts" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateIntent where
  hashWithSalt _salt CreateIntent' {..} =
    _salt `Prelude.hashWithSalt` intentClosingSetting
      `Prelude.hashWithSalt` sampleUtterances
      `Prelude.hashWithSalt` kendraConfiguration
      `Prelude.hashWithSalt` dialogCodeHook
      `Prelude.hashWithSalt` outputContexts
      `Prelude.hashWithSalt` intentConfirmationSetting
      `Prelude.hashWithSalt` parentIntentSignature
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` fulfillmentCodeHook
      `Prelude.hashWithSalt` initialResponseSetting
      `Prelude.hashWithSalt` inputContexts
      `Prelude.hashWithSalt` intentName
      `Prelude.hashWithSalt` botId
      `Prelude.hashWithSalt` botVersion
      `Prelude.hashWithSalt` localeId

instance Prelude.NFData CreateIntent where
  rnf CreateIntent' {..} =
    Prelude.rnf intentClosingSetting
      `Prelude.seq` Prelude.rnf sampleUtterances
      `Prelude.seq` Prelude.rnf kendraConfiguration
      `Prelude.seq` Prelude.rnf dialogCodeHook
      `Prelude.seq` Prelude.rnf outputContexts
      `Prelude.seq` Prelude.rnf intentConfirmationSetting
      `Prelude.seq` Prelude.rnf parentIntentSignature
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf fulfillmentCodeHook
      `Prelude.seq` Prelude.rnf initialResponseSetting
      `Prelude.seq` Prelude.rnf inputContexts
      `Prelude.seq` Prelude.rnf intentName
      `Prelude.seq` Prelude.rnf botId
      `Prelude.seq` Prelude.rnf botVersion
      `Prelude.seq` Prelude.rnf localeId

instance Core.ToHeaders CreateIntent where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateIntent where
  toJSON CreateIntent' {..} =
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
            ("fulfillmentCodeHook" Core..=)
              Prelude.<$> fulfillmentCodeHook,
            ("initialResponseSetting" Core..=)
              Prelude.<$> initialResponseSetting,
            ("inputContexts" Core..=) Prelude.<$> inputContexts,
            Prelude.Just ("intentName" Core..= intentName)
          ]
      )

instance Core.ToPath CreateIntent where
  toPath CreateIntent' {..} =
    Prelude.mconcat
      [ "/bots/",
        Core.toBS botId,
        "/botversions/",
        Core.toBS botVersion,
        "/botlocales/",
        Core.toBS localeId,
        "/intents/"
      ]

instance Core.ToQuery CreateIntent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateIntentResponse' smart constructor.
data CreateIntentResponse = CreateIntentResponse'
  { -- | The closing setting specified for the intent.
    intentClosingSetting :: Prelude.Maybe IntentClosingSetting,
    -- | The sample utterances specified for the intent.
    sampleUtterances :: Prelude.Maybe [SampleUtterance],
    -- | Configuration for searching a Amazon Kendra index specified for the
    -- intent.
    kendraConfiguration :: Prelude.Maybe KendraConfiguration,
    -- | The identifier of the version of the bot associated with the intent.
    botVersion :: Prelude.Maybe Prelude.Text,
    -- | A timestamp of the date and time that the intent was created.
    creationDateTime :: Prelude.Maybe Core.POSIX,
    -- | The dialog Lambda function specified for the intent.
    dialogCodeHook :: Prelude.Maybe DialogCodeHookSettings,
    -- | The locale that the intent is specified to use.
    localeId :: Prelude.Maybe Prelude.Text,
    -- | The list of output contexts specified for the intent.
    outputContexts :: Prelude.Maybe [OutputContext],
    -- | The confirmation setting specified for the intent.
    intentConfirmationSetting :: Prelude.Maybe IntentConfirmationSetting,
    -- | The signature of the parent intent specified for the intent.
    parentIntentSignature :: Prelude.Maybe Prelude.Text,
    -- | The description specified for the intent.
    description :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the bot associated with the intent.
    botId :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the intent.
    intentId :: Prelude.Maybe Prelude.Text,
    -- | The name specified for the intent.
    intentName :: Prelude.Maybe Prelude.Text,
    -- | The fulfillment Lambda function specified for the intent.
    fulfillmentCodeHook :: Prelude.Maybe FulfillmentCodeHookSettings,
    -- | Configuration settings for the response that is sent to the user at the
    -- beginning of a conversation, before eliciting slot values.
    initialResponseSetting :: Prelude.Maybe InitialResponseSetting,
    -- | The list of input contexts specified for the intent.
    inputContexts :: Prelude.Maybe [InputContext],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateIntentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'intentClosingSetting', 'createIntentResponse_intentClosingSetting' - The closing setting specified for the intent.
--
-- 'sampleUtterances', 'createIntentResponse_sampleUtterances' - The sample utterances specified for the intent.
--
-- 'kendraConfiguration', 'createIntentResponse_kendraConfiguration' - Configuration for searching a Amazon Kendra index specified for the
-- intent.
--
-- 'botVersion', 'createIntentResponse_botVersion' - The identifier of the version of the bot associated with the intent.
--
-- 'creationDateTime', 'createIntentResponse_creationDateTime' - A timestamp of the date and time that the intent was created.
--
-- 'dialogCodeHook', 'createIntentResponse_dialogCodeHook' - The dialog Lambda function specified for the intent.
--
-- 'localeId', 'createIntentResponse_localeId' - The locale that the intent is specified to use.
--
-- 'outputContexts', 'createIntentResponse_outputContexts' - The list of output contexts specified for the intent.
--
-- 'intentConfirmationSetting', 'createIntentResponse_intentConfirmationSetting' - The confirmation setting specified for the intent.
--
-- 'parentIntentSignature', 'createIntentResponse_parentIntentSignature' - The signature of the parent intent specified for the intent.
--
-- 'description', 'createIntentResponse_description' - The description specified for the intent.
--
-- 'botId', 'createIntentResponse_botId' - The identifier of the bot associated with the intent.
--
-- 'intentId', 'createIntentResponse_intentId' - A unique identifier for the intent.
--
-- 'intentName', 'createIntentResponse_intentName' - The name specified for the intent.
--
-- 'fulfillmentCodeHook', 'createIntentResponse_fulfillmentCodeHook' - The fulfillment Lambda function specified for the intent.
--
-- 'initialResponseSetting', 'createIntentResponse_initialResponseSetting' - Configuration settings for the response that is sent to the user at the
-- beginning of a conversation, before eliciting slot values.
--
-- 'inputContexts', 'createIntentResponse_inputContexts' - The list of input contexts specified for the intent.
--
-- 'httpStatus', 'createIntentResponse_httpStatus' - The response's http status code.
newCreateIntentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateIntentResponse
newCreateIntentResponse pHttpStatus_ =
  CreateIntentResponse'
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
      intentId = Prelude.Nothing,
      intentName = Prelude.Nothing,
      fulfillmentCodeHook = Prelude.Nothing,
      initialResponseSetting = Prelude.Nothing,
      inputContexts = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The closing setting specified for the intent.
createIntentResponse_intentClosingSetting :: Lens.Lens' CreateIntentResponse (Prelude.Maybe IntentClosingSetting)
createIntentResponse_intentClosingSetting = Lens.lens (\CreateIntentResponse' {intentClosingSetting} -> intentClosingSetting) (\s@CreateIntentResponse' {} a -> s {intentClosingSetting = a} :: CreateIntentResponse)

-- | The sample utterances specified for the intent.
createIntentResponse_sampleUtterances :: Lens.Lens' CreateIntentResponse (Prelude.Maybe [SampleUtterance])
createIntentResponse_sampleUtterances = Lens.lens (\CreateIntentResponse' {sampleUtterances} -> sampleUtterances) (\s@CreateIntentResponse' {} a -> s {sampleUtterances = a} :: CreateIntentResponse) Prelude.. Lens.mapping Lens.coerced

-- | Configuration for searching a Amazon Kendra index specified for the
-- intent.
createIntentResponse_kendraConfiguration :: Lens.Lens' CreateIntentResponse (Prelude.Maybe KendraConfiguration)
createIntentResponse_kendraConfiguration = Lens.lens (\CreateIntentResponse' {kendraConfiguration} -> kendraConfiguration) (\s@CreateIntentResponse' {} a -> s {kendraConfiguration = a} :: CreateIntentResponse)

-- | The identifier of the version of the bot associated with the intent.
createIntentResponse_botVersion :: Lens.Lens' CreateIntentResponse (Prelude.Maybe Prelude.Text)
createIntentResponse_botVersion = Lens.lens (\CreateIntentResponse' {botVersion} -> botVersion) (\s@CreateIntentResponse' {} a -> s {botVersion = a} :: CreateIntentResponse)

-- | A timestamp of the date and time that the intent was created.
createIntentResponse_creationDateTime :: Lens.Lens' CreateIntentResponse (Prelude.Maybe Prelude.UTCTime)
createIntentResponse_creationDateTime = Lens.lens (\CreateIntentResponse' {creationDateTime} -> creationDateTime) (\s@CreateIntentResponse' {} a -> s {creationDateTime = a} :: CreateIntentResponse) Prelude.. Lens.mapping Core._Time

-- | The dialog Lambda function specified for the intent.
createIntentResponse_dialogCodeHook :: Lens.Lens' CreateIntentResponse (Prelude.Maybe DialogCodeHookSettings)
createIntentResponse_dialogCodeHook = Lens.lens (\CreateIntentResponse' {dialogCodeHook} -> dialogCodeHook) (\s@CreateIntentResponse' {} a -> s {dialogCodeHook = a} :: CreateIntentResponse)

-- | The locale that the intent is specified to use.
createIntentResponse_localeId :: Lens.Lens' CreateIntentResponse (Prelude.Maybe Prelude.Text)
createIntentResponse_localeId = Lens.lens (\CreateIntentResponse' {localeId} -> localeId) (\s@CreateIntentResponse' {} a -> s {localeId = a} :: CreateIntentResponse)

-- | The list of output contexts specified for the intent.
createIntentResponse_outputContexts :: Lens.Lens' CreateIntentResponse (Prelude.Maybe [OutputContext])
createIntentResponse_outputContexts = Lens.lens (\CreateIntentResponse' {outputContexts} -> outputContexts) (\s@CreateIntentResponse' {} a -> s {outputContexts = a} :: CreateIntentResponse) Prelude.. Lens.mapping Lens.coerced

-- | The confirmation setting specified for the intent.
createIntentResponse_intentConfirmationSetting :: Lens.Lens' CreateIntentResponse (Prelude.Maybe IntentConfirmationSetting)
createIntentResponse_intentConfirmationSetting = Lens.lens (\CreateIntentResponse' {intentConfirmationSetting} -> intentConfirmationSetting) (\s@CreateIntentResponse' {} a -> s {intentConfirmationSetting = a} :: CreateIntentResponse)

-- | The signature of the parent intent specified for the intent.
createIntentResponse_parentIntentSignature :: Lens.Lens' CreateIntentResponse (Prelude.Maybe Prelude.Text)
createIntentResponse_parentIntentSignature = Lens.lens (\CreateIntentResponse' {parentIntentSignature} -> parentIntentSignature) (\s@CreateIntentResponse' {} a -> s {parentIntentSignature = a} :: CreateIntentResponse)

-- | The description specified for the intent.
createIntentResponse_description :: Lens.Lens' CreateIntentResponse (Prelude.Maybe Prelude.Text)
createIntentResponse_description = Lens.lens (\CreateIntentResponse' {description} -> description) (\s@CreateIntentResponse' {} a -> s {description = a} :: CreateIntentResponse)

-- | The identifier of the bot associated with the intent.
createIntentResponse_botId :: Lens.Lens' CreateIntentResponse (Prelude.Maybe Prelude.Text)
createIntentResponse_botId = Lens.lens (\CreateIntentResponse' {botId} -> botId) (\s@CreateIntentResponse' {} a -> s {botId = a} :: CreateIntentResponse)

-- | A unique identifier for the intent.
createIntentResponse_intentId :: Lens.Lens' CreateIntentResponse (Prelude.Maybe Prelude.Text)
createIntentResponse_intentId = Lens.lens (\CreateIntentResponse' {intentId} -> intentId) (\s@CreateIntentResponse' {} a -> s {intentId = a} :: CreateIntentResponse)

-- | The name specified for the intent.
createIntentResponse_intentName :: Lens.Lens' CreateIntentResponse (Prelude.Maybe Prelude.Text)
createIntentResponse_intentName = Lens.lens (\CreateIntentResponse' {intentName} -> intentName) (\s@CreateIntentResponse' {} a -> s {intentName = a} :: CreateIntentResponse)

-- | The fulfillment Lambda function specified for the intent.
createIntentResponse_fulfillmentCodeHook :: Lens.Lens' CreateIntentResponse (Prelude.Maybe FulfillmentCodeHookSettings)
createIntentResponse_fulfillmentCodeHook = Lens.lens (\CreateIntentResponse' {fulfillmentCodeHook} -> fulfillmentCodeHook) (\s@CreateIntentResponse' {} a -> s {fulfillmentCodeHook = a} :: CreateIntentResponse)

-- | Configuration settings for the response that is sent to the user at the
-- beginning of a conversation, before eliciting slot values.
createIntentResponse_initialResponseSetting :: Lens.Lens' CreateIntentResponse (Prelude.Maybe InitialResponseSetting)
createIntentResponse_initialResponseSetting = Lens.lens (\CreateIntentResponse' {initialResponseSetting} -> initialResponseSetting) (\s@CreateIntentResponse' {} a -> s {initialResponseSetting = a} :: CreateIntentResponse)

-- | The list of input contexts specified for the intent.
createIntentResponse_inputContexts :: Lens.Lens' CreateIntentResponse (Prelude.Maybe [InputContext])
createIntentResponse_inputContexts = Lens.lens (\CreateIntentResponse' {inputContexts} -> inputContexts) (\s@CreateIntentResponse' {} a -> s {inputContexts = a} :: CreateIntentResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createIntentResponse_httpStatus :: Lens.Lens' CreateIntentResponse Prelude.Int
createIntentResponse_httpStatus = Lens.lens (\CreateIntentResponse' {httpStatus} -> httpStatus) (\s@CreateIntentResponse' {} a -> s {httpStatus = a} :: CreateIntentResponse)

instance Prelude.NFData CreateIntentResponse where
  rnf CreateIntentResponse' {..} =
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
      `Prelude.seq` Prelude.rnf intentId
      `Prelude.seq` Prelude.rnf intentName
      `Prelude.seq` Prelude.rnf fulfillmentCodeHook
      `Prelude.seq` Prelude.rnf initialResponseSetting
      `Prelude.seq` Prelude.rnf inputContexts
      `Prelude.seq` Prelude.rnf httpStatus
