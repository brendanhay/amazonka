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
-- Module      : Amazonka.LexModels.GetIntent
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about an intent. In addition to the intent name, you
-- must specify the intent version.
--
-- This operation requires permissions to perform the @lex:GetIntent@
-- action.
module Amazonka.LexModels.GetIntent
  ( -- * Creating a Request
    GetIntent (..),
    newGetIntent,

    -- * Request Lenses
    getIntent_name,
    getIntent_version,

    -- * Destructuring the Response
    GetIntentResponse (..),
    newGetIntentResponse,

    -- * Response Lenses
    getIntentResponse_checksum,
    getIntentResponse_conclusionStatement,
    getIntentResponse_confirmationPrompt,
    getIntentResponse_createdDate,
    getIntentResponse_description,
    getIntentResponse_dialogCodeHook,
    getIntentResponse_followUpPrompt,
    getIntentResponse_fulfillmentActivity,
    getIntentResponse_inputContexts,
    getIntentResponse_kendraConfiguration,
    getIntentResponse_lastUpdatedDate,
    getIntentResponse_name,
    getIntentResponse_outputContexts,
    getIntentResponse_parentIntentSignature,
    getIntentResponse_rejectionStatement,
    getIntentResponse_sampleUtterances,
    getIntentResponse_slots,
    getIntentResponse_version,
    getIntentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexModels.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetIntent' smart constructor.
data GetIntent = GetIntent'
  { -- | The name of the intent. The name is case sensitive.
    name :: Prelude.Text,
    -- | The version of the intent.
    version :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetIntent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'getIntent_name' - The name of the intent. The name is case sensitive.
--
-- 'version', 'getIntent_version' - The version of the intent.
newGetIntent ::
  -- | 'name'
  Prelude.Text ->
  -- | 'version'
  Prelude.Text ->
  GetIntent
newGetIntent pName_ pVersion_ =
  GetIntent' {name = pName_, version = pVersion_}

-- | The name of the intent. The name is case sensitive.
getIntent_name :: Lens.Lens' GetIntent Prelude.Text
getIntent_name = Lens.lens (\GetIntent' {name} -> name) (\s@GetIntent' {} a -> s {name = a} :: GetIntent)

-- | The version of the intent.
getIntent_version :: Lens.Lens' GetIntent Prelude.Text
getIntent_version = Lens.lens (\GetIntent' {version} -> version) (\s@GetIntent' {} a -> s {version = a} :: GetIntent)

instance Core.AWSRequest GetIntent where
  type AWSResponse GetIntent = GetIntentResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetIntentResponse'
            Prelude.<$> (x Data..?> "checksum")
            Prelude.<*> (x Data..?> "conclusionStatement")
            Prelude.<*> (x Data..?> "confirmationPrompt")
            Prelude.<*> (x Data..?> "createdDate")
            Prelude.<*> (x Data..?> "description")
            Prelude.<*> (x Data..?> "dialogCodeHook")
            Prelude.<*> (x Data..?> "followUpPrompt")
            Prelude.<*> (x Data..?> "fulfillmentActivity")
            Prelude.<*> (x Data..?> "inputContexts" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "kendraConfiguration")
            Prelude.<*> (x Data..?> "lastUpdatedDate")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "outputContexts" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "parentIntentSignature")
            Prelude.<*> (x Data..?> "rejectionStatement")
            Prelude.<*> ( x Data..?> "sampleUtterances"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "slots" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "version")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetIntent where
  hashWithSalt _salt GetIntent' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` version

instance Prelude.NFData GetIntent where
  rnf GetIntent' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf version

instance Data.ToHeaders GetIntent where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetIntent where
  toPath GetIntent' {..} =
    Prelude.mconcat
      [ "/intents/",
        Data.toBS name,
        "/versions/",
        Data.toBS version
      ]

instance Data.ToQuery GetIntent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetIntentResponse' smart constructor.
data GetIntentResponse = GetIntentResponse'
  { -- | Checksum of the intent.
    checksum :: Prelude.Maybe Prelude.Text,
    -- | After the Lambda function specified in the @fulfillmentActivity@ element
    -- fulfills the intent, Amazon Lex conveys this statement to the user.
    conclusionStatement :: Prelude.Maybe Statement,
    -- | If defined in the bot, Amazon Lex uses prompt to confirm the intent
    -- before fulfilling the user\'s request. For more information, see
    -- PutIntent.
    confirmationPrompt :: Prelude.Maybe Prompt,
    -- | The date that the intent was created.
    createdDate :: Prelude.Maybe Data.POSIX,
    -- | A description of the intent.
    description :: Prelude.Maybe Prelude.Text,
    -- | If defined in the bot, Amazon Amazon Lex invokes this Lambda function
    -- for each user input. For more information, see PutIntent.
    dialogCodeHook :: Prelude.Maybe CodeHook,
    -- | If defined in the bot, Amazon Lex uses this prompt to solicit additional
    -- user activity after the intent is fulfilled. For more information, see
    -- PutIntent.
    followUpPrompt :: Prelude.Maybe FollowUpPrompt,
    -- | Describes how the intent is fulfilled. For more information, see
    -- PutIntent.
    fulfillmentActivity :: Prelude.Maybe FulfillmentActivity,
    -- | An array of @InputContext@ objects that lists the contexts that must be
    -- active for Amazon Lex to choose the intent in a conversation with the
    -- user.
    inputContexts :: Prelude.Maybe [InputContext],
    -- | Configuration information, if any, to connect to an Amazon Kendra index
    -- with the @AMAZON.KendraSearchIntent@ intent.
    kendraConfiguration :: Prelude.Maybe KendraConfiguration,
    -- | The date that the intent was updated. When you create a resource, the
    -- creation date and the last updated date are the same.
    lastUpdatedDate :: Prelude.Maybe Data.POSIX,
    -- | The name of the intent.
    name :: Prelude.Maybe Prelude.Text,
    -- | An array of @OutputContext@ objects that lists the contexts that the
    -- intent activates when the intent is fulfilled.
    outputContexts :: Prelude.Maybe [OutputContext],
    -- | A unique identifier for a built-in intent.
    parentIntentSignature :: Prelude.Maybe Prelude.Text,
    -- | If the user answers \"no\" to the question defined in
    -- @confirmationPrompt@, Amazon Lex responds with this statement to
    -- acknowledge that the intent was canceled.
    rejectionStatement :: Prelude.Maybe Statement,
    -- | An array of sample utterances configured for the intent.
    sampleUtterances :: Prelude.Maybe [Prelude.Text],
    -- | An array of intent slots configured for the intent.
    slots :: Prelude.Maybe [Slot],
    -- | The version of the intent.
    version :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetIntentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'checksum', 'getIntentResponse_checksum' - Checksum of the intent.
--
-- 'conclusionStatement', 'getIntentResponse_conclusionStatement' - After the Lambda function specified in the @fulfillmentActivity@ element
-- fulfills the intent, Amazon Lex conveys this statement to the user.
--
-- 'confirmationPrompt', 'getIntentResponse_confirmationPrompt' - If defined in the bot, Amazon Lex uses prompt to confirm the intent
-- before fulfilling the user\'s request. For more information, see
-- PutIntent.
--
-- 'createdDate', 'getIntentResponse_createdDate' - The date that the intent was created.
--
-- 'description', 'getIntentResponse_description' - A description of the intent.
--
-- 'dialogCodeHook', 'getIntentResponse_dialogCodeHook' - If defined in the bot, Amazon Amazon Lex invokes this Lambda function
-- for each user input. For more information, see PutIntent.
--
-- 'followUpPrompt', 'getIntentResponse_followUpPrompt' - If defined in the bot, Amazon Lex uses this prompt to solicit additional
-- user activity after the intent is fulfilled. For more information, see
-- PutIntent.
--
-- 'fulfillmentActivity', 'getIntentResponse_fulfillmentActivity' - Describes how the intent is fulfilled. For more information, see
-- PutIntent.
--
-- 'inputContexts', 'getIntentResponse_inputContexts' - An array of @InputContext@ objects that lists the contexts that must be
-- active for Amazon Lex to choose the intent in a conversation with the
-- user.
--
-- 'kendraConfiguration', 'getIntentResponse_kendraConfiguration' - Configuration information, if any, to connect to an Amazon Kendra index
-- with the @AMAZON.KendraSearchIntent@ intent.
--
-- 'lastUpdatedDate', 'getIntentResponse_lastUpdatedDate' - The date that the intent was updated. When you create a resource, the
-- creation date and the last updated date are the same.
--
-- 'name', 'getIntentResponse_name' - The name of the intent.
--
-- 'outputContexts', 'getIntentResponse_outputContexts' - An array of @OutputContext@ objects that lists the contexts that the
-- intent activates when the intent is fulfilled.
--
-- 'parentIntentSignature', 'getIntentResponse_parentIntentSignature' - A unique identifier for a built-in intent.
--
-- 'rejectionStatement', 'getIntentResponse_rejectionStatement' - If the user answers \"no\" to the question defined in
-- @confirmationPrompt@, Amazon Lex responds with this statement to
-- acknowledge that the intent was canceled.
--
-- 'sampleUtterances', 'getIntentResponse_sampleUtterances' - An array of sample utterances configured for the intent.
--
-- 'slots', 'getIntentResponse_slots' - An array of intent slots configured for the intent.
--
-- 'version', 'getIntentResponse_version' - The version of the intent.
--
-- 'httpStatus', 'getIntentResponse_httpStatus' - The response's http status code.
newGetIntentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetIntentResponse
newGetIntentResponse pHttpStatus_ =
  GetIntentResponse'
    { checksum = Prelude.Nothing,
      conclusionStatement = Prelude.Nothing,
      confirmationPrompt = Prelude.Nothing,
      createdDate = Prelude.Nothing,
      description = Prelude.Nothing,
      dialogCodeHook = Prelude.Nothing,
      followUpPrompt = Prelude.Nothing,
      fulfillmentActivity = Prelude.Nothing,
      inputContexts = Prelude.Nothing,
      kendraConfiguration = Prelude.Nothing,
      lastUpdatedDate = Prelude.Nothing,
      name = Prelude.Nothing,
      outputContexts = Prelude.Nothing,
      parentIntentSignature = Prelude.Nothing,
      rejectionStatement = Prelude.Nothing,
      sampleUtterances = Prelude.Nothing,
      slots = Prelude.Nothing,
      version = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Checksum of the intent.
getIntentResponse_checksum :: Lens.Lens' GetIntentResponse (Prelude.Maybe Prelude.Text)
getIntentResponse_checksum = Lens.lens (\GetIntentResponse' {checksum} -> checksum) (\s@GetIntentResponse' {} a -> s {checksum = a} :: GetIntentResponse)

-- | After the Lambda function specified in the @fulfillmentActivity@ element
-- fulfills the intent, Amazon Lex conveys this statement to the user.
getIntentResponse_conclusionStatement :: Lens.Lens' GetIntentResponse (Prelude.Maybe Statement)
getIntentResponse_conclusionStatement = Lens.lens (\GetIntentResponse' {conclusionStatement} -> conclusionStatement) (\s@GetIntentResponse' {} a -> s {conclusionStatement = a} :: GetIntentResponse)

-- | If defined in the bot, Amazon Lex uses prompt to confirm the intent
-- before fulfilling the user\'s request. For more information, see
-- PutIntent.
getIntentResponse_confirmationPrompt :: Lens.Lens' GetIntentResponse (Prelude.Maybe Prompt)
getIntentResponse_confirmationPrompt = Lens.lens (\GetIntentResponse' {confirmationPrompt} -> confirmationPrompt) (\s@GetIntentResponse' {} a -> s {confirmationPrompt = a} :: GetIntentResponse)

-- | The date that the intent was created.
getIntentResponse_createdDate :: Lens.Lens' GetIntentResponse (Prelude.Maybe Prelude.UTCTime)
getIntentResponse_createdDate = Lens.lens (\GetIntentResponse' {createdDate} -> createdDate) (\s@GetIntentResponse' {} a -> s {createdDate = a} :: GetIntentResponse) Prelude.. Lens.mapping Data._Time

-- | A description of the intent.
getIntentResponse_description :: Lens.Lens' GetIntentResponse (Prelude.Maybe Prelude.Text)
getIntentResponse_description = Lens.lens (\GetIntentResponse' {description} -> description) (\s@GetIntentResponse' {} a -> s {description = a} :: GetIntentResponse)

-- | If defined in the bot, Amazon Amazon Lex invokes this Lambda function
-- for each user input. For more information, see PutIntent.
getIntentResponse_dialogCodeHook :: Lens.Lens' GetIntentResponse (Prelude.Maybe CodeHook)
getIntentResponse_dialogCodeHook = Lens.lens (\GetIntentResponse' {dialogCodeHook} -> dialogCodeHook) (\s@GetIntentResponse' {} a -> s {dialogCodeHook = a} :: GetIntentResponse)

-- | If defined in the bot, Amazon Lex uses this prompt to solicit additional
-- user activity after the intent is fulfilled. For more information, see
-- PutIntent.
getIntentResponse_followUpPrompt :: Lens.Lens' GetIntentResponse (Prelude.Maybe FollowUpPrompt)
getIntentResponse_followUpPrompt = Lens.lens (\GetIntentResponse' {followUpPrompt} -> followUpPrompt) (\s@GetIntentResponse' {} a -> s {followUpPrompt = a} :: GetIntentResponse)

-- | Describes how the intent is fulfilled. For more information, see
-- PutIntent.
getIntentResponse_fulfillmentActivity :: Lens.Lens' GetIntentResponse (Prelude.Maybe FulfillmentActivity)
getIntentResponse_fulfillmentActivity = Lens.lens (\GetIntentResponse' {fulfillmentActivity} -> fulfillmentActivity) (\s@GetIntentResponse' {} a -> s {fulfillmentActivity = a} :: GetIntentResponse)

-- | An array of @InputContext@ objects that lists the contexts that must be
-- active for Amazon Lex to choose the intent in a conversation with the
-- user.
getIntentResponse_inputContexts :: Lens.Lens' GetIntentResponse (Prelude.Maybe [InputContext])
getIntentResponse_inputContexts = Lens.lens (\GetIntentResponse' {inputContexts} -> inputContexts) (\s@GetIntentResponse' {} a -> s {inputContexts = a} :: GetIntentResponse) Prelude.. Lens.mapping Lens.coerced

-- | Configuration information, if any, to connect to an Amazon Kendra index
-- with the @AMAZON.KendraSearchIntent@ intent.
getIntentResponse_kendraConfiguration :: Lens.Lens' GetIntentResponse (Prelude.Maybe KendraConfiguration)
getIntentResponse_kendraConfiguration = Lens.lens (\GetIntentResponse' {kendraConfiguration} -> kendraConfiguration) (\s@GetIntentResponse' {} a -> s {kendraConfiguration = a} :: GetIntentResponse)

-- | The date that the intent was updated. When you create a resource, the
-- creation date and the last updated date are the same.
getIntentResponse_lastUpdatedDate :: Lens.Lens' GetIntentResponse (Prelude.Maybe Prelude.UTCTime)
getIntentResponse_lastUpdatedDate = Lens.lens (\GetIntentResponse' {lastUpdatedDate} -> lastUpdatedDate) (\s@GetIntentResponse' {} a -> s {lastUpdatedDate = a} :: GetIntentResponse) Prelude.. Lens.mapping Data._Time

-- | The name of the intent.
getIntentResponse_name :: Lens.Lens' GetIntentResponse (Prelude.Maybe Prelude.Text)
getIntentResponse_name = Lens.lens (\GetIntentResponse' {name} -> name) (\s@GetIntentResponse' {} a -> s {name = a} :: GetIntentResponse)

-- | An array of @OutputContext@ objects that lists the contexts that the
-- intent activates when the intent is fulfilled.
getIntentResponse_outputContexts :: Lens.Lens' GetIntentResponse (Prelude.Maybe [OutputContext])
getIntentResponse_outputContexts = Lens.lens (\GetIntentResponse' {outputContexts} -> outputContexts) (\s@GetIntentResponse' {} a -> s {outputContexts = a} :: GetIntentResponse) Prelude.. Lens.mapping Lens.coerced

-- | A unique identifier for a built-in intent.
getIntentResponse_parentIntentSignature :: Lens.Lens' GetIntentResponse (Prelude.Maybe Prelude.Text)
getIntentResponse_parentIntentSignature = Lens.lens (\GetIntentResponse' {parentIntentSignature} -> parentIntentSignature) (\s@GetIntentResponse' {} a -> s {parentIntentSignature = a} :: GetIntentResponse)

-- | If the user answers \"no\" to the question defined in
-- @confirmationPrompt@, Amazon Lex responds with this statement to
-- acknowledge that the intent was canceled.
getIntentResponse_rejectionStatement :: Lens.Lens' GetIntentResponse (Prelude.Maybe Statement)
getIntentResponse_rejectionStatement = Lens.lens (\GetIntentResponse' {rejectionStatement} -> rejectionStatement) (\s@GetIntentResponse' {} a -> s {rejectionStatement = a} :: GetIntentResponse)

-- | An array of sample utterances configured for the intent.
getIntentResponse_sampleUtterances :: Lens.Lens' GetIntentResponse (Prelude.Maybe [Prelude.Text])
getIntentResponse_sampleUtterances = Lens.lens (\GetIntentResponse' {sampleUtterances} -> sampleUtterances) (\s@GetIntentResponse' {} a -> s {sampleUtterances = a} :: GetIntentResponse) Prelude.. Lens.mapping Lens.coerced

-- | An array of intent slots configured for the intent.
getIntentResponse_slots :: Lens.Lens' GetIntentResponse (Prelude.Maybe [Slot])
getIntentResponse_slots = Lens.lens (\GetIntentResponse' {slots} -> slots) (\s@GetIntentResponse' {} a -> s {slots = a} :: GetIntentResponse) Prelude.. Lens.mapping Lens.coerced

-- | The version of the intent.
getIntentResponse_version :: Lens.Lens' GetIntentResponse (Prelude.Maybe Prelude.Text)
getIntentResponse_version = Lens.lens (\GetIntentResponse' {version} -> version) (\s@GetIntentResponse' {} a -> s {version = a} :: GetIntentResponse)

-- | The response's http status code.
getIntentResponse_httpStatus :: Lens.Lens' GetIntentResponse Prelude.Int
getIntentResponse_httpStatus = Lens.lens (\GetIntentResponse' {httpStatus} -> httpStatus) (\s@GetIntentResponse' {} a -> s {httpStatus = a} :: GetIntentResponse)

instance Prelude.NFData GetIntentResponse where
  rnf GetIntentResponse' {..} =
    Prelude.rnf checksum
      `Prelude.seq` Prelude.rnf conclusionStatement
      `Prelude.seq` Prelude.rnf confirmationPrompt
      `Prelude.seq` Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf dialogCodeHook
      `Prelude.seq` Prelude.rnf followUpPrompt
      `Prelude.seq` Prelude.rnf fulfillmentActivity
      `Prelude.seq` Prelude.rnf inputContexts
      `Prelude.seq` Prelude.rnf kendraConfiguration
      `Prelude.seq` Prelude.rnf lastUpdatedDate
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf outputContexts
      `Prelude.seq` Prelude.rnf parentIntentSignature
      `Prelude.seq` Prelude.rnf rejectionStatement
      `Prelude.seq` Prelude.rnf sampleUtterances
      `Prelude.seq` Prelude.rnf slots
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf httpStatus
