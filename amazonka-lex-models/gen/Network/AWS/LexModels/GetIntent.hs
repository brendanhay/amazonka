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
-- Module      : Network.AWS.LexModels.GetIntent
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.LexModels.GetIntent
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
    getIntentResponse_kendraConfiguration,
    getIntentResponse_createdDate,
    getIntentResponse_parentIntentSignature,
    getIntentResponse_dialogCodeHook,
    getIntentResponse_conclusionStatement,
    getIntentResponse_lastUpdatedDate,
    getIntentResponse_inputContexts,
    getIntentResponse_version,
    getIntentResponse_rejectionStatement,
    getIntentResponse_name,
    getIntentResponse_slots,
    getIntentResponse_fulfillmentActivity,
    getIntentResponse_sampleUtterances,
    getIntentResponse_description,
    getIntentResponse_confirmationPrompt,
    getIntentResponse_outputContexts,
    getIntentResponse_followUpPrompt,
    getIntentResponse_checksum,
    getIntentResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetIntent' smart constructor.
data GetIntent = GetIntent'
  { -- | The name of the intent. The name is case sensitive.
    name :: Core.Text,
    -- | The version of the intent.
    version :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'version'
  Core.Text ->
  GetIntent
newGetIntent pName_ pVersion_ =
  GetIntent' {name = pName_, version = pVersion_}

-- | The name of the intent. The name is case sensitive.
getIntent_name :: Lens.Lens' GetIntent Core.Text
getIntent_name = Lens.lens (\GetIntent' {name} -> name) (\s@GetIntent' {} a -> s {name = a} :: GetIntent)

-- | The version of the intent.
getIntent_version :: Lens.Lens' GetIntent Core.Text
getIntent_version = Lens.lens (\GetIntent' {version} -> version) (\s@GetIntent' {} a -> s {version = a} :: GetIntent)

instance Core.AWSRequest GetIntent where
  type AWSResponse GetIntent = GetIntentResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetIntentResponse'
            Core.<$> (x Core..?> "kendraConfiguration")
            Core.<*> (x Core..?> "createdDate")
            Core.<*> (x Core..?> "parentIntentSignature")
            Core.<*> (x Core..?> "dialogCodeHook")
            Core.<*> (x Core..?> "conclusionStatement")
            Core.<*> (x Core..?> "lastUpdatedDate")
            Core.<*> (x Core..?> "inputContexts" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "version")
            Core.<*> (x Core..?> "rejectionStatement")
            Core.<*> (x Core..?> "name")
            Core.<*> (x Core..?> "slots" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "fulfillmentActivity")
            Core.<*> (x Core..?> "sampleUtterances" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "description")
            Core.<*> (x Core..?> "confirmationPrompt")
            Core.<*> (x Core..?> "outputContexts" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "followUpPrompt")
            Core.<*> (x Core..?> "checksum")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetIntent

instance Core.NFData GetIntent

instance Core.ToHeaders GetIntent where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetIntent where
  toPath GetIntent' {..} =
    Core.mconcat
      [ "/intents/",
        Core.toBS name,
        "/versions/",
        Core.toBS version
      ]

instance Core.ToQuery GetIntent where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetIntentResponse' smart constructor.
data GetIntentResponse = GetIntentResponse'
  { -- | Configuration information, if any, to connect to an Amazon Kendra index
    -- with the @AMAZON.KendraSearchIntent@ intent.
    kendraConfiguration :: Core.Maybe KendraConfiguration,
    -- | The date that the intent was created.
    createdDate :: Core.Maybe Core.POSIX,
    -- | A unique identifier for a built-in intent.
    parentIntentSignature :: Core.Maybe Core.Text,
    -- | If defined in the bot, Amazon Amazon Lex invokes this Lambda function
    -- for each user input. For more information, see PutIntent.
    dialogCodeHook :: Core.Maybe CodeHook,
    -- | After the Lambda function specified in the @fulfillmentActivity@ element
    -- fulfills the intent, Amazon Lex conveys this statement to the user.
    conclusionStatement :: Core.Maybe Statement,
    -- | The date that the intent was updated. When you create a resource, the
    -- creation date and the last updated date are the same.
    lastUpdatedDate :: Core.Maybe Core.POSIX,
    -- | An array of @InputContext@ objects that lists the contexts that must be
    -- active for Amazon Lex to choose the intent in a conversation with the
    -- user.
    inputContexts :: Core.Maybe [InputContext],
    -- | The version of the intent.
    version :: Core.Maybe Core.Text,
    -- | If the user answers \"no\" to the question defined in
    -- @confirmationPrompt@, Amazon Lex responds with this statement to
    -- acknowledge that the intent was canceled.
    rejectionStatement :: Core.Maybe Statement,
    -- | The name of the intent.
    name :: Core.Maybe Core.Text,
    -- | An array of intent slots configured for the intent.
    slots :: Core.Maybe [Slot],
    -- | Describes how the intent is fulfilled. For more information, see
    -- PutIntent.
    fulfillmentActivity :: Core.Maybe FulfillmentActivity,
    -- | An array of sample utterances configured for the intent.
    sampleUtterances :: Core.Maybe [Core.Text],
    -- | A description of the intent.
    description :: Core.Maybe Core.Text,
    -- | If defined in the bot, Amazon Lex uses prompt to confirm the intent
    -- before fulfilling the user\'s request. For more information, see
    -- PutIntent.
    confirmationPrompt :: Core.Maybe Prompt,
    -- | An array of @OutputContext@ objects that lists the contexts that the
    -- intent activates when the intent is fulfilled.
    outputContexts :: Core.Maybe [OutputContext],
    -- | If defined in the bot, Amazon Lex uses this prompt to solicit additional
    -- user activity after the intent is fulfilled. For more information, see
    -- PutIntent.
    followUpPrompt :: Core.Maybe FollowUpPrompt,
    -- | Checksum of the intent.
    checksum :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetIntentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kendraConfiguration', 'getIntentResponse_kendraConfiguration' - Configuration information, if any, to connect to an Amazon Kendra index
-- with the @AMAZON.KendraSearchIntent@ intent.
--
-- 'createdDate', 'getIntentResponse_createdDate' - The date that the intent was created.
--
-- 'parentIntentSignature', 'getIntentResponse_parentIntentSignature' - A unique identifier for a built-in intent.
--
-- 'dialogCodeHook', 'getIntentResponse_dialogCodeHook' - If defined in the bot, Amazon Amazon Lex invokes this Lambda function
-- for each user input. For more information, see PutIntent.
--
-- 'conclusionStatement', 'getIntentResponse_conclusionStatement' - After the Lambda function specified in the @fulfillmentActivity@ element
-- fulfills the intent, Amazon Lex conveys this statement to the user.
--
-- 'lastUpdatedDate', 'getIntentResponse_lastUpdatedDate' - The date that the intent was updated. When you create a resource, the
-- creation date and the last updated date are the same.
--
-- 'inputContexts', 'getIntentResponse_inputContexts' - An array of @InputContext@ objects that lists the contexts that must be
-- active for Amazon Lex to choose the intent in a conversation with the
-- user.
--
-- 'version', 'getIntentResponse_version' - The version of the intent.
--
-- 'rejectionStatement', 'getIntentResponse_rejectionStatement' - If the user answers \"no\" to the question defined in
-- @confirmationPrompt@, Amazon Lex responds with this statement to
-- acknowledge that the intent was canceled.
--
-- 'name', 'getIntentResponse_name' - The name of the intent.
--
-- 'slots', 'getIntentResponse_slots' - An array of intent slots configured for the intent.
--
-- 'fulfillmentActivity', 'getIntentResponse_fulfillmentActivity' - Describes how the intent is fulfilled. For more information, see
-- PutIntent.
--
-- 'sampleUtterances', 'getIntentResponse_sampleUtterances' - An array of sample utterances configured for the intent.
--
-- 'description', 'getIntentResponse_description' - A description of the intent.
--
-- 'confirmationPrompt', 'getIntentResponse_confirmationPrompt' - If defined in the bot, Amazon Lex uses prompt to confirm the intent
-- before fulfilling the user\'s request. For more information, see
-- PutIntent.
--
-- 'outputContexts', 'getIntentResponse_outputContexts' - An array of @OutputContext@ objects that lists the contexts that the
-- intent activates when the intent is fulfilled.
--
-- 'followUpPrompt', 'getIntentResponse_followUpPrompt' - If defined in the bot, Amazon Lex uses this prompt to solicit additional
-- user activity after the intent is fulfilled. For more information, see
-- PutIntent.
--
-- 'checksum', 'getIntentResponse_checksum' - Checksum of the intent.
--
-- 'httpStatus', 'getIntentResponse_httpStatus' - The response's http status code.
newGetIntentResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetIntentResponse
newGetIntentResponse pHttpStatus_ =
  GetIntentResponse'
    { kendraConfiguration =
        Core.Nothing,
      createdDate = Core.Nothing,
      parentIntentSignature = Core.Nothing,
      dialogCodeHook = Core.Nothing,
      conclusionStatement = Core.Nothing,
      lastUpdatedDate = Core.Nothing,
      inputContexts = Core.Nothing,
      version = Core.Nothing,
      rejectionStatement = Core.Nothing,
      name = Core.Nothing,
      slots = Core.Nothing,
      fulfillmentActivity = Core.Nothing,
      sampleUtterances = Core.Nothing,
      description = Core.Nothing,
      confirmationPrompt = Core.Nothing,
      outputContexts = Core.Nothing,
      followUpPrompt = Core.Nothing,
      checksum = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Configuration information, if any, to connect to an Amazon Kendra index
-- with the @AMAZON.KendraSearchIntent@ intent.
getIntentResponse_kendraConfiguration :: Lens.Lens' GetIntentResponse (Core.Maybe KendraConfiguration)
getIntentResponse_kendraConfiguration = Lens.lens (\GetIntentResponse' {kendraConfiguration} -> kendraConfiguration) (\s@GetIntentResponse' {} a -> s {kendraConfiguration = a} :: GetIntentResponse)

-- | The date that the intent was created.
getIntentResponse_createdDate :: Lens.Lens' GetIntentResponse (Core.Maybe Core.UTCTime)
getIntentResponse_createdDate = Lens.lens (\GetIntentResponse' {createdDate} -> createdDate) (\s@GetIntentResponse' {} a -> s {createdDate = a} :: GetIntentResponse) Core.. Lens.mapping Core._Time

-- | A unique identifier for a built-in intent.
getIntentResponse_parentIntentSignature :: Lens.Lens' GetIntentResponse (Core.Maybe Core.Text)
getIntentResponse_parentIntentSignature = Lens.lens (\GetIntentResponse' {parentIntentSignature} -> parentIntentSignature) (\s@GetIntentResponse' {} a -> s {parentIntentSignature = a} :: GetIntentResponse)

-- | If defined in the bot, Amazon Amazon Lex invokes this Lambda function
-- for each user input. For more information, see PutIntent.
getIntentResponse_dialogCodeHook :: Lens.Lens' GetIntentResponse (Core.Maybe CodeHook)
getIntentResponse_dialogCodeHook = Lens.lens (\GetIntentResponse' {dialogCodeHook} -> dialogCodeHook) (\s@GetIntentResponse' {} a -> s {dialogCodeHook = a} :: GetIntentResponse)

-- | After the Lambda function specified in the @fulfillmentActivity@ element
-- fulfills the intent, Amazon Lex conveys this statement to the user.
getIntentResponse_conclusionStatement :: Lens.Lens' GetIntentResponse (Core.Maybe Statement)
getIntentResponse_conclusionStatement = Lens.lens (\GetIntentResponse' {conclusionStatement} -> conclusionStatement) (\s@GetIntentResponse' {} a -> s {conclusionStatement = a} :: GetIntentResponse)

-- | The date that the intent was updated. When you create a resource, the
-- creation date and the last updated date are the same.
getIntentResponse_lastUpdatedDate :: Lens.Lens' GetIntentResponse (Core.Maybe Core.UTCTime)
getIntentResponse_lastUpdatedDate = Lens.lens (\GetIntentResponse' {lastUpdatedDate} -> lastUpdatedDate) (\s@GetIntentResponse' {} a -> s {lastUpdatedDate = a} :: GetIntentResponse) Core.. Lens.mapping Core._Time

-- | An array of @InputContext@ objects that lists the contexts that must be
-- active for Amazon Lex to choose the intent in a conversation with the
-- user.
getIntentResponse_inputContexts :: Lens.Lens' GetIntentResponse (Core.Maybe [InputContext])
getIntentResponse_inputContexts = Lens.lens (\GetIntentResponse' {inputContexts} -> inputContexts) (\s@GetIntentResponse' {} a -> s {inputContexts = a} :: GetIntentResponse) Core.. Lens.mapping Lens._Coerce

-- | The version of the intent.
getIntentResponse_version :: Lens.Lens' GetIntentResponse (Core.Maybe Core.Text)
getIntentResponse_version = Lens.lens (\GetIntentResponse' {version} -> version) (\s@GetIntentResponse' {} a -> s {version = a} :: GetIntentResponse)

-- | If the user answers \"no\" to the question defined in
-- @confirmationPrompt@, Amazon Lex responds with this statement to
-- acknowledge that the intent was canceled.
getIntentResponse_rejectionStatement :: Lens.Lens' GetIntentResponse (Core.Maybe Statement)
getIntentResponse_rejectionStatement = Lens.lens (\GetIntentResponse' {rejectionStatement} -> rejectionStatement) (\s@GetIntentResponse' {} a -> s {rejectionStatement = a} :: GetIntentResponse)

-- | The name of the intent.
getIntentResponse_name :: Lens.Lens' GetIntentResponse (Core.Maybe Core.Text)
getIntentResponse_name = Lens.lens (\GetIntentResponse' {name} -> name) (\s@GetIntentResponse' {} a -> s {name = a} :: GetIntentResponse)

-- | An array of intent slots configured for the intent.
getIntentResponse_slots :: Lens.Lens' GetIntentResponse (Core.Maybe [Slot])
getIntentResponse_slots = Lens.lens (\GetIntentResponse' {slots} -> slots) (\s@GetIntentResponse' {} a -> s {slots = a} :: GetIntentResponse) Core.. Lens.mapping Lens._Coerce

-- | Describes how the intent is fulfilled. For more information, see
-- PutIntent.
getIntentResponse_fulfillmentActivity :: Lens.Lens' GetIntentResponse (Core.Maybe FulfillmentActivity)
getIntentResponse_fulfillmentActivity = Lens.lens (\GetIntentResponse' {fulfillmentActivity} -> fulfillmentActivity) (\s@GetIntentResponse' {} a -> s {fulfillmentActivity = a} :: GetIntentResponse)

-- | An array of sample utterances configured for the intent.
getIntentResponse_sampleUtterances :: Lens.Lens' GetIntentResponse (Core.Maybe [Core.Text])
getIntentResponse_sampleUtterances = Lens.lens (\GetIntentResponse' {sampleUtterances} -> sampleUtterances) (\s@GetIntentResponse' {} a -> s {sampleUtterances = a} :: GetIntentResponse) Core.. Lens.mapping Lens._Coerce

-- | A description of the intent.
getIntentResponse_description :: Lens.Lens' GetIntentResponse (Core.Maybe Core.Text)
getIntentResponse_description = Lens.lens (\GetIntentResponse' {description} -> description) (\s@GetIntentResponse' {} a -> s {description = a} :: GetIntentResponse)

-- | If defined in the bot, Amazon Lex uses prompt to confirm the intent
-- before fulfilling the user\'s request. For more information, see
-- PutIntent.
getIntentResponse_confirmationPrompt :: Lens.Lens' GetIntentResponse (Core.Maybe Prompt)
getIntentResponse_confirmationPrompt = Lens.lens (\GetIntentResponse' {confirmationPrompt} -> confirmationPrompt) (\s@GetIntentResponse' {} a -> s {confirmationPrompt = a} :: GetIntentResponse)

-- | An array of @OutputContext@ objects that lists the contexts that the
-- intent activates when the intent is fulfilled.
getIntentResponse_outputContexts :: Lens.Lens' GetIntentResponse (Core.Maybe [OutputContext])
getIntentResponse_outputContexts = Lens.lens (\GetIntentResponse' {outputContexts} -> outputContexts) (\s@GetIntentResponse' {} a -> s {outputContexts = a} :: GetIntentResponse) Core.. Lens.mapping Lens._Coerce

-- | If defined in the bot, Amazon Lex uses this prompt to solicit additional
-- user activity after the intent is fulfilled. For more information, see
-- PutIntent.
getIntentResponse_followUpPrompt :: Lens.Lens' GetIntentResponse (Core.Maybe FollowUpPrompt)
getIntentResponse_followUpPrompt = Lens.lens (\GetIntentResponse' {followUpPrompt} -> followUpPrompt) (\s@GetIntentResponse' {} a -> s {followUpPrompt = a} :: GetIntentResponse)

-- | Checksum of the intent.
getIntentResponse_checksum :: Lens.Lens' GetIntentResponse (Core.Maybe Core.Text)
getIntentResponse_checksum = Lens.lens (\GetIntentResponse' {checksum} -> checksum) (\s@GetIntentResponse' {} a -> s {checksum = a} :: GetIntentResponse)

-- | The response's http status code.
getIntentResponse_httpStatus :: Lens.Lens' GetIntentResponse Core.Int
getIntentResponse_httpStatus = Lens.lens (\GetIntentResponse' {httpStatus} -> httpStatus) (\s@GetIntentResponse' {} a -> s {httpStatus = a} :: GetIntentResponse)

instance Core.NFData GetIntentResponse
