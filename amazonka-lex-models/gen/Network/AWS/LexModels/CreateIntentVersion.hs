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
-- Module      : Network.AWS.LexModels.CreateIntentVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new version of an intent based on the @$LATEST@ version of the
-- intent. If the @$LATEST@ version of this intent hasn\'t changed since
-- you last updated it, Amazon Lex doesn\'t create a new version. It
-- returns the last version you created.
--
-- You can update only the @$LATEST@ version of the intent. You can\'t
-- update the numbered versions that you create with the
-- @CreateIntentVersion@ operation.
--
-- When you create a version of an intent, Amazon Lex sets the version to
-- 1. Subsequent versions increment by 1. For more information, see
-- versioning-intro.
--
-- This operation requires permissions to perform the
-- @lex:CreateIntentVersion@ action.
module Network.AWS.LexModels.CreateIntentVersion
  ( -- * Creating a Request
    CreateIntentVersion (..),
    newCreateIntentVersion,

    -- * Request Lenses
    createIntentVersion_checksum,
    createIntentVersion_name,

    -- * Destructuring the Response
    CreateIntentVersionResponse (..),
    newCreateIntentVersionResponse,

    -- * Response Lenses
    createIntentVersionResponse_kendraConfiguration,
    createIntentVersionResponse_createdDate,
    createIntentVersionResponse_parentIntentSignature,
    createIntentVersionResponse_dialogCodeHook,
    createIntentVersionResponse_conclusionStatement,
    createIntentVersionResponse_lastUpdatedDate,
    createIntentVersionResponse_inputContexts,
    createIntentVersionResponse_version,
    createIntentVersionResponse_rejectionStatement,
    createIntentVersionResponse_name,
    createIntentVersionResponse_slots,
    createIntentVersionResponse_fulfillmentActivity,
    createIntentVersionResponse_sampleUtterances,
    createIntentVersionResponse_description,
    createIntentVersionResponse_confirmationPrompt,
    createIntentVersionResponse_outputContexts,
    createIntentVersionResponse_followUpPrompt,
    createIntentVersionResponse_checksum,
    createIntentVersionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateIntentVersion' smart constructor.
data CreateIntentVersion = CreateIntentVersion'
  { -- | Checksum of the @$LATEST@ version of the intent that should be used to
    -- create the new version. If you specify a checksum and the @$LATEST@
    -- version of the intent has a different checksum, Amazon Lex returns a
    -- @PreconditionFailedException@ exception and doesn\'t publish a new
    -- version. If you don\'t specify a checksum, Amazon Lex publishes the
    -- @$LATEST@ version.
    checksum :: Core.Maybe Core.Text,
    -- | The name of the intent that you want to create a new version of. The
    -- name is case sensitive.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateIntentVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'checksum', 'createIntentVersion_checksum' - Checksum of the @$LATEST@ version of the intent that should be used to
-- create the new version. If you specify a checksum and the @$LATEST@
-- version of the intent has a different checksum, Amazon Lex returns a
-- @PreconditionFailedException@ exception and doesn\'t publish a new
-- version. If you don\'t specify a checksum, Amazon Lex publishes the
-- @$LATEST@ version.
--
-- 'name', 'createIntentVersion_name' - The name of the intent that you want to create a new version of. The
-- name is case sensitive.
newCreateIntentVersion ::
  -- | 'name'
  Core.Text ->
  CreateIntentVersion
newCreateIntentVersion pName_ =
  CreateIntentVersion'
    { checksum = Core.Nothing,
      name = pName_
    }

-- | Checksum of the @$LATEST@ version of the intent that should be used to
-- create the new version. If you specify a checksum and the @$LATEST@
-- version of the intent has a different checksum, Amazon Lex returns a
-- @PreconditionFailedException@ exception and doesn\'t publish a new
-- version. If you don\'t specify a checksum, Amazon Lex publishes the
-- @$LATEST@ version.
createIntentVersion_checksum :: Lens.Lens' CreateIntentVersion (Core.Maybe Core.Text)
createIntentVersion_checksum = Lens.lens (\CreateIntentVersion' {checksum} -> checksum) (\s@CreateIntentVersion' {} a -> s {checksum = a} :: CreateIntentVersion)

-- | The name of the intent that you want to create a new version of. The
-- name is case sensitive.
createIntentVersion_name :: Lens.Lens' CreateIntentVersion Core.Text
createIntentVersion_name = Lens.lens (\CreateIntentVersion' {name} -> name) (\s@CreateIntentVersion' {} a -> s {name = a} :: CreateIntentVersion)

instance Core.AWSRequest CreateIntentVersion where
  type
    AWSResponse CreateIntentVersion =
      CreateIntentVersionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateIntentVersionResponse'
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

instance Core.Hashable CreateIntentVersion

instance Core.NFData CreateIntentVersion

instance Core.ToHeaders CreateIntentVersion where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateIntentVersion where
  toJSON CreateIntentVersion' {..} =
    Core.object
      ( Core.catMaybes
          [("checksum" Core..=) Core.<$> checksum]
      )

instance Core.ToPath CreateIntentVersion where
  toPath CreateIntentVersion' {..} =
    Core.mconcat
      ["/intents/", Core.toBS name, "/versions"]

instance Core.ToQuery CreateIntentVersion where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateIntentVersionResponse' smart constructor.
data CreateIntentVersionResponse = CreateIntentVersionResponse'
  { -- | Configuration information, if any, for connecting an Amazon Kendra index
    -- with the @AMAZON.KendraSearchIntent@ intent.
    kendraConfiguration :: Core.Maybe KendraConfiguration,
    -- | The date that the intent was created.
    createdDate :: Core.Maybe Core.POSIX,
    -- | A unique identifier for a built-in intent.
    parentIntentSignature :: Core.Maybe Core.Text,
    -- | If defined, Amazon Lex invokes this Lambda function for each user input.
    dialogCodeHook :: Core.Maybe CodeHook,
    -- | After the Lambda function specified in the @fulfillmentActivity@ field
    -- fulfills the intent, Amazon Lex conveys this statement to the user.
    conclusionStatement :: Core.Maybe Statement,
    -- | The date that the intent was updated.
    lastUpdatedDate :: Core.Maybe Core.POSIX,
    -- | An array of @InputContext@ objects that lists the contexts that must be
    -- active for Amazon Lex to choose the intent in a conversation with the
    -- user.
    inputContexts :: Core.Maybe [InputContext],
    -- | The version number assigned to the new version of the intent.
    version :: Core.Maybe Core.Text,
    -- | If the user answers \"no\" to the question defined in
    -- @confirmationPrompt@, Amazon Lex responds with this statement to
    -- acknowledge that the intent was canceled.
    rejectionStatement :: Core.Maybe Statement,
    -- | The name of the intent.
    name :: Core.Maybe Core.Text,
    -- | An array of slot types that defines the information required to fulfill
    -- the intent.
    slots :: Core.Maybe [Slot],
    -- | Describes how the intent is fulfilled.
    fulfillmentActivity :: Core.Maybe FulfillmentActivity,
    -- | An array of sample utterances configured for the intent.
    sampleUtterances :: Core.Maybe [Core.Text],
    -- | A description of the intent.
    description :: Core.Maybe Core.Text,
    -- | If defined, the prompt that Amazon Lex uses to confirm the user\'s
    -- intent before fulfilling it.
    confirmationPrompt :: Core.Maybe Prompt,
    -- | An array of @OutputContext@ objects that lists the contexts that the
    -- intent activates when the intent is fulfilled.
    outputContexts :: Core.Maybe [OutputContext],
    -- | If defined, Amazon Lex uses this prompt to solicit additional user
    -- activity after the intent is fulfilled.
    followUpPrompt :: Core.Maybe FollowUpPrompt,
    -- | Checksum of the intent version created.
    checksum :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateIntentVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kendraConfiguration', 'createIntentVersionResponse_kendraConfiguration' - Configuration information, if any, for connecting an Amazon Kendra index
-- with the @AMAZON.KendraSearchIntent@ intent.
--
-- 'createdDate', 'createIntentVersionResponse_createdDate' - The date that the intent was created.
--
-- 'parentIntentSignature', 'createIntentVersionResponse_parentIntentSignature' - A unique identifier for a built-in intent.
--
-- 'dialogCodeHook', 'createIntentVersionResponse_dialogCodeHook' - If defined, Amazon Lex invokes this Lambda function for each user input.
--
-- 'conclusionStatement', 'createIntentVersionResponse_conclusionStatement' - After the Lambda function specified in the @fulfillmentActivity@ field
-- fulfills the intent, Amazon Lex conveys this statement to the user.
--
-- 'lastUpdatedDate', 'createIntentVersionResponse_lastUpdatedDate' - The date that the intent was updated.
--
-- 'inputContexts', 'createIntentVersionResponse_inputContexts' - An array of @InputContext@ objects that lists the contexts that must be
-- active for Amazon Lex to choose the intent in a conversation with the
-- user.
--
-- 'version', 'createIntentVersionResponse_version' - The version number assigned to the new version of the intent.
--
-- 'rejectionStatement', 'createIntentVersionResponse_rejectionStatement' - If the user answers \"no\" to the question defined in
-- @confirmationPrompt@, Amazon Lex responds with this statement to
-- acknowledge that the intent was canceled.
--
-- 'name', 'createIntentVersionResponse_name' - The name of the intent.
--
-- 'slots', 'createIntentVersionResponse_slots' - An array of slot types that defines the information required to fulfill
-- the intent.
--
-- 'fulfillmentActivity', 'createIntentVersionResponse_fulfillmentActivity' - Describes how the intent is fulfilled.
--
-- 'sampleUtterances', 'createIntentVersionResponse_sampleUtterances' - An array of sample utterances configured for the intent.
--
-- 'description', 'createIntentVersionResponse_description' - A description of the intent.
--
-- 'confirmationPrompt', 'createIntentVersionResponse_confirmationPrompt' - If defined, the prompt that Amazon Lex uses to confirm the user\'s
-- intent before fulfilling it.
--
-- 'outputContexts', 'createIntentVersionResponse_outputContexts' - An array of @OutputContext@ objects that lists the contexts that the
-- intent activates when the intent is fulfilled.
--
-- 'followUpPrompt', 'createIntentVersionResponse_followUpPrompt' - If defined, Amazon Lex uses this prompt to solicit additional user
-- activity after the intent is fulfilled.
--
-- 'checksum', 'createIntentVersionResponse_checksum' - Checksum of the intent version created.
--
-- 'httpStatus', 'createIntentVersionResponse_httpStatus' - The response's http status code.
newCreateIntentVersionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateIntentVersionResponse
newCreateIntentVersionResponse pHttpStatus_ =
  CreateIntentVersionResponse'
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

-- | Configuration information, if any, for connecting an Amazon Kendra index
-- with the @AMAZON.KendraSearchIntent@ intent.
createIntentVersionResponse_kendraConfiguration :: Lens.Lens' CreateIntentVersionResponse (Core.Maybe KendraConfiguration)
createIntentVersionResponse_kendraConfiguration = Lens.lens (\CreateIntentVersionResponse' {kendraConfiguration} -> kendraConfiguration) (\s@CreateIntentVersionResponse' {} a -> s {kendraConfiguration = a} :: CreateIntentVersionResponse)

-- | The date that the intent was created.
createIntentVersionResponse_createdDate :: Lens.Lens' CreateIntentVersionResponse (Core.Maybe Core.UTCTime)
createIntentVersionResponse_createdDate = Lens.lens (\CreateIntentVersionResponse' {createdDate} -> createdDate) (\s@CreateIntentVersionResponse' {} a -> s {createdDate = a} :: CreateIntentVersionResponse) Core.. Lens.mapping Core._Time

-- | A unique identifier for a built-in intent.
createIntentVersionResponse_parentIntentSignature :: Lens.Lens' CreateIntentVersionResponse (Core.Maybe Core.Text)
createIntentVersionResponse_parentIntentSignature = Lens.lens (\CreateIntentVersionResponse' {parentIntentSignature} -> parentIntentSignature) (\s@CreateIntentVersionResponse' {} a -> s {parentIntentSignature = a} :: CreateIntentVersionResponse)

-- | If defined, Amazon Lex invokes this Lambda function for each user input.
createIntentVersionResponse_dialogCodeHook :: Lens.Lens' CreateIntentVersionResponse (Core.Maybe CodeHook)
createIntentVersionResponse_dialogCodeHook = Lens.lens (\CreateIntentVersionResponse' {dialogCodeHook} -> dialogCodeHook) (\s@CreateIntentVersionResponse' {} a -> s {dialogCodeHook = a} :: CreateIntentVersionResponse)

-- | After the Lambda function specified in the @fulfillmentActivity@ field
-- fulfills the intent, Amazon Lex conveys this statement to the user.
createIntentVersionResponse_conclusionStatement :: Lens.Lens' CreateIntentVersionResponse (Core.Maybe Statement)
createIntentVersionResponse_conclusionStatement = Lens.lens (\CreateIntentVersionResponse' {conclusionStatement} -> conclusionStatement) (\s@CreateIntentVersionResponse' {} a -> s {conclusionStatement = a} :: CreateIntentVersionResponse)

-- | The date that the intent was updated.
createIntentVersionResponse_lastUpdatedDate :: Lens.Lens' CreateIntentVersionResponse (Core.Maybe Core.UTCTime)
createIntentVersionResponse_lastUpdatedDate = Lens.lens (\CreateIntentVersionResponse' {lastUpdatedDate} -> lastUpdatedDate) (\s@CreateIntentVersionResponse' {} a -> s {lastUpdatedDate = a} :: CreateIntentVersionResponse) Core.. Lens.mapping Core._Time

-- | An array of @InputContext@ objects that lists the contexts that must be
-- active for Amazon Lex to choose the intent in a conversation with the
-- user.
createIntentVersionResponse_inputContexts :: Lens.Lens' CreateIntentVersionResponse (Core.Maybe [InputContext])
createIntentVersionResponse_inputContexts = Lens.lens (\CreateIntentVersionResponse' {inputContexts} -> inputContexts) (\s@CreateIntentVersionResponse' {} a -> s {inputContexts = a} :: CreateIntentVersionResponse) Core.. Lens.mapping Lens._Coerce

-- | The version number assigned to the new version of the intent.
createIntentVersionResponse_version :: Lens.Lens' CreateIntentVersionResponse (Core.Maybe Core.Text)
createIntentVersionResponse_version = Lens.lens (\CreateIntentVersionResponse' {version} -> version) (\s@CreateIntentVersionResponse' {} a -> s {version = a} :: CreateIntentVersionResponse)

-- | If the user answers \"no\" to the question defined in
-- @confirmationPrompt@, Amazon Lex responds with this statement to
-- acknowledge that the intent was canceled.
createIntentVersionResponse_rejectionStatement :: Lens.Lens' CreateIntentVersionResponse (Core.Maybe Statement)
createIntentVersionResponse_rejectionStatement = Lens.lens (\CreateIntentVersionResponse' {rejectionStatement} -> rejectionStatement) (\s@CreateIntentVersionResponse' {} a -> s {rejectionStatement = a} :: CreateIntentVersionResponse)

-- | The name of the intent.
createIntentVersionResponse_name :: Lens.Lens' CreateIntentVersionResponse (Core.Maybe Core.Text)
createIntentVersionResponse_name = Lens.lens (\CreateIntentVersionResponse' {name} -> name) (\s@CreateIntentVersionResponse' {} a -> s {name = a} :: CreateIntentVersionResponse)

-- | An array of slot types that defines the information required to fulfill
-- the intent.
createIntentVersionResponse_slots :: Lens.Lens' CreateIntentVersionResponse (Core.Maybe [Slot])
createIntentVersionResponse_slots = Lens.lens (\CreateIntentVersionResponse' {slots} -> slots) (\s@CreateIntentVersionResponse' {} a -> s {slots = a} :: CreateIntentVersionResponse) Core.. Lens.mapping Lens._Coerce

-- | Describes how the intent is fulfilled.
createIntentVersionResponse_fulfillmentActivity :: Lens.Lens' CreateIntentVersionResponse (Core.Maybe FulfillmentActivity)
createIntentVersionResponse_fulfillmentActivity = Lens.lens (\CreateIntentVersionResponse' {fulfillmentActivity} -> fulfillmentActivity) (\s@CreateIntentVersionResponse' {} a -> s {fulfillmentActivity = a} :: CreateIntentVersionResponse)

-- | An array of sample utterances configured for the intent.
createIntentVersionResponse_sampleUtterances :: Lens.Lens' CreateIntentVersionResponse (Core.Maybe [Core.Text])
createIntentVersionResponse_sampleUtterances = Lens.lens (\CreateIntentVersionResponse' {sampleUtterances} -> sampleUtterances) (\s@CreateIntentVersionResponse' {} a -> s {sampleUtterances = a} :: CreateIntentVersionResponse) Core.. Lens.mapping Lens._Coerce

-- | A description of the intent.
createIntentVersionResponse_description :: Lens.Lens' CreateIntentVersionResponse (Core.Maybe Core.Text)
createIntentVersionResponse_description = Lens.lens (\CreateIntentVersionResponse' {description} -> description) (\s@CreateIntentVersionResponse' {} a -> s {description = a} :: CreateIntentVersionResponse)

-- | If defined, the prompt that Amazon Lex uses to confirm the user\'s
-- intent before fulfilling it.
createIntentVersionResponse_confirmationPrompt :: Lens.Lens' CreateIntentVersionResponse (Core.Maybe Prompt)
createIntentVersionResponse_confirmationPrompt = Lens.lens (\CreateIntentVersionResponse' {confirmationPrompt} -> confirmationPrompt) (\s@CreateIntentVersionResponse' {} a -> s {confirmationPrompt = a} :: CreateIntentVersionResponse)

-- | An array of @OutputContext@ objects that lists the contexts that the
-- intent activates when the intent is fulfilled.
createIntentVersionResponse_outputContexts :: Lens.Lens' CreateIntentVersionResponse (Core.Maybe [OutputContext])
createIntentVersionResponse_outputContexts = Lens.lens (\CreateIntentVersionResponse' {outputContexts} -> outputContexts) (\s@CreateIntentVersionResponse' {} a -> s {outputContexts = a} :: CreateIntentVersionResponse) Core.. Lens.mapping Lens._Coerce

-- | If defined, Amazon Lex uses this prompt to solicit additional user
-- activity after the intent is fulfilled.
createIntentVersionResponse_followUpPrompt :: Lens.Lens' CreateIntentVersionResponse (Core.Maybe FollowUpPrompt)
createIntentVersionResponse_followUpPrompt = Lens.lens (\CreateIntentVersionResponse' {followUpPrompt} -> followUpPrompt) (\s@CreateIntentVersionResponse' {} a -> s {followUpPrompt = a} :: CreateIntentVersionResponse)

-- | Checksum of the intent version created.
createIntentVersionResponse_checksum :: Lens.Lens' CreateIntentVersionResponse (Core.Maybe Core.Text)
createIntentVersionResponse_checksum = Lens.lens (\CreateIntentVersionResponse' {checksum} -> checksum) (\s@CreateIntentVersionResponse' {} a -> s {checksum = a} :: CreateIntentVersionResponse)

-- | The response's http status code.
createIntentVersionResponse_httpStatus :: Lens.Lens' CreateIntentVersionResponse Core.Int
createIntentVersionResponse_httpStatus = Lens.lens (\CreateIntentVersionResponse' {httpStatus} -> httpStatus) (\s@CreateIntentVersionResponse' {} a -> s {httpStatus = a} :: CreateIntentVersionResponse)

instance Core.NFData CreateIntentVersionResponse
