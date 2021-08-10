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
import qualified Network.AWS.Prelude as Prelude
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
    checksum :: Prelude.Maybe Prelude.Text,
    -- | The name of the intent that you want to create a new version of. The
    -- name is case sensitive.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  CreateIntentVersion
newCreateIntentVersion pName_ =
  CreateIntentVersion'
    { checksum = Prelude.Nothing,
      name = pName_
    }

-- | Checksum of the @$LATEST@ version of the intent that should be used to
-- create the new version. If you specify a checksum and the @$LATEST@
-- version of the intent has a different checksum, Amazon Lex returns a
-- @PreconditionFailedException@ exception and doesn\'t publish a new
-- version. If you don\'t specify a checksum, Amazon Lex publishes the
-- @$LATEST@ version.
createIntentVersion_checksum :: Lens.Lens' CreateIntentVersion (Prelude.Maybe Prelude.Text)
createIntentVersion_checksum = Lens.lens (\CreateIntentVersion' {checksum} -> checksum) (\s@CreateIntentVersion' {} a -> s {checksum = a} :: CreateIntentVersion)

-- | The name of the intent that you want to create a new version of. The
-- name is case sensitive.
createIntentVersion_name :: Lens.Lens' CreateIntentVersion Prelude.Text
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
            Prelude.<$> (x Core..?> "kendraConfiguration")
            Prelude.<*> (x Core..?> "createdDate")
            Prelude.<*> (x Core..?> "parentIntentSignature")
            Prelude.<*> (x Core..?> "dialogCodeHook")
            Prelude.<*> (x Core..?> "conclusionStatement")
            Prelude.<*> (x Core..?> "lastUpdatedDate")
            Prelude.<*> (x Core..?> "inputContexts" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "version")
            Prelude.<*> (x Core..?> "rejectionStatement")
            Prelude.<*> (x Core..?> "name")
            Prelude.<*> (x Core..?> "slots" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "fulfillmentActivity")
            Prelude.<*> ( x Core..?> "sampleUtterances"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "description")
            Prelude.<*> (x Core..?> "confirmationPrompt")
            Prelude.<*> (x Core..?> "outputContexts" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "followUpPrompt")
            Prelude.<*> (x Core..?> "checksum")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateIntentVersion

instance Prelude.NFData CreateIntentVersion

instance Core.ToHeaders CreateIntentVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateIntentVersion where
  toJSON CreateIntentVersion' {..} =
    Core.object
      ( Prelude.catMaybes
          [("checksum" Core..=) Prelude.<$> checksum]
      )

instance Core.ToPath CreateIntentVersion where
  toPath CreateIntentVersion' {..} =
    Prelude.mconcat
      ["/intents/", Core.toBS name, "/versions"]

instance Core.ToQuery CreateIntentVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateIntentVersionResponse' smart constructor.
data CreateIntentVersionResponse = CreateIntentVersionResponse'
  { -- | Configuration information, if any, for connecting an Amazon Kendra index
    -- with the @AMAZON.KendraSearchIntent@ intent.
    kendraConfiguration :: Prelude.Maybe KendraConfiguration,
    -- | The date that the intent was created.
    createdDate :: Prelude.Maybe Core.POSIX,
    -- | A unique identifier for a built-in intent.
    parentIntentSignature :: Prelude.Maybe Prelude.Text,
    -- | If defined, Amazon Lex invokes this Lambda function for each user input.
    dialogCodeHook :: Prelude.Maybe CodeHook,
    -- | After the Lambda function specified in the @fulfillmentActivity@ field
    -- fulfills the intent, Amazon Lex conveys this statement to the user.
    conclusionStatement :: Prelude.Maybe Statement,
    -- | The date that the intent was updated.
    lastUpdatedDate :: Prelude.Maybe Core.POSIX,
    -- | An array of @InputContext@ objects that lists the contexts that must be
    -- active for Amazon Lex to choose the intent in a conversation with the
    -- user.
    inputContexts :: Prelude.Maybe [InputContext],
    -- | The version number assigned to the new version of the intent.
    version :: Prelude.Maybe Prelude.Text,
    -- | If the user answers \"no\" to the question defined in
    -- @confirmationPrompt@, Amazon Lex responds with this statement to
    -- acknowledge that the intent was canceled.
    rejectionStatement :: Prelude.Maybe Statement,
    -- | The name of the intent.
    name :: Prelude.Maybe Prelude.Text,
    -- | An array of slot types that defines the information required to fulfill
    -- the intent.
    slots :: Prelude.Maybe [Slot],
    -- | Describes how the intent is fulfilled.
    fulfillmentActivity :: Prelude.Maybe FulfillmentActivity,
    -- | An array of sample utterances configured for the intent.
    sampleUtterances :: Prelude.Maybe [Prelude.Text],
    -- | A description of the intent.
    description :: Prelude.Maybe Prelude.Text,
    -- | If defined, the prompt that Amazon Lex uses to confirm the user\'s
    -- intent before fulfilling it.
    confirmationPrompt :: Prelude.Maybe Prompt,
    -- | An array of @OutputContext@ objects that lists the contexts that the
    -- intent activates when the intent is fulfilled.
    outputContexts :: Prelude.Maybe [OutputContext],
    -- | If defined, Amazon Lex uses this prompt to solicit additional user
    -- activity after the intent is fulfilled.
    followUpPrompt :: Prelude.Maybe FollowUpPrompt,
    -- | Checksum of the intent version created.
    checksum :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  CreateIntentVersionResponse
newCreateIntentVersionResponse pHttpStatus_ =
  CreateIntentVersionResponse'
    { kendraConfiguration =
        Prelude.Nothing,
      createdDate = Prelude.Nothing,
      parentIntentSignature = Prelude.Nothing,
      dialogCodeHook = Prelude.Nothing,
      conclusionStatement = Prelude.Nothing,
      lastUpdatedDate = Prelude.Nothing,
      inputContexts = Prelude.Nothing,
      version = Prelude.Nothing,
      rejectionStatement = Prelude.Nothing,
      name = Prelude.Nothing,
      slots = Prelude.Nothing,
      fulfillmentActivity = Prelude.Nothing,
      sampleUtterances = Prelude.Nothing,
      description = Prelude.Nothing,
      confirmationPrompt = Prelude.Nothing,
      outputContexts = Prelude.Nothing,
      followUpPrompt = Prelude.Nothing,
      checksum = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Configuration information, if any, for connecting an Amazon Kendra index
-- with the @AMAZON.KendraSearchIntent@ intent.
createIntentVersionResponse_kendraConfiguration :: Lens.Lens' CreateIntentVersionResponse (Prelude.Maybe KendraConfiguration)
createIntentVersionResponse_kendraConfiguration = Lens.lens (\CreateIntentVersionResponse' {kendraConfiguration} -> kendraConfiguration) (\s@CreateIntentVersionResponse' {} a -> s {kendraConfiguration = a} :: CreateIntentVersionResponse)

-- | The date that the intent was created.
createIntentVersionResponse_createdDate :: Lens.Lens' CreateIntentVersionResponse (Prelude.Maybe Prelude.UTCTime)
createIntentVersionResponse_createdDate = Lens.lens (\CreateIntentVersionResponse' {createdDate} -> createdDate) (\s@CreateIntentVersionResponse' {} a -> s {createdDate = a} :: CreateIntentVersionResponse) Prelude.. Lens.mapping Core._Time

-- | A unique identifier for a built-in intent.
createIntentVersionResponse_parentIntentSignature :: Lens.Lens' CreateIntentVersionResponse (Prelude.Maybe Prelude.Text)
createIntentVersionResponse_parentIntentSignature = Lens.lens (\CreateIntentVersionResponse' {parentIntentSignature} -> parentIntentSignature) (\s@CreateIntentVersionResponse' {} a -> s {parentIntentSignature = a} :: CreateIntentVersionResponse)

-- | If defined, Amazon Lex invokes this Lambda function for each user input.
createIntentVersionResponse_dialogCodeHook :: Lens.Lens' CreateIntentVersionResponse (Prelude.Maybe CodeHook)
createIntentVersionResponse_dialogCodeHook = Lens.lens (\CreateIntentVersionResponse' {dialogCodeHook} -> dialogCodeHook) (\s@CreateIntentVersionResponse' {} a -> s {dialogCodeHook = a} :: CreateIntentVersionResponse)

-- | After the Lambda function specified in the @fulfillmentActivity@ field
-- fulfills the intent, Amazon Lex conveys this statement to the user.
createIntentVersionResponse_conclusionStatement :: Lens.Lens' CreateIntentVersionResponse (Prelude.Maybe Statement)
createIntentVersionResponse_conclusionStatement = Lens.lens (\CreateIntentVersionResponse' {conclusionStatement} -> conclusionStatement) (\s@CreateIntentVersionResponse' {} a -> s {conclusionStatement = a} :: CreateIntentVersionResponse)

-- | The date that the intent was updated.
createIntentVersionResponse_lastUpdatedDate :: Lens.Lens' CreateIntentVersionResponse (Prelude.Maybe Prelude.UTCTime)
createIntentVersionResponse_lastUpdatedDate = Lens.lens (\CreateIntentVersionResponse' {lastUpdatedDate} -> lastUpdatedDate) (\s@CreateIntentVersionResponse' {} a -> s {lastUpdatedDate = a} :: CreateIntentVersionResponse) Prelude.. Lens.mapping Core._Time

-- | An array of @InputContext@ objects that lists the contexts that must be
-- active for Amazon Lex to choose the intent in a conversation with the
-- user.
createIntentVersionResponse_inputContexts :: Lens.Lens' CreateIntentVersionResponse (Prelude.Maybe [InputContext])
createIntentVersionResponse_inputContexts = Lens.lens (\CreateIntentVersionResponse' {inputContexts} -> inputContexts) (\s@CreateIntentVersionResponse' {} a -> s {inputContexts = a} :: CreateIntentVersionResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The version number assigned to the new version of the intent.
createIntentVersionResponse_version :: Lens.Lens' CreateIntentVersionResponse (Prelude.Maybe Prelude.Text)
createIntentVersionResponse_version = Lens.lens (\CreateIntentVersionResponse' {version} -> version) (\s@CreateIntentVersionResponse' {} a -> s {version = a} :: CreateIntentVersionResponse)

-- | If the user answers \"no\" to the question defined in
-- @confirmationPrompt@, Amazon Lex responds with this statement to
-- acknowledge that the intent was canceled.
createIntentVersionResponse_rejectionStatement :: Lens.Lens' CreateIntentVersionResponse (Prelude.Maybe Statement)
createIntentVersionResponse_rejectionStatement = Lens.lens (\CreateIntentVersionResponse' {rejectionStatement} -> rejectionStatement) (\s@CreateIntentVersionResponse' {} a -> s {rejectionStatement = a} :: CreateIntentVersionResponse)

-- | The name of the intent.
createIntentVersionResponse_name :: Lens.Lens' CreateIntentVersionResponse (Prelude.Maybe Prelude.Text)
createIntentVersionResponse_name = Lens.lens (\CreateIntentVersionResponse' {name} -> name) (\s@CreateIntentVersionResponse' {} a -> s {name = a} :: CreateIntentVersionResponse)

-- | An array of slot types that defines the information required to fulfill
-- the intent.
createIntentVersionResponse_slots :: Lens.Lens' CreateIntentVersionResponse (Prelude.Maybe [Slot])
createIntentVersionResponse_slots = Lens.lens (\CreateIntentVersionResponse' {slots} -> slots) (\s@CreateIntentVersionResponse' {} a -> s {slots = a} :: CreateIntentVersionResponse) Prelude.. Lens.mapping Lens._Coerce

-- | Describes how the intent is fulfilled.
createIntentVersionResponse_fulfillmentActivity :: Lens.Lens' CreateIntentVersionResponse (Prelude.Maybe FulfillmentActivity)
createIntentVersionResponse_fulfillmentActivity = Lens.lens (\CreateIntentVersionResponse' {fulfillmentActivity} -> fulfillmentActivity) (\s@CreateIntentVersionResponse' {} a -> s {fulfillmentActivity = a} :: CreateIntentVersionResponse)

-- | An array of sample utterances configured for the intent.
createIntentVersionResponse_sampleUtterances :: Lens.Lens' CreateIntentVersionResponse (Prelude.Maybe [Prelude.Text])
createIntentVersionResponse_sampleUtterances = Lens.lens (\CreateIntentVersionResponse' {sampleUtterances} -> sampleUtterances) (\s@CreateIntentVersionResponse' {} a -> s {sampleUtterances = a} :: CreateIntentVersionResponse) Prelude.. Lens.mapping Lens._Coerce

-- | A description of the intent.
createIntentVersionResponse_description :: Lens.Lens' CreateIntentVersionResponse (Prelude.Maybe Prelude.Text)
createIntentVersionResponse_description = Lens.lens (\CreateIntentVersionResponse' {description} -> description) (\s@CreateIntentVersionResponse' {} a -> s {description = a} :: CreateIntentVersionResponse)

-- | If defined, the prompt that Amazon Lex uses to confirm the user\'s
-- intent before fulfilling it.
createIntentVersionResponse_confirmationPrompt :: Lens.Lens' CreateIntentVersionResponse (Prelude.Maybe Prompt)
createIntentVersionResponse_confirmationPrompt = Lens.lens (\CreateIntentVersionResponse' {confirmationPrompt} -> confirmationPrompt) (\s@CreateIntentVersionResponse' {} a -> s {confirmationPrompt = a} :: CreateIntentVersionResponse)

-- | An array of @OutputContext@ objects that lists the contexts that the
-- intent activates when the intent is fulfilled.
createIntentVersionResponse_outputContexts :: Lens.Lens' CreateIntentVersionResponse (Prelude.Maybe [OutputContext])
createIntentVersionResponse_outputContexts = Lens.lens (\CreateIntentVersionResponse' {outputContexts} -> outputContexts) (\s@CreateIntentVersionResponse' {} a -> s {outputContexts = a} :: CreateIntentVersionResponse) Prelude.. Lens.mapping Lens._Coerce

-- | If defined, Amazon Lex uses this prompt to solicit additional user
-- activity after the intent is fulfilled.
createIntentVersionResponse_followUpPrompt :: Lens.Lens' CreateIntentVersionResponse (Prelude.Maybe FollowUpPrompt)
createIntentVersionResponse_followUpPrompt = Lens.lens (\CreateIntentVersionResponse' {followUpPrompt} -> followUpPrompt) (\s@CreateIntentVersionResponse' {} a -> s {followUpPrompt = a} :: CreateIntentVersionResponse)

-- | Checksum of the intent version created.
createIntentVersionResponse_checksum :: Lens.Lens' CreateIntentVersionResponse (Prelude.Maybe Prelude.Text)
createIntentVersionResponse_checksum = Lens.lens (\CreateIntentVersionResponse' {checksum} -> checksum) (\s@CreateIntentVersionResponse' {} a -> s {checksum = a} :: CreateIntentVersionResponse)

-- | The response's http status code.
createIntentVersionResponse_httpStatus :: Lens.Lens' CreateIntentVersionResponse Prelude.Int
createIntentVersionResponse_httpStatus = Lens.lens (\CreateIntentVersionResponse' {httpStatus} -> httpStatus) (\s@CreateIntentVersionResponse' {} a -> s {httpStatus = a} :: CreateIntentVersionResponse)

instance Prelude.NFData CreateIntentVersionResponse
