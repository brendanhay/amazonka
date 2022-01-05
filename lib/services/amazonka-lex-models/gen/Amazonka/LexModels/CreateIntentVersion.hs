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
-- Module      : Amazonka.LexModels.CreateIntentVersion
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
module Amazonka.LexModels.CreateIntentVersion
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
    createIntentVersionResponse_fulfillmentActivity,
    createIntentVersionResponse_slots,
    createIntentVersionResponse_rejectionStatement,
    createIntentVersionResponse_checksum,
    createIntentVersionResponse_conclusionStatement,
    createIntentVersionResponse_sampleUtterances,
    createIntentVersionResponse_parentIntentSignature,
    createIntentVersionResponse_createdDate,
    createIntentVersionResponse_kendraConfiguration,
    createIntentVersionResponse_name,
    createIntentVersionResponse_version,
    createIntentVersionResponse_inputContexts,
    createIntentVersionResponse_followUpPrompt,
    createIntentVersionResponse_lastUpdatedDate,
    createIntentVersionResponse_outputContexts,
    createIntentVersionResponse_confirmationPrompt,
    createIntentVersionResponse_dialogCodeHook,
    createIntentVersionResponse_description,
    createIntentVersionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.LexModels.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
            Prelude.<$> (x Core..?> "fulfillmentActivity")
            Prelude.<*> (x Core..?> "slots" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "rejectionStatement")
            Prelude.<*> (x Core..?> "checksum")
            Prelude.<*> (x Core..?> "conclusionStatement")
            Prelude.<*> ( x Core..?> "sampleUtterances"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "parentIntentSignature")
            Prelude.<*> (x Core..?> "createdDate")
            Prelude.<*> (x Core..?> "kendraConfiguration")
            Prelude.<*> (x Core..?> "name")
            Prelude.<*> (x Core..?> "version")
            Prelude.<*> (x Core..?> "inputContexts" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "followUpPrompt")
            Prelude.<*> (x Core..?> "lastUpdatedDate")
            Prelude.<*> (x Core..?> "outputContexts" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "confirmationPrompt")
            Prelude.<*> (x Core..?> "dialogCodeHook")
            Prelude.<*> (x Core..?> "description")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateIntentVersion where
  hashWithSalt _salt CreateIntentVersion' {..} =
    _salt `Prelude.hashWithSalt` checksum
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateIntentVersion where
  rnf CreateIntentVersion' {..} =
    Prelude.rnf checksum `Prelude.seq` Prelude.rnf name

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
  { -- | Describes how the intent is fulfilled.
    fulfillmentActivity :: Prelude.Maybe FulfillmentActivity,
    -- | An array of slot types that defines the information required to fulfill
    -- the intent.
    slots :: Prelude.Maybe [Slot],
    -- | If the user answers \"no\" to the question defined in
    -- @confirmationPrompt@, Amazon Lex responds with this statement to
    -- acknowledge that the intent was canceled.
    rejectionStatement :: Prelude.Maybe Statement,
    -- | Checksum of the intent version created.
    checksum :: Prelude.Maybe Prelude.Text,
    -- | After the Lambda function specified in the @fulfillmentActivity@ field
    -- fulfills the intent, Amazon Lex conveys this statement to the user.
    conclusionStatement :: Prelude.Maybe Statement,
    -- | An array of sample utterances configured for the intent.
    sampleUtterances :: Prelude.Maybe [Prelude.Text],
    -- | A unique identifier for a built-in intent.
    parentIntentSignature :: Prelude.Maybe Prelude.Text,
    -- | The date that the intent was created.
    createdDate :: Prelude.Maybe Core.POSIX,
    -- | Configuration information, if any, for connecting an Amazon Kendra index
    -- with the @AMAZON.KendraSearchIntent@ intent.
    kendraConfiguration :: Prelude.Maybe KendraConfiguration,
    -- | The name of the intent.
    name :: Prelude.Maybe Prelude.Text,
    -- | The version number assigned to the new version of the intent.
    version :: Prelude.Maybe Prelude.Text,
    -- | An array of @InputContext@ objects that lists the contexts that must be
    -- active for Amazon Lex to choose the intent in a conversation with the
    -- user.
    inputContexts :: Prelude.Maybe [InputContext],
    -- | If defined, Amazon Lex uses this prompt to solicit additional user
    -- activity after the intent is fulfilled.
    followUpPrompt :: Prelude.Maybe FollowUpPrompt,
    -- | The date that the intent was updated.
    lastUpdatedDate :: Prelude.Maybe Core.POSIX,
    -- | An array of @OutputContext@ objects that lists the contexts that the
    -- intent activates when the intent is fulfilled.
    outputContexts :: Prelude.Maybe [OutputContext],
    -- | If defined, the prompt that Amazon Lex uses to confirm the user\'s
    -- intent before fulfilling it.
    confirmationPrompt :: Prelude.Maybe Prompt,
    -- | If defined, Amazon Lex invokes this Lambda function for each user input.
    dialogCodeHook :: Prelude.Maybe CodeHook,
    -- | A description of the intent.
    description :: Prelude.Maybe Prelude.Text,
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
-- 'fulfillmentActivity', 'createIntentVersionResponse_fulfillmentActivity' - Describes how the intent is fulfilled.
--
-- 'slots', 'createIntentVersionResponse_slots' - An array of slot types that defines the information required to fulfill
-- the intent.
--
-- 'rejectionStatement', 'createIntentVersionResponse_rejectionStatement' - If the user answers \"no\" to the question defined in
-- @confirmationPrompt@, Amazon Lex responds with this statement to
-- acknowledge that the intent was canceled.
--
-- 'checksum', 'createIntentVersionResponse_checksum' - Checksum of the intent version created.
--
-- 'conclusionStatement', 'createIntentVersionResponse_conclusionStatement' - After the Lambda function specified in the @fulfillmentActivity@ field
-- fulfills the intent, Amazon Lex conveys this statement to the user.
--
-- 'sampleUtterances', 'createIntentVersionResponse_sampleUtterances' - An array of sample utterances configured for the intent.
--
-- 'parentIntentSignature', 'createIntentVersionResponse_parentIntentSignature' - A unique identifier for a built-in intent.
--
-- 'createdDate', 'createIntentVersionResponse_createdDate' - The date that the intent was created.
--
-- 'kendraConfiguration', 'createIntentVersionResponse_kendraConfiguration' - Configuration information, if any, for connecting an Amazon Kendra index
-- with the @AMAZON.KendraSearchIntent@ intent.
--
-- 'name', 'createIntentVersionResponse_name' - The name of the intent.
--
-- 'version', 'createIntentVersionResponse_version' - The version number assigned to the new version of the intent.
--
-- 'inputContexts', 'createIntentVersionResponse_inputContexts' - An array of @InputContext@ objects that lists the contexts that must be
-- active for Amazon Lex to choose the intent in a conversation with the
-- user.
--
-- 'followUpPrompt', 'createIntentVersionResponse_followUpPrompt' - If defined, Amazon Lex uses this prompt to solicit additional user
-- activity after the intent is fulfilled.
--
-- 'lastUpdatedDate', 'createIntentVersionResponse_lastUpdatedDate' - The date that the intent was updated.
--
-- 'outputContexts', 'createIntentVersionResponse_outputContexts' - An array of @OutputContext@ objects that lists the contexts that the
-- intent activates when the intent is fulfilled.
--
-- 'confirmationPrompt', 'createIntentVersionResponse_confirmationPrompt' - If defined, the prompt that Amazon Lex uses to confirm the user\'s
-- intent before fulfilling it.
--
-- 'dialogCodeHook', 'createIntentVersionResponse_dialogCodeHook' - If defined, Amazon Lex invokes this Lambda function for each user input.
--
-- 'description', 'createIntentVersionResponse_description' - A description of the intent.
--
-- 'httpStatus', 'createIntentVersionResponse_httpStatus' - The response's http status code.
newCreateIntentVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateIntentVersionResponse
newCreateIntentVersionResponse pHttpStatus_ =
  CreateIntentVersionResponse'
    { fulfillmentActivity =
        Prelude.Nothing,
      slots = Prelude.Nothing,
      rejectionStatement = Prelude.Nothing,
      checksum = Prelude.Nothing,
      conclusionStatement = Prelude.Nothing,
      sampleUtterances = Prelude.Nothing,
      parentIntentSignature = Prelude.Nothing,
      createdDate = Prelude.Nothing,
      kendraConfiguration = Prelude.Nothing,
      name = Prelude.Nothing,
      version = Prelude.Nothing,
      inputContexts = Prelude.Nothing,
      followUpPrompt = Prelude.Nothing,
      lastUpdatedDate = Prelude.Nothing,
      outputContexts = Prelude.Nothing,
      confirmationPrompt = Prelude.Nothing,
      dialogCodeHook = Prelude.Nothing,
      description = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Describes how the intent is fulfilled.
createIntentVersionResponse_fulfillmentActivity :: Lens.Lens' CreateIntentVersionResponse (Prelude.Maybe FulfillmentActivity)
createIntentVersionResponse_fulfillmentActivity = Lens.lens (\CreateIntentVersionResponse' {fulfillmentActivity} -> fulfillmentActivity) (\s@CreateIntentVersionResponse' {} a -> s {fulfillmentActivity = a} :: CreateIntentVersionResponse)

-- | An array of slot types that defines the information required to fulfill
-- the intent.
createIntentVersionResponse_slots :: Lens.Lens' CreateIntentVersionResponse (Prelude.Maybe [Slot])
createIntentVersionResponse_slots = Lens.lens (\CreateIntentVersionResponse' {slots} -> slots) (\s@CreateIntentVersionResponse' {} a -> s {slots = a} :: CreateIntentVersionResponse) Prelude.. Lens.mapping Lens.coerced

-- | If the user answers \"no\" to the question defined in
-- @confirmationPrompt@, Amazon Lex responds with this statement to
-- acknowledge that the intent was canceled.
createIntentVersionResponse_rejectionStatement :: Lens.Lens' CreateIntentVersionResponse (Prelude.Maybe Statement)
createIntentVersionResponse_rejectionStatement = Lens.lens (\CreateIntentVersionResponse' {rejectionStatement} -> rejectionStatement) (\s@CreateIntentVersionResponse' {} a -> s {rejectionStatement = a} :: CreateIntentVersionResponse)

-- | Checksum of the intent version created.
createIntentVersionResponse_checksum :: Lens.Lens' CreateIntentVersionResponse (Prelude.Maybe Prelude.Text)
createIntentVersionResponse_checksum = Lens.lens (\CreateIntentVersionResponse' {checksum} -> checksum) (\s@CreateIntentVersionResponse' {} a -> s {checksum = a} :: CreateIntentVersionResponse)

-- | After the Lambda function specified in the @fulfillmentActivity@ field
-- fulfills the intent, Amazon Lex conveys this statement to the user.
createIntentVersionResponse_conclusionStatement :: Lens.Lens' CreateIntentVersionResponse (Prelude.Maybe Statement)
createIntentVersionResponse_conclusionStatement = Lens.lens (\CreateIntentVersionResponse' {conclusionStatement} -> conclusionStatement) (\s@CreateIntentVersionResponse' {} a -> s {conclusionStatement = a} :: CreateIntentVersionResponse)

-- | An array of sample utterances configured for the intent.
createIntentVersionResponse_sampleUtterances :: Lens.Lens' CreateIntentVersionResponse (Prelude.Maybe [Prelude.Text])
createIntentVersionResponse_sampleUtterances = Lens.lens (\CreateIntentVersionResponse' {sampleUtterances} -> sampleUtterances) (\s@CreateIntentVersionResponse' {} a -> s {sampleUtterances = a} :: CreateIntentVersionResponse) Prelude.. Lens.mapping Lens.coerced

-- | A unique identifier for a built-in intent.
createIntentVersionResponse_parentIntentSignature :: Lens.Lens' CreateIntentVersionResponse (Prelude.Maybe Prelude.Text)
createIntentVersionResponse_parentIntentSignature = Lens.lens (\CreateIntentVersionResponse' {parentIntentSignature} -> parentIntentSignature) (\s@CreateIntentVersionResponse' {} a -> s {parentIntentSignature = a} :: CreateIntentVersionResponse)

-- | The date that the intent was created.
createIntentVersionResponse_createdDate :: Lens.Lens' CreateIntentVersionResponse (Prelude.Maybe Prelude.UTCTime)
createIntentVersionResponse_createdDate = Lens.lens (\CreateIntentVersionResponse' {createdDate} -> createdDate) (\s@CreateIntentVersionResponse' {} a -> s {createdDate = a} :: CreateIntentVersionResponse) Prelude.. Lens.mapping Core._Time

-- | Configuration information, if any, for connecting an Amazon Kendra index
-- with the @AMAZON.KendraSearchIntent@ intent.
createIntentVersionResponse_kendraConfiguration :: Lens.Lens' CreateIntentVersionResponse (Prelude.Maybe KendraConfiguration)
createIntentVersionResponse_kendraConfiguration = Lens.lens (\CreateIntentVersionResponse' {kendraConfiguration} -> kendraConfiguration) (\s@CreateIntentVersionResponse' {} a -> s {kendraConfiguration = a} :: CreateIntentVersionResponse)

-- | The name of the intent.
createIntentVersionResponse_name :: Lens.Lens' CreateIntentVersionResponse (Prelude.Maybe Prelude.Text)
createIntentVersionResponse_name = Lens.lens (\CreateIntentVersionResponse' {name} -> name) (\s@CreateIntentVersionResponse' {} a -> s {name = a} :: CreateIntentVersionResponse)

-- | The version number assigned to the new version of the intent.
createIntentVersionResponse_version :: Lens.Lens' CreateIntentVersionResponse (Prelude.Maybe Prelude.Text)
createIntentVersionResponse_version = Lens.lens (\CreateIntentVersionResponse' {version} -> version) (\s@CreateIntentVersionResponse' {} a -> s {version = a} :: CreateIntentVersionResponse)

-- | An array of @InputContext@ objects that lists the contexts that must be
-- active for Amazon Lex to choose the intent in a conversation with the
-- user.
createIntentVersionResponse_inputContexts :: Lens.Lens' CreateIntentVersionResponse (Prelude.Maybe [InputContext])
createIntentVersionResponse_inputContexts = Lens.lens (\CreateIntentVersionResponse' {inputContexts} -> inputContexts) (\s@CreateIntentVersionResponse' {} a -> s {inputContexts = a} :: CreateIntentVersionResponse) Prelude.. Lens.mapping Lens.coerced

-- | If defined, Amazon Lex uses this prompt to solicit additional user
-- activity after the intent is fulfilled.
createIntentVersionResponse_followUpPrompt :: Lens.Lens' CreateIntentVersionResponse (Prelude.Maybe FollowUpPrompt)
createIntentVersionResponse_followUpPrompt = Lens.lens (\CreateIntentVersionResponse' {followUpPrompt} -> followUpPrompt) (\s@CreateIntentVersionResponse' {} a -> s {followUpPrompt = a} :: CreateIntentVersionResponse)

-- | The date that the intent was updated.
createIntentVersionResponse_lastUpdatedDate :: Lens.Lens' CreateIntentVersionResponse (Prelude.Maybe Prelude.UTCTime)
createIntentVersionResponse_lastUpdatedDate = Lens.lens (\CreateIntentVersionResponse' {lastUpdatedDate} -> lastUpdatedDate) (\s@CreateIntentVersionResponse' {} a -> s {lastUpdatedDate = a} :: CreateIntentVersionResponse) Prelude.. Lens.mapping Core._Time

-- | An array of @OutputContext@ objects that lists the contexts that the
-- intent activates when the intent is fulfilled.
createIntentVersionResponse_outputContexts :: Lens.Lens' CreateIntentVersionResponse (Prelude.Maybe [OutputContext])
createIntentVersionResponse_outputContexts = Lens.lens (\CreateIntentVersionResponse' {outputContexts} -> outputContexts) (\s@CreateIntentVersionResponse' {} a -> s {outputContexts = a} :: CreateIntentVersionResponse) Prelude.. Lens.mapping Lens.coerced

-- | If defined, the prompt that Amazon Lex uses to confirm the user\'s
-- intent before fulfilling it.
createIntentVersionResponse_confirmationPrompt :: Lens.Lens' CreateIntentVersionResponse (Prelude.Maybe Prompt)
createIntentVersionResponse_confirmationPrompt = Lens.lens (\CreateIntentVersionResponse' {confirmationPrompt} -> confirmationPrompt) (\s@CreateIntentVersionResponse' {} a -> s {confirmationPrompt = a} :: CreateIntentVersionResponse)

-- | If defined, Amazon Lex invokes this Lambda function for each user input.
createIntentVersionResponse_dialogCodeHook :: Lens.Lens' CreateIntentVersionResponse (Prelude.Maybe CodeHook)
createIntentVersionResponse_dialogCodeHook = Lens.lens (\CreateIntentVersionResponse' {dialogCodeHook} -> dialogCodeHook) (\s@CreateIntentVersionResponse' {} a -> s {dialogCodeHook = a} :: CreateIntentVersionResponse)

-- | A description of the intent.
createIntentVersionResponse_description :: Lens.Lens' CreateIntentVersionResponse (Prelude.Maybe Prelude.Text)
createIntentVersionResponse_description = Lens.lens (\CreateIntentVersionResponse' {description} -> description) (\s@CreateIntentVersionResponse' {} a -> s {description = a} :: CreateIntentVersionResponse)

-- | The response's http status code.
createIntentVersionResponse_httpStatus :: Lens.Lens' CreateIntentVersionResponse Prelude.Int
createIntentVersionResponse_httpStatus = Lens.lens (\CreateIntentVersionResponse' {httpStatus} -> httpStatus) (\s@CreateIntentVersionResponse' {} a -> s {httpStatus = a} :: CreateIntentVersionResponse)

instance Prelude.NFData CreateIntentVersionResponse where
  rnf CreateIntentVersionResponse' {..} =
    Prelude.rnf fulfillmentActivity
      `Prelude.seq` Prelude.rnf slots
      `Prelude.seq` Prelude.rnf rejectionStatement
      `Prelude.seq` Prelude.rnf checksum
      `Prelude.seq` Prelude.rnf conclusionStatement
      `Prelude.seq` Prelude.rnf sampleUtterances
      `Prelude.seq` Prelude.rnf parentIntentSignature
      `Prelude.seq` Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf kendraConfiguration
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf inputContexts
      `Prelude.seq` Prelude.rnf followUpPrompt
      `Prelude.seq` Prelude.rnf lastUpdatedDate
      `Prelude.seq` Prelude.rnf outputContexts
      `Prelude.seq` Prelude.rnf confirmationPrompt
      `Prelude.seq` Prelude.rnf dialogCodeHook
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf httpStatus
