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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
    createIntentVersionResponse_sampleUtterances,
    createIntentVersionResponse_kendraConfiguration,
    createIntentVersionResponse_name,
    createIntentVersionResponse_dialogCodeHook,
    createIntentVersionResponse_outputContexts,
    createIntentVersionResponse_fulfillmentActivity,
    createIntentVersionResponse_lastUpdatedDate,
    createIntentVersionResponse_followUpPrompt,
    createIntentVersionResponse_parentIntentSignature,
    createIntentVersionResponse_description,
    createIntentVersionResponse_rejectionStatement,
    createIntentVersionResponse_checksum,
    createIntentVersionResponse_confirmationPrompt,
    createIntentVersionResponse_createdDate,
    createIntentVersionResponse_slots,
    createIntentVersionResponse_inputContexts,
    createIntentVersionResponse_conclusionStatement,
    createIntentVersionResponse_version,
    createIntentVersionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateIntentVersionResponse'
            Prelude.<$> ( x Data..?> "sampleUtterances"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "kendraConfiguration")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "dialogCodeHook")
            Prelude.<*> (x Data..?> "outputContexts" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "fulfillmentActivity")
            Prelude.<*> (x Data..?> "lastUpdatedDate")
            Prelude.<*> (x Data..?> "followUpPrompt")
            Prelude.<*> (x Data..?> "parentIntentSignature")
            Prelude.<*> (x Data..?> "description")
            Prelude.<*> (x Data..?> "rejectionStatement")
            Prelude.<*> (x Data..?> "checksum")
            Prelude.<*> (x Data..?> "confirmationPrompt")
            Prelude.<*> (x Data..?> "createdDate")
            Prelude.<*> (x Data..?> "slots" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "inputContexts" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "conclusionStatement")
            Prelude.<*> (x Data..?> "version")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateIntentVersion where
  hashWithSalt _salt CreateIntentVersion' {..} =
    _salt `Prelude.hashWithSalt` checksum
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateIntentVersion where
  rnf CreateIntentVersion' {..} =
    Prelude.rnf checksum `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders CreateIntentVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateIntentVersion where
  toJSON CreateIntentVersion' {..} =
    Data.object
      ( Prelude.catMaybes
          [("checksum" Data..=) Prelude.<$> checksum]
      )

instance Data.ToPath CreateIntentVersion where
  toPath CreateIntentVersion' {..} =
    Prelude.mconcat
      ["/intents/", Data.toBS name, "/versions"]

instance Data.ToQuery CreateIntentVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateIntentVersionResponse' smart constructor.
data CreateIntentVersionResponse = CreateIntentVersionResponse'
  { -- | An array of sample utterances configured for the intent.
    sampleUtterances :: Prelude.Maybe [Prelude.Text],
    -- | Configuration information, if any, for connecting an Amazon Kendra index
    -- with the @AMAZON.KendraSearchIntent@ intent.
    kendraConfiguration :: Prelude.Maybe KendraConfiguration,
    -- | The name of the intent.
    name :: Prelude.Maybe Prelude.Text,
    -- | If defined, Amazon Lex invokes this Lambda function for each user input.
    dialogCodeHook :: Prelude.Maybe CodeHook,
    -- | An array of @OutputContext@ objects that lists the contexts that the
    -- intent activates when the intent is fulfilled.
    outputContexts :: Prelude.Maybe [OutputContext],
    -- | Describes how the intent is fulfilled.
    fulfillmentActivity :: Prelude.Maybe FulfillmentActivity,
    -- | The date that the intent was updated.
    lastUpdatedDate :: Prelude.Maybe Data.POSIX,
    -- | If defined, Amazon Lex uses this prompt to solicit additional user
    -- activity after the intent is fulfilled.
    followUpPrompt :: Prelude.Maybe FollowUpPrompt,
    -- | A unique identifier for a built-in intent.
    parentIntentSignature :: Prelude.Maybe Prelude.Text,
    -- | A description of the intent.
    description :: Prelude.Maybe Prelude.Text,
    -- | If the user answers \"no\" to the question defined in
    -- @confirmationPrompt@, Amazon Lex responds with this statement to
    -- acknowledge that the intent was canceled.
    rejectionStatement :: Prelude.Maybe Statement,
    -- | Checksum of the intent version created.
    checksum :: Prelude.Maybe Prelude.Text,
    -- | If defined, the prompt that Amazon Lex uses to confirm the user\'s
    -- intent before fulfilling it.
    confirmationPrompt :: Prelude.Maybe Prompt,
    -- | The date that the intent was created.
    createdDate :: Prelude.Maybe Data.POSIX,
    -- | An array of slot types that defines the information required to fulfill
    -- the intent.
    slots :: Prelude.Maybe [Slot],
    -- | An array of @InputContext@ objects that lists the contexts that must be
    -- active for Amazon Lex to choose the intent in a conversation with the
    -- user.
    inputContexts :: Prelude.Maybe [InputContext],
    -- | After the Lambda function specified in the @fulfillmentActivity@ field
    -- fulfills the intent, Amazon Lex conveys this statement to the user.
    conclusionStatement :: Prelude.Maybe Statement,
    -- | The version number assigned to the new version of the intent.
    version :: Prelude.Maybe Prelude.Text,
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
-- 'sampleUtterances', 'createIntentVersionResponse_sampleUtterances' - An array of sample utterances configured for the intent.
--
-- 'kendraConfiguration', 'createIntentVersionResponse_kendraConfiguration' - Configuration information, if any, for connecting an Amazon Kendra index
-- with the @AMAZON.KendraSearchIntent@ intent.
--
-- 'name', 'createIntentVersionResponse_name' - The name of the intent.
--
-- 'dialogCodeHook', 'createIntentVersionResponse_dialogCodeHook' - If defined, Amazon Lex invokes this Lambda function for each user input.
--
-- 'outputContexts', 'createIntentVersionResponse_outputContexts' - An array of @OutputContext@ objects that lists the contexts that the
-- intent activates when the intent is fulfilled.
--
-- 'fulfillmentActivity', 'createIntentVersionResponse_fulfillmentActivity' - Describes how the intent is fulfilled.
--
-- 'lastUpdatedDate', 'createIntentVersionResponse_lastUpdatedDate' - The date that the intent was updated.
--
-- 'followUpPrompt', 'createIntentVersionResponse_followUpPrompt' - If defined, Amazon Lex uses this prompt to solicit additional user
-- activity after the intent is fulfilled.
--
-- 'parentIntentSignature', 'createIntentVersionResponse_parentIntentSignature' - A unique identifier for a built-in intent.
--
-- 'description', 'createIntentVersionResponse_description' - A description of the intent.
--
-- 'rejectionStatement', 'createIntentVersionResponse_rejectionStatement' - If the user answers \"no\" to the question defined in
-- @confirmationPrompt@, Amazon Lex responds with this statement to
-- acknowledge that the intent was canceled.
--
-- 'checksum', 'createIntentVersionResponse_checksum' - Checksum of the intent version created.
--
-- 'confirmationPrompt', 'createIntentVersionResponse_confirmationPrompt' - If defined, the prompt that Amazon Lex uses to confirm the user\'s
-- intent before fulfilling it.
--
-- 'createdDate', 'createIntentVersionResponse_createdDate' - The date that the intent was created.
--
-- 'slots', 'createIntentVersionResponse_slots' - An array of slot types that defines the information required to fulfill
-- the intent.
--
-- 'inputContexts', 'createIntentVersionResponse_inputContexts' - An array of @InputContext@ objects that lists the contexts that must be
-- active for Amazon Lex to choose the intent in a conversation with the
-- user.
--
-- 'conclusionStatement', 'createIntentVersionResponse_conclusionStatement' - After the Lambda function specified in the @fulfillmentActivity@ field
-- fulfills the intent, Amazon Lex conveys this statement to the user.
--
-- 'version', 'createIntentVersionResponse_version' - The version number assigned to the new version of the intent.
--
-- 'httpStatus', 'createIntentVersionResponse_httpStatus' - The response's http status code.
newCreateIntentVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateIntentVersionResponse
newCreateIntentVersionResponse pHttpStatus_ =
  CreateIntentVersionResponse'
    { sampleUtterances =
        Prelude.Nothing,
      kendraConfiguration = Prelude.Nothing,
      name = Prelude.Nothing,
      dialogCodeHook = Prelude.Nothing,
      outputContexts = Prelude.Nothing,
      fulfillmentActivity = Prelude.Nothing,
      lastUpdatedDate = Prelude.Nothing,
      followUpPrompt = Prelude.Nothing,
      parentIntentSignature = Prelude.Nothing,
      description = Prelude.Nothing,
      rejectionStatement = Prelude.Nothing,
      checksum = Prelude.Nothing,
      confirmationPrompt = Prelude.Nothing,
      createdDate = Prelude.Nothing,
      slots = Prelude.Nothing,
      inputContexts = Prelude.Nothing,
      conclusionStatement = Prelude.Nothing,
      version = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of sample utterances configured for the intent.
createIntentVersionResponse_sampleUtterances :: Lens.Lens' CreateIntentVersionResponse (Prelude.Maybe [Prelude.Text])
createIntentVersionResponse_sampleUtterances = Lens.lens (\CreateIntentVersionResponse' {sampleUtterances} -> sampleUtterances) (\s@CreateIntentVersionResponse' {} a -> s {sampleUtterances = a} :: CreateIntentVersionResponse) Prelude.. Lens.mapping Lens.coerced

-- | Configuration information, if any, for connecting an Amazon Kendra index
-- with the @AMAZON.KendraSearchIntent@ intent.
createIntentVersionResponse_kendraConfiguration :: Lens.Lens' CreateIntentVersionResponse (Prelude.Maybe KendraConfiguration)
createIntentVersionResponse_kendraConfiguration = Lens.lens (\CreateIntentVersionResponse' {kendraConfiguration} -> kendraConfiguration) (\s@CreateIntentVersionResponse' {} a -> s {kendraConfiguration = a} :: CreateIntentVersionResponse)

-- | The name of the intent.
createIntentVersionResponse_name :: Lens.Lens' CreateIntentVersionResponse (Prelude.Maybe Prelude.Text)
createIntentVersionResponse_name = Lens.lens (\CreateIntentVersionResponse' {name} -> name) (\s@CreateIntentVersionResponse' {} a -> s {name = a} :: CreateIntentVersionResponse)

-- | If defined, Amazon Lex invokes this Lambda function for each user input.
createIntentVersionResponse_dialogCodeHook :: Lens.Lens' CreateIntentVersionResponse (Prelude.Maybe CodeHook)
createIntentVersionResponse_dialogCodeHook = Lens.lens (\CreateIntentVersionResponse' {dialogCodeHook} -> dialogCodeHook) (\s@CreateIntentVersionResponse' {} a -> s {dialogCodeHook = a} :: CreateIntentVersionResponse)

-- | An array of @OutputContext@ objects that lists the contexts that the
-- intent activates when the intent is fulfilled.
createIntentVersionResponse_outputContexts :: Lens.Lens' CreateIntentVersionResponse (Prelude.Maybe [OutputContext])
createIntentVersionResponse_outputContexts = Lens.lens (\CreateIntentVersionResponse' {outputContexts} -> outputContexts) (\s@CreateIntentVersionResponse' {} a -> s {outputContexts = a} :: CreateIntentVersionResponse) Prelude.. Lens.mapping Lens.coerced

-- | Describes how the intent is fulfilled.
createIntentVersionResponse_fulfillmentActivity :: Lens.Lens' CreateIntentVersionResponse (Prelude.Maybe FulfillmentActivity)
createIntentVersionResponse_fulfillmentActivity = Lens.lens (\CreateIntentVersionResponse' {fulfillmentActivity} -> fulfillmentActivity) (\s@CreateIntentVersionResponse' {} a -> s {fulfillmentActivity = a} :: CreateIntentVersionResponse)

-- | The date that the intent was updated.
createIntentVersionResponse_lastUpdatedDate :: Lens.Lens' CreateIntentVersionResponse (Prelude.Maybe Prelude.UTCTime)
createIntentVersionResponse_lastUpdatedDate = Lens.lens (\CreateIntentVersionResponse' {lastUpdatedDate} -> lastUpdatedDate) (\s@CreateIntentVersionResponse' {} a -> s {lastUpdatedDate = a} :: CreateIntentVersionResponse) Prelude.. Lens.mapping Data._Time

-- | If defined, Amazon Lex uses this prompt to solicit additional user
-- activity after the intent is fulfilled.
createIntentVersionResponse_followUpPrompt :: Lens.Lens' CreateIntentVersionResponse (Prelude.Maybe FollowUpPrompt)
createIntentVersionResponse_followUpPrompt = Lens.lens (\CreateIntentVersionResponse' {followUpPrompt} -> followUpPrompt) (\s@CreateIntentVersionResponse' {} a -> s {followUpPrompt = a} :: CreateIntentVersionResponse)

-- | A unique identifier for a built-in intent.
createIntentVersionResponse_parentIntentSignature :: Lens.Lens' CreateIntentVersionResponse (Prelude.Maybe Prelude.Text)
createIntentVersionResponse_parentIntentSignature = Lens.lens (\CreateIntentVersionResponse' {parentIntentSignature} -> parentIntentSignature) (\s@CreateIntentVersionResponse' {} a -> s {parentIntentSignature = a} :: CreateIntentVersionResponse)

-- | A description of the intent.
createIntentVersionResponse_description :: Lens.Lens' CreateIntentVersionResponse (Prelude.Maybe Prelude.Text)
createIntentVersionResponse_description = Lens.lens (\CreateIntentVersionResponse' {description} -> description) (\s@CreateIntentVersionResponse' {} a -> s {description = a} :: CreateIntentVersionResponse)

-- | If the user answers \"no\" to the question defined in
-- @confirmationPrompt@, Amazon Lex responds with this statement to
-- acknowledge that the intent was canceled.
createIntentVersionResponse_rejectionStatement :: Lens.Lens' CreateIntentVersionResponse (Prelude.Maybe Statement)
createIntentVersionResponse_rejectionStatement = Lens.lens (\CreateIntentVersionResponse' {rejectionStatement} -> rejectionStatement) (\s@CreateIntentVersionResponse' {} a -> s {rejectionStatement = a} :: CreateIntentVersionResponse)

-- | Checksum of the intent version created.
createIntentVersionResponse_checksum :: Lens.Lens' CreateIntentVersionResponse (Prelude.Maybe Prelude.Text)
createIntentVersionResponse_checksum = Lens.lens (\CreateIntentVersionResponse' {checksum} -> checksum) (\s@CreateIntentVersionResponse' {} a -> s {checksum = a} :: CreateIntentVersionResponse)

-- | If defined, the prompt that Amazon Lex uses to confirm the user\'s
-- intent before fulfilling it.
createIntentVersionResponse_confirmationPrompt :: Lens.Lens' CreateIntentVersionResponse (Prelude.Maybe Prompt)
createIntentVersionResponse_confirmationPrompt = Lens.lens (\CreateIntentVersionResponse' {confirmationPrompt} -> confirmationPrompt) (\s@CreateIntentVersionResponse' {} a -> s {confirmationPrompt = a} :: CreateIntentVersionResponse)

-- | The date that the intent was created.
createIntentVersionResponse_createdDate :: Lens.Lens' CreateIntentVersionResponse (Prelude.Maybe Prelude.UTCTime)
createIntentVersionResponse_createdDate = Lens.lens (\CreateIntentVersionResponse' {createdDate} -> createdDate) (\s@CreateIntentVersionResponse' {} a -> s {createdDate = a} :: CreateIntentVersionResponse) Prelude.. Lens.mapping Data._Time

-- | An array of slot types that defines the information required to fulfill
-- the intent.
createIntentVersionResponse_slots :: Lens.Lens' CreateIntentVersionResponse (Prelude.Maybe [Slot])
createIntentVersionResponse_slots = Lens.lens (\CreateIntentVersionResponse' {slots} -> slots) (\s@CreateIntentVersionResponse' {} a -> s {slots = a} :: CreateIntentVersionResponse) Prelude.. Lens.mapping Lens.coerced

-- | An array of @InputContext@ objects that lists the contexts that must be
-- active for Amazon Lex to choose the intent in a conversation with the
-- user.
createIntentVersionResponse_inputContexts :: Lens.Lens' CreateIntentVersionResponse (Prelude.Maybe [InputContext])
createIntentVersionResponse_inputContexts = Lens.lens (\CreateIntentVersionResponse' {inputContexts} -> inputContexts) (\s@CreateIntentVersionResponse' {} a -> s {inputContexts = a} :: CreateIntentVersionResponse) Prelude.. Lens.mapping Lens.coerced

-- | After the Lambda function specified in the @fulfillmentActivity@ field
-- fulfills the intent, Amazon Lex conveys this statement to the user.
createIntentVersionResponse_conclusionStatement :: Lens.Lens' CreateIntentVersionResponse (Prelude.Maybe Statement)
createIntentVersionResponse_conclusionStatement = Lens.lens (\CreateIntentVersionResponse' {conclusionStatement} -> conclusionStatement) (\s@CreateIntentVersionResponse' {} a -> s {conclusionStatement = a} :: CreateIntentVersionResponse)

-- | The version number assigned to the new version of the intent.
createIntentVersionResponse_version :: Lens.Lens' CreateIntentVersionResponse (Prelude.Maybe Prelude.Text)
createIntentVersionResponse_version = Lens.lens (\CreateIntentVersionResponse' {version} -> version) (\s@CreateIntentVersionResponse' {} a -> s {version = a} :: CreateIntentVersionResponse)

-- | The response's http status code.
createIntentVersionResponse_httpStatus :: Lens.Lens' CreateIntentVersionResponse Prelude.Int
createIntentVersionResponse_httpStatus = Lens.lens (\CreateIntentVersionResponse' {httpStatus} -> httpStatus) (\s@CreateIntentVersionResponse' {} a -> s {httpStatus = a} :: CreateIntentVersionResponse)

instance Prelude.NFData CreateIntentVersionResponse where
  rnf CreateIntentVersionResponse' {..} =
    Prelude.rnf sampleUtterances
      `Prelude.seq` Prelude.rnf kendraConfiguration
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf dialogCodeHook
      `Prelude.seq` Prelude.rnf outputContexts
      `Prelude.seq` Prelude.rnf fulfillmentActivity
      `Prelude.seq` Prelude.rnf lastUpdatedDate
      `Prelude.seq` Prelude.rnf followUpPrompt
      `Prelude.seq` Prelude.rnf parentIntentSignature
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf rejectionStatement
      `Prelude.seq` Prelude.rnf checksum
      `Prelude.seq` Prelude.rnf confirmationPrompt
      `Prelude.seq` Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf slots
      `Prelude.seq` Prelude.rnf inputContexts
      `Prelude.seq` Prelude.rnf conclusionStatement
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf httpStatus
