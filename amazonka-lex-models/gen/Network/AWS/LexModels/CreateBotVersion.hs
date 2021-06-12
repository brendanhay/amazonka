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
-- Module      : Network.AWS.LexModels.CreateBotVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new version of the bot based on the @$LATEST@ version. If the
-- @$LATEST@ version of this resource hasn\'t changed since you created the
-- last version, Amazon Lex doesn\'t create a new version. It returns the
-- last created version.
--
-- You can update only the @$LATEST@ version of the bot. You can\'t update
-- the numbered versions that you create with the @CreateBotVersion@
-- operation.
--
-- When you create the first version of a bot, Amazon Lex sets the version
-- to 1. Subsequent versions increment by 1. For more information, see
-- versioning-intro.
--
-- This operation requires permission for the @lex:CreateBotVersion@
-- action.
module Network.AWS.LexModels.CreateBotVersion
  ( -- * Creating a Request
    CreateBotVersion (..),
    newCreateBotVersion,

    -- * Request Lenses
    createBotVersion_checksum,
    createBotVersion_name,

    -- * Destructuring the Response
    CreateBotVersionResponse (..),
    newCreateBotVersionResponse,

    -- * Response Lenses
    createBotVersionResponse_abortStatement,
    createBotVersionResponse_createdDate,
    createBotVersionResponse_status,
    createBotVersionResponse_voiceId,
    createBotVersionResponse_lastUpdatedDate,
    createBotVersionResponse_locale,
    createBotVersionResponse_clarificationPrompt,
    createBotVersionResponse_enableModelImprovements,
    createBotVersionResponse_version,
    createBotVersionResponse_idleSessionTTLInSeconds,
    createBotVersionResponse_name,
    createBotVersionResponse_intents,
    createBotVersionResponse_failureReason,
    createBotVersionResponse_childDirected,
    createBotVersionResponse_description,
    createBotVersionResponse_detectSentiment,
    createBotVersionResponse_checksum,
    createBotVersionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateBotVersion' smart constructor.
data CreateBotVersion = CreateBotVersion'
  { -- | Identifies a specific revision of the @$LATEST@ version of the bot. If
    -- you specify a checksum and the @$LATEST@ version of the bot has a
    -- different checksum, a @PreconditionFailedException@ exception is
    -- returned and Amazon Lex doesn\'t publish a new version. If you don\'t
    -- specify a checksum, Amazon Lex publishes the @$LATEST@ version.
    checksum :: Core.Maybe Core.Text,
    -- | The name of the bot that you want to create a new version of. The name
    -- is case sensitive.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateBotVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'checksum', 'createBotVersion_checksum' - Identifies a specific revision of the @$LATEST@ version of the bot. If
-- you specify a checksum and the @$LATEST@ version of the bot has a
-- different checksum, a @PreconditionFailedException@ exception is
-- returned and Amazon Lex doesn\'t publish a new version. If you don\'t
-- specify a checksum, Amazon Lex publishes the @$LATEST@ version.
--
-- 'name', 'createBotVersion_name' - The name of the bot that you want to create a new version of. The name
-- is case sensitive.
newCreateBotVersion ::
  -- | 'name'
  Core.Text ->
  CreateBotVersion
newCreateBotVersion pName_ =
  CreateBotVersion'
    { checksum = Core.Nothing,
      name = pName_
    }

-- | Identifies a specific revision of the @$LATEST@ version of the bot. If
-- you specify a checksum and the @$LATEST@ version of the bot has a
-- different checksum, a @PreconditionFailedException@ exception is
-- returned and Amazon Lex doesn\'t publish a new version. If you don\'t
-- specify a checksum, Amazon Lex publishes the @$LATEST@ version.
createBotVersion_checksum :: Lens.Lens' CreateBotVersion (Core.Maybe Core.Text)
createBotVersion_checksum = Lens.lens (\CreateBotVersion' {checksum} -> checksum) (\s@CreateBotVersion' {} a -> s {checksum = a} :: CreateBotVersion)

-- | The name of the bot that you want to create a new version of. The name
-- is case sensitive.
createBotVersion_name :: Lens.Lens' CreateBotVersion Core.Text
createBotVersion_name = Lens.lens (\CreateBotVersion' {name} -> name) (\s@CreateBotVersion' {} a -> s {name = a} :: CreateBotVersion)

instance Core.AWSRequest CreateBotVersion where
  type
    AWSResponse CreateBotVersion =
      CreateBotVersionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateBotVersionResponse'
            Core.<$> (x Core..?> "abortStatement")
            Core.<*> (x Core..?> "createdDate")
            Core.<*> (x Core..?> "status")
            Core.<*> (x Core..?> "voiceId")
            Core.<*> (x Core..?> "lastUpdatedDate")
            Core.<*> (x Core..?> "locale")
            Core.<*> (x Core..?> "clarificationPrompt")
            Core.<*> (x Core..?> "enableModelImprovements")
            Core.<*> (x Core..?> "version")
            Core.<*> (x Core..?> "idleSessionTTLInSeconds")
            Core.<*> (x Core..?> "name")
            Core.<*> (x Core..?> "intents" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "failureReason")
            Core.<*> (x Core..?> "childDirected")
            Core.<*> (x Core..?> "description")
            Core.<*> (x Core..?> "detectSentiment")
            Core.<*> (x Core..?> "checksum")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateBotVersion

instance Core.NFData CreateBotVersion

instance Core.ToHeaders CreateBotVersion where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateBotVersion where
  toJSON CreateBotVersion' {..} =
    Core.object
      ( Core.catMaybes
          [("checksum" Core..=) Core.<$> checksum]
      )

instance Core.ToPath CreateBotVersion where
  toPath CreateBotVersion' {..} =
    Core.mconcat
      ["/bots/", Core.toBS name, "/versions"]

instance Core.ToQuery CreateBotVersion where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateBotVersionResponse' smart constructor.
data CreateBotVersionResponse = CreateBotVersionResponse'
  { -- | The message that Amazon Lex uses to cancel a conversation. For more
    -- information, see PutBot.
    abortStatement :: Core.Maybe Statement,
    -- | The date when the bot version was created.
    createdDate :: Core.Maybe Core.POSIX,
    -- | When you send a request to create or update a bot, Amazon Lex sets the
    -- @status@ response element to @BUILDING@. After Amazon Lex builds the
    -- bot, it sets @status@ to @READY@. If Amazon Lex can\'t build the bot, it
    -- sets @status@ to @FAILED@. Amazon Lex returns the reason for the failure
    -- in the @failureReason@ response element.
    status :: Core.Maybe LexStatus,
    -- | The Amazon Polly voice ID that Amazon Lex uses for voice interactions
    -- with the user.
    voiceId :: Core.Maybe Core.Text,
    -- | The date when the @$LATEST@ version of this bot was updated.
    lastUpdatedDate :: Core.Maybe Core.POSIX,
    -- | Specifies the target locale for the bot.
    locale :: Core.Maybe Locale,
    -- | The message that Amazon Lex uses when it doesn\'t understand the user\'s
    -- request. For more information, see PutBot.
    clarificationPrompt :: Core.Maybe Prompt,
    -- | Indicates whether the bot uses accuracy improvements. @true@ indicates
    -- that the bot is using the improvements, otherwise, @false@.
    enableModelImprovements :: Core.Maybe Core.Bool,
    -- | The version of the bot.
    version :: Core.Maybe Core.Text,
    -- | The maximum time in seconds that Amazon Lex retains the data gathered in
    -- a conversation. For more information, see PutBot.
    idleSessionTTLInSeconds :: Core.Maybe Core.Natural,
    -- | The name of the bot.
    name :: Core.Maybe Core.Text,
    -- | An array of @Intent@ objects. For more information, see PutBot.
    intents :: Core.Maybe [Intent],
    -- | If @status@ is @FAILED@, Amazon Lex provides the reason that it failed
    -- to build the bot.
    failureReason :: Core.Maybe Core.Text,
    -- | For each Amazon Lex bot created with the Amazon Lex Model Building
    -- Service, you must specify whether your use of Amazon Lex is related to a
    -- website, program, or other application that is directed or targeted, in
    -- whole or in part, to children under age 13 and subject to the
    -- Children\'s Online Privacy Protection Act (COPPA) by specifying @true@
    -- or @false@ in the @childDirected@ field. By specifying @true@ in the
    -- @childDirected@ field, you confirm that your use of Amazon Lex __is__
    -- related to a website, program, or other application that is directed or
    -- targeted, in whole or in part, to children under age 13 and subject to
    -- COPPA. By specifying @false@ in the @childDirected@ field, you confirm
    -- that your use of Amazon Lex __is not__ related to a website, program, or
    -- other application that is directed or targeted, in whole or in part, to
    -- children under age 13 and subject to COPPA. You may not specify a
    -- default value for the @childDirected@ field that does not accurately
    -- reflect whether your use of Amazon Lex is related to a website, program,
    -- or other application that is directed or targeted, in whole or in part,
    -- to children under age 13 and subject to COPPA.
    --
    -- If your use of Amazon Lex relates to a website, program, or other
    -- application that is directed in whole or in part, to children under age
    -- 13, you must obtain any required verifiable parental consent under
    -- COPPA. For information regarding the use of Amazon Lex in connection
    -- with websites, programs, or other applications that are directed or
    -- targeted, in whole or in part, to children under age 13, see the
    -- <https://aws.amazon.com/lex/faqs#data-security Amazon Lex FAQ.>
    childDirected :: Core.Maybe Core.Bool,
    -- | A description of the bot.
    description :: Core.Maybe Core.Text,
    -- | Indicates whether utterances entered by the user should be sent to
    -- Amazon Comprehend for sentiment analysis.
    detectSentiment :: Core.Maybe Core.Bool,
    -- | Checksum identifying the version of the bot that was created.
    checksum :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateBotVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'abortStatement', 'createBotVersionResponse_abortStatement' - The message that Amazon Lex uses to cancel a conversation. For more
-- information, see PutBot.
--
-- 'createdDate', 'createBotVersionResponse_createdDate' - The date when the bot version was created.
--
-- 'status', 'createBotVersionResponse_status' - When you send a request to create or update a bot, Amazon Lex sets the
-- @status@ response element to @BUILDING@. After Amazon Lex builds the
-- bot, it sets @status@ to @READY@. If Amazon Lex can\'t build the bot, it
-- sets @status@ to @FAILED@. Amazon Lex returns the reason for the failure
-- in the @failureReason@ response element.
--
-- 'voiceId', 'createBotVersionResponse_voiceId' - The Amazon Polly voice ID that Amazon Lex uses for voice interactions
-- with the user.
--
-- 'lastUpdatedDate', 'createBotVersionResponse_lastUpdatedDate' - The date when the @$LATEST@ version of this bot was updated.
--
-- 'locale', 'createBotVersionResponse_locale' - Specifies the target locale for the bot.
--
-- 'clarificationPrompt', 'createBotVersionResponse_clarificationPrompt' - The message that Amazon Lex uses when it doesn\'t understand the user\'s
-- request. For more information, see PutBot.
--
-- 'enableModelImprovements', 'createBotVersionResponse_enableModelImprovements' - Indicates whether the bot uses accuracy improvements. @true@ indicates
-- that the bot is using the improvements, otherwise, @false@.
--
-- 'version', 'createBotVersionResponse_version' - The version of the bot.
--
-- 'idleSessionTTLInSeconds', 'createBotVersionResponse_idleSessionTTLInSeconds' - The maximum time in seconds that Amazon Lex retains the data gathered in
-- a conversation. For more information, see PutBot.
--
-- 'name', 'createBotVersionResponse_name' - The name of the bot.
--
-- 'intents', 'createBotVersionResponse_intents' - An array of @Intent@ objects. For more information, see PutBot.
--
-- 'failureReason', 'createBotVersionResponse_failureReason' - If @status@ is @FAILED@, Amazon Lex provides the reason that it failed
-- to build the bot.
--
-- 'childDirected', 'createBotVersionResponse_childDirected' - For each Amazon Lex bot created with the Amazon Lex Model Building
-- Service, you must specify whether your use of Amazon Lex is related to a
-- website, program, or other application that is directed or targeted, in
-- whole or in part, to children under age 13 and subject to the
-- Children\'s Online Privacy Protection Act (COPPA) by specifying @true@
-- or @false@ in the @childDirected@ field. By specifying @true@ in the
-- @childDirected@ field, you confirm that your use of Amazon Lex __is__
-- related to a website, program, or other application that is directed or
-- targeted, in whole or in part, to children under age 13 and subject to
-- COPPA. By specifying @false@ in the @childDirected@ field, you confirm
-- that your use of Amazon Lex __is not__ related to a website, program, or
-- other application that is directed or targeted, in whole or in part, to
-- children under age 13 and subject to COPPA. You may not specify a
-- default value for the @childDirected@ field that does not accurately
-- reflect whether your use of Amazon Lex is related to a website, program,
-- or other application that is directed or targeted, in whole or in part,
-- to children under age 13 and subject to COPPA.
--
-- If your use of Amazon Lex relates to a website, program, or other
-- application that is directed in whole or in part, to children under age
-- 13, you must obtain any required verifiable parental consent under
-- COPPA. For information regarding the use of Amazon Lex in connection
-- with websites, programs, or other applications that are directed or
-- targeted, in whole or in part, to children under age 13, see the
-- <https://aws.amazon.com/lex/faqs#data-security Amazon Lex FAQ.>
--
-- 'description', 'createBotVersionResponse_description' - A description of the bot.
--
-- 'detectSentiment', 'createBotVersionResponse_detectSentiment' - Indicates whether utterances entered by the user should be sent to
-- Amazon Comprehend for sentiment analysis.
--
-- 'checksum', 'createBotVersionResponse_checksum' - Checksum identifying the version of the bot that was created.
--
-- 'httpStatus', 'createBotVersionResponse_httpStatus' - The response's http status code.
newCreateBotVersionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateBotVersionResponse
newCreateBotVersionResponse pHttpStatus_ =
  CreateBotVersionResponse'
    { abortStatement =
        Core.Nothing,
      createdDate = Core.Nothing,
      status = Core.Nothing,
      voiceId = Core.Nothing,
      lastUpdatedDate = Core.Nothing,
      locale = Core.Nothing,
      clarificationPrompt = Core.Nothing,
      enableModelImprovements = Core.Nothing,
      version = Core.Nothing,
      idleSessionTTLInSeconds = Core.Nothing,
      name = Core.Nothing,
      intents = Core.Nothing,
      failureReason = Core.Nothing,
      childDirected = Core.Nothing,
      description = Core.Nothing,
      detectSentiment = Core.Nothing,
      checksum = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The message that Amazon Lex uses to cancel a conversation. For more
-- information, see PutBot.
createBotVersionResponse_abortStatement :: Lens.Lens' CreateBotVersionResponse (Core.Maybe Statement)
createBotVersionResponse_abortStatement = Lens.lens (\CreateBotVersionResponse' {abortStatement} -> abortStatement) (\s@CreateBotVersionResponse' {} a -> s {abortStatement = a} :: CreateBotVersionResponse)

-- | The date when the bot version was created.
createBotVersionResponse_createdDate :: Lens.Lens' CreateBotVersionResponse (Core.Maybe Core.UTCTime)
createBotVersionResponse_createdDate = Lens.lens (\CreateBotVersionResponse' {createdDate} -> createdDate) (\s@CreateBotVersionResponse' {} a -> s {createdDate = a} :: CreateBotVersionResponse) Core.. Lens.mapping Core._Time

-- | When you send a request to create or update a bot, Amazon Lex sets the
-- @status@ response element to @BUILDING@. After Amazon Lex builds the
-- bot, it sets @status@ to @READY@. If Amazon Lex can\'t build the bot, it
-- sets @status@ to @FAILED@. Amazon Lex returns the reason for the failure
-- in the @failureReason@ response element.
createBotVersionResponse_status :: Lens.Lens' CreateBotVersionResponse (Core.Maybe LexStatus)
createBotVersionResponse_status = Lens.lens (\CreateBotVersionResponse' {status} -> status) (\s@CreateBotVersionResponse' {} a -> s {status = a} :: CreateBotVersionResponse)

-- | The Amazon Polly voice ID that Amazon Lex uses for voice interactions
-- with the user.
createBotVersionResponse_voiceId :: Lens.Lens' CreateBotVersionResponse (Core.Maybe Core.Text)
createBotVersionResponse_voiceId = Lens.lens (\CreateBotVersionResponse' {voiceId} -> voiceId) (\s@CreateBotVersionResponse' {} a -> s {voiceId = a} :: CreateBotVersionResponse)

-- | The date when the @$LATEST@ version of this bot was updated.
createBotVersionResponse_lastUpdatedDate :: Lens.Lens' CreateBotVersionResponse (Core.Maybe Core.UTCTime)
createBotVersionResponse_lastUpdatedDate = Lens.lens (\CreateBotVersionResponse' {lastUpdatedDate} -> lastUpdatedDate) (\s@CreateBotVersionResponse' {} a -> s {lastUpdatedDate = a} :: CreateBotVersionResponse) Core.. Lens.mapping Core._Time

-- | Specifies the target locale for the bot.
createBotVersionResponse_locale :: Lens.Lens' CreateBotVersionResponse (Core.Maybe Locale)
createBotVersionResponse_locale = Lens.lens (\CreateBotVersionResponse' {locale} -> locale) (\s@CreateBotVersionResponse' {} a -> s {locale = a} :: CreateBotVersionResponse)

-- | The message that Amazon Lex uses when it doesn\'t understand the user\'s
-- request. For more information, see PutBot.
createBotVersionResponse_clarificationPrompt :: Lens.Lens' CreateBotVersionResponse (Core.Maybe Prompt)
createBotVersionResponse_clarificationPrompt = Lens.lens (\CreateBotVersionResponse' {clarificationPrompt} -> clarificationPrompt) (\s@CreateBotVersionResponse' {} a -> s {clarificationPrompt = a} :: CreateBotVersionResponse)

-- | Indicates whether the bot uses accuracy improvements. @true@ indicates
-- that the bot is using the improvements, otherwise, @false@.
createBotVersionResponse_enableModelImprovements :: Lens.Lens' CreateBotVersionResponse (Core.Maybe Core.Bool)
createBotVersionResponse_enableModelImprovements = Lens.lens (\CreateBotVersionResponse' {enableModelImprovements} -> enableModelImprovements) (\s@CreateBotVersionResponse' {} a -> s {enableModelImprovements = a} :: CreateBotVersionResponse)

-- | The version of the bot.
createBotVersionResponse_version :: Lens.Lens' CreateBotVersionResponse (Core.Maybe Core.Text)
createBotVersionResponse_version = Lens.lens (\CreateBotVersionResponse' {version} -> version) (\s@CreateBotVersionResponse' {} a -> s {version = a} :: CreateBotVersionResponse)

-- | The maximum time in seconds that Amazon Lex retains the data gathered in
-- a conversation. For more information, see PutBot.
createBotVersionResponse_idleSessionTTLInSeconds :: Lens.Lens' CreateBotVersionResponse (Core.Maybe Core.Natural)
createBotVersionResponse_idleSessionTTLInSeconds = Lens.lens (\CreateBotVersionResponse' {idleSessionTTLInSeconds} -> idleSessionTTLInSeconds) (\s@CreateBotVersionResponse' {} a -> s {idleSessionTTLInSeconds = a} :: CreateBotVersionResponse)

-- | The name of the bot.
createBotVersionResponse_name :: Lens.Lens' CreateBotVersionResponse (Core.Maybe Core.Text)
createBotVersionResponse_name = Lens.lens (\CreateBotVersionResponse' {name} -> name) (\s@CreateBotVersionResponse' {} a -> s {name = a} :: CreateBotVersionResponse)

-- | An array of @Intent@ objects. For more information, see PutBot.
createBotVersionResponse_intents :: Lens.Lens' CreateBotVersionResponse (Core.Maybe [Intent])
createBotVersionResponse_intents = Lens.lens (\CreateBotVersionResponse' {intents} -> intents) (\s@CreateBotVersionResponse' {} a -> s {intents = a} :: CreateBotVersionResponse) Core.. Lens.mapping Lens._Coerce

-- | If @status@ is @FAILED@, Amazon Lex provides the reason that it failed
-- to build the bot.
createBotVersionResponse_failureReason :: Lens.Lens' CreateBotVersionResponse (Core.Maybe Core.Text)
createBotVersionResponse_failureReason = Lens.lens (\CreateBotVersionResponse' {failureReason} -> failureReason) (\s@CreateBotVersionResponse' {} a -> s {failureReason = a} :: CreateBotVersionResponse)

-- | For each Amazon Lex bot created with the Amazon Lex Model Building
-- Service, you must specify whether your use of Amazon Lex is related to a
-- website, program, or other application that is directed or targeted, in
-- whole or in part, to children under age 13 and subject to the
-- Children\'s Online Privacy Protection Act (COPPA) by specifying @true@
-- or @false@ in the @childDirected@ field. By specifying @true@ in the
-- @childDirected@ field, you confirm that your use of Amazon Lex __is__
-- related to a website, program, or other application that is directed or
-- targeted, in whole or in part, to children under age 13 and subject to
-- COPPA. By specifying @false@ in the @childDirected@ field, you confirm
-- that your use of Amazon Lex __is not__ related to a website, program, or
-- other application that is directed or targeted, in whole or in part, to
-- children under age 13 and subject to COPPA. You may not specify a
-- default value for the @childDirected@ field that does not accurately
-- reflect whether your use of Amazon Lex is related to a website, program,
-- or other application that is directed or targeted, in whole or in part,
-- to children under age 13 and subject to COPPA.
--
-- If your use of Amazon Lex relates to a website, program, or other
-- application that is directed in whole or in part, to children under age
-- 13, you must obtain any required verifiable parental consent under
-- COPPA. For information regarding the use of Amazon Lex in connection
-- with websites, programs, or other applications that are directed or
-- targeted, in whole or in part, to children under age 13, see the
-- <https://aws.amazon.com/lex/faqs#data-security Amazon Lex FAQ.>
createBotVersionResponse_childDirected :: Lens.Lens' CreateBotVersionResponse (Core.Maybe Core.Bool)
createBotVersionResponse_childDirected = Lens.lens (\CreateBotVersionResponse' {childDirected} -> childDirected) (\s@CreateBotVersionResponse' {} a -> s {childDirected = a} :: CreateBotVersionResponse)

-- | A description of the bot.
createBotVersionResponse_description :: Lens.Lens' CreateBotVersionResponse (Core.Maybe Core.Text)
createBotVersionResponse_description = Lens.lens (\CreateBotVersionResponse' {description} -> description) (\s@CreateBotVersionResponse' {} a -> s {description = a} :: CreateBotVersionResponse)

-- | Indicates whether utterances entered by the user should be sent to
-- Amazon Comprehend for sentiment analysis.
createBotVersionResponse_detectSentiment :: Lens.Lens' CreateBotVersionResponse (Core.Maybe Core.Bool)
createBotVersionResponse_detectSentiment = Lens.lens (\CreateBotVersionResponse' {detectSentiment} -> detectSentiment) (\s@CreateBotVersionResponse' {} a -> s {detectSentiment = a} :: CreateBotVersionResponse)

-- | Checksum identifying the version of the bot that was created.
createBotVersionResponse_checksum :: Lens.Lens' CreateBotVersionResponse (Core.Maybe Core.Text)
createBotVersionResponse_checksum = Lens.lens (\CreateBotVersionResponse' {checksum} -> checksum) (\s@CreateBotVersionResponse' {} a -> s {checksum = a} :: CreateBotVersionResponse)

-- | The response's http status code.
createBotVersionResponse_httpStatus :: Lens.Lens' CreateBotVersionResponse Core.Int
createBotVersionResponse_httpStatus = Lens.lens (\CreateBotVersionResponse' {httpStatus} -> httpStatus) (\s@CreateBotVersionResponse' {} a -> s {httpStatus = a} :: CreateBotVersionResponse)

instance Core.NFData CreateBotVersionResponse
