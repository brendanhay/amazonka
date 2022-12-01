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
-- Module      : Amazonka.LexModels.GetBot
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns metadata information for a specific bot. You must provide the
-- bot name and the bot version or alias.
--
-- This operation requires permissions for the @lex:GetBot@ action.
module Amazonka.LexModels.GetBot
  ( -- * Creating a Request
    GetBot (..),
    newGetBot,

    -- * Request Lenses
    getBot_name,
    getBot_versionOrAlias,

    -- * Destructuring the Response
    GetBotResponse (..),
    newGetBotResponse,

    -- * Response Lenses
    getBotResponse_detectSentiment,
    getBotResponse_voiceId,
    getBotResponse_name,
    getBotResponse_nluIntentConfidenceThreshold,
    getBotResponse_locale,
    getBotResponse_lastUpdatedDate,
    getBotResponse_intents,
    getBotResponse_status,
    getBotResponse_description,
    getBotResponse_idleSessionTTLInSeconds,
    getBotResponse_enableModelImprovements,
    getBotResponse_checksum,
    getBotResponse_childDirected,
    getBotResponse_abortStatement,
    getBotResponse_clarificationPrompt,
    getBotResponse_createdDate,
    getBotResponse_failureReason,
    getBotResponse_version,
    getBotResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LexModels.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetBot' smart constructor.
data GetBot = GetBot'
  { -- | The name of the bot. The name is case sensitive.
    name :: Prelude.Text,
    -- | The version or alias of the bot.
    versionOrAlias :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'getBot_name' - The name of the bot. The name is case sensitive.
--
-- 'versionOrAlias', 'getBot_versionOrAlias' - The version or alias of the bot.
newGetBot ::
  -- | 'name'
  Prelude.Text ->
  -- | 'versionOrAlias'
  Prelude.Text ->
  GetBot
newGetBot pName_ pVersionOrAlias_ =
  GetBot'
    { name = pName_,
      versionOrAlias = pVersionOrAlias_
    }

-- | The name of the bot. The name is case sensitive.
getBot_name :: Lens.Lens' GetBot Prelude.Text
getBot_name = Lens.lens (\GetBot' {name} -> name) (\s@GetBot' {} a -> s {name = a} :: GetBot)

-- | The version or alias of the bot.
getBot_versionOrAlias :: Lens.Lens' GetBot Prelude.Text
getBot_versionOrAlias = Lens.lens (\GetBot' {versionOrAlias} -> versionOrAlias) (\s@GetBot' {} a -> s {versionOrAlias = a} :: GetBot)

instance Core.AWSRequest GetBot where
  type AWSResponse GetBot = GetBotResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBotResponse'
            Prelude.<$> (x Core..?> "detectSentiment")
            Prelude.<*> (x Core..?> "voiceId")
            Prelude.<*> (x Core..?> "name")
            Prelude.<*> (x Core..?> "nluIntentConfidenceThreshold")
            Prelude.<*> (x Core..?> "locale")
            Prelude.<*> (x Core..?> "lastUpdatedDate")
            Prelude.<*> (x Core..?> "intents" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "status")
            Prelude.<*> (x Core..?> "description")
            Prelude.<*> (x Core..?> "idleSessionTTLInSeconds")
            Prelude.<*> (x Core..?> "enableModelImprovements")
            Prelude.<*> (x Core..?> "checksum")
            Prelude.<*> (x Core..?> "childDirected")
            Prelude.<*> (x Core..?> "abortStatement")
            Prelude.<*> (x Core..?> "clarificationPrompt")
            Prelude.<*> (x Core..?> "createdDate")
            Prelude.<*> (x Core..?> "failureReason")
            Prelude.<*> (x Core..?> "version")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetBot where
  hashWithSalt _salt GetBot' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` versionOrAlias

instance Prelude.NFData GetBot where
  rnf GetBot' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf versionOrAlias

instance Core.ToHeaders GetBot where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetBot where
  toPath GetBot' {..} =
    Prelude.mconcat
      [ "/bots/",
        Core.toBS name,
        "/versions/",
        Core.toBS versionOrAlias
      ]

instance Core.ToQuery GetBot where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetBotResponse' smart constructor.
data GetBotResponse = GetBotResponse'
  { -- | Indicates whether user utterances should be sent to Amazon Comprehend
    -- for sentiment analysis.
    detectSentiment :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Polly voice ID that Amazon Lex uses for voice interaction
    -- with the user. For more information, see PutBot.
    voiceId :: Prelude.Maybe Prelude.Text,
    -- | The name of the bot.
    name :: Prelude.Maybe Prelude.Text,
    -- | The score that determines where Amazon Lex inserts the
    -- @AMAZON.FallbackIntent@, @AMAZON.KendraSearchIntent@, or both when
    -- returning alternative intents in a
    -- <https://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostContent.html PostContent>
    -- or
    -- <https://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostText.html PostText>
    -- response. @AMAZON.FallbackIntent@ is inserted if the confidence score
    -- for all intents is below this value. @AMAZON.KendraSearchIntent@ is only
    -- inserted if it is configured for the bot.
    nluIntentConfidenceThreshold :: Prelude.Maybe Prelude.Double,
    -- | The target locale for the bot.
    locale :: Prelude.Maybe Locale,
    -- | The date that the bot was updated. When you create a resource, the
    -- creation date and last updated date are the same.
    lastUpdatedDate :: Prelude.Maybe Core.POSIX,
    -- | An array of @intent@ objects. For more information, see PutBot.
    intents :: Prelude.Maybe [Intent],
    -- | The status of the bot.
    --
    -- When the status is @BUILDING@ Amazon Lex is building the bot for testing
    -- and use.
    --
    -- If the status of the bot is @READY_BASIC_TESTING@, you can test the bot
    -- using the exact utterances specified in the bot\'s intents. When the bot
    -- is ready for full testing or to run, the status is @READY@.
    --
    -- If there was a problem with building the bot, the status is @FAILED@ and
    -- the @failureReason@ field explains why the bot did not build.
    --
    -- If the bot was saved but not built, the status is @NOT_BUILT@.
    status :: Prelude.Maybe LexStatus,
    -- | A description of the bot.
    description :: Prelude.Maybe Prelude.Text,
    -- | The maximum time in seconds that Amazon Lex retains the data gathered in
    -- a conversation. For more information, see PutBot.
    idleSessionTTLInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | Indicates whether the bot uses accuracy improvements. @true@ indicates
    -- that the bot is using the improvements, otherwise, @false@.
    enableModelImprovements :: Prelude.Maybe Prelude.Bool,
    -- | Checksum of the bot used to identify a specific revision of the bot\'s
    -- @$LATEST@ version.
    checksum :: Prelude.Maybe Prelude.Text,
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
    childDirected :: Prelude.Maybe Prelude.Bool,
    -- | The message that Amazon Lex returns when the user elects to end the
    -- conversation without completing it. For more information, see PutBot.
    abortStatement :: Prelude.Maybe Statement,
    -- | The message Amazon Lex uses when it doesn\'t understand the user\'s
    -- request. For more information, see PutBot.
    clarificationPrompt :: Prelude.Maybe Prompt,
    -- | The date that the bot was created.
    createdDate :: Prelude.Maybe Core.POSIX,
    -- | If @status@ is @FAILED@, Amazon Lex explains why it failed to build the
    -- bot.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The version of the bot. For a new bot, the version is always @$LATEST@.
    version :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'detectSentiment', 'getBotResponse_detectSentiment' - Indicates whether user utterances should be sent to Amazon Comprehend
-- for sentiment analysis.
--
-- 'voiceId', 'getBotResponse_voiceId' - The Amazon Polly voice ID that Amazon Lex uses for voice interaction
-- with the user. For more information, see PutBot.
--
-- 'name', 'getBotResponse_name' - The name of the bot.
--
-- 'nluIntentConfidenceThreshold', 'getBotResponse_nluIntentConfidenceThreshold' - The score that determines where Amazon Lex inserts the
-- @AMAZON.FallbackIntent@, @AMAZON.KendraSearchIntent@, or both when
-- returning alternative intents in a
-- <https://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostContent.html PostContent>
-- or
-- <https://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostText.html PostText>
-- response. @AMAZON.FallbackIntent@ is inserted if the confidence score
-- for all intents is below this value. @AMAZON.KendraSearchIntent@ is only
-- inserted if it is configured for the bot.
--
-- 'locale', 'getBotResponse_locale' - The target locale for the bot.
--
-- 'lastUpdatedDate', 'getBotResponse_lastUpdatedDate' - The date that the bot was updated. When you create a resource, the
-- creation date and last updated date are the same.
--
-- 'intents', 'getBotResponse_intents' - An array of @intent@ objects. For more information, see PutBot.
--
-- 'status', 'getBotResponse_status' - The status of the bot.
--
-- When the status is @BUILDING@ Amazon Lex is building the bot for testing
-- and use.
--
-- If the status of the bot is @READY_BASIC_TESTING@, you can test the bot
-- using the exact utterances specified in the bot\'s intents. When the bot
-- is ready for full testing or to run, the status is @READY@.
--
-- If there was a problem with building the bot, the status is @FAILED@ and
-- the @failureReason@ field explains why the bot did not build.
--
-- If the bot was saved but not built, the status is @NOT_BUILT@.
--
-- 'description', 'getBotResponse_description' - A description of the bot.
--
-- 'idleSessionTTLInSeconds', 'getBotResponse_idleSessionTTLInSeconds' - The maximum time in seconds that Amazon Lex retains the data gathered in
-- a conversation. For more information, see PutBot.
--
-- 'enableModelImprovements', 'getBotResponse_enableModelImprovements' - Indicates whether the bot uses accuracy improvements. @true@ indicates
-- that the bot is using the improvements, otherwise, @false@.
--
-- 'checksum', 'getBotResponse_checksum' - Checksum of the bot used to identify a specific revision of the bot\'s
-- @$LATEST@ version.
--
-- 'childDirected', 'getBotResponse_childDirected' - For each Amazon Lex bot created with the Amazon Lex Model Building
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
-- 'abortStatement', 'getBotResponse_abortStatement' - The message that Amazon Lex returns when the user elects to end the
-- conversation without completing it. For more information, see PutBot.
--
-- 'clarificationPrompt', 'getBotResponse_clarificationPrompt' - The message Amazon Lex uses when it doesn\'t understand the user\'s
-- request. For more information, see PutBot.
--
-- 'createdDate', 'getBotResponse_createdDate' - The date that the bot was created.
--
-- 'failureReason', 'getBotResponse_failureReason' - If @status@ is @FAILED@, Amazon Lex explains why it failed to build the
-- bot.
--
-- 'version', 'getBotResponse_version' - The version of the bot. For a new bot, the version is always @$LATEST@.
--
-- 'httpStatus', 'getBotResponse_httpStatus' - The response's http status code.
newGetBotResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetBotResponse
newGetBotResponse pHttpStatus_ =
  GetBotResponse'
    { detectSentiment = Prelude.Nothing,
      voiceId = Prelude.Nothing,
      name = Prelude.Nothing,
      nluIntentConfidenceThreshold = Prelude.Nothing,
      locale = Prelude.Nothing,
      lastUpdatedDate = Prelude.Nothing,
      intents = Prelude.Nothing,
      status = Prelude.Nothing,
      description = Prelude.Nothing,
      idleSessionTTLInSeconds = Prelude.Nothing,
      enableModelImprovements = Prelude.Nothing,
      checksum = Prelude.Nothing,
      childDirected = Prelude.Nothing,
      abortStatement = Prelude.Nothing,
      clarificationPrompt = Prelude.Nothing,
      createdDate = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      version = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Indicates whether user utterances should be sent to Amazon Comprehend
-- for sentiment analysis.
getBotResponse_detectSentiment :: Lens.Lens' GetBotResponse (Prelude.Maybe Prelude.Bool)
getBotResponse_detectSentiment = Lens.lens (\GetBotResponse' {detectSentiment} -> detectSentiment) (\s@GetBotResponse' {} a -> s {detectSentiment = a} :: GetBotResponse)

-- | The Amazon Polly voice ID that Amazon Lex uses for voice interaction
-- with the user. For more information, see PutBot.
getBotResponse_voiceId :: Lens.Lens' GetBotResponse (Prelude.Maybe Prelude.Text)
getBotResponse_voiceId = Lens.lens (\GetBotResponse' {voiceId} -> voiceId) (\s@GetBotResponse' {} a -> s {voiceId = a} :: GetBotResponse)

-- | The name of the bot.
getBotResponse_name :: Lens.Lens' GetBotResponse (Prelude.Maybe Prelude.Text)
getBotResponse_name = Lens.lens (\GetBotResponse' {name} -> name) (\s@GetBotResponse' {} a -> s {name = a} :: GetBotResponse)

-- | The score that determines where Amazon Lex inserts the
-- @AMAZON.FallbackIntent@, @AMAZON.KendraSearchIntent@, or both when
-- returning alternative intents in a
-- <https://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostContent.html PostContent>
-- or
-- <https://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostText.html PostText>
-- response. @AMAZON.FallbackIntent@ is inserted if the confidence score
-- for all intents is below this value. @AMAZON.KendraSearchIntent@ is only
-- inserted if it is configured for the bot.
getBotResponse_nluIntentConfidenceThreshold :: Lens.Lens' GetBotResponse (Prelude.Maybe Prelude.Double)
getBotResponse_nluIntentConfidenceThreshold = Lens.lens (\GetBotResponse' {nluIntentConfidenceThreshold} -> nluIntentConfidenceThreshold) (\s@GetBotResponse' {} a -> s {nluIntentConfidenceThreshold = a} :: GetBotResponse)

-- | The target locale for the bot.
getBotResponse_locale :: Lens.Lens' GetBotResponse (Prelude.Maybe Locale)
getBotResponse_locale = Lens.lens (\GetBotResponse' {locale} -> locale) (\s@GetBotResponse' {} a -> s {locale = a} :: GetBotResponse)

-- | The date that the bot was updated. When you create a resource, the
-- creation date and last updated date are the same.
getBotResponse_lastUpdatedDate :: Lens.Lens' GetBotResponse (Prelude.Maybe Prelude.UTCTime)
getBotResponse_lastUpdatedDate = Lens.lens (\GetBotResponse' {lastUpdatedDate} -> lastUpdatedDate) (\s@GetBotResponse' {} a -> s {lastUpdatedDate = a} :: GetBotResponse) Prelude.. Lens.mapping Core._Time

-- | An array of @intent@ objects. For more information, see PutBot.
getBotResponse_intents :: Lens.Lens' GetBotResponse (Prelude.Maybe [Intent])
getBotResponse_intents = Lens.lens (\GetBotResponse' {intents} -> intents) (\s@GetBotResponse' {} a -> s {intents = a} :: GetBotResponse) Prelude.. Lens.mapping Lens.coerced

-- | The status of the bot.
--
-- When the status is @BUILDING@ Amazon Lex is building the bot for testing
-- and use.
--
-- If the status of the bot is @READY_BASIC_TESTING@, you can test the bot
-- using the exact utterances specified in the bot\'s intents. When the bot
-- is ready for full testing or to run, the status is @READY@.
--
-- If there was a problem with building the bot, the status is @FAILED@ and
-- the @failureReason@ field explains why the bot did not build.
--
-- If the bot was saved but not built, the status is @NOT_BUILT@.
getBotResponse_status :: Lens.Lens' GetBotResponse (Prelude.Maybe LexStatus)
getBotResponse_status = Lens.lens (\GetBotResponse' {status} -> status) (\s@GetBotResponse' {} a -> s {status = a} :: GetBotResponse)

-- | A description of the bot.
getBotResponse_description :: Lens.Lens' GetBotResponse (Prelude.Maybe Prelude.Text)
getBotResponse_description = Lens.lens (\GetBotResponse' {description} -> description) (\s@GetBotResponse' {} a -> s {description = a} :: GetBotResponse)

-- | The maximum time in seconds that Amazon Lex retains the data gathered in
-- a conversation. For more information, see PutBot.
getBotResponse_idleSessionTTLInSeconds :: Lens.Lens' GetBotResponse (Prelude.Maybe Prelude.Natural)
getBotResponse_idleSessionTTLInSeconds = Lens.lens (\GetBotResponse' {idleSessionTTLInSeconds} -> idleSessionTTLInSeconds) (\s@GetBotResponse' {} a -> s {idleSessionTTLInSeconds = a} :: GetBotResponse)

-- | Indicates whether the bot uses accuracy improvements. @true@ indicates
-- that the bot is using the improvements, otherwise, @false@.
getBotResponse_enableModelImprovements :: Lens.Lens' GetBotResponse (Prelude.Maybe Prelude.Bool)
getBotResponse_enableModelImprovements = Lens.lens (\GetBotResponse' {enableModelImprovements} -> enableModelImprovements) (\s@GetBotResponse' {} a -> s {enableModelImprovements = a} :: GetBotResponse)

-- | Checksum of the bot used to identify a specific revision of the bot\'s
-- @$LATEST@ version.
getBotResponse_checksum :: Lens.Lens' GetBotResponse (Prelude.Maybe Prelude.Text)
getBotResponse_checksum = Lens.lens (\GetBotResponse' {checksum} -> checksum) (\s@GetBotResponse' {} a -> s {checksum = a} :: GetBotResponse)

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
getBotResponse_childDirected :: Lens.Lens' GetBotResponse (Prelude.Maybe Prelude.Bool)
getBotResponse_childDirected = Lens.lens (\GetBotResponse' {childDirected} -> childDirected) (\s@GetBotResponse' {} a -> s {childDirected = a} :: GetBotResponse)

-- | The message that Amazon Lex returns when the user elects to end the
-- conversation without completing it. For more information, see PutBot.
getBotResponse_abortStatement :: Lens.Lens' GetBotResponse (Prelude.Maybe Statement)
getBotResponse_abortStatement = Lens.lens (\GetBotResponse' {abortStatement} -> abortStatement) (\s@GetBotResponse' {} a -> s {abortStatement = a} :: GetBotResponse)

-- | The message Amazon Lex uses when it doesn\'t understand the user\'s
-- request. For more information, see PutBot.
getBotResponse_clarificationPrompt :: Lens.Lens' GetBotResponse (Prelude.Maybe Prompt)
getBotResponse_clarificationPrompt = Lens.lens (\GetBotResponse' {clarificationPrompt} -> clarificationPrompt) (\s@GetBotResponse' {} a -> s {clarificationPrompt = a} :: GetBotResponse)

-- | The date that the bot was created.
getBotResponse_createdDate :: Lens.Lens' GetBotResponse (Prelude.Maybe Prelude.UTCTime)
getBotResponse_createdDate = Lens.lens (\GetBotResponse' {createdDate} -> createdDate) (\s@GetBotResponse' {} a -> s {createdDate = a} :: GetBotResponse) Prelude.. Lens.mapping Core._Time

-- | If @status@ is @FAILED@, Amazon Lex explains why it failed to build the
-- bot.
getBotResponse_failureReason :: Lens.Lens' GetBotResponse (Prelude.Maybe Prelude.Text)
getBotResponse_failureReason = Lens.lens (\GetBotResponse' {failureReason} -> failureReason) (\s@GetBotResponse' {} a -> s {failureReason = a} :: GetBotResponse)

-- | The version of the bot. For a new bot, the version is always @$LATEST@.
getBotResponse_version :: Lens.Lens' GetBotResponse (Prelude.Maybe Prelude.Text)
getBotResponse_version = Lens.lens (\GetBotResponse' {version} -> version) (\s@GetBotResponse' {} a -> s {version = a} :: GetBotResponse)

-- | The response's http status code.
getBotResponse_httpStatus :: Lens.Lens' GetBotResponse Prelude.Int
getBotResponse_httpStatus = Lens.lens (\GetBotResponse' {httpStatus} -> httpStatus) (\s@GetBotResponse' {} a -> s {httpStatus = a} :: GetBotResponse)

instance Prelude.NFData GetBotResponse where
  rnf GetBotResponse' {..} =
    Prelude.rnf detectSentiment
      `Prelude.seq` Prelude.rnf voiceId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf nluIntentConfidenceThreshold
      `Prelude.seq` Prelude.rnf locale
      `Prelude.seq` Prelude.rnf lastUpdatedDate
      `Prelude.seq` Prelude.rnf intents
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf idleSessionTTLInSeconds
      `Prelude.seq` Prelude.rnf enableModelImprovements
      `Prelude.seq` Prelude.rnf checksum
      `Prelude.seq` Prelude.rnf childDirected
      `Prelude.seq` Prelude.rnf abortStatement
      `Prelude.seq` Prelude.rnf clarificationPrompt
      `Prelude.seq` Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf httpStatus
