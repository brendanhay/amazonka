{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.LexModels.GetBot
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns metadata information for a specific bot. You must provide the
-- bot name and the bot version or alias.
--
-- This operation requires permissions for the @lex:GetBot@ action.
module Network.AWS.LexModels.GetBot
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
    getBotResponse_abortStatement,
    getBotResponse_createdDate,
    getBotResponse_status,
    getBotResponse_voiceId,
    getBotResponse_lastUpdatedDate,
    getBotResponse_nluIntentConfidenceThreshold,
    getBotResponse_locale,
    getBotResponse_clarificationPrompt,
    getBotResponse_enableModelImprovements,
    getBotResponse_version,
    getBotResponse_idleSessionTTLInSeconds,
    getBotResponse_name,
    getBotResponse_intents,
    getBotResponse_failureReason,
    getBotResponse_childDirected,
    getBotResponse_description,
    getBotResponse_detectSentiment,
    getBotResponse_checksum,
    getBotResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetBot' smart constructor.
data GetBot = GetBot'
  { -- | The name of the bot. The name is case sensitive.
    name :: Prelude.Text,
    -- | The version or alias of the bot.
    versionOrAlias :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest GetBot where
  type Rs GetBot = GetBotResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBotResponse'
            Prelude.<$> (x Prelude..?> "abortStatement")
            Prelude.<*> (x Prelude..?> "createdDate")
            Prelude.<*> (x Prelude..?> "status")
            Prelude.<*> (x Prelude..?> "voiceId")
            Prelude.<*> (x Prelude..?> "lastUpdatedDate")
            Prelude.<*> (x Prelude..?> "nluIntentConfidenceThreshold")
            Prelude.<*> (x Prelude..?> "locale")
            Prelude.<*> (x Prelude..?> "clarificationPrompt")
            Prelude.<*> (x Prelude..?> "enableModelImprovements")
            Prelude.<*> (x Prelude..?> "version")
            Prelude.<*> (x Prelude..?> "idleSessionTTLInSeconds")
            Prelude.<*> (x Prelude..?> "name")
            Prelude.<*> (x Prelude..?> "intents" Prelude..!@ Prelude.mempty)
            Prelude.<*> (x Prelude..?> "failureReason")
            Prelude.<*> (x Prelude..?> "childDirected")
            Prelude.<*> (x Prelude..?> "description")
            Prelude.<*> (x Prelude..?> "detectSentiment")
            Prelude.<*> (x Prelude..?> "checksum")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetBot

instance Prelude.NFData GetBot

instance Prelude.ToHeaders GetBot where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToPath GetBot where
  toPath GetBot' {..} =
    Prelude.mconcat
      [ "/bots/",
        Prelude.toBS name,
        "/versions/",
        Prelude.toBS versionOrAlias
      ]

instance Prelude.ToQuery GetBot where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetBotResponse' smart constructor.
data GetBotResponse = GetBotResponse'
  { -- | The message that Amazon Lex returns when the user elects to end the
    -- conversation without completing it. For more information, see PutBot.
    abortStatement :: Prelude.Maybe Statement,
    -- | The date that the bot was created.
    createdDate :: Prelude.Maybe Prelude.POSIX,
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
    -- | The Amazon Polly voice ID that Amazon Lex uses for voice interaction
    -- with the user. For more information, see PutBot.
    voiceId :: Prelude.Maybe Prelude.Text,
    -- | The date that the bot was updated. When you create a resource, the
    -- creation date and last updated date are the same.
    lastUpdatedDate :: Prelude.Maybe Prelude.POSIX,
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
    -- | The message Amazon Lex uses when it doesn\'t understand the user\'s
    -- request. For more information, see PutBot.
    clarificationPrompt :: Prelude.Maybe Prompt,
    -- | Indicates whether the bot uses accuracy improvements. @true@ indicates
    -- that the bot is using the improvements, otherwise, @false@.
    enableModelImprovements :: Prelude.Maybe Prelude.Bool,
    -- | The version of the bot. For a new bot, the version is always @$LATEST@.
    version :: Prelude.Maybe Prelude.Text,
    -- | The maximum time in seconds that Amazon Lex retains the data gathered in
    -- a conversation. For more information, see PutBot.
    idleSessionTTLInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The name of the bot.
    name :: Prelude.Maybe Prelude.Text,
    -- | An array of @intent@ objects. For more information, see PutBot.
    intents :: Prelude.Maybe [Intent],
    -- | If @status@ is @FAILED@, Amazon Lex explains why it failed to build the
    -- bot.
    failureReason :: Prelude.Maybe Prelude.Text,
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
    -- | A description of the bot.
    description :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether user utterances should be sent to Amazon Comprehend
    -- for sentiment analysis.
    detectSentiment :: Prelude.Maybe Prelude.Bool,
    -- | Checksum of the bot used to identify a specific revision of the bot\'s
    -- @$LATEST@ version.
    checksum :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetBotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'abortStatement', 'getBotResponse_abortStatement' - The message that Amazon Lex returns when the user elects to end the
-- conversation without completing it. For more information, see PutBot.
--
-- 'createdDate', 'getBotResponse_createdDate' - The date that the bot was created.
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
-- 'voiceId', 'getBotResponse_voiceId' - The Amazon Polly voice ID that Amazon Lex uses for voice interaction
-- with the user. For more information, see PutBot.
--
-- 'lastUpdatedDate', 'getBotResponse_lastUpdatedDate' - The date that the bot was updated. When you create a resource, the
-- creation date and last updated date are the same.
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
-- 'clarificationPrompt', 'getBotResponse_clarificationPrompt' - The message Amazon Lex uses when it doesn\'t understand the user\'s
-- request. For more information, see PutBot.
--
-- 'enableModelImprovements', 'getBotResponse_enableModelImprovements' - Indicates whether the bot uses accuracy improvements. @true@ indicates
-- that the bot is using the improvements, otherwise, @false@.
--
-- 'version', 'getBotResponse_version' - The version of the bot. For a new bot, the version is always @$LATEST@.
--
-- 'idleSessionTTLInSeconds', 'getBotResponse_idleSessionTTLInSeconds' - The maximum time in seconds that Amazon Lex retains the data gathered in
-- a conversation. For more information, see PutBot.
--
-- 'name', 'getBotResponse_name' - The name of the bot.
--
-- 'intents', 'getBotResponse_intents' - An array of @intent@ objects. For more information, see PutBot.
--
-- 'failureReason', 'getBotResponse_failureReason' - If @status@ is @FAILED@, Amazon Lex explains why it failed to build the
-- bot.
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
-- 'description', 'getBotResponse_description' - A description of the bot.
--
-- 'detectSentiment', 'getBotResponse_detectSentiment' - Indicates whether user utterances should be sent to Amazon Comprehend
-- for sentiment analysis.
--
-- 'checksum', 'getBotResponse_checksum' - Checksum of the bot used to identify a specific revision of the bot\'s
-- @$LATEST@ version.
--
-- 'httpStatus', 'getBotResponse_httpStatus' - The response's http status code.
newGetBotResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetBotResponse
newGetBotResponse pHttpStatus_ =
  GetBotResponse'
    { abortStatement = Prelude.Nothing,
      createdDate = Prelude.Nothing,
      status = Prelude.Nothing,
      voiceId = Prelude.Nothing,
      lastUpdatedDate = Prelude.Nothing,
      nluIntentConfidenceThreshold = Prelude.Nothing,
      locale = Prelude.Nothing,
      clarificationPrompt = Prelude.Nothing,
      enableModelImprovements = Prelude.Nothing,
      version = Prelude.Nothing,
      idleSessionTTLInSeconds = Prelude.Nothing,
      name = Prelude.Nothing,
      intents = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      childDirected = Prelude.Nothing,
      description = Prelude.Nothing,
      detectSentiment = Prelude.Nothing,
      checksum = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The message that Amazon Lex returns when the user elects to end the
-- conversation without completing it. For more information, see PutBot.
getBotResponse_abortStatement :: Lens.Lens' GetBotResponse (Prelude.Maybe Statement)
getBotResponse_abortStatement = Lens.lens (\GetBotResponse' {abortStatement} -> abortStatement) (\s@GetBotResponse' {} a -> s {abortStatement = a} :: GetBotResponse)

-- | The date that the bot was created.
getBotResponse_createdDate :: Lens.Lens' GetBotResponse (Prelude.Maybe Prelude.UTCTime)
getBotResponse_createdDate = Lens.lens (\GetBotResponse' {createdDate} -> createdDate) (\s@GetBotResponse' {} a -> s {createdDate = a} :: GetBotResponse) Prelude.. Lens.mapping Prelude._Time

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

-- | The Amazon Polly voice ID that Amazon Lex uses for voice interaction
-- with the user. For more information, see PutBot.
getBotResponse_voiceId :: Lens.Lens' GetBotResponse (Prelude.Maybe Prelude.Text)
getBotResponse_voiceId = Lens.lens (\GetBotResponse' {voiceId} -> voiceId) (\s@GetBotResponse' {} a -> s {voiceId = a} :: GetBotResponse)

-- | The date that the bot was updated. When you create a resource, the
-- creation date and last updated date are the same.
getBotResponse_lastUpdatedDate :: Lens.Lens' GetBotResponse (Prelude.Maybe Prelude.UTCTime)
getBotResponse_lastUpdatedDate = Lens.lens (\GetBotResponse' {lastUpdatedDate} -> lastUpdatedDate) (\s@GetBotResponse' {} a -> s {lastUpdatedDate = a} :: GetBotResponse) Prelude.. Lens.mapping Prelude._Time

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

-- | The message Amazon Lex uses when it doesn\'t understand the user\'s
-- request. For more information, see PutBot.
getBotResponse_clarificationPrompt :: Lens.Lens' GetBotResponse (Prelude.Maybe Prompt)
getBotResponse_clarificationPrompt = Lens.lens (\GetBotResponse' {clarificationPrompt} -> clarificationPrompt) (\s@GetBotResponse' {} a -> s {clarificationPrompt = a} :: GetBotResponse)

-- | Indicates whether the bot uses accuracy improvements. @true@ indicates
-- that the bot is using the improvements, otherwise, @false@.
getBotResponse_enableModelImprovements :: Lens.Lens' GetBotResponse (Prelude.Maybe Prelude.Bool)
getBotResponse_enableModelImprovements = Lens.lens (\GetBotResponse' {enableModelImprovements} -> enableModelImprovements) (\s@GetBotResponse' {} a -> s {enableModelImprovements = a} :: GetBotResponse)

-- | The version of the bot. For a new bot, the version is always @$LATEST@.
getBotResponse_version :: Lens.Lens' GetBotResponse (Prelude.Maybe Prelude.Text)
getBotResponse_version = Lens.lens (\GetBotResponse' {version} -> version) (\s@GetBotResponse' {} a -> s {version = a} :: GetBotResponse)

-- | The maximum time in seconds that Amazon Lex retains the data gathered in
-- a conversation. For more information, see PutBot.
getBotResponse_idleSessionTTLInSeconds :: Lens.Lens' GetBotResponse (Prelude.Maybe Prelude.Natural)
getBotResponse_idleSessionTTLInSeconds = Lens.lens (\GetBotResponse' {idleSessionTTLInSeconds} -> idleSessionTTLInSeconds) (\s@GetBotResponse' {} a -> s {idleSessionTTLInSeconds = a} :: GetBotResponse)

-- | The name of the bot.
getBotResponse_name :: Lens.Lens' GetBotResponse (Prelude.Maybe Prelude.Text)
getBotResponse_name = Lens.lens (\GetBotResponse' {name} -> name) (\s@GetBotResponse' {} a -> s {name = a} :: GetBotResponse)

-- | An array of @intent@ objects. For more information, see PutBot.
getBotResponse_intents :: Lens.Lens' GetBotResponse (Prelude.Maybe [Intent])
getBotResponse_intents = Lens.lens (\GetBotResponse' {intents} -> intents) (\s@GetBotResponse' {} a -> s {intents = a} :: GetBotResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | If @status@ is @FAILED@, Amazon Lex explains why it failed to build the
-- bot.
getBotResponse_failureReason :: Lens.Lens' GetBotResponse (Prelude.Maybe Prelude.Text)
getBotResponse_failureReason = Lens.lens (\GetBotResponse' {failureReason} -> failureReason) (\s@GetBotResponse' {} a -> s {failureReason = a} :: GetBotResponse)

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

-- | A description of the bot.
getBotResponse_description :: Lens.Lens' GetBotResponse (Prelude.Maybe Prelude.Text)
getBotResponse_description = Lens.lens (\GetBotResponse' {description} -> description) (\s@GetBotResponse' {} a -> s {description = a} :: GetBotResponse)

-- | Indicates whether user utterances should be sent to Amazon Comprehend
-- for sentiment analysis.
getBotResponse_detectSentiment :: Lens.Lens' GetBotResponse (Prelude.Maybe Prelude.Bool)
getBotResponse_detectSentiment = Lens.lens (\GetBotResponse' {detectSentiment} -> detectSentiment) (\s@GetBotResponse' {} a -> s {detectSentiment = a} :: GetBotResponse)

-- | Checksum of the bot used to identify a specific revision of the bot\'s
-- @$LATEST@ version.
getBotResponse_checksum :: Lens.Lens' GetBotResponse (Prelude.Maybe Prelude.Text)
getBotResponse_checksum = Lens.lens (\GetBotResponse' {checksum} -> checksum) (\s@GetBotResponse' {} a -> s {checksum = a} :: GetBotResponse)

-- | The response's http status code.
getBotResponse_httpStatus :: Lens.Lens' GetBotResponse Prelude.Int
getBotResponse_httpStatus = Lens.lens (\GetBotResponse' {httpStatus} -> httpStatus) (\s@GetBotResponse' {} a -> s {httpStatus = a} :: GetBotResponse)

instance Prelude.NFData GetBotResponse
