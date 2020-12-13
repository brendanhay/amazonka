{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.GetBot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns metadata information for a specific bot. You must provide the bot name and the bot version or alias.
--
-- This operation requires permissions for the @lex:GetBot@ action.
module Network.AWS.LexModels.GetBot
  ( -- * Creating a request
    GetBot (..),
    mkGetBot,

    -- ** Request lenses
    gbVersionOrAlias,
    gbName,

    -- * Destructuring the response
    GetBotResponse (..),
    mkGetBotResponse,

    -- ** Response lenses
    gbrsFailureReason,
    gbrsStatus,
    gbrsAbortStatement,
    gbrsIntents,
    gbrsChecksum,
    gbrsEnableModelImprovements,
    gbrsNluIntentConfidenceThreshold,
    gbrsDetectSentiment,
    gbrsLocale,
    gbrsCreatedDate,
    gbrsName,
    gbrsVersion,
    gbrsIdleSessionTTLInSeconds,
    gbrsClarificationPrompt,
    gbrsVoiceId,
    gbrsLastUpdatedDate,
    gbrsChildDirected,
    gbrsDescription,
    gbrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetBot' smart constructor.
data GetBot = GetBot'
  { -- | The version or alias of the bot.
    versionOrAlias :: Lude.Text,
    -- | The name of the bot. The name is case sensitive.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetBot' with the minimum fields required to make a request.
--
-- * 'versionOrAlias' - The version or alias of the bot.
-- * 'name' - The name of the bot. The name is case sensitive.
mkGetBot ::
  -- | 'versionOrAlias'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  GetBot
mkGetBot pVersionOrAlias_ pName_ =
  GetBot' {versionOrAlias = pVersionOrAlias_, name = pName_}

-- | The version or alias of the bot.
--
-- /Note:/ Consider using 'versionOrAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbVersionOrAlias :: Lens.Lens' GetBot Lude.Text
gbVersionOrAlias = Lens.lens (versionOrAlias :: GetBot -> Lude.Text) (\s a -> s {versionOrAlias = a} :: GetBot)
{-# DEPRECATED gbVersionOrAlias "Use generic-lens or generic-optics with 'versionOrAlias' instead." #-}

-- | The name of the bot. The name is case sensitive.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbName :: Lens.Lens' GetBot Lude.Text
gbName = Lens.lens (name :: GetBot -> Lude.Text) (\s a -> s {name = a} :: GetBot)
{-# DEPRECATED gbName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest GetBot where
  type Rs GetBot = GetBotResponse
  request = Req.get lexModelsService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetBotResponse'
            Lude.<$> (x Lude..?> "failureReason")
            Lude.<*> (x Lude..?> "status")
            Lude.<*> (x Lude..?> "abortStatement")
            Lude.<*> (x Lude..?> "intents" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "checksum")
            Lude.<*> (x Lude..?> "enableModelImprovements")
            Lude.<*> (x Lude..?> "nluIntentConfidenceThreshold")
            Lude.<*> (x Lude..?> "detectSentiment")
            Lude.<*> (x Lude..?> "locale")
            Lude.<*> (x Lude..?> "createdDate")
            Lude.<*> (x Lude..?> "name")
            Lude.<*> (x Lude..?> "version")
            Lude.<*> (x Lude..?> "idleSessionTTLInSeconds")
            Lude.<*> (x Lude..?> "clarificationPrompt")
            Lude.<*> (x Lude..?> "voiceId")
            Lude.<*> (x Lude..?> "lastUpdatedDate")
            Lude.<*> (x Lude..?> "childDirected")
            Lude.<*> (x Lude..?> "description")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetBot where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetBot where
  toPath GetBot' {..} =
    Lude.mconcat
      ["/bots/", Lude.toBS name, "/versions/", Lude.toBS versionOrAlias]

instance Lude.ToQuery GetBot where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetBotResponse' smart constructor.
data GetBotResponse = GetBotResponse'
  { -- | If @status@ is @FAILED@ , Amazon Lex explains why it failed to build the bot.
    failureReason :: Lude.Maybe Lude.Text,
    -- | The status of the bot.
    --
    -- When the status is @BUILDING@ Amazon Lex is building the bot for testing and use.
    -- If the status of the bot is @READY_BASIC_TESTING@ , you can test the bot using the exact utterances specified in the bot's intents. When the bot is ready for full testing or to run, the status is @READY@ .
    -- If there was a problem with building the bot, the status is @FAILED@ and the @failureReason@ field explains why the bot did not build.
    -- If the bot was saved but not built, the status is @NOT_BUILT@ .
    status :: Lude.Maybe LexStatus,
    -- | The message that Amazon Lex returns when the user elects to end the conversation without completing it. For more information, see 'PutBot' .
    abortStatement :: Lude.Maybe Statement,
    -- | An array of @intent@ objects. For more information, see 'PutBot' .
    intents :: Lude.Maybe [Intent],
    -- | Checksum of the bot used to identify a specific revision of the bot's @> LATEST@ version.
    checksum :: Lude.Maybe Lude.Text,
    -- | Indicates whether the bot uses accuracy improvements. @true@ indicates that the bot is using the improvements, otherwise, @false@ .
    enableModelImprovements :: Lude.Maybe Lude.Bool,
    -- | The score that determines where Amazon Lex inserts the @AMAZON.FallbackIntent@ , @AMAZON.KendraSearchIntent@ , or both when returning alternative intents in a <https://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostContent.html PostContent> or <https://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostText.html PostText> response. @AMAZON.FallbackIntent@ is inserted if the confidence score for all intents is below this value. @AMAZON.KendraSearchIntent@ is only inserted if it is configured for the bot.
    nluIntentConfidenceThreshold :: Lude.Maybe Lude.Double,
    -- | Indicates whether user utterances should be sent to Amazon Comprehend for sentiment analysis.
    detectSentiment :: Lude.Maybe Lude.Bool,
    -- | The target locale for the bot.
    locale :: Lude.Maybe Locale,
    -- | The date that the bot was created.
    createdDate :: Lude.Maybe Lude.Timestamp,
    -- | The name of the bot.
    name :: Lude.Maybe Lude.Text,
    -- | The version of the bot. For a new bot, the version is always @> LATEST@ .
    version :: Lude.Maybe Lude.Text,
    -- | The maximum time in seconds that Amazon Lex retains the data gathered in a conversation. For more information, see 'PutBot' .
    idleSessionTTLInSeconds :: Lude.Maybe Lude.Natural,
    -- | The message Amazon Lex uses when it doesn't understand the user's request. For more information, see 'PutBot' .
    clarificationPrompt :: Lude.Maybe Prompt,
    -- | The Amazon Polly voice ID that Amazon Lex uses for voice interaction with the user. For more information, see 'PutBot' .
    voiceId :: Lude.Maybe Lude.Text,
    -- | The date that the bot was updated. When you create a resource, the creation date and last updated date are the same.
    lastUpdatedDate :: Lude.Maybe Lude.Timestamp,
    -- | For each Amazon Lex bot created with the Amazon Lex Model Building Service, you must specify whether your use of Amazon Lex is related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to the Children's Online Privacy Protection Act (COPPA) by specifying @true@ or @false@ in the @childDirected@ field. By specifying @true@ in the @childDirected@ field, you confirm that your use of Amazon Lex __is__ related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA. By specifying @false@ in the @childDirected@ field, you confirm that your use of Amazon Lex __is not__ related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA. You may not specify a default value for the @childDirected@ field that does not accurately reflect whether your use of Amazon Lex is related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA.
    --
    -- If your use of Amazon Lex relates to a website, program, or other application that is directed in whole or in part, to children under age 13, you must obtain any required verifiable parental consent under COPPA. For information regarding the use of Amazon Lex in connection with websites, programs, or other applications that are directed or targeted, in whole or in part, to children under age 13, see the <https://aws.amazon.com/lex/faqs#data-security Amazon Lex FAQ.>
    childDirected :: Lude.Maybe Lude.Bool,
    -- | A description of the bot.
    description :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetBotResponse' with the minimum fields required to make a request.
--
-- * 'failureReason' - If @status@ is @FAILED@ , Amazon Lex explains why it failed to build the bot.
-- * 'status' - The status of the bot.
--
-- When the status is @BUILDING@ Amazon Lex is building the bot for testing and use.
-- If the status of the bot is @READY_BASIC_TESTING@ , you can test the bot using the exact utterances specified in the bot's intents. When the bot is ready for full testing or to run, the status is @READY@ .
-- If there was a problem with building the bot, the status is @FAILED@ and the @failureReason@ field explains why the bot did not build.
-- If the bot was saved but not built, the status is @NOT_BUILT@ .
-- * 'abortStatement' - The message that Amazon Lex returns when the user elects to end the conversation without completing it. For more information, see 'PutBot' .
-- * 'intents' - An array of @intent@ objects. For more information, see 'PutBot' .
-- * 'checksum' - Checksum of the bot used to identify a specific revision of the bot's @> LATEST@ version.
-- * 'enableModelImprovements' - Indicates whether the bot uses accuracy improvements. @true@ indicates that the bot is using the improvements, otherwise, @false@ .
-- * 'nluIntentConfidenceThreshold' - The score that determines where Amazon Lex inserts the @AMAZON.FallbackIntent@ , @AMAZON.KendraSearchIntent@ , or both when returning alternative intents in a <https://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostContent.html PostContent> or <https://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostText.html PostText> response. @AMAZON.FallbackIntent@ is inserted if the confidence score for all intents is below this value. @AMAZON.KendraSearchIntent@ is only inserted if it is configured for the bot.
-- * 'detectSentiment' - Indicates whether user utterances should be sent to Amazon Comprehend for sentiment analysis.
-- * 'locale' - The target locale for the bot.
-- * 'createdDate' - The date that the bot was created.
-- * 'name' - The name of the bot.
-- * 'version' - The version of the bot. For a new bot, the version is always @> LATEST@ .
-- * 'idleSessionTTLInSeconds' - The maximum time in seconds that Amazon Lex retains the data gathered in a conversation. For more information, see 'PutBot' .
-- * 'clarificationPrompt' - The message Amazon Lex uses when it doesn't understand the user's request. For more information, see 'PutBot' .
-- * 'voiceId' - The Amazon Polly voice ID that Amazon Lex uses for voice interaction with the user. For more information, see 'PutBot' .
-- * 'lastUpdatedDate' - The date that the bot was updated. When you create a resource, the creation date and last updated date are the same.
-- * 'childDirected' - For each Amazon Lex bot created with the Amazon Lex Model Building Service, you must specify whether your use of Amazon Lex is related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to the Children's Online Privacy Protection Act (COPPA) by specifying @true@ or @false@ in the @childDirected@ field. By specifying @true@ in the @childDirected@ field, you confirm that your use of Amazon Lex __is__ related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA. By specifying @false@ in the @childDirected@ field, you confirm that your use of Amazon Lex __is not__ related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA. You may not specify a default value for the @childDirected@ field that does not accurately reflect whether your use of Amazon Lex is related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA.
--
-- If your use of Amazon Lex relates to a website, program, or other application that is directed in whole or in part, to children under age 13, you must obtain any required verifiable parental consent under COPPA. For information regarding the use of Amazon Lex in connection with websites, programs, or other applications that are directed or targeted, in whole or in part, to children under age 13, see the <https://aws.amazon.com/lex/faqs#data-security Amazon Lex FAQ.>
-- * 'description' - A description of the bot.
-- * 'responseStatus' - The response status code.
mkGetBotResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetBotResponse
mkGetBotResponse pResponseStatus_ =
  GetBotResponse'
    { failureReason = Lude.Nothing,
      status = Lude.Nothing,
      abortStatement = Lude.Nothing,
      intents = Lude.Nothing,
      checksum = Lude.Nothing,
      enableModelImprovements = Lude.Nothing,
      nluIntentConfidenceThreshold = Lude.Nothing,
      detectSentiment = Lude.Nothing,
      locale = Lude.Nothing,
      createdDate = Lude.Nothing,
      name = Lude.Nothing,
      version = Lude.Nothing,
      idleSessionTTLInSeconds = Lude.Nothing,
      clarificationPrompt = Lude.Nothing,
      voiceId = Lude.Nothing,
      lastUpdatedDate = Lude.Nothing,
      childDirected = Lude.Nothing,
      description = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If @status@ is @FAILED@ , Amazon Lex explains why it failed to build the bot.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrsFailureReason :: Lens.Lens' GetBotResponse (Lude.Maybe Lude.Text)
gbrsFailureReason = Lens.lens (failureReason :: GetBotResponse -> Lude.Maybe Lude.Text) (\s a -> s {failureReason = a} :: GetBotResponse)
{-# DEPRECATED gbrsFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | The status of the bot.
--
-- When the status is @BUILDING@ Amazon Lex is building the bot for testing and use.
-- If the status of the bot is @READY_BASIC_TESTING@ , you can test the bot using the exact utterances specified in the bot's intents. When the bot is ready for full testing or to run, the status is @READY@ .
-- If there was a problem with building the bot, the status is @FAILED@ and the @failureReason@ field explains why the bot did not build.
-- If the bot was saved but not built, the status is @NOT_BUILT@ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrsStatus :: Lens.Lens' GetBotResponse (Lude.Maybe LexStatus)
gbrsStatus = Lens.lens (status :: GetBotResponse -> Lude.Maybe LexStatus) (\s a -> s {status = a} :: GetBotResponse)
{-# DEPRECATED gbrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The message that Amazon Lex returns when the user elects to end the conversation without completing it. For more information, see 'PutBot' .
--
-- /Note:/ Consider using 'abortStatement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrsAbortStatement :: Lens.Lens' GetBotResponse (Lude.Maybe Statement)
gbrsAbortStatement = Lens.lens (abortStatement :: GetBotResponse -> Lude.Maybe Statement) (\s a -> s {abortStatement = a} :: GetBotResponse)
{-# DEPRECATED gbrsAbortStatement "Use generic-lens or generic-optics with 'abortStatement' instead." #-}

-- | An array of @intent@ objects. For more information, see 'PutBot' .
--
-- /Note:/ Consider using 'intents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrsIntents :: Lens.Lens' GetBotResponse (Lude.Maybe [Intent])
gbrsIntents = Lens.lens (intents :: GetBotResponse -> Lude.Maybe [Intent]) (\s a -> s {intents = a} :: GetBotResponse)
{-# DEPRECATED gbrsIntents "Use generic-lens or generic-optics with 'intents' instead." #-}

-- | Checksum of the bot used to identify a specific revision of the bot's @> LATEST@ version.
--
-- /Note:/ Consider using 'checksum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrsChecksum :: Lens.Lens' GetBotResponse (Lude.Maybe Lude.Text)
gbrsChecksum = Lens.lens (checksum :: GetBotResponse -> Lude.Maybe Lude.Text) (\s a -> s {checksum = a} :: GetBotResponse)
{-# DEPRECATED gbrsChecksum "Use generic-lens or generic-optics with 'checksum' instead." #-}

-- | Indicates whether the bot uses accuracy improvements. @true@ indicates that the bot is using the improvements, otherwise, @false@ .
--
-- /Note:/ Consider using 'enableModelImprovements' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrsEnableModelImprovements :: Lens.Lens' GetBotResponse (Lude.Maybe Lude.Bool)
gbrsEnableModelImprovements = Lens.lens (enableModelImprovements :: GetBotResponse -> Lude.Maybe Lude.Bool) (\s a -> s {enableModelImprovements = a} :: GetBotResponse)
{-# DEPRECATED gbrsEnableModelImprovements "Use generic-lens or generic-optics with 'enableModelImprovements' instead." #-}

-- | The score that determines where Amazon Lex inserts the @AMAZON.FallbackIntent@ , @AMAZON.KendraSearchIntent@ , or both when returning alternative intents in a <https://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostContent.html PostContent> or <https://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostText.html PostText> response. @AMAZON.FallbackIntent@ is inserted if the confidence score for all intents is below this value. @AMAZON.KendraSearchIntent@ is only inserted if it is configured for the bot.
--
-- /Note:/ Consider using 'nluIntentConfidenceThreshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrsNluIntentConfidenceThreshold :: Lens.Lens' GetBotResponse (Lude.Maybe Lude.Double)
gbrsNluIntentConfidenceThreshold = Lens.lens (nluIntentConfidenceThreshold :: GetBotResponse -> Lude.Maybe Lude.Double) (\s a -> s {nluIntentConfidenceThreshold = a} :: GetBotResponse)
{-# DEPRECATED gbrsNluIntentConfidenceThreshold "Use generic-lens or generic-optics with 'nluIntentConfidenceThreshold' instead." #-}

-- | Indicates whether user utterances should be sent to Amazon Comprehend for sentiment analysis.
--
-- /Note:/ Consider using 'detectSentiment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrsDetectSentiment :: Lens.Lens' GetBotResponse (Lude.Maybe Lude.Bool)
gbrsDetectSentiment = Lens.lens (detectSentiment :: GetBotResponse -> Lude.Maybe Lude.Bool) (\s a -> s {detectSentiment = a} :: GetBotResponse)
{-# DEPRECATED gbrsDetectSentiment "Use generic-lens or generic-optics with 'detectSentiment' instead." #-}

-- | The target locale for the bot.
--
-- /Note:/ Consider using 'locale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrsLocale :: Lens.Lens' GetBotResponse (Lude.Maybe Locale)
gbrsLocale = Lens.lens (locale :: GetBotResponse -> Lude.Maybe Locale) (\s a -> s {locale = a} :: GetBotResponse)
{-# DEPRECATED gbrsLocale "Use generic-lens or generic-optics with 'locale' instead." #-}

-- | The date that the bot was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrsCreatedDate :: Lens.Lens' GetBotResponse (Lude.Maybe Lude.Timestamp)
gbrsCreatedDate = Lens.lens (createdDate :: GetBotResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdDate = a} :: GetBotResponse)
{-# DEPRECATED gbrsCreatedDate "Use generic-lens or generic-optics with 'createdDate' instead." #-}

-- | The name of the bot.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrsName :: Lens.Lens' GetBotResponse (Lude.Maybe Lude.Text)
gbrsName = Lens.lens (name :: GetBotResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: GetBotResponse)
{-# DEPRECATED gbrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The version of the bot. For a new bot, the version is always @> LATEST@ .
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrsVersion :: Lens.Lens' GetBotResponse (Lude.Maybe Lude.Text)
gbrsVersion = Lens.lens (version :: GetBotResponse -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: GetBotResponse)
{-# DEPRECATED gbrsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The maximum time in seconds that Amazon Lex retains the data gathered in a conversation. For more information, see 'PutBot' .
--
-- /Note:/ Consider using 'idleSessionTTLInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrsIdleSessionTTLInSeconds :: Lens.Lens' GetBotResponse (Lude.Maybe Lude.Natural)
gbrsIdleSessionTTLInSeconds = Lens.lens (idleSessionTTLInSeconds :: GetBotResponse -> Lude.Maybe Lude.Natural) (\s a -> s {idleSessionTTLInSeconds = a} :: GetBotResponse)
{-# DEPRECATED gbrsIdleSessionTTLInSeconds "Use generic-lens or generic-optics with 'idleSessionTTLInSeconds' instead." #-}

-- | The message Amazon Lex uses when it doesn't understand the user's request. For more information, see 'PutBot' .
--
-- /Note:/ Consider using 'clarificationPrompt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrsClarificationPrompt :: Lens.Lens' GetBotResponse (Lude.Maybe Prompt)
gbrsClarificationPrompt = Lens.lens (clarificationPrompt :: GetBotResponse -> Lude.Maybe Prompt) (\s a -> s {clarificationPrompt = a} :: GetBotResponse)
{-# DEPRECATED gbrsClarificationPrompt "Use generic-lens or generic-optics with 'clarificationPrompt' instead." #-}

-- | The Amazon Polly voice ID that Amazon Lex uses for voice interaction with the user. For more information, see 'PutBot' .
--
-- /Note:/ Consider using 'voiceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrsVoiceId :: Lens.Lens' GetBotResponse (Lude.Maybe Lude.Text)
gbrsVoiceId = Lens.lens (voiceId :: GetBotResponse -> Lude.Maybe Lude.Text) (\s a -> s {voiceId = a} :: GetBotResponse)
{-# DEPRECATED gbrsVoiceId "Use generic-lens or generic-optics with 'voiceId' instead." #-}

-- | The date that the bot was updated. When you create a resource, the creation date and last updated date are the same.
--
-- /Note:/ Consider using 'lastUpdatedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrsLastUpdatedDate :: Lens.Lens' GetBotResponse (Lude.Maybe Lude.Timestamp)
gbrsLastUpdatedDate = Lens.lens (lastUpdatedDate :: GetBotResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdatedDate = a} :: GetBotResponse)
{-# DEPRECATED gbrsLastUpdatedDate "Use generic-lens or generic-optics with 'lastUpdatedDate' instead." #-}

-- | For each Amazon Lex bot created with the Amazon Lex Model Building Service, you must specify whether your use of Amazon Lex is related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to the Children's Online Privacy Protection Act (COPPA) by specifying @true@ or @false@ in the @childDirected@ field. By specifying @true@ in the @childDirected@ field, you confirm that your use of Amazon Lex __is__ related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA. By specifying @false@ in the @childDirected@ field, you confirm that your use of Amazon Lex __is not__ related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA. You may not specify a default value for the @childDirected@ field that does not accurately reflect whether your use of Amazon Lex is related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA.
--
-- If your use of Amazon Lex relates to a website, program, or other application that is directed in whole or in part, to children under age 13, you must obtain any required verifiable parental consent under COPPA. For information regarding the use of Amazon Lex in connection with websites, programs, or other applications that are directed or targeted, in whole or in part, to children under age 13, see the <https://aws.amazon.com/lex/faqs#data-security Amazon Lex FAQ.>
--
-- /Note:/ Consider using 'childDirected' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrsChildDirected :: Lens.Lens' GetBotResponse (Lude.Maybe Lude.Bool)
gbrsChildDirected = Lens.lens (childDirected :: GetBotResponse -> Lude.Maybe Lude.Bool) (\s a -> s {childDirected = a} :: GetBotResponse)
{-# DEPRECATED gbrsChildDirected "Use generic-lens or generic-optics with 'childDirected' instead." #-}

-- | A description of the bot.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrsDescription :: Lens.Lens' GetBotResponse (Lude.Maybe Lude.Text)
gbrsDescription = Lens.lens (description :: GetBotResponse -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: GetBotResponse)
{-# DEPRECATED gbrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrsResponseStatus :: Lens.Lens' GetBotResponse Lude.Int
gbrsResponseStatus = Lens.lens (responseStatus :: GetBotResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetBotResponse)
{-# DEPRECATED gbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
