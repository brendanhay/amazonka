{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.CreateBotVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new version of the bot based on the @> LATEST@ version. If the @> LATEST@ version of this resource hasn't changed since you created the last version, Amazon Lex doesn't create a new version. It returns the last created version.
--
-- When you create the first version of a bot, Amazon Lex sets the version to 1. Subsequent versions increment by 1. For more information, see 'versioning-intro' .
-- This operation requires permission for the @lex:CreateBotVersion@ action.
module Network.AWS.LexModels.CreateBotVersion
  ( -- * Creating a request
    CreateBotVersion (..),
    mkCreateBotVersion,

    -- ** Request lenses
    cbvChecksum,
    cbvName,

    -- * Destructuring the response
    CreateBotVersionResponse (..),
    mkCreateBotVersionResponse,

    -- ** Response lenses
    cbvrsFailureReason,
    cbvrsStatus,
    cbvrsAbortStatement,
    cbvrsIntents,
    cbvrsChecksum,
    cbvrsEnableModelImprovements,
    cbvrsDetectSentiment,
    cbvrsLocale,
    cbvrsCreatedDate,
    cbvrsName,
    cbvrsVersion,
    cbvrsIdleSessionTTLInSeconds,
    cbvrsClarificationPrompt,
    cbvrsVoiceId,
    cbvrsLastUpdatedDate,
    cbvrsChildDirected,
    cbvrsDescription,
    cbvrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateBotVersion' smart constructor.
data CreateBotVersion = CreateBotVersion'
  { checksum ::
      Lude.Maybe Lude.Text,
    name :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateBotVersion' with the minimum fields required to make a request.
--
-- * 'checksum' - Identifies a specific revision of the @> LATEST@ version of the bot. If you specify a checksum and the @> LATEST@ version of the bot has a different checksum, a @PreconditionFailedException@ exception is returned and Amazon Lex doesn't publish a new version. If you don't specify a checksum, Amazon Lex publishes the @> LATEST@ version.
-- * 'name' - The name of the bot that you want to create a new version of. The name is case sensitive.
mkCreateBotVersion ::
  -- | 'name'
  Lude.Text ->
  CreateBotVersion
mkCreateBotVersion pName_ =
  CreateBotVersion' {checksum = Lude.Nothing, name = pName_}

-- | Identifies a specific revision of the @> LATEST@ version of the bot. If you specify a checksum and the @> LATEST@ version of the bot has a different checksum, a @PreconditionFailedException@ exception is returned and Amazon Lex doesn't publish a new version. If you don't specify a checksum, Amazon Lex publishes the @> LATEST@ version.
--
-- /Note:/ Consider using 'checksum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbvChecksum :: Lens.Lens' CreateBotVersion (Lude.Maybe Lude.Text)
cbvChecksum = Lens.lens (checksum :: CreateBotVersion -> Lude.Maybe Lude.Text) (\s a -> s {checksum = a} :: CreateBotVersion)
{-# DEPRECATED cbvChecksum "Use generic-lens or generic-optics with 'checksum' instead." #-}

-- | The name of the bot that you want to create a new version of. The name is case sensitive.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbvName :: Lens.Lens' CreateBotVersion Lude.Text
cbvName = Lens.lens (name :: CreateBotVersion -> Lude.Text) (\s a -> s {name = a} :: CreateBotVersion)
{-# DEPRECATED cbvName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest CreateBotVersion where
  type Rs CreateBotVersion = CreateBotVersionResponse
  request = Req.postJSON lexModelsService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateBotVersionResponse'
            Lude.<$> (x Lude..?> "failureReason")
            Lude.<*> (x Lude..?> "status")
            Lude.<*> (x Lude..?> "abortStatement")
            Lude.<*> (x Lude..?> "intents" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "checksum")
            Lude.<*> (x Lude..?> "enableModelImprovements")
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

instance Lude.ToHeaders CreateBotVersion where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateBotVersion where
  toJSON CreateBotVersion' {..} =
    Lude.object
      (Lude.catMaybes [("checksum" Lude..=) Lude.<$> checksum])

instance Lude.ToPath CreateBotVersion where
  toPath CreateBotVersion' {..} =
    Lude.mconcat ["/bots/", Lude.toBS name, "/versions"]

instance Lude.ToQuery CreateBotVersion where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateBotVersionResponse' smart constructor.
data CreateBotVersionResponse = CreateBotVersionResponse'
  { failureReason ::
      Lude.Maybe Lude.Text,
    status :: Lude.Maybe LexStatus,
    abortStatement :: Lude.Maybe Statement,
    intents :: Lude.Maybe [Intent],
    checksum :: Lude.Maybe Lude.Text,
    enableModelImprovements ::
      Lude.Maybe Lude.Bool,
    detectSentiment :: Lude.Maybe Lude.Bool,
    locale :: Lude.Maybe Locale,
    createdDate :: Lude.Maybe Lude.Timestamp,
    name :: Lude.Maybe Lude.Text,
    version :: Lude.Maybe Lude.Text,
    idleSessionTTLInSeconds ::
      Lude.Maybe Lude.Natural,
    clarificationPrompt :: Lude.Maybe Prompt,
    voiceId :: Lude.Maybe Lude.Text,
    lastUpdatedDate ::
      Lude.Maybe Lude.Timestamp,
    childDirected :: Lude.Maybe Lude.Bool,
    description :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateBotVersionResponse' with the minimum fields required to make a request.
--
-- * 'abortStatement' - The message that Amazon Lex uses to cancel a conversation. For more information, see 'PutBot' .
-- * 'checksum' - Checksum identifying the version of the bot that was created.
-- * 'childDirected' - For each Amazon Lex bot created with the Amazon Lex Model Building Service, you must specify whether your use of Amazon Lex is related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to the Children's Online Privacy Protection Act (COPPA) by specifying @true@ or @false@ in the @childDirected@ field. By specifying @true@ in the @childDirected@ field, you confirm that your use of Amazon Lex __is__ related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA. By specifying @false@ in the @childDirected@ field, you confirm that your use of Amazon Lex __is not__ related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA. You may not specify a default value for the @childDirected@ field that does not accurately reflect whether your use of Amazon Lex is related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA.
--
-- If your use of Amazon Lex relates to a website, program, or other application that is directed in whole or in part, to children under age 13, you must obtain any required verifiable parental consent under COPPA. For information regarding the use of Amazon Lex in connection with websites, programs, or other applications that are directed or targeted, in whole or in part, to children under age 13, see the <https://aws.amazon.com/lex/faqs#data-security Amazon Lex FAQ.>
-- * 'clarificationPrompt' - The message that Amazon Lex uses when it doesn't understand the user's request. For more information, see 'PutBot' .
-- * 'createdDate' - The date when the bot version was created.
-- * 'description' - A description of the bot.
-- * 'detectSentiment' - Indicates whether utterances entered by the user should be sent to Amazon Comprehend for sentiment analysis.
-- * 'enableModelImprovements' - Indicates whether the bot uses accuracy improvements. @true@ indicates that the bot is using the improvements, otherwise, @false@ .
-- * 'failureReason' - If @status@ is @FAILED@ , Amazon Lex provides the reason that it failed to build the bot.
-- * 'idleSessionTTLInSeconds' - The maximum time in seconds that Amazon Lex retains the data gathered in a conversation. For more information, see 'PutBot' .
-- * 'intents' - An array of @Intent@ objects. For more information, see 'PutBot' .
-- * 'lastUpdatedDate' - The date when the @> LATEST@ version of this bot was updated.
-- * 'locale' - Specifies the target locale for the bot.
-- * 'name' - The name of the bot.
-- * 'responseStatus' - The response status code.
-- * 'status' - When you send a request to create or update a bot, Amazon Lex sets the @status@ response element to @BUILDING@ . After Amazon Lex builds the bot, it sets @status@ to @READY@ . If Amazon Lex can't build the bot, it sets @status@ to @FAILED@ . Amazon Lex returns the reason for the failure in the @failureReason@ response element.
-- * 'version' - The version of the bot.
-- * 'voiceId' - The Amazon Polly voice ID that Amazon Lex uses for voice interactions with the user.
mkCreateBotVersionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateBotVersionResponse
mkCreateBotVersionResponse pResponseStatus_ =
  CreateBotVersionResponse'
    { failureReason = Lude.Nothing,
      status = Lude.Nothing,
      abortStatement = Lude.Nothing,
      intents = Lude.Nothing,
      checksum = Lude.Nothing,
      enableModelImprovements = Lude.Nothing,
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

-- | If @status@ is @FAILED@ , Amazon Lex provides the reason that it failed to build the bot.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbvrsFailureReason :: Lens.Lens' CreateBotVersionResponse (Lude.Maybe Lude.Text)
cbvrsFailureReason = Lens.lens (failureReason :: CreateBotVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {failureReason = a} :: CreateBotVersionResponse)
{-# DEPRECATED cbvrsFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | When you send a request to create or update a bot, Amazon Lex sets the @status@ response element to @BUILDING@ . After Amazon Lex builds the bot, it sets @status@ to @READY@ . If Amazon Lex can't build the bot, it sets @status@ to @FAILED@ . Amazon Lex returns the reason for the failure in the @failureReason@ response element.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbvrsStatus :: Lens.Lens' CreateBotVersionResponse (Lude.Maybe LexStatus)
cbvrsStatus = Lens.lens (status :: CreateBotVersionResponse -> Lude.Maybe LexStatus) (\s a -> s {status = a} :: CreateBotVersionResponse)
{-# DEPRECATED cbvrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The message that Amazon Lex uses to cancel a conversation. For more information, see 'PutBot' .
--
-- /Note:/ Consider using 'abortStatement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbvrsAbortStatement :: Lens.Lens' CreateBotVersionResponse (Lude.Maybe Statement)
cbvrsAbortStatement = Lens.lens (abortStatement :: CreateBotVersionResponse -> Lude.Maybe Statement) (\s a -> s {abortStatement = a} :: CreateBotVersionResponse)
{-# DEPRECATED cbvrsAbortStatement "Use generic-lens or generic-optics with 'abortStatement' instead." #-}

-- | An array of @Intent@ objects. For more information, see 'PutBot' .
--
-- /Note:/ Consider using 'intents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbvrsIntents :: Lens.Lens' CreateBotVersionResponse (Lude.Maybe [Intent])
cbvrsIntents = Lens.lens (intents :: CreateBotVersionResponse -> Lude.Maybe [Intent]) (\s a -> s {intents = a} :: CreateBotVersionResponse)
{-# DEPRECATED cbvrsIntents "Use generic-lens or generic-optics with 'intents' instead." #-}

-- | Checksum identifying the version of the bot that was created.
--
-- /Note:/ Consider using 'checksum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbvrsChecksum :: Lens.Lens' CreateBotVersionResponse (Lude.Maybe Lude.Text)
cbvrsChecksum = Lens.lens (checksum :: CreateBotVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {checksum = a} :: CreateBotVersionResponse)
{-# DEPRECATED cbvrsChecksum "Use generic-lens or generic-optics with 'checksum' instead." #-}

-- | Indicates whether the bot uses accuracy improvements. @true@ indicates that the bot is using the improvements, otherwise, @false@ .
--
-- /Note:/ Consider using 'enableModelImprovements' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbvrsEnableModelImprovements :: Lens.Lens' CreateBotVersionResponse (Lude.Maybe Lude.Bool)
cbvrsEnableModelImprovements = Lens.lens (enableModelImprovements :: CreateBotVersionResponse -> Lude.Maybe Lude.Bool) (\s a -> s {enableModelImprovements = a} :: CreateBotVersionResponse)
{-# DEPRECATED cbvrsEnableModelImprovements "Use generic-lens or generic-optics with 'enableModelImprovements' instead." #-}

-- | Indicates whether utterances entered by the user should be sent to Amazon Comprehend for sentiment analysis.
--
-- /Note:/ Consider using 'detectSentiment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbvrsDetectSentiment :: Lens.Lens' CreateBotVersionResponse (Lude.Maybe Lude.Bool)
cbvrsDetectSentiment = Lens.lens (detectSentiment :: CreateBotVersionResponse -> Lude.Maybe Lude.Bool) (\s a -> s {detectSentiment = a} :: CreateBotVersionResponse)
{-# DEPRECATED cbvrsDetectSentiment "Use generic-lens or generic-optics with 'detectSentiment' instead." #-}

-- | Specifies the target locale for the bot.
--
-- /Note:/ Consider using 'locale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbvrsLocale :: Lens.Lens' CreateBotVersionResponse (Lude.Maybe Locale)
cbvrsLocale = Lens.lens (locale :: CreateBotVersionResponse -> Lude.Maybe Locale) (\s a -> s {locale = a} :: CreateBotVersionResponse)
{-# DEPRECATED cbvrsLocale "Use generic-lens or generic-optics with 'locale' instead." #-}

-- | The date when the bot version was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbvrsCreatedDate :: Lens.Lens' CreateBotVersionResponse (Lude.Maybe Lude.Timestamp)
cbvrsCreatedDate = Lens.lens (createdDate :: CreateBotVersionResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdDate = a} :: CreateBotVersionResponse)
{-# DEPRECATED cbvrsCreatedDate "Use generic-lens or generic-optics with 'createdDate' instead." #-}

-- | The name of the bot.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbvrsName :: Lens.Lens' CreateBotVersionResponse (Lude.Maybe Lude.Text)
cbvrsName = Lens.lens (name :: CreateBotVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: CreateBotVersionResponse)
{-# DEPRECATED cbvrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The version of the bot.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbvrsVersion :: Lens.Lens' CreateBotVersionResponse (Lude.Maybe Lude.Text)
cbvrsVersion = Lens.lens (version :: CreateBotVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: CreateBotVersionResponse)
{-# DEPRECATED cbvrsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The maximum time in seconds that Amazon Lex retains the data gathered in a conversation. For more information, see 'PutBot' .
--
-- /Note:/ Consider using 'idleSessionTTLInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbvrsIdleSessionTTLInSeconds :: Lens.Lens' CreateBotVersionResponse (Lude.Maybe Lude.Natural)
cbvrsIdleSessionTTLInSeconds = Lens.lens (idleSessionTTLInSeconds :: CreateBotVersionResponse -> Lude.Maybe Lude.Natural) (\s a -> s {idleSessionTTLInSeconds = a} :: CreateBotVersionResponse)
{-# DEPRECATED cbvrsIdleSessionTTLInSeconds "Use generic-lens or generic-optics with 'idleSessionTTLInSeconds' instead." #-}

-- | The message that Amazon Lex uses when it doesn't understand the user's request. For more information, see 'PutBot' .
--
-- /Note:/ Consider using 'clarificationPrompt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbvrsClarificationPrompt :: Lens.Lens' CreateBotVersionResponse (Lude.Maybe Prompt)
cbvrsClarificationPrompt = Lens.lens (clarificationPrompt :: CreateBotVersionResponse -> Lude.Maybe Prompt) (\s a -> s {clarificationPrompt = a} :: CreateBotVersionResponse)
{-# DEPRECATED cbvrsClarificationPrompt "Use generic-lens or generic-optics with 'clarificationPrompt' instead." #-}

-- | The Amazon Polly voice ID that Amazon Lex uses for voice interactions with the user.
--
-- /Note:/ Consider using 'voiceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbvrsVoiceId :: Lens.Lens' CreateBotVersionResponse (Lude.Maybe Lude.Text)
cbvrsVoiceId = Lens.lens (voiceId :: CreateBotVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {voiceId = a} :: CreateBotVersionResponse)
{-# DEPRECATED cbvrsVoiceId "Use generic-lens or generic-optics with 'voiceId' instead." #-}

-- | The date when the @> LATEST@ version of this bot was updated.
--
-- /Note:/ Consider using 'lastUpdatedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbvrsLastUpdatedDate :: Lens.Lens' CreateBotVersionResponse (Lude.Maybe Lude.Timestamp)
cbvrsLastUpdatedDate = Lens.lens (lastUpdatedDate :: CreateBotVersionResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdatedDate = a} :: CreateBotVersionResponse)
{-# DEPRECATED cbvrsLastUpdatedDate "Use generic-lens or generic-optics with 'lastUpdatedDate' instead." #-}

-- | For each Amazon Lex bot created with the Amazon Lex Model Building Service, you must specify whether your use of Amazon Lex is related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to the Children's Online Privacy Protection Act (COPPA) by specifying @true@ or @false@ in the @childDirected@ field. By specifying @true@ in the @childDirected@ field, you confirm that your use of Amazon Lex __is__ related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA. By specifying @false@ in the @childDirected@ field, you confirm that your use of Amazon Lex __is not__ related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA. You may not specify a default value for the @childDirected@ field that does not accurately reflect whether your use of Amazon Lex is related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA.
--
-- If your use of Amazon Lex relates to a website, program, or other application that is directed in whole or in part, to children under age 13, you must obtain any required verifiable parental consent under COPPA. For information regarding the use of Amazon Lex in connection with websites, programs, or other applications that are directed or targeted, in whole or in part, to children under age 13, see the <https://aws.amazon.com/lex/faqs#data-security Amazon Lex FAQ.>
--
-- /Note:/ Consider using 'childDirected' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbvrsChildDirected :: Lens.Lens' CreateBotVersionResponse (Lude.Maybe Lude.Bool)
cbvrsChildDirected = Lens.lens (childDirected :: CreateBotVersionResponse -> Lude.Maybe Lude.Bool) (\s a -> s {childDirected = a} :: CreateBotVersionResponse)
{-# DEPRECATED cbvrsChildDirected "Use generic-lens or generic-optics with 'childDirected' instead." #-}

-- | A description of the bot.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbvrsDescription :: Lens.Lens' CreateBotVersionResponse (Lude.Maybe Lude.Text)
cbvrsDescription = Lens.lens (description :: CreateBotVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateBotVersionResponse)
{-# DEPRECATED cbvrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbvrsResponseStatus :: Lens.Lens' CreateBotVersionResponse Lude.Int
cbvrsResponseStatus = Lens.lens (responseStatus :: CreateBotVersionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateBotVersionResponse)
{-# DEPRECATED cbvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
