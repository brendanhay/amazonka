{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    cbvName,
    cbvChecksum,

    -- * Destructuring the response
    CreateBotVersionResponse (..),
    mkCreateBotVersionResponse,

    -- ** Response lenses
    cbvrrsAbortStatement,
    cbvrrsChecksum,
    cbvrrsChildDirected,
    cbvrrsClarificationPrompt,
    cbvrrsCreatedDate,
    cbvrrsDescription,
    cbvrrsDetectSentiment,
    cbvrrsEnableModelImprovements,
    cbvrrsFailureReason,
    cbvrrsIdleSessionTTLInSeconds,
    cbvrrsIntents,
    cbvrrsLastUpdatedDate,
    cbvrrsLocale,
    cbvrrsName,
    cbvrrsStatus,
    cbvrrsVersion,
    cbvrrsVoiceId,
    cbvrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexModels.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateBotVersion' smart constructor.
data CreateBotVersion = CreateBotVersion'
  { -- | The name of the bot that you want to create a new version of. The name is case sensitive.
    name :: Types.BotName,
    -- | Identifies a specific revision of the @> LATEST@ version of the bot. If you specify a checksum and the @> LATEST@ version of the bot has a different checksum, a @PreconditionFailedException@ exception is returned and Amazon Lex doesn't publish a new version. If you don't specify a checksum, Amazon Lex publishes the @> LATEST@ version.
    checksum :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateBotVersion' value with any optional fields omitted.
mkCreateBotVersion ::
  -- | 'name'
  Types.BotName ->
  CreateBotVersion
mkCreateBotVersion name =
  CreateBotVersion' {name, checksum = Core.Nothing}

-- | The name of the bot that you want to create a new version of. The name is case sensitive.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbvName :: Lens.Lens' CreateBotVersion Types.BotName
cbvName = Lens.field @"name"
{-# DEPRECATED cbvName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Identifies a specific revision of the @> LATEST@ version of the bot. If you specify a checksum and the @> LATEST@ version of the bot has a different checksum, a @PreconditionFailedException@ exception is returned and Amazon Lex doesn't publish a new version. If you don't specify a checksum, Amazon Lex publishes the @> LATEST@ version.
--
-- /Note:/ Consider using 'checksum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbvChecksum :: Lens.Lens' CreateBotVersion (Core.Maybe Types.String)
cbvChecksum = Lens.field @"checksum"
{-# DEPRECATED cbvChecksum "Use generic-lens or generic-optics with 'checksum' instead." #-}

instance Core.FromJSON CreateBotVersion where
  toJSON CreateBotVersion {..} =
    Core.object
      (Core.catMaybes [("checksum" Core..=) Core.<$> checksum])

instance Core.AWSRequest CreateBotVersion where
  type Rs CreateBotVersion = CreateBotVersionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ("/bots/" Core.<> (Core.toText name) Core.<> ("/versions")),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateBotVersionResponse'
            Core.<$> (x Core..:? "abortStatement")
            Core.<*> (x Core..:? "checksum")
            Core.<*> (x Core..:? "childDirected")
            Core.<*> (x Core..:? "clarificationPrompt")
            Core.<*> (x Core..:? "createdDate")
            Core.<*> (x Core..:? "description")
            Core.<*> (x Core..:? "detectSentiment")
            Core.<*> (x Core..:? "enableModelImprovements")
            Core.<*> (x Core..:? "failureReason")
            Core.<*> (x Core..:? "idleSessionTTLInSeconds")
            Core.<*> (x Core..:? "intents")
            Core.<*> (x Core..:? "lastUpdatedDate")
            Core.<*> (x Core..:? "locale")
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "status")
            Core.<*> (x Core..:? "version")
            Core.<*> (x Core..:? "voiceId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateBotVersionResponse' smart constructor.
data CreateBotVersionResponse = CreateBotVersionResponse'
  { -- | The message that Amazon Lex uses to cancel a conversation. For more information, see 'PutBot' .
    abortStatement :: Core.Maybe Types.Statement,
    -- | Checksum identifying the version of the bot that was created.
    checksum :: Core.Maybe Types.String,
    -- | For each Amazon Lex bot created with the Amazon Lex Model Building Service, you must specify whether your use of Amazon Lex is related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to the Children's Online Privacy Protection Act (COPPA) by specifying @true@ or @false@ in the @childDirected@ field. By specifying @true@ in the @childDirected@ field, you confirm that your use of Amazon Lex __is__ related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA. By specifying @false@ in the @childDirected@ field, you confirm that your use of Amazon Lex __is not__ related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA. You may not specify a default value for the @childDirected@ field that does not accurately reflect whether your use of Amazon Lex is related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA.
    --
    -- If your use of Amazon Lex relates to a website, program, or other application that is directed in whole or in part, to children under age 13, you must obtain any required verifiable parental consent under COPPA. For information regarding the use of Amazon Lex in connection with websites, programs, or other applications that are directed or targeted, in whole or in part, to children under age 13, see the <https://aws.amazon.com/lex/faqs#data-security Amazon Lex FAQ.>
    childDirected :: Core.Maybe Core.Bool,
    -- | The message that Amazon Lex uses when it doesn't understand the user's request. For more information, see 'PutBot' .
    clarificationPrompt :: Core.Maybe Types.Prompt,
    -- | The date when the bot version was created.
    createdDate :: Core.Maybe Core.NominalDiffTime,
    -- | A description of the bot.
    description :: Core.Maybe Types.Description,
    -- | Indicates whether utterances entered by the user should be sent to Amazon Comprehend for sentiment analysis.
    detectSentiment :: Core.Maybe Core.Bool,
    -- | Indicates whether the bot uses accuracy improvements. @true@ indicates that the bot is using the improvements, otherwise, @false@ .
    enableModelImprovements :: Core.Maybe Core.Bool,
    -- | If @status@ is @FAILED@ , Amazon Lex provides the reason that it failed to build the bot.
    failureReason :: Core.Maybe Types.String,
    -- | The maximum time in seconds that Amazon Lex retains the data gathered in a conversation. For more information, see 'PutBot' .
    idleSessionTTLInSeconds :: Core.Maybe Core.Natural,
    -- | An array of @Intent@ objects. For more information, see 'PutBot' .
    intents :: Core.Maybe [Types.Intent],
    -- | The date when the @> LATEST@ version of this bot was updated.
    lastUpdatedDate :: Core.Maybe Core.NominalDiffTime,
    -- | Specifies the target locale for the bot.
    locale :: Core.Maybe Types.Locale,
    -- | The name of the bot.
    name :: Core.Maybe Types.BotName,
    -- | When you send a request to create or update a bot, Amazon Lex sets the @status@ response element to @BUILDING@ . After Amazon Lex builds the bot, it sets @status@ to @READY@ . If Amazon Lex can't build the bot, it sets @status@ to @FAILED@ . Amazon Lex returns the reason for the failure in the @failureReason@ response element.
    status :: Core.Maybe Types.LexStatus,
    -- | The version of the bot.
    version :: Core.Maybe Types.Version,
    -- | The Amazon Polly voice ID that Amazon Lex uses for voice interactions with the user.
    voiceId :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateBotVersionResponse' value with any optional fields omitted.
mkCreateBotVersionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateBotVersionResponse
mkCreateBotVersionResponse responseStatus =
  CreateBotVersionResponse'
    { abortStatement = Core.Nothing,
      checksum = Core.Nothing,
      childDirected = Core.Nothing,
      clarificationPrompt = Core.Nothing,
      createdDate = Core.Nothing,
      description = Core.Nothing,
      detectSentiment = Core.Nothing,
      enableModelImprovements = Core.Nothing,
      failureReason = Core.Nothing,
      idleSessionTTLInSeconds = Core.Nothing,
      intents = Core.Nothing,
      lastUpdatedDate = Core.Nothing,
      locale = Core.Nothing,
      name = Core.Nothing,
      status = Core.Nothing,
      version = Core.Nothing,
      voiceId = Core.Nothing,
      responseStatus
    }

-- | The message that Amazon Lex uses to cancel a conversation. For more information, see 'PutBot' .
--
-- /Note:/ Consider using 'abortStatement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbvrrsAbortStatement :: Lens.Lens' CreateBotVersionResponse (Core.Maybe Types.Statement)
cbvrrsAbortStatement = Lens.field @"abortStatement"
{-# DEPRECATED cbvrrsAbortStatement "Use generic-lens or generic-optics with 'abortStatement' instead." #-}

-- | Checksum identifying the version of the bot that was created.
--
-- /Note:/ Consider using 'checksum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbvrrsChecksum :: Lens.Lens' CreateBotVersionResponse (Core.Maybe Types.String)
cbvrrsChecksum = Lens.field @"checksum"
{-# DEPRECATED cbvrrsChecksum "Use generic-lens or generic-optics with 'checksum' instead." #-}

-- | For each Amazon Lex bot created with the Amazon Lex Model Building Service, you must specify whether your use of Amazon Lex is related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to the Children's Online Privacy Protection Act (COPPA) by specifying @true@ or @false@ in the @childDirected@ field. By specifying @true@ in the @childDirected@ field, you confirm that your use of Amazon Lex __is__ related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA. By specifying @false@ in the @childDirected@ field, you confirm that your use of Amazon Lex __is not__ related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA. You may not specify a default value for the @childDirected@ field that does not accurately reflect whether your use of Amazon Lex is related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA.
--
-- If your use of Amazon Lex relates to a website, program, or other application that is directed in whole or in part, to children under age 13, you must obtain any required verifiable parental consent under COPPA. For information regarding the use of Amazon Lex in connection with websites, programs, or other applications that are directed or targeted, in whole or in part, to children under age 13, see the <https://aws.amazon.com/lex/faqs#data-security Amazon Lex FAQ.>
--
-- /Note:/ Consider using 'childDirected' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbvrrsChildDirected :: Lens.Lens' CreateBotVersionResponse (Core.Maybe Core.Bool)
cbvrrsChildDirected = Lens.field @"childDirected"
{-# DEPRECATED cbvrrsChildDirected "Use generic-lens or generic-optics with 'childDirected' instead." #-}

-- | The message that Amazon Lex uses when it doesn't understand the user's request. For more information, see 'PutBot' .
--
-- /Note:/ Consider using 'clarificationPrompt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbvrrsClarificationPrompt :: Lens.Lens' CreateBotVersionResponse (Core.Maybe Types.Prompt)
cbvrrsClarificationPrompt = Lens.field @"clarificationPrompt"
{-# DEPRECATED cbvrrsClarificationPrompt "Use generic-lens or generic-optics with 'clarificationPrompt' instead." #-}

-- | The date when the bot version was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbvrrsCreatedDate :: Lens.Lens' CreateBotVersionResponse (Core.Maybe Core.NominalDiffTime)
cbvrrsCreatedDate = Lens.field @"createdDate"
{-# DEPRECATED cbvrrsCreatedDate "Use generic-lens or generic-optics with 'createdDate' instead." #-}

-- | A description of the bot.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbvrrsDescription :: Lens.Lens' CreateBotVersionResponse (Core.Maybe Types.Description)
cbvrrsDescription = Lens.field @"description"
{-# DEPRECATED cbvrrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Indicates whether utterances entered by the user should be sent to Amazon Comprehend for sentiment analysis.
--
-- /Note:/ Consider using 'detectSentiment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbvrrsDetectSentiment :: Lens.Lens' CreateBotVersionResponse (Core.Maybe Core.Bool)
cbvrrsDetectSentiment = Lens.field @"detectSentiment"
{-# DEPRECATED cbvrrsDetectSentiment "Use generic-lens or generic-optics with 'detectSentiment' instead." #-}

-- | Indicates whether the bot uses accuracy improvements. @true@ indicates that the bot is using the improvements, otherwise, @false@ .
--
-- /Note:/ Consider using 'enableModelImprovements' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbvrrsEnableModelImprovements :: Lens.Lens' CreateBotVersionResponse (Core.Maybe Core.Bool)
cbvrrsEnableModelImprovements = Lens.field @"enableModelImprovements"
{-# DEPRECATED cbvrrsEnableModelImprovements "Use generic-lens or generic-optics with 'enableModelImprovements' instead." #-}

-- | If @status@ is @FAILED@ , Amazon Lex provides the reason that it failed to build the bot.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbvrrsFailureReason :: Lens.Lens' CreateBotVersionResponse (Core.Maybe Types.String)
cbvrrsFailureReason = Lens.field @"failureReason"
{-# DEPRECATED cbvrrsFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | The maximum time in seconds that Amazon Lex retains the data gathered in a conversation. For more information, see 'PutBot' .
--
-- /Note:/ Consider using 'idleSessionTTLInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbvrrsIdleSessionTTLInSeconds :: Lens.Lens' CreateBotVersionResponse (Core.Maybe Core.Natural)
cbvrrsIdleSessionTTLInSeconds = Lens.field @"idleSessionTTLInSeconds"
{-# DEPRECATED cbvrrsIdleSessionTTLInSeconds "Use generic-lens or generic-optics with 'idleSessionTTLInSeconds' instead." #-}

-- | An array of @Intent@ objects. For more information, see 'PutBot' .
--
-- /Note:/ Consider using 'intents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbvrrsIntents :: Lens.Lens' CreateBotVersionResponse (Core.Maybe [Types.Intent])
cbvrrsIntents = Lens.field @"intents"
{-# DEPRECATED cbvrrsIntents "Use generic-lens or generic-optics with 'intents' instead." #-}

-- | The date when the @> LATEST@ version of this bot was updated.
--
-- /Note:/ Consider using 'lastUpdatedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbvrrsLastUpdatedDate :: Lens.Lens' CreateBotVersionResponse (Core.Maybe Core.NominalDiffTime)
cbvrrsLastUpdatedDate = Lens.field @"lastUpdatedDate"
{-# DEPRECATED cbvrrsLastUpdatedDate "Use generic-lens or generic-optics with 'lastUpdatedDate' instead." #-}

-- | Specifies the target locale for the bot.
--
-- /Note:/ Consider using 'locale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbvrrsLocale :: Lens.Lens' CreateBotVersionResponse (Core.Maybe Types.Locale)
cbvrrsLocale = Lens.field @"locale"
{-# DEPRECATED cbvrrsLocale "Use generic-lens or generic-optics with 'locale' instead." #-}

-- | The name of the bot.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbvrrsName :: Lens.Lens' CreateBotVersionResponse (Core.Maybe Types.BotName)
cbvrrsName = Lens.field @"name"
{-# DEPRECATED cbvrrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | When you send a request to create or update a bot, Amazon Lex sets the @status@ response element to @BUILDING@ . After Amazon Lex builds the bot, it sets @status@ to @READY@ . If Amazon Lex can't build the bot, it sets @status@ to @FAILED@ . Amazon Lex returns the reason for the failure in the @failureReason@ response element.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbvrrsStatus :: Lens.Lens' CreateBotVersionResponse (Core.Maybe Types.LexStatus)
cbvrrsStatus = Lens.field @"status"
{-# DEPRECATED cbvrrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The version of the bot.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbvrrsVersion :: Lens.Lens' CreateBotVersionResponse (Core.Maybe Types.Version)
cbvrrsVersion = Lens.field @"version"
{-# DEPRECATED cbvrrsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The Amazon Polly voice ID that Amazon Lex uses for voice interactions with the user.
--
-- /Note:/ Consider using 'voiceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbvrrsVoiceId :: Lens.Lens' CreateBotVersionResponse (Core.Maybe Types.String)
cbvrrsVoiceId = Lens.field @"voiceId"
{-# DEPRECATED cbvrrsVoiceId "Use generic-lens or generic-optics with 'voiceId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbvrrsResponseStatus :: Lens.Lens' CreateBotVersionResponse Core.Int
cbvrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cbvrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
