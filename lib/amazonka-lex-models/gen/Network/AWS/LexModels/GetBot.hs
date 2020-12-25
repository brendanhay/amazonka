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
    gbName,
    gbVersionOrAlias,

    -- * Destructuring the response
    GetBotResponse (..),
    mkGetBotResponse,

    -- ** Response lenses
    gbrrsAbortStatement,
    gbrrsChecksum,
    gbrrsChildDirected,
    gbrrsClarificationPrompt,
    gbrrsCreatedDate,
    gbrrsDescription,
    gbrrsDetectSentiment,
    gbrrsEnableModelImprovements,
    gbrrsFailureReason,
    gbrrsIdleSessionTTLInSeconds,
    gbrrsIntents,
    gbrrsLastUpdatedDate,
    gbrrsLocale,
    gbrrsName,
    gbrrsNluIntentConfidenceThreshold,
    gbrrsStatus,
    gbrrsVersion,
    gbrrsVoiceId,
    gbrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexModels.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetBot' smart constructor.
data GetBot = GetBot'
  { -- | The name of the bot. The name is case sensitive.
    name :: Types.BotName,
    -- | The version or alias of the bot.
    versionOrAlias :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetBot' value with any optional fields omitted.
mkGetBot ::
  -- | 'name'
  Types.BotName ->
  -- | 'versionOrAlias'
  Types.String ->
  GetBot
mkGetBot name versionOrAlias = GetBot' {name, versionOrAlias}

-- | The name of the bot. The name is case sensitive.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbName :: Lens.Lens' GetBot Types.BotName
gbName = Lens.field @"name"
{-# DEPRECATED gbName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The version or alias of the bot.
--
-- /Note:/ Consider using 'versionOrAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbVersionOrAlias :: Lens.Lens' GetBot Types.String
gbVersionOrAlias = Lens.field @"versionOrAlias"
{-# DEPRECATED gbVersionOrAlias "Use generic-lens or generic-optics with 'versionOrAlias' instead." #-}

instance Core.AWSRequest GetBot where
  type Rs GetBot = GetBotResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/bots/" Core.<> (Core.toText name) Core.<> ("/versions/")
                Core.<> (Core.toText versionOrAlias)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBotResponse'
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
            Core.<*> (x Core..:? "nluIntentConfidenceThreshold")
            Core.<*> (x Core..:? "status")
            Core.<*> (x Core..:? "version")
            Core.<*> (x Core..:? "voiceId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetBotResponse' smart constructor.
data GetBotResponse = GetBotResponse'
  { -- | The message that Amazon Lex returns when the user elects to end the conversation without completing it. For more information, see 'PutBot' .
    abortStatement :: Core.Maybe Types.Statement,
    -- | Checksum of the bot used to identify a specific revision of the bot's @> LATEST@ version.
    checksum :: Core.Maybe Types.String,
    -- | For each Amazon Lex bot created with the Amazon Lex Model Building Service, you must specify whether your use of Amazon Lex is related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to the Children's Online Privacy Protection Act (COPPA) by specifying @true@ or @false@ in the @childDirected@ field. By specifying @true@ in the @childDirected@ field, you confirm that your use of Amazon Lex __is__ related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA. By specifying @false@ in the @childDirected@ field, you confirm that your use of Amazon Lex __is not__ related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA. You may not specify a default value for the @childDirected@ field that does not accurately reflect whether your use of Amazon Lex is related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA.
    --
    -- If your use of Amazon Lex relates to a website, program, or other application that is directed in whole or in part, to children under age 13, you must obtain any required verifiable parental consent under COPPA. For information regarding the use of Amazon Lex in connection with websites, programs, or other applications that are directed or targeted, in whole or in part, to children under age 13, see the <https://aws.amazon.com/lex/faqs#data-security Amazon Lex FAQ.>
    childDirected :: Core.Maybe Core.Bool,
    -- | The message Amazon Lex uses when it doesn't understand the user's request. For more information, see 'PutBot' .
    clarificationPrompt :: Core.Maybe Types.Prompt,
    -- | The date that the bot was created.
    createdDate :: Core.Maybe Core.NominalDiffTime,
    -- | A description of the bot.
    description :: Core.Maybe Types.Description,
    -- | Indicates whether user utterances should be sent to Amazon Comprehend for sentiment analysis.
    detectSentiment :: Core.Maybe Core.Bool,
    -- | Indicates whether the bot uses accuracy improvements. @true@ indicates that the bot is using the improvements, otherwise, @false@ .
    enableModelImprovements :: Core.Maybe Core.Bool,
    -- | If @status@ is @FAILED@ , Amazon Lex explains why it failed to build the bot.
    failureReason :: Core.Maybe Types.String,
    -- | The maximum time in seconds that Amazon Lex retains the data gathered in a conversation. For more information, see 'PutBot' .
    idleSessionTTLInSeconds :: Core.Maybe Core.Natural,
    -- | An array of @intent@ objects. For more information, see 'PutBot' .
    intents :: Core.Maybe [Types.Intent],
    -- | The date that the bot was updated. When you create a resource, the creation date and last updated date are the same.
    lastUpdatedDate :: Core.Maybe Core.NominalDiffTime,
    -- | The target locale for the bot.
    locale :: Core.Maybe Types.Locale,
    -- | The name of the bot.
    name :: Core.Maybe Types.BotName,
    -- | The score that determines where Amazon Lex inserts the @AMAZON.FallbackIntent@ , @AMAZON.KendraSearchIntent@ , or both when returning alternative intents in a <https://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostContent.html PostContent> or <https://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostText.html PostText> response. @AMAZON.FallbackIntent@ is inserted if the confidence score for all intents is below this value. @AMAZON.KendraSearchIntent@ is only inserted if it is configured for the bot.
    nluIntentConfidenceThreshold :: Core.Maybe Core.Double,
    -- | The status of the bot.
    --
    -- When the status is @BUILDING@ Amazon Lex is building the bot for testing and use.
    -- If the status of the bot is @READY_BASIC_TESTING@ , you can test the bot using the exact utterances specified in the bot's intents. When the bot is ready for full testing or to run, the status is @READY@ .
    -- If there was a problem with building the bot, the status is @FAILED@ and the @failureReason@ field explains why the bot did not build.
    -- If the bot was saved but not built, the status is @NOT_BUILT@ .
    status :: Core.Maybe Types.LexStatus,
    -- | The version of the bot. For a new bot, the version is always @> LATEST@ .
    version :: Core.Maybe Types.Version,
    -- | The Amazon Polly voice ID that Amazon Lex uses for voice interaction with the user. For more information, see 'PutBot' .
    voiceId :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetBotResponse' value with any optional fields omitted.
mkGetBotResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetBotResponse
mkGetBotResponse responseStatus =
  GetBotResponse'
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
      nluIntentConfidenceThreshold = Core.Nothing,
      status = Core.Nothing,
      version = Core.Nothing,
      voiceId = Core.Nothing,
      responseStatus
    }

-- | The message that Amazon Lex returns when the user elects to end the conversation without completing it. For more information, see 'PutBot' .
--
-- /Note:/ Consider using 'abortStatement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrrsAbortStatement :: Lens.Lens' GetBotResponse (Core.Maybe Types.Statement)
gbrrsAbortStatement = Lens.field @"abortStatement"
{-# DEPRECATED gbrrsAbortStatement "Use generic-lens or generic-optics with 'abortStatement' instead." #-}

-- | Checksum of the bot used to identify a specific revision of the bot's @> LATEST@ version.
--
-- /Note:/ Consider using 'checksum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrrsChecksum :: Lens.Lens' GetBotResponse (Core.Maybe Types.String)
gbrrsChecksum = Lens.field @"checksum"
{-# DEPRECATED gbrrsChecksum "Use generic-lens or generic-optics with 'checksum' instead." #-}

-- | For each Amazon Lex bot created with the Amazon Lex Model Building Service, you must specify whether your use of Amazon Lex is related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to the Children's Online Privacy Protection Act (COPPA) by specifying @true@ or @false@ in the @childDirected@ field. By specifying @true@ in the @childDirected@ field, you confirm that your use of Amazon Lex __is__ related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA. By specifying @false@ in the @childDirected@ field, you confirm that your use of Amazon Lex __is not__ related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA. You may not specify a default value for the @childDirected@ field that does not accurately reflect whether your use of Amazon Lex is related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA.
--
-- If your use of Amazon Lex relates to a website, program, or other application that is directed in whole or in part, to children under age 13, you must obtain any required verifiable parental consent under COPPA. For information regarding the use of Amazon Lex in connection with websites, programs, or other applications that are directed or targeted, in whole or in part, to children under age 13, see the <https://aws.amazon.com/lex/faqs#data-security Amazon Lex FAQ.>
--
-- /Note:/ Consider using 'childDirected' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrrsChildDirected :: Lens.Lens' GetBotResponse (Core.Maybe Core.Bool)
gbrrsChildDirected = Lens.field @"childDirected"
{-# DEPRECATED gbrrsChildDirected "Use generic-lens or generic-optics with 'childDirected' instead." #-}

-- | The message Amazon Lex uses when it doesn't understand the user's request. For more information, see 'PutBot' .
--
-- /Note:/ Consider using 'clarificationPrompt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrrsClarificationPrompt :: Lens.Lens' GetBotResponse (Core.Maybe Types.Prompt)
gbrrsClarificationPrompt = Lens.field @"clarificationPrompt"
{-# DEPRECATED gbrrsClarificationPrompt "Use generic-lens or generic-optics with 'clarificationPrompt' instead." #-}

-- | The date that the bot was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrrsCreatedDate :: Lens.Lens' GetBotResponse (Core.Maybe Core.NominalDiffTime)
gbrrsCreatedDate = Lens.field @"createdDate"
{-# DEPRECATED gbrrsCreatedDate "Use generic-lens or generic-optics with 'createdDate' instead." #-}

-- | A description of the bot.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrrsDescription :: Lens.Lens' GetBotResponse (Core.Maybe Types.Description)
gbrrsDescription = Lens.field @"description"
{-# DEPRECATED gbrrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Indicates whether user utterances should be sent to Amazon Comprehend for sentiment analysis.
--
-- /Note:/ Consider using 'detectSentiment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrrsDetectSentiment :: Lens.Lens' GetBotResponse (Core.Maybe Core.Bool)
gbrrsDetectSentiment = Lens.field @"detectSentiment"
{-# DEPRECATED gbrrsDetectSentiment "Use generic-lens or generic-optics with 'detectSentiment' instead." #-}

-- | Indicates whether the bot uses accuracy improvements. @true@ indicates that the bot is using the improvements, otherwise, @false@ .
--
-- /Note:/ Consider using 'enableModelImprovements' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrrsEnableModelImprovements :: Lens.Lens' GetBotResponse (Core.Maybe Core.Bool)
gbrrsEnableModelImprovements = Lens.field @"enableModelImprovements"
{-# DEPRECATED gbrrsEnableModelImprovements "Use generic-lens or generic-optics with 'enableModelImprovements' instead." #-}

-- | If @status@ is @FAILED@ , Amazon Lex explains why it failed to build the bot.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrrsFailureReason :: Lens.Lens' GetBotResponse (Core.Maybe Types.String)
gbrrsFailureReason = Lens.field @"failureReason"
{-# DEPRECATED gbrrsFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | The maximum time in seconds that Amazon Lex retains the data gathered in a conversation. For more information, see 'PutBot' .
--
-- /Note:/ Consider using 'idleSessionTTLInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrrsIdleSessionTTLInSeconds :: Lens.Lens' GetBotResponse (Core.Maybe Core.Natural)
gbrrsIdleSessionTTLInSeconds = Lens.field @"idleSessionTTLInSeconds"
{-# DEPRECATED gbrrsIdleSessionTTLInSeconds "Use generic-lens or generic-optics with 'idleSessionTTLInSeconds' instead." #-}

-- | An array of @intent@ objects. For more information, see 'PutBot' .
--
-- /Note:/ Consider using 'intents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrrsIntents :: Lens.Lens' GetBotResponse (Core.Maybe [Types.Intent])
gbrrsIntents = Lens.field @"intents"
{-# DEPRECATED gbrrsIntents "Use generic-lens or generic-optics with 'intents' instead." #-}

-- | The date that the bot was updated. When you create a resource, the creation date and last updated date are the same.
--
-- /Note:/ Consider using 'lastUpdatedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrrsLastUpdatedDate :: Lens.Lens' GetBotResponse (Core.Maybe Core.NominalDiffTime)
gbrrsLastUpdatedDate = Lens.field @"lastUpdatedDate"
{-# DEPRECATED gbrrsLastUpdatedDate "Use generic-lens or generic-optics with 'lastUpdatedDate' instead." #-}

-- | The target locale for the bot.
--
-- /Note:/ Consider using 'locale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrrsLocale :: Lens.Lens' GetBotResponse (Core.Maybe Types.Locale)
gbrrsLocale = Lens.field @"locale"
{-# DEPRECATED gbrrsLocale "Use generic-lens or generic-optics with 'locale' instead." #-}

-- | The name of the bot.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrrsName :: Lens.Lens' GetBotResponse (Core.Maybe Types.BotName)
gbrrsName = Lens.field @"name"
{-# DEPRECATED gbrrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The score that determines where Amazon Lex inserts the @AMAZON.FallbackIntent@ , @AMAZON.KendraSearchIntent@ , or both when returning alternative intents in a <https://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostContent.html PostContent> or <https://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostText.html PostText> response. @AMAZON.FallbackIntent@ is inserted if the confidence score for all intents is below this value. @AMAZON.KendraSearchIntent@ is only inserted if it is configured for the bot.
--
-- /Note:/ Consider using 'nluIntentConfidenceThreshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrrsNluIntentConfidenceThreshold :: Lens.Lens' GetBotResponse (Core.Maybe Core.Double)
gbrrsNluIntentConfidenceThreshold = Lens.field @"nluIntentConfidenceThreshold"
{-# DEPRECATED gbrrsNluIntentConfidenceThreshold "Use generic-lens or generic-optics with 'nluIntentConfidenceThreshold' instead." #-}

-- | The status of the bot.
--
-- When the status is @BUILDING@ Amazon Lex is building the bot for testing and use.
-- If the status of the bot is @READY_BASIC_TESTING@ , you can test the bot using the exact utterances specified in the bot's intents. When the bot is ready for full testing or to run, the status is @READY@ .
-- If there was a problem with building the bot, the status is @FAILED@ and the @failureReason@ field explains why the bot did not build.
-- If the bot was saved but not built, the status is @NOT_BUILT@ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrrsStatus :: Lens.Lens' GetBotResponse (Core.Maybe Types.LexStatus)
gbrrsStatus = Lens.field @"status"
{-# DEPRECATED gbrrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The version of the bot. For a new bot, the version is always @> LATEST@ .
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrrsVersion :: Lens.Lens' GetBotResponse (Core.Maybe Types.Version)
gbrrsVersion = Lens.field @"version"
{-# DEPRECATED gbrrsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The Amazon Polly voice ID that Amazon Lex uses for voice interaction with the user. For more information, see 'PutBot' .
--
-- /Note:/ Consider using 'voiceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrrsVoiceId :: Lens.Lens' GetBotResponse (Core.Maybe Types.String)
gbrrsVoiceId = Lens.field @"voiceId"
{-# DEPRECATED gbrrsVoiceId "Use generic-lens or generic-optics with 'voiceId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrrsResponseStatus :: Lens.Lens' GetBotResponse Core.Int
gbrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gbrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
