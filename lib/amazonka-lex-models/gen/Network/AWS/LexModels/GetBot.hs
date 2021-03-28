{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetBot (..)
    , mkGetBot
    -- ** Request lenses
    , gbName
    , gbVersionOrAlias

    -- * Destructuring the response
    , GetBotResponse (..)
    , mkGetBotResponse
    -- ** Response lenses
    , gbrrsAbortStatement
    , gbrrsChecksum
    , gbrrsChildDirected
    , gbrrsClarificationPrompt
    , gbrrsCreatedDate
    , gbrrsDescription
    , gbrrsDetectSentiment
    , gbrrsEnableModelImprovements
    , gbrrsFailureReason
    , gbrrsIdleSessionTTLInSeconds
    , gbrrsIntents
    , gbrrsLastUpdatedDate
    , gbrrsLocale
    , gbrrsName
    , gbrrsNluIntentConfidenceThreshold
    , gbrrsStatus
    , gbrrsVersion
    , gbrrsVoiceId
    , gbrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexModels.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetBot' smart constructor.
data GetBot = GetBot'
  { name :: Types.BotName
    -- ^ The name of the bot. The name is case sensitive. 
  , versionOrAlias :: Core.Text
    -- ^ The version or alias of the bot.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetBot' value with any optional fields omitted.
mkGetBot
    :: Types.BotName -- ^ 'name'
    -> Core.Text -- ^ 'versionOrAlias'
    -> GetBot
mkGetBot name versionOrAlias = GetBot'{name, versionOrAlias}

-- | The name of the bot. The name is case sensitive. 
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbName :: Lens.Lens' GetBot Types.BotName
gbName = Lens.field @"name"
{-# INLINEABLE gbName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The version or alias of the bot.
--
-- /Note:/ Consider using 'versionOrAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbVersionOrAlias :: Lens.Lens' GetBot Core.Text
gbVersionOrAlias = Lens.field @"versionOrAlias"
{-# INLINEABLE gbVersionOrAlias #-}
{-# DEPRECATED versionOrAlias "Use generic-lens or generic-optics with 'versionOrAlias' instead"  #-}

instance Core.ToQuery GetBot where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetBot where
        toHeaders GetBot{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest GetBot where
        type Rs GetBot = GetBotResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/bots/" Core.<> Core.toText name Core.<> "/versions/" Core.<>
                             Core.toText versionOrAlias,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetBotResponse' Core.<$>
                   (x Core..:? "abortStatement") Core.<*> x Core..:? "checksum"
                     Core.<*> x Core..:? "childDirected"
                     Core.<*> x Core..:? "clarificationPrompt"
                     Core.<*> x Core..:? "createdDate"
                     Core.<*> x Core..:? "description"
                     Core.<*> x Core..:? "detectSentiment"
                     Core.<*> x Core..:? "enableModelImprovements"
                     Core.<*> x Core..:? "failureReason"
                     Core.<*> x Core..:? "idleSessionTTLInSeconds"
                     Core.<*> x Core..:? "intents"
                     Core.<*> x Core..:? "lastUpdatedDate"
                     Core.<*> x Core..:? "locale"
                     Core.<*> x Core..:? "name"
                     Core.<*> x Core..:? "nluIntentConfidenceThreshold"
                     Core.<*> x Core..:? "status"
                     Core.<*> x Core..:? "version"
                     Core.<*> x Core..:? "voiceId"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetBotResponse' smart constructor.
data GetBotResponse = GetBotResponse'
  { abortStatement :: Core.Maybe Types.Statement
    -- ^ The message that Amazon Lex returns when the user elects to end the conversation without completing it. For more information, see 'PutBot' .
  , checksum :: Core.Maybe Core.Text
    -- ^ Checksum of the bot used to identify a specific revision of the bot's @> LATEST@ version.
  , childDirected :: Core.Maybe Core.Bool
    -- ^ For each Amazon Lex bot created with the Amazon Lex Model Building Service, you must specify whether your use of Amazon Lex is related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to the Children's Online Privacy Protection Act (COPPA) by specifying @true@ or @false@ in the @childDirected@ field. By specifying @true@ in the @childDirected@ field, you confirm that your use of Amazon Lex __is__ related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA. By specifying @false@ in the @childDirected@ field, you confirm that your use of Amazon Lex __is not__ related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA. You may not specify a default value for the @childDirected@ field that does not accurately reflect whether your use of Amazon Lex is related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA.
--
-- If your use of Amazon Lex relates to a website, program, or other application that is directed in whole or in part, to children under age 13, you must obtain any required verifiable parental consent under COPPA. For information regarding the use of Amazon Lex in connection with websites, programs, or other applications that are directed or targeted, in whole or in part, to children under age 13, see the <https://aws.amazon.com/lex/faqs#data-security Amazon Lex FAQ.> 
  , clarificationPrompt :: Core.Maybe Types.Prompt
    -- ^ The message Amazon Lex uses when it doesn't understand the user's request. For more information, see 'PutBot' . 
  , createdDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date that the bot was created.
  , description :: Core.Maybe Types.Description
    -- ^ A description of the bot.
  , detectSentiment :: Core.Maybe Core.Bool
    -- ^ Indicates whether user utterances should be sent to Amazon Comprehend for sentiment analysis.
  , enableModelImprovements :: Core.Maybe Core.Bool
    -- ^ Indicates whether the bot uses accuracy improvements. @true@ indicates that the bot is using the improvements, otherwise, @false@ .
  , failureReason :: Core.Maybe Core.Text
    -- ^ If @status@ is @FAILED@ , Amazon Lex explains why it failed to build the bot.
  , idleSessionTTLInSeconds :: Core.Maybe Core.Natural
    -- ^ The maximum time in seconds that Amazon Lex retains the data gathered in a conversation. For more information, see 'PutBot' .
  , intents :: Core.Maybe [Types.Intent]
    -- ^ An array of @intent@ objects. For more information, see 'PutBot' .
  , lastUpdatedDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date that the bot was updated. When you create a resource, the creation date and last updated date are the same. 
  , locale :: Core.Maybe Types.Locale
    -- ^ The target locale for the bot. 
  , name :: Core.Maybe Types.BotName
    -- ^ The name of the bot.
  , nluIntentConfidenceThreshold :: Core.Maybe Core.Double
    -- ^ The score that determines where Amazon Lex inserts the @AMAZON.FallbackIntent@ , @AMAZON.KendraSearchIntent@ , or both when returning alternative intents in a <https://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostContent.html PostContent> or <https://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostText.html PostText> response. @AMAZON.FallbackIntent@ is inserted if the confidence score for all intents is below this value. @AMAZON.KendraSearchIntent@ is only inserted if it is configured for the bot.
  , status :: Core.Maybe Types.LexStatus
    -- ^ The status of the bot. 
--
-- When the status is @BUILDING@ Amazon Lex is building the bot for testing and use.
-- If the status of the bot is @READY_BASIC_TESTING@ , you can test the bot using the exact utterances specified in the bot's intents. When the bot is ready for full testing or to run, the status is @READY@ .
-- If there was a problem with building the bot, the status is @FAILED@ and the @failureReason@ field explains why the bot did not build.
-- If the bot was saved but not built, the status is @NOT_BUILT@ .
  , version :: Core.Maybe Types.Version
    -- ^ The version of the bot. For a new bot, the version is always @> LATEST@ .
  , voiceId :: Core.Maybe Core.Text
    -- ^ The Amazon Polly voice ID that Amazon Lex uses for voice interaction with the user. For more information, see 'PutBot' .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetBotResponse' value with any optional fields omitted.
mkGetBotResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetBotResponse
mkGetBotResponse responseStatus
  = GetBotResponse'{abortStatement = Core.Nothing,
                    checksum = Core.Nothing, childDirected = Core.Nothing,
                    clarificationPrompt = Core.Nothing, createdDate = Core.Nothing,
                    description = Core.Nothing, detectSentiment = Core.Nothing,
                    enableModelImprovements = Core.Nothing,
                    failureReason = Core.Nothing,
                    idleSessionTTLInSeconds = Core.Nothing, intents = Core.Nothing,
                    lastUpdatedDate = Core.Nothing, locale = Core.Nothing,
                    name = Core.Nothing, nluIntentConfidenceThreshold = Core.Nothing,
                    status = Core.Nothing, version = Core.Nothing,
                    voiceId = Core.Nothing, responseStatus}

-- | The message that Amazon Lex returns when the user elects to end the conversation without completing it. For more information, see 'PutBot' .
--
-- /Note:/ Consider using 'abortStatement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrrsAbortStatement :: Lens.Lens' GetBotResponse (Core.Maybe Types.Statement)
gbrrsAbortStatement = Lens.field @"abortStatement"
{-# INLINEABLE gbrrsAbortStatement #-}
{-# DEPRECATED abortStatement "Use generic-lens or generic-optics with 'abortStatement' instead"  #-}

-- | Checksum of the bot used to identify a specific revision of the bot's @> LATEST@ version.
--
-- /Note:/ Consider using 'checksum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrrsChecksum :: Lens.Lens' GetBotResponse (Core.Maybe Core.Text)
gbrrsChecksum = Lens.field @"checksum"
{-# INLINEABLE gbrrsChecksum #-}
{-# DEPRECATED checksum "Use generic-lens or generic-optics with 'checksum' instead"  #-}

-- | For each Amazon Lex bot created with the Amazon Lex Model Building Service, you must specify whether your use of Amazon Lex is related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to the Children's Online Privacy Protection Act (COPPA) by specifying @true@ or @false@ in the @childDirected@ field. By specifying @true@ in the @childDirected@ field, you confirm that your use of Amazon Lex __is__ related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA. By specifying @false@ in the @childDirected@ field, you confirm that your use of Amazon Lex __is not__ related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA. You may not specify a default value for the @childDirected@ field that does not accurately reflect whether your use of Amazon Lex is related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA.
--
-- If your use of Amazon Lex relates to a website, program, or other application that is directed in whole or in part, to children under age 13, you must obtain any required verifiable parental consent under COPPA. For information regarding the use of Amazon Lex in connection with websites, programs, or other applications that are directed or targeted, in whole or in part, to children under age 13, see the <https://aws.amazon.com/lex/faqs#data-security Amazon Lex FAQ.> 
--
-- /Note:/ Consider using 'childDirected' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrrsChildDirected :: Lens.Lens' GetBotResponse (Core.Maybe Core.Bool)
gbrrsChildDirected = Lens.field @"childDirected"
{-# INLINEABLE gbrrsChildDirected #-}
{-# DEPRECATED childDirected "Use generic-lens or generic-optics with 'childDirected' instead"  #-}

-- | The message Amazon Lex uses when it doesn't understand the user's request. For more information, see 'PutBot' . 
--
-- /Note:/ Consider using 'clarificationPrompt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrrsClarificationPrompt :: Lens.Lens' GetBotResponse (Core.Maybe Types.Prompt)
gbrrsClarificationPrompt = Lens.field @"clarificationPrompt"
{-# INLINEABLE gbrrsClarificationPrompt #-}
{-# DEPRECATED clarificationPrompt "Use generic-lens or generic-optics with 'clarificationPrompt' instead"  #-}

-- | The date that the bot was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrrsCreatedDate :: Lens.Lens' GetBotResponse (Core.Maybe Core.NominalDiffTime)
gbrrsCreatedDate = Lens.field @"createdDate"
{-# INLINEABLE gbrrsCreatedDate #-}
{-# DEPRECATED createdDate "Use generic-lens or generic-optics with 'createdDate' instead"  #-}

-- | A description of the bot.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrrsDescription :: Lens.Lens' GetBotResponse (Core.Maybe Types.Description)
gbrrsDescription = Lens.field @"description"
{-# INLINEABLE gbrrsDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Indicates whether user utterances should be sent to Amazon Comprehend for sentiment analysis.
--
-- /Note:/ Consider using 'detectSentiment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrrsDetectSentiment :: Lens.Lens' GetBotResponse (Core.Maybe Core.Bool)
gbrrsDetectSentiment = Lens.field @"detectSentiment"
{-# INLINEABLE gbrrsDetectSentiment #-}
{-# DEPRECATED detectSentiment "Use generic-lens or generic-optics with 'detectSentiment' instead"  #-}

-- | Indicates whether the bot uses accuracy improvements. @true@ indicates that the bot is using the improvements, otherwise, @false@ .
--
-- /Note:/ Consider using 'enableModelImprovements' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrrsEnableModelImprovements :: Lens.Lens' GetBotResponse (Core.Maybe Core.Bool)
gbrrsEnableModelImprovements = Lens.field @"enableModelImprovements"
{-# INLINEABLE gbrrsEnableModelImprovements #-}
{-# DEPRECATED enableModelImprovements "Use generic-lens or generic-optics with 'enableModelImprovements' instead"  #-}

-- | If @status@ is @FAILED@ , Amazon Lex explains why it failed to build the bot.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrrsFailureReason :: Lens.Lens' GetBotResponse (Core.Maybe Core.Text)
gbrrsFailureReason = Lens.field @"failureReason"
{-# INLINEABLE gbrrsFailureReason #-}
{-# DEPRECATED failureReason "Use generic-lens or generic-optics with 'failureReason' instead"  #-}

-- | The maximum time in seconds that Amazon Lex retains the data gathered in a conversation. For more information, see 'PutBot' .
--
-- /Note:/ Consider using 'idleSessionTTLInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrrsIdleSessionTTLInSeconds :: Lens.Lens' GetBotResponse (Core.Maybe Core.Natural)
gbrrsIdleSessionTTLInSeconds = Lens.field @"idleSessionTTLInSeconds"
{-# INLINEABLE gbrrsIdleSessionTTLInSeconds #-}
{-# DEPRECATED idleSessionTTLInSeconds "Use generic-lens or generic-optics with 'idleSessionTTLInSeconds' instead"  #-}

-- | An array of @intent@ objects. For more information, see 'PutBot' .
--
-- /Note:/ Consider using 'intents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrrsIntents :: Lens.Lens' GetBotResponse (Core.Maybe [Types.Intent])
gbrrsIntents = Lens.field @"intents"
{-# INLINEABLE gbrrsIntents #-}
{-# DEPRECATED intents "Use generic-lens or generic-optics with 'intents' instead"  #-}

-- | The date that the bot was updated. When you create a resource, the creation date and last updated date are the same. 
--
-- /Note:/ Consider using 'lastUpdatedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrrsLastUpdatedDate :: Lens.Lens' GetBotResponse (Core.Maybe Core.NominalDiffTime)
gbrrsLastUpdatedDate = Lens.field @"lastUpdatedDate"
{-# INLINEABLE gbrrsLastUpdatedDate #-}
{-# DEPRECATED lastUpdatedDate "Use generic-lens or generic-optics with 'lastUpdatedDate' instead"  #-}

-- | The target locale for the bot. 
--
-- /Note:/ Consider using 'locale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrrsLocale :: Lens.Lens' GetBotResponse (Core.Maybe Types.Locale)
gbrrsLocale = Lens.field @"locale"
{-# INLINEABLE gbrrsLocale #-}
{-# DEPRECATED locale "Use generic-lens or generic-optics with 'locale' instead"  #-}

-- | The name of the bot.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrrsName :: Lens.Lens' GetBotResponse (Core.Maybe Types.BotName)
gbrrsName = Lens.field @"name"
{-# INLINEABLE gbrrsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The score that determines where Amazon Lex inserts the @AMAZON.FallbackIntent@ , @AMAZON.KendraSearchIntent@ , or both when returning alternative intents in a <https://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostContent.html PostContent> or <https://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostText.html PostText> response. @AMAZON.FallbackIntent@ is inserted if the confidence score for all intents is below this value. @AMAZON.KendraSearchIntent@ is only inserted if it is configured for the bot.
--
-- /Note:/ Consider using 'nluIntentConfidenceThreshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrrsNluIntentConfidenceThreshold :: Lens.Lens' GetBotResponse (Core.Maybe Core.Double)
gbrrsNluIntentConfidenceThreshold = Lens.field @"nluIntentConfidenceThreshold"
{-# INLINEABLE gbrrsNluIntentConfidenceThreshold #-}
{-# DEPRECATED nluIntentConfidenceThreshold "Use generic-lens or generic-optics with 'nluIntentConfidenceThreshold' instead"  #-}

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
{-# INLINEABLE gbrrsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The version of the bot. For a new bot, the version is always @> LATEST@ .
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrrsVersion :: Lens.Lens' GetBotResponse (Core.Maybe Types.Version)
gbrrsVersion = Lens.field @"version"
{-# INLINEABLE gbrrsVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

-- | The Amazon Polly voice ID that Amazon Lex uses for voice interaction with the user. For more information, see 'PutBot' .
--
-- /Note:/ Consider using 'voiceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrrsVoiceId :: Lens.Lens' GetBotResponse (Core.Maybe Core.Text)
gbrrsVoiceId = Lens.field @"voiceId"
{-# INLINEABLE gbrrsVoiceId #-}
{-# DEPRECATED voiceId "Use generic-lens or generic-optics with 'voiceId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrrsResponseStatus :: Lens.Lens' GetBotResponse Core.Int
gbrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gbrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
