{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateBotVersion (..)
    , mkCreateBotVersion
    -- ** Request lenses
    , cbvName
    , cbvChecksum

    -- * Destructuring the response
    , CreateBotVersionResponse (..)
    , mkCreateBotVersionResponse
    -- ** Response lenses
    , cbvrrsAbortStatement
    , cbvrrsChecksum
    , cbvrrsChildDirected
    , cbvrrsClarificationPrompt
    , cbvrrsCreatedDate
    , cbvrrsDescription
    , cbvrrsDetectSentiment
    , cbvrrsEnableModelImprovements
    , cbvrrsFailureReason
    , cbvrrsIdleSessionTTLInSeconds
    , cbvrrsIntents
    , cbvrrsLastUpdatedDate
    , cbvrrsLocale
    , cbvrrsName
    , cbvrrsStatus
    , cbvrrsVersion
    , cbvrrsVoiceId
    , cbvrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexModels.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateBotVersion' smart constructor.
data CreateBotVersion = CreateBotVersion'
  { name :: Types.BotName
    -- ^ The name of the bot that you want to create a new version of. The name is case sensitive. 
  , checksum :: Core.Maybe Core.Text
    -- ^ Identifies a specific revision of the @> LATEST@ version of the bot. If you specify a checksum and the @> LATEST@ version of the bot has a different checksum, a @PreconditionFailedException@ exception is returned and Amazon Lex doesn't publish a new version. If you don't specify a checksum, Amazon Lex publishes the @> LATEST@ version.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateBotVersion' value with any optional fields omitted.
mkCreateBotVersion
    :: Types.BotName -- ^ 'name'
    -> CreateBotVersion
mkCreateBotVersion name
  = CreateBotVersion'{name, checksum = Core.Nothing}

-- | The name of the bot that you want to create a new version of. The name is case sensitive. 
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbvName :: Lens.Lens' CreateBotVersion Types.BotName
cbvName = Lens.field @"name"
{-# INLINEABLE cbvName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Identifies a specific revision of the @> LATEST@ version of the bot. If you specify a checksum and the @> LATEST@ version of the bot has a different checksum, a @PreconditionFailedException@ exception is returned and Amazon Lex doesn't publish a new version. If you don't specify a checksum, Amazon Lex publishes the @> LATEST@ version.
--
-- /Note:/ Consider using 'checksum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbvChecksum :: Lens.Lens' CreateBotVersion (Core.Maybe Core.Text)
cbvChecksum = Lens.field @"checksum"
{-# INLINEABLE cbvChecksum #-}
{-# DEPRECATED checksum "Use generic-lens or generic-optics with 'checksum' instead"  #-}

instance Core.ToQuery CreateBotVersion where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateBotVersion where
        toHeaders CreateBotVersion{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateBotVersion where
        toJSON CreateBotVersion{..}
          = Core.object
              (Core.catMaybes [("checksum" Core..=) Core.<$> checksum])

instance Core.AWSRequest CreateBotVersion where
        type Rs CreateBotVersion = CreateBotVersionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/bots/" Core.<> Core.toText name Core.<> "/versions",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateBotVersionResponse' Core.<$>
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
                     Core.<*> x Core..:? "status"
                     Core.<*> x Core..:? "version"
                     Core.<*> x Core..:? "voiceId"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateBotVersionResponse' smart constructor.
data CreateBotVersionResponse = CreateBotVersionResponse'
  { abortStatement :: Core.Maybe Types.Statement
    -- ^ The message that Amazon Lex uses to cancel a conversation. For more information, see 'PutBot' .
  , checksum :: Core.Maybe Core.Text
    -- ^ Checksum identifying the version of the bot that was created.
  , childDirected :: Core.Maybe Core.Bool
    -- ^ For each Amazon Lex bot created with the Amazon Lex Model Building Service, you must specify whether your use of Amazon Lex is related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to the Children's Online Privacy Protection Act (COPPA) by specifying @true@ or @false@ in the @childDirected@ field. By specifying @true@ in the @childDirected@ field, you confirm that your use of Amazon Lex __is__ related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA. By specifying @false@ in the @childDirected@ field, you confirm that your use of Amazon Lex __is not__ related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA. You may not specify a default value for the @childDirected@ field that does not accurately reflect whether your use of Amazon Lex is related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA.
--
-- If your use of Amazon Lex relates to a website, program, or other application that is directed in whole or in part, to children under age 13, you must obtain any required verifiable parental consent under COPPA. For information regarding the use of Amazon Lex in connection with websites, programs, or other applications that are directed or targeted, in whole or in part, to children under age 13, see the <https://aws.amazon.com/lex/faqs#data-security Amazon Lex FAQ.> 
  , clarificationPrompt :: Core.Maybe Types.Prompt
    -- ^ The message that Amazon Lex uses when it doesn't understand the user's request. For more information, see 'PutBot' . 
  , createdDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date when the bot version was created.
  , description :: Core.Maybe Types.Description
    -- ^ A description of the bot.
  , detectSentiment :: Core.Maybe Core.Bool
    -- ^ Indicates whether utterances entered by the user should be sent to Amazon Comprehend for sentiment analysis.
  , enableModelImprovements :: Core.Maybe Core.Bool
    -- ^ Indicates whether the bot uses accuracy improvements. @true@ indicates that the bot is using the improvements, otherwise, @false@ .
  , failureReason :: Core.Maybe Core.Text
    -- ^ If @status@ is @FAILED@ , Amazon Lex provides the reason that it failed to build the bot.
  , idleSessionTTLInSeconds :: Core.Maybe Core.Natural
    -- ^ The maximum time in seconds that Amazon Lex retains the data gathered in a conversation. For more information, see 'PutBot' .
  , intents :: Core.Maybe [Types.Intent]
    -- ^ An array of @Intent@ objects. For more information, see 'PutBot' .
  , lastUpdatedDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date when the @> LATEST@ version of this bot was updated. 
  , locale :: Core.Maybe Types.Locale
    -- ^ Specifies the target locale for the bot. 
  , name :: Core.Maybe Types.BotName
    -- ^ The name of the bot.
  , status :: Core.Maybe Types.LexStatus
    -- ^ When you send a request to create or update a bot, Amazon Lex sets the @status@ response element to @BUILDING@ . After Amazon Lex builds the bot, it sets @status@ to @READY@ . If Amazon Lex can't build the bot, it sets @status@ to @FAILED@ . Amazon Lex returns the reason for the failure in the @failureReason@ response element. 
  , version :: Core.Maybe Types.Version
    -- ^ The version of the bot. 
  , voiceId :: Core.Maybe Core.Text
    -- ^ The Amazon Polly voice ID that Amazon Lex uses for voice interactions with the user.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateBotVersionResponse' value with any optional fields omitted.
mkCreateBotVersionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateBotVersionResponse
mkCreateBotVersionResponse responseStatus
  = CreateBotVersionResponse'{abortStatement = Core.Nothing,
                              checksum = Core.Nothing, childDirected = Core.Nothing,
                              clarificationPrompt = Core.Nothing, createdDate = Core.Nothing,
                              description = Core.Nothing, detectSentiment = Core.Nothing,
                              enableModelImprovements = Core.Nothing,
                              failureReason = Core.Nothing,
                              idleSessionTTLInSeconds = Core.Nothing, intents = Core.Nothing,
                              lastUpdatedDate = Core.Nothing, locale = Core.Nothing,
                              name = Core.Nothing, status = Core.Nothing, version = Core.Nothing,
                              voiceId = Core.Nothing, responseStatus}

-- | The message that Amazon Lex uses to cancel a conversation. For more information, see 'PutBot' .
--
-- /Note:/ Consider using 'abortStatement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbvrrsAbortStatement :: Lens.Lens' CreateBotVersionResponse (Core.Maybe Types.Statement)
cbvrrsAbortStatement = Lens.field @"abortStatement"
{-# INLINEABLE cbvrrsAbortStatement #-}
{-# DEPRECATED abortStatement "Use generic-lens or generic-optics with 'abortStatement' instead"  #-}

-- | Checksum identifying the version of the bot that was created.
--
-- /Note:/ Consider using 'checksum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbvrrsChecksum :: Lens.Lens' CreateBotVersionResponse (Core.Maybe Core.Text)
cbvrrsChecksum = Lens.field @"checksum"
{-# INLINEABLE cbvrrsChecksum #-}
{-# DEPRECATED checksum "Use generic-lens or generic-optics with 'checksum' instead"  #-}

-- | For each Amazon Lex bot created with the Amazon Lex Model Building Service, you must specify whether your use of Amazon Lex is related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to the Children's Online Privacy Protection Act (COPPA) by specifying @true@ or @false@ in the @childDirected@ field. By specifying @true@ in the @childDirected@ field, you confirm that your use of Amazon Lex __is__ related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA. By specifying @false@ in the @childDirected@ field, you confirm that your use of Amazon Lex __is not__ related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA. You may not specify a default value for the @childDirected@ field that does not accurately reflect whether your use of Amazon Lex is related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA.
--
-- If your use of Amazon Lex relates to a website, program, or other application that is directed in whole or in part, to children under age 13, you must obtain any required verifiable parental consent under COPPA. For information regarding the use of Amazon Lex in connection with websites, programs, or other applications that are directed or targeted, in whole or in part, to children under age 13, see the <https://aws.amazon.com/lex/faqs#data-security Amazon Lex FAQ.> 
--
-- /Note:/ Consider using 'childDirected' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbvrrsChildDirected :: Lens.Lens' CreateBotVersionResponse (Core.Maybe Core.Bool)
cbvrrsChildDirected = Lens.field @"childDirected"
{-# INLINEABLE cbvrrsChildDirected #-}
{-# DEPRECATED childDirected "Use generic-lens or generic-optics with 'childDirected' instead"  #-}

-- | The message that Amazon Lex uses when it doesn't understand the user's request. For more information, see 'PutBot' . 
--
-- /Note:/ Consider using 'clarificationPrompt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbvrrsClarificationPrompt :: Lens.Lens' CreateBotVersionResponse (Core.Maybe Types.Prompt)
cbvrrsClarificationPrompt = Lens.field @"clarificationPrompt"
{-# INLINEABLE cbvrrsClarificationPrompt #-}
{-# DEPRECATED clarificationPrompt "Use generic-lens or generic-optics with 'clarificationPrompt' instead"  #-}

-- | The date when the bot version was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbvrrsCreatedDate :: Lens.Lens' CreateBotVersionResponse (Core.Maybe Core.NominalDiffTime)
cbvrrsCreatedDate = Lens.field @"createdDate"
{-# INLINEABLE cbvrrsCreatedDate #-}
{-# DEPRECATED createdDate "Use generic-lens or generic-optics with 'createdDate' instead"  #-}

-- | A description of the bot.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbvrrsDescription :: Lens.Lens' CreateBotVersionResponse (Core.Maybe Types.Description)
cbvrrsDescription = Lens.field @"description"
{-# INLINEABLE cbvrrsDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Indicates whether utterances entered by the user should be sent to Amazon Comprehend for sentiment analysis.
--
-- /Note:/ Consider using 'detectSentiment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbvrrsDetectSentiment :: Lens.Lens' CreateBotVersionResponse (Core.Maybe Core.Bool)
cbvrrsDetectSentiment = Lens.field @"detectSentiment"
{-# INLINEABLE cbvrrsDetectSentiment #-}
{-# DEPRECATED detectSentiment "Use generic-lens or generic-optics with 'detectSentiment' instead"  #-}

-- | Indicates whether the bot uses accuracy improvements. @true@ indicates that the bot is using the improvements, otherwise, @false@ .
--
-- /Note:/ Consider using 'enableModelImprovements' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbvrrsEnableModelImprovements :: Lens.Lens' CreateBotVersionResponse (Core.Maybe Core.Bool)
cbvrrsEnableModelImprovements = Lens.field @"enableModelImprovements"
{-# INLINEABLE cbvrrsEnableModelImprovements #-}
{-# DEPRECATED enableModelImprovements "Use generic-lens or generic-optics with 'enableModelImprovements' instead"  #-}

-- | If @status@ is @FAILED@ , Amazon Lex provides the reason that it failed to build the bot.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbvrrsFailureReason :: Lens.Lens' CreateBotVersionResponse (Core.Maybe Core.Text)
cbvrrsFailureReason = Lens.field @"failureReason"
{-# INLINEABLE cbvrrsFailureReason #-}
{-# DEPRECATED failureReason "Use generic-lens or generic-optics with 'failureReason' instead"  #-}

-- | The maximum time in seconds that Amazon Lex retains the data gathered in a conversation. For more information, see 'PutBot' .
--
-- /Note:/ Consider using 'idleSessionTTLInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbvrrsIdleSessionTTLInSeconds :: Lens.Lens' CreateBotVersionResponse (Core.Maybe Core.Natural)
cbvrrsIdleSessionTTLInSeconds = Lens.field @"idleSessionTTLInSeconds"
{-# INLINEABLE cbvrrsIdleSessionTTLInSeconds #-}
{-# DEPRECATED idleSessionTTLInSeconds "Use generic-lens or generic-optics with 'idleSessionTTLInSeconds' instead"  #-}

-- | An array of @Intent@ objects. For more information, see 'PutBot' .
--
-- /Note:/ Consider using 'intents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbvrrsIntents :: Lens.Lens' CreateBotVersionResponse (Core.Maybe [Types.Intent])
cbvrrsIntents = Lens.field @"intents"
{-# INLINEABLE cbvrrsIntents #-}
{-# DEPRECATED intents "Use generic-lens or generic-optics with 'intents' instead"  #-}

-- | The date when the @> LATEST@ version of this bot was updated. 
--
-- /Note:/ Consider using 'lastUpdatedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbvrrsLastUpdatedDate :: Lens.Lens' CreateBotVersionResponse (Core.Maybe Core.NominalDiffTime)
cbvrrsLastUpdatedDate = Lens.field @"lastUpdatedDate"
{-# INLINEABLE cbvrrsLastUpdatedDate #-}
{-# DEPRECATED lastUpdatedDate "Use generic-lens or generic-optics with 'lastUpdatedDate' instead"  #-}

-- | Specifies the target locale for the bot. 
--
-- /Note:/ Consider using 'locale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbvrrsLocale :: Lens.Lens' CreateBotVersionResponse (Core.Maybe Types.Locale)
cbvrrsLocale = Lens.field @"locale"
{-# INLINEABLE cbvrrsLocale #-}
{-# DEPRECATED locale "Use generic-lens or generic-optics with 'locale' instead"  #-}

-- | The name of the bot.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbvrrsName :: Lens.Lens' CreateBotVersionResponse (Core.Maybe Types.BotName)
cbvrrsName = Lens.field @"name"
{-# INLINEABLE cbvrrsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | When you send a request to create or update a bot, Amazon Lex sets the @status@ response element to @BUILDING@ . After Amazon Lex builds the bot, it sets @status@ to @READY@ . If Amazon Lex can't build the bot, it sets @status@ to @FAILED@ . Amazon Lex returns the reason for the failure in the @failureReason@ response element. 
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbvrrsStatus :: Lens.Lens' CreateBotVersionResponse (Core.Maybe Types.LexStatus)
cbvrrsStatus = Lens.field @"status"
{-# INLINEABLE cbvrrsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The version of the bot. 
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbvrrsVersion :: Lens.Lens' CreateBotVersionResponse (Core.Maybe Types.Version)
cbvrrsVersion = Lens.field @"version"
{-# INLINEABLE cbvrrsVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

-- | The Amazon Polly voice ID that Amazon Lex uses for voice interactions with the user.
--
-- /Note:/ Consider using 'voiceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbvrrsVoiceId :: Lens.Lens' CreateBotVersionResponse (Core.Maybe Core.Text)
cbvrrsVoiceId = Lens.field @"voiceId"
{-# INLINEABLE cbvrrsVoiceId #-}
{-# DEPRECATED voiceId "Use generic-lens or generic-optics with 'voiceId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbvrrsResponseStatus :: Lens.Lens' CreateBotVersionResponse Core.Int
cbvrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cbvrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
