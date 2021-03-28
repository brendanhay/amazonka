{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.PutBot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon Lex conversational bot or replaces an existing bot. When you create or update a bot you are only required to specify a name, a locale, and whether the bot is directed toward children under age 13. You can use this to add intents later, or to remove intents from an existing bot. When you create a bot with the minimum information, the bot is created or updated but Amazon Lex returns the response @FAILED@ . You can build the bot after you add one or more intents. For more information about Amazon Lex bots, see 'how-it-works' . 
--
-- If you specify the name of an existing bot, the fields in the request replace the existing values in the @> LATEST@ version of the bot. Amazon Lex removes any fields that you don't provide values for in the request, except for the @idleTTLInSeconds@ and @privacySettings@ fields, which are set to their default values. If you don't specify values for required fields, Amazon Lex throws an exception.
-- This operation requires permissions for the @lex:PutBot@ action. For more information, see 'security-iam' .
module Network.AWS.LexModels.PutBot
    (
    -- * Creating a request
      PutBot (..)
    , mkPutBot
    -- ** Request lenses
    , pbName
    , pbLocale
    , pbChildDirected
    , pbAbortStatement
    , pbChecksum
    , pbClarificationPrompt
    , pbCreateVersion
    , pbDescription
    , pbDetectSentiment
    , pbEnableModelImprovements
    , pbIdleSessionTTLInSeconds
    , pbIntents
    , pbNluIntentConfidenceThreshold
    , pbProcessBehavior
    , pbTags
    , pbVoiceId

    -- * Destructuring the response
    , PutBotResponse (..)
    , mkPutBotResponse
    -- ** Response lenses
    , pbrrsAbortStatement
    , pbrrsChecksum
    , pbrrsChildDirected
    , pbrrsClarificationPrompt
    , pbrrsCreateVersion
    , pbrrsCreatedDate
    , pbrrsDescription
    , pbrrsDetectSentiment
    , pbrrsEnableModelImprovements
    , pbrrsFailureReason
    , pbrrsIdleSessionTTLInSeconds
    , pbrrsIntents
    , pbrrsLastUpdatedDate
    , pbrrsLocale
    , pbrrsName
    , pbrrsNluIntentConfidenceThreshold
    , pbrrsStatus
    , pbrrsTags
    , pbrrsVersion
    , pbrrsVoiceId
    , pbrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexModels.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutBot' smart constructor.
data PutBot = PutBot'
  { name :: Types.BotName
    -- ^ The name of the bot. The name is /not/ case sensitive. 
  , locale :: Types.Locale
    -- ^ Specifies the target locale for the bot. Any intent used in the bot must be compatible with the locale of the bot. 
--
-- The default is @en-US@ .
  , childDirected :: Core.Bool
    -- ^ For each Amazon Lex bot created with the Amazon Lex Model Building Service, you must specify whether your use of Amazon Lex is related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to the Children's Online Privacy Protection Act (COPPA) by specifying @true@ or @false@ in the @childDirected@ field. By specifying @true@ in the @childDirected@ field, you confirm that your use of Amazon Lex __is__ related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA. By specifying @false@ in the @childDirected@ field, you confirm that your use of Amazon Lex __is not__ related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA. You may not specify a default value for the @childDirected@ field that does not accurately reflect whether your use of Amazon Lex is related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA.
--
-- If your use of Amazon Lex relates to a website, program, or other application that is directed in whole or in part, to children under age 13, you must obtain any required verifiable parental consent under COPPA. For information regarding the use of Amazon Lex in connection with websites, programs, or other applications that are directed or targeted, in whole or in part, to children under age 13, see the <https://aws.amazon.com/lex/faqs#data-security Amazon Lex FAQ.> 
  , abortStatement :: Core.Maybe Types.Statement
    -- ^ When Amazon Lex can't understand the user's input in context, it tries to elicit the information a few times. After that, Amazon Lex sends the message defined in @abortStatement@ to the user, and then cancels the conversation. To set the number of retries, use the @valueElicitationPrompt@ field for the slot type. 
--
-- For example, in a pizza ordering bot, Amazon Lex might ask a user "What type of crust would you like?" If the user's response is not one of the expected responses (for example, "thin crust, "deep dish," etc.), Amazon Lex tries to elicit a correct response a few more times. 
-- For example, in a pizza ordering application, @OrderPizza@ might be one of the intents. This intent might require the @CrustType@ slot. You specify the @valueElicitationPrompt@ field when you create the @CrustType@ slot.
-- If you have defined a fallback intent the cancel statement will not be sent to the user, the fallback intent is used instead. For more information, see <https://docs.aws.amazon.com/lex/latest/dg/built-in-intent-fallback.html AMAZON.FallbackIntent> .
  , checksum :: Core.Maybe Core.Text
    -- ^ Identifies a specific revision of the @> LATEST@ version.
--
-- When you create a new bot, leave the @checksum@ field blank. If you specify a checksum you get a @BadRequestException@ exception.
-- When you want to update a bot, set the @checksum@ field to the checksum of the most recent revision of the @> LATEST@ version. If you don't specify the @checksum@ field, or if the checksum does not match the @> LATEST@ version, you get a @PreconditionFailedException@ exception.
  , clarificationPrompt :: Core.Maybe Types.Prompt
    -- ^ When Amazon Lex doesn't understand the user's intent, it uses this message to get clarification. To specify how many times Amazon Lex should repeat the clarification prompt, use the @maxAttempts@ field. If Amazon Lex still doesn't understand, it sends the message in the @abortStatement@ field. 
--
-- When you create a clarification prompt, make sure that it suggests the correct response from the user. for example, for a bot that orders pizza and drinks, you might create this clarification prompt: "What would you like to do? You can say 'Order a pizza' or 'Order a drink.'"
-- If you have defined a fallback intent, it will be invoked if the clarification prompt is repeated the number of times defined in the @maxAttempts@ field. For more information, see <https://docs.aws.amazon.com/lex/latest/dg/built-in-intent-fallback.html AMAZON.FallbackIntent> .
-- If you don't define a clarification prompt, at runtime Amazon Lex will return a 400 Bad Request exception in three cases: 
--
--     * Follow-up prompt - When the user responds to a follow-up prompt but does not provide an intent. For example, in response to a follow-up prompt that says "Would you like anything else today?" the user says "Yes." Amazon Lex will return a 400 Bad Request exception because it does not have a clarification prompt to send to the user to get an intent.
--
--
--     * Lambda function - When using a Lambda function, you return an @ElicitIntent@ dialog type. Since Amazon Lex does not have a clarification prompt to get an intent from the user, it returns a 400 Bad Request exception.
--
--
--     * PutSession operation - When using the @PutSession@ operation, you send an @ElicitIntent@ dialog type. Since Amazon Lex does not have a clarification prompt to get an intent from the user, it returns a 400 Bad Request exception.
--
--
  , createVersion :: Core.Maybe Core.Bool
    -- ^ When set to @true@ a new numbered version of the bot is created. This is the same as calling the @CreateBotVersion@ operation. If you don't specify @createVersion@ , the default is @false@ .
  , description :: Core.Maybe Types.Description
    -- ^ A description of the bot.
  , detectSentiment :: Core.Maybe Core.Bool
    -- ^ When set to @true@ user utterances are sent to Amazon Comprehend for sentiment analysis. If you don't specify @detectSentiment@ , the default is @false@ .
  , enableModelImprovements :: Core.Maybe Core.Bool
    -- ^ Set to @true@ to enable access to natural language understanding improvements. 
--
-- When you set the @enableModelImprovements@ parameter to @true@ you can use the @nluIntentConfidenceThreshold@ parameter to configure confidence scores. For more information, see <https://docs.aws.amazon.com/lex/latest/dg/confidence-scores.html Confidence Scores> .
-- You can only set the @enableModelImprovements@ parameter in certain Regions. If you set the parameter to @true@ , your bot has access to accuracy improvements.
-- The Regions where you can set the @enableModelImprovements@ parameter to @true@ are:
--
--     * US East (N. Virginia) (us-east-1)
--
--
--     * US West (Oregon) (us-west-2)
--
--
--     * Asia Pacific (Sydney) (ap-southeast-2)
--
--
--     * EU (Ireland) (eu-west-1)
--
--
-- In other Regions, the @enableModelImprovements@ parameter is set to @true@ by default. In these Regions setting the parameter to @false@ throws a @ValidationException@ exception.
  , idleSessionTTLInSeconds :: Core.Maybe Core.Natural
    -- ^ The maximum time in seconds that Amazon Lex retains the data gathered in a conversation.
--
-- A user interaction session remains active for the amount of time specified. If no conversation occurs during this time, the session expires and Amazon Lex deletes any data provided before the timeout.
-- For example, suppose that a user chooses the OrderPizza intent, but gets sidetracked halfway through placing an order. If the user doesn't complete the order within the specified time, Amazon Lex discards the slot information that it gathered, and the user must start over.
-- If you don't include the @idleSessionTTLInSeconds@ element in a @PutBot@ operation request, Amazon Lex uses the default value. This is also true if the request replaces an existing bot.
-- The default is 300 seconds (5 minutes).
  , intents :: Core.Maybe [Types.Intent]
    -- ^ An array of @Intent@ objects. Each intent represents a command that a user can express. For example, a pizza ordering bot might support an OrderPizza intent. For more information, see 'how-it-works' .
  , nluIntentConfidenceThreshold :: Core.Maybe Core.Double
    -- ^ Determines the threshold where Amazon Lex will insert the @AMAZON.FallbackIntent@ , @AMAZON.KendraSearchIntent@ , or both when returning alternative intents in a <https://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostContent.html PostContent> or <https://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostText.html PostText> response. @AMAZON.FallbackIntent@ and @AMAZON.KendraSearchIntent@ are only inserted if they are configured for the bot.
--
-- You must set the @enableModelImprovements@ parameter to @true@ to use confidence scores in the following regions.
--
--     * US East (N. Virginia) (us-east-1)
--
--
--     * US West (Oregon) (us-west-2)
--
--
--     * Asia Pacific (Sydney) (ap-southeast-2)
--
--
--     * EU (Ireland) (eu-west-1)
--
--
-- In other Regions, the @enableModelImprovements@ parameter is set to @true@ by default.
-- For example, suppose a bot is configured with the confidence threshold of 0.80 and the @AMAZON.FallbackIntent@ . Amazon Lex returns three alternative intents with the following confidence scores: IntentA (0.70), IntentB (0.60), IntentC (0.50). The response from the @PostText@ operation would be:
--
--     * AMAZON.FallbackIntent
--
--
--     * IntentA
--
--
--     * IntentB
--
--
--     * IntentC
--
--
  , processBehavior :: Core.Maybe Types.ProcessBehavior
    -- ^ If you set the @processBehavior@ element to @BUILD@ , Amazon Lex builds the bot so that it can be run. If you set the element to @SAVE@ Amazon Lex saves the bot, but doesn't build it. 
--
-- If you don't specify this value, the default value is @BUILD@ .
  , tags :: Core.Maybe [Types.Tag]
    -- ^ A list of tags to add to the bot. You can only add tags when you create a bot, you can't use the @PutBot@ operation to update the tags on a bot. To update tags, use the @TagResource@ operation.
  , voiceId :: Core.Maybe Core.Text
    -- ^ The Amazon Polly voice ID that you want Amazon Lex to use for voice interactions with the user. The locale configured for the voice must match the locale of the bot. For more information, see <https://docs.aws.amazon.com/polly/latest/dg/voicelist.html Voices in Amazon Polly> in the /Amazon Polly Developer Guide/ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutBot' value with any optional fields omitted.
mkPutBot
    :: Types.BotName -- ^ 'name'
    -> Types.Locale -- ^ 'locale'
    -> Core.Bool -- ^ 'childDirected'
    -> PutBot
mkPutBot name locale childDirected
  = PutBot'{name, locale, childDirected,
            abortStatement = Core.Nothing, checksum = Core.Nothing,
            clarificationPrompt = Core.Nothing, createVersion = Core.Nothing,
            description = Core.Nothing, detectSentiment = Core.Nothing,
            enableModelImprovements = Core.Nothing,
            idleSessionTTLInSeconds = Core.Nothing, intents = Core.Nothing,
            nluIntentConfidenceThreshold = Core.Nothing,
            processBehavior = Core.Nothing, tags = Core.Nothing,
            voiceId = Core.Nothing}

-- | The name of the bot. The name is /not/ case sensitive. 
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbName :: Lens.Lens' PutBot Types.BotName
pbName = Lens.field @"name"
{-# INLINEABLE pbName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Specifies the target locale for the bot. Any intent used in the bot must be compatible with the locale of the bot. 
--
-- The default is @en-US@ .
--
-- /Note:/ Consider using 'locale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbLocale :: Lens.Lens' PutBot Types.Locale
pbLocale = Lens.field @"locale"
{-# INLINEABLE pbLocale #-}
{-# DEPRECATED locale "Use generic-lens or generic-optics with 'locale' instead"  #-}

-- | For each Amazon Lex bot created with the Amazon Lex Model Building Service, you must specify whether your use of Amazon Lex is related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to the Children's Online Privacy Protection Act (COPPA) by specifying @true@ or @false@ in the @childDirected@ field. By specifying @true@ in the @childDirected@ field, you confirm that your use of Amazon Lex __is__ related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA. By specifying @false@ in the @childDirected@ field, you confirm that your use of Amazon Lex __is not__ related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA. You may not specify a default value for the @childDirected@ field that does not accurately reflect whether your use of Amazon Lex is related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA.
--
-- If your use of Amazon Lex relates to a website, program, or other application that is directed in whole or in part, to children under age 13, you must obtain any required verifiable parental consent under COPPA. For information regarding the use of Amazon Lex in connection with websites, programs, or other applications that are directed or targeted, in whole or in part, to children under age 13, see the <https://aws.amazon.com/lex/faqs#data-security Amazon Lex FAQ.> 
--
-- /Note:/ Consider using 'childDirected' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbChildDirected :: Lens.Lens' PutBot Core.Bool
pbChildDirected = Lens.field @"childDirected"
{-# INLINEABLE pbChildDirected #-}
{-# DEPRECATED childDirected "Use generic-lens or generic-optics with 'childDirected' instead"  #-}

-- | When Amazon Lex can't understand the user's input in context, it tries to elicit the information a few times. After that, Amazon Lex sends the message defined in @abortStatement@ to the user, and then cancels the conversation. To set the number of retries, use the @valueElicitationPrompt@ field for the slot type. 
--
-- For example, in a pizza ordering bot, Amazon Lex might ask a user "What type of crust would you like?" If the user's response is not one of the expected responses (for example, "thin crust, "deep dish," etc.), Amazon Lex tries to elicit a correct response a few more times. 
-- For example, in a pizza ordering application, @OrderPizza@ might be one of the intents. This intent might require the @CrustType@ slot. You specify the @valueElicitationPrompt@ field when you create the @CrustType@ slot.
-- If you have defined a fallback intent the cancel statement will not be sent to the user, the fallback intent is used instead. For more information, see <https://docs.aws.amazon.com/lex/latest/dg/built-in-intent-fallback.html AMAZON.FallbackIntent> .
--
-- /Note:/ Consider using 'abortStatement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbAbortStatement :: Lens.Lens' PutBot (Core.Maybe Types.Statement)
pbAbortStatement = Lens.field @"abortStatement"
{-# INLINEABLE pbAbortStatement #-}
{-# DEPRECATED abortStatement "Use generic-lens or generic-optics with 'abortStatement' instead"  #-}

-- | Identifies a specific revision of the @> LATEST@ version.
--
-- When you create a new bot, leave the @checksum@ field blank. If you specify a checksum you get a @BadRequestException@ exception.
-- When you want to update a bot, set the @checksum@ field to the checksum of the most recent revision of the @> LATEST@ version. If you don't specify the @checksum@ field, or if the checksum does not match the @> LATEST@ version, you get a @PreconditionFailedException@ exception.
--
-- /Note:/ Consider using 'checksum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbChecksum :: Lens.Lens' PutBot (Core.Maybe Core.Text)
pbChecksum = Lens.field @"checksum"
{-# INLINEABLE pbChecksum #-}
{-# DEPRECATED checksum "Use generic-lens or generic-optics with 'checksum' instead"  #-}

-- | When Amazon Lex doesn't understand the user's intent, it uses this message to get clarification. To specify how many times Amazon Lex should repeat the clarification prompt, use the @maxAttempts@ field. If Amazon Lex still doesn't understand, it sends the message in the @abortStatement@ field. 
--
-- When you create a clarification prompt, make sure that it suggests the correct response from the user. for example, for a bot that orders pizza and drinks, you might create this clarification prompt: "What would you like to do? You can say 'Order a pizza' or 'Order a drink.'"
-- If you have defined a fallback intent, it will be invoked if the clarification prompt is repeated the number of times defined in the @maxAttempts@ field. For more information, see <https://docs.aws.amazon.com/lex/latest/dg/built-in-intent-fallback.html AMAZON.FallbackIntent> .
-- If you don't define a clarification prompt, at runtime Amazon Lex will return a 400 Bad Request exception in three cases: 
--
--     * Follow-up prompt - When the user responds to a follow-up prompt but does not provide an intent. For example, in response to a follow-up prompt that says "Would you like anything else today?" the user says "Yes." Amazon Lex will return a 400 Bad Request exception because it does not have a clarification prompt to send to the user to get an intent.
--
--
--     * Lambda function - When using a Lambda function, you return an @ElicitIntent@ dialog type. Since Amazon Lex does not have a clarification prompt to get an intent from the user, it returns a 400 Bad Request exception.
--
--
--     * PutSession operation - When using the @PutSession@ operation, you send an @ElicitIntent@ dialog type. Since Amazon Lex does not have a clarification prompt to get an intent from the user, it returns a 400 Bad Request exception.
--
--
--
-- /Note:/ Consider using 'clarificationPrompt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbClarificationPrompt :: Lens.Lens' PutBot (Core.Maybe Types.Prompt)
pbClarificationPrompt = Lens.field @"clarificationPrompt"
{-# INLINEABLE pbClarificationPrompt #-}
{-# DEPRECATED clarificationPrompt "Use generic-lens or generic-optics with 'clarificationPrompt' instead"  #-}

-- | When set to @true@ a new numbered version of the bot is created. This is the same as calling the @CreateBotVersion@ operation. If you don't specify @createVersion@ , the default is @false@ .
--
-- /Note:/ Consider using 'createVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbCreateVersion :: Lens.Lens' PutBot (Core.Maybe Core.Bool)
pbCreateVersion = Lens.field @"createVersion"
{-# INLINEABLE pbCreateVersion #-}
{-# DEPRECATED createVersion "Use generic-lens or generic-optics with 'createVersion' instead"  #-}

-- | A description of the bot.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbDescription :: Lens.Lens' PutBot (Core.Maybe Types.Description)
pbDescription = Lens.field @"description"
{-# INLINEABLE pbDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | When set to @true@ user utterances are sent to Amazon Comprehend for sentiment analysis. If you don't specify @detectSentiment@ , the default is @false@ .
--
-- /Note:/ Consider using 'detectSentiment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbDetectSentiment :: Lens.Lens' PutBot (Core.Maybe Core.Bool)
pbDetectSentiment = Lens.field @"detectSentiment"
{-# INLINEABLE pbDetectSentiment #-}
{-# DEPRECATED detectSentiment "Use generic-lens or generic-optics with 'detectSentiment' instead"  #-}

-- | Set to @true@ to enable access to natural language understanding improvements. 
--
-- When you set the @enableModelImprovements@ parameter to @true@ you can use the @nluIntentConfidenceThreshold@ parameter to configure confidence scores. For more information, see <https://docs.aws.amazon.com/lex/latest/dg/confidence-scores.html Confidence Scores> .
-- You can only set the @enableModelImprovements@ parameter in certain Regions. If you set the parameter to @true@ , your bot has access to accuracy improvements.
-- The Regions where you can set the @enableModelImprovements@ parameter to @true@ are:
--
--     * US East (N. Virginia) (us-east-1)
--
--
--     * US West (Oregon) (us-west-2)
--
--
--     * Asia Pacific (Sydney) (ap-southeast-2)
--
--
--     * EU (Ireland) (eu-west-1)
--
--
-- In other Regions, the @enableModelImprovements@ parameter is set to @true@ by default. In these Regions setting the parameter to @false@ throws a @ValidationException@ exception.
--
-- /Note:/ Consider using 'enableModelImprovements' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbEnableModelImprovements :: Lens.Lens' PutBot (Core.Maybe Core.Bool)
pbEnableModelImprovements = Lens.field @"enableModelImprovements"
{-# INLINEABLE pbEnableModelImprovements #-}
{-# DEPRECATED enableModelImprovements "Use generic-lens or generic-optics with 'enableModelImprovements' instead"  #-}

-- | The maximum time in seconds that Amazon Lex retains the data gathered in a conversation.
--
-- A user interaction session remains active for the amount of time specified. If no conversation occurs during this time, the session expires and Amazon Lex deletes any data provided before the timeout.
-- For example, suppose that a user chooses the OrderPizza intent, but gets sidetracked halfway through placing an order. If the user doesn't complete the order within the specified time, Amazon Lex discards the slot information that it gathered, and the user must start over.
-- If you don't include the @idleSessionTTLInSeconds@ element in a @PutBot@ operation request, Amazon Lex uses the default value. This is also true if the request replaces an existing bot.
-- The default is 300 seconds (5 minutes).
--
-- /Note:/ Consider using 'idleSessionTTLInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbIdleSessionTTLInSeconds :: Lens.Lens' PutBot (Core.Maybe Core.Natural)
pbIdleSessionTTLInSeconds = Lens.field @"idleSessionTTLInSeconds"
{-# INLINEABLE pbIdleSessionTTLInSeconds #-}
{-# DEPRECATED idleSessionTTLInSeconds "Use generic-lens or generic-optics with 'idleSessionTTLInSeconds' instead"  #-}

-- | An array of @Intent@ objects. Each intent represents a command that a user can express. For example, a pizza ordering bot might support an OrderPizza intent. For more information, see 'how-it-works' .
--
-- /Note:/ Consider using 'intents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbIntents :: Lens.Lens' PutBot (Core.Maybe [Types.Intent])
pbIntents = Lens.field @"intents"
{-# INLINEABLE pbIntents #-}
{-# DEPRECATED intents "Use generic-lens or generic-optics with 'intents' instead"  #-}

-- | Determines the threshold where Amazon Lex will insert the @AMAZON.FallbackIntent@ , @AMAZON.KendraSearchIntent@ , or both when returning alternative intents in a <https://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostContent.html PostContent> or <https://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostText.html PostText> response. @AMAZON.FallbackIntent@ and @AMAZON.KendraSearchIntent@ are only inserted if they are configured for the bot.
--
-- You must set the @enableModelImprovements@ parameter to @true@ to use confidence scores in the following regions.
--
--     * US East (N. Virginia) (us-east-1)
--
--
--     * US West (Oregon) (us-west-2)
--
--
--     * Asia Pacific (Sydney) (ap-southeast-2)
--
--
--     * EU (Ireland) (eu-west-1)
--
--
-- In other Regions, the @enableModelImprovements@ parameter is set to @true@ by default.
-- For example, suppose a bot is configured with the confidence threshold of 0.80 and the @AMAZON.FallbackIntent@ . Amazon Lex returns three alternative intents with the following confidence scores: IntentA (0.70), IntentB (0.60), IntentC (0.50). The response from the @PostText@ operation would be:
--
--     * AMAZON.FallbackIntent
--
--
--     * IntentA
--
--
--     * IntentB
--
--
--     * IntentC
--
--
--
-- /Note:/ Consider using 'nluIntentConfidenceThreshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbNluIntentConfidenceThreshold :: Lens.Lens' PutBot (Core.Maybe Core.Double)
pbNluIntentConfidenceThreshold = Lens.field @"nluIntentConfidenceThreshold"
{-# INLINEABLE pbNluIntentConfidenceThreshold #-}
{-# DEPRECATED nluIntentConfidenceThreshold "Use generic-lens or generic-optics with 'nluIntentConfidenceThreshold' instead"  #-}

-- | If you set the @processBehavior@ element to @BUILD@ , Amazon Lex builds the bot so that it can be run. If you set the element to @SAVE@ Amazon Lex saves the bot, but doesn't build it. 
--
-- If you don't specify this value, the default value is @BUILD@ .
--
-- /Note:/ Consider using 'processBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbProcessBehavior :: Lens.Lens' PutBot (Core.Maybe Types.ProcessBehavior)
pbProcessBehavior = Lens.field @"processBehavior"
{-# INLINEABLE pbProcessBehavior #-}
{-# DEPRECATED processBehavior "Use generic-lens or generic-optics with 'processBehavior' instead"  #-}

-- | A list of tags to add to the bot. You can only add tags when you create a bot, you can't use the @PutBot@ operation to update the tags on a bot. To update tags, use the @TagResource@ operation.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbTags :: Lens.Lens' PutBot (Core.Maybe [Types.Tag])
pbTags = Lens.field @"tags"
{-# INLINEABLE pbTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The Amazon Polly voice ID that you want Amazon Lex to use for voice interactions with the user. The locale configured for the voice must match the locale of the bot. For more information, see <https://docs.aws.amazon.com/polly/latest/dg/voicelist.html Voices in Amazon Polly> in the /Amazon Polly Developer Guide/ .
--
-- /Note:/ Consider using 'voiceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbVoiceId :: Lens.Lens' PutBot (Core.Maybe Core.Text)
pbVoiceId = Lens.field @"voiceId"
{-# INLINEABLE pbVoiceId #-}
{-# DEPRECATED voiceId "Use generic-lens or generic-optics with 'voiceId' instead"  #-}

instance Core.ToQuery PutBot where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders PutBot where
        toHeaders PutBot{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON PutBot where
        toJSON PutBot{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("locale" Core..= locale),
                  Core.Just ("childDirected" Core..= childDirected),
                  ("abortStatement" Core..=) Core.<$> abortStatement,
                  ("checksum" Core..=) Core.<$> checksum,
                  ("clarificationPrompt" Core..=) Core.<$> clarificationPrompt,
                  ("createVersion" Core..=) Core.<$> createVersion,
                  ("description" Core..=) Core.<$> description,
                  ("detectSentiment" Core..=) Core.<$> detectSentiment,
                  ("enableModelImprovements" Core..=) Core.<$>
                    enableModelImprovements,
                  ("idleSessionTTLInSeconds" Core..=) Core.<$>
                    idleSessionTTLInSeconds,
                  ("intents" Core..=) Core.<$> intents,
                  ("nluIntentConfidenceThreshold" Core..=) Core.<$>
                    nluIntentConfidenceThreshold,
                  ("processBehavior" Core..=) Core.<$> processBehavior,
                  ("tags" Core..=) Core.<$> tags,
                  ("voiceId" Core..=) Core.<$> voiceId])

instance Core.AWSRequest PutBot where
        type Rs PutBot = PutBotResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath =
                           "/bots/" Core.<> Core.toText name Core.<> "/versions/$LATEST",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 PutBotResponse' Core.<$>
                   (x Core..:? "abortStatement") Core.<*> x Core..:? "checksum"
                     Core.<*> x Core..:? "childDirected"
                     Core.<*> x Core..:? "clarificationPrompt"
                     Core.<*> x Core..:? "createVersion"
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
                     Core.<*> x Core..:? "tags"
                     Core.<*> x Core..:? "version"
                     Core.<*> x Core..:? "voiceId"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPutBotResponse' smart constructor.
data PutBotResponse = PutBotResponse'
  { abortStatement :: Core.Maybe Types.Statement
    -- ^ The message that Amazon Lex uses to cancel a conversation. For more information, see 'PutBot' .
  , checksum :: Core.Maybe Core.Text
    -- ^ Checksum of the bot that you created.
  , childDirected :: Core.Maybe Core.Bool
    -- ^ For each Amazon Lex bot created with the Amazon Lex Model Building Service, you must specify whether your use of Amazon Lex is related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to the Children's Online Privacy Protection Act (COPPA) by specifying @true@ or @false@ in the @childDirected@ field. By specifying @true@ in the @childDirected@ field, you confirm that your use of Amazon Lex __is__ related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA. By specifying @false@ in the @childDirected@ field, you confirm that your use of Amazon Lex __is not__ related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA. You may not specify a default value for the @childDirected@ field that does not accurately reflect whether your use of Amazon Lex is related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA.
--
-- If your use of Amazon Lex relates to a website, program, or other application that is directed in whole or in part, to children under age 13, you must obtain any required verifiable parental consent under COPPA. For information regarding the use of Amazon Lex in connection with websites, programs, or other applications that are directed or targeted, in whole or in part, to children under age 13, see the <https://aws.amazon.com/lex/faqs#data-security Amazon Lex FAQ.> 
  , clarificationPrompt :: Core.Maybe Types.Prompt
    -- ^ The prompts that Amazon Lex uses when it doesn't understand the user's intent. For more information, see 'PutBot' . 
  , createVersion :: Core.Maybe Core.Bool
    -- ^ @True@ if a new version of the bot was created. If the @createVersion@ field was not specified in the request, the @createVersion@ field is set to false in the response.
  , createdDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date that the bot was created.
  , description :: Core.Maybe Types.Description
    -- ^ A description of the bot.
  , detectSentiment :: Core.Maybe Core.Bool
    -- ^ @true@ if the bot is configured to send user utterances to Amazon Comprehend for sentiment analysis. If the @detectSentiment@ field was not specified in the request, the @detectSentiment@ field is @false@ in the response.
  , enableModelImprovements :: Core.Maybe Core.Bool
    -- ^ Indicates whether the bot uses accuracy improvements. @true@ indicates that the bot is using the improvements, otherwise, @false@ .
  , failureReason :: Core.Maybe Core.Text
    -- ^ If @status@ is @FAILED@ , Amazon Lex provides the reason that it failed to build the bot.
  , idleSessionTTLInSeconds :: Core.Maybe Core.Natural
    -- ^ The maximum length of time that Amazon Lex retains the data gathered in a conversation. For more information, see 'PutBot' .
  , intents :: Core.Maybe [Types.Intent]
    -- ^ An array of @Intent@ objects. For more information, see 'PutBot' .
  , lastUpdatedDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date that the bot was updated. When you create a resource, the creation date and last updated date are the same.
  , locale :: Core.Maybe Types.Locale
    -- ^ The target locale for the bot. 
  , name :: Core.Maybe Types.BotName
    -- ^ The name of the bot.
  , nluIntentConfidenceThreshold :: Core.Maybe Core.Double
    -- ^ The score that determines where Amazon Lex inserts the @AMAZON.FallbackIntent@ , @AMAZON.KendraSearchIntent@ , or both when returning alternative intents in a <https://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostContent.html PostContent> or <https://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostText.html PostText> response. @AMAZON.FallbackIntent@ is inserted if the confidence score for all intents is below this value. @AMAZON.KendraSearchIntent@ is only inserted if it is configured for the bot.
  , status :: Core.Maybe Types.LexStatus
    -- ^ When you send a request to create a bot with @processBehavior@ set to @BUILD@ , Amazon Lex sets the @status@ response element to @BUILDING@ .
--
-- In the @READY_BASIC_TESTING@ state you can test the bot with user inputs that exactly match the utterances configured for the bot's intents and values in the slot types.
-- If Amazon Lex can't build the bot, Amazon Lex sets @status@ to @FAILED@ . Amazon Lex returns the reason for the failure in the @failureReason@ response element. 
-- When you set @processBehavior@ to @SAVE@ , Amazon Lex sets the status code to @NOT BUILT@ .
-- When the bot is in the @READY@ state you can test and publish the bot.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ A list of tags associated with the bot.
  , version :: Core.Maybe Types.Version
    -- ^ The version of the bot. For a new bot, the version is always @> LATEST@ .
  , voiceId :: Core.Maybe Core.Text
    -- ^ The Amazon Polly voice ID that Amazon Lex uses for voice interaction with the user. For more information, see 'PutBot' .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'PutBotResponse' value with any optional fields omitted.
mkPutBotResponse
    :: Core.Int -- ^ 'responseStatus'
    -> PutBotResponse
mkPutBotResponse responseStatus
  = PutBotResponse'{abortStatement = Core.Nothing,
                    checksum = Core.Nothing, childDirected = Core.Nothing,
                    clarificationPrompt = Core.Nothing, createVersion = Core.Nothing,
                    createdDate = Core.Nothing, description = Core.Nothing,
                    detectSentiment = Core.Nothing,
                    enableModelImprovements = Core.Nothing,
                    failureReason = Core.Nothing,
                    idleSessionTTLInSeconds = Core.Nothing, intents = Core.Nothing,
                    lastUpdatedDate = Core.Nothing, locale = Core.Nothing,
                    name = Core.Nothing, nluIntentConfidenceThreshold = Core.Nothing,
                    status = Core.Nothing, tags = Core.Nothing, version = Core.Nothing,
                    voiceId = Core.Nothing, responseStatus}

-- | The message that Amazon Lex uses to cancel a conversation. For more information, see 'PutBot' .
--
-- /Note:/ Consider using 'abortStatement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbrrsAbortStatement :: Lens.Lens' PutBotResponse (Core.Maybe Types.Statement)
pbrrsAbortStatement = Lens.field @"abortStatement"
{-# INLINEABLE pbrrsAbortStatement #-}
{-# DEPRECATED abortStatement "Use generic-lens or generic-optics with 'abortStatement' instead"  #-}

-- | Checksum of the bot that you created.
--
-- /Note:/ Consider using 'checksum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbrrsChecksum :: Lens.Lens' PutBotResponse (Core.Maybe Core.Text)
pbrrsChecksum = Lens.field @"checksum"
{-# INLINEABLE pbrrsChecksum #-}
{-# DEPRECATED checksum "Use generic-lens or generic-optics with 'checksum' instead"  #-}

-- | For each Amazon Lex bot created with the Amazon Lex Model Building Service, you must specify whether your use of Amazon Lex is related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to the Children's Online Privacy Protection Act (COPPA) by specifying @true@ or @false@ in the @childDirected@ field. By specifying @true@ in the @childDirected@ field, you confirm that your use of Amazon Lex __is__ related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA. By specifying @false@ in the @childDirected@ field, you confirm that your use of Amazon Lex __is not__ related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA. You may not specify a default value for the @childDirected@ field that does not accurately reflect whether your use of Amazon Lex is related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA.
--
-- If your use of Amazon Lex relates to a website, program, or other application that is directed in whole or in part, to children under age 13, you must obtain any required verifiable parental consent under COPPA. For information regarding the use of Amazon Lex in connection with websites, programs, or other applications that are directed or targeted, in whole or in part, to children under age 13, see the <https://aws.amazon.com/lex/faqs#data-security Amazon Lex FAQ.> 
--
-- /Note:/ Consider using 'childDirected' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbrrsChildDirected :: Lens.Lens' PutBotResponse (Core.Maybe Core.Bool)
pbrrsChildDirected = Lens.field @"childDirected"
{-# INLINEABLE pbrrsChildDirected #-}
{-# DEPRECATED childDirected "Use generic-lens or generic-optics with 'childDirected' instead"  #-}

-- | The prompts that Amazon Lex uses when it doesn't understand the user's intent. For more information, see 'PutBot' . 
--
-- /Note:/ Consider using 'clarificationPrompt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbrrsClarificationPrompt :: Lens.Lens' PutBotResponse (Core.Maybe Types.Prompt)
pbrrsClarificationPrompt = Lens.field @"clarificationPrompt"
{-# INLINEABLE pbrrsClarificationPrompt #-}
{-# DEPRECATED clarificationPrompt "Use generic-lens or generic-optics with 'clarificationPrompt' instead"  #-}

-- | @True@ if a new version of the bot was created. If the @createVersion@ field was not specified in the request, the @createVersion@ field is set to false in the response.
--
-- /Note:/ Consider using 'createVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbrrsCreateVersion :: Lens.Lens' PutBotResponse (Core.Maybe Core.Bool)
pbrrsCreateVersion = Lens.field @"createVersion"
{-# INLINEABLE pbrrsCreateVersion #-}
{-# DEPRECATED createVersion "Use generic-lens or generic-optics with 'createVersion' instead"  #-}

-- | The date that the bot was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbrrsCreatedDate :: Lens.Lens' PutBotResponse (Core.Maybe Core.NominalDiffTime)
pbrrsCreatedDate = Lens.field @"createdDate"
{-# INLINEABLE pbrrsCreatedDate #-}
{-# DEPRECATED createdDate "Use generic-lens or generic-optics with 'createdDate' instead"  #-}

-- | A description of the bot.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbrrsDescription :: Lens.Lens' PutBotResponse (Core.Maybe Types.Description)
pbrrsDescription = Lens.field @"description"
{-# INLINEABLE pbrrsDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | @true@ if the bot is configured to send user utterances to Amazon Comprehend for sentiment analysis. If the @detectSentiment@ field was not specified in the request, the @detectSentiment@ field is @false@ in the response.
--
-- /Note:/ Consider using 'detectSentiment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbrrsDetectSentiment :: Lens.Lens' PutBotResponse (Core.Maybe Core.Bool)
pbrrsDetectSentiment = Lens.field @"detectSentiment"
{-# INLINEABLE pbrrsDetectSentiment #-}
{-# DEPRECATED detectSentiment "Use generic-lens or generic-optics with 'detectSentiment' instead"  #-}

-- | Indicates whether the bot uses accuracy improvements. @true@ indicates that the bot is using the improvements, otherwise, @false@ .
--
-- /Note:/ Consider using 'enableModelImprovements' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbrrsEnableModelImprovements :: Lens.Lens' PutBotResponse (Core.Maybe Core.Bool)
pbrrsEnableModelImprovements = Lens.field @"enableModelImprovements"
{-# INLINEABLE pbrrsEnableModelImprovements #-}
{-# DEPRECATED enableModelImprovements "Use generic-lens or generic-optics with 'enableModelImprovements' instead"  #-}

-- | If @status@ is @FAILED@ , Amazon Lex provides the reason that it failed to build the bot.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbrrsFailureReason :: Lens.Lens' PutBotResponse (Core.Maybe Core.Text)
pbrrsFailureReason = Lens.field @"failureReason"
{-# INLINEABLE pbrrsFailureReason #-}
{-# DEPRECATED failureReason "Use generic-lens or generic-optics with 'failureReason' instead"  #-}

-- | The maximum length of time that Amazon Lex retains the data gathered in a conversation. For more information, see 'PutBot' .
--
-- /Note:/ Consider using 'idleSessionTTLInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbrrsIdleSessionTTLInSeconds :: Lens.Lens' PutBotResponse (Core.Maybe Core.Natural)
pbrrsIdleSessionTTLInSeconds = Lens.field @"idleSessionTTLInSeconds"
{-# INLINEABLE pbrrsIdleSessionTTLInSeconds #-}
{-# DEPRECATED idleSessionTTLInSeconds "Use generic-lens or generic-optics with 'idleSessionTTLInSeconds' instead"  #-}

-- | An array of @Intent@ objects. For more information, see 'PutBot' .
--
-- /Note:/ Consider using 'intents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbrrsIntents :: Lens.Lens' PutBotResponse (Core.Maybe [Types.Intent])
pbrrsIntents = Lens.field @"intents"
{-# INLINEABLE pbrrsIntents #-}
{-# DEPRECATED intents "Use generic-lens or generic-optics with 'intents' instead"  #-}

-- | The date that the bot was updated. When you create a resource, the creation date and last updated date are the same.
--
-- /Note:/ Consider using 'lastUpdatedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbrrsLastUpdatedDate :: Lens.Lens' PutBotResponse (Core.Maybe Core.NominalDiffTime)
pbrrsLastUpdatedDate = Lens.field @"lastUpdatedDate"
{-# INLINEABLE pbrrsLastUpdatedDate #-}
{-# DEPRECATED lastUpdatedDate "Use generic-lens or generic-optics with 'lastUpdatedDate' instead"  #-}

-- | The target locale for the bot. 
--
-- /Note:/ Consider using 'locale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbrrsLocale :: Lens.Lens' PutBotResponse (Core.Maybe Types.Locale)
pbrrsLocale = Lens.field @"locale"
{-# INLINEABLE pbrrsLocale #-}
{-# DEPRECATED locale "Use generic-lens or generic-optics with 'locale' instead"  #-}

-- | The name of the bot.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbrrsName :: Lens.Lens' PutBotResponse (Core.Maybe Types.BotName)
pbrrsName = Lens.field @"name"
{-# INLINEABLE pbrrsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The score that determines where Amazon Lex inserts the @AMAZON.FallbackIntent@ , @AMAZON.KendraSearchIntent@ , or both when returning alternative intents in a <https://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostContent.html PostContent> or <https://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostText.html PostText> response. @AMAZON.FallbackIntent@ is inserted if the confidence score for all intents is below this value. @AMAZON.KendraSearchIntent@ is only inserted if it is configured for the bot.
--
-- /Note:/ Consider using 'nluIntentConfidenceThreshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbrrsNluIntentConfidenceThreshold :: Lens.Lens' PutBotResponse (Core.Maybe Core.Double)
pbrrsNluIntentConfidenceThreshold = Lens.field @"nluIntentConfidenceThreshold"
{-# INLINEABLE pbrrsNluIntentConfidenceThreshold #-}
{-# DEPRECATED nluIntentConfidenceThreshold "Use generic-lens or generic-optics with 'nluIntentConfidenceThreshold' instead"  #-}

-- | When you send a request to create a bot with @processBehavior@ set to @BUILD@ , Amazon Lex sets the @status@ response element to @BUILDING@ .
--
-- In the @READY_BASIC_TESTING@ state you can test the bot with user inputs that exactly match the utterances configured for the bot's intents and values in the slot types.
-- If Amazon Lex can't build the bot, Amazon Lex sets @status@ to @FAILED@ . Amazon Lex returns the reason for the failure in the @failureReason@ response element. 
-- When you set @processBehavior@ to @SAVE@ , Amazon Lex sets the status code to @NOT BUILT@ .
-- When the bot is in the @READY@ state you can test and publish the bot.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbrrsStatus :: Lens.Lens' PutBotResponse (Core.Maybe Types.LexStatus)
pbrrsStatus = Lens.field @"status"
{-# INLINEABLE pbrrsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | A list of tags associated with the bot.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbrrsTags :: Lens.Lens' PutBotResponse (Core.Maybe [Types.Tag])
pbrrsTags = Lens.field @"tags"
{-# INLINEABLE pbrrsTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The version of the bot. For a new bot, the version is always @> LATEST@ .
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbrrsVersion :: Lens.Lens' PutBotResponse (Core.Maybe Types.Version)
pbrrsVersion = Lens.field @"version"
{-# INLINEABLE pbrrsVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

-- | The Amazon Polly voice ID that Amazon Lex uses for voice interaction with the user. For more information, see 'PutBot' .
--
-- /Note:/ Consider using 'voiceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbrrsVoiceId :: Lens.Lens' PutBotResponse (Core.Maybe Core.Text)
pbrrsVoiceId = Lens.field @"voiceId"
{-# INLINEABLE pbrrsVoiceId #-}
{-# DEPRECATED voiceId "Use generic-lens or generic-optics with 'voiceId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbrrsResponseStatus :: Lens.Lens' PutBotResponse Core.Int
pbrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE pbrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
