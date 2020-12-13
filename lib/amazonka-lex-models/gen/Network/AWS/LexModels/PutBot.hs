{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    PutBot (..),
    mkPutBot,

    -- ** Request lenses
    pbAbortStatement,
    pbIntents,
    pbChecksum,
    pbEnableModelImprovements,
    pbNluIntentConfidenceThreshold,
    pbDetectSentiment,
    pbLocale,
    pbName,
    pbProcessBehavior,
    pbIdleSessionTTLInSeconds,
    pbClarificationPrompt,
    pbVoiceId,
    pbCreateVersion,
    pbChildDirected,
    pbDescription,
    pbTags,

    -- * Destructuring the response
    PutBotResponse (..),
    mkPutBotResponse,

    -- ** Response lenses
    pbrsFailureReason,
    pbrsStatus,
    pbrsAbortStatement,
    pbrsIntents,
    pbrsChecksum,
    pbrsEnableModelImprovements,
    pbrsNluIntentConfidenceThreshold,
    pbrsDetectSentiment,
    pbrsLocale,
    pbrsCreatedDate,
    pbrsName,
    pbrsVersion,
    pbrsIdleSessionTTLInSeconds,
    pbrsClarificationPrompt,
    pbrsVoiceId,
    pbrsLastUpdatedDate,
    pbrsCreateVersion,
    pbrsChildDirected,
    pbrsDescription,
    pbrsTags,
    pbrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutBot' smart constructor.
data PutBot = PutBot'
  { -- | When Amazon Lex can't understand the user's input in context, it tries to elicit the information a few times. After that, Amazon Lex sends the message defined in @abortStatement@ to the user, and then cancels the conversation. To set the number of retries, use the @valueElicitationPrompt@ field for the slot type.
    --
    -- For example, in a pizza ordering bot, Amazon Lex might ask a user "What type of crust would you like?" If the user's response is not one of the expected responses (for example, "thin crust, "deep dish," etc.), Amazon Lex tries to elicit a correct response a few more times.
    -- For example, in a pizza ordering application, @OrderPizza@ might be one of the intents. This intent might require the @CrustType@ slot. You specify the @valueElicitationPrompt@ field when you create the @CrustType@ slot.
    -- If you have defined a fallback intent the cancel statement will not be sent to the user, the fallback intent is used instead. For more information, see <https://docs.aws.amazon.com/lex/latest/dg/built-in-intent-fallback.html AMAZON.FallbackIntent> .
    abortStatement :: Lude.Maybe Statement,
    -- | An array of @Intent@ objects. Each intent represents a command that a user can express. For example, a pizza ordering bot might support an OrderPizza intent. For more information, see 'how-it-works' .
    intents :: Lude.Maybe [Intent],
    -- | Identifies a specific revision of the @> LATEST@ version.
    --
    -- When you create a new bot, leave the @checksum@ field blank. If you specify a checksum you get a @BadRequestException@ exception.
    -- When you want to update a bot, set the @checksum@ field to the checksum of the most recent revision of the @> LATEST@ version. If you don't specify the @checksum@ field, or if the checksum does not match the @> LATEST@ version, you get a @PreconditionFailedException@ exception.
    checksum :: Lude.Maybe Lude.Text,
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
    enableModelImprovements :: Lude.Maybe Lude.Bool,
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
    nluIntentConfidenceThreshold :: Lude.Maybe Lude.Double,
    -- | When set to @true@ user utterances are sent to Amazon Comprehend for sentiment analysis. If you don't specify @detectSentiment@ , the default is @false@ .
    detectSentiment :: Lude.Maybe Lude.Bool,
    -- | Specifies the target locale for the bot. Any intent used in the bot must be compatible with the locale of the bot.
    --
    -- The default is @en-US@ .
    locale :: Locale,
    -- | The name of the bot. The name is /not/ case sensitive.
    name :: Lude.Text,
    -- | If you set the @processBehavior@ element to @BUILD@ , Amazon Lex builds the bot so that it can be run. If you set the element to @SAVE@ Amazon Lex saves the bot, but doesn't build it.
    --
    -- If you don't specify this value, the default value is @BUILD@ .
    processBehavior :: Lude.Maybe ProcessBehavior,
    -- | The maximum time in seconds that Amazon Lex retains the data gathered in a conversation.
    --
    -- A user interaction session remains active for the amount of time specified. If no conversation occurs during this time, the session expires and Amazon Lex deletes any data provided before the timeout.
    -- For example, suppose that a user chooses the OrderPizza intent, but gets sidetracked halfway through placing an order. If the user doesn't complete the order within the specified time, Amazon Lex discards the slot information that it gathered, and the user must start over.
    -- If you don't include the @idleSessionTTLInSeconds@ element in a @PutBot@ operation request, Amazon Lex uses the default value. This is also true if the request replaces an existing bot.
    -- The default is 300 seconds (5 minutes).
    idleSessionTTLInSeconds :: Lude.Maybe Lude.Natural,
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
    clarificationPrompt :: Lude.Maybe Prompt,
    -- | The Amazon Polly voice ID that you want Amazon Lex to use for voice interactions with the user. The locale configured for the voice must match the locale of the bot. For more information, see <https://docs.aws.amazon.com/polly/latest/dg/voicelist.html Voices in Amazon Polly> in the /Amazon Polly Developer Guide/ .
    voiceId :: Lude.Maybe Lude.Text,
    -- | When set to @true@ a new numbered version of the bot is created. This is the same as calling the @CreateBotVersion@ operation. If you don't specify @createVersion@ , the default is @false@ .
    createVersion :: Lude.Maybe Lude.Bool,
    -- | For each Amazon Lex bot created with the Amazon Lex Model Building Service, you must specify whether your use of Amazon Lex is related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to the Children's Online Privacy Protection Act (COPPA) by specifying @true@ or @false@ in the @childDirected@ field. By specifying @true@ in the @childDirected@ field, you confirm that your use of Amazon Lex __is__ related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA. By specifying @false@ in the @childDirected@ field, you confirm that your use of Amazon Lex __is not__ related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA. You may not specify a default value for the @childDirected@ field that does not accurately reflect whether your use of Amazon Lex is related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA.
    --
    -- If your use of Amazon Lex relates to a website, program, or other application that is directed in whole or in part, to children under age 13, you must obtain any required verifiable parental consent under COPPA. For information regarding the use of Amazon Lex in connection with websites, programs, or other applications that are directed or targeted, in whole or in part, to children under age 13, see the <https://aws.amazon.com/lex/faqs#data-security Amazon Lex FAQ.>
    childDirected :: Lude.Bool,
    -- | A description of the bot.
    description :: Lude.Maybe Lude.Text,
    -- | A list of tags to add to the bot. You can only add tags when you create a bot, you can't use the @PutBot@ operation to update the tags on a bot. To update tags, use the @TagResource@ operation.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutBot' with the minimum fields required to make a request.
--
-- * 'abortStatement' - When Amazon Lex can't understand the user's input in context, it tries to elicit the information a few times. After that, Amazon Lex sends the message defined in @abortStatement@ to the user, and then cancels the conversation. To set the number of retries, use the @valueElicitationPrompt@ field for the slot type.
--
-- For example, in a pizza ordering bot, Amazon Lex might ask a user "What type of crust would you like?" If the user's response is not one of the expected responses (for example, "thin crust, "deep dish," etc.), Amazon Lex tries to elicit a correct response a few more times.
-- For example, in a pizza ordering application, @OrderPizza@ might be one of the intents. This intent might require the @CrustType@ slot. You specify the @valueElicitationPrompt@ field when you create the @CrustType@ slot.
-- If you have defined a fallback intent the cancel statement will not be sent to the user, the fallback intent is used instead. For more information, see <https://docs.aws.amazon.com/lex/latest/dg/built-in-intent-fallback.html AMAZON.FallbackIntent> .
-- * 'intents' - An array of @Intent@ objects. Each intent represents a command that a user can express. For example, a pizza ordering bot might support an OrderPizza intent. For more information, see 'how-it-works' .
-- * 'checksum' - Identifies a specific revision of the @> LATEST@ version.
--
-- When you create a new bot, leave the @checksum@ field blank. If you specify a checksum you get a @BadRequestException@ exception.
-- When you want to update a bot, set the @checksum@ field to the checksum of the most recent revision of the @> LATEST@ version. If you don't specify the @checksum@ field, or if the checksum does not match the @> LATEST@ version, you get a @PreconditionFailedException@ exception.
-- * 'enableModelImprovements' - Set to @true@ to enable access to natural language understanding improvements.
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
-- * 'nluIntentConfidenceThreshold' - Determines the threshold where Amazon Lex will insert the @AMAZON.FallbackIntent@ , @AMAZON.KendraSearchIntent@ , or both when returning alternative intents in a <https://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostContent.html PostContent> or <https://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostText.html PostText> response. @AMAZON.FallbackIntent@ and @AMAZON.KendraSearchIntent@ are only inserted if they are configured for the bot.
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
-- * 'detectSentiment' - When set to @true@ user utterances are sent to Amazon Comprehend for sentiment analysis. If you don't specify @detectSentiment@ , the default is @false@ .
-- * 'locale' - Specifies the target locale for the bot. Any intent used in the bot must be compatible with the locale of the bot.
--
-- The default is @en-US@ .
-- * 'name' - The name of the bot. The name is /not/ case sensitive.
-- * 'processBehavior' - If you set the @processBehavior@ element to @BUILD@ , Amazon Lex builds the bot so that it can be run. If you set the element to @SAVE@ Amazon Lex saves the bot, but doesn't build it.
--
-- If you don't specify this value, the default value is @BUILD@ .
-- * 'idleSessionTTLInSeconds' - The maximum time in seconds that Amazon Lex retains the data gathered in a conversation.
--
-- A user interaction session remains active for the amount of time specified. If no conversation occurs during this time, the session expires and Amazon Lex deletes any data provided before the timeout.
-- For example, suppose that a user chooses the OrderPizza intent, but gets sidetracked halfway through placing an order. If the user doesn't complete the order within the specified time, Amazon Lex discards the slot information that it gathered, and the user must start over.
-- If you don't include the @idleSessionTTLInSeconds@ element in a @PutBot@ operation request, Amazon Lex uses the default value. This is also true if the request replaces an existing bot.
-- The default is 300 seconds (5 minutes).
-- * 'clarificationPrompt' - When Amazon Lex doesn't understand the user's intent, it uses this message to get clarification. To specify how many times Amazon Lex should repeat the clarification prompt, use the @maxAttempts@ field. If Amazon Lex still doesn't understand, it sends the message in the @abortStatement@ field.
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
-- * 'voiceId' - The Amazon Polly voice ID that you want Amazon Lex to use for voice interactions with the user. The locale configured for the voice must match the locale of the bot. For more information, see <https://docs.aws.amazon.com/polly/latest/dg/voicelist.html Voices in Amazon Polly> in the /Amazon Polly Developer Guide/ .
-- * 'createVersion' - When set to @true@ a new numbered version of the bot is created. This is the same as calling the @CreateBotVersion@ operation. If you don't specify @createVersion@ , the default is @false@ .
-- * 'childDirected' - For each Amazon Lex bot created with the Amazon Lex Model Building Service, you must specify whether your use of Amazon Lex is related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to the Children's Online Privacy Protection Act (COPPA) by specifying @true@ or @false@ in the @childDirected@ field. By specifying @true@ in the @childDirected@ field, you confirm that your use of Amazon Lex __is__ related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA. By specifying @false@ in the @childDirected@ field, you confirm that your use of Amazon Lex __is not__ related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA. You may not specify a default value for the @childDirected@ field that does not accurately reflect whether your use of Amazon Lex is related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA.
--
-- If your use of Amazon Lex relates to a website, program, or other application that is directed in whole or in part, to children under age 13, you must obtain any required verifiable parental consent under COPPA. For information regarding the use of Amazon Lex in connection with websites, programs, or other applications that are directed or targeted, in whole or in part, to children under age 13, see the <https://aws.amazon.com/lex/faqs#data-security Amazon Lex FAQ.>
-- * 'description' - A description of the bot.
-- * 'tags' - A list of tags to add to the bot. You can only add tags when you create a bot, you can't use the @PutBot@ operation to update the tags on a bot. To update tags, use the @TagResource@ operation.
mkPutBot ::
  -- | 'locale'
  Locale ->
  -- | 'name'
  Lude.Text ->
  -- | 'childDirected'
  Lude.Bool ->
  PutBot
mkPutBot pLocale_ pName_ pChildDirected_ =
  PutBot'
    { abortStatement = Lude.Nothing,
      intents = Lude.Nothing,
      checksum = Lude.Nothing,
      enableModelImprovements = Lude.Nothing,
      nluIntentConfidenceThreshold = Lude.Nothing,
      detectSentiment = Lude.Nothing,
      locale = pLocale_,
      name = pName_,
      processBehavior = Lude.Nothing,
      idleSessionTTLInSeconds = Lude.Nothing,
      clarificationPrompt = Lude.Nothing,
      voiceId = Lude.Nothing,
      createVersion = Lude.Nothing,
      childDirected = pChildDirected_,
      description = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | When Amazon Lex can't understand the user's input in context, it tries to elicit the information a few times. After that, Amazon Lex sends the message defined in @abortStatement@ to the user, and then cancels the conversation. To set the number of retries, use the @valueElicitationPrompt@ field for the slot type.
--
-- For example, in a pizza ordering bot, Amazon Lex might ask a user "What type of crust would you like?" If the user's response is not one of the expected responses (for example, "thin crust, "deep dish," etc.), Amazon Lex tries to elicit a correct response a few more times.
-- For example, in a pizza ordering application, @OrderPizza@ might be one of the intents. This intent might require the @CrustType@ slot. You specify the @valueElicitationPrompt@ field when you create the @CrustType@ slot.
-- If you have defined a fallback intent the cancel statement will not be sent to the user, the fallback intent is used instead. For more information, see <https://docs.aws.amazon.com/lex/latest/dg/built-in-intent-fallback.html AMAZON.FallbackIntent> .
--
-- /Note:/ Consider using 'abortStatement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbAbortStatement :: Lens.Lens' PutBot (Lude.Maybe Statement)
pbAbortStatement = Lens.lens (abortStatement :: PutBot -> Lude.Maybe Statement) (\s a -> s {abortStatement = a} :: PutBot)
{-# DEPRECATED pbAbortStatement "Use generic-lens or generic-optics with 'abortStatement' instead." #-}

-- | An array of @Intent@ objects. Each intent represents a command that a user can express. For example, a pizza ordering bot might support an OrderPizza intent. For more information, see 'how-it-works' .
--
-- /Note:/ Consider using 'intents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbIntents :: Lens.Lens' PutBot (Lude.Maybe [Intent])
pbIntents = Lens.lens (intents :: PutBot -> Lude.Maybe [Intent]) (\s a -> s {intents = a} :: PutBot)
{-# DEPRECATED pbIntents "Use generic-lens or generic-optics with 'intents' instead." #-}

-- | Identifies a specific revision of the @> LATEST@ version.
--
-- When you create a new bot, leave the @checksum@ field blank. If you specify a checksum you get a @BadRequestException@ exception.
-- When you want to update a bot, set the @checksum@ field to the checksum of the most recent revision of the @> LATEST@ version. If you don't specify the @checksum@ field, or if the checksum does not match the @> LATEST@ version, you get a @PreconditionFailedException@ exception.
--
-- /Note:/ Consider using 'checksum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbChecksum :: Lens.Lens' PutBot (Lude.Maybe Lude.Text)
pbChecksum = Lens.lens (checksum :: PutBot -> Lude.Maybe Lude.Text) (\s a -> s {checksum = a} :: PutBot)
{-# DEPRECATED pbChecksum "Use generic-lens or generic-optics with 'checksum' instead." #-}

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
pbEnableModelImprovements :: Lens.Lens' PutBot (Lude.Maybe Lude.Bool)
pbEnableModelImprovements = Lens.lens (enableModelImprovements :: PutBot -> Lude.Maybe Lude.Bool) (\s a -> s {enableModelImprovements = a} :: PutBot)
{-# DEPRECATED pbEnableModelImprovements "Use generic-lens or generic-optics with 'enableModelImprovements' instead." #-}

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
pbNluIntentConfidenceThreshold :: Lens.Lens' PutBot (Lude.Maybe Lude.Double)
pbNluIntentConfidenceThreshold = Lens.lens (nluIntentConfidenceThreshold :: PutBot -> Lude.Maybe Lude.Double) (\s a -> s {nluIntentConfidenceThreshold = a} :: PutBot)
{-# DEPRECATED pbNluIntentConfidenceThreshold "Use generic-lens or generic-optics with 'nluIntentConfidenceThreshold' instead." #-}

-- | When set to @true@ user utterances are sent to Amazon Comprehend for sentiment analysis. If you don't specify @detectSentiment@ , the default is @false@ .
--
-- /Note:/ Consider using 'detectSentiment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbDetectSentiment :: Lens.Lens' PutBot (Lude.Maybe Lude.Bool)
pbDetectSentiment = Lens.lens (detectSentiment :: PutBot -> Lude.Maybe Lude.Bool) (\s a -> s {detectSentiment = a} :: PutBot)
{-# DEPRECATED pbDetectSentiment "Use generic-lens or generic-optics with 'detectSentiment' instead." #-}

-- | Specifies the target locale for the bot. Any intent used in the bot must be compatible with the locale of the bot.
--
-- The default is @en-US@ .
--
-- /Note:/ Consider using 'locale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbLocale :: Lens.Lens' PutBot Locale
pbLocale = Lens.lens (locale :: PutBot -> Locale) (\s a -> s {locale = a} :: PutBot)
{-# DEPRECATED pbLocale "Use generic-lens or generic-optics with 'locale' instead." #-}

-- | The name of the bot. The name is /not/ case sensitive.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbName :: Lens.Lens' PutBot Lude.Text
pbName = Lens.lens (name :: PutBot -> Lude.Text) (\s a -> s {name = a} :: PutBot)
{-# DEPRECATED pbName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | If you set the @processBehavior@ element to @BUILD@ , Amazon Lex builds the bot so that it can be run. If you set the element to @SAVE@ Amazon Lex saves the bot, but doesn't build it.
--
-- If you don't specify this value, the default value is @BUILD@ .
--
-- /Note:/ Consider using 'processBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbProcessBehavior :: Lens.Lens' PutBot (Lude.Maybe ProcessBehavior)
pbProcessBehavior = Lens.lens (processBehavior :: PutBot -> Lude.Maybe ProcessBehavior) (\s a -> s {processBehavior = a} :: PutBot)
{-# DEPRECATED pbProcessBehavior "Use generic-lens or generic-optics with 'processBehavior' instead." #-}

-- | The maximum time in seconds that Amazon Lex retains the data gathered in a conversation.
--
-- A user interaction session remains active for the amount of time specified. If no conversation occurs during this time, the session expires and Amazon Lex deletes any data provided before the timeout.
-- For example, suppose that a user chooses the OrderPizza intent, but gets sidetracked halfway through placing an order. If the user doesn't complete the order within the specified time, Amazon Lex discards the slot information that it gathered, and the user must start over.
-- If you don't include the @idleSessionTTLInSeconds@ element in a @PutBot@ operation request, Amazon Lex uses the default value. This is also true if the request replaces an existing bot.
-- The default is 300 seconds (5 minutes).
--
-- /Note:/ Consider using 'idleSessionTTLInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbIdleSessionTTLInSeconds :: Lens.Lens' PutBot (Lude.Maybe Lude.Natural)
pbIdleSessionTTLInSeconds = Lens.lens (idleSessionTTLInSeconds :: PutBot -> Lude.Maybe Lude.Natural) (\s a -> s {idleSessionTTLInSeconds = a} :: PutBot)
{-# DEPRECATED pbIdleSessionTTLInSeconds "Use generic-lens or generic-optics with 'idleSessionTTLInSeconds' instead." #-}

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
pbClarificationPrompt :: Lens.Lens' PutBot (Lude.Maybe Prompt)
pbClarificationPrompt = Lens.lens (clarificationPrompt :: PutBot -> Lude.Maybe Prompt) (\s a -> s {clarificationPrompt = a} :: PutBot)
{-# DEPRECATED pbClarificationPrompt "Use generic-lens or generic-optics with 'clarificationPrompt' instead." #-}

-- | The Amazon Polly voice ID that you want Amazon Lex to use for voice interactions with the user. The locale configured for the voice must match the locale of the bot. For more information, see <https://docs.aws.amazon.com/polly/latest/dg/voicelist.html Voices in Amazon Polly> in the /Amazon Polly Developer Guide/ .
--
-- /Note:/ Consider using 'voiceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbVoiceId :: Lens.Lens' PutBot (Lude.Maybe Lude.Text)
pbVoiceId = Lens.lens (voiceId :: PutBot -> Lude.Maybe Lude.Text) (\s a -> s {voiceId = a} :: PutBot)
{-# DEPRECATED pbVoiceId "Use generic-lens or generic-optics with 'voiceId' instead." #-}

-- | When set to @true@ a new numbered version of the bot is created. This is the same as calling the @CreateBotVersion@ operation. If you don't specify @createVersion@ , the default is @false@ .
--
-- /Note:/ Consider using 'createVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbCreateVersion :: Lens.Lens' PutBot (Lude.Maybe Lude.Bool)
pbCreateVersion = Lens.lens (createVersion :: PutBot -> Lude.Maybe Lude.Bool) (\s a -> s {createVersion = a} :: PutBot)
{-# DEPRECATED pbCreateVersion "Use generic-lens or generic-optics with 'createVersion' instead." #-}

-- | For each Amazon Lex bot created with the Amazon Lex Model Building Service, you must specify whether your use of Amazon Lex is related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to the Children's Online Privacy Protection Act (COPPA) by specifying @true@ or @false@ in the @childDirected@ field. By specifying @true@ in the @childDirected@ field, you confirm that your use of Amazon Lex __is__ related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA. By specifying @false@ in the @childDirected@ field, you confirm that your use of Amazon Lex __is not__ related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA. You may not specify a default value for the @childDirected@ field that does not accurately reflect whether your use of Amazon Lex is related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA.
--
-- If your use of Amazon Lex relates to a website, program, or other application that is directed in whole or in part, to children under age 13, you must obtain any required verifiable parental consent under COPPA. For information regarding the use of Amazon Lex in connection with websites, programs, or other applications that are directed or targeted, in whole or in part, to children under age 13, see the <https://aws.amazon.com/lex/faqs#data-security Amazon Lex FAQ.>
--
-- /Note:/ Consider using 'childDirected' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbChildDirected :: Lens.Lens' PutBot Lude.Bool
pbChildDirected = Lens.lens (childDirected :: PutBot -> Lude.Bool) (\s a -> s {childDirected = a} :: PutBot)
{-# DEPRECATED pbChildDirected "Use generic-lens or generic-optics with 'childDirected' instead." #-}

-- | A description of the bot.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbDescription :: Lens.Lens' PutBot (Lude.Maybe Lude.Text)
pbDescription = Lens.lens (description :: PutBot -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: PutBot)
{-# DEPRECATED pbDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A list of tags to add to the bot. You can only add tags when you create a bot, you can't use the @PutBot@ operation to update the tags on a bot. To update tags, use the @TagResource@ operation.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbTags :: Lens.Lens' PutBot (Lude.Maybe [Tag])
pbTags = Lens.lens (tags :: PutBot -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: PutBot)
{-# DEPRECATED pbTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest PutBot where
  type Rs PutBot = PutBotResponse
  request = Req.putJSON lexModelsService
  response =
    Res.receiveJSON
      ( \s h x ->
          PutBotResponse'
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
            Lude.<*> (x Lude..?> "createVersion")
            Lude.<*> (x Lude..?> "childDirected")
            Lude.<*> (x Lude..?> "description")
            Lude.<*> (x Lude..?> "tags" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutBot where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutBot where
  toJSON PutBot' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("abortStatement" Lude..=) Lude.<$> abortStatement,
            ("intents" Lude..=) Lude.<$> intents,
            ("checksum" Lude..=) Lude.<$> checksum,
            ("enableModelImprovements" Lude..=)
              Lude.<$> enableModelImprovements,
            ("nluIntentConfidenceThreshold" Lude..=)
              Lude.<$> nluIntentConfidenceThreshold,
            ("detectSentiment" Lude..=) Lude.<$> detectSentiment,
            Lude.Just ("locale" Lude..= locale),
            ("processBehavior" Lude..=) Lude.<$> processBehavior,
            ("idleSessionTTLInSeconds" Lude..=)
              Lude.<$> idleSessionTTLInSeconds,
            ("clarificationPrompt" Lude..=) Lude.<$> clarificationPrompt,
            ("voiceId" Lude..=) Lude.<$> voiceId,
            ("createVersion" Lude..=) Lude.<$> createVersion,
            Lude.Just ("childDirected" Lude..= childDirected),
            ("description" Lude..=) Lude.<$> description,
            ("tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath PutBot where
  toPath PutBot' {..} =
    Lude.mconcat ["/bots/", Lude.toBS name, "/versions/$LATEST"]

instance Lude.ToQuery PutBot where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutBotResponse' smart constructor.
data PutBotResponse = PutBotResponse'
  { -- | If @status@ is @FAILED@ , Amazon Lex provides the reason that it failed to build the bot.
    failureReason :: Lude.Maybe Lude.Text,
    -- | When you send a request to create a bot with @processBehavior@ set to @BUILD@ , Amazon Lex sets the @status@ response element to @BUILDING@ .
    --
    -- In the @READY_BASIC_TESTING@ state you can test the bot with user inputs that exactly match the utterances configured for the bot's intents and values in the slot types.
    -- If Amazon Lex can't build the bot, Amazon Lex sets @status@ to @FAILED@ . Amazon Lex returns the reason for the failure in the @failureReason@ response element.
    -- When you set @processBehavior@ to @SAVE@ , Amazon Lex sets the status code to @NOT BUILT@ .
    -- When the bot is in the @READY@ state you can test and publish the bot.
    status :: Lude.Maybe LexStatus,
    -- | The message that Amazon Lex uses to cancel a conversation. For more information, see 'PutBot' .
    abortStatement :: Lude.Maybe Statement,
    -- | An array of @Intent@ objects. For more information, see 'PutBot' .
    intents :: Lude.Maybe [Intent],
    -- | Checksum of the bot that you created.
    checksum :: Lude.Maybe Lude.Text,
    -- | Indicates whether the bot uses accuracy improvements. @true@ indicates that the bot is using the improvements, otherwise, @false@ .
    enableModelImprovements :: Lude.Maybe Lude.Bool,
    -- | The score that determines where Amazon Lex inserts the @AMAZON.FallbackIntent@ , @AMAZON.KendraSearchIntent@ , or both when returning alternative intents in a <https://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostContent.html PostContent> or <https://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostText.html PostText> response. @AMAZON.FallbackIntent@ is inserted if the confidence score for all intents is below this value. @AMAZON.KendraSearchIntent@ is only inserted if it is configured for the bot.
    nluIntentConfidenceThreshold :: Lude.Maybe Lude.Double,
    -- | @true@ if the bot is configured to send user utterances to Amazon Comprehend for sentiment analysis. If the @detectSentiment@ field was not specified in the request, the @detectSentiment@ field is @false@ in the response.
    detectSentiment :: Lude.Maybe Lude.Bool,
    -- | The target locale for the bot.
    locale :: Lude.Maybe Locale,
    -- | The date that the bot was created.
    createdDate :: Lude.Maybe Lude.Timestamp,
    -- | The name of the bot.
    name :: Lude.Maybe Lude.Text,
    -- | The version of the bot. For a new bot, the version is always @> LATEST@ .
    version :: Lude.Maybe Lude.Text,
    -- | The maximum length of time that Amazon Lex retains the data gathered in a conversation. For more information, see 'PutBot' .
    idleSessionTTLInSeconds :: Lude.Maybe Lude.Natural,
    -- | The prompts that Amazon Lex uses when it doesn't understand the user's intent. For more information, see 'PutBot' .
    clarificationPrompt :: Lude.Maybe Prompt,
    -- | The Amazon Polly voice ID that Amazon Lex uses for voice interaction with the user. For more information, see 'PutBot' .
    voiceId :: Lude.Maybe Lude.Text,
    -- | The date that the bot was updated. When you create a resource, the creation date and last updated date are the same.
    lastUpdatedDate :: Lude.Maybe Lude.Timestamp,
    -- | @True@ if a new version of the bot was created. If the @createVersion@ field was not specified in the request, the @createVersion@ field is set to false in the response.
    createVersion :: Lude.Maybe Lude.Bool,
    -- | For each Amazon Lex bot created with the Amazon Lex Model Building Service, you must specify whether your use of Amazon Lex is related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to the Children's Online Privacy Protection Act (COPPA) by specifying @true@ or @false@ in the @childDirected@ field. By specifying @true@ in the @childDirected@ field, you confirm that your use of Amazon Lex __is__ related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA. By specifying @false@ in the @childDirected@ field, you confirm that your use of Amazon Lex __is not__ related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA. You may not specify a default value for the @childDirected@ field that does not accurately reflect whether your use of Amazon Lex is related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA.
    --
    -- If your use of Amazon Lex relates to a website, program, or other application that is directed in whole or in part, to children under age 13, you must obtain any required verifiable parental consent under COPPA. For information regarding the use of Amazon Lex in connection with websites, programs, or other applications that are directed or targeted, in whole or in part, to children under age 13, see the <https://aws.amazon.com/lex/faqs#data-security Amazon Lex FAQ.>
    childDirected :: Lude.Maybe Lude.Bool,
    -- | A description of the bot.
    description :: Lude.Maybe Lude.Text,
    -- | A list of tags associated with the bot.
    tags :: Lude.Maybe [Tag],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutBotResponse' with the minimum fields required to make a request.
--
-- * 'failureReason' - If @status@ is @FAILED@ , Amazon Lex provides the reason that it failed to build the bot.
-- * 'status' - When you send a request to create a bot with @processBehavior@ set to @BUILD@ , Amazon Lex sets the @status@ response element to @BUILDING@ .
--
-- In the @READY_BASIC_TESTING@ state you can test the bot with user inputs that exactly match the utterances configured for the bot's intents and values in the slot types.
-- If Amazon Lex can't build the bot, Amazon Lex sets @status@ to @FAILED@ . Amazon Lex returns the reason for the failure in the @failureReason@ response element.
-- When you set @processBehavior@ to @SAVE@ , Amazon Lex sets the status code to @NOT BUILT@ .
-- When the bot is in the @READY@ state you can test and publish the bot.
-- * 'abortStatement' - The message that Amazon Lex uses to cancel a conversation. For more information, see 'PutBot' .
-- * 'intents' - An array of @Intent@ objects. For more information, see 'PutBot' .
-- * 'checksum' - Checksum of the bot that you created.
-- * 'enableModelImprovements' - Indicates whether the bot uses accuracy improvements. @true@ indicates that the bot is using the improvements, otherwise, @false@ .
-- * 'nluIntentConfidenceThreshold' - The score that determines where Amazon Lex inserts the @AMAZON.FallbackIntent@ , @AMAZON.KendraSearchIntent@ , or both when returning alternative intents in a <https://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostContent.html PostContent> or <https://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostText.html PostText> response. @AMAZON.FallbackIntent@ is inserted if the confidence score for all intents is below this value. @AMAZON.KendraSearchIntent@ is only inserted if it is configured for the bot.
-- * 'detectSentiment' - @true@ if the bot is configured to send user utterances to Amazon Comprehend for sentiment analysis. If the @detectSentiment@ field was not specified in the request, the @detectSentiment@ field is @false@ in the response.
-- * 'locale' - The target locale for the bot.
-- * 'createdDate' - The date that the bot was created.
-- * 'name' - The name of the bot.
-- * 'version' - The version of the bot. For a new bot, the version is always @> LATEST@ .
-- * 'idleSessionTTLInSeconds' - The maximum length of time that Amazon Lex retains the data gathered in a conversation. For more information, see 'PutBot' .
-- * 'clarificationPrompt' - The prompts that Amazon Lex uses when it doesn't understand the user's intent. For more information, see 'PutBot' .
-- * 'voiceId' - The Amazon Polly voice ID that Amazon Lex uses for voice interaction with the user. For more information, see 'PutBot' .
-- * 'lastUpdatedDate' - The date that the bot was updated. When you create a resource, the creation date and last updated date are the same.
-- * 'createVersion' - @True@ if a new version of the bot was created. If the @createVersion@ field was not specified in the request, the @createVersion@ field is set to false in the response.
-- * 'childDirected' - For each Amazon Lex bot created with the Amazon Lex Model Building Service, you must specify whether your use of Amazon Lex is related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to the Children's Online Privacy Protection Act (COPPA) by specifying @true@ or @false@ in the @childDirected@ field. By specifying @true@ in the @childDirected@ field, you confirm that your use of Amazon Lex __is__ related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA. By specifying @false@ in the @childDirected@ field, you confirm that your use of Amazon Lex __is not__ related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA. You may not specify a default value for the @childDirected@ field that does not accurately reflect whether your use of Amazon Lex is related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA.
--
-- If your use of Amazon Lex relates to a website, program, or other application that is directed in whole or in part, to children under age 13, you must obtain any required verifiable parental consent under COPPA. For information regarding the use of Amazon Lex in connection with websites, programs, or other applications that are directed or targeted, in whole or in part, to children under age 13, see the <https://aws.amazon.com/lex/faqs#data-security Amazon Lex FAQ.>
-- * 'description' - A description of the bot.
-- * 'tags' - A list of tags associated with the bot.
-- * 'responseStatus' - The response status code.
mkPutBotResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutBotResponse
mkPutBotResponse pResponseStatus_ =
  PutBotResponse'
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
      createVersion = Lude.Nothing,
      childDirected = Lude.Nothing,
      description = Lude.Nothing,
      tags = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If @status@ is @FAILED@ , Amazon Lex provides the reason that it failed to build the bot.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbrsFailureReason :: Lens.Lens' PutBotResponse (Lude.Maybe Lude.Text)
pbrsFailureReason = Lens.lens (failureReason :: PutBotResponse -> Lude.Maybe Lude.Text) (\s a -> s {failureReason = a} :: PutBotResponse)
{-# DEPRECATED pbrsFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | When you send a request to create a bot with @processBehavior@ set to @BUILD@ , Amazon Lex sets the @status@ response element to @BUILDING@ .
--
-- In the @READY_BASIC_TESTING@ state you can test the bot with user inputs that exactly match the utterances configured for the bot's intents and values in the slot types.
-- If Amazon Lex can't build the bot, Amazon Lex sets @status@ to @FAILED@ . Amazon Lex returns the reason for the failure in the @failureReason@ response element.
-- When you set @processBehavior@ to @SAVE@ , Amazon Lex sets the status code to @NOT BUILT@ .
-- When the bot is in the @READY@ state you can test and publish the bot.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbrsStatus :: Lens.Lens' PutBotResponse (Lude.Maybe LexStatus)
pbrsStatus = Lens.lens (status :: PutBotResponse -> Lude.Maybe LexStatus) (\s a -> s {status = a} :: PutBotResponse)
{-# DEPRECATED pbrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The message that Amazon Lex uses to cancel a conversation. For more information, see 'PutBot' .
--
-- /Note:/ Consider using 'abortStatement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbrsAbortStatement :: Lens.Lens' PutBotResponse (Lude.Maybe Statement)
pbrsAbortStatement = Lens.lens (abortStatement :: PutBotResponse -> Lude.Maybe Statement) (\s a -> s {abortStatement = a} :: PutBotResponse)
{-# DEPRECATED pbrsAbortStatement "Use generic-lens or generic-optics with 'abortStatement' instead." #-}

-- | An array of @Intent@ objects. For more information, see 'PutBot' .
--
-- /Note:/ Consider using 'intents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbrsIntents :: Lens.Lens' PutBotResponse (Lude.Maybe [Intent])
pbrsIntents = Lens.lens (intents :: PutBotResponse -> Lude.Maybe [Intent]) (\s a -> s {intents = a} :: PutBotResponse)
{-# DEPRECATED pbrsIntents "Use generic-lens or generic-optics with 'intents' instead." #-}

-- | Checksum of the bot that you created.
--
-- /Note:/ Consider using 'checksum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbrsChecksum :: Lens.Lens' PutBotResponse (Lude.Maybe Lude.Text)
pbrsChecksum = Lens.lens (checksum :: PutBotResponse -> Lude.Maybe Lude.Text) (\s a -> s {checksum = a} :: PutBotResponse)
{-# DEPRECATED pbrsChecksum "Use generic-lens or generic-optics with 'checksum' instead." #-}

-- | Indicates whether the bot uses accuracy improvements. @true@ indicates that the bot is using the improvements, otherwise, @false@ .
--
-- /Note:/ Consider using 'enableModelImprovements' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbrsEnableModelImprovements :: Lens.Lens' PutBotResponse (Lude.Maybe Lude.Bool)
pbrsEnableModelImprovements = Lens.lens (enableModelImprovements :: PutBotResponse -> Lude.Maybe Lude.Bool) (\s a -> s {enableModelImprovements = a} :: PutBotResponse)
{-# DEPRECATED pbrsEnableModelImprovements "Use generic-lens or generic-optics with 'enableModelImprovements' instead." #-}

-- | The score that determines where Amazon Lex inserts the @AMAZON.FallbackIntent@ , @AMAZON.KendraSearchIntent@ , or both when returning alternative intents in a <https://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostContent.html PostContent> or <https://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostText.html PostText> response. @AMAZON.FallbackIntent@ is inserted if the confidence score for all intents is below this value. @AMAZON.KendraSearchIntent@ is only inserted if it is configured for the bot.
--
-- /Note:/ Consider using 'nluIntentConfidenceThreshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbrsNluIntentConfidenceThreshold :: Lens.Lens' PutBotResponse (Lude.Maybe Lude.Double)
pbrsNluIntentConfidenceThreshold = Lens.lens (nluIntentConfidenceThreshold :: PutBotResponse -> Lude.Maybe Lude.Double) (\s a -> s {nluIntentConfidenceThreshold = a} :: PutBotResponse)
{-# DEPRECATED pbrsNluIntentConfidenceThreshold "Use generic-lens or generic-optics with 'nluIntentConfidenceThreshold' instead." #-}

-- | @true@ if the bot is configured to send user utterances to Amazon Comprehend for sentiment analysis. If the @detectSentiment@ field was not specified in the request, the @detectSentiment@ field is @false@ in the response.
--
-- /Note:/ Consider using 'detectSentiment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbrsDetectSentiment :: Lens.Lens' PutBotResponse (Lude.Maybe Lude.Bool)
pbrsDetectSentiment = Lens.lens (detectSentiment :: PutBotResponse -> Lude.Maybe Lude.Bool) (\s a -> s {detectSentiment = a} :: PutBotResponse)
{-# DEPRECATED pbrsDetectSentiment "Use generic-lens or generic-optics with 'detectSentiment' instead." #-}

-- | The target locale for the bot.
--
-- /Note:/ Consider using 'locale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbrsLocale :: Lens.Lens' PutBotResponse (Lude.Maybe Locale)
pbrsLocale = Lens.lens (locale :: PutBotResponse -> Lude.Maybe Locale) (\s a -> s {locale = a} :: PutBotResponse)
{-# DEPRECATED pbrsLocale "Use generic-lens or generic-optics with 'locale' instead." #-}

-- | The date that the bot was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbrsCreatedDate :: Lens.Lens' PutBotResponse (Lude.Maybe Lude.Timestamp)
pbrsCreatedDate = Lens.lens (createdDate :: PutBotResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdDate = a} :: PutBotResponse)
{-# DEPRECATED pbrsCreatedDate "Use generic-lens or generic-optics with 'createdDate' instead." #-}

-- | The name of the bot.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbrsName :: Lens.Lens' PutBotResponse (Lude.Maybe Lude.Text)
pbrsName = Lens.lens (name :: PutBotResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: PutBotResponse)
{-# DEPRECATED pbrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The version of the bot. For a new bot, the version is always @> LATEST@ .
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbrsVersion :: Lens.Lens' PutBotResponse (Lude.Maybe Lude.Text)
pbrsVersion = Lens.lens (version :: PutBotResponse -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: PutBotResponse)
{-# DEPRECATED pbrsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The maximum length of time that Amazon Lex retains the data gathered in a conversation. For more information, see 'PutBot' .
--
-- /Note:/ Consider using 'idleSessionTTLInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbrsIdleSessionTTLInSeconds :: Lens.Lens' PutBotResponse (Lude.Maybe Lude.Natural)
pbrsIdleSessionTTLInSeconds = Lens.lens (idleSessionTTLInSeconds :: PutBotResponse -> Lude.Maybe Lude.Natural) (\s a -> s {idleSessionTTLInSeconds = a} :: PutBotResponse)
{-# DEPRECATED pbrsIdleSessionTTLInSeconds "Use generic-lens or generic-optics with 'idleSessionTTLInSeconds' instead." #-}

-- | The prompts that Amazon Lex uses when it doesn't understand the user's intent. For more information, see 'PutBot' .
--
-- /Note:/ Consider using 'clarificationPrompt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbrsClarificationPrompt :: Lens.Lens' PutBotResponse (Lude.Maybe Prompt)
pbrsClarificationPrompt = Lens.lens (clarificationPrompt :: PutBotResponse -> Lude.Maybe Prompt) (\s a -> s {clarificationPrompt = a} :: PutBotResponse)
{-# DEPRECATED pbrsClarificationPrompt "Use generic-lens or generic-optics with 'clarificationPrompt' instead." #-}

-- | The Amazon Polly voice ID that Amazon Lex uses for voice interaction with the user. For more information, see 'PutBot' .
--
-- /Note:/ Consider using 'voiceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbrsVoiceId :: Lens.Lens' PutBotResponse (Lude.Maybe Lude.Text)
pbrsVoiceId = Lens.lens (voiceId :: PutBotResponse -> Lude.Maybe Lude.Text) (\s a -> s {voiceId = a} :: PutBotResponse)
{-# DEPRECATED pbrsVoiceId "Use generic-lens or generic-optics with 'voiceId' instead." #-}

-- | The date that the bot was updated. When you create a resource, the creation date and last updated date are the same.
--
-- /Note:/ Consider using 'lastUpdatedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbrsLastUpdatedDate :: Lens.Lens' PutBotResponse (Lude.Maybe Lude.Timestamp)
pbrsLastUpdatedDate = Lens.lens (lastUpdatedDate :: PutBotResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdatedDate = a} :: PutBotResponse)
{-# DEPRECATED pbrsLastUpdatedDate "Use generic-lens or generic-optics with 'lastUpdatedDate' instead." #-}

-- | @True@ if a new version of the bot was created. If the @createVersion@ field was not specified in the request, the @createVersion@ field is set to false in the response.
--
-- /Note:/ Consider using 'createVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbrsCreateVersion :: Lens.Lens' PutBotResponse (Lude.Maybe Lude.Bool)
pbrsCreateVersion = Lens.lens (createVersion :: PutBotResponse -> Lude.Maybe Lude.Bool) (\s a -> s {createVersion = a} :: PutBotResponse)
{-# DEPRECATED pbrsCreateVersion "Use generic-lens or generic-optics with 'createVersion' instead." #-}

-- | For each Amazon Lex bot created with the Amazon Lex Model Building Service, you must specify whether your use of Amazon Lex is related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to the Children's Online Privacy Protection Act (COPPA) by specifying @true@ or @false@ in the @childDirected@ field. By specifying @true@ in the @childDirected@ field, you confirm that your use of Amazon Lex __is__ related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA. By specifying @false@ in the @childDirected@ field, you confirm that your use of Amazon Lex __is not__ related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA. You may not specify a default value for the @childDirected@ field that does not accurately reflect whether your use of Amazon Lex is related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA.
--
-- If your use of Amazon Lex relates to a website, program, or other application that is directed in whole or in part, to children under age 13, you must obtain any required verifiable parental consent under COPPA. For information regarding the use of Amazon Lex in connection with websites, programs, or other applications that are directed or targeted, in whole or in part, to children under age 13, see the <https://aws.amazon.com/lex/faqs#data-security Amazon Lex FAQ.>
--
-- /Note:/ Consider using 'childDirected' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbrsChildDirected :: Lens.Lens' PutBotResponse (Lude.Maybe Lude.Bool)
pbrsChildDirected = Lens.lens (childDirected :: PutBotResponse -> Lude.Maybe Lude.Bool) (\s a -> s {childDirected = a} :: PutBotResponse)
{-# DEPRECATED pbrsChildDirected "Use generic-lens or generic-optics with 'childDirected' instead." #-}

-- | A description of the bot.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbrsDescription :: Lens.Lens' PutBotResponse (Lude.Maybe Lude.Text)
pbrsDescription = Lens.lens (description :: PutBotResponse -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: PutBotResponse)
{-# DEPRECATED pbrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A list of tags associated with the bot.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbrsTags :: Lens.Lens' PutBotResponse (Lude.Maybe [Tag])
pbrsTags = Lens.lens (tags :: PutBotResponse -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: PutBotResponse)
{-# DEPRECATED pbrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbrsResponseStatus :: Lens.Lens' PutBotResponse Lude.Int
pbrsResponseStatus = Lens.lens (responseStatus :: PutBotResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutBotResponse)
{-# DEPRECATED pbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
