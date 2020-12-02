{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
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
--
-- If you specify the name of an existing bot, the fields in the request replace the existing values in the @> LATEST@ version of the bot. Amazon Lex removes any fields that you don't provide values for in the request, except for the @idleTTLInSeconds@ and @privacySettings@ fields, which are set to their default values. If you don't specify values for required fields, Amazon Lex throws an exception.
--
-- This operation requires permissions for the @lex:PutBot@ action. For more information, see 'security-iam' .
module Network.AWS.LexModels.PutBot
  ( -- * Creating a Request
    putBot,
    PutBot,

    -- * Request Lenses
    pbAbortStatement,
    pbIntents,
    pbChecksum,
    pbEnableModelImprovements,
    pbNluIntentConfidenceThreshold,
    pbDetectSentiment,
    pbProcessBehavior,
    pbIdleSessionTTLInSeconds,
    pbClarificationPrompt,
    pbVoiceId,
    pbCreateVersion,
    pbDescription,
    pbTags,
    pbName,
    pbLocale,
    pbChildDirected,

    -- * Destructuring the Response
    putBotResponse,
    PutBotResponse,

    -- * Response Lenses
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

import Network.AWS.Lens
import Network.AWS.LexModels.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putBot' smart constructor.
data PutBot = PutBot'
  { _pbAbortStatement :: !(Maybe Statement),
    _pbIntents :: !(Maybe [Intent]),
    _pbChecksum :: !(Maybe Text),
    _pbEnableModelImprovements :: !(Maybe Bool),
    _pbNluIntentConfidenceThreshold :: !(Maybe Double),
    _pbDetectSentiment :: !(Maybe Bool),
    _pbProcessBehavior :: !(Maybe ProcessBehavior),
    _pbIdleSessionTTLInSeconds :: !(Maybe Nat),
    _pbClarificationPrompt :: !(Maybe Prompt),
    _pbVoiceId :: !(Maybe Text),
    _pbCreateVersion :: !(Maybe Bool),
    _pbDescription :: !(Maybe Text),
    _pbTags :: !(Maybe [Tag]),
    _pbName :: !Text,
    _pbLocale :: !Locale,
    _pbChildDirected :: !Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutBot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pbAbortStatement' - When Amazon Lex can't understand the user's input in context, it tries to elicit the information a few times. After that, Amazon Lex sends the message defined in @abortStatement@ to the user, and then cancels the conversation. To set the number of retries, use the @valueElicitationPrompt@ field for the slot type.  For example, in a pizza ordering bot, Amazon Lex might ask a user "What type of crust would you like?" If the user's response is not one of the expected responses (for example, "thin crust, "deep dish," etc.), Amazon Lex tries to elicit a correct response a few more times.  For example, in a pizza ordering application, @OrderPizza@ might be one of the intents. This intent might require the @CrustType@ slot. You specify the @valueElicitationPrompt@ field when you create the @CrustType@ slot. If you have defined a fallback intent the cancel statement will not be sent to the user, the fallback intent is used instead. For more information, see <https://docs.aws.amazon.com/lex/latest/dg/built-in-intent-fallback.html AMAZON.FallbackIntent> .
--
-- * 'pbIntents' - An array of @Intent@ objects. Each intent represents a command that a user can express. For example, a pizza ordering bot might support an OrderPizza intent. For more information, see 'how-it-works' .
--
-- * 'pbChecksum' - Identifies a specific revision of the @> LATEST@ version. When you create a new bot, leave the @checksum@ field blank. If you specify a checksum you get a @BadRequestException@ exception. When you want to update a bot, set the @checksum@ field to the checksum of the most recent revision of the @> LATEST@ version. If you don't specify the @checksum@ field, or if the checksum does not match the @> LATEST@ version, you get a @PreconditionFailedException@ exception.
--
-- * 'pbEnableModelImprovements' - Set to @true@ to enable access to natural language understanding improvements.  When you set the @enableModelImprovements@ parameter to @true@ you can use the @nluIntentConfidenceThreshold@ parameter to configure confidence scores. For more information, see <https://docs.aws.amazon.com/lex/latest/dg/confidence-scores.html Confidence Scores> . You can only set the @enableModelImprovements@ parameter in certain Regions. If you set the parameter to @true@ , your bot has access to accuracy improvements. The Regions where you can set the @enableModelImprovements@ parameter to @true@ are:     * US East (N. Virginia) (us-east-1)     * US West (Oregon) (us-west-2)     * Asia Pacific (Sydney) (ap-southeast-2)     * EU (Ireland) (eu-west-1) In other Regions, the @enableModelImprovements@ parameter is set to @true@ by default. In these Regions setting the parameter to @false@ throws a @ValidationException@ exception.
--
-- * 'pbNluIntentConfidenceThreshold' - Determines the threshold where Amazon Lex will insert the @AMAZON.FallbackIntent@ , @AMAZON.KendraSearchIntent@ , or both when returning alternative intents in a <https://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostContent.html PostContent> or <https://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostText.html PostText> response. @AMAZON.FallbackIntent@ and @AMAZON.KendraSearchIntent@ are only inserted if they are configured for the bot. You must set the @enableModelImprovements@ parameter to @true@ to use confidence scores in the following regions.     * US East (N. Virginia) (us-east-1)     * US West (Oregon) (us-west-2)     * Asia Pacific (Sydney) (ap-southeast-2)     * EU (Ireland) (eu-west-1) In other Regions, the @enableModelImprovements@ parameter is set to @true@ by default. For example, suppose a bot is configured with the confidence threshold of 0.80 and the @AMAZON.FallbackIntent@ . Amazon Lex returns three alternative intents with the following confidence scores: IntentA (0.70), IntentB (0.60), IntentC (0.50). The response from the @PostText@ operation would be:     * AMAZON.FallbackIntent     * IntentA     * IntentB     * IntentC
--
-- * 'pbDetectSentiment' - When set to @true@ user utterances are sent to Amazon Comprehend for sentiment analysis. If you don't specify @detectSentiment@ , the default is @false@ .
--
-- * 'pbProcessBehavior' - If you set the @processBehavior@ element to @BUILD@ , Amazon Lex builds the bot so that it can be run. If you set the element to @SAVE@ Amazon Lex saves the bot, but doesn't build it.  If you don't specify this value, the default value is @BUILD@ .
--
-- * 'pbIdleSessionTTLInSeconds' - The maximum time in seconds that Amazon Lex retains the data gathered in a conversation. A user interaction session remains active for the amount of time specified. If no conversation occurs during this time, the session expires and Amazon Lex deletes any data provided before the timeout. For example, suppose that a user chooses the OrderPizza intent, but gets sidetracked halfway through placing an order. If the user doesn't complete the order within the specified time, Amazon Lex discards the slot information that it gathered, and the user must start over. If you don't include the @idleSessionTTLInSeconds@ element in a @PutBot@ operation request, Amazon Lex uses the default value. This is also true if the request replaces an existing bot. The default is 300 seconds (5 minutes).
--
-- * 'pbClarificationPrompt' - When Amazon Lex doesn't understand the user's intent, it uses this message to get clarification. To specify how many times Amazon Lex should repeat the clarification prompt, use the @maxAttempts@ field. If Amazon Lex still doesn't understand, it sends the message in the @abortStatement@ field.  When you create a clarification prompt, make sure that it suggests the correct response from the user. for example, for a bot that orders pizza and drinks, you might create this clarification prompt: "What would you like to do? You can say 'Order a pizza' or 'Order a drink.'" If you have defined a fallback intent, it will be invoked if the clarification prompt is repeated the number of times defined in the @maxAttempts@ field. For more information, see <https://docs.aws.amazon.com/lex/latest/dg/built-in-intent-fallback.html AMAZON.FallbackIntent> . If you don't define a clarification prompt, at runtime Amazon Lex will return a 400 Bad Request exception in three cases:      * Follow-up prompt - When the user responds to a follow-up prompt but does not provide an intent. For example, in response to a follow-up prompt that says "Would you like anything else today?" the user says "Yes." Amazon Lex will return a 400 Bad Request exception because it does not have a clarification prompt to send to the user to get an intent.     * Lambda function - When using a Lambda function, you return an @ElicitIntent@ dialog type. Since Amazon Lex does not have a clarification prompt to get an intent from the user, it returns a 400 Bad Request exception.     * PutSession operation - When using the @PutSession@ operation, you send an @ElicitIntent@ dialog type. Since Amazon Lex does not have a clarification prompt to get an intent from the user, it returns a 400 Bad Request exception.
--
-- * 'pbVoiceId' - The Amazon Polly voice ID that you want Amazon Lex to use for voice interactions with the user. The locale configured for the voice must match the locale of the bot. For more information, see <https://docs.aws.amazon.com/polly/latest/dg/voicelist.html Voices in Amazon Polly> in the /Amazon Polly Developer Guide/ .
--
-- * 'pbCreateVersion' - When set to @true@ a new numbered version of the bot is created. This is the same as calling the @CreateBotVersion@ operation. If you don't specify @createVersion@ , the default is @false@ .
--
-- * 'pbDescription' - A description of the bot.
--
-- * 'pbTags' - A list of tags to add to the bot. You can only add tags when you create a bot, you can't use the @PutBot@ operation to update the tags on a bot. To update tags, use the @TagResource@ operation.
--
-- * 'pbName' - The name of the bot. The name is /not/ case sensitive.
--
-- * 'pbLocale' - Specifies the target locale for the bot. Any intent used in the bot must be compatible with the locale of the bot.  The default is @en-US@ .
--
-- * 'pbChildDirected' - For each Amazon Lex bot created with the Amazon Lex Model Building Service, you must specify whether your use of Amazon Lex is related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to the Children's Online Privacy Protection Act (COPPA) by specifying @true@ or @false@ in the @childDirected@ field. By specifying @true@ in the @childDirected@ field, you confirm that your use of Amazon Lex __is__ related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA. By specifying @false@ in the @childDirected@ field, you confirm that your use of Amazon Lex __is not__ related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA. You may not specify a default value for the @childDirected@ field that does not accurately reflect whether your use of Amazon Lex is related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA. If your use of Amazon Lex relates to a website, program, or other application that is directed in whole or in part, to children under age 13, you must obtain any required verifiable parental consent under COPPA. For information regarding the use of Amazon Lex in connection with websites, programs, or other applications that are directed or targeted, in whole or in part, to children under age 13, see the <https://aws.amazon.com/lex/faqs#data-security Amazon Lex FAQ.>
putBot ::
  -- | 'pbName'
  Text ->
  -- | 'pbLocale'
  Locale ->
  -- | 'pbChildDirected'
  Bool ->
  PutBot
putBot pName_ pLocale_ pChildDirected_ =
  PutBot'
    { _pbAbortStatement = Nothing,
      _pbIntents = Nothing,
      _pbChecksum = Nothing,
      _pbEnableModelImprovements = Nothing,
      _pbNluIntentConfidenceThreshold = Nothing,
      _pbDetectSentiment = Nothing,
      _pbProcessBehavior = Nothing,
      _pbIdleSessionTTLInSeconds = Nothing,
      _pbClarificationPrompt = Nothing,
      _pbVoiceId = Nothing,
      _pbCreateVersion = Nothing,
      _pbDescription = Nothing,
      _pbTags = Nothing,
      _pbName = pName_,
      _pbLocale = pLocale_,
      _pbChildDirected = pChildDirected_
    }

-- | When Amazon Lex can't understand the user's input in context, it tries to elicit the information a few times. After that, Amazon Lex sends the message defined in @abortStatement@ to the user, and then cancels the conversation. To set the number of retries, use the @valueElicitationPrompt@ field for the slot type.  For example, in a pizza ordering bot, Amazon Lex might ask a user "What type of crust would you like?" If the user's response is not one of the expected responses (for example, "thin crust, "deep dish," etc.), Amazon Lex tries to elicit a correct response a few more times.  For example, in a pizza ordering application, @OrderPizza@ might be one of the intents. This intent might require the @CrustType@ slot. You specify the @valueElicitationPrompt@ field when you create the @CrustType@ slot. If you have defined a fallback intent the cancel statement will not be sent to the user, the fallback intent is used instead. For more information, see <https://docs.aws.amazon.com/lex/latest/dg/built-in-intent-fallback.html AMAZON.FallbackIntent> .
pbAbortStatement :: Lens' PutBot (Maybe Statement)
pbAbortStatement = lens _pbAbortStatement (\s a -> s {_pbAbortStatement = a})

-- | An array of @Intent@ objects. Each intent represents a command that a user can express. For example, a pizza ordering bot might support an OrderPizza intent. For more information, see 'how-it-works' .
pbIntents :: Lens' PutBot [Intent]
pbIntents = lens _pbIntents (\s a -> s {_pbIntents = a}) . _Default . _Coerce

-- | Identifies a specific revision of the @> LATEST@ version. When you create a new bot, leave the @checksum@ field blank. If you specify a checksum you get a @BadRequestException@ exception. When you want to update a bot, set the @checksum@ field to the checksum of the most recent revision of the @> LATEST@ version. If you don't specify the @checksum@ field, or if the checksum does not match the @> LATEST@ version, you get a @PreconditionFailedException@ exception.
pbChecksum :: Lens' PutBot (Maybe Text)
pbChecksum = lens _pbChecksum (\s a -> s {_pbChecksum = a})

-- | Set to @true@ to enable access to natural language understanding improvements.  When you set the @enableModelImprovements@ parameter to @true@ you can use the @nluIntentConfidenceThreshold@ parameter to configure confidence scores. For more information, see <https://docs.aws.amazon.com/lex/latest/dg/confidence-scores.html Confidence Scores> . You can only set the @enableModelImprovements@ parameter in certain Regions. If you set the parameter to @true@ , your bot has access to accuracy improvements. The Regions where you can set the @enableModelImprovements@ parameter to @true@ are:     * US East (N. Virginia) (us-east-1)     * US West (Oregon) (us-west-2)     * Asia Pacific (Sydney) (ap-southeast-2)     * EU (Ireland) (eu-west-1) In other Regions, the @enableModelImprovements@ parameter is set to @true@ by default. In these Regions setting the parameter to @false@ throws a @ValidationException@ exception.
pbEnableModelImprovements :: Lens' PutBot (Maybe Bool)
pbEnableModelImprovements = lens _pbEnableModelImprovements (\s a -> s {_pbEnableModelImprovements = a})

-- | Determines the threshold where Amazon Lex will insert the @AMAZON.FallbackIntent@ , @AMAZON.KendraSearchIntent@ , or both when returning alternative intents in a <https://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostContent.html PostContent> or <https://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostText.html PostText> response. @AMAZON.FallbackIntent@ and @AMAZON.KendraSearchIntent@ are only inserted if they are configured for the bot. You must set the @enableModelImprovements@ parameter to @true@ to use confidence scores in the following regions.     * US East (N. Virginia) (us-east-1)     * US West (Oregon) (us-west-2)     * Asia Pacific (Sydney) (ap-southeast-2)     * EU (Ireland) (eu-west-1) In other Regions, the @enableModelImprovements@ parameter is set to @true@ by default. For example, suppose a bot is configured with the confidence threshold of 0.80 and the @AMAZON.FallbackIntent@ . Amazon Lex returns three alternative intents with the following confidence scores: IntentA (0.70), IntentB (0.60), IntentC (0.50). The response from the @PostText@ operation would be:     * AMAZON.FallbackIntent     * IntentA     * IntentB     * IntentC
pbNluIntentConfidenceThreshold :: Lens' PutBot (Maybe Double)
pbNluIntentConfidenceThreshold = lens _pbNluIntentConfidenceThreshold (\s a -> s {_pbNluIntentConfidenceThreshold = a})

-- | When set to @true@ user utterances are sent to Amazon Comprehend for sentiment analysis. If you don't specify @detectSentiment@ , the default is @false@ .
pbDetectSentiment :: Lens' PutBot (Maybe Bool)
pbDetectSentiment = lens _pbDetectSentiment (\s a -> s {_pbDetectSentiment = a})

-- | If you set the @processBehavior@ element to @BUILD@ , Amazon Lex builds the bot so that it can be run. If you set the element to @SAVE@ Amazon Lex saves the bot, but doesn't build it.  If you don't specify this value, the default value is @BUILD@ .
pbProcessBehavior :: Lens' PutBot (Maybe ProcessBehavior)
pbProcessBehavior = lens _pbProcessBehavior (\s a -> s {_pbProcessBehavior = a})

-- | The maximum time in seconds that Amazon Lex retains the data gathered in a conversation. A user interaction session remains active for the amount of time specified. If no conversation occurs during this time, the session expires and Amazon Lex deletes any data provided before the timeout. For example, suppose that a user chooses the OrderPizza intent, but gets sidetracked halfway through placing an order. If the user doesn't complete the order within the specified time, Amazon Lex discards the slot information that it gathered, and the user must start over. If you don't include the @idleSessionTTLInSeconds@ element in a @PutBot@ operation request, Amazon Lex uses the default value. This is also true if the request replaces an existing bot. The default is 300 seconds (5 minutes).
pbIdleSessionTTLInSeconds :: Lens' PutBot (Maybe Natural)
pbIdleSessionTTLInSeconds = lens _pbIdleSessionTTLInSeconds (\s a -> s {_pbIdleSessionTTLInSeconds = a}) . mapping _Nat

-- | When Amazon Lex doesn't understand the user's intent, it uses this message to get clarification. To specify how many times Amazon Lex should repeat the clarification prompt, use the @maxAttempts@ field. If Amazon Lex still doesn't understand, it sends the message in the @abortStatement@ field.  When you create a clarification prompt, make sure that it suggests the correct response from the user. for example, for a bot that orders pizza and drinks, you might create this clarification prompt: "What would you like to do? You can say 'Order a pizza' or 'Order a drink.'" If you have defined a fallback intent, it will be invoked if the clarification prompt is repeated the number of times defined in the @maxAttempts@ field. For more information, see <https://docs.aws.amazon.com/lex/latest/dg/built-in-intent-fallback.html AMAZON.FallbackIntent> . If you don't define a clarification prompt, at runtime Amazon Lex will return a 400 Bad Request exception in three cases:      * Follow-up prompt - When the user responds to a follow-up prompt but does not provide an intent. For example, in response to a follow-up prompt that says "Would you like anything else today?" the user says "Yes." Amazon Lex will return a 400 Bad Request exception because it does not have a clarification prompt to send to the user to get an intent.     * Lambda function - When using a Lambda function, you return an @ElicitIntent@ dialog type. Since Amazon Lex does not have a clarification prompt to get an intent from the user, it returns a 400 Bad Request exception.     * PutSession operation - When using the @PutSession@ operation, you send an @ElicitIntent@ dialog type. Since Amazon Lex does not have a clarification prompt to get an intent from the user, it returns a 400 Bad Request exception.
pbClarificationPrompt :: Lens' PutBot (Maybe Prompt)
pbClarificationPrompt = lens _pbClarificationPrompt (\s a -> s {_pbClarificationPrompt = a})

-- | The Amazon Polly voice ID that you want Amazon Lex to use for voice interactions with the user. The locale configured for the voice must match the locale of the bot. For more information, see <https://docs.aws.amazon.com/polly/latest/dg/voicelist.html Voices in Amazon Polly> in the /Amazon Polly Developer Guide/ .
pbVoiceId :: Lens' PutBot (Maybe Text)
pbVoiceId = lens _pbVoiceId (\s a -> s {_pbVoiceId = a})

-- | When set to @true@ a new numbered version of the bot is created. This is the same as calling the @CreateBotVersion@ operation. If you don't specify @createVersion@ , the default is @false@ .
pbCreateVersion :: Lens' PutBot (Maybe Bool)
pbCreateVersion = lens _pbCreateVersion (\s a -> s {_pbCreateVersion = a})

-- | A description of the bot.
pbDescription :: Lens' PutBot (Maybe Text)
pbDescription = lens _pbDescription (\s a -> s {_pbDescription = a})

-- | A list of tags to add to the bot. You can only add tags when you create a bot, you can't use the @PutBot@ operation to update the tags on a bot. To update tags, use the @TagResource@ operation.
pbTags :: Lens' PutBot [Tag]
pbTags = lens _pbTags (\s a -> s {_pbTags = a}) . _Default . _Coerce

-- | The name of the bot. The name is /not/ case sensitive.
pbName :: Lens' PutBot Text
pbName = lens _pbName (\s a -> s {_pbName = a})

-- | Specifies the target locale for the bot. Any intent used in the bot must be compatible with the locale of the bot.  The default is @en-US@ .
pbLocale :: Lens' PutBot Locale
pbLocale = lens _pbLocale (\s a -> s {_pbLocale = a})

-- | For each Amazon Lex bot created with the Amazon Lex Model Building Service, you must specify whether your use of Amazon Lex is related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to the Children's Online Privacy Protection Act (COPPA) by specifying @true@ or @false@ in the @childDirected@ field. By specifying @true@ in the @childDirected@ field, you confirm that your use of Amazon Lex __is__ related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA. By specifying @false@ in the @childDirected@ field, you confirm that your use of Amazon Lex __is not__ related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA. You may not specify a default value for the @childDirected@ field that does not accurately reflect whether your use of Amazon Lex is related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA. If your use of Amazon Lex relates to a website, program, or other application that is directed in whole or in part, to children under age 13, you must obtain any required verifiable parental consent under COPPA. For information regarding the use of Amazon Lex in connection with websites, programs, or other applications that are directed or targeted, in whole or in part, to children under age 13, see the <https://aws.amazon.com/lex/faqs#data-security Amazon Lex FAQ.>
pbChildDirected :: Lens' PutBot Bool
pbChildDirected = lens _pbChildDirected (\s a -> s {_pbChildDirected = a})

instance AWSRequest PutBot where
  type Rs PutBot = PutBotResponse
  request = putJSON lexModels
  response =
    receiveJSON
      ( \s h x ->
          PutBotResponse'
            <$> (x .?> "failureReason")
            <*> (x .?> "status")
            <*> (x .?> "abortStatement")
            <*> (x .?> "intents" .!@ mempty)
            <*> (x .?> "checksum")
            <*> (x .?> "enableModelImprovements")
            <*> (x .?> "nluIntentConfidenceThreshold")
            <*> (x .?> "detectSentiment")
            <*> (x .?> "locale")
            <*> (x .?> "createdDate")
            <*> (x .?> "name")
            <*> (x .?> "version")
            <*> (x .?> "idleSessionTTLInSeconds")
            <*> (x .?> "clarificationPrompt")
            <*> (x .?> "voiceId")
            <*> (x .?> "lastUpdatedDate")
            <*> (x .?> "createVersion")
            <*> (x .?> "childDirected")
            <*> (x .?> "description")
            <*> (x .?> "tags" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable PutBot

instance NFData PutBot

instance ToHeaders PutBot where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON PutBot where
  toJSON PutBot' {..} =
    object
      ( catMaybes
          [ ("abortStatement" .=) <$> _pbAbortStatement,
            ("intents" .=) <$> _pbIntents,
            ("checksum" .=) <$> _pbChecksum,
            ("enableModelImprovements" .=) <$> _pbEnableModelImprovements,
            ("nluIntentConfidenceThreshold" .=)
              <$> _pbNluIntentConfidenceThreshold,
            ("detectSentiment" .=) <$> _pbDetectSentiment,
            ("processBehavior" .=) <$> _pbProcessBehavior,
            ("idleSessionTTLInSeconds" .=) <$> _pbIdleSessionTTLInSeconds,
            ("clarificationPrompt" .=) <$> _pbClarificationPrompt,
            ("voiceId" .=) <$> _pbVoiceId,
            ("createVersion" .=) <$> _pbCreateVersion,
            ("description" .=) <$> _pbDescription,
            ("tags" .=) <$> _pbTags,
            Just ("locale" .= _pbLocale),
            Just ("childDirected" .= _pbChildDirected)
          ]
      )

instance ToPath PutBot where
  toPath PutBot' {..} =
    mconcat ["/bots/", toBS _pbName, "/versions/$LATEST"]

instance ToQuery PutBot where
  toQuery = const mempty

-- | /See:/ 'putBotResponse' smart constructor.
data PutBotResponse = PutBotResponse'
  { _pbrsFailureReason ::
      !(Maybe Text),
    _pbrsStatus :: !(Maybe LexStatus),
    _pbrsAbortStatement :: !(Maybe Statement),
    _pbrsIntents :: !(Maybe [Intent]),
    _pbrsChecksum :: !(Maybe Text),
    _pbrsEnableModelImprovements :: !(Maybe Bool),
    _pbrsNluIntentConfidenceThreshold :: !(Maybe Double),
    _pbrsDetectSentiment :: !(Maybe Bool),
    _pbrsLocale :: !(Maybe Locale),
    _pbrsCreatedDate :: !(Maybe POSIX),
    _pbrsName :: !(Maybe Text),
    _pbrsVersion :: !(Maybe Text),
    _pbrsIdleSessionTTLInSeconds :: !(Maybe Nat),
    _pbrsClarificationPrompt :: !(Maybe Prompt),
    _pbrsVoiceId :: !(Maybe Text),
    _pbrsLastUpdatedDate :: !(Maybe POSIX),
    _pbrsCreateVersion :: !(Maybe Bool),
    _pbrsChildDirected :: !(Maybe Bool),
    _pbrsDescription :: !(Maybe Text),
    _pbrsTags :: !(Maybe [Tag]),
    _pbrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutBotResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pbrsFailureReason' - If @status@ is @FAILED@ , Amazon Lex provides the reason that it failed to build the bot.
--
-- * 'pbrsStatus' - When you send a request to create a bot with @processBehavior@ set to @BUILD@ , Amazon Lex sets the @status@ response element to @BUILDING@ . In the @READY_BASIC_TESTING@ state you can test the bot with user inputs that exactly match the utterances configured for the bot's intents and values in the slot types. If Amazon Lex can't build the bot, Amazon Lex sets @status@ to @FAILED@ . Amazon Lex returns the reason for the failure in the @failureReason@ response element.  When you set @processBehavior@ to @SAVE@ , Amazon Lex sets the status code to @NOT BUILT@ . When the bot is in the @READY@ state you can test and publish the bot.
--
-- * 'pbrsAbortStatement' - The message that Amazon Lex uses to cancel a conversation. For more information, see 'PutBot' .
--
-- * 'pbrsIntents' - An array of @Intent@ objects. For more information, see 'PutBot' .
--
-- * 'pbrsChecksum' - Checksum of the bot that you created.
--
-- * 'pbrsEnableModelImprovements' - Indicates whether the bot uses accuracy improvements. @true@ indicates that the bot is using the improvements, otherwise, @false@ .
--
-- * 'pbrsNluIntentConfidenceThreshold' - The score that determines where Amazon Lex inserts the @AMAZON.FallbackIntent@ , @AMAZON.KendraSearchIntent@ , or both when returning alternative intents in a <https://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostContent.html PostContent> or <https://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostText.html PostText> response. @AMAZON.FallbackIntent@ is inserted if the confidence score for all intents is below this value. @AMAZON.KendraSearchIntent@ is only inserted if it is configured for the bot.
--
-- * 'pbrsDetectSentiment' - @true@ if the bot is configured to send user utterances to Amazon Comprehend for sentiment analysis. If the @detectSentiment@ field was not specified in the request, the @detectSentiment@ field is @false@ in the response.
--
-- * 'pbrsLocale' - The target locale for the bot.
--
-- * 'pbrsCreatedDate' - The date that the bot was created.
--
-- * 'pbrsName' - The name of the bot.
--
-- * 'pbrsVersion' - The version of the bot. For a new bot, the version is always @> LATEST@ .
--
-- * 'pbrsIdleSessionTTLInSeconds' - The maximum length of time that Amazon Lex retains the data gathered in a conversation. For more information, see 'PutBot' .
--
-- * 'pbrsClarificationPrompt' - The prompts that Amazon Lex uses when it doesn't understand the user's intent. For more information, see 'PutBot' .
--
-- * 'pbrsVoiceId' - The Amazon Polly voice ID that Amazon Lex uses for voice interaction with the user. For more information, see 'PutBot' .
--
-- * 'pbrsLastUpdatedDate' - The date that the bot was updated. When you create a resource, the creation date and last updated date are the same.
--
-- * 'pbrsCreateVersion' - @True@ if a new version of the bot was created. If the @createVersion@ field was not specified in the request, the @createVersion@ field is set to false in the response.
--
-- * 'pbrsChildDirected' - For each Amazon Lex bot created with the Amazon Lex Model Building Service, you must specify whether your use of Amazon Lex is related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to the Children's Online Privacy Protection Act (COPPA) by specifying @true@ or @false@ in the @childDirected@ field. By specifying @true@ in the @childDirected@ field, you confirm that your use of Amazon Lex __is__ related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA. By specifying @false@ in the @childDirected@ field, you confirm that your use of Amazon Lex __is not__ related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA. You may not specify a default value for the @childDirected@ field that does not accurately reflect whether your use of Amazon Lex is related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA. If your use of Amazon Lex relates to a website, program, or other application that is directed in whole or in part, to children under age 13, you must obtain any required verifiable parental consent under COPPA. For information regarding the use of Amazon Lex in connection with websites, programs, or other applications that are directed or targeted, in whole or in part, to children under age 13, see the <https://aws.amazon.com/lex/faqs#data-security Amazon Lex FAQ.>
--
-- * 'pbrsDescription' - A description of the bot.
--
-- * 'pbrsTags' - A list of tags associated with the bot.
--
-- * 'pbrsResponseStatus' - -- | The response status code.
putBotResponse ::
  -- | 'pbrsResponseStatus'
  Int ->
  PutBotResponse
putBotResponse pResponseStatus_ =
  PutBotResponse'
    { _pbrsFailureReason = Nothing,
      _pbrsStatus = Nothing,
      _pbrsAbortStatement = Nothing,
      _pbrsIntents = Nothing,
      _pbrsChecksum = Nothing,
      _pbrsEnableModelImprovements = Nothing,
      _pbrsNluIntentConfidenceThreshold = Nothing,
      _pbrsDetectSentiment = Nothing,
      _pbrsLocale = Nothing,
      _pbrsCreatedDate = Nothing,
      _pbrsName = Nothing,
      _pbrsVersion = Nothing,
      _pbrsIdleSessionTTLInSeconds = Nothing,
      _pbrsClarificationPrompt = Nothing,
      _pbrsVoiceId = Nothing,
      _pbrsLastUpdatedDate = Nothing,
      _pbrsCreateVersion = Nothing,
      _pbrsChildDirected = Nothing,
      _pbrsDescription = Nothing,
      _pbrsTags = Nothing,
      _pbrsResponseStatus = pResponseStatus_
    }

-- | If @status@ is @FAILED@ , Amazon Lex provides the reason that it failed to build the bot.
pbrsFailureReason :: Lens' PutBotResponse (Maybe Text)
pbrsFailureReason = lens _pbrsFailureReason (\s a -> s {_pbrsFailureReason = a})

-- | When you send a request to create a bot with @processBehavior@ set to @BUILD@ , Amazon Lex sets the @status@ response element to @BUILDING@ . In the @READY_BASIC_TESTING@ state you can test the bot with user inputs that exactly match the utterances configured for the bot's intents and values in the slot types. If Amazon Lex can't build the bot, Amazon Lex sets @status@ to @FAILED@ . Amazon Lex returns the reason for the failure in the @failureReason@ response element.  When you set @processBehavior@ to @SAVE@ , Amazon Lex sets the status code to @NOT BUILT@ . When the bot is in the @READY@ state you can test and publish the bot.
pbrsStatus :: Lens' PutBotResponse (Maybe LexStatus)
pbrsStatus = lens _pbrsStatus (\s a -> s {_pbrsStatus = a})

-- | The message that Amazon Lex uses to cancel a conversation. For more information, see 'PutBot' .
pbrsAbortStatement :: Lens' PutBotResponse (Maybe Statement)
pbrsAbortStatement = lens _pbrsAbortStatement (\s a -> s {_pbrsAbortStatement = a})

-- | An array of @Intent@ objects. For more information, see 'PutBot' .
pbrsIntents :: Lens' PutBotResponse [Intent]
pbrsIntents = lens _pbrsIntents (\s a -> s {_pbrsIntents = a}) . _Default . _Coerce

-- | Checksum of the bot that you created.
pbrsChecksum :: Lens' PutBotResponse (Maybe Text)
pbrsChecksum = lens _pbrsChecksum (\s a -> s {_pbrsChecksum = a})

-- | Indicates whether the bot uses accuracy improvements. @true@ indicates that the bot is using the improvements, otherwise, @false@ .
pbrsEnableModelImprovements :: Lens' PutBotResponse (Maybe Bool)
pbrsEnableModelImprovements = lens _pbrsEnableModelImprovements (\s a -> s {_pbrsEnableModelImprovements = a})

-- | The score that determines where Amazon Lex inserts the @AMAZON.FallbackIntent@ , @AMAZON.KendraSearchIntent@ , or both when returning alternative intents in a <https://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostContent.html PostContent> or <https://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostText.html PostText> response. @AMAZON.FallbackIntent@ is inserted if the confidence score for all intents is below this value. @AMAZON.KendraSearchIntent@ is only inserted if it is configured for the bot.
pbrsNluIntentConfidenceThreshold :: Lens' PutBotResponse (Maybe Double)
pbrsNluIntentConfidenceThreshold = lens _pbrsNluIntentConfidenceThreshold (\s a -> s {_pbrsNluIntentConfidenceThreshold = a})

-- | @true@ if the bot is configured to send user utterances to Amazon Comprehend for sentiment analysis. If the @detectSentiment@ field was not specified in the request, the @detectSentiment@ field is @false@ in the response.
pbrsDetectSentiment :: Lens' PutBotResponse (Maybe Bool)
pbrsDetectSentiment = lens _pbrsDetectSentiment (\s a -> s {_pbrsDetectSentiment = a})

-- | The target locale for the bot.
pbrsLocale :: Lens' PutBotResponse (Maybe Locale)
pbrsLocale = lens _pbrsLocale (\s a -> s {_pbrsLocale = a})

-- | The date that the bot was created.
pbrsCreatedDate :: Lens' PutBotResponse (Maybe UTCTime)
pbrsCreatedDate = lens _pbrsCreatedDate (\s a -> s {_pbrsCreatedDate = a}) . mapping _Time

-- | The name of the bot.
pbrsName :: Lens' PutBotResponse (Maybe Text)
pbrsName = lens _pbrsName (\s a -> s {_pbrsName = a})

-- | The version of the bot. For a new bot, the version is always @> LATEST@ .
pbrsVersion :: Lens' PutBotResponse (Maybe Text)
pbrsVersion = lens _pbrsVersion (\s a -> s {_pbrsVersion = a})

-- | The maximum length of time that Amazon Lex retains the data gathered in a conversation. For more information, see 'PutBot' .
pbrsIdleSessionTTLInSeconds :: Lens' PutBotResponse (Maybe Natural)
pbrsIdleSessionTTLInSeconds = lens _pbrsIdleSessionTTLInSeconds (\s a -> s {_pbrsIdleSessionTTLInSeconds = a}) . mapping _Nat

-- | The prompts that Amazon Lex uses when it doesn't understand the user's intent. For more information, see 'PutBot' .
pbrsClarificationPrompt :: Lens' PutBotResponse (Maybe Prompt)
pbrsClarificationPrompt = lens _pbrsClarificationPrompt (\s a -> s {_pbrsClarificationPrompt = a})

-- | The Amazon Polly voice ID that Amazon Lex uses for voice interaction with the user. For more information, see 'PutBot' .
pbrsVoiceId :: Lens' PutBotResponse (Maybe Text)
pbrsVoiceId = lens _pbrsVoiceId (\s a -> s {_pbrsVoiceId = a})

-- | The date that the bot was updated. When you create a resource, the creation date and last updated date are the same.
pbrsLastUpdatedDate :: Lens' PutBotResponse (Maybe UTCTime)
pbrsLastUpdatedDate = lens _pbrsLastUpdatedDate (\s a -> s {_pbrsLastUpdatedDate = a}) . mapping _Time

-- | @True@ if a new version of the bot was created. If the @createVersion@ field was not specified in the request, the @createVersion@ field is set to false in the response.
pbrsCreateVersion :: Lens' PutBotResponse (Maybe Bool)
pbrsCreateVersion = lens _pbrsCreateVersion (\s a -> s {_pbrsCreateVersion = a})

-- | For each Amazon Lex bot created with the Amazon Lex Model Building Service, you must specify whether your use of Amazon Lex is related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to the Children's Online Privacy Protection Act (COPPA) by specifying @true@ or @false@ in the @childDirected@ field. By specifying @true@ in the @childDirected@ field, you confirm that your use of Amazon Lex __is__ related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA. By specifying @false@ in the @childDirected@ field, you confirm that your use of Amazon Lex __is not__ related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA. You may not specify a default value for the @childDirected@ field that does not accurately reflect whether your use of Amazon Lex is related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA. If your use of Amazon Lex relates to a website, program, or other application that is directed in whole or in part, to children under age 13, you must obtain any required verifiable parental consent under COPPA. For information regarding the use of Amazon Lex in connection with websites, programs, or other applications that are directed or targeted, in whole or in part, to children under age 13, see the <https://aws.amazon.com/lex/faqs#data-security Amazon Lex FAQ.>
pbrsChildDirected :: Lens' PutBotResponse (Maybe Bool)
pbrsChildDirected = lens _pbrsChildDirected (\s a -> s {_pbrsChildDirected = a})

-- | A description of the bot.
pbrsDescription :: Lens' PutBotResponse (Maybe Text)
pbrsDescription = lens _pbrsDescription (\s a -> s {_pbrsDescription = a})

-- | A list of tags associated with the bot.
pbrsTags :: Lens' PutBotResponse [Tag]
pbrsTags = lens _pbrsTags (\s a -> s {_pbrsTags = a}) . _Default . _Coerce

-- | -- | The response status code.
pbrsResponseStatus :: Lens' PutBotResponse Int
pbrsResponseStatus = lens _pbrsResponseStatus (\s a -> s {_pbrsResponseStatus = a})

instance NFData PutBotResponse
