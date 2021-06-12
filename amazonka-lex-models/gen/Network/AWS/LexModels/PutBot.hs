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
-- Module      : Network.AWS.LexModels.PutBot
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon Lex conversational bot or replaces an existing bot.
-- When you create or update a bot you are only required to specify a name,
-- a locale, and whether the bot is directed toward children under age 13.
-- You can use this to add intents later, or to remove intents from an
-- existing bot. When you create a bot with the minimum information, the
-- bot is created or updated but Amazon Lex returns the @@ response
-- @FAILED@. You can build the bot after you add one or more intents. For
-- more information about Amazon Lex bots, see how-it-works.
--
-- If you specify the name of an existing bot, the fields in the request
-- replace the existing values in the @$LATEST@ version of the bot. Amazon
-- Lex removes any fields that you don\'t provide values for in the
-- request, except for the @idleTTLInSeconds@ and @privacySettings@ fields,
-- which are set to their default values. If you don\'t specify values for
-- required fields, Amazon Lex throws an exception.
--
-- This operation requires permissions for the @lex:PutBot@ action. For
-- more information, see security-iam.
module Network.AWS.LexModels.PutBot
  ( -- * Creating a Request
    PutBot (..),
    newPutBot,

    -- * Request Lenses
    putBot_processBehavior,
    putBot_abortStatement,
    putBot_voiceId,
    putBot_nluIntentConfidenceThreshold,
    putBot_clarificationPrompt,
    putBot_enableModelImprovements,
    putBot_idleSessionTTLInSeconds,
    putBot_intents,
    putBot_tags,
    putBot_createVersion,
    putBot_description,
    putBot_detectSentiment,
    putBot_checksum,
    putBot_name,
    putBot_locale,
    putBot_childDirected,

    -- * Destructuring the Response
    PutBotResponse (..),
    newPutBotResponse,

    -- * Response Lenses
    putBotResponse_abortStatement,
    putBotResponse_createdDate,
    putBotResponse_status,
    putBotResponse_voiceId,
    putBotResponse_lastUpdatedDate,
    putBotResponse_nluIntentConfidenceThreshold,
    putBotResponse_locale,
    putBotResponse_clarificationPrompt,
    putBotResponse_enableModelImprovements,
    putBotResponse_version,
    putBotResponse_idleSessionTTLInSeconds,
    putBotResponse_name,
    putBotResponse_intents,
    putBotResponse_failureReason,
    putBotResponse_tags,
    putBotResponse_createVersion,
    putBotResponse_childDirected,
    putBotResponse_description,
    putBotResponse_detectSentiment,
    putBotResponse_checksum,
    putBotResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutBot' smart constructor.
data PutBot = PutBot'
  { -- | If you set the @processBehavior@ element to @BUILD@, Amazon Lex builds
    -- the bot so that it can be run. If you set the element to @SAVE@ Amazon
    -- Lex saves the bot, but doesn\'t build it.
    --
    -- If you don\'t specify this value, the default value is @BUILD@.
    processBehavior :: Core.Maybe ProcessBehavior,
    -- | When Amazon Lex can\'t understand the user\'s input in context, it tries
    -- to elicit the information a few times. After that, Amazon Lex sends the
    -- message defined in @abortStatement@ to the user, and then cancels the
    -- conversation. To set the number of retries, use the
    -- @valueElicitationPrompt@ field for the slot type.
    --
    -- For example, in a pizza ordering bot, Amazon Lex might ask a user \"What
    -- type of crust would you like?\" If the user\'s response is not one of
    -- the expected responses (for example, \"thin crust, \"deep dish,\" etc.),
    -- Amazon Lex tries to elicit a correct response a few more times.
    --
    -- For example, in a pizza ordering application, @OrderPizza@ might be one
    -- of the intents. This intent might require the @CrustType@ slot. You
    -- specify the @valueElicitationPrompt@ field when you create the
    -- @CrustType@ slot.
    --
    -- If you have defined a fallback intent the cancel statement will not be
    -- sent to the user, the fallback intent is used instead. For more
    -- information, see
    -- <https://docs.aws.amazon.com/lex/latest/dg/built-in-intent-fallback.html AMAZON.FallbackIntent>.
    abortStatement :: Core.Maybe Statement,
    -- | The Amazon Polly voice ID that you want Amazon Lex to use for voice
    -- interactions with the user. The locale configured for the voice must
    -- match the locale of the bot. For more information, see
    -- <https://docs.aws.amazon.com/polly/latest/dg/voicelist.html Voices in Amazon Polly>
    -- in the /Amazon Polly Developer Guide/.
    voiceId :: Core.Maybe Core.Text,
    -- | Determines the threshold where Amazon Lex will insert the
    -- @AMAZON.FallbackIntent@, @AMAZON.KendraSearchIntent@, or both when
    -- returning alternative intents in a
    -- <https://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostContent.html PostContent>
    -- or
    -- <https://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostText.html PostText>
    -- response. @AMAZON.FallbackIntent@ and @AMAZON.KendraSearchIntent@ are
    -- only inserted if they are configured for the bot.
    --
    -- You must set the @enableModelImprovements@ parameter to @true@ to use
    -- confidence scores in the following regions.
    --
    -- -   US East (N. Virginia) (us-east-1)
    --
    -- -   US West (Oregon) (us-west-2)
    --
    -- -   Asia Pacific (Sydney) (ap-southeast-2)
    --
    -- -   EU (Ireland) (eu-west-1)
    --
    -- In other Regions, the @enableModelImprovements@ parameter is set to
    -- @true@ by default.
    --
    -- For example, suppose a bot is configured with the confidence threshold
    -- of 0.80 and the @AMAZON.FallbackIntent@. Amazon Lex returns three
    -- alternative intents with the following confidence scores: IntentA
    -- (0.70), IntentB (0.60), IntentC (0.50). The response from the @PostText@
    -- operation would be:
    --
    -- -   AMAZON.FallbackIntent
    --
    -- -   IntentA
    --
    -- -   IntentB
    --
    -- -   IntentC
    nluIntentConfidenceThreshold :: Core.Maybe Core.Double,
    -- | When Amazon Lex doesn\'t understand the user\'s intent, it uses this
    -- message to get clarification. To specify how many times Amazon Lex
    -- should repeat the clarification prompt, use the @maxAttempts@ field. If
    -- Amazon Lex still doesn\'t understand, it sends the message in the
    -- @abortStatement@ field.
    --
    -- When you create a clarification prompt, make sure that it suggests the
    -- correct response from the user. for example, for a bot that orders pizza
    -- and drinks, you might create this clarification prompt: \"What would you
    -- like to do? You can say \'Order a pizza\' or \'Order a drink.\'\"
    --
    -- If you have defined a fallback intent, it will be invoked if the
    -- clarification prompt is repeated the number of times defined in the
    -- @maxAttempts@ field. For more information, see
    -- <https://docs.aws.amazon.com/lex/latest/dg/built-in-intent-fallback.html AMAZON.FallbackIntent>.
    --
    -- If you don\'t define a clarification prompt, at runtime Amazon Lex will
    -- return a 400 Bad Request exception in three cases:
    --
    -- -   Follow-up prompt - When the user responds to a follow-up prompt but
    --     does not provide an intent. For example, in response to a follow-up
    --     prompt that says \"Would you like anything else today?\" the user
    --     says \"Yes.\" Amazon Lex will return a 400 Bad Request exception
    --     because it does not have a clarification prompt to send to the user
    --     to get an intent.
    --
    -- -   Lambda function - When using a Lambda function, you return an
    --     @ElicitIntent@ dialog type. Since Amazon Lex does not have a
    --     clarification prompt to get an intent from the user, it returns a
    --     400 Bad Request exception.
    --
    -- -   PutSession operation - When using the @PutSession@ operation, you
    --     send an @ElicitIntent@ dialog type. Since Amazon Lex does not have a
    --     clarification prompt to get an intent from the user, it returns a
    --     400 Bad Request exception.
    clarificationPrompt :: Core.Maybe Prompt,
    -- | Set to @true@ to enable access to natural language understanding
    -- improvements.
    --
    -- When you set the @enableModelImprovements@ parameter to @true@ you can
    -- use the @nluIntentConfidenceThreshold@ parameter to configure confidence
    -- scores. For more information, see
    -- <https://docs.aws.amazon.com/lex/latest/dg/confidence-scores.html Confidence Scores>.
    --
    -- You can only set the @enableModelImprovements@ parameter in certain
    -- Regions. If you set the parameter to @true@, your bot has access to
    -- accuracy improvements.
    --
    -- The Regions where you can set the @enableModelImprovements@ parameter to
    -- @true@ are:
    --
    -- -   US East (N. Virginia) (us-east-1)
    --
    -- -   US West (Oregon) (us-west-2)
    --
    -- -   Asia Pacific (Sydney) (ap-southeast-2)
    --
    -- -   EU (Ireland) (eu-west-1)
    --
    -- In other Regions, the @enableModelImprovements@ parameter is set to
    -- @true@ by default. In these Regions setting the parameter to @false@
    -- throws a @ValidationException@ exception.
    enableModelImprovements :: Core.Maybe Core.Bool,
    -- | The maximum time in seconds that Amazon Lex retains the data gathered in
    -- a conversation.
    --
    -- A user interaction session remains active for the amount of time
    -- specified. If no conversation occurs during this time, the session
    -- expires and Amazon Lex deletes any data provided before the timeout.
    --
    -- For example, suppose that a user chooses the OrderPizza intent, but gets
    -- sidetracked halfway through placing an order. If the user doesn\'t
    -- complete the order within the specified time, Amazon Lex discards the
    -- slot information that it gathered, and the user must start over.
    --
    -- If you don\'t include the @idleSessionTTLInSeconds@ element in a
    -- @PutBot@ operation request, Amazon Lex uses the default value. This is
    -- also true if the request replaces an existing bot.
    --
    -- The default is 300 seconds (5 minutes).
    idleSessionTTLInSeconds :: Core.Maybe Core.Natural,
    -- | An array of @Intent@ objects. Each intent represents a command that a
    -- user can express. For example, a pizza ordering bot might support an
    -- OrderPizza intent. For more information, see how-it-works.
    intents :: Core.Maybe [Intent],
    -- | A list of tags to add to the bot. You can only add tags when you create
    -- a bot, you can\'t use the @PutBot@ operation to update the tags on a
    -- bot. To update tags, use the @TagResource@ operation.
    tags :: Core.Maybe [Tag],
    -- | When set to @true@ a new numbered version of the bot is created. This is
    -- the same as calling the @CreateBotVersion@ operation. If you don\'t
    -- specify @createVersion@, the default is @false@.
    createVersion :: Core.Maybe Core.Bool,
    -- | A description of the bot.
    description :: Core.Maybe Core.Text,
    -- | When set to @true@ user utterances are sent to Amazon Comprehend for
    -- sentiment analysis. If you don\'t specify @detectSentiment@, the default
    -- is @false@.
    detectSentiment :: Core.Maybe Core.Bool,
    -- | Identifies a specific revision of the @$LATEST@ version.
    --
    -- When you create a new bot, leave the @checksum@ field blank. If you
    -- specify a checksum you get a @BadRequestException@ exception.
    --
    -- When you want to update a bot, set the @checksum@ field to the checksum
    -- of the most recent revision of the @$LATEST@ version. If you don\'t
    -- specify the @ checksum@ field, or if the checksum does not match the
    -- @$LATEST@ version, you get a @PreconditionFailedException@ exception.
    checksum :: Core.Maybe Core.Text,
    -- | The name of the bot. The name is /not/ case sensitive.
    name :: Core.Text,
    -- | Specifies the target locale for the bot. Any intent used in the bot must
    -- be compatible with the locale of the bot.
    --
    -- The default is @en-US@.
    locale :: Locale,
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
    childDirected :: Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutBot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'processBehavior', 'putBot_processBehavior' - If you set the @processBehavior@ element to @BUILD@, Amazon Lex builds
-- the bot so that it can be run. If you set the element to @SAVE@ Amazon
-- Lex saves the bot, but doesn\'t build it.
--
-- If you don\'t specify this value, the default value is @BUILD@.
--
-- 'abortStatement', 'putBot_abortStatement' - When Amazon Lex can\'t understand the user\'s input in context, it tries
-- to elicit the information a few times. After that, Amazon Lex sends the
-- message defined in @abortStatement@ to the user, and then cancels the
-- conversation. To set the number of retries, use the
-- @valueElicitationPrompt@ field for the slot type.
--
-- For example, in a pizza ordering bot, Amazon Lex might ask a user \"What
-- type of crust would you like?\" If the user\'s response is not one of
-- the expected responses (for example, \"thin crust, \"deep dish,\" etc.),
-- Amazon Lex tries to elicit a correct response a few more times.
--
-- For example, in a pizza ordering application, @OrderPizza@ might be one
-- of the intents. This intent might require the @CrustType@ slot. You
-- specify the @valueElicitationPrompt@ field when you create the
-- @CrustType@ slot.
--
-- If you have defined a fallback intent the cancel statement will not be
-- sent to the user, the fallback intent is used instead. For more
-- information, see
-- <https://docs.aws.amazon.com/lex/latest/dg/built-in-intent-fallback.html AMAZON.FallbackIntent>.
--
-- 'voiceId', 'putBot_voiceId' - The Amazon Polly voice ID that you want Amazon Lex to use for voice
-- interactions with the user. The locale configured for the voice must
-- match the locale of the bot. For more information, see
-- <https://docs.aws.amazon.com/polly/latest/dg/voicelist.html Voices in Amazon Polly>
-- in the /Amazon Polly Developer Guide/.
--
-- 'nluIntentConfidenceThreshold', 'putBot_nluIntentConfidenceThreshold' - Determines the threshold where Amazon Lex will insert the
-- @AMAZON.FallbackIntent@, @AMAZON.KendraSearchIntent@, or both when
-- returning alternative intents in a
-- <https://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostContent.html PostContent>
-- or
-- <https://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostText.html PostText>
-- response. @AMAZON.FallbackIntent@ and @AMAZON.KendraSearchIntent@ are
-- only inserted if they are configured for the bot.
--
-- You must set the @enableModelImprovements@ parameter to @true@ to use
-- confidence scores in the following regions.
--
-- -   US East (N. Virginia) (us-east-1)
--
-- -   US West (Oregon) (us-west-2)
--
-- -   Asia Pacific (Sydney) (ap-southeast-2)
--
-- -   EU (Ireland) (eu-west-1)
--
-- In other Regions, the @enableModelImprovements@ parameter is set to
-- @true@ by default.
--
-- For example, suppose a bot is configured with the confidence threshold
-- of 0.80 and the @AMAZON.FallbackIntent@. Amazon Lex returns three
-- alternative intents with the following confidence scores: IntentA
-- (0.70), IntentB (0.60), IntentC (0.50). The response from the @PostText@
-- operation would be:
--
-- -   AMAZON.FallbackIntent
--
-- -   IntentA
--
-- -   IntentB
--
-- -   IntentC
--
-- 'clarificationPrompt', 'putBot_clarificationPrompt' - When Amazon Lex doesn\'t understand the user\'s intent, it uses this
-- message to get clarification. To specify how many times Amazon Lex
-- should repeat the clarification prompt, use the @maxAttempts@ field. If
-- Amazon Lex still doesn\'t understand, it sends the message in the
-- @abortStatement@ field.
--
-- When you create a clarification prompt, make sure that it suggests the
-- correct response from the user. for example, for a bot that orders pizza
-- and drinks, you might create this clarification prompt: \"What would you
-- like to do? You can say \'Order a pizza\' or \'Order a drink.\'\"
--
-- If you have defined a fallback intent, it will be invoked if the
-- clarification prompt is repeated the number of times defined in the
-- @maxAttempts@ field. For more information, see
-- <https://docs.aws.amazon.com/lex/latest/dg/built-in-intent-fallback.html AMAZON.FallbackIntent>.
--
-- If you don\'t define a clarification prompt, at runtime Amazon Lex will
-- return a 400 Bad Request exception in three cases:
--
-- -   Follow-up prompt - When the user responds to a follow-up prompt but
--     does not provide an intent. For example, in response to a follow-up
--     prompt that says \"Would you like anything else today?\" the user
--     says \"Yes.\" Amazon Lex will return a 400 Bad Request exception
--     because it does not have a clarification prompt to send to the user
--     to get an intent.
--
-- -   Lambda function - When using a Lambda function, you return an
--     @ElicitIntent@ dialog type. Since Amazon Lex does not have a
--     clarification prompt to get an intent from the user, it returns a
--     400 Bad Request exception.
--
-- -   PutSession operation - When using the @PutSession@ operation, you
--     send an @ElicitIntent@ dialog type. Since Amazon Lex does not have a
--     clarification prompt to get an intent from the user, it returns a
--     400 Bad Request exception.
--
-- 'enableModelImprovements', 'putBot_enableModelImprovements' - Set to @true@ to enable access to natural language understanding
-- improvements.
--
-- When you set the @enableModelImprovements@ parameter to @true@ you can
-- use the @nluIntentConfidenceThreshold@ parameter to configure confidence
-- scores. For more information, see
-- <https://docs.aws.amazon.com/lex/latest/dg/confidence-scores.html Confidence Scores>.
--
-- You can only set the @enableModelImprovements@ parameter in certain
-- Regions. If you set the parameter to @true@, your bot has access to
-- accuracy improvements.
--
-- The Regions where you can set the @enableModelImprovements@ parameter to
-- @true@ are:
--
-- -   US East (N. Virginia) (us-east-1)
--
-- -   US West (Oregon) (us-west-2)
--
-- -   Asia Pacific (Sydney) (ap-southeast-2)
--
-- -   EU (Ireland) (eu-west-1)
--
-- In other Regions, the @enableModelImprovements@ parameter is set to
-- @true@ by default. In these Regions setting the parameter to @false@
-- throws a @ValidationException@ exception.
--
-- 'idleSessionTTLInSeconds', 'putBot_idleSessionTTLInSeconds' - The maximum time in seconds that Amazon Lex retains the data gathered in
-- a conversation.
--
-- A user interaction session remains active for the amount of time
-- specified. If no conversation occurs during this time, the session
-- expires and Amazon Lex deletes any data provided before the timeout.
--
-- For example, suppose that a user chooses the OrderPizza intent, but gets
-- sidetracked halfway through placing an order. If the user doesn\'t
-- complete the order within the specified time, Amazon Lex discards the
-- slot information that it gathered, and the user must start over.
--
-- If you don\'t include the @idleSessionTTLInSeconds@ element in a
-- @PutBot@ operation request, Amazon Lex uses the default value. This is
-- also true if the request replaces an existing bot.
--
-- The default is 300 seconds (5 minutes).
--
-- 'intents', 'putBot_intents' - An array of @Intent@ objects. Each intent represents a command that a
-- user can express. For example, a pizza ordering bot might support an
-- OrderPizza intent. For more information, see how-it-works.
--
-- 'tags', 'putBot_tags' - A list of tags to add to the bot. You can only add tags when you create
-- a bot, you can\'t use the @PutBot@ operation to update the tags on a
-- bot. To update tags, use the @TagResource@ operation.
--
-- 'createVersion', 'putBot_createVersion' - When set to @true@ a new numbered version of the bot is created. This is
-- the same as calling the @CreateBotVersion@ operation. If you don\'t
-- specify @createVersion@, the default is @false@.
--
-- 'description', 'putBot_description' - A description of the bot.
--
-- 'detectSentiment', 'putBot_detectSentiment' - When set to @true@ user utterances are sent to Amazon Comprehend for
-- sentiment analysis. If you don\'t specify @detectSentiment@, the default
-- is @false@.
--
-- 'checksum', 'putBot_checksum' - Identifies a specific revision of the @$LATEST@ version.
--
-- When you create a new bot, leave the @checksum@ field blank. If you
-- specify a checksum you get a @BadRequestException@ exception.
--
-- When you want to update a bot, set the @checksum@ field to the checksum
-- of the most recent revision of the @$LATEST@ version. If you don\'t
-- specify the @ checksum@ field, or if the checksum does not match the
-- @$LATEST@ version, you get a @PreconditionFailedException@ exception.
--
-- 'name', 'putBot_name' - The name of the bot. The name is /not/ case sensitive.
--
-- 'locale', 'putBot_locale' - Specifies the target locale for the bot. Any intent used in the bot must
-- be compatible with the locale of the bot.
--
-- The default is @en-US@.
--
-- 'childDirected', 'putBot_childDirected' - For each Amazon Lex bot created with the Amazon Lex Model Building
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
newPutBot ::
  -- | 'name'
  Core.Text ->
  -- | 'locale'
  Locale ->
  -- | 'childDirected'
  Core.Bool ->
  PutBot
newPutBot pName_ pLocale_ pChildDirected_ =
  PutBot'
    { processBehavior = Core.Nothing,
      abortStatement = Core.Nothing,
      voiceId = Core.Nothing,
      nluIntentConfidenceThreshold = Core.Nothing,
      clarificationPrompt = Core.Nothing,
      enableModelImprovements = Core.Nothing,
      idleSessionTTLInSeconds = Core.Nothing,
      intents = Core.Nothing,
      tags = Core.Nothing,
      createVersion = Core.Nothing,
      description = Core.Nothing,
      detectSentiment = Core.Nothing,
      checksum = Core.Nothing,
      name = pName_,
      locale = pLocale_,
      childDirected = pChildDirected_
    }

-- | If you set the @processBehavior@ element to @BUILD@, Amazon Lex builds
-- the bot so that it can be run. If you set the element to @SAVE@ Amazon
-- Lex saves the bot, but doesn\'t build it.
--
-- If you don\'t specify this value, the default value is @BUILD@.
putBot_processBehavior :: Lens.Lens' PutBot (Core.Maybe ProcessBehavior)
putBot_processBehavior = Lens.lens (\PutBot' {processBehavior} -> processBehavior) (\s@PutBot' {} a -> s {processBehavior = a} :: PutBot)

-- | When Amazon Lex can\'t understand the user\'s input in context, it tries
-- to elicit the information a few times. After that, Amazon Lex sends the
-- message defined in @abortStatement@ to the user, and then cancels the
-- conversation. To set the number of retries, use the
-- @valueElicitationPrompt@ field for the slot type.
--
-- For example, in a pizza ordering bot, Amazon Lex might ask a user \"What
-- type of crust would you like?\" If the user\'s response is not one of
-- the expected responses (for example, \"thin crust, \"deep dish,\" etc.),
-- Amazon Lex tries to elicit a correct response a few more times.
--
-- For example, in a pizza ordering application, @OrderPizza@ might be one
-- of the intents. This intent might require the @CrustType@ slot. You
-- specify the @valueElicitationPrompt@ field when you create the
-- @CrustType@ slot.
--
-- If you have defined a fallback intent the cancel statement will not be
-- sent to the user, the fallback intent is used instead. For more
-- information, see
-- <https://docs.aws.amazon.com/lex/latest/dg/built-in-intent-fallback.html AMAZON.FallbackIntent>.
putBot_abortStatement :: Lens.Lens' PutBot (Core.Maybe Statement)
putBot_abortStatement = Lens.lens (\PutBot' {abortStatement} -> abortStatement) (\s@PutBot' {} a -> s {abortStatement = a} :: PutBot)

-- | The Amazon Polly voice ID that you want Amazon Lex to use for voice
-- interactions with the user. The locale configured for the voice must
-- match the locale of the bot. For more information, see
-- <https://docs.aws.amazon.com/polly/latest/dg/voicelist.html Voices in Amazon Polly>
-- in the /Amazon Polly Developer Guide/.
putBot_voiceId :: Lens.Lens' PutBot (Core.Maybe Core.Text)
putBot_voiceId = Lens.lens (\PutBot' {voiceId} -> voiceId) (\s@PutBot' {} a -> s {voiceId = a} :: PutBot)

-- | Determines the threshold where Amazon Lex will insert the
-- @AMAZON.FallbackIntent@, @AMAZON.KendraSearchIntent@, or both when
-- returning alternative intents in a
-- <https://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostContent.html PostContent>
-- or
-- <https://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostText.html PostText>
-- response. @AMAZON.FallbackIntent@ and @AMAZON.KendraSearchIntent@ are
-- only inserted if they are configured for the bot.
--
-- You must set the @enableModelImprovements@ parameter to @true@ to use
-- confidence scores in the following regions.
--
-- -   US East (N. Virginia) (us-east-1)
--
-- -   US West (Oregon) (us-west-2)
--
-- -   Asia Pacific (Sydney) (ap-southeast-2)
--
-- -   EU (Ireland) (eu-west-1)
--
-- In other Regions, the @enableModelImprovements@ parameter is set to
-- @true@ by default.
--
-- For example, suppose a bot is configured with the confidence threshold
-- of 0.80 and the @AMAZON.FallbackIntent@. Amazon Lex returns three
-- alternative intents with the following confidence scores: IntentA
-- (0.70), IntentB (0.60), IntentC (0.50). The response from the @PostText@
-- operation would be:
--
-- -   AMAZON.FallbackIntent
--
-- -   IntentA
--
-- -   IntentB
--
-- -   IntentC
putBot_nluIntentConfidenceThreshold :: Lens.Lens' PutBot (Core.Maybe Core.Double)
putBot_nluIntentConfidenceThreshold = Lens.lens (\PutBot' {nluIntentConfidenceThreshold} -> nluIntentConfidenceThreshold) (\s@PutBot' {} a -> s {nluIntentConfidenceThreshold = a} :: PutBot)

-- | When Amazon Lex doesn\'t understand the user\'s intent, it uses this
-- message to get clarification. To specify how many times Amazon Lex
-- should repeat the clarification prompt, use the @maxAttempts@ field. If
-- Amazon Lex still doesn\'t understand, it sends the message in the
-- @abortStatement@ field.
--
-- When you create a clarification prompt, make sure that it suggests the
-- correct response from the user. for example, for a bot that orders pizza
-- and drinks, you might create this clarification prompt: \"What would you
-- like to do? You can say \'Order a pizza\' or \'Order a drink.\'\"
--
-- If you have defined a fallback intent, it will be invoked if the
-- clarification prompt is repeated the number of times defined in the
-- @maxAttempts@ field. For more information, see
-- <https://docs.aws.amazon.com/lex/latest/dg/built-in-intent-fallback.html AMAZON.FallbackIntent>.
--
-- If you don\'t define a clarification prompt, at runtime Amazon Lex will
-- return a 400 Bad Request exception in three cases:
--
-- -   Follow-up prompt - When the user responds to a follow-up prompt but
--     does not provide an intent. For example, in response to a follow-up
--     prompt that says \"Would you like anything else today?\" the user
--     says \"Yes.\" Amazon Lex will return a 400 Bad Request exception
--     because it does not have a clarification prompt to send to the user
--     to get an intent.
--
-- -   Lambda function - When using a Lambda function, you return an
--     @ElicitIntent@ dialog type. Since Amazon Lex does not have a
--     clarification prompt to get an intent from the user, it returns a
--     400 Bad Request exception.
--
-- -   PutSession operation - When using the @PutSession@ operation, you
--     send an @ElicitIntent@ dialog type. Since Amazon Lex does not have a
--     clarification prompt to get an intent from the user, it returns a
--     400 Bad Request exception.
putBot_clarificationPrompt :: Lens.Lens' PutBot (Core.Maybe Prompt)
putBot_clarificationPrompt = Lens.lens (\PutBot' {clarificationPrompt} -> clarificationPrompt) (\s@PutBot' {} a -> s {clarificationPrompt = a} :: PutBot)

-- | Set to @true@ to enable access to natural language understanding
-- improvements.
--
-- When you set the @enableModelImprovements@ parameter to @true@ you can
-- use the @nluIntentConfidenceThreshold@ parameter to configure confidence
-- scores. For more information, see
-- <https://docs.aws.amazon.com/lex/latest/dg/confidence-scores.html Confidence Scores>.
--
-- You can only set the @enableModelImprovements@ parameter in certain
-- Regions. If you set the parameter to @true@, your bot has access to
-- accuracy improvements.
--
-- The Regions where you can set the @enableModelImprovements@ parameter to
-- @true@ are:
--
-- -   US East (N. Virginia) (us-east-1)
--
-- -   US West (Oregon) (us-west-2)
--
-- -   Asia Pacific (Sydney) (ap-southeast-2)
--
-- -   EU (Ireland) (eu-west-1)
--
-- In other Regions, the @enableModelImprovements@ parameter is set to
-- @true@ by default. In these Regions setting the parameter to @false@
-- throws a @ValidationException@ exception.
putBot_enableModelImprovements :: Lens.Lens' PutBot (Core.Maybe Core.Bool)
putBot_enableModelImprovements = Lens.lens (\PutBot' {enableModelImprovements} -> enableModelImprovements) (\s@PutBot' {} a -> s {enableModelImprovements = a} :: PutBot)

-- | The maximum time in seconds that Amazon Lex retains the data gathered in
-- a conversation.
--
-- A user interaction session remains active for the amount of time
-- specified. If no conversation occurs during this time, the session
-- expires and Amazon Lex deletes any data provided before the timeout.
--
-- For example, suppose that a user chooses the OrderPizza intent, but gets
-- sidetracked halfway through placing an order. If the user doesn\'t
-- complete the order within the specified time, Amazon Lex discards the
-- slot information that it gathered, and the user must start over.
--
-- If you don\'t include the @idleSessionTTLInSeconds@ element in a
-- @PutBot@ operation request, Amazon Lex uses the default value. This is
-- also true if the request replaces an existing bot.
--
-- The default is 300 seconds (5 minutes).
putBot_idleSessionTTLInSeconds :: Lens.Lens' PutBot (Core.Maybe Core.Natural)
putBot_idleSessionTTLInSeconds = Lens.lens (\PutBot' {idleSessionTTLInSeconds} -> idleSessionTTLInSeconds) (\s@PutBot' {} a -> s {idleSessionTTLInSeconds = a} :: PutBot)

-- | An array of @Intent@ objects. Each intent represents a command that a
-- user can express. For example, a pizza ordering bot might support an
-- OrderPizza intent. For more information, see how-it-works.
putBot_intents :: Lens.Lens' PutBot (Core.Maybe [Intent])
putBot_intents = Lens.lens (\PutBot' {intents} -> intents) (\s@PutBot' {} a -> s {intents = a} :: PutBot) Core.. Lens.mapping Lens._Coerce

-- | A list of tags to add to the bot. You can only add tags when you create
-- a bot, you can\'t use the @PutBot@ operation to update the tags on a
-- bot. To update tags, use the @TagResource@ operation.
putBot_tags :: Lens.Lens' PutBot (Core.Maybe [Tag])
putBot_tags = Lens.lens (\PutBot' {tags} -> tags) (\s@PutBot' {} a -> s {tags = a} :: PutBot) Core.. Lens.mapping Lens._Coerce

-- | When set to @true@ a new numbered version of the bot is created. This is
-- the same as calling the @CreateBotVersion@ operation. If you don\'t
-- specify @createVersion@, the default is @false@.
putBot_createVersion :: Lens.Lens' PutBot (Core.Maybe Core.Bool)
putBot_createVersion = Lens.lens (\PutBot' {createVersion} -> createVersion) (\s@PutBot' {} a -> s {createVersion = a} :: PutBot)

-- | A description of the bot.
putBot_description :: Lens.Lens' PutBot (Core.Maybe Core.Text)
putBot_description = Lens.lens (\PutBot' {description} -> description) (\s@PutBot' {} a -> s {description = a} :: PutBot)

-- | When set to @true@ user utterances are sent to Amazon Comprehend for
-- sentiment analysis. If you don\'t specify @detectSentiment@, the default
-- is @false@.
putBot_detectSentiment :: Lens.Lens' PutBot (Core.Maybe Core.Bool)
putBot_detectSentiment = Lens.lens (\PutBot' {detectSentiment} -> detectSentiment) (\s@PutBot' {} a -> s {detectSentiment = a} :: PutBot)

-- | Identifies a specific revision of the @$LATEST@ version.
--
-- When you create a new bot, leave the @checksum@ field blank. If you
-- specify a checksum you get a @BadRequestException@ exception.
--
-- When you want to update a bot, set the @checksum@ field to the checksum
-- of the most recent revision of the @$LATEST@ version. If you don\'t
-- specify the @ checksum@ field, or if the checksum does not match the
-- @$LATEST@ version, you get a @PreconditionFailedException@ exception.
putBot_checksum :: Lens.Lens' PutBot (Core.Maybe Core.Text)
putBot_checksum = Lens.lens (\PutBot' {checksum} -> checksum) (\s@PutBot' {} a -> s {checksum = a} :: PutBot)

-- | The name of the bot. The name is /not/ case sensitive.
putBot_name :: Lens.Lens' PutBot Core.Text
putBot_name = Lens.lens (\PutBot' {name} -> name) (\s@PutBot' {} a -> s {name = a} :: PutBot)

-- | Specifies the target locale for the bot. Any intent used in the bot must
-- be compatible with the locale of the bot.
--
-- The default is @en-US@.
putBot_locale :: Lens.Lens' PutBot Locale
putBot_locale = Lens.lens (\PutBot' {locale} -> locale) (\s@PutBot' {} a -> s {locale = a} :: PutBot)

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
putBot_childDirected :: Lens.Lens' PutBot Core.Bool
putBot_childDirected = Lens.lens (\PutBot' {childDirected} -> childDirected) (\s@PutBot' {} a -> s {childDirected = a} :: PutBot)

instance Core.AWSRequest PutBot where
  type AWSResponse PutBot = PutBotResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PutBotResponse'
            Core.<$> (x Core..?> "abortStatement")
            Core.<*> (x Core..?> "createdDate")
            Core.<*> (x Core..?> "status")
            Core.<*> (x Core..?> "voiceId")
            Core.<*> (x Core..?> "lastUpdatedDate")
            Core.<*> (x Core..?> "nluIntentConfidenceThreshold")
            Core.<*> (x Core..?> "locale")
            Core.<*> (x Core..?> "clarificationPrompt")
            Core.<*> (x Core..?> "enableModelImprovements")
            Core.<*> (x Core..?> "version")
            Core.<*> (x Core..?> "idleSessionTTLInSeconds")
            Core.<*> (x Core..?> "name")
            Core.<*> (x Core..?> "intents" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "failureReason")
            Core.<*> (x Core..?> "tags" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "createVersion")
            Core.<*> (x Core..?> "childDirected")
            Core.<*> (x Core..?> "description")
            Core.<*> (x Core..?> "detectSentiment")
            Core.<*> (x Core..?> "checksum")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable PutBot

instance Core.NFData PutBot

instance Core.ToHeaders PutBot where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON PutBot where
  toJSON PutBot' {..} =
    Core.object
      ( Core.catMaybes
          [ ("processBehavior" Core..=)
              Core.<$> processBehavior,
            ("abortStatement" Core..=) Core.<$> abortStatement,
            ("voiceId" Core..=) Core.<$> voiceId,
            ("nluIntentConfidenceThreshold" Core..=)
              Core.<$> nluIntentConfidenceThreshold,
            ("clarificationPrompt" Core..=)
              Core.<$> clarificationPrompt,
            ("enableModelImprovements" Core..=)
              Core.<$> enableModelImprovements,
            ("idleSessionTTLInSeconds" Core..=)
              Core.<$> idleSessionTTLInSeconds,
            ("intents" Core..=) Core.<$> intents,
            ("tags" Core..=) Core.<$> tags,
            ("createVersion" Core..=) Core.<$> createVersion,
            ("description" Core..=) Core.<$> description,
            ("detectSentiment" Core..=) Core.<$> detectSentiment,
            ("checksum" Core..=) Core.<$> checksum,
            Core.Just ("locale" Core..= locale),
            Core.Just ("childDirected" Core..= childDirected)
          ]
      )

instance Core.ToPath PutBot where
  toPath PutBot' {..} =
    Core.mconcat
      ["/bots/", Core.toBS name, "/versions/$LATEST"]

instance Core.ToQuery PutBot where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newPutBotResponse' smart constructor.
data PutBotResponse = PutBotResponse'
  { -- | The message that Amazon Lex uses to cancel a conversation. For more
    -- information, see PutBot.
    abortStatement :: Core.Maybe Statement,
    -- | The date that the bot was created.
    createdDate :: Core.Maybe Core.POSIX,
    -- | When you send a request to create a bot with @processBehavior@ set to
    -- @BUILD@, Amazon Lex sets the @status@ response element to @BUILDING@.
    --
    -- In the @READY_BASIC_TESTING@ state you can test the bot with user inputs
    -- that exactly match the utterances configured for the bot\'s intents and
    -- values in the slot types.
    --
    -- If Amazon Lex can\'t build the bot, Amazon Lex sets @status@ to
    -- @FAILED@. Amazon Lex returns the reason for the failure in the
    -- @failureReason@ response element.
    --
    -- When you set @processBehavior@ to @SAVE@, Amazon Lex sets the status
    -- code to @NOT BUILT@.
    --
    -- When the bot is in the @READY@ state you can test and publish the bot.
    status :: Core.Maybe LexStatus,
    -- | The Amazon Polly voice ID that Amazon Lex uses for voice interaction
    -- with the user. For more information, see PutBot.
    voiceId :: Core.Maybe Core.Text,
    -- | The date that the bot was updated. When you create a resource, the
    -- creation date and last updated date are the same.
    lastUpdatedDate :: Core.Maybe Core.POSIX,
    -- | The score that determines where Amazon Lex inserts the
    -- @AMAZON.FallbackIntent@, @AMAZON.KendraSearchIntent@, or both when
    -- returning alternative intents in a
    -- <https://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostContent.html PostContent>
    -- or
    -- <https://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostText.html PostText>
    -- response. @AMAZON.FallbackIntent@ is inserted if the confidence score
    -- for all intents is below this value. @AMAZON.KendraSearchIntent@ is only
    -- inserted if it is configured for the bot.
    nluIntentConfidenceThreshold :: Core.Maybe Core.Double,
    -- | The target locale for the bot.
    locale :: Core.Maybe Locale,
    -- | The prompts that Amazon Lex uses when it doesn\'t understand the user\'s
    -- intent. For more information, see PutBot.
    clarificationPrompt :: Core.Maybe Prompt,
    -- | Indicates whether the bot uses accuracy improvements. @true@ indicates
    -- that the bot is using the improvements, otherwise, @false@.
    enableModelImprovements :: Core.Maybe Core.Bool,
    -- | The version of the bot. For a new bot, the version is always @$LATEST@.
    version :: Core.Maybe Core.Text,
    -- | The maximum length of time that Amazon Lex retains the data gathered in
    -- a conversation. For more information, see PutBot.
    idleSessionTTLInSeconds :: Core.Maybe Core.Natural,
    -- | The name of the bot.
    name :: Core.Maybe Core.Text,
    -- | An array of @Intent@ objects. For more information, see PutBot.
    intents :: Core.Maybe [Intent],
    -- | If @status@ is @FAILED@, Amazon Lex provides the reason that it failed
    -- to build the bot.
    failureReason :: Core.Maybe Core.Text,
    -- | A list of tags associated with the bot.
    tags :: Core.Maybe [Tag],
    -- | @True@ if a new version of the bot was created. If the @createVersion@
    -- field was not specified in the request, the @createVersion@ field is set
    -- to false in the response.
    createVersion :: Core.Maybe Core.Bool,
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
    childDirected :: Core.Maybe Core.Bool,
    -- | A description of the bot.
    description :: Core.Maybe Core.Text,
    -- | @true@ if the bot is configured to send user utterances to Amazon
    -- Comprehend for sentiment analysis. If the @detectSentiment@ field was
    -- not specified in the request, the @detectSentiment@ field is @false@ in
    -- the response.
    detectSentiment :: Core.Maybe Core.Bool,
    -- | Checksum of the bot that you created.
    checksum :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutBotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'abortStatement', 'putBotResponse_abortStatement' - The message that Amazon Lex uses to cancel a conversation. For more
-- information, see PutBot.
--
-- 'createdDate', 'putBotResponse_createdDate' - The date that the bot was created.
--
-- 'status', 'putBotResponse_status' - When you send a request to create a bot with @processBehavior@ set to
-- @BUILD@, Amazon Lex sets the @status@ response element to @BUILDING@.
--
-- In the @READY_BASIC_TESTING@ state you can test the bot with user inputs
-- that exactly match the utterances configured for the bot\'s intents and
-- values in the slot types.
--
-- If Amazon Lex can\'t build the bot, Amazon Lex sets @status@ to
-- @FAILED@. Amazon Lex returns the reason for the failure in the
-- @failureReason@ response element.
--
-- When you set @processBehavior@ to @SAVE@, Amazon Lex sets the status
-- code to @NOT BUILT@.
--
-- When the bot is in the @READY@ state you can test and publish the bot.
--
-- 'voiceId', 'putBotResponse_voiceId' - The Amazon Polly voice ID that Amazon Lex uses for voice interaction
-- with the user. For more information, see PutBot.
--
-- 'lastUpdatedDate', 'putBotResponse_lastUpdatedDate' - The date that the bot was updated. When you create a resource, the
-- creation date and last updated date are the same.
--
-- 'nluIntentConfidenceThreshold', 'putBotResponse_nluIntentConfidenceThreshold' - The score that determines where Amazon Lex inserts the
-- @AMAZON.FallbackIntent@, @AMAZON.KendraSearchIntent@, or both when
-- returning alternative intents in a
-- <https://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostContent.html PostContent>
-- or
-- <https://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostText.html PostText>
-- response. @AMAZON.FallbackIntent@ is inserted if the confidence score
-- for all intents is below this value. @AMAZON.KendraSearchIntent@ is only
-- inserted if it is configured for the bot.
--
-- 'locale', 'putBotResponse_locale' - The target locale for the bot.
--
-- 'clarificationPrompt', 'putBotResponse_clarificationPrompt' - The prompts that Amazon Lex uses when it doesn\'t understand the user\'s
-- intent. For more information, see PutBot.
--
-- 'enableModelImprovements', 'putBotResponse_enableModelImprovements' - Indicates whether the bot uses accuracy improvements. @true@ indicates
-- that the bot is using the improvements, otherwise, @false@.
--
-- 'version', 'putBotResponse_version' - The version of the bot. For a new bot, the version is always @$LATEST@.
--
-- 'idleSessionTTLInSeconds', 'putBotResponse_idleSessionTTLInSeconds' - The maximum length of time that Amazon Lex retains the data gathered in
-- a conversation. For more information, see PutBot.
--
-- 'name', 'putBotResponse_name' - The name of the bot.
--
-- 'intents', 'putBotResponse_intents' - An array of @Intent@ objects. For more information, see PutBot.
--
-- 'failureReason', 'putBotResponse_failureReason' - If @status@ is @FAILED@, Amazon Lex provides the reason that it failed
-- to build the bot.
--
-- 'tags', 'putBotResponse_tags' - A list of tags associated with the bot.
--
-- 'createVersion', 'putBotResponse_createVersion' - @True@ if a new version of the bot was created. If the @createVersion@
-- field was not specified in the request, the @createVersion@ field is set
-- to false in the response.
--
-- 'childDirected', 'putBotResponse_childDirected' - For each Amazon Lex bot created with the Amazon Lex Model Building
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
-- 'description', 'putBotResponse_description' - A description of the bot.
--
-- 'detectSentiment', 'putBotResponse_detectSentiment' - @true@ if the bot is configured to send user utterances to Amazon
-- Comprehend for sentiment analysis. If the @detectSentiment@ field was
-- not specified in the request, the @detectSentiment@ field is @false@ in
-- the response.
--
-- 'checksum', 'putBotResponse_checksum' - Checksum of the bot that you created.
--
-- 'httpStatus', 'putBotResponse_httpStatus' - The response's http status code.
newPutBotResponse ::
  -- | 'httpStatus'
  Core.Int ->
  PutBotResponse
newPutBotResponse pHttpStatus_ =
  PutBotResponse'
    { abortStatement = Core.Nothing,
      createdDate = Core.Nothing,
      status = Core.Nothing,
      voiceId = Core.Nothing,
      lastUpdatedDate = Core.Nothing,
      nluIntentConfidenceThreshold = Core.Nothing,
      locale = Core.Nothing,
      clarificationPrompt = Core.Nothing,
      enableModelImprovements = Core.Nothing,
      version = Core.Nothing,
      idleSessionTTLInSeconds = Core.Nothing,
      name = Core.Nothing,
      intents = Core.Nothing,
      failureReason = Core.Nothing,
      tags = Core.Nothing,
      createVersion = Core.Nothing,
      childDirected = Core.Nothing,
      description = Core.Nothing,
      detectSentiment = Core.Nothing,
      checksum = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The message that Amazon Lex uses to cancel a conversation. For more
-- information, see PutBot.
putBotResponse_abortStatement :: Lens.Lens' PutBotResponse (Core.Maybe Statement)
putBotResponse_abortStatement = Lens.lens (\PutBotResponse' {abortStatement} -> abortStatement) (\s@PutBotResponse' {} a -> s {abortStatement = a} :: PutBotResponse)

-- | The date that the bot was created.
putBotResponse_createdDate :: Lens.Lens' PutBotResponse (Core.Maybe Core.UTCTime)
putBotResponse_createdDate = Lens.lens (\PutBotResponse' {createdDate} -> createdDate) (\s@PutBotResponse' {} a -> s {createdDate = a} :: PutBotResponse) Core.. Lens.mapping Core._Time

-- | When you send a request to create a bot with @processBehavior@ set to
-- @BUILD@, Amazon Lex sets the @status@ response element to @BUILDING@.
--
-- In the @READY_BASIC_TESTING@ state you can test the bot with user inputs
-- that exactly match the utterances configured for the bot\'s intents and
-- values in the slot types.
--
-- If Amazon Lex can\'t build the bot, Amazon Lex sets @status@ to
-- @FAILED@. Amazon Lex returns the reason for the failure in the
-- @failureReason@ response element.
--
-- When you set @processBehavior@ to @SAVE@, Amazon Lex sets the status
-- code to @NOT BUILT@.
--
-- When the bot is in the @READY@ state you can test and publish the bot.
putBotResponse_status :: Lens.Lens' PutBotResponse (Core.Maybe LexStatus)
putBotResponse_status = Lens.lens (\PutBotResponse' {status} -> status) (\s@PutBotResponse' {} a -> s {status = a} :: PutBotResponse)

-- | The Amazon Polly voice ID that Amazon Lex uses for voice interaction
-- with the user. For more information, see PutBot.
putBotResponse_voiceId :: Lens.Lens' PutBotResponse (Core.Maybe Core.Text)
putBotResponse_voiceId = Lens.lens (\PutBotResponse' {voiceId} -> voiceId) (\s@PutBotResponse' {} a -> s {voiceId = a} :: PutBotResponse)

-- | The date that the bot was updated. When you create a resource, the
-- creation date and last updated date are the same.
putBotResponse_lastUpdatedDate :: Lens.Lens' PutBotResponse (Core.Maybe Core.UTCTime)
putBotResponse_lastUpdatedDate = Lens.lens (\PutBotResponse' {lastUpdatedDate} -> lastUpdatedDate) (\s@PutBotResponse' {} a -> s {lastUpdatedDate = a} :: PutBotResponse) Core.. Lens.mapping Core._Time

-- | The score that determines where Amazon Lex inserts the
-- @AMAZON.FallbackIntent@, @AMAZON.KendraSearchIntent@, or both when
-- returning alternative intents in a
-- <https://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostContent.html PostContent>
-- or
-- <https://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostText.html PostText>
-- response. @AMAZON.FallbackIntent@ is inserted if the confidence score
-- for all intents is below this value. @AMAZON.KendraSearchIntent@ is only
-- inserted if it is configured for the bot.
putBotResponse_nluIntentConfidenceThreshold :: Lens.Lens' PutBotResponse (Core.Maybe Core.Double)
putBotResponse_nluIntentConfidenceThreshold = Lens.lens (\PutBotResponse' {nluIntentConfidenceThreshold} -> nluIntentConfidenceThreshold) (\s@PutBotResponse' {} a -> s {nluIntentConfidenceThreshold = a} :: PutBotResponse)

-- | The target locale for the bot.
putBotResponse_locale :: Lens.Lens' PutBotResponse (Core.Maybe Locale)
putBotResponse_locale = Lens.lens (\PutBotResponse' {locale} -> locale) (\s@PutBotResponse' {} a -> s {locale = a} :: PutBotResponse)

-- | The prompts that Amazon Lex uses when it doesn\'t understand the user\'s
-- intent. For more information, see PutBot.
putBotResponse_clarificationPrompt :: Lens.Lens' PutBotResponse (Core.Maybe Prompt)
putBotResponse_clarificationPrompt = Lens.lens (\PutBotResponse' {clarificationPrompt} -> clarificationPrompt) (\s@PutBotResponse' {} a -> s {clarificationPrompt = a} :: PutBotResponse)

-- | Indicates whether the bot uses accuracy improvements. @true@ indicates
-- that the bot is using the improvements, otherwise, @false@.
putBotResponse_enableModelImprovements :: Lens.Lens' PutBotResponse (Core.Maybe Core.Bool)
putBotResponse_enableModelImprovements = Lens.lens (\PutBotResponse' {enableModelImprovements} -> enableModelImprovements) (\s@PutBotResponse' {} a -> s {enableModelImprovements = a} :: PutBotResponse)

-- | The version of the bot. For a new bot, the version is always @$LATEST@.
putBotResponse_version :: Lens.Lens' PutBotResponse (Core.Maybe Core.Text)
putBotResponse_version = Lens.lens (\PutBotResponse' {version} -> version) (\s@PutBotResponse' {} a -> s {version = a} :: PutBotResponse)

-- | The maximum length of time that Amazon Lex retains the data gathered in
-- a conversation. For more information, see PutBot.
putBotResponse_idleSessionTTLInSeconds :: Lens.Lens' PutBotResponse (Core.Maybe Core.Natural)
putBotResponse_idleSessionTTLInSeconds = Lens.lens (\PutBotResponse' {idleSessionTTLInSeconds} -> idleSessionTTLInSeconds) (\s@PutBotResponse' {} a -> s {idleSessionTTLInSeconds = a} :: PutBotResponse)

-- | The name of the bot.
putBotResponse_name :: Lens.Lens' PutBotResponse (Core.Maybe Core.Text)
putBotResponse_name = Lens.lens (\PutBotResponse' {name} -> name) (\s@PutBotResponse' {} a -> s {name = a} :: PutBotResponse)

-- | An array of @Intent@ objects. For more information, see PutBot.
putBotResponse_intents :: Lens.Lens' PutBotResponse (Core.Maybe [Intent])
putBotResponse_intents = Lens.lens (\PutBotResponse' {intents} -> intents) (\s@PutBotResponse' {} a -> s {intents = a} :: PutBotResponse) Core.. Lens.mapping Lens._Coerce

-- | If @status@ is @FAILED@, Amazon Lex provides the reason that it failed
-- to build the bot.
putBotResponse_failureReason :: Lens.Lens' PutBotResponse (Core.Maybe Core.Text)
putBotResponse_failureReason = Lens.lens (\PutBotResponse' {failureReason} -> failureReason) (\s@PutBotResponse' {} a -> s {failureReason = a} :: PutBotResponse)

-- | A list of tags associated with the bot.
putBotResponse_tags :: Lens.Lens' PutBotResponse (Core.Maybe [Tag])
putBotResponse_tags = Lens.lens (\PutBotResponse' {tags} -> tags) (\s@PutBotResponse' {} a -> s {tags = a} :: PutBotResponse) Core.. Lens.mapping Lens._Coerce

-- | @True@ if a new version of the bot was created. If the @createVersion@
-- field was not specified in the request, the @createVersion@ field is set
-- to false in the response.
putBotResponse_createVersion :: Lens.Lens' PutBotResponse (Core.Maybe Core.Bool)
putBotResponse_createVersion = Lens.lens (\PutBotResponse' {createVersion} -> createVersion) (\s@PutBotResponse' {} a -> s {createVersion = a} :: PutBotResponse)

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
putBotResponse_childDirected :: Lens.Lens' PutBotResponse (Core.Maybe Core.Bool)
putBotResponse_childDirected = Lens.lens (\PutBotResponse' {childDirected} -> childDirected) (\s@PutBotResponse' {} a -> s {childDirected = a} :: PutBotResponse)

-- | A description of the bot.
putBotResponse_description :: Lens.Lens' PutBotResponse (Core.Maybe Core.Text)
putBotResponse_description = Lens.lens (\PutBotResponse' {description} -> description) (\s@PutBotResponse' {} a -> s {description = a} :: PutBotResponse)

-- | @true@ if the bot is configured to send user utterances to Amazon
-- Comprehend for sentiment analysis. If the @detectSentiment@ field was
-- not specified in the request, the @detectSentiment@ field is @false@ in
-- the response.
putBotResponse_detectSentiment :: Lens.Lens' PutBotResponse (Core.Maybe Core.Bool)
putBotResponse_detectSentiment = Lens.lens (\PutBotResponse' {detectSentiment} -> detectSentiment) (\s@PutBotResponse' {} a -> s {detectSentiment = a} :: PutBotResponse)

-- | Checksum of the bot that you created.
putBotResponse_checksum :: Lens.Lens' PutBotResponse (Core.Maybe Core.Text)
putBotResponse_checksum = Lens.lens (\PutBotResponse' {checksum} -> checksum) (\s@PutBotResponse' {} a -> s {checksum = a} :: PutBotResponse)

-- | The response's http status code.
putBotResponse_httpStatus :: Lens.Lens' PutBotResponse Core.Int
putBotResponse_httpStatus = Lens.lens (\PutBotResponse' {httpStatus} -> httpStatus) (\s@PutBotResponse' {} a -> s {httpStatus = a} :: PutBotResponse)

instance Core.NFData PutBotResponse
