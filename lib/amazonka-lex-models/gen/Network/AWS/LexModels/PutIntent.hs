{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.PutIntent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an intent or replaces an existing intent.
--
-- To define the interaction between the user and your bot, you use one or more intents. For a pizza ordering bot, for example, you would create an @OrderPizza@ intent.
-- To create an intent or replace an existing intent, you must provide the following:
--
--     * Intent name. For example, @OrderPizza@ .
--
--
--     * Sample utterances. For example, "Can I order a pizza, please." and "I want to order a pizza."
--
--
--     * Information to be gathered. You specify slot types for the information that your bot will request from the user. You can specify standard slot types, such as a date or a time, or custom slot types such as the size and crust of a pizza.
--
--
--     * How the intent will be fulfilled. You can provide a Lambda function or configure the intent to return the intent information to the client application. If you use a Lambda function, when all of the intent information is available, Amazon Lex invokes your Lambda function. If you configure your intent to return the intent information to the client application.
--
--
-- You can specify other optional information in the request, such as:
--
--     * A confirmation prompt to ask the user to confirm an intent. For example, "Shall I order your pizza?"
--
--
--     * A conclusion statement to send to the user after the intent has been fulfilled. For example, "I placed your pizza order."
--
--
--     * A follow-up prompt that asks the user for additional activity. For example, asking "Do you want to order a drink with your pizza?"
--
--
-- If you specify an existing intent name to update the intent, Amazon Lex replaces the values in the @> LATEST@ version of the intent with the values in the request. Amazon Lex removes fields that you don't provide in the request. If you don't specify the required fields, Amazon Lex throws an exception. When you update the @> LATEST@ version of an intent, the @status@ field of any bot that uses the @> LATEST@ version of the intent is set to @NOT_BUILT@ .
-- For more information, see 'how-it-works' .
-- This operation requires permissions for the @lex:PutIntent@ action.
module Network.AWS.LexModels.PutIntent
  ( -- * Creating a request
    PutIntent (..),
    mkPutIntent,

    -- ** Request lenses
    piFulfillmentActivity,
    piSlots,
    piRejectionStatement,
    piChecksum,
    piConclusionStatement,
    piSampleUtterances,
    piParentIntentSignature,
    piKendraConfiguration,
    piName,
    piInputContexts,
    piFollowUpPrompt,
    piOutputContexts,
    piConfirmationPrompt,
    piCreateVersion,
    piDialogCodeHook,
    piDescription,

    -- * Destructuring the response
    PutIntentResponse (..),
    mkPutIntentResponse,

    -- ** Response lenses
    pirsFulfillmentActivity,
    pirsSlots,
    pirsRejectionStatement,
    pirsChecksum,
    pirsConclusionStatement,
    pirsSampleUtterances,
    pirsParentIntentSignature,
    pirsCreatedDate,
    pirsKendraConfiguration,
    pirsName,
    pirsVersion,
    pirsInputContexts,
    pirsFollowUpPrompt,
    pirsLastUpdatedDate,
    pirsOutputContexts,
    pirsConfirmationPrompt,
    pirsCreateVersion,
    pirsDialogCodeHook,
    pirsDescription,
    pirsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutIntent' smart constructor.
data PutIntent = PutIntent'
  { -- | Required. Describes how the intent is fulfilled. For example, after a user provides all of the information for a pizza order, @fulfillmentActivity@ defines how the bot places an order with a local pizza store.
    --
    -- You might configure Amazon Lex to return all of the intent information to the client application, or direct it to invoke a Lambda function that can process the intent (for example, place an order with a pizzeria).
    fulfillmentActivity :: Lude.Maybe FulfillmentActivity,
    -- | An array of intent slots. At runtime, Amazon Lex elicits required slot values from the user using prompts defined in the slots. For more information, see 'how-it-works' .
    slots :: Lude.Maybe [Slot],
    -- | When the user answers "no" to the question defined in @confirmationPrompt@ , Amazon Lex responds with this statement to acknowledge that the intent was canceled.
    rejectionStatement :: Lude.Maybe Statement,
    -- | Identifies a specific revision of the @> LATEST@ version.
    --
    -- When you create a new intent, leave the @checksum@ field blank. If you specify a checksum you get a @BadRequestException@ exception.
    -- When you want to update a intent, set the @checksum@ field to the checksum of the most recent revision of the @> LATEST@ version. If you don't specify the @checksum@ field, or if the checksum does not match the @> LATEST@ version, you get a @PreconditionFailedException@ exception.
    checksum :: Lude.Maybe Lude.Text,
    -- | The statement that you want Amazon Lex to convey to the user after the intent is successfully fulfilled by the Lambda function.
    --
    -- This element is relevant only if you provide a Lambda function in the @fulfillmentActivity@ . If you return the intent to the client application, you can't specify this element.
    conclusionStatement :: Lude.Maybe Statement,
    -- | An array of utterances (strings) that a user might say to signal the intent. For example, "I want {PizzaSize} pizza", "Order {Quantity} {PizzaSize} pizzas".
    --
    -- In each utterance, a slot name is enclosed in curly braces.
    sampleUtterances :: Lude.Maybe [Lude.Text],
    -- | A unique identifier for the built-in intent to base this intent on. To find the signature for an intent, see <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/standard-intents Standard Built-in Intents> in the /Alexa Skills Kit/ .
    parentIntentSignature :: Lude.Maybe Lude.Text,
    -- | Configuration information required to use the @AMAZON.KendraSearchIntent@ intent to connect to an Amazon Kendra index. For more information, see <http://docs.aws.amazon.com/lex/latest/dg/built-in-intent-kendra-search.html AMAZON.KendraSearchIntent> .
    kendraConfiguration :: Lude.Maybe KendraConfiguration,
    -- | The name of the intent. The name is /not/ case sensitive.
    --
    -- The name can't match a built-in intent name, or a built-in intent name with "AMAZON." removed. For example, because there is a built-in intent called @AMAZON.HelpIntent@ , you can't create a custom intent called @HelpIntent@ .
    -- For a list of built-in intents, see <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/standard-intents Standard Built-in Intents> in the /Alexa Skills Kit/ .
    name :: Lude.Text,
    -- | An array of @InputContext@ objects that lists the contexts that must be active for Amazon Lex to choose the intent in a conversation with the user.
    inputContexts :: Lude.Maybe [InputContext],
    -- | Amazon Lex uses this prompt to solicit additional activity after fulfilling an intent. For example, after the @OrderPizza@ intent is fulfilled, you might prompt the user to order a drink.
    --
    -- The action that Amazon Lex takes depends on the user's response, as follows:
    --
    --     * If the user says "Yes" it responds with the clarification prompt that is configured for the bot.
    --
    --
    --     * if the user says "Yes" and continues with an utterance that triggers an intent it starts a conversation for the intent.
    --
    --
    --     * If the user says "No" it responds with the rejection statement configured for the the follow-up prompt.
    --
    --
    --     * If it doesn't recognize the utterance it repeats the follow-up prompt again.
    --
    --
    -- The @followUpPrompt@ field and the @conclusionStatement@ field are mutually exclusive. You can specify only one.
    followUpPrompt :: Lude.Maybe FollowUpPrompt,
    -- | An array of @OutputContext@ objects that lists the contexts that the intent activates when the intent is fulfilled.
    outputContexts :: Lude.Maybe [OutputContext],
    -- | Prompts the user to confirm the intent. This question should have a yes or no answer.
    --
    -- Amazon Lex uses this prompt to ensure that the user acknowledges that the intent is ready for fulfillment. For example, with the @OrderPizza@ intent, you might want to confirm that the order is correct before placing it. For other intents, such as intents that simply respond to user questions, you might not need to ask the user for confirmation before providing the information.
    confirmationPrompt :: Lude.Maybe Prompt,
    -- | When set to @true@ a new numbered version of the intent is created. This is the same as calling the @CreateIntentVersion@ operation. If you do not specify @createVersion@ , the default is @false@ .
    createVersion :: Lude.Maybe Lude.Bool,
    -- | Specifies a Lambda function to invoke for each user input. You can invoke this Lambda function to personalize user interaction.
    --
    -- For example, suppose your bot determines that the user is John. Your Lambda function might retrieve John's information from a backend database and prepopulate some of the values. For example, if you find that John is gluten intolerant, you might set the corresponding intent slot, @GlutenIntolerant@ , to true. You might find John's phone number and set the corresponding session attribute.
    dialogCodeHook :: Lude.Maybe CodeHook,
    -- | A description of the intent.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutIntent' with the minimum fields required to make a request.
--
-- * 'fulfillmentActivity' - Required. Describes how the intent is fulfilled. For example, after a user provides all of the information for a pizza order, @fulfillmentActivity@ defines how the bot places an order with a local pizza store.
--
-- You might configure Amazon Lex to return all of the intent information to the client application, or direct it to invoke a Lambda function that can process the intent (for example, place an order with a pizzeria).
-- * 'slots' - An array of intent slots. At runtime, Amazon Lex elicits required slot values from the user using prompts defined in the slots. For more information, see 'how-it-works' .
-- * 'rejectionStatement' - When the user answers "no" to the question defined in @confirmationPrompt@ , Amazon Lex responds with this statement to acknowledge that the intent was canceled.
-- * 'checksum' - Identifies a specific revision of the @> LATEST@ version.
--
-- When you create a new intent, leave the @checksum@ field blank. If you specify a checksum you get a @BadRequestException@ exception.
-- When you want to update a intent, set the @checksum@ field to the checksum of the most recent revision of the @> LATEST@ version. If you don't specify the @checksum@ field, or if the checksum does not match the @> LATEST@ version, you get a @PreconditionFailedException@ exception.
-- * 'conclusionStatement' - The statement that you want Amazon Lex to convey to the user after the intent is successfully fulfilled by the Lambda function.
--
-- This element is relevant only if you provide a Lambda function in the @fulfillmentActivity@ . If you return the intent to the client application, you can't specify this element.
-- * 'sampleUtterances' - An array of utterances (strings) that a user might say to signal the intent. For example, "I want {PizzaSize} pizza", "Order {Quantity} {PizzaSize} pizzas".
--
-- In each utterance, a slot name is enclosed in curly braces.
-- * 'parentIntentSignature' - A unique identifier for the built-in intent to base this intent on. To find the signature for an intent, see <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/standard-intents Standard Built-in Intents> in the /Alexa Skills Kit/ .
-- * 'kendraConfiguration' - Configuration information required to use the @AMAZON.KendraSearchIntent@ intent to connect to an Amazon Kendra index. For more information, see <http://docs.aws.amazon.com/lex/latest/dg/built-in-intent-kendra-search.html AMAZON.KendraSearchIntent> .
-- * 'name' - The name of the intent. The name is /not/ case sensitive.
--
-- The name can't match a built-in intent name, or a built-in intent name with "AMAZON." removed. For example, because there is a built-in intent called @AMAZON.HelpIntent@ , you can't create a custom intent called @HelpIntent@ .
-- For a list of built-in intents, see <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/standard-intents Standard Built-in Intents> in the /Alexa Skills Kit/ .
-- * 'inputContexts' - An array of @InputContext@ objects that lists the contexts that must be active for Amazon Lex to choose the intent in a conversation with the user.
-- * 'followUpPrompt' - Amazon Lex uses this prompt to solicit additional activity after fulfilling an intent. For example, after the @OrderPizza@ intent is fulfilled, you might prompt the user to order a drink.
--
-- The action that Amazon Lex takes depends on the user's response, as follows:
--
--     * If the user says "Yes" it responds with the clarification prompt that is configured for the bot.
--
--
--     * if the user says "Yes" and continues with an utterance that triggers an intent it starts a conversation for the intent.
--
--
--     * If the user says "No" it responds with the rejection statement configured for the the follow-up prompt.
--
--
--     * If it doesn't recognize the utterance it repeats the follow-up prompt again.
--
--
-- The @followUpPrompt@ field and the @conclusionStatement@ field are mutually exclusive. You can specify only one.
-- * 'outputContexts' - An array of @OutputContext@ objects that lists the contexts that the intent activates when the intent is fulfilled.
-- * 'confirmationPrompt' - Prompts the user to confirm the intent. This question should have a yes or no answer.
--
-- Amazon Lex uses this prompt to ensure that the user acknowledges that the intent is ready for fulfillment. For example, with the @OrderPizza@ intent, you might want to confirm that the order is correct before placing it. For other intents, such as intents that simply respond to user questions, you might not need to ask the user for confirmation before providing the information.
-- * 'createVersion' - When set to @true@ a new numbered version of the intent is created. This is the same as calling the @CreateIntentVersion@ operation. If you do not specify @createVersion@ , the default is @false@ .
-- * 'dialogCodeHook' - Specifies a Lambda function to invoke for each user input. You can invoke this Lambda function to personalize user interaction.
--
-- For example, suppose your bot determines that the user is John. Your Lambda function might retrieve John's information from a backend database and prepopulate some of the values. For example, if you find that John is gluten intolerant, you might set the corresponding intent slot, @GlutenIntolerant@ , to true. You might find John's phone number and set the corresponding session attribute.
-- * 'description' - A description of the intent.
mkPutIntent ::
  -- | 'name'
  Lude.Text ->
  PutIntent
mkPutIntent pName_ =
  PutIntent'
    { fulfillmentActivity = Lude.Nothing,
      slots = Lude.Nothing,
      rejectionStatement = Lude.Nothing,
      checksum = Lude.Nothing,
      conclusionStatement = Lude.Nothing,
      sampleUtterances = Lude.Nothing,
      parentIntentSignature = Lude.Nothing,
      kendraConfiguration = Lude.Nothing,
      name = pName_,
      inputContexts = Lude.Nothing,
      followUpPrompt = Lude.Nothing,
      outputContexts = Lude.Nothing,
      confirmationPrompt = Lude.Nothing,
      createVersion = Lude.Nothing,
      dialogCodeHook = Lude.Nothing,
      description = Lude.Nothing
    }

-- | Required. Describes how the intent is fulfilled. For example, after a user provides all of the information for a pizza order, @fulfillmentActivity@ defines how the bot places an order with a local pizza store.
--
-- You might configure Amazon Lex to return all of the intent information to the client application, or direct it to invoke a Lambda function that can process the intent (for example, place an order with a pizzeria).
--
-- /Note:/ Consider using 'fulfillmentActivity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piFulfillmentActivity :: Lens.Lens' PutIntent (Lude.Maybe FulfillmentActivity)
piFulfillmentActivity = Lens.lens (fulfillmentActivity :: PutIntent -> Lude.Maybe FulfillmentActivity) (\s a -> s {fulfillmentActivity = a} :: PutIntent)
{-# DEPRECATED piFulfillmentActivity "Use generic-lens or generic-optics with 'fulfillmentActivity' instead." #-}

-- | An array of intent slots. At runtime, Amazon Lex elicits required slot values from the user using prompts defined in the slots. For more information, see 'how-it-works' .
--
-- /Note:/ Consider using 'slots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piSlots :: Lens.Lens' PutIntent (Lude.Maybe [Slot])
piSlots = Lens.lens (slots :: PutIntent -> Lude.Maybe [Slot]) (\s a -> s {slots = a} :: PutIntent)
{-# DEPRECATED piSlots "Use generic-lens or generic-optics with 'slots' instead." #-}

-- | When the user answers "no" to the question defined in @confirmationPrompt@ , Amazon Lex responds with this statement to acknowledge that the intent was canceled.
--
-- /Note:/ Consider using 'rejectionStatement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piRejectionStatement :: Lens.Lens' PutIntent (Lude.Maybe Statement)
piRejectionStatement = Lens.lens (rejectionStatement :: PutIntent -> Lude.Maybe Statement) (\s a -> s {rejectionStatement = a} :: PutIntent)
{-# DEPRECATED piRejectionStatement "Use generic-lens or generic-optics with 'rejectionStatement' instead." #-}

-- | Identifies a specific revision of the @> LATEST@ version.
--
-- When you create a new intent, leave the @checksum@ field blank. If you specify a checksum you get a @BadRequestException@ exception.
-- When you want to update a intent, set the @checksum@ field to the checksum of the most recent revision of the @> LATEST@ version. If you don't specify the @checksum@ field, or if the checksum does not match the @> LATEST@ version, you get a @PreconditionFailedException@ exception.
--
-- /Note:/ Consider using 'checksum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piChecksum :: Lens.Lens' PutIntent (Lude.Maybe Lude.Text)
piChecksum = Lens.lens (checksum :: PutIntent -> Lude.Maybe Lude.Text) (\s a -> s {checksum = a} :: PutIntent)
{-# DEPRECATED piChecksum "Use generic-lens or generic-optics with 'checksum' instead." #-}

-- | The statement that you want Amazon Lex to convey to the user after the intent is successfully fulfilled by the Lambda function.
--
-- This element is relevant only if you provide a Lambda function in the @fulfillmentActivity@ . If you return the intent to the client application, you can't specify this element.
--
-- /Note:/ Consider using 'conclusionStatement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piConclusionStatement :: Lens.Lens' PutIntent (Lude.Maybe Statement)
piConclusionStatement = Lens.lens (conclusionStatement :: PutIntent -> Lude.Maybe Statement) (\s a -> s {conclusionStatement = a} :: PutIntent)
{-# DEPRECATED piConclusionStatement "Use generic-lens or generic-optics with 'conclusionStatement' instead." #-}

-- | An array of utterances (strings) that a user might say to signal the intent. For example, "I want {PizzaSize} pizza", "Order {Quantity} {PizzaSize} pizzas".
--
-- In each utterance, a slot name is enclosed in curly braces.
--
-- /Note:/ Consider using 'sampleUtterances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piSampleUtterances :: Lens.Lens' PutIntent (Lude.Maybe [Lude.Text])
piSampleUtterances = Lens.lens (sampleUtterances :: PutIntent -> Lude.Maybe [Lude.Text]) (\s a -> s {sampleUtterances = a} :: PutIntent)
{-# DEPRECATED piSampleUtterances "Use generic-lens or generic-optics with 'sampleUtterances' instead." #-}

-- | A unique identifier for the built-in intent to base this intent on. To find the signature for an intent, see <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/standard-intents Standard Built-in Intents> in the /Alexa Skills Kit/ .
--
-- /Note:/ Consider using 'parentIntentSignature' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piParentIntentSignature :: Lens.Lens' PutIntent (Lude.Maybe Lude.Text)
piParentIntentSignature = Lens.lens (parentIntentSignature :: PutIntent -> Lude.Maybe Lude.Text) (\s a -> s {parentIntentSignature = a} :: PutIntent)
{-# DEPRECATED piParentIntentSignature "Use generic-lens or generic-optics with 'parentIntentSignature' instead." #-}

-- | Configuration information required to use the @AMAZON.KendraSearchIntent@ intent to connect to an Amazon Kendra index. For more information, see <http://docs.aws.amazon.com/lex/latest/dg/built-in-intent-kendra-search.html AMAZON.KendraSearchIntent> .
--
-- /Note:/ Consider using 'kendraConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piKendraConfiguration :: Lens.Lens' PutIntent (Lude.Maybe KendraConfiguration)
piKendraConfiguration = Lens.lens (kendraConfiguration :: PutIntent -> Lude.Maybe KendraConfiguration) (\s a -> s {kendraConfiguration = a} :: PutIntent)
{-# DEPRECATED piKendraConfiguration "Use generic-lens or generic-optics with 'kendraConfiguration' instead." #-}

-- | The name of the intent. The name is /not/ case sensitive.
--
-- The name can't match a built-in intent name, or a built-in intent name with "AMAZON." removed. For example, because there is a built-in intent called @AMAZON.HelpIntent@ , you can't create a custom intent called @HelpIntent@ .
-- For a list of built-in intents, see <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/standard-intents Standard Built-in Intents> in the /Alexa Skills Kit/ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piName :: Lens.Lens' PutIntent Lude.Text
piName = Lens.lens (name :: PutIntent -> Lude.Text) (\s a -> s {name = a} :: PutIntent)
{-# DEPRECATED piName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | An array of @InputContext@ objects that lists the contexts that must be active for Amazon Lex to choose the intent in a conversation with the user.
--
-- /Note:/ Consider using 'inputContexts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piInputContexts :: Lens.Lens' PutIntent (Lude.Maybe [InputContext])
piInputContexts = Lens.lens (inputContexts :: PutIntent -> Lude.Maybe [InputContext]) (\s a -> s {inputContexts = a} :: PutIntent)
{-# DEPRECATED piInputContexts "Use generic-lens or generic-optics with 'inputContexts' instead." #-}

-- | Amazon Lex uses this prompt to solicit additional activity after fulfilling an intent. For example, after the @OrderPizza@ intent is fulfilled, you might prompt the user to order a drink.
--
-- The action that Amazon Lex takes depends on the user's response, as follows:
--
--     * If the user says "Yes" it responds with the clarification prompt that is configured for the bot.
--
--
--     * if the user says "Yes" and continues with an utterance that triggers an intent it starts a conversation for the intent.
--
--
--     * If the user says "No" it responds with the rejection statement configured for the the follow-up prompt.
--
--
--     * If it doesn't recognize the utterance it repeats the follow-up prompt again.
--
--
-- The @followUpPrompt@ field and the @conclusionStatement@ field are mutually exclusive. You can specify only one.
--
-- /Note:/ Consider using 'followUpPrompt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piFollowUpPrompt :: Lens.Lens' PutIntent (Lude.Maybe FollowUpPrompt)
piFollowUpPrompt = Lens.lens (followUpPrompt :: PutIntent -> Lude.Maybe FollowUpPrompt) (\s a -> s {followUpPrompt = a} :: PutIntent)
{-# DEPRECATED piFollowUpPrompt "Use generic-lens or generic-optics with 'followUpPrompt' instead." #-}

-- | An array of @OutputContext@ objects that lists the contexts that the intent activates when the intent is fulfilled.
--
-- /Note:/ Consider using 'outputContexts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piOutputContexts :: Lens.Lens' PutIntent (Lude.Maybe [OutputContext])
piOutputContexts = Lens.lens (outputContexts :: PutIntent -> Lude.Maybe [OutputContext]) (\s a -> s {outputContexts = a} :: PutIntent)
{-# DEPRECATED piOutputContexts "Use generic-lens or generic-optics with 'outputContexts' instead." #-}

-- | Prompts the user to confirm the intent. This question should have a yes or no answer.
--
-- Amazon Lex uses this prompt to ensure that the user acknowledges that the intent is ready for fulfillment. For example, with the @OrderPizza@ intent, you might want to confirm that the order is correct before placing it. For other intents, such as intents that simply respond to user questions, you might not need to ask the user for confirmation before providing the information.
--
-- /Note:/ Consider using 'confirmationPrompt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piConfirmationPrompt :: Lens.Lens' PutIntent (Lude.Maybe Prompt)
piConfirmationPrompt = Lens.lens (confirmationPrompt :: PutIntent -> Lude.Maybe Prompt) (\s a -> s {confirmationPrompt = a} :: PutIntent)
{-# DEPRECATED piConfirmationPrompt "Use generic-lens or generic-optics with 'confirmationPrompt' instead." #-}

-- | When set to @true@ a new numbered version of the intent is created. This is the same as calling the @CreateIntentVersion@ operation. If you do not specify @createVersion@ , the default is @false@ .
--
-- /Note:/ Consider using 'createVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piCreateVersion :: Lens.Lens' PutIntent (Lude.Maybe Lude.Bool)
piCreateVersion = Lens.lens (createVersion :: PutIntent -> Lude.Maybe Lude.Bool) (\s a -> s {createVersion = a} :: PutIntent)
{-# DEPRECATED piCreateVersion "Use generic-lens or generic-optics with 'createVersion' instead." #-}

-- | Specifies a Lambda function to invoke for each user input. You can invoke this Lambda function to personalize user interaction.
--
-- For example, suppose your bot determines that the user is John. Your Lambda function might retrieve John's information from a backend database and prepopulate some of the values. For example, if you find that John is gluten intolerant, you might set the corresponding intent slot, @GlutenIntolerant@ , to true. You might find John's phone number and set the corresponding session attribute.
--
-- /Note:/ Consider using 'dialogCodeHook' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piDialogCodeHook :: Lens.Lens' PutIntent (Lude.Maybe CodeHook)
piDialogCodeHook = Lens.lens (dialogCodeHook :: PutIntent -> Lude.Maybe CodeHook) (\s a -> s {dialogCodeHook = a} :: PutIntent)
{-# DEPRECATED piDialogCodeHook "Use generic-lens or generic-optics with 'dialogCodeHook' instead." #-}

-- | A description of the intent.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piDescription :: Lens.Lens' PutIntent (Lude.Maybe Lude.Text)
piDescription = Lens.lens (description :: PutIntent -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: PutIntent)
{-# DEPRECATED piDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.AWSRequest PutIntent where
  type Rs PutIntent = PutIntentResponse
  request = Req.putJSON lexModelsService
  response =
    Res.receiveJSON
      ( \s h x ->
          PutIntentResponse'
            Lude.<$> (x Lude..?> "fulfillmentActivity")
            Lude.<*> (x Lude..?> "slots" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "rejectionStatement")
            Lude.<*> (x Lude..?> "checksum")
            Lude.<*> (x Lude..?> "conclusionStatement")
            Lude.<*> (x Lude..?> "sampleUtterances" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "parentIntentSignature")
            Lude.<*> (x Lude..?> "createdDate")
            Lude.<*> (x Lude..?> "kendraConfiguration")
            Lude.<*> (x Lude..?> "name")
            Lude.<*> (x Lude..?> "version")
            Lude.<*> (x Lude..?> "inputContexts" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "followUpPrompt")
            Lude.<*> (x Lude..?> "lastUpdatedDate")
            Lude.<*> (x Lude..?> "outputContexts" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "confirmationPrompt")
            Lude.<*> (x Lude..?> "createVersion")
            Lude.<*> (x Lude..?> "dialogCodeHook")
            Lude.<*> (x Lude..?> "description")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutIntent where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutIntent where
  toJSON PutIntent' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("fulfillmentActivity" Lude..=) Lude.<$> fulfillmentActivity,
            ("slots" Lude..=) Lude.<$> slots,
            ("rejectionStatement" Lude..=) Lude.<$> rejectionStatement,
            ("checksum" Lude..=) Lude.<$> checksum,
            ("conclusionStatement" Lude..=) Lude.<$> conclusionStatement,
            ("sampleUtterances" Lude..=) Lude.<$> sampleUtterances,
            ("parentIntentSignature" Lude..=) Lude.<$> parentIntentSignature,
            ("kendraConfiguration" Lude..=) Lude.<$> kendraConfiguration,
            ("inputContexts" Lude..=) Lude.<$> inputContexts,
            ("followUpPrompt" Lude..=) Lude.<$> followUpPrompt,
            ("outputContexts" Lude..=) Lude.<$> outputContexts,
            ("confirmationPrompt" Lude..=) Lude.<$> confirmationPrompt,
            ("createVersion" Lude..=) Lude.<$> createVersion,
            ("dialogCodeHook" Lude..=) Lude.<$> dialogCodeHook,
            ("description" Lude..=) Lude.<$> description
          ]
      )

instance Lude.ToPath PutIntent where
  toPath PutIntent' {..} =
    Lude.mconcat ["/intents/", Lude.toBS name, "/versions/$LATEST"]

instance Lude.ToQuery PutIntent where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutIntentResponse' smart constructor.
data PutIntentResponse = PutIntentResponse'
  { -- | If defined in the intent, Amazon Lex invokes this Lambda function to fulfill the intent after the user provides all of the information required by the intent.
    fulfillmentActivity :: Lude.Maybe FulfillmentActivity,
    -- | An array of intent slots that are configured for the intent.
    slots :: Lude.Maybe [Slot],
    -- | If the user answers "no" to the question defined in @confirmationPrompt@ Amazon Lex responds with this statement to acknowledge that the intent was canceled.
    rejectionStatement :: Lude.Maybe Statement,
    -- | Checksum of the @> LATEST@ version of the intent created or updated.
    checksum :: Lude.Maybe Lude.Text,
    -- | After the Lambda function specified in the@fulfillmentActivity@ intent fulfills the intent, Amazon Lex conveys this statement to the user.
    conclusionStatement :: Lude.Maybe Statement,
    -- | An array of sample utterances that are configured for the intent.
    sampleUtterances :: Lude.Maybe [Lude.Text],
    -- | A unique identifier for the built-in intent that this intent is based on.
    parentIntentSignature :: Lude.Maybe Lude.Text,
    -- | The date that the intent was created.
    createdDate :: Lude.Maybe Lude.Timestamp,
    -- | Configuration information, if any, required to connect to an Amazon Kendra index and use the @AMAZON.KendraSearchIntent@ intent.
    kendraConfiguration :: Lude.Maybe KendraConfiguration,
    -- | The name of the intent.
    name :: Lude.Maybe Lude.Text,
    -- | The version of the intent. For a new intent, the version is always @> LATEST@ .
    version :: Lude.Maybe Lude.Text,
    -- | An array of @InputContext@ objects that lists the contexts that must be active for Amazon Lex to choose the intent in a conversation with the user.
    inputContexts :: Lude.Maybe [InputContext],
    -- | If defined in the intent, Amazon Lex uses this prompt to solicit additional user activity after the intent is fulfilled.
    followUpPrompt :: Lude.Maybe FollowUpPrompt,
    -- | The date that the intent was updated. When you create a resource, the creation date and last update dates are the same.
    lastUpdatedDate :: Lude.Maybe Lude.Timestamp,
    -- | An array of @OutputContext@ objects that lists the contexts that the intent activates when the intent is fulfilled.
    outputContexts :: Lude.Maybe [OutputContext],
    -- | If defined in the intent, Amazon Lex prompts the user to confirm the intent before fulfilling it.
    confirmationPrompt :: Lude.Maybe Prompt,
    -- | @True@ if a new version of the intent was created. If the @createVersion@ field was not specified in the request, the @createVersion@ field is set to false in the response.
    createVersion :: Lude.Maybe Lude.Bool,
    -- | If defined in the intent, Amazon Lex invokes this Lambda function for each user input.
    dialogCodeHook :: Lude.Maybe CodeHook,
    -- | A description of the intent.
    description :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutIntentResponse' with the minimum fields required to make a request.
--
-- * 'fulfillmentActivity' - If defined in the intent, Amazon Lex invokes this Lambda function to fulfill the intent after the user provides all of the information required by the intent.
-- * 'slots' - An array of intent slots that are configured for the intent.
-- * 'rejectionStatement' - If the user answers "no" to the question defined in @confirmationPrompt@ Amazon Lex responds with this statement to acknowledge that the intent was canceled.
-- * 'checksum' - Checksum of the @> LATEST@ version of the intent created or updated.
-- * 'conclusionStatement' - After the Lambda function specified in the@fulfillmentActivity@ intent fulfills the intent, Amazon Lex conveys this statement to the user.
-- * 'sampleUtterances' - An array of sample utterances that are configured for the intent.
-- * 'parentIntentSignature' - A unique identifier for the built-in intent that this intent is based on.
-- * 'createdDate' - The date that the intent was created.
-- * 'kendraConfiguration' - Configuration information, if any, required to connect to an Amazon Kendra index and use the @AMAZON.KendraSearchIntent@ intent.
-- * 'name' - The name of the intent.
-- * 'version' - The version of the intent. For a new intent, the version is always @> LATEST@ .
-- * 'inputContexts' - An array of @InputContext@ objects that lists the contexts that must be active for Amazon Lex to choose the intent in a conversation with the user.
-- * 'followUpPrompt' - If defined in the intent, Amazon Lex uses this prompt to solicit additional user activity after the intent is fulfilled.
-- * 'lastUpdatedDate' - The date that the intent was updated. When you create a resource, the creation date and last update dates are the same.
-- * 'outputContexts' - An array of @OutputContext@ objects that lists the contexts that the intent activates when the intent is fulfilled.
-- * 'confirmationPrompt' - If defined in the intent, Amazon Lex prompts the user to confirm the intent before fulfilling it.
-- * 'createVersion' - @True@ if a new version of the intent was created. If the @createVersion@ field was not specified in the request, the @createVersion@ field is set to false in the response.
-- * 'dialogCodeHook' - If defined in the intent, Amazon Lex invokes this Lambda function for each user input.
-- * 'description' - A description of the intent.
-- * 'responseStatus' - The response status code.
mkPutIntentResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutIntentResponse
mkPutIntentResponse pResponseStatus_ =
  PutIntentResponse'
    { fulfillmentActivity = Lude.Nothing,
      slots = Lude.Nothing,
      rejectionStatement = Lude.Nothing,
      checksum = Lude.Nothing,
      conclusionStatement = Lude.Nothing,
      sampleUtterances = Lude.Nothing,
      parentIntentSignature = Lude.Nothing,
      createdDate = Lude.Nothing,
      kendraConfiguration = Lude.Nothing,
      name = Lude.Nothing,
      version = Lude.Nothing,
      inputContexts = Lude.Nothing,
      followUpPrompt = Lude.Nothing,
      lastUpdatedDate = Lude.Nothing,
      outputContexts = Lude.Nothing,
      confirmationPrompt = Lude.Nothing,
      createVersion = Lude.Nothing,
      dialogCodeHook = Lude.Nothing,
      description = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If defined in the intent, Amazon Lex invokes this Lambda function to fulfill the intent after the user provides all of the information required by the intent.
--
-- /Note:/ Consider using 'fulfillmentActivity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirsFulfillmentActivity :: Lens.Lens' PutIntentResponse (Lude.Maybe FulfillmentActivity)
pirsFulfillmentActivity = Lens.lens (fulfillmentActivity :: PutIntentResponse -> Lude.Maybe FulfillmentActivity) (\s a -> s {fulfillmentActivity = a} :: PutIntentResponse)
{-# DEPRECATED pirsFulfillmentActivity "Use generic-lens or generic-optics with 'fulfillmentActivity' instead." #-}

-- | An array of intent slots that are configured for the intent.
--
-- /Note:/ Consider using 'slots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirsSlots :: Lens.Lens' PutIntentResponse (Lude.Maybe [Slot])
pirsSlots = Lens.lens (slots :: PutIntentResponse -> Lude.Maybe [Slot]) (\s a -> s {slots = a} :: PutIntentResponse)
{-# DEPRECATED pirsSlots "Use generic-lens or generic-optics with 'slots' instead." #-}

-- | If the user answers "no" to the question defined in @confirmationPrompt@ Amazon Lex responds with this statement to acknowledge that the intent was canceled.
--
-- /Note:/ Consider using 'rejectionStatement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirsRejectionStatement :: Lens.Lens' PutIntentResponse (Lude.Maybe Statement)
pirsRejectionStatement = Lens.lens (rejectionStatement :: PutIntentResponse -> Lude.Maybe Statement) (\s a -> s {rejectionStatement = a} :: PutIntentResponse)
{-# DEPRECATED pirsRejectionStatement "Use generic-lens or generic-optics with 'rejectionStatement' instead." #-}

-- | Checksum of the @> LATEST@ version of the intent created or updated.
--
-- /Note:/ Consider using 'checksum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirsChecksum :: Lens.Lens' PutIntentResponse (Lude.Maybe Lude.Text)
pirsChecksum = Lens.lens (checksum :: PutIntentResponse -> Lude.Maybe Lude.Text) (\s a -> s {checksum = a} :: PutIntentResponse)
{-# DEPRECATED pirsChecksum "Use generic-lens or generic-optics with 'checksum' instead." #-}

-- | After the Lambda function specified in the@fulfillmentActivity@ intent fulfills the intent, Amazon Lex conveys this statement to the user.
--
-- /Note:/ Consider using 'conclusionStatement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirsConclusionStatement :: Lens.Lens' PutIntentResponse (Lude.Maybe Statement)
pirsConclusionStatement = Lens.lens (conclusionStatement :: PutIntentResponse -> Lude.Maybe Statement) (\s a -> s {conclusionStatement = a} :: PutIntentResponse)
{-# DEPRECATED pirsConclusionStatement "Use generic-lens or generic-optics with 'conclusionStatement' instead." #-}

-- | An array of sample utterances that are configured for the intent.
--
-- /Note:/ Consider using 'sampleUtterances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirsSampleUtterances :: Lens.Lens' PutIntentResponse (Lude.Maybe [Lude.Text])
pirsSampleUtterances = Lens.lens (sampleUtterances :: PutIntentResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {sampleUtterances = a} :: PutIntentResponse)
{-# DEPRECATED pirsSampleUtterances "Use generic-lens or generic-optics with 'sampleUtterances' instead." #-}

-- | A unique identifier for the built-in intent that this intent is based on.
--
-- /Note:/ Consider using 'parentIntentSignature' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirsParentIntentSignature :: Lens.Lens' PutIntentResponse (Lude.Maybe Lude.Text)
pirsParentIntentSignature = Lens.lens (parentIntentSignature :: PutIntentResponse -> Lude.Maybe Lude.Text) (\s a -> s {parentIntentSignature = a} :: PutIntentResponse)
{-# DEPRECATED pirsParentIntentSignature "Use generic-lens or generic-optics with 'parentIntentSignature' instead." #-}

-- | The date that the intent was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirsCreatedDate :: Lens.Lens' PutIntentResponse (Lude.Maybe Lude.Timestamp)
pirsCreatedDate = Lens.lens (createdDate :: PutIntentResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdDate = a} :: PutIntentResponse)
{-# DEPRECATED pirsCreatedDate "Use generic-lens or generic-optics with 'createdDate' instead." #-}

-- | Configuration information, if any, required to connect to an Amazon Kendra index and use the @AMAZON.KendraSearchIntent@ intent.
--
-- /Note:/ Consider using 'kendraConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirsKendraConfiguration :: Lens.Lens' PutIntentResponse (Lude.Maybe KendraConfiguration)
pirsKendraConfiguration = Lens.lens (kendraConfiguration :: PutIntentResponse -> Lude.Maybe KendraConfiguration) (\s a -> s {kendraConfiguration = a} :: PutIntentResponse)
{-# DEPRECATED pirsKendraConfiguration "Use generic-lens or generic-optics with 'kendraConfiguration' instead." #-}

-- | The name of the intent.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirsName :: Lens.Lens' PutIntentResponse (Lude.Maybe Lude.Text)
pirsName = Lens.lens (name :: PutIntentResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: PutIntentResponse)
{-# DEPRECATED pirsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The version of the intent. For a new intent, the version is always @> LATEST@ .
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirsVersion :: Lens.Lens' PutIntentResponse (Lude.Maybe Lude.Text)
pirsVersion = Lens.lens (version :: PutIntentResponse -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: PutIntentResponse)
{-# DEPRECATED pirsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | An array of @InputContext@ objects that lists the contexts that must be active for Amazon Lex to choose the intent in a conversation with the user.
--
-- /Note:/ Consider using 'inputContexts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirsInputContexts :: Lens.Lens' PutIntentResponse (Lude.Maybe [InputContext])
pirsInputContexts = Lens.lens (inputContexts :: PutIntentResponse -> Lude.Maybe [InputContext]) (\s a -> s {inputContexts = a} :: PutIntentResponse)
{-# DEPRECATED pirsInputContexts "Use generic-lens or generic-optics with 'inputContexts' instead." #-}

-- | If defined in the intent, Amazon Lex uses this prompt to solicit additional user activity after the intent is fulfilled.
--
-- /Note:/ Consider using 'followUpPrompt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirsFollowUpPrompt :: Lens.Lens' PutIntentResponse (Lude.Maybe FollowUpPrompt)
pirsFollowUpPrompt = Lens.lens (followUpPrompt :: PutIntentResponse -> Lude.Maybe FollowUpPrompt) (\s a -> s {followUpPrompt = a} :: PutIntentResponse)
{-# DEPRECATED pirsFollowUpPrompt "Use generic-lens or generic-optics with 'followUpPrompt' instead." #-}

-- | The date that the intent was updated. When you create a resource, the creation date and last update dates are the same.
--
-- /Note:/ Consider using 'lastUpdatedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirsLastUpdatedDate :: Lens.Lens' PutIntentResponse (Lude.Maybe Lude.Timestamp)
pirsLastUpdatedDate = Lens.lens (lastUpdatedDate :: PutIntentResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdatedDate = a} :: PutIntentResponse)
{-# DEPRECATED pirsLastUpdatedDate "Use generic-lens or generic-optics with 'lastUpdatedDate' instead." #-}

-- | An array of @OutputContext@ objects that lists the contexts that the intent activates when the intent is fulfilled.
--
-- /Note:/ Consider using 'outputContexts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirsOutputContexts :: Lens.Lens' PutIntentResponse (Lude.Maybe [OutputContext])
pirsOutputContexts = Lens.lens (outputContexts :: PutIntentResponse -> Lude.Maybe [OutputContext]) (\s a -> s {outputContexts = a} :: PutIntentResponse)
{-# DEPRECATED pirsOutputContexts "Use generic-lens or generic-optics with 'outputContexts' instead." #-}

-- | If defined in the intent, Amazon Lex prompts the user to confirm the intent before fulfilling it.
--
-- /Note:/ Consider using 'confirmationPrompt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirsConfirmationPrompt :: Lens.Lens' PutIntentResponse (Lude.Maybe Prompt)
pirsConfirmationPrompt = Lens.lens (confirmationPrompt :: PutIntentResponse -> Lude.Maybe Prompt) (\s a -> s {confirmationPrompt = a} :: PutIntentResponse)
{-# DEPRECATED pirsConfirmationPrompt "Use generic-lens or generic-optics with 'confirmationPrompt' instead." #-}

-- | @True@ if a new version of the intent was created. If the @createVersion@ field was not specified in the request, the @createVersion@ field is set to false in the response.
--
-- /Note:/ Consider using 'createVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirsCreateVersion :: Lens.Lens' PutIntentResponse (Lude.Maybe Lude.Bool)
pirsCreateVersion = Lens.lens (createVersion :: PutIntentResponse -> Lude.Maybe Lude.Bool) (\s a -> s {createVersion = a} :: PutIntentResponse)
{-# DEPRECATED pirsCreateVersion "Use generic-lens or generic-optics with 'createVersion' instead." #-}

-- | If defined in the intent, Amazon Lex invokes this Lambda function for each user input.
--
-- /Note:/ Consider using 'dialogCodeHook' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirsDialogCodeHook :: Lens.Lens' PutIntentResponse (Lude.Maybe CodeHook)
pirsDialogCodeHook = Lens.lens (dialogCodeHook :: PutIntentResponse -> Lude.Maybe CodeHook) (\s a -> s {dialogCodeHook = a} :: PutIntentResponse)
{-# DEPRECATED pirsDialogCodeHook "Use generic-lens or generic-optics with 'dialogCodeHook' instead." #-}

-- | A description of the intent.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirsDescription :: Lens.Lens' PutIntentResponse (Lude.Maybe Lude.Text)
pirsDescription = Lens.lens (description :: PutIntentResponse -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: PutIntentResponse)
{-# DEPRECATED pirsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirsResponseStatus :: Lens.Lens' PutIntentResponse Lude.Int
pirsResponseStatus = Lens.lens (responseStatus :: PutIntentResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutIntentResponse)
{-# DEPRECATED pirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
